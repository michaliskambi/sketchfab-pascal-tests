{ Search for and download a model from Sketchfab.

  Before compiling, create a file sketchfab_token.inc with your Sketchfab token
  as Pascal string, like '123456789'.
  You can find your Sketchfab token on https://sketchfab.com/settings/password .

  Compile this application using Castle Game Engine editor or build tool,
  it will automatically define proper syntax mode (FPC ObjFpc mode) and make
  CGE units available. E.g.

  - Open this directory in CGE editor and compile.
  - Or run "castle-engine compile" from command-line in this directory.

  Copyright 2023-2023 Michalis Kamburelis.
  Licensed on the permissive terms (modified BSD 3-clause).
}

{$apptype CONSOLE}

program sketchfab_download;

uses
  { Using CThreads, as threads are required by TCastleDownload asynchronous version.
    Note: All downloads in this application are actually synchronous (blocking),
    you could rewrite it to just use blocking TCastleDownload.Download.
    But we want to show how to use asynchronous TCastleDownload as that's
    what we'll use in CGE editor to not freeze UI. }
  {$ifdef UNIX} CThreads, {$endif}
  SysUtils, Classes, Generics.Collections,
  {$ifndef VER3_0} OpenSslSockets, {$endif}
  FpJson, JsonParser, Zipper,
  { CGE units }
  CastleFilesUtils, CastleDownload, CastleStringUtils, CastleURIUtils, CastleLog,
  CastleUtils, CastleClassUtils, CastleApplicationProperties;

const
  { Sketchfab API returns thumbnails in various sizes,
    we will pick the closest one to this. }
  BestThumbnailSize = 256;

type
  TSketchfabModel = class;

  TSketchfabModelList = {$ifdef FPC}specialize{$endif} TObjectList<TSketchfabModel>;

  TSketchfabModel = class
  private
    DownloadURL: String;
    DownloadSize: Int64;
  public
    ModelId: String;

    { Various additional info about model,
      set by @link(Search) and @link(SearchGetFirst).
      Not used in this application, but will be useful for UI in CGE. }
    Name, Description: String;
    FaceCount: UInt64;
    ThumbnailUrl: String;
    License: String;

    { Search Sketchfab for Query, return list of model ids. }
    class function Search(const Query: String): TSketchfabModelList;
    { Search Sketchfab for Query, show list of model ids, return 1st. }
    class function SearchGetFirst(const Query: String): TSketchfabModel;

    { Set Download* fields based on ModelId }
    procedure StartDownload;
    { Use Download* fields to get model.zip }
    procedure DownloadZip;
    { Extract model.zip to model/ModelId/ directory}
    procedure ExtractZip;
    { Run view3dscene on extracted model. }
    procedure RunView3dscene;
  end;

{ Show progress of download in a console,
  waiting for download to finish.
  Make exception if download fails. }
procedure WaitForFinish(const Description: String; const Download: TCastleDownload); forward;

{ TSketchfabModel (class methods) --------------------------------------------- }

class function TSketchfabModel.Search(const Query: String): TSketchfabModelList;

  { Find thumbnail URL best matching given Size, in a JSON array
    as returned by Sketchfab search query.
    Returns '' if no thumbnail found. }
  function SearchThumbnails(const Thumbnails: TJSONArray; const DesiredSize: Integer): String;
  var
    I: Integer;
    Thumbnail: TJSONObject;
    ThumbnailSize: Integer;
    BestThumbnail: TJSONObject;
    BestThumbnailSize: Integer;
  begin
    BestThumbnail := nil;
    BestThumbnailSize := 0;
    for I := 0 to Thumbnails.Count - 1 do
    begin
      Thumbnail := Thumbnails[I] as TJSONObject;
      // average width and height
      ThumbnailSize := (Thumbnail.Integers['width'] + Thumbnail.Integers['height']) div 2;
      if (BestThumbnail = nil) or
         ( Abs(ThumbnailSize - DesiredSize) <
           Abs(BestThumbnailSize - DesiredSize) ) then
      begin
        BestThumbnail := Thumbnail;
        BestThumbnailSize := ThumbnailSize;
      end;
    end;
    if BestThumbnail <> nil then
    begin
      Result := BestThumbnail.Strings['url'];
      // WritelnLog('Best thumbnail dimensions: %d x %d, url: %s', [
      //   BestThumbnail.Integers['width'],
      //   BestThumbnail.Integers['height'],
      //   Result
      // ]);
    end else
      Result := '';
  end;

var
  Download: TCastleDownload;
  Response: String;
  JSONData: TJSONData;
  JSONArray: TJSONArray;
  JSONObject: TJSONObject;
  I: Integer;
  Model: TSketchfabModel;
begin
  Result := TSketchfabModelList.Create(true);
  Download := TCastleDownload.Create(nil);
  try
    Download.Url := 'https://api.sketchfab.com/v3/search?type=models&downloadable=true&q=' + InternalUriEscape(Query);
    Download.Start;
    WaitForFinish('Searching Sketchfab', Download);
    Response := StreamToString(Download.Contents);

    JSONData := GetJSON(Response);

    WritelnLog('Got response, storing in response-search.json (for debug)');
    StringToFile('response-search.json', Response);

    if JSONData.JSONType = jtObject then
    begin
      JSONArray := TJSONObject(JSONData).Arrays['results'];
      for I := 0 to JSONArray.Count - 1 do
      begin
        JSONObject := JSONArray.Objects[I];

        // sanity check
        if not JSONObject.Booleans['isDownloadable'] then
        begin
          WritelnWarning('Model %s not downloadable, even though we requested only downloadable');
          Continue;
        end;

        Model := TSketchfabModel.Create;
        Model.ModelId := JSONObject.Strings['uid'];
        Model.Name := JSONObject.Strings['name'];
        Model.Description := JSONObject.Strings['description'];
        Model.FaceCount := JSONObject.QWords['faceCount'];
        Model.ThumbnailUrl := SearchThumbnails(JSONObject.Objects['thumbnails'].Arrays['images'], BestThumbnailSize);
        Model.License := JSONObject.Objects['license'].Strings['label'];
        Result.Add(Model);
      end;
    end else
      raise Exception.Create('Unexpected JSON response: ' + Response);
  finally
    FreeAndNil(Download);
  end;
end;

class function TSketchfabModel.SearchGetFirst(const Query: String): TSketchfabModel;

  function DescriptionCut(const OriginalDescription: String): String;
  const
    MaxLen = 80;
  var
    NewlinePos: Integer;
  begin
    Result := OriginalDescription;
    NewlinePos := CharsPos([#10, #13], Result);
    if NewlinePos <> 0 then
      Result := Copy(Result, 1, NewlinePos - 1);
    if Length(Result) > MaxLen then
      Result := Copy(Result, 1, MaxLen) + '...';
  end;

var
  Models: TSketchfabModelList;
  I: Integer;
begin
  Models := TSketchfabModel.Search(Query);
  try
    WritelnLog('Found %d models for query: %s', [Models.Count, Query]);
    if Models.Count = 0 then
      raise Exception.Create('No models found for query: ' + Query);
    for I := 0 to Models.Count - 1 do
      WritelnLog('%d : %s' + NL +
        '  View on Sketchfab: https://sketchfab.com/3d-models/%s' + NL +
        '  Description: %s' + NL +
        '  Face Count: %d' + NL +
        '  License: %s' + NL +
        '  Best (closest to %d) thumbnail: %s' + NL, [
        I,
        Models[I].Name,
        Models[I].ModelId,
        DescriptionCut(Models[I].Description),
        Models[I].FaceCount,
        Models[I].License,
        BestThumbnailSize,
        Models[I].ThumbnailUrl
      ]);
    Result := Models.ExtractIndex(0);
  finally
    FreeAndNil(Models);
  end;
end;

{ TSketchfabModel (instance methods) ----------------------------------------- }

procedure TSketchfabModel.StartDownload;
const
  ApiUrl = 'https://api.sketchfab.com/v3/models';
  ApiToken = {$I sketchfab_token.inc};
var
  Download: TCastleDownload;
  Response: String;
  JSONData: TJSONData;
begin
  Download := TCastleDownload.Create(nil);
  try
    WritelnLog('Starting download of model id ' + ModelId);
    Download.HttpHeader('Authorization', 'Token ' + ApiToken);
    Download.Url := ApiUrl + '/' + ModelId + '/download';
    Download.Start;
    WaitForFinish('Starting download of model', Download);
    Response := StreamToString(Download.Contents);
  finally
    FreeAndNil(Download);
  end;

  WritelnLog('Got response, storing in response-download.json (for debug)');
  StringToFile('response-download.json', Response);

  JSONData := GetJSON(Response);
  try
    DownloadURL := JSONData.FindPath('gltf.url').AsString;
    DownloadSize := JSONData.FindPath('gltf.size').AsInt64;
  finally
    FreeAndNil(JSONData);
  end;

  WritelnLog('Downloading model from: '+ DownloadURL);
  WritelnLog('Download size: ' + SizeToStr(DownloadSize));
end;

procedure TSketchfabModel.DownloadZip;
var
  Download: TCastleDownload;
begin
  Download := TCastleDownload.Create(nil);
  try
    Download.Url := DownloadURL;
    Download.Start;
    WaitForFinish('Downloading model', Download);
    StreamSaveToFile(Download.Contents, 'model.zip');
    WritelnLog('Model downloaded to: model.zip, file size: ' + SizeToStr(Download.Contents.Size));
  finally
    FreeAndNil(Download);
  end;
end;

procedure TSketchfabModel.ExtractZip;
var
  Zip: TUnZipper;
  DirName: String;
begin
  { Unzip model.zip to model/ directory. }
  DirName := 'model/' + ModelId;
  RemoveNonEmptyDir(DirName, true);
  ForceDirectories(DirName);

  Zip := TUnZipper.Create;
  try
    Zip.FileName := 'model.zip';
    Zip.OutputPath := DirName;
    Zip.Examine;
    Zip.UnZipAllFiles;
  finally
    FreeAndNil(Zip);
  end;

  WritelnLog('Model extracted to: ' + DirName);
end;

procedure TSketchfabModel.RunView3dscene;
var
  Exe: String;
begin
  Exe := FindExe('view3dscene');
  if Exe = '' then
    raise Exception.Create('view3dscene not found on $PATH, please install Castle Game Engine and make sure view3dscene is on $PATH');
  ExecuteProcess(Exe, ['model/' + ModelId + '/scene.gltf']);
end;

{ routines ------------------------------------------------------------------- }

procedure WaitForFinish(const Description: String; const Download: TCastleDownload);
const
  MaxDots = 60;
var
  ProgressDots: Integer;

  procedure BumpProgress(const NewProgress: Single);
  var
    NewProgressDots: Integer;
  begin
    NewProgressDots := Round(MaxDots * NewProgress);
    MaxVar(NewProgressDots, ProgressDots); // do not go backward
    Write(StringOfChar('.', NewProgressDots - ProgressDots));
    ProgressDots := NewProgressDots;
  end;

begin
  Write(Description + ': ');
  ProgressDots := 0;

  while Download.Status = dsDownloading do
  begin
    if Download.TotalBytes <> -1 then
      BumpProgress(Download.DownloadedBytes / Download.TotalBytes);
    Sleep(100);
    ApplicationProperties._Update; // to process downloading
  end;

  if Download.Status = dsSuccess then
    BumpProgress(1.0)
  else
    raise Exception.CreateFmt('Download failed: %s', [Download.ErrorMessage]);

  Writeln;
end;

{ main ----------------------------------------------------------------------- }

var
  Query: String = 'cthulhu';
  Model: TSketchfabModel;
begin
  InitializeLog;

  if ParamCount >= 1 then
    Query := ParamStr(1);
  Model := TSketchfabModel.SearchGetFirst(Query);

  // from https://sketchfab.com/3d-models/statue-of-cthulhu-f936c896d628415597b762d4e3944ffc
  // Model := TSketchfabModel.Create;
  // Model.ModelId := 'f936c896d628415597b762d4e3944ffc';

  try
    Model.StartDownload;
    Model.DownloadZip;
    Model.ExtractZip;
    Model.RunView3dscene;
  finally
    FreeAndNil(Model);
  end;
end.