{ Search for and download a model from Sketchfab.

  Compile using CGE build tool, it will automatically
  define ObjFpc mode, $H+ etc.

  TODO: Use TCastleDownload for all TFPHTTPClient calls.
}

{$apptype CONSOLE}

program SketchfabDownload;

uses SysUtils, Classes,
  {$ifndef VER3_0} OpenSSLSockets, {$endif}
  fpHTTPClient, fpJSON, JSONParser, Zipper,
  CastleFilesUtils, CastleDownload, CastleStringUtils, CastleURIUtils, CastleLog;

type
  TSketchfabModel = class
  private
    DownloadURL: String;
    DownloadSize: Int64;
  public
    ModelID: String;
    { Search Sketchfab for Query, return list of model ids. }
    class function Search(const Query: String): TStringList;
    { Search Sketchfab for Query, show list of model ids, return 1st. }
    class function SearchGetFirst(const Query: String): String;
    { Set Download* fields based on ModelID }
    procedure StartDownload;
    { Use Download* fields to get model.zip }
    procedure DownloadZip;
    { Extract model.zip to model/ModelID/ directory}
    procedure ExtractZip;
    { Run view3dscene on extracted model. }
    procedure RunView3dscene;
  end;

class function TSketchfabModel.Search(const Query: String): TStringList;
var
  HTTP: TFPHTTPClient;
  Response: String;
  JSONData: TJSONData;
  JSONArray: TJSONArray;
  JSONObject: TJSONObject;
  I: Integer;
begin
  Result := TStringList.Create;
  HTTP := TFPHTTPClient.Create(nil);
  try
    Response := HTTP.Get('https://api.sketchfab.com/v3/search?type=models&downloadable=true&q=' + InternalUriEscape(Query));
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
        Result.Add(JSONObject.Strings['uid']);
      end;
    end else
      raise Exception.Create('Unexpected JSON response: ' + Response);
  finally
    FreeAndNil(HTTP);
  end;
end;

class function TSketchfabModel.SearchGetFirst(const Query: String): String;
var
  Models: TStringList;
  I: Integer;
begin
  Models := TSketchfabModel.Search(Query);
  try
    WritelnLog('Found %d models for query: %s', [Models.Count, Query]);
    if Models.Count = 0 then
      raise Exception.Create('No models found for query: ' + Query);
    for I := 0 to Models.Count - 1 do
      WritelnLog('%d : https://sketchfab.com/3d-models/%s', [I, Models[I]]);
    Result := Models[0];
  finally
    FreeAndNil(Models);
  end;
end;

procedure TSketchfabModel.StartDownload;
const
  ApiUrl = 'https://api.sketchfab.com/v3/models';
  ApiToken = {$I sketchfab_token.inc};
var
  Client: TFPHTTPClient;
  Response: String;
  JSONData: TJSONData;
begin
  Client := TFPHTTPClient.Create(nil);
  try
    WritelnLog('Starting download of model id ', ModelID);
    Client.AddHeader('Authorization', 'Token ' + ApiToken);
    Response := Client.Get(ApiUrl + '/' + ModelID + '/download');
  finally
    FreeAndNil(Client);
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
  Stream: TStream;
begin
  EnableBlockingDownloads := true;
  Stream := Download(DownloadURL, []);
  try
    StreamSaveToFile(Stream, 'model.zip');
    WritelnLog('Model downloaded to: model.zip, file size: ' + SizeToStr(Stream.Size));
  finally
    FreeAndNil(Stream);
  end;
end;

procedure TSketchfabModel.ExtractZip;
var
  Zip: TUnZipper;
  DirName: String;
begin
  { Unzip model.zip to model/ directory. }
  DirName := 'model/' + ModelID;
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
  ExecuteProcess(Exe, ['model/' + ModelID + '/scene.gltf']);
end;

var
  Query: String = 'cthulhu';
  ModelID: String;
  Model: TSketchfabModel;
begin
  InitializeLog;

  if ParamCount >= 1 then
    Query := ParamStr(1);
  ModelID := TSketchfabModel.SearchGetFirst(Query);

  // from https://sketchfab.com/3d-models/statue-of-cthulhu-f936c896d628415597b762d4e3944ffc
  // ModelID := 'f936c896d628415597b762d4e3944ffc';

  Model := TSketchfabModel.Create;
  try
    Model.ModelID := ModelID;
    Model.StartDownload;
    Model.DownloadZip;
    Model.ExtractZip;
    Model.RunView3dscene;
  finally
    FreeAndNil(Model);
  end;
end.