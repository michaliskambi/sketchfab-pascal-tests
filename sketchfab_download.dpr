{ Search for and download a model from Sketchfab.
  
  Compile using CGE build tool, it will automatically
  define ObjFpc mode, $H+ etc. }

program SketchfabDownload;

uses SysUtils, Classes,
  {$ifndef VER3_0} OpenSSLSockets, {$endif}
  fpHTTPClient, fpJSON, JSONParser, Zipper,
  CastleFilesUtils, CastleDownload, CastleStringUtils;

type
  TSketchfabModel = class
  private
    DownloadURL: String;
    DownloadSize: Int64;
  public
    ModelID: String;
    { Set Download* fields based on ModelID }
    procedure StartDownload;
    { Use Download* fields to get model.zip }
    procedure DownloadZip;
    { Extract model.zip to model/ModelID/ directory}
    procedure ExtractZip;
    { Run view3dscene on extracted model. }
    procedure RunView3dscene;
  end;

procedure TSketchfabModel.StartDownload;
const
  API_URL = 'https://api.sketchfab.com/v3/models';
  API_TOKEN = {$I sketchfab_token.inc};
var
  Client: TFPHTTPClient;
  Response: String;
  JSONData: TJSONData;
begin
  Client := TFPHTTPClient.Create(nil);
  try
    Writeln('Starting download of model id ', ModelID);
    Client.AddHeader('Authorization', 'Token ' + API_TOKEN);
    Response := Client.Get(API_URL + '/' + ModelID + '/download');
  finally
    FreeAndNil(Client);
  end;

  Writeln('Got response, storing in response.json (for debug)');
  StringToFile('response.json', Response);

  JSONData := GetJSON(Response);
  try
    DownloadURL := JSONData.FindPath('gltf.url').AsString;
    DownloadSize := JSONData.FindPath('gltf.size').AsInt64;
  finally
    FreeAndNil(JSONData);
  end;

  Writeln('Downloading model from: ', DownloadURL);
  Writeln('Download size: ', SizeToStr(DownloadSize));
end;

procedure TSketchfabModel.DownloadZip;
var
  Stream: TStream;
begin
  EnableBlockingDownloads := true;
  Stream := Download(DownloadURL, []);
  try
    StreamSaveToFile(Stream, 'model.zip');
    Writeln('Model downloaded to: model.zip, file size: ', SizeToStr(Stream.Size));
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

  Writeln('Model extracted to: ', DirName);
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
  ModelID: String;
  Model: TSketchfabModel;
begin
  // TODO: Search for model id on Sketchfab
  
  // https://sketchfab.com/3d-models/statue-of-cthulhu-f936c896d628415597b762d4e3944ffc
  ModelID := 'f936c896d628415597b762d4e3944ffc';

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