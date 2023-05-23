{ Search for and download a model from Sketchfab.
  
  Compile using CGE build tool, it will automatically
  define ObjFpc mode, $H+ etc. }

program SketchfabDownload;

uses SysUtils, Classes,
  {$ifndef VER3_0} OpenSSLSockets, {$endif}
  fpHTTPClient, fpJSON, JSONParser,
  CastleFilesUtils, CastleDownload, CastleStringUtils;

const
  API_URL = 'https://api.sketchfab.com/v3/models';
  API_TOKEN = {$I sketchfab_token.inc};

var
  Client: TFPHTTPClient;
  Response: String;
  JSONData: TJSONData;
  ModelID, DownloadURL: String;
  Stream: TStream;
  DownloadSize: Int64;
begin
  // TODO: Search for model id on Sketchfab
  
  // https://sketchfab.com/3d-models/statue-of-cthulhu-f936c896d628415597b762d4e3944ffc
  ModelID := 'f936c896d628415597b762d4e3944ffc';
  Client := TFPHTTPClient.Create(nil);
  try
    Writeln('Starting download of model id ', ModelID);
    Client.AddHeader('Authorization', 'Token ' + API_TOKEN);
    Response := Client.Get(API_URL + '/' + ModelID + '/download');

    Writeln('Got response, storing in response.json (for debug)');
    StringToFile('response.json', Response);

    JSONData := GetJSON(Response);
    DownloadURL := JSONData.FindPath('gltf.url').AsString;
    DownloadSize := JSONData.FindPath('gltf.size').AsInt64;

    Writeln('Downloading model from: ', DownloadURL);
    Writeln('Download size: ', SizeToStr(DownloadSize));

    // fails with HTTP 400, ignore, we want to use CGE Download anyway
    //Client.Get(DownloadURL, 'model.zip'); 

    EnableBlockingDownloads := true;
    Stream := Download(DownloadURL, []);
    try
      StreamSaveToFile(Stream, 'model.zip');
      Writeln('Model downloaded to: model.zip, file size: ', SizeToStr(Stream.Size));
    finally
      Stream.Free;
    end;

    { Unzip model.zip to model/ directory. }
    // TODO
  finally
    Client.Free;
  end;
end.