unit config;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TConfig }

  TConfig = class
  private
    FAddress: String;
  published
    property Address: String read FAddress write FAddress;
  end;

var
  _Config: TConfig;

implementation

uses
  fpjsonrtti, fpjson, jsonparser, jsonscanner
  ;

procedure LoadFromJSON(AObject: TObject; const AFileName: String);
var
  ADeStreamer: TJSONDeStreamer;
  AJSON: TStringList;
begin
  if not FileExists(AFileName) then
    Exit;
  ADeStreamer:=TJSONDeStreamer.Create(nil);
  try
    AJSON:=TStringList.Create;
    try
      AJSON.LoadFromFile(AFileName);
      try
        ADeStreamer.JSONToObject(AJSON.Text, AObject);
      except
      end;
    finally
      AJSON.Free;
    end;
  finally
    ADeStreamer.Free;
  end;
end;

function StringToJSONObject(const AString: String): TJSONObject;
var
  lParser: TJSONParser;
begin
  Result := nil;
  if AString<>EmptyStr then
  begin
    lParser := TJSONParser.Create(AString, DefaultOptions);
    try
      try
        Result := lParser.Parse as TJSONObject
      except
        Result:=nil;
      end;
    finally
      lParser.Free;
    end;
  end;
end;


procedure SaveToJSON(AObject: TObject; const AFileName: String);
var
  AStreamer: TJSONStreamer;
  AJSON: TStringList;
  AJSONObject: TJSONObject;
begin
  AStreamer:=TJSONStreamer.Create(nil);
  AStreamer.Options:=[jsoTStringsAsArray];
  try
    AJSON:=TStringList.Create;
    try
      try
        AJSONObject:=AStreamer.ObjectToJSON(AObject);
        try
          AJSON.Text:=AJSONObject.FormatJSON();
          AJSON.SaveToFile(AFileName);
        finally
          AJSONObject.Free;
        end;
      except
      end;
    finally
      AJSON.Free;
    end;
  finally
    AStreamer.Free;
  end;
end;

initialization
  _Config:=TConfig.Create;
  LoadFromJSON(_Config, 'config.json');

finalization
  SaveToJSON(_Config, '~stratch_config.json');
  _Config.Free;

end.

