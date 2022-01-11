unit tonapi;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson
  ;

type

  { TgetAddressInformationResult }

  TgetAddressInformationResult = class
  private
    FBalance: Int64;
    FCode: String;
    FData: String;
    FState: String;
  published
    property balance: Int64 read FBalance write FBalance; // in nanograms
    property code: String read FCode write FCode;
    property data: String read FData write FData;
    property state: String read FState write FState;
  end;

  { TTonAPI }

  TTonAPI = class
  private
    class function APIMethod(const aUrl: String; aResponce: TObject; out aCode: Integer; out aError: String): Boolean;
  public
    class function getAddressInformation(const aAddress: String; aResponce: TgetAddressInformationResult;
      out aCode: Integer; out aError: String): Boolean;
  end;



implementation

uses
  fphttpclient, opensslsockets, fpjsonrtti
  ;

const
  _APIENDPOINT = 'https://toncenter.com/api/v2/';

function RoteEndpoint(const aMethod, aAddress: String): String;
begin
  Result:=Format(_APIENDPOINT+aMethod+'?address=%s', [aAddress]);
end;

{ TTonAPI }

class function TTonAPI.APIMethod(const aUrl: String; aResponce: TObject; out aCode: Integer; out aError: String
  ): Boolean;
var
  aDestreamer: TJSONDeStreamer;
  aJSONObject: TJSONObject;
begin
  Result:=False;
  aError:=EmptyStr;
  aCode:=0;
  try
    aJSONObject:=GetJSON(TFPHTTPClient.SimpleGet(aUrl)) as TJSONObject;
    if not aJSONObject.Booleans['ok'] then
    begin
      aError:=aJSONObject.Get('error', EmptyStr);
      aCode:=aJSONObject.Get('code', 0);
      Exit;
    end;
    aDestreamer:=TJSONDeStreamer.Create(nil);
    try
      aDestreamer.JSONToObject(aJSONObject.Objects['result'], aResponce);
      Result:=True;
    finally
      aDestreamer.Free;
    end;
  finally
    aJSONObject.Free;
  end;
end;

class function TTonAPI.getAddressInformation(const aAddress: String; aResponce: TgetAddressInformationResult;
  out aCode: Integer; out aError: String): Boolean;
begin
  Result:=APIMethod(RoteEndpoint('getAddressInformation', aAddress), aResponce, aCode, aError);
end;

end.

