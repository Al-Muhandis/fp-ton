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
    FCode: Integer;
    FError: String;
    FRawResponce: TJSONObject;                                                                                        
    function APIMethod(const aUrl: String; aResponce: TObject): Boolean;
  public
    destructor Destroy; override;
    function getAddressInformation(const aAddress: String; aResult: TgetAddressInformationResult): Boolean;
    property ErrorCode: Integer read FCode;
    property ErrorDescription: String read FError;
    property RawResponce: TJSONObject read FRawResponce;
  end;

  { Procedure style }

function getAddressInformation(const aAddress: String; aResult: TgetAddressInformationResult;
  out aCode: Integer; out aError: String): Boolean;

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

function getAddressInformation(const aAddress: String; aResult: TgetAddressInformationResult; out aCode: Integer; out
  aError: String): Boolean;
var
  aTON: TTonAPI;
begin
  aTON:=TTonAPI.Create;
  try
    Result:=aTON.getAddressInformation(aAddress, aResult);
    aCode:=aTON.ErrorCode;
    aError:=aTON.ErrorDescription;
  finally   
    aTON.Free;
  end;
end;

{ TTonAPI }

function TTonAPI.APIMethod(const aUrl: String; aResponce: TObject): Boolean;
var
  aDestreamer: TJSONDeStreamer;
begin
  Result:=False;
  FError:=EmptyStr;
  FCode:=0;
  FreeAndNil(FRawResponce);
  FRawResponce:=GetJSON(TFPHTTPClient.SimpleGet(aUrl)) as TJSONObject;
  if not FRawResponce.Booleans['ok'] then
  begin
    FError:=FRawResponce.Get('error', EmptyStr);
    FCode:=FRawResponce.Get('code', 0);
    Exit;
  end;
  aDestreamer:=TJSONDeStreamer.Create(nil);
  try
    aDestreamer.JSONToObject(FRawResponce.Objects['result'], aResponce);
    Result:=True;
  finally
    aDestreamer.Free;
  end;
end;

destructor TTonAPI.Destroy;
begin
  FRawResponce.Free;
  inherited Destroy;
end;

function TTonAPI.getAddressInformation(const aAddress: String; aResult: TgetAddressInformationResult): Boolean;
begin
  FreeAndNil(FRawResponce);
  Result:=APIMethod(RoteEndpoint('getAddressInformation', aAddress), aResult);
end;

end.

