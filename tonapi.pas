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
    function APIMethod(const aUrl: String; aResponce: TObject = nil): Boolean;
  public
    destructor Destroy; override;
    function getAddressInformation(const aAddress: String; aResult: TgetAddressInformationResult): Boolean; 
    function getTransactions(const aAddress: String; aLimit: Integer = 10): Boolean;
    property ErrorCode: Integer read FCode;
    property ErrorDescription: String read FError;
    property RawResponce: TJSONObject read FRawResponce;
  end;

  { Procedure style }

function getAddressInformation(const aAddress: String; aResult: TgetAddressInformationResult;
  out aCode: Integer; out aError: String): Boolean;
function getTransactions(const aAddress: String; out aCode: Integer; out aError: String): Boolean;

implementation

uses
  fphttpclient, opensslsockets, fpjsonrtti
  ;

const
  _APIENDPOINT = 'https://toncenter.com/api/v2/';

function RoteEndpoint(const aMethod, aAddress: String; aLimit: Integer = 0): String;
begin
  if aLimit=0 then
    Result:=Format(_APIENDPOINT+aMethod+'?address=%s', [aAddress])
  else
    Result:=Format(_APIENDPOINT+aMethod+'?address=%s&limit=%d', [aAddress, aLimit]);
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

function getTransactions(const aAddress: String; out aCode: Integer; out
  aError: String): Boolean;
var
  aTON: TTonAPI;
begin
  aTON:=TTonAPI.Create;
  try
    Result:=aTON.getTransactions(aAddress);
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
  if not Assigned(aResponce) then
    Exit; // If you do not need to stream JSON to the Object, then we simply do not pass it in the parameters
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

function TTonAPI.getTransactions(const aAddress: String; aLimit: Integer): Boolean;
begin
  FreeAndNil(FRawResponce);
  Result:=APIMethod(RoteEndpoint('getTransactions', aAddress, aLimit));
end;

end.

