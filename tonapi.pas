unit tonapi;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, eventlog
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
    FDebugLog: Boolean;
    FError: String;
    FEventLog: TEventLog;
    FRawResponce: TJSONObject;                                                                                        
    function APIMethod(const aUrl: String; aResponce: TObject = nil): Boolean;
    function EndpointGet(const aUrl: String): String;
    procedure Log(aEventType: TEventType; const aMessage: String);
  public
    destructor Destroy; override;
    function getAddressInformation(const aAddress: String; aResult: TgetAddressInformationResult): Boolean; 
    function getTransactions(const aAddress: String; aLimit: Integer = 10;
      aLT: Int64 = 0; const aHash: String = ''; aToLT: Int64 = 0; aArchival: Boolean = False): Boolean;
    property ErrorCode: Integer read FCode;
    property ErrorDescription: String read FError;
    property RawResponce: TJSONObject read FRawResponce;
    property EventLog: TEventLog read FEventLog write FEventLog;
    property DebugLog: Boolean read FDebugLog write FDebugLog;
  end;

  { Procedure style }

function getAddressInformation(const aAddress: String; aResult: TgetAddressInformationResult;
  out aCode: Integer; out aError: String): Boolean;
function getTransactions(const aAddress: String; out aCode: Integer; out aError: String): Boolean;

const
  _adrsTonDns = 'EQC3dNlesgVD8YbAazcauIrXBPfiVhMMr5YYk2in0Mtsz0Bz';

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

function RoteEndpoint(const aMethod: String; aGetParameters: TStrings): String;
var
  aGetS: String;
begin
  aGetParameters.StrictDelimiter:=True;
  aGetParameters.Delimiter:='&';
  aGetS:=aGetParameters.DelimitedText;
  Result:=_APIENDPOINT+aMethod;
  if not aGetS.IsEmpty then
    Result+='?';
  Result+=aGetS;
end;

function RoteGetTransactions(const aAddress: String; aLimit: Integer=0; aLogicalTime: Int64 = 0; aHash: String = '';
  aLogicalTimeTo: Int64 = 0; aArchival: Boolean = False): String;
var
  aParameters: TStringList;
begin
  aParameters:=TStringList.Create;
  try
    aParameters.AddPair('address', aAddress);
    if aLimit<>0 then
      aParameters.AddPair('limit', aLimit.ToString);  
    if aLogicalTime<>0 then
      aParameters.AddPair('lt', aLogicalTime.ToString);
    if aHash<>'' then
      aParameters.AddPair('hash', EncodeURLElement(aHash));
    if aLogicalTimeTo<>0 then
      aParameters.AddPair('to_lt', aLogicalTimeTo.ToString);
    if aArchival<>False then
      aParameters.AddPair('archival', BoolToStr(aArchival, 'true', 'false'));
    Result:=RoteEndpoint('getTransactions', aParameters);
  finally
    aParameters.Free;
  end;
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
  FRawResponce:=GetJSON(EndpointGet(aUrl)) as TJSONObject;
  if not Assigned(FRawResponce) then
    Exit;
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

function TTonAPI.EndpointGet(const aUrl: String): String;
var
  aHTTP: TFPHTTPClient;
begin
  aHTTP:=TFPHTTPClient.Create(nil);
  try
    try
      Result:=aHTTP.SimpleGet(aUrl);
    except
      on E:Exception do
        Log(etError, E.ClassName+': '+E.Message);
    end;
  finally
    aHTTP.Free;
  end;
end;

procedure TTonAPI.Log(aEventType: TEventType; const aMessage: String);
begin
  if (FDebugLog or (aEventType<>etDebug)) and Assigned(FEventLog) then
    FEventLog.Log(aEventType, aMessage);
end;

destructor TTonAPI.Destroy;
begin
  FRawResponce.Free;
  inherited Destroy;
end;

function TTonAPI.getAddressInformation(const aAddress: String; aResult: TgetAddressInformationResult): Boolean;
begin
  FreeAndNil(FRawResponce);
  Log(etDebug, 'getAddressInformation. Address: '+aAddress);
  Result:=APIMethod(RoteEndpoint('getAddressInformation', aAddress), aResult);
end;

function TTonAPI.getTransactions(const aAddress: String; aLimit: Integer; aLT: Int64; const aHash: String;
  aToLT: Int64; aArchival: Boolean): Boolean;
begin
  FreeAndNil(FRawResponce);
  Result:=APIMethod(RoteGetTransactions(aAddress, aLimit, aLT, aHash, aToLT, aArchival));
end;

end.

