unit testtonapi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, tonapi;

type

  { TTestTONAPI }

  TTestTONAPI= class(TTestCase)
  private
    FTONAPI: TTonAPI;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure getAddressInformation; 
    procedure getTransactions;
  end;

implementation

uses
  fpjson, config
  ;

procedure TTestTONAPI.SetUp;
begin
  FTONAPI:=TTonAPI.Create;
end;

procedure TTestTONAPI.TearDown;
var
  aResponce: TStringList;
begin
  aResponce:=TStringList.Create;
  try
    aResponce.Text:=FTONAPI.RawResponce.FormatJSON;
    aResponce.SaveToFile('~responce.json');
  finally                                  
    aResponce.Free;
    FTONAPI.Free;
  end;
end;

procedure TTestTONAPI.getAddressInformation;
var
  aResult: TgetAddressInformationResult;
begin
  aResult:=TgetAddressInformationResult.Create;
  try
    FTONAPI.getAddressInformation(_Config.Address, aResult);
    if not FTONAPI.ErrorDescription.IsEmpty then
      Fail('Error: '+FTONAPI.ErrorDescription);
  finally
    aResult.Free;
  end;
end;

procedure TTestTONAPI.getTransactions;
begin
  try
    FTONAPI.getTransactions(_Config.Address);
    if not FTONAPI.ErrorDescription.IsEmpty then
      Fail('Error: '+FTONAPI.ErrorDescription);
  finally
  end;
end;



initialization

  RegisterTest(TTestTONAPI);
end.

