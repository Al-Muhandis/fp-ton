unit testtonapi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  TTestTONAPI= class(TTestCase)
  published
    procedure getAddressInformation;
  end;

implementation

uses
  tonapi
  ;

procedure TTestTONAPI.getAddressInformation;
var
  aResult: TgetAddressInformationResult;
  aCode: Integer;
  aError: String;
begin
  aResult:=TgetAddressInformationResult.Create;
  try
    TTonAPI.getAddressInformation('0QCyt4ltzak71h6XkyK4ePfZCzJQDSVUNuvZ3VE7hP_Q-GTE', aResult, aCode, aError);
    if not aError.IsEmpty then
      Fail('Error: '+aError);
  finally      
    aResult.Free;
  end;
end;



initialization

  RegisterTest(TTestTONAPI);
end.

