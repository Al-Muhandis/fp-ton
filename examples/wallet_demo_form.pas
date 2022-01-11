unit wallet_demo_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Spin;

type

  { TFrmMain }

  TFrmMain = class(TForm)
    BtnGet: TButton;
    EdtAddress: TLabeledEdit;
    EdtBalance: TLabeledEdit;
    EdtState: TLabeledEdit;
    EdtCode: TLabeledEdit;
    EdtData: TLabeledEdit;
    procedure BtnGetClick(Sender: TObject);
  private

  public

  end;

var
  FrmMain: TFrmMain;

implementation

uses
  tonapi
  ;

{$R *.lfm}

{ TFrmMain }

procedure TFrmMain.BtnGetClick(Sender: TObject);
var
  aResponce: TgetAddressInformationResult;
  aCode: Integer;
  aError: String;
begin
  aResponce:=TgetAddressInformationResult.Create;
  try
    TTonAPI.getAddressInformation(EdtAddress.Text, aResponce, aCode, aError);
    EdtBalance.Text:=(aResponce.balance/1e9).ToString;
    EdtCode.Text:=aResponce.code;
    EdtData.Text:=aResponce.data;
  finally
    aResponce.Free;
  end;
end;

end.

