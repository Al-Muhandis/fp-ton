unit wallet_demo_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, IniPropStorage, ComCtrls
  ;

type

  { TFrmMain }

  TFrmMain = class(TForm)
    BtnGet: TButton;
    EdtAddress: TLabeledEdit;
    EdtBalance: TLabeledEdit;
    EdtErrorCode: TLabeledEdit;
    EdtError: TLabeledEdit;
    EdtState: TLabeledEdit;
    EdtCode: TLabeledEdit;
    EdtData: TLabeledEdit;
    IniPrpStrg: TIniPropStorage;
    PgCntrl: TPageControl;
    TbShtError: TTabSheet;
    TbShtResult: TTabSheet;
    procedure BtnGetClick({%H-}Sender: TObject);
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
  EdtBalance.Text:=EmptyStr;
  EdtCode.Text:=EmptyStr;
  EdtData.Text:=EmptyStr;
  EdtError.Text:=EmptyStr;
  EdtErrorCode.Text:=EmptyStr;
  aResponce:=TgetAddressInformationResult.Create;
  try
    if TTonAPI.getAddressInformation(EdtAddress.Text, aResponce, aCode, aError) then
    begin
      PgCntrl.ActivePage:=TbShtResult;
      EdtBalance.Text:=(aResponce.balance/1e9).ToString;
      EdtCode.Text:=aResponce.code;
      EdtData.Text:=aResponce.data;
      EdtState.Text:=aResponce.state;
    end
    else begin
      PgCntrl.ActivePage:=TbShtError;
      EdtError.Text:=aError;
      EdtErrorCode.Text:=aCode.ToString;
    end;
  finally
    aResponce.Free;
  end;
end;

end.

