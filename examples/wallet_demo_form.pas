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
    MmResponce: TMemo;
    PgCntrl: TPageControl;
    TabSheet1: TTabSheet;
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
  aTON: TTonAPI;
begin
  EdtBalance.Text:=EmptyStr;
  EdtCode.Text:=EmptyStr;
  EdtData.Text:=EmptyStr;
  EdtError.Text:=EmptyStr;
  EdtErrorCode.Text:=EmptyStr;
  aResponce:=TgetAddressInformationResult.Create;
  aTON:=TTonAPI.Create;
  try
    if aTON.getAddressInformation(EdtAddress.Text, aResponce) then
    begin
      PgCntrl.ActivePage:=TbShtResult;
      EdtBalance.Text:=(aResponce.balance/1e9).ToString;
      EdtCode.Text:=aResponce.code;
      EdtData.Text:=aResponce.data;
      EdtState.Text:=aResponce.state;
    end
    else begin
      PgCntrl.ActivePage:=TbShtError;
      EdtError.Text:=aTON.ErrorDescription;
      EdtErrorCode.Text:=aTON.ErrorCode.ToString;
    end;
    MmResponce.Lines.Text:=aTON.RawResponce.FormatJSON();
  finally
    aTON.Free;
    aResponce.Free;
  end;
end;

end.

