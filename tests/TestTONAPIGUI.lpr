program TestTONAPIGUI;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, testtonapi, tonapi;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

