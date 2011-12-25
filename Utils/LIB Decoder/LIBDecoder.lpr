program LIBDecoder;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, umain, uGoTo, ufind;
{$IFDEF Win32}
{$R *.RES}
{$ENDIF}

begin
  Application.Title:='LIB Decoder';
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmGoTo, frmGoTo);
  Application.CreateForm(TfrmFind, frmFind);
  Application.Run;
end.

