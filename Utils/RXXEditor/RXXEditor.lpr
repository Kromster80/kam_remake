program RXXEditor;
{$I RXXEditor.inc}
{$IFDEF FPC}
  {$Mode Delphi} {$H+}
{$ENDIF}

uses
  Forms,
  {$IFDEF FPC} Interfaces, {$ENDIF}
  RXXEditorForm in 'RXXEditorForm.pas' {RXXForm1};


{$IFDEF WDC}
{$R *.res}
{$ENDIF}


var
  RXXForm1: TRXXForm1;


begin
  Application.Initialize;
  Application.CreateForm(TRXXForm1, RXXForm1);
  Application.Run;
end.
