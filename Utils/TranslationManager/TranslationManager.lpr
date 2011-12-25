program TranslationManager;

{$IFDEF FPC}
  {$Mode Delphi} {$H+}
{$ENDIF}

uses
  {$IFDEF FPC} Interfaces, {$ENDIF}
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$IFDEF WDC}
{$R *.res}
{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
