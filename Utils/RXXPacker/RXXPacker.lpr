program RXXPacker;
{$I RXXPacker.inc}
{$IFDEF FPC}
  {$Mode Delphi} {$H+}
{$ENDIF}

uses
  Forms,
  {$IFDEF FPC} Interfaces, {$ENDIF}
  RXXPackerForm in 'RXXPackerForm.pas' {RXXForm1};


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
