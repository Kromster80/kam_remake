program RXXPacker;

uses
  Forms,
  RXXPackerForm in 'RXXPackerForm.pas' {RXXForm1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TRXXForm1, RXXForm1);
  Application.Run;
end.
