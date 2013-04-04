program playwin;

uses
  Forms,
  main in 'main.pas' {FormPlayWin};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormPlayWin, FormPlayWin);
  Application.Run;
end.
