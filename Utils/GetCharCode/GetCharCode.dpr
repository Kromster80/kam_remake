program GetCharCode;

uses
  Vcl.Forms,
  CharCode_Form in 'CharCode_Form.pas' {CharCode};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TCharCode, CharCode);
  Application.Run;
end.
