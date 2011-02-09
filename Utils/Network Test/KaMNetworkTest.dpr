program KaMNetworkTest;

uses
  Forms,
  NetTest in 'NetTest.pas'; {frmNetTest}

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmNetTest, frmNetTest);
  Application.Run;
end.
