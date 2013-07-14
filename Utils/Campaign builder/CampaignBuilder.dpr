program CampaignBuilder;
{$I ..\..\KaM_Remake.inc}
uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  KM_Campaigns in '..\..\KM_Campaigns.pas',
  KM_ResSprites in '..\..\KM_ResSprites.pas',
  KM_ResSpritesEdit in '..\..\KM_ResSpritesEdit.pas';


{$R *.res}


begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
