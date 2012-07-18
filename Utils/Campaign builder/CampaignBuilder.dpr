program CampaignBuilder;
{$I ..\..\KaM_Remake.inc}
uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  KM_Campaigns in '..\..\KM_Campaigns.pas',
  KM_ResourceSprites in '..\..\KM_ResourceSprites.pas',
  KM_ResourceSpritesEdit in '..\..\KM_ResourceSpritesEdit.pas';


{$R *.res}


begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
