unit Unit1;
interface
uses
  SysUtils,
  Controls, Forms,
  KM_Points, KM_Defaults, KM_CommonClasses, Classes, KromUtils,
  KM_Game, KM_Locales, KM_Log, KM_PlayersCollection, KM_TextLibrary, KM_Terrain, KM_Units_Warrior, KM_Utils, Math,
  StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure Button3Click(Sender: TObject);
  private
    procedure SetUp;
    procedure TearDown;
    procedure ControlsEnable(aFlag: Boolean);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
{$R *.dfm}

procedure TForm1.ControlsEnable(aFlag: Boolean);
var I: Integer;
begin
  for I := 0 to ControlCount - 1 do
    if Controls[I] is TButton then
      Controls[I].Enabled := aFlag;
end;


procedure TForm1.SetUp;
begin
  SKIP_RENDER := True;
  SKIP_SOUND := True;
  ExeDir := ExtractFilePath(ParamStr(0)) + '..\..\';
  fLog := TKMLog.Create(ExtractFilePath(ParamStr(0)) + 'temp.log');
  fLocales := TKMLocales.Create(ExeDir+'data\locales.txt');
  fTextLibrary := TTextLibrary.Create(ExeDir + 'data\text\', 'eng');
  fGame := TKMGame.Create(0, 1024, 768, False, nil, nil, True);
  fGame.GlobalSettings.Autosave := False;
end;


procedure TForm1.TearDown;
begin
  fGame.Stop(gr_Silent);
  FreeAndNil(fGame);
  FreeAndNil(fTextLibrary);
  FreeAndNil(fLocales);
  FreeAndNil(fLog);
end;


procedure TForm1.Button3Click(Sender: TObject);
var
  I: Integer;
begin
  ControlsEnable(False);
  SetUp;

  for I := 0 to fGame.Campaigns.CampaignByTitle('TPR').MapCount - 1 do
  begin
    fGame.StartCampaignMap(fGame.Campaigns.CampaignByTitle('TPR'), I);

    fPlayers[0].Goals.ExportMessages(ExtractFilePath(ParamStr(0)) + Format('TPR%.2d.evt', [I+1]));

    fGame.Stop(gr_Silent);
  end;

  TearDown;
  ControlsEnable(True);
end;


end.
