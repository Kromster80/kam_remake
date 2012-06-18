unit Unit1;
interface
uses
  Windows, Classes, Controls, Forms, Math, StdCtrls, SysUtils,
  KM_Points, KM_Defaults, KM_CommonClasses, KromUtils,
  KM_GameApp, KM_Locales, KM_Log, KM_PlayersCollection, KM_TextLibrary, KM_Terrain, KM_Units_Warrior, KM_Utils;


type
  TForm1 = class(TForm)
    Button3: TButton;
    Button1: TButton;
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    procedure SetUp;
    procedure TearDown;
    procedure ControlsEnable(aFlag: Boolean);
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
  fGameApp := TKMGameApp.Create(0, 1024, 768, False, nil, nil, nil, True);
  fGameApp.GlobalSettings.Autosave := False;
end;


procedure TForm1.TearDown;
begin
  fGameApp.Stop(gr_Silent);
  FreeAndNil(fGameApp);
  FreeAndNil(fTextLibrary);
  FreeAndNil(fLocales);
  FreeAndNil(fLog);
end;


procedure TForm1.Button1Click(Sender: TObject);
var
  SearchRec: TSearchRec;
  NewName: string;
  B: Boolean;
begin
  begin
    FindFirst('..\..\SpriteResource\9\9*.png', faAnyFile, SearchRec);
    repeat
      NewName := SearchRec.Name;
      NewName := StringReplace(NewName, '9_00', '2_17', [rfReplaceAll, rfIgnoreCase]);
      NewName := StringReplace(NewName, '9_01', '2_18', [rfReplaceAll, rfIgnoreCase]);
      NewName := StringReplace(NewName, '9_02', '2_19', [rfReplaceAll, rfIgnoreCase]);
      NewName := StringReplace(NewName, '9_03', '2_20', [rfReplaceAll, rfIgnoreCase]);
      B := RenameFile(ExtractFilePath(ParamStr(0)) + '..\..\SpriteResource\9\' + SearchRec.Name,
                      ExtractFilePath(ParamStr(0)) + '..\..\SpriteResource\9\' + NewName);
      Assert(B);
    until (FindNext(SearchRec) <> 0);
    FindClose(SearchRec);
  end;
end;


procedure TForm1.Button3Click(Sender: TObject);
var
  I: Integer;
begin
  ControlsEnable(False);
  SetUp;

  for I := 0 to fGameApp.Campaigns.CampaignByTitle('TPR').MapCount - 1 do
  begin
    fGameApp.NewCampaignMap(fGameApp.Campaigns.CampaignByTitle('TPR'), I);

    fPlayers[0].Goals.ExportMessages(ExtractFilePath(ParamStr(0)) + Format('TPR%.2d.evt', [I+1]));

    fGameApp.Stop(gr_Silent);
  end;

  TearDown;
  ControlsEnable(True);
end;


end.
