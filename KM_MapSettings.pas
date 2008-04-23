unit KM_MapSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, CheckLst;

type
  TForm_MapSettings = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    EditMapFile: TEdit;
    Button1: TButton;
    Label1: TLabel;
    RGHumanPlayer: TRadioGroup;
    TabSheet2: TTabSheet;
    CLBReleaseHouse: TCheckListBox;
    CLBBlockHouse: TCheckListBox;
    Label2: TLabel;
    Label3: TLabel;
    procedure EditMapFileChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form_MapSettings: TForm_MapSettings;

  SettingsUpdate:boolean=false;

implementation

uses KM_Unit1, KM_Defaults, KM_Global_Data;

{$R *.dfm}

procedure TForm_MapSettings.FormShow(Sender: TObject);
var i:integer;
begin
Form1.Enabled:=false;
SettingsUpdate:=true;
EditMapFile.Text:=Mission.SetMapFile;
RGHumanPlayer.ItemIndex:=Mission.SetHumanPlayer-1;
CLBReleaseHouse.Clear;
for i:=1 to 29 do CLBReleaseHouse.Items.Add(HouseName[i]);
for i:=1 to 29 do CLBReleaseHouse.Checked[i-1]:=Mission.Player[Mission.ActivePlayer].ReleaseHouse[i];
CLBBlockHouse.Clear;
for i:=1 to 29 do CLBBlockHouse.Items.Add(HouseName[i]);
for i:=1 to 29 do CLBBlockHouse.Checked[i-1]:=Mission.Player[Mission.ActivePlayer].BlockHouse[i];
SettingsUpdate:=false;
end;

procedure TForm_MapSettings.EditMapFileChange(Sender: TObject);
var i:integer;
begin
if SettingsUpdate then exit;
Mission.SetMapFile:=EditMapFile.Text;
Mission.SetHumanPlayer:=RGHumanPlayer.ItemIndex+1;

for i:=1 to 29 do begin
Mission.Player[Mission.ActivePlayer].BlockHouse[i]:=CLBBlockHouse.Checked[i-1];
CLBReleaseHouse.ItemEnabled[i-1]:=not CLBBlockHouse.Checked[i-1];
CLBReleaseHouse.Checked[i-1]:=CLBReleaseHouse.Checked[i-1] and not CLBBlockHouse.Checked[i-1];
end;
for i:=1 to 29 do Mission.Player[Mission.ActivePlayer].ReleaseHouse[i]:=CLBReleaseHouse.Checked[i-1];
end;

procedure TForm_MapSettings.Button1Click(Sender: TObject);
begin
Form_MapSettings.Close;
end;

procedure TForm_MapSettings.FormClose(Sender: TObject; var Action: TCloseAction);
begin
Form1.Enabled:=true;
end;

end.
