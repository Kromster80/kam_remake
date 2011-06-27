unit Unit1;
{$I uniteditor.inc}
{$IFDEF FPC}
  {$Mode Delphi} {$H+}
{$ENDIF}
interface
uses
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls,
  Menus, Spin
  {$IFDEF FPC}, LResources{$ENDIF};


const
 UnitNames: array[1..41] of string = (
 'Serf', 'Woodcutter', 'Miner', 'Animal Breeder',
 'Farmer', 'Lamberjack', 'Baker', 'Butcher', 'Fisher', 'Worker', 'StoneCutter',
 'Smith', 'Metallurgist', 'Recruit', 'Militia', 'AxeFighter', 'Swordsman',
 'Bowman', 'Arbaletman', 'Pikeman', 'Hallebardman', 'HorseScout', 'Cavalry',
 'Barbarian', 'Peasant', 'Slingshot', 'MetalBarbarian', 'Horseman', 'Catapult',
 'Ballista', 'Wolf', 'Fish', 'Watersnake', 'Seastar', 'Crab', 'Waterflower',
 'Waterleaf', 'Duck', 'Unknown', 'Unknown', 'Unknown');


type
  TForm1 = class(TForm)
    Button1: TButton;
    Defence: TLabel;
    ListBox1: TListBox;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Label10: TLabel;
    Label9: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label6: TLabel;
    label5: TLabel;
    Speed: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Open: TButton;
    HitPoints: TSpinEdit;
    x10: TSpinEdit;
    CanWalkOut: TSpinEdit;
    x11: TSpinEdit;
    Attack: TSpinEdit;
    AttackHorseBonus: TSpinEdit;
    x4: TSpinEdit;
    DefenceSpinEdit: TSpinEdit;
    SpeedSpinEdit: TSpinEdit;
    x7: TSpinEdit;
    Sight: TSpinEdit;
    x9: TSpinEdit;
    procedure ChangeSpinEdits(Sender: TObject);
    procedure open_file(Sender: TObject);
    procedure saveDAT(Sender: TObject);
    procedure showDAT(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

  TAppState = ( asWait=0, asView=1 );

var
  Form1: TForm1;
  AppState : TAppState;

  UnitStat:array[1..41]of record
    HitPoints,Attack,AttackHorseBonus,x4,Defence,Speed,label5,Sight:smallint;
    x9,x10:shortint;
    CanWalkOut,x11:smallint;
  end;
  SerfCarry:array[1..28] of packed record
    Dir:array[1..8]of packed record
      Step:array[1..30]of smallint;
      Count:smallint;
      MoveX,MoveY:integer;
    end;
  end;
  UnitSprite:array[1..41]of packed record
    Act:array[1..14]of packed record
      Dir:array[1..8]of packed record
        Step:array[1..30]of smallint;
        Count:smallint;
        MoveX,MoveY:integer;
      end;
    end;
  end;
  UnitSprite2:array[1..41,1..18]of smallint; //Sound indices vs sprite ID
  ExeDir:string;

implementation
{$IFDEF WDC}
  {$R *.dfm}
{$ENDIF}
procedure TForm1.FormCreate(Sender: TObject);
var i : Integer;
begin
  for i:=Low(UnitNames) to High(UnitNames) do
    ListBox1.Items.Add(UnitNames[i]);
  AppState := asWait;
end;

function LoadUnitDAT(FileName:string):boolean;
var
  i:integer;
  f:file;
begin
  Result := false;

  if not FileExists(FileName) then exit;
  assignfile(f,FileName); reset(f,1);

  for i:=1 to 28 do
    blockread(f,SerfCarry[i],8*70);

  for i:=1 to 41 do
  begin
    blockread(f,UnitStat[i],22);
    blockread(f,UnitSprite[i],112*70);
    blockread(f,UnitSprite2[i],36);
  end;
  closefile(f);

  Result := true;
end;


function SaveUnitDAT(FileName:string):boolean;
var
  i:integer;
  f:file;
begin
  Result := false;
  AssignFile(f,FileName);
  Reset(f,1);

  Seek(f, 15680); // 15680 = 28 * (8 * 70)
  {for i:=1 to 28 do
    blockwrite(f,SerfCarry[i],8*70);}

  for i:=1 to 41 do
  begin
    blockwrite(f,UnitStat[i],22);
    blockwrite(f,UnitSprite[i],112*70);
    blockwrite(f,UnitSprite2[i],36);
  end;
  closefile(f);

  Result := true;
end;


procedure TForm1.open_file(Sender: TObject);
begin
  if DirectoryExists('..\..\Data\Defines\') then
    OpenDialog1.InitialDir := '..\..\Data\Defines\'
  else
    OpenDialog1.InitialDir := '';

  if OpenDialog1.Execute then
  begin
    LoadUnitDAT(OpenDialog1.Filename);
    SaveDialog1.InitialDir := OpenDialog1.GetNamePath;
  end;
  AppState := asWait;
end;

procedure TForm1.ChangeSpinEdits(Sender: TObject);
var ID:integer;
begin
  if AppState <> asWait then exit;
   ID := ListBox1.ItemIndex + 1;
   if ID = 0 then Exit;

  UnitStat[ID].HitPoints        := HitPoints.Value;
  UnitStat[ID].Attack           := Attack.Value;
  UnitStat[ID].AttackHorseBonus := AttackHorseBonus.Value;
  UnitStat[ID].x4               := x4.Value;
  UnitStat[ID].Defence          := DefenceSpinEdit.Value;
  UnitStat[ID].Speed            := SpeedSpinEdit.Value;
  UnitStat[ID].Sight            := Sight.Value;
  UnitStat[ID].x9               := x9.Value;
  UnitStat[ID].x10              := x10.Value;
  UnitStat[ID].CanWalkOut       := CanWalkOut.Value;
  UnitStat[ID].x11              := x11.Value;
end;


procedure TForm1.saveDAT(Sender: TObject);
begin
  if SaveDialog1.Execute then
    SaveUnitDAT(SaveDialog1.Filename);
end;


procedure TForm1.showDAT(Sender: TObject);
var ID:integer;
begin
   ID := ListBox1.ItemIndex + 1;
   if ID = 0 then Exit;

   AppState := asView;
   HitPoints.Value        := UnitStat[ID].HitPoints;
   Attack.Value           := UnitStat[ID].Attack;
   AttackHorseBonus.Value := UnitStat[ID].AttackHorseBonus;
   x4.Value               := UnitStat[ID].x4;
   DefenceSpinEdit.Value  := UnitStat[ID].Defence;
   SpeedSpinEdit.Value    := UnitStat[ID].Speed;
   x7.Value               := 0;
   Sight.Value            := UnitStat[ID].Sight;
   x9.Value               := UnitStat[ID].x9;
   x10.Value              := UnitStat[ID].x10;
   CanWalkOut.Value       := UnitStat[ID].CanWalkOut;
   x11.Value              := UnitStat[ID].x11;
   AppState := asWait;
end;


{$IFDEF FPC}

initialization
  {$I unit1.lrs}
{$ENDIF}

end.

