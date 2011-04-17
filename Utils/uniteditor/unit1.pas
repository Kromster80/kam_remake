unit Unit1;
{$I uniteditor.inc}
{$IFDEF FPC}
  {$Mode Delphi} {$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus
  {$IFDEF FPC}, LResources, Spin{$ENDIF};


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

  { TForm1 }

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
    procedure init(Sender: TObject);
    procedure ChangeSpinEdits(Sender: TObject);
    procedure open_file(Sender: TObject);
    procedure saveDAT(Sender: TObject);
    procedure showDAT(Sender: TObject);
  end;

var
  Form1: TForm1;

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


function LoadUnitDAT(FileName:string):boolean;
var
  ii:integer;
  f:file;
begin
  Result := false;

  if not FileExists(FileName) then exit;
  assignfile(f,FileName); reset(f,1);

  for ii:=1 to 28 do
    blockread(f,SerfCarry[ii],8*70);

  for ii:=1 to 41 do begin
    blockread(f,UnitStat[ii],22);
    blockread(f,UnitSprite[ii],112*70);
    blockread(f,UnitSprite2[ii],36);
  end;
  closefile(f);

  Result := true;
end;


function SaveUnitDAT(FileName:string):boolean;
var
  ii:integer;
  f:file;
begin
  Result := false;
  assignfile(f,FileName); reset(f,1);

  for ii:=1 to 28 do
    blockread(f,SerfCarry[ii],8*70);

  for ii:=1 to 41 do begin
    blockwrite(f,UnitStat[ii],22);
    blockwrite(f,UnitSprite[ii],112*70);
    blockwrite(f,UnitSprite2[ii],36);
  end;
  closefile(f);

  Result := true;
end;


procedure TForm1.open_file(Sender: TObject);
begin
  if DirectoryExists('../../Data/Defines/') then
  SaveDialog1.InitialDir := '../../Data/Defines/'
  else
  SaveDialog1.InitialDir := '';
  if OpenDialog1.Execute then
  LoadUnitDAT(OpenDialog1.Filename);
end;

procedure TForm1.init(Sender: TObject);
var
  x :integer;
begin
x := 1;
 with ListBox1 do begin
     while (x <42) do begin
      Items.Add(UnitNames[x]);
      x := x + 1;
     end;
 end;
end;

procedure TForm1.ChangeSpinEdits(Sender: TObject);
begin
  UnitStat[ListBox1.ItemIndex].HitPoints := HitPoints.Value;
  UnitStat[ListBox1.ItemIndex].Attack := Attack.Value;
  UnitStat[ListBox1.ItemIndex].AttackHorseBonus := AttackHorseBonus.Value;
  UnitStat[ListBox1.ItemIndex].x4 := x4.Value;
  UnitStat[ListBox1.ItemIndex].Defence := DefenceSpinEdit.Value;
  UnitStat[ListBox1.ItemIndex].Speed := SpeedSpinEdit.Value;
  UnitStat[ListBox1.ItemIndex].Sight := Sight.Value;
  UnitStat[ListBox1.ItemIndex].x9 := x9.Value;
  UnitStat[ListBox1.ItemIndex].x10 := x10.Value;
  UnitStat[ListBox1.ItemIndex].CanWalkOut := CanWalkOut.Value;
  UnitStat[ListBox1.ItemIndex].x11 := x11.Value;
end;


procedure TForm1.saveDAT(Sender: TObject);
begin
  if SaveDialog1.Execute then
  SaveUnitDAT(SaveDialog1.Filename);
end;


procedure TForm1.showDAT(Sender: TObject);
begin
   HitPoints.Value := UnitStat[ListBox1.ItemIndex].HitPoints;
   Attack.Value :=UnitStat[ListBox1.ItemIndex].Attack;
   AttackHorseBonus.Value :=UnitStat[ListBox1.ItemIndex].AttackHorseBonus;
   x4.Value :=UnitStat[ListBox1.ItemIndex].x4;
   DefenceSpinEdit.Value :=UnitStat[ListBox1.ItemIndex].Defence;
   SpeedSpinEdit.Value :=UnitStat[ListBox1.ItemIndex].Speed;
   x7.Value :=0;
   Sight.Value :=UnitStat[ListBox1.ItemIndex].Sight;
   x9.Value :=UnitStat[ListBox1.ItemIndex].x9;
   x10.Value :=UnitStat[ListBox1.ItemIndex].x10;
   CanWalkOut.Value :=UnitStat[ListBox1.ItemIndex].CanWalkOut;
   x11.Value :=UnitStat[ListBox1.ItemIndex].x11;
end;


{$IFDEF FPC}
initialization
  {$I unit1.lrs}
{$ENDIF}

end.

