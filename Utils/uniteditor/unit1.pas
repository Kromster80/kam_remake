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
    SpinEdit1: TSpinEdit;
    SpinEdit10: TSpinEdit;
    SpinEdit11: TSpinEdit;
    SpinEdit12: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    SpinEdit5: TSpinEdit;
    SpinEdit6: TSpinEdit;
    SpinEdit7: TSpinEdit;
    SpinEdit8: TSpinEdit;
    SpinEdit9: TSpinEdit;
    procedure init(Sender: TObject);
    procedure Label7Click(Sender: TObject);
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

procedure TForm1.Label7Click(Sender: TObject);
begin

end;


procedure TForm1.saveDAT(Sender: TObject);
begin
  UnitStat[ListBox1.ItemIndex].HitPoints := SpinEdit1.Value;
  UnitStat[ListBox1.ItemIndex].Attack := SpinEdit2.Value;
  UnitStat[ListBox1.ItemIndex].AttackHorseBonus := SpinEdit3.Value;
  UnitStat[ListBox1.ItemIndex].x4 := SpinEdit4.Value;
  UnitStat[ListBox1.ItemIndex].Defence := SpinEdit5.Value;
  UnitStat[ListBox1.ItemIndex].Speed := SpinEdit6.Value;
  UnitStat[ListBox1.ItemIndex].Sight := SpinEdit8.Value;
  UnitStat[ListBox1.ItemIndex].x9 := SpinEdit9.Value;
  UnitStat[ListBox1.ItemIndex].x10 := SpinEdit10.Value;
  UnitStat[ListBox1.ItemIndex].CanWalkOut := SpinEdit11.Value;
  UnitStat[ListBox1.ItemIndex].x11 := SpinEdit12.Value;
  if SaveDialog1.Execute then
  SaveUnitDAT(SaveDialog1.Filename);
end;


procedure TForm1.showDAT(Sender: TObject);
begin
   SpinEdit1.Value := UnitStat[ListBox1.ItemIndex].HitPoints;
   SpinEdit2.Value :=UnitStat[ListBox1.ItemIndex].Attack;
   SpinEdit3.Value :=UnitStat[ListBox1.ItemIndex].AttackHorseBonus;
   SpinEdit4.Value :=UnitStat[ListBox1.ItemIndex].x4;
   SpinEdit5.Value :=UnitStat[ListBox1.ItemIndex].Defence;
   SpinEdit6.Value :=UnitStat[ListBox1.ItemIndex].Speed;
   SpinEdit7.Value :=0;
   SpinEdit8.Value :=UnitStat[ListBox1.ItemIndex].Sight;
   SpinEdit9.Value :=UnitStat[ListBox1.ItemIndex].x9;
   SpinEdit10.Value :=UnitStat[ListBox1.ItemIndex].x10;
   SpinEdit11.Value :=UnitStat[ListBox1.ItemIndex].CanWalkOut;
   SpinEdit12.Value :=UnitStat[ListBox1.ItemIndex].x11;
end;


{$IFDEF FPC}
initialization
  {$I unit1.lrs}
{$ENDIF}

end.

