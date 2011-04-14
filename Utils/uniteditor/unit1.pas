unit Unit1;
{$I uniteditor.inc}
{$IFDEF FPC}
  {$Mode Delphi} {$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus //, KromUtils
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
    ATK: TEdit;
    AHB: TEdit;
    Button1: TButton;
    Defence: TLabel;
    DEF: TEdit;
    CVA: TEdit;
    Label11: TLabel;
    x11: TEdit;
    Label10: TLabel;
    Label9: TLabel;
    x10: TEdit;
    x9: TEdit;
    Label7: TLabel;
    Label8: TLabel;
    Sight: TEdit;
    Label6: TLabel;
    x7: TEdit;
    label5: TLabel;
    SPD: TEdit;
    Speed: TLabel;
    x4: TEdit;
    HP: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    number: TEdit;
    SName: TLabel;
    UnitNumber: TLabel;
    Open: TButton;
    File_name: TEdit;
    procedure init(Sender: TObject);
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

  //todo: Add file-save dialog

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
  //todo: Add file-open dialog
  LoadUnitDAT(File_name.Text);

  //todo: Replace UnitNumber field with TListBox
end;


procedure TForm1.init(Sender: TObject);
begin
  //default location
  File_name.Text := '../../Data/Defines/Unit.dat';
end;


procedure TForm1.saveDAT(Sender: TObject);
var
  i :integer;
begin
  i := strtoint(number.Text);
  UnitStat[i].HitPoints := strtoint(HP.Text);
  UnitStat[i].Attack := strtoint(ATK.Text);
  UnitStat[i].AttackHorseBonus := strtoint(AHB.Text);
  UnitStat[i].x4 := strtoint(x4.Text);
  UnitStat[i].Defence := strtoint(DEF.Text);
  UnitStat[i].Speed := strtoint(SPD.Text);
  UnitStat[i].Sight := strtoint(Sight.Text);
  UnitStat[i].x9 := strtoint(x9.Text);
  UnitStat[i].x10 := strtoint(x10.Text);
  UnitStat[i].CanWalkOut := strtoint(CVA.Text);
  UnitStat[i].x11 := strtoint(x11.Text);

  SaveUnitDAT(File_name.Text);
end;


procedure TForm1.showDAT(Sender: TObject);
var
  i :integer;
begin
  //todo: Replace all TEdits with TSpinEdits
  i := strtoint(number.Text);
  if (i<42) then begin
   Label11.Caption := UnitNames[i];
   HP.Text := inttostr(UnitStat[i].HitPoints);
   ATK.Text := inttostr(UnitStat[i].Attack);
   AHB.Text := inttostr(UnitStat[i].AttackHorseBonus);
   x4.Text := inttostr(UnitStat[i].x4);
   DEF.Text := inttostr(UnitStat[i].Defence);
   SPD.Text := inttostr(UnitStat[i].Speed);
   x7.Text := '0'; // inttostr(UnitStat[i].x7);
   Sight.Text := inttostr(UnitStat[i].Sight);
   x9.Text := inttostr(UnitStat[i].x9);
   x10.Text := inttostr(UnitStat[i].x10);
   CVA.Text := inttostr(UnitStat[i].CanWalkOut);
   x11.Text := inttostr(UnitStat[i].x11);
  end
  else ShowMessage ('wrong unit number');
end;


{$IFDEF FPC}
initialization
  {$I unit1.lrs}
{$ENDIF}

end.

