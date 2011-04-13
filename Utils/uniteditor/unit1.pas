unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, KromUtils;

type
   TUnitType = ( ut_None=0,
    ut_Serf=1,          ut_Woodcutter=2,    ut_Miner=3,         ut_AnimalBreeder=4,
    ut_Farmer=5,        ut_Lamberjack=6,    ut_Baker=7,         ut_Butcher=8,
    ut_Fisher=9,        ut_Worker=10,       ut_StoneCutter=11,  ut_Smith=12,
    ut_Metallurgist=13, ut_Recruit=14,

    ut_Militia=15,      ut_AxeFighter=16,   ut_Swordsman=17,    ut_Bowman=18,
    ut_Arbaletman=19,   ut_Pikeman=20,      ut_Hallebardman=21, ut_HorseScout=22,
    ut_Cavalry=23,      ut_Barbarian=24,

    ut_Peasant=25,    ut_Slingshot=26,    ut_MetalBarbarian=27,ut_Horseman=28,
    ut_Catapult=29,   ut_Ballista=30,

    ut_Wolf=31,         ut_Fish=32,         ut_Watersnake=33,   ut_Seastar=34,
    ut_Crab=35,         ut_Waterflower=36,  ut_Waterleaf=37,    ut_Duck=38,
    ut_39=39,
    ut_Any=40);

const
 siUnitNames = 69;
 UnitNames: array[ut_Serf..ut_Any] of string = ('None', 'Serf', 'Woodcutter',
 'Miner', 'Animal Breeder',
 'Farmer', 'Lamberjack', 'Baker', 'Butcher', 'Fisher', 'Worker', 'StoneCutter',
 'Smith', 'Metallurgist', 'Recruit', 'Militia', 'AxeFighter', 'Swordsman',
 'Bowman', 'Arbaletman', 'Pikeman', 'Hallebardman', 'HorseScout', 'Cavalry',
 'Barbarian', 'Peasant', 'Slingshot', 'MetalBarbarian', 'Horseman', 'Catapult',
 'Ballista', 'Wolf', 'Fish', 'Watersnake', 'Seastar', 'Crab', 'Waterflower',
 'Duck', 'x', 'Any');

  { TForm1 }
type

  TForm1 = class(TForm)
    ATK: TEdit;
    AHB: TEdit;
    Defence: TLabel;
    DEF: TEdit;
    CVA: TEdit;
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
    uname : TMainMenu;
    unit_name: TEdit;
    SolderName: TLabel;
    Open: TButton;
    File_name: TEdit;
  procedure FormActivate(Sender: TObject);
  procedure open_file(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1;
  WriteResourceInfoToTXT :Boolean;
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
  //fTextLibrary:TTextLibrary;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormActivate(Sender: TObject);
begin
end;

function LoadUnitDAT(FileName:string):boolean;
var
  ii:integer;
// kk,jj,hh:integer;
// ft:textfile;
  f:file;
begin
  Result := false;

  if not CheckFileExists(FileName) then exit;
  assignfile(f,FileName); reset(f,1);

  for ii:=1 to 28 do
    blockread(f,SerfCarry[ii],8*70);

  for ii:=1 to 41 do begin
    blockread(f,UnitStat[ii],22);
    blockread(f,UnitSprite[ii],112*70);
    blockread(f,UnitSprite2[ii],36);
  end;

  closefile(f);

//  if WriteResourceInfoToTXT then begin
//    assignfile(ft,ExeDir+'UnitDAT.csv'); rewrite(ft);
//    writeln(ft,'Name;x1;Attack;AttackHorseBonus;x4;HitPoints;Speed;x7;Sight;x9;x10;CanWalkOut;0;');
//   for ii:=1 to 40 do begin
//      write(ft,fTextLibrary.GetTextString(siUnitNames+ii)+';');
//      write(ft,inttostr(UnitStat[ii].HitPoints)+';');
//      write(ft,inttostr(UnitStat[ii].Attack)+';');
//      write(ft,inttostr(UnitStat[ii].AttackHorseBonus)+';');
//     write(ft,inttostr(UnitStat[ii].x4)+';');
//      write(ft,inttostr(UnitStat[ii].Defence)+';');
//      write(ft,inttostr(UnitStat[ii].Speed)+';');
//      write(ft,inttostr(UnitStat[ii].x7)+';');
//      write(ft,inttostr(UnitStat[ii].Sight)+';');
//      write(ft,inttostr(UnitStat[ii].x9)+';');
//      write(ft,inttostr(UnitStat[ii].x10)+';');
//      write(ft,inttostr(UnitStat[ii].CanWalkOut)+';');
//      write(ft,inttostr(UnitStat[ii].x11)+';');
//      for kk:=1 to 18 do
//        write(ft,inttostr(UnitSprite2[ii,kk])+';');
//      writeln(ft);
//    end;
//    closefile(ft);
//
//    assignfile(ft,ExeDir+'Units.txt'); rewrite(ft);
//    for ii:=1 to 40 do begin
//    writeln(ft);
//    writeln(ft);
//    writeln(ft,'NewUnit'+inttostr(ii));
//    for kk:=1 to 14 do
//    for hh:=1 to 8 do
//    //  if UnitSprite[ii].Act[kk].Dir[hh].Step[1]>0 then
//       begin
//         write(ft,inttostr(kk)+'.'+inttostr(hh)+#9);
//          for jj:=1 to 30 do
//          if UnitSprite[ii].Act[kk].Dir[hh].Step[jj]>0 then //write(ft,'#');
//          write(ft,inttostr(UnitSprite[ii].Act[kk].Dir[hh].Step[jj])+'. ');
//          write(ft,inttostr(UnitSprite[ii].Act[kk].Dir[hh].Count)+' ');
//          write(ft,inttostr(UnitSprite[ii].Act[kk].Dir[hh].MoveX)+' ');
//          write(ft,inttostr(UnitSprite[ii].Act[kk].Dir[hh].MoveY)+' ');
//          writeln(ft);
//        end;
//    end;
//    closefile(ft);
//  end;
Result:=true;
end;

procedure TForm1.open_file(Sender: TObject);
var
  i :integer;
begin
  //default location
  File_name.Text := '../../Data/Defines/Unit.dat';

  LoadUnitDAT(File_name.Text);
  i := strtoint(number.Text);
  unit_name.Text := UnitNames[TUnitType(i)];
  HP.Text := inttostr(UnitStat[i].HitPoints);
  ATK.Text := inttostr(UnitStat[i].Attack);
  AHB.Text := inttostr(UnitStat[i].AttackHorseBonus);
  x4.Text := inttostr(UnitStat[i].x4);
  DEF.Text := inttostr(UnitStat[i].Defence);
  SPD.Text := inttostr(UnitStat[i].Speed);
 // x7.Text := inttostr(UnitStat[i].x7);
  Sight.Text := inttostr(UnitStat[i].Sight);
  x9.Text := inttostr(UnitStat[i].x9);
  x10.Text := inttostr(UnitStat[i].x10);
  CVA.Text := inttostr(UnitStat[i].CanWalkOut);
  x11.Text := inttostr(UnitStat[i].x11);
end;

end.

