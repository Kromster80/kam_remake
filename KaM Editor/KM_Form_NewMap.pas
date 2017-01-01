unit KM_Form_NewMap;
{$I ..\KaM_Remake.inc}
interface

uses
  {$IFDEF FPC} LResources, {$ENDIF}
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} FileUtil, LCLIntf, LCLType, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, KM_Unit1, ExtCtrls, Math;

type
  TFormNewMap = class(TForm)
    ButtonOK: TButton;
    ButtonCancel: TButton;
    RGMapVersion: TRadioGroup;
    GrMapSize: TGroupBox;
    RGX: TComboBox;
    RGY: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    ButtonHelp: TButton;
    procedure CreateMap(Sender: TObject);
    procedure InitializeNewMap(MapX,MapY:integer);
    procedure CreateCancel(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;


var
  FormNewMap: TFormNewMap;

implementation
{$IFDEF WDC} {$R *.dfm} {$ENDIF}

uses KM_Defaults;

procedure TFormNewMap.CreateMap(Sender: TObject);
begin
  if not (InRange(MapSizes[RGX.ItemIndex+1],32,192) and InRange(MapSizes[RGY.ItemIndex+1],32,192)) then
    MessageBox(FormNewMap.Handle,'KaM won''t be able to handle such a big map. It can be used only as reference.','Warning',MB_OK or MB_ICONWARNING{$IFDEF MSWindows} or MB_APPLMODAL{$ENDIF});
  Map.X:=MapSizes[RGX.ItemIndex+1]; //0..10 -> 32..192
  Map.Y:=MapSizes[RGY.ItemIndex+1]; //0..10 -> 32..192
  InitializeNewMap(Map.X,Map.Y);
  FormNewMap.Close;
  Form1.Enabled:=true;
end;

procedure TFormNewMap.InitializeNewMap(MapX,MapY:integer);
var i,k:integer;
begin
  Map.X:=MapX;
  Map.Y:=MapY;

  for i:=1 to Map.Y do
  for k:=1 to Map.X do
  with Land[i,k] do
  begin
    Land2[i, k].TerType := 1;
    Terrain := 0;
    Light := 16;
    Height1 := random(4); //small variation in height
    Rot := random(4);
    x3 := 0; //unknown ~20, 20 at edges
    Obj := 255; //none
    Pass := 255; //allow anything
    c1 := 255;
    c2 := 255;
    c3 := 255;
    c4 := 255;
    y1 := 0;
    y2 := 0;
    y3 := 0;
    Border := 255; //
    y5 := 255;
    y6 := 205;
    y7 := 0;
    y8 := 0;
    y9 := 0;
    y10 := 0;
    y11 := 0;
    y12 := 0;
  end;

  for k := 1 to Map.X do
    Land[Map.Y, k].Height1 := 0;

  ResHead.x1 := 0;
  ResHead.Allocated := Map.X + Map.Y;
  ResHead.Qty1 := 0;
  ResHead.Qty2 := ResHead.Qty1;
  if ResHead.Qty1 > 0 then
    ResHead.x5 := ResHead.Qty1 - 1
  else
    ResHead.x5 := 0;
  ResHead.Len17 := 17;

  for i := 1 to ResHead.Allocated do
  begin
    Res[i].x1 := -842150451;
    Res[i].y1 := -842150451;
    Res[i].X2 := -842150451;
    Res[i].y2 := -842150451;
    Res[i].Typ := 255;
  end;

  AnyChangesMade := true;

  Form1.PrepareLoadedMap('New Map');
end;

procedure TFormNewMap.CreateCancel(Sender: TObject);
begin
  FormNewMap.Close;
end;


procedure TFormNewMap.FormShow(Sender: TObject);
begin Form1.Enabled:=false;
//Form2.SetFocus;
end;


procedure TFormNewMap.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Form1.Enabled := true;
end;


procedure TFormNewMap.ButtonHelpClick(Sender: TObject);
begin
  {$IFDEF WDC} Application.HelpJump('CreatingNewMap'); {$ENDIF}
end;


procedure TFormNewMap.FormCreate(Sender: TObject);
var i:integer;
begin
  RGX.Clear;
  RGY.Clear;
  for i:= 1 to length(MapSizes) do RGX.Items.Add(IntToStr(MapSizes[i]));
  for i:= 1 to length(MapSizes) do RGY.Items.Add(IntToStr(MapSizes[i]));
  RGX.ItemIndex:=6;
  RGY.ItemIndex:=6;
end;


{$IFDEF FPC}
initialization
  {$I KM_Form_NewMap.lrs}
{$ENDIF}


end.
