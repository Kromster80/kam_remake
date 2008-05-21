unit KM_Form_NewMap;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, KM_Unit1, ExtCtrls, KM_Terrain, KM_Houses, KM_Defaults;

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
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  var
  FormNewMap: TFormNewMap;

implementation

uses
  KM_Global_Data;

{$R *.dfm}

procedure TFormNewMap.CreateMap(Sender: TObject);
begin
Map.X:=RGX.ItemIndex*16+32; //0..10 -> 32..192
Map.Y:=RGY.ItemIndex*16+32; //0..10 -> 32..192
InitializeNewMap(Map.X,Map.Y);
FormNewMap.Close;
Form1.Enabled:=true;
end;

procedure TFormNewMap.InitializeNewMap(MapX,MapY:integer);
begin
fTerrain.Create;
fTerrain.MakeNewMap(96,96);
fMiniMap.ReSize(Map.X,Map.Y);
fViewport.SetZoom(1);
Form1.FormResize(nil);
Form1.Caption:='KaM Editor - '+'New';
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
begin Form1.Enabled:=true; end;

procedure TFormNewMap.ButtonHelpClick(Sender: TObject);
begin Application.HelpJump('CreatingNewMap'); end;



end.
