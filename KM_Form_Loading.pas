unit KM_Form_Loading;
interface
uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Graphics,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, KromUtils;

type
  TFormLoading = class(TForm)
    Label1: TLabel;
    Bar1: TProgressBar;
    Image1: TImage;
    Label3: TLabel;
    Bevel1: TBevel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormLoading: TFormLoading;

implementation
uses KM_Unit1, KM_Render, KM_ReadGFX1, KM_Global_Data, KM_Log, KM_Defaults, KM_Terrain, KM_Viewport;
{$R *.dfm}

procedure TFormLoading.FormCreate(Sender: TObject);
begin
Form1.Hide;
ExeDir:=IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
  fLog:=TKMLog.Create(ExeDir+'KaM.log');

FormLoading.Show; FormLoading.Refresh;
FormLoading.Label1.Caption:='Initializing 3D';
FormLoading.Bar1.Position:=0; FormLoading.Refresh;

fRender.SetRender(Form1.Panel5.Handle);
fRender.SetRenderDefaults;

ReadGFX(ExeDir);

Form1.Show;
Form1.WindowState:=wsMaximized;
FormLoading.Hide;

Screen.Cursor:=c_Default;

//fTerrain.OpenMapFromFile('save01.map');
//  if ExtractOpenedFileName(cmdline)='' then
fTerrain.Create;
fTerrain.MakeNewMap(96,96);
fViewport.SetZoom(1);
Form1.FormResize(nil);
Form1.Caption:='KaM Editor - '+'New';
//    FormNewMap.InitializeNewMap(96,96);
//  if fileexists(ExeDir+'krom.dev') then
//    Form1.OpenMap('save01.map');
//if fileexists(ExeDir+'krom.dev') then Form1.OpenMap('test.map');
//  if fileexists(ExeDir+'krom.dev') then
//    LoadDAT('mission1.dat');

fLog.AppendLog('Form create is done');

end;

end.
