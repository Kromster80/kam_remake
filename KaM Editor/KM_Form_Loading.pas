unit KM_Form_Loading;
{$I ..\KaM_Remake.inc}
interface

uses
  SysUtils, Classes, Controls, Forms, Graphics,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, KromOGLUtils,
  {$IFDEF FPC} GL, LResources, {$ENDIF}
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  dglOpenGL, KromUtils;

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
    procedure FormCreate(Sender: TObject);
    procedure Label7Click(Sender: TObject);
    procedure Label4Click(Sender: TObject);
  end;


var
  FormLoading: TFormLoading;


implementation
uses
  KM_Unit1, KM_ReadGFX1, KM_Form_NewMap, KM_LoadDAT;

{$IFDEF WDC} {$R *.dfm} {$ENDIF}

procedure TFormLoading.FormCreate(Sender: TObject);
var
  InputParam: string;
begin
  Form1.Hide;
  ExeDir := ExtractFilePath(ParamStr(0));

  Show;
  Refresh;
  Label1.Caption:='Initializing 3D';
  Bar1.Position:=0;
  Refresh;

  SetRenderFrame(Form1.Panel1.Handle, h_DC, h_RC);
  Form1.RenderInit;

  BuildFont(h_DC, 16);
  {$IFDEF WDC}FormatSettings.{$ENDIF}DecimalSeparator := '.';

  if ReadGFX(ExeDir) then
  begin
    MakeObjectsGFX(nil);
    MakeHousesGFX(nil);
  end
  else
  begin
    MessageBox(FormLoading.Handle, 'Objects tab is disabled', 'Warning', MB_OK or MB_ICONWARNING);
    Form1.ObjBlock.Enabled := false;
    Form1.ObjErase.Enabled := false;
    Form1.ObjPallete.Enabled := false;
    Form1.ObjPalleteScroll.Enabled := false;
  end;

  Hide;
  DoClientAreaResize(Form1);
  Form1.Show;
  Form1.WindowState := wsMaximized;

  InputParam:=ExtractOpenedFileName(cmdline);
  if FileExists(InputParam) then
    if GetFileExt(InputParam)='MAP' then
      Form1.OpenMap(InputParam)
    else
      if GetFileExt(InputParam)='PRO' then
        Form1.OpenPro(InputParam)
      else
        FormNewMap.InitializeNewMap(96,96)
  else
    FormNewMap.InitializeNewMap(96,96);

  //Form1.OpenMap('save01.map');
  //LoadDAT('mission1.dat');
end;


procedure TFormLoading.Label4Click(Sender: TObject);
begin
  MailTo('kromster80@gmail.com','KaM Editor','');
end;


procedure TFormLoading.Label7Click(Sender: TObject);
begin
  OpenMySite('KaM_Editor');
end;


{$IFDEF FPC}
initialization
  {$I KM_Form_Loading.lrs}
{$ENDIF}


end.
