unit KM_FormLoading;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF FPC} LResources, {$ENDIF}
  Forms, Controls, ComCtrls, ExtCtrls, StdCtrls, Graphics, Classes;

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
    procedure FormHide(Sender: TObject);
  public
    procedure LoadingStep;
    procedure LoadingText(const aData: UnicodeString);
  end;


implementation
//{$IFDEF WDC}
  {$R *.dfm}
//{$ENDIF}


{ TFormLoading }
procedure TFormLoading.FormHide(Sender: TObject);
begin
  //Put loading screen out of screen bounds, while hidden
  //This fixes bug, when on loading with fullscreen mode, user Alt-Tab into other window.
  //Then loading screen will be invisible, but prevents user interaction (block clicks etc)
  Left := -1000;
  Top := -1000;
end;


procedure TFormLoading.LoadingStep;
begin
  if not Visible then Exit;
  Bar1.StepIt;
  Refresh;
end;


procedure TFormLoading.LoadingText(const aData: UnicodeString);
begin
  if not Visible then Exit;
  Label1.Caption := aData;
  Refresh;
end;


{$IFDEF FPC}
initialization
{$I KM_FormLoading.lrs}
{$ENDIF}

end.
