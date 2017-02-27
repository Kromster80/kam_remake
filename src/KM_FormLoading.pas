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
  public
    procedure LoadingStep;
    procedure LoadingText(const aData: UnicodeString);
  end;


implementation
//{$IFDEF WDC}
  {$R *.dfm}
//{$ENDIF}


{ TFormLoading }
procedure TFormLoading.LoadingStep;
begin
  if not Visible then exit;
  Bar1.StepIt;
  Refresh;
end;


procedure TFormLoading.LoadingText(const aData: UnicodeString);
begin
  if not Visible then exit;
  Label1.Caption := aData;
  Refresh;
end;


{$IFDEF FPC}
initialization
{$I KM_FormLoading.lrs}
{$ENDIF}

end.
