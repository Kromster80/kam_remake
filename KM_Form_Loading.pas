unit KM_Form_Loading;
interface
uses {$IFDEF FPC} LResources, {$ENDIF} Forms, Controls,
ComCtrls, ExtCtrls, StdCtrls, Graphics, Classes;

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
  end;

implementation
{$R *.dfm}

//initialization
//{$I KM_Form_Loading.lrs}

end.
