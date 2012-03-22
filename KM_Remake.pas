unit KM_Remake;
{$I KaM_Remake.inc}
interface
uses
  Classes, Controls, Forms, KM_Form_Loading, KM_Unit1 {KM_Form_Main},
  KM_Game;

type
  TKMRemake = class
  private
    fForm1: TForm1;
    fFormLoading: TFormLoading;

  public
    constructor Create;

  end;


implementation


{ TKMRemake }
constructor TKMRemake.Create;
begin
  inherited;

  //fFormLoading.Show;
end;


end.
