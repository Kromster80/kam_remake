unit KM_FormLogistics;

interface
uses
  {$IFDEF FPC} LResources, {$ENDIF}
  {$IFDEF MSWindows} Windows, Messages, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ComCtrls;

type
  TFormLogistics = class(TForm)
    ListView: TListView;
  private

  public

  end;

var
  FormLogistics: TFormLogistics;

implementation

{$R *.dfm}

uses KM_HandLogistics;

end.
