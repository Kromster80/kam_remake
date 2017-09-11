unit KM_FormLogistics;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls;

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
