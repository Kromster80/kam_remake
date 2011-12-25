unit uGoTo;

{$MODE objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  LResources,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  Spin,
  StdCtrls,
  Buttons;

type

  { TfrmGoTo }

  TfrmGoTo = class(TForm)
    btnGo : TBitBtn;
    btnCancel : TBitBtn;
    cbType: TComboBox;
    lblType: TLabel;
    lblseCap : TLabel;
    seIndex : TSpinEdit;
    procedure btnGoClick(Sender: TObject);
    procedure FormShow(Sender : TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmGoTo : TfrmGoTo;

implementation

procedure TfrmGoTo.FormShow(Sender : TObject);
begin
  //seIndex.Focus;
  //seIndex.SelectAll;
end;

procedure TfrmGoTo.btnGoClick(Sender: TObject);
begin

end;

initialization
  {$I ugoto.lrs}

end.
