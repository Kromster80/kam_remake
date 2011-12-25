unit ufind;

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
  StdCtrls,
  Buttons,
  ExtCtrls;

type

  { TfrmFind }

  TfrmFind = class(TForm)
    btnFind : TBitBtn;
    btnClose : TBitBtn;
    cbMatchCase : TCheckBox;
    cbBackwards: TCheckBox;
    cbTreatAsSpaces: TCheckBox;
    cgOptions : TCheckGroup;
    cbFromCursor : TCheckBox;
    edtText : TEdit;
    lblFind : TLabel;
    procedure btnFindClick(Sender : TObject);
    procedure edtTextKeyPress(Sender : TObject; var Key : char);
    procedure FormShow(Sender : TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmFind : TfrmFind;

implementation

{ TfrmFind }

procedure TfrmFind.btnFindClick(Sender : TObject);
begin
  ModalResult := mrOK;
end;

procedure TfrmFind.edtTextKeyPress(Sender : TObject; var Key : char);
begin
  // if they press enter
  if ord(Key) = 13 then
    btnFindClick(Sender);
end;

procedure TfrmFind.FormShow(Sender : TObject);
begin
  edtText.SetFocus;
end;

initialization
  {$I ufind.lrs}

end.
