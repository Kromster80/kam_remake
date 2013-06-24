unit Unit_Main;
{$I ..\..\KaM_Remake.inc}
interface
uses
  SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, StdCtrls, Spin,
  KM_ResourceFonts;


type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    SpinEdit1: TSpinEdit;
    Label3: TLabel;
    Label4: TLabel;
    Image1: TImage;
    Button2: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    SaveDialog1: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    Fnt: TKMFontData;
  end;


var
  Form1: TForm1;


implementation
{$R *.dfm}


procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  for I := 32 to 2550 do
    S := S + Char(I);

  Memo1.Lines.Text := S + 'йцукен';
end;


procedure TForm1.Button1Click(Sender: TObject);
var
  useChars: array of Char;
  fntStyle: TFontStyles;
  Bmp: TBitmap;
begin
  Fnt := TKMFontData.Create;

  SetLength(useChars, Length(Memo1.Text));
  Move(Memo1.Text[1], useChars[0], Length(Memo1.Text) * SizeOf(Char));

  fntStyle := [];
  if CheckBox1.Checked then
    fntStyle := fntStyle + [fsBold];
  if CheckBox2.Checked then
    fntStyle := fntStyle + [fsItalic];

  Fnt.CreateFont(Edit1.Text, SpinEdit1.Value, fntStyle, useChars);

  Bmp := TBitmap.Create;
  Fnt.ExportBimap(Image1.Picture.Bitmap, True);
  Bmp.Free;
end;


procedure TForm1.Button2Click(Sender: TObject);
begin
  if not SaveDialog1.Execute then Exit;

  Fnt.SaveToFontX(SaveDialog1.FileName);
end;


end.
