unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure BtnDecodeUClick(Sender: TObject);
    procedure BtnDecodeAClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.BtnDecodeAClick(Sender: TObject);
var
  LibHead: packed record
    txtCount, unk1, unk2: Word;
  end;
  c: AnsiChar;
  offsets: array of SmallInt;
  txtLen: SmallInt;
  txt: array of AnsiString;
  I,K: Integer;
  libx: TStringList;
begin
  with TMemoryStream.Create do
  begin
    LoadFromFile('setupA.lib');

    Read(LibHead, SizeOf(LibHead));

    SetLength(offsets, LibHead.txtCount);
    Read(offsets[0], LibHead.txtCount * 2);

    SetLength(txt, LibHead.txtCount);
    for I := 0 to LibHead.txtCount - 1 do
    if offsets[I] <> -1 then
    begin
      Seek(SizeOf(LibHead)+LibHead.txtCount*2+offsets[I], soFromBeginning);
      K := 0;
      repeat
        Read(c, 1);
        if (Ord(c) <> 0) then
          txt[I] := txt[I] + c;
        Inc(K);
      until((Ord(c) = 0) or ((I <> LibHead.txtCount - 1) and (offsets[I] + K = offsets[I+1])));
    end;
    Free;
  end;

  libx := TStringList.Create;
  libx.DefaultEncoding := TEncoding.ANSI;
  libx.Append('');
  libx.Append('MaxID: ' + IntToStr(LibHead.txtCount));
  libx.Append('');

  for I := 0 to LibHead.txtCount - 1 do
    libx.Append(IntToStr(I) + '. ' + txt[I]);

  libx.SaveToFile('setupA.libx');
  libx.Free;
end;

procedure TForm1.BtnDecodeUClick(Sender: TObject);
var
  LibHead: packed record
    txtCount, unk1, unk2: Word;
  end;
  c: WideChar;
  offsets: array of SmallInt;
  txtLen: SmallInt;
  txt: array of UnicodeString;
  I,K: Integer;
  libx: TStringList;
begin
  with TMemoryStream.Create do
  begin
    LoadFromFile('text.lib');

    Read(LibHead, SizeOf(LibHead));

    SetLength(offsets, LibHead.txtCount);
    Read(offsets[0], LibHead.txtCount * 2);

    SetLength(txt, LibHead.txtCount);
    for I := 0 to LibHead.txtCount - 1 do
    if offsets[I] <> -1 then
    begin
      Seek(SizeOf(LibHead)+LibHead.txtCount*2+offsets[I] * 2, soFromBeginning);
      K := 0;
      repeat
        Read(c, 2);
        if (Ord(c) <> 0) then
          txt[I] := txt[I] + c;
        Inc(K);
      until((Ord(c) = 0) or ((I <> LibHead.txtCount - 1) and (offsets[I] + K = offsets[I+1])));
    end;
    Free;
  end;

  libx := TStringList.Create;
  libx.DefaultEncoding := TEncoding.Unicode;
  libx.Append('');
  libx.Append('MaxID: ' + IntToStr(LibHead.txtCount));
  libx.Append('');

  for I := 0 to LibHead.txtCount - 1 do
    libx.Append(IntToStr(I) + '. ' + txt[I]);

  libx.SaveToFile('text.libx');
  libx.Free;
end;

end.
