unit Unit_Form;
interface
uses
  Windows, SysUtils, System.Classes, StrUtils, Controls, Forms, StdCtrls, Grids;

type
  TForm1 = class(TForm)
    Button1: TButton;
    StringGrid1: TStringGrid;
    Edit1: TEdit;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure StringGrid1FixedCellClick(Sender: TObject; ACol, ARow: Integer);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fLastSortColumn: Integer;
  public
    procedure ScanFolder(aPath: string);
    procedure SortStringGrid(var aStringGrid: TStringGrid; aColumn: Integer);
  end;

  TStringArray = array [0..5] of string;


var
  Form1: TForm1;


implementation
{$R *.dfm}


procedure TForm1.Button1Click(Sender: TObject);
var
  Path: string;
begin
  Path := Edit1.Text;
  ScanFolder(Path);
end;


procedure TForm1.Button2Click(Sender: TObject);
begin
  StringGrid1.Cols[5].SaveToFile(Edit1.Text + 'url_list2.txt');
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
  fLastSortColumn := -1;
end;


procedure TForm1.ScanFolder(aPath: string);
  function ExtractInfo(aFilename: string): TStringArray;
  var
    S: TStringList;
    SS: string;
    I, K: Integer;
  begin
    Result[0] := '';
    Result[1] := '';
    Result[2] := '';
    Result[3] := '';
    Result[4] := '';
    Result[5] := '';
    S := TStringList.Create;
    S.LoadFromFile(aFilename);
    SS := S.Text;

    //Date/time
    I := Pos('date/time         : ', SS);
    if I <> 0 then
    begin
      Inc(I, 20);
      K := PosEx(#10, SS, I);
      if K > I then
        Result[0] := Copy(SS, I, K-I-1);
    end;

    //Exception text
    I := Pos('exception message : ', SS);
    if I <> 0 then
    begin
      Inc(I, 20);
      K := PosEx(#10, SS, I);
      if K > I then
        Result[1] := Copy(SS, I, K-I-1);
    end;

    //Username
    I := Pos('from user:'#13#10, SS);
    if I <> 0 then
    begin
      Inc(I, 12);
      K := PosEx(#10, SS, I);
      if K > I then
        Result[2] := Copy(SS, I, K-I-1);
    end;

    //Contact email
    I := Pos('contact email     : ', SS);
    if I <> 0 then
    begin
      Inc(I, 20);
      K := PosEx(#10, SS, I);
      if K > I then
        Result[3] := Copy(SS, I, K-I-1);
    end;

    //Link to crashreport
    I := Pos('http://kam', SS);
    if I <> 0 then
    begin
      K := PosEx('.zip', SS, I);
      if K > I then
        Result[4] := Copy(SS, I, K-I+4);
    end;
  end;
var
  SearchRec: TSearchRec;
  I: Integer;
  StringInfo: TStringArray;
begin
  if not DirectoryExists(aPath) then Exit;

  StringGrid1.ColCount := 10;
  StringGrid1.ColWidths[0] := 40;
  StringGrid1.ColWidths[1] := 200;
  StringGrid1.ColWidths[2] := 300;
  StringGrid1.ColWidths[3] := 300;
  StringGrid1.ColWidths[4] := 200;
  StringGrid1.ColWidths[5] := 350;

  StringGrid1.Cols[0].Text := '#';
  StringGrid1.Cols[1].Text := 'Timestamp';
  StringGrid1.Cols[2].Text := 'Exception';
  StringGrid1.Cols[3].Text := 'Description';
  StringGrid1.Cols[4].Text := 'Contact';
  StringGrid1.Cols[5].Text := 'Link';

  FindFirst(aPath + '*.eml', faAnyFile - faDirectory, SearchRec);
  repeat
    StringInfo := ExtractInfo(aPath + SearchRec.Name);

    StringGrid1.RowCount := StringGrid1.RowCount + 1;
    StringGrid1.Cells[0, StringGrid1.RowCount-1] := IntToStr(StringGrid1.RowCount-1);

    for I := 0 to High(StringInfo) do
      StringGrid1.Cells[I+1, StringGrid1.RowCount-1] := StringInfo[I];

  until (FindNext(SearchRec) <> 0);
  FindClose(SearchRec);
end;


procedure TForm1.SortStringGrid(var aStringGrid: TStringGrid; aColumn: Integer);
const
  TheSeparator = '^`';
var
  RCount, I, ThePosition: Integer;
  MyList: TStringList;
  MyString, TempString: string;
begin
  RCount := aStringGrid.RowCount;

  MyList := TStringList.Create;
  MyList.Sorted := False;
  try
    for I := 1 to RCount - 1 do
      MyList.Add(aStringGrid.Rows[I].Strings[aColumn] + TheSeparator + aStringGrid.Rows[I].Text);

    Mylist.Sort;

    //Trim sort key
    for I := 0 to Mylist.Count - 1 do
    begin
      MyString := MyList.Strings[I];
      ThePosition := Pos(TheSeparator, MyString);
      //Eliminate the Text of the column on which we have sorted the StringGrid
      TempString := Copy(MyString, ThePosition + Length(TheSeparator), Length(MyString));
      MyList.Strings[I] := TempString;
    end;

    if fLastSortColumn = aColumn then
    begin
      for I := 1 to RCount - 1 do
        aStringGrid.Rows[I].Text := MyList.Strings[RCount - I-1];
      //Reset column so next time we use straight sort direction
      fLastSortColumn := -1;
    end
    else
    begin
      for I := 1 to RCount - 1 do
        aStringGrid.Rows[I].Text := MyList.Strings[I-1];
      //Remember column so next time we invert sort direction
      fLastSortColumn := aColumn;
    end;

  finally
    MyList.Free;
  end;
end;


procedure TForm1.StringGrid1FixedCellClick(Sender: TObject; ACol, ARow: Integer);
begin
  SortStringGrid(StringGrid1, aCol);
end;


end.
