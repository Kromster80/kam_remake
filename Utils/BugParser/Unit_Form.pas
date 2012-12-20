unit Unit_Form;
interface
uses
  Windows, SysUtils, System.Classes, StrUtils, Controls, Forms, StdCtrls, Grids;

type
  TForm1 = class(TForm)
    Button1: TButton;
    StringGrid1: TStringGrid;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure StringGrid1Click(Sender: TObject);
  public
    InfoList: TStringList;
    procedure ScanFolder(aPath: string; aList: TStringList);
    procedure SortStringGrid(var aStringGrid: TStringGrid; aColumn: Integer);
  end;

  TStringArray = array [0..5] of string;

var
  Form1: TForm1;

implementation
{$R *.dfm}


procedure TForm1.FormCreate(Sender: TObject);
begin
  InfoList := TStringList.Create;
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  InfoList.Free;
end;


procedure TForm1.Button1Click(Sender: TObject);
var Path: string;
begin
  Path := ExtractFilePath(Application.ExeName);
  Path := 'C:\Users\Krom\Desktop\123\';
  ScanFolder(Path, InfoList);

  //InfoList.SaveToFile(Path + 'url_list.txt');
end;


procedure TForm1.ScanFolder(aPath: string; aList: TStringList);
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

    I := Pos('date/time         : ', SS);
    if I <> 0 then
    begin
      Inc(I, 20);
      K := PosEx(#10, SS, I);
      if K > I then
        Result[2] := Copy(SS, I, K-I+1);
    end;

    I := Pos('http://kam', SS);
    if I <> 0 then
    begin
      K := PosEx('.zip', SS, I);

      if K > I then
        Result[0] := Copy(SS, I, K-I+4);
    end;

    I := Pos('exception message : ', SS);
    if I <> 0 then
    begin
      Inc(I, 20);
      K := PosEx(#10, SS, I);
      if K > I then
        Result[1] := Copy(SS, I, K-I+1);
    end;
    I := Pos('contact email     : ', SS);
    if I <> 0 then
    begin
      Inc(I, 20);
      K := PosEx(#10, SS, I);
      if K > I then
        Result[2] := Copy(SS, I, K-I+1);
    end;
  end;
var
  SearchRec: TSearchRec;
  I: Integer;
  StringInfo: TStringArray;
begin
  aList.Clear;

  if not DirectoryExists(aPath) then Exit;

  StringGrid1.ColCount := 10;
    StringGrid1.ColWidths[0] := 30;
    StringGrid1.ColWidths[1] := 130;
    StringGrid1.ColWidths[2] := 230;

  FindFirst(aPath + '*.eml', faAnyFile - faDirectory, SearchRec);
  repeat

    StringInfo := ExtractInfo(aPath + SearchRec.Name);

    aList.Add(StringInfo[0]);

    StringGrid1.RowCount := StringGrid1.RowCount + 1;
    StringGrid1.Cells[0, StringGrid1.RowCount-1] := IntToStr(StringGrid1.RowCount-1);

    for I := 0 to High(StringInfo) do
      StringGrid1.Cells[I+1, StringGrid1.RowCount-1] := StringInfo[I];

  until (FindNext(SearchRec) <> 0);
  FindClose(SearchRec);
end;


procedure TForm1.SortStringGrid(var aStringGrid: TStringGrid; aColumn: Integer);
const
  TheSeparator = '^';
var
  CountItem, I, J, K, ThePosition: integer;
  MyList: TStringList;
  MyString, TempString: string;
begin
  CountItem := aStringGrid.RowCount;
  MyList := TStringList.Create;
  MyList.Sorted := False;
  try
    begin
      for I := 1 to (CountItem - 1) do
        MyList.Add(aStringGrid.Rows[I].Strings[aColumn] + TheSeparator +
          aStringGrid.Rows[I].Text);
      Mylist.Sort;
      for K := 1 to Mylist.Count do
      begin
        MyString := MyList.Strings[(K - 1)];
        ThePosition := Pos(TheSeparator, MyString);
        TempString  := '';
        {Eliminate the Text of the column on which we have sorted the StringGrid}
        TempString := Copy(MyString, (ThePosition + 1), Length(MyString));
        MyList.Strings[(K - 1)] := '';
        MyList.Strings[(K - 1)] := TempString;
      end;
      for J := 1 to (CountItem - 1) do
        aStringGrid.Rows[J].Text := MyList.Strings[(J - 1)];
    end;
  finally
    MyList.Free;
  end;
end;


procedure TForm1.StringGrid1Click(Sender: TObject);
begin
  SortStringGrid(StringGrid1, 2);
end;

end.
