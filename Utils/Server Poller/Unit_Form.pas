unit Unit_Form;
interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, StrUtils, Math,
  Controls, Forms, Dialogs, Grids, StdCtrls, ExtCtrls,
  KM_Defaults, KM_Settings, KM_ServerQuery, KM_CommonUtils;

type
  TForm1 = class(TForm)
    StringGrid1: TStringGrid;
    Label1: TLabel;
    Button2: TButton;
    Memo2: TMemo;
    CheckBox1: TCheckBox;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    fGameSettings: TGameSettings;
    fServerQuery: TKMServerQuery;
    fBaseTime: TDateTime;
    fBase: array [0..63] of record
      Room: string;
      Time: TDateTime;
    end;
    fCurrent: array [0..63] of record
      Room: string;
      Time: TDateTime;
    end;
    procedure AnnouncementsUpdate(const S: UnicodeString);
    procedure DoIdle(Sender: TObject; var Done: Boolean);
    procedure UpdateList(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation
{$R *.dfm}


procedure TForm1.FormCreate(Sender: TObject);
begin
  Application.OnIdle := DoIdle;
  ExeDir := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\..\');

  fGameSettings := TGameSettings.Create;

  Label1.Caption := 'Master-server: ' + fGameSettings.MasterServerAddress;

  fServerQuery := TKMServerQuery.Create(fGameSettings.MasterServerAddress);

  fServerQuery.OnAnnouncements := AnnouncementsUpdate;
  fServerQuery.FetchAnnouncements(AnsiString('eng'));

  StringGrid1.ColCount := 5;
  StringGrid1.ColWidths[0] := 250;
  StringGrid1.ColWidths[1] := 250;
  StringGrid1.ColWidths[2] := 60;
  StringGrid1.ColWidths[3] := 100;
  StringGrid1.ColWidths[4] := 180;

  StringGrid1.Cols[0].Text := 'Name';
  StringGrid1.Cols[1].Text := 'Description';
  StringGrid1.Cols[2].Text := 'Players';
  StringGrid1.Cols[3].Text := 'Duration';
  StringGrid1.Cols[4].Text := 'Map';
end;


procedure TForm1.Timer1Timer(Sender: TObject);
begin
  fServerQuery.OnListUpdated := UpdateList;
  fServerQuery.RefreshList;
end;

procedure TForm1.AnnouncementsUpdate(const S: UnicodeString);
begin
end;


procedure TForm1.Button2Click(Sender: TObject);
var
  I,K: Integer;
  R: TKMRoomInfo;
  S: TKMServerInfo;
  DisplayName: string;
  dtRoom: TDateTime;
  dt: TDateTime;
  L: Integer;
begin
  if fBaseTime = 0 then
  begin
    fBaseTime := Now;
    K := 0;
    for I := 0 to fServerQuery.Rooms.Count - 1 do
    begin
      R := fServerQuery.Rooms[I];
      S := fServerQuery.Servers[R.ServerIndex];
      DisplayName := IfThen(R.OnlyRoom, S.Name, S.Name + ' #' + IntToStr(R.RoomID + 1));

      if R.GameInfo.GameTime <> -1 then
      begin
        fBase[K].Room := DisplayName;
        fBase[K].Time := R.GameInfo.GameTime;
        Inc(K);
      end;
    end;
  end;

  K := 0;
  for I := 0 to fServerQuery.Rooms.Count - 1 do
  begin
    R := fServerQuery.Rooms[I];
    S := fServerQuery.Servers[R.ServerIndex];
    DisplayName := IfThen(R.OnlyRoom, S.Name, S.Name + ' #' + IntToStr(R.RoomID + 1));

    if R.GameInfo.GameTime <> -1 then
    begin
      fCurrent[K].Room := DisplayName;
      fCurrent[K].Time := R.GameInfo.GameTime;
      Inc(K);
    end;
  end;

  Memo2.Clear;
  for I := 0 to 63 do
  if fBase[I].Room <> '' then
    for K := 0 to 63 do
    if fCurrent[K].Room = fBase[I].Room then
    begin
      dtRoom := fCurrent[K].Time - fBase[I].Time;
      dt := Max(Now - fBaseTime, 0.000001);

      L := 1;
      DisplayName := '';
      while L <= Length(fBase[I].Room) do
      begin
        if (L < Length(fBase[I].Room)) and (fBase[I].Room[L] + fBase[I].Room[L+1] = '[$') then
          Inc(L, 9);
        DisplayName := DisplayName + fBase[I].Room[L];
        Inc(L);
      end;

      Memo2.Lines.Append(DisplayName + ' - x' + FloatToStr(dtRoom / dt));
    end;

  Memo2.Lines.Append('Delta = ' + TimeToStr(Now - fBaseTime));
end;


procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  Timer1.Enabled := CheckBox1.Checked;
  Timer1Timer(Timer1);
end;

procedure TForm1.UpdateList(Sender: TObject);
var
  I: Integer;
  R: TKMRoomInfo;
  S: TKMServerInfo;
  DisplayName, ShortName: string;
begin
  StringGrid1.RowCount := fServerQuery.Rooms.Count + 2;

  for I := 0 to fServerQuery.Rooms.Count - 1 do
  begin
    R := fServerQuery.Rooms[I];
    S := fServerQuery.Servers[R.ServerIndex];
    DisplayName := IfThen(R.OnlyRoom, S.Name, S.Name + ' #' + IntToStr(R.RoomID + 1));

    ShortName := StripColor(DisplayName);

    StringGrid1.Cells[0, I+1] := ShortName;
    StringGrid1.Cells[1, I+1] := R.GameInfo.Description;
    StringGrid1.Cells[2, I+1] := IntToStr(R.GameInfo.PlayerCount);
    StringGrid1.Cells[3, I+1] := TimeToStr(R.GameInfo.GameTime);
    StringGrid1.Cells[4, I+1] := R.GameInfo.Map;
  end;
end;


procedure TForm1.DoIdle(Sender: TObject; var Done: Boolean);
begin
  if fServerQuery <> nil then
    fServerQuery.UpdateStateIdle;

  Done := False; //Repeats OnIdle asap without performing Form-specific idle code
end;


end.
