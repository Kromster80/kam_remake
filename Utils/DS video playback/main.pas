unit main;
interface
uses
  Forms, Classes, Controls, DSPack;

  
type
  TFormPlayWin = class(TForm)
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FilterGraph: TFilterGraph;
    VideoWindow: TDSVideoWindowEx2;
  end;


var
  FormPlayWin: TFormPlayWin;


implementation
{$R *.dfm}


procedure TFormPlayWin.FormCreate(Sender: TObject);
begin
  FilterGraph := TFilterGraph.Create(Self);
  VideoWindow := TDSVideoWindowEx2.Create(Self);
  VideoWindow.Parent := Self;

  VideoWindow.AspectRatio := rmLetterBox;
  VideoWindow.FilterGraph := FilterGraph;
  VideoWindow.FullScreenTopMost := True;
  VideoWindow.StartFullScreen;
  VideoWindow.Width := Screen.Width;
  VideoWindow.Height := Screen.Height;
  VideoWindow.AutoHideCursor := 10000;
  VideoWindow.OnKeyDown := KeyDown;

  VideoWindow.Cursor := crNone; //Not working
  Cursor := crNone; //Not working
  Enabled := False;

  FilterGraph.Active := True;
  FilterGraph.ClearGraph;
  FilterGraph.RenderFile('kamlogo.avi');
  FilterGraph.Volume := 100;
  FilterGraph.Play;
end;


procedure TFormPlayWin.KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Close;
end;


procedure TFormPlayWin.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  FilterGraph.Active := False;
  FilterGraph.ClearGraph;
end;


end.
