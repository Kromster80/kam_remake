unit Unit1;
interface
uses
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, DependenciesGrapher,
  Vcl.ComCtrls, Vcl.ExtCtrls;


type
  TForm1 = class(TForm)
    ButChooseDpr: TButton;
    OpenDialog: TOpenDialog;
    ButBuildGraph: TButton;
    ChConsSystem: TCheckBox;
    ProgressBar: TProgressBar;
    Timer: TTimer;
    procedure ButChooseDprClick(Sender: TObject);
    procedure ButBuildGraphClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  end;

  TDepGraphThread = class(TThread)
  private
    fDprFile: string;
    DepGraph : TDependenciesGrapher;
  protected
    constructor Create(aSuspended: Boolean; aDprFile: string);
    procedure Execute; override;
    function Progress: Integer;
    procedure DoTerminate; override;
  end;

var
  Form1: TForm1;
  DepGraphThread: TDepGraphThread;


implementation
{$R *.dfm}


constructor TDepGraphThread.Create(aSuspended: Boolean; aDprFile: string);
begin
  inherited Create(aSuspended);

  fDprFile := aDprFile;
end;


procedure TDepGraphThread.DoTerminate;
begin
  inherited;

  if DepGraph <> nil then
    DepGraph.Cancel();
end;


procedure TDepGraphThread.Execute;
begin
  DepGraph := TDependenciesGrapher.Create;

  DepGraph.BuildGraph(fDprFile);
  DepGraph.PrintOutput(ExtractFilePath(fDprFile) + 'dependencies.csv');

  FreeAndNil(DepGraph);
end;


function TDepGraphThread.Progress: Integer;
begin
  Result := 0;

  if DepGraph <> nil then
    Result := DepGraph.GetAnalyseProgress;
end;


procedure TForm1.TimerTimer(Sender: TObject);
begin
  ProgressBar.Position := DepGraphThread.Progress;
end;


procedure TForm1.ButChooseDprClick(Sender: TObject);
begin
  if not OpenDialog.Execute then
    Exit;

  OpenDialog.InitialDir := ExpandFileName(ExtractFilePath(Application.ExeName) + '..\..\');

  Assert(SameText(ExtractFileExt(OpenDialog.FileName), '.dpr'));

  ButChooseDpr.Visible := False;
  ButBuildGraph.Visible := True;
  ChConsSystem.Visible := True;
  ProgressBar.Visible := True;
end;


procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if DepGraphThread <> nil then
  begin
    if DepGraphThread.DepGraph <> nil then
      DepGraphThread.DepGraph.Cancel();
    DepGraphThread.Terminate;
    DepGraphThread.WaitFor;
  end;
end;

procedure TForm1.ButBuildGraphClick(Sender: TObject);
begin
  Timer.Enabled := True;
  DepGraphThread := TDepGraphThread.Create(False, OpenDialog.FileName);
end;


end.
