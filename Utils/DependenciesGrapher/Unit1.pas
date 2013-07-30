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
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TimerTimer(Sender: TObject);
  end;

  TDepGraphThread = class(TThread)
    private
    { Private declarations }
  protected
    procedure Execute; override;
  end;

var
  Form1: TForm1;
  DepGraph : TDependenciesGrapher;
  DepGraphThread: TDepGraphThread;


implementation
{$R *.dfm}

procedure TDepGraphThread.Execute;
begin
  DepGraph.BuildGraph( Form1.OpenDialog.FileName );
  DepGraph.PrintOutput( ExtractFilePath( Application.ExeName ) + 'dependencies.csv' );
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  DepGraph := TDependenciesGrapher.Create;

  //DepGraph.BuildGraph('C:\Users\Neo7k\Documents\KaM Remake\ScripringDemo\KaM_Remake.dpr');
  //DepGraph.PrintOutput(ExtractFilePath(Application.ExeName) + 'dependencies.csv');
end;


procedure TForm1.TimerTimer(Sender: TObject);
begin
  ProgressBar.Position := DepGraph.GetAnalyseProgress;
end;

procedure TForm1.ButChooseDprClick(Sender: TObject);
begin
  if not OpenDialog.Execute then
    Exit;

  OpenDialog.InitialDir := ExpandFileName(ExtractFilePath(Application.ExeName) + '..\..\');

  Assert(SameText(ExtractFileExt(OpenDialog.FileName ), '.dpr'));

  ButChooseDpr.Visible := False;
  ButBuildGraph.Visible := True;
  ChConsSystem.Visible := True;
  ProgressBar.Visible := True;
  Timer.Enabled := True;
end;


procedure TForm1.ButBuildGraphClick(Sender: TObject);
begin
  DepGraphThread := TDepGraphThread.Create( False );
  //DepGraphThread.Execute();
end;


procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DepGraph.Free;
end;


end.
