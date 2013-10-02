unit Unit1;
interface
uses
  Vcl.Forms, Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls, Vcl.Dialogs, Vcl.ComCtrls,
  System.Classes, System.SysUtils,
  DependenciesGrapher;


type
  TForm1 = class(TForm)
    btnSelectDpr: TButton;
    odSelectProject: TOpenDialog;
    btnExportCsv: TButton;
    ChConsSystem: TCheckBox;
    pbProgress: TProgressBar;
    Timer: TTimer;
    procedure btnSelectDprClick(Sender: TObject);
    procedure btnExportCsvClick(Sender: TObject);
  end;


var
  Form1: TForm1;
  DepGraph : TDependenciesGrapher;


implementation
{$R *.dfm}


procedure TForm1.btnSelectDprClick(Sender: TObject);
begin
  odSelectProject.FileName := ExpandFileName('..\..\KaM_Remake.dproj');
  //if not odSelectProject.Execute then Exit;

  odSelectProject.InitialDir := ExpandFileName(ExtractFilePath(Application.ExeName) + '..\..\');

  Assert(SameText(ExtractFileExt(odSelectProject.FileName), '.dproj'));

  DepGraph := TDependenciesGrapher.Create;
  DepGraph.BuildGraph(odSelectProject.FileName);

  btnSelectDpr.Enabled := False;
  btnExportCsv.Enabled := True;
  btnExportCsv.SetFocus;
  ChConsSystem.Visible := True;
end;


procedure TForm1.btnExportCsvClick(Sender: TObject);
begin
  Timer.Enabled := True;

  DepGraph.ExportAsCsv(ExtractFilePath(odSelectProject.FileName) + 'dependencies.csv');

  FreeAndNil(DepGraph);
end;


end.
