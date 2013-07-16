unit Unit1;
interface
uses
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, DependenciesGrapher;


type
  TForm1 = class(TForm)
    ButChooseDpr: TButton;
    OpenDialog: TOpenDialog;
    ButBuildGraph: TButton;
    ChConsSystem: TCheckBox;
    procedure ButChooseDprClick(Sender: TObject);
    procedure ButBuildGraphClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  end;


var
  Form1: TForm1;
  DepGraph : TDependenciesGrapher;


implementation
{$R *.dfm}


procedure TForm1.FormCreate(Sender: TObject);
begin
  DepGraph := TDependenciesGrapher.Create;
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
end;


procedure TForm1.ButBuildGraphClick(Sender: TObject);
begin
  DepGraph.BuildGraph(OpenDialog.FileName);
  DepGraph.PrintOutput(ExtractFilePath(Application.ExeName) + 'dependencies.csv');
end;


procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DepGraph.Free;
end;


end.
