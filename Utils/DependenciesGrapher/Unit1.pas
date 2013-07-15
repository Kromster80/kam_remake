unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, DependenciesGrapher;

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
  private
    { Private declarations }
  public
    { Public declarations }
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
  if not OpenDialog.Execute() then
    exit;
  if not SameText( ExtractFileExt(OpenDialog.FileName ), '.dpr' ) then
  begin
    MessageBox( 0, 'File must be a Delphi project', 'Error', MB_OK );
    exit;
  end;
  ButChooseDpr.Visible := false;
  ButBuildGraph.Visible := true;
  ChConsSystem.Visible := true;
end;


procedure TForm1.ButBuildGraphClick(Sender: TObject);
begin
  DepGraph.BuildGraph(  OpenDialog.FileName );
  DepGraph.PrintOutput( 'dependencies.txt' );
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DepGraph.Free;
end;

end.
