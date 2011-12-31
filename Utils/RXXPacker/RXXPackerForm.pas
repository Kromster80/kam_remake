unit RXXPackerForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, KM_Defaults, KM_TextLibrary, KM_Render, KM_ResourceGFX, KM_ResourceSprites, KM_Log;

type
  TRXXForm1 = class(TForm)
    btnPackMenu: TButton;
    btnPackGame: TButton;
    procedure btnPackMenuClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnPackGameClick(Sender: TObject);
  end;

var
  RXXForm1: TRXXForm1;

implementation

{$R *.dfm}

procedure TRXXForm1.FormCreate(Sender: TObject);
begin
  ExeDir := '..\..\';
  //Although we don't need them in this tool, these are required to load sprites
  fLog := TKMLog.Create(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+'log.txt');
  fTextLibrary := TTextLibrary.Create(ExeDir+'data\misc\', 'eng');
  fRender := TRender.Create(Handle, 1024, 768, False);
  fResource := TResource.Create(nil,nil);
end;

procedure TRXXForm1.btnPackMenuClick(Sender: TObject);
begin
  fResource.Sprites.PackMenuRXX('RemakeMenu.rxx');
end;

procedure TRXXForm1.btnPackGameClick(Sender: TObject);
begin
  fResource.Sprites.PackGameRXX('RemakeGame.rxx');
end;

end.
