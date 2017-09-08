program RXXPacker;
{$I ..\..\KaM_Remake.inc}
uses
  Forms, SysUtils,
  {$IFDEF FPC}Interfaces,{$ENDIF}
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF FPC} LResources, LCLIntf, {$ENDIF}
  RXXPackerForm in 'RXXPackerForm.pas' {RXXForm1},
	RXXPackerProc in 'RXXPackerProc.pas',
  KM_PNG in '..\..\src\utils\KM_PNG.pas',
  KM_Pics in '..\..\src\utils\KM_Pics.pas',
  KM_ResSprites in '..\..\src\res\KM_ResSprites.pas',
  KM_ResSpritesEdit in '..\..\src\res\KM_ResSpritesEdit.pas',
  KM_ResPalettes in '..\..\src\res\KM_ResPalettes.pas',
  KM_SoftShadows in '..\..\src\KM_SoftShadows.pas',
  KM_Defaults in '..\..\src\common\KM_Defaults.pas';

	{$APPTYPE CONSOLE}
{$IFDEF WDC}
{$R *.res}
{$ENDIF}

var
  I: Integer;
  J: TRXType;
  fRXXPacker: TRXXPacker;
  fPalettes: TKMResPalettes;
  Tick: Cardinal;

begin
  if ParamCount >= 1 then
  begin
    writeln(sLineBreak + 'KaM Remake RXX Packer' + sLineBreak);

    if ParamCount = 0 then
    begin
      writeln('No rx packages were set');
      writeln('Usage example: RXXPacker.exe gui guimain houses trees units');
      Exit;
    end;


    ExeDir := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\..\');
    fRXXPacker := TRXXPacker.Create;
    fPalettes := TKMResPalettes.Create;
    fPalettes.LoadPalettes(ExeDir + 'data\gfx\');
    try
      for I := 1 to ParamCount do // Skip 0, as this is the EXE-path
        for J := Low(TRXType) to High(TRXType) do
          if (LowerCase(ParamStr(I)) = LowerCase(RXInfo[J].FileName)) then
          begin
            Tick := GetTickCount;
            fRXXPacker.Pack(J, fPalettes);
            writeln(RXInfo[J].FileName + '.rxx packed in ' + IntToStr(GetTickCount - Tick) + ' ms');
          end;
    finally
      fRXXPacker.Free;
      fPalettes.Free;
    end;


  end else begin
    Application.Initialize;
    Application.CreateForm(TRXXForm1, RXXForm1);
    Application.Run;
  end;
end.
