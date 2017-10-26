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
  I, K: Integer;
  RXType: TRXType;
  fRXXPacker: TRXXPacker;
  fPalettes: TKMResPalettes;
  Tick: Cardinal;

const
  RXToPack: array of TRXType = [
    rxTrees,
    rxHouses,
    rxUnits,
    rxGui,
    rxGuiMain,
    rxTiles];


function IsConsoleMode: Boolean;
var
  SI: TStartupInfo;
begin
  SI.cb := SizeOf(StartUpInfo);
  GetStartupInfo(SI);
  Result := (SI.dwFlags and STARTF_USESHOWWINDOW) = 0;
end;


begin
  if not IsConsoleMode then
  begin
    FreeConsole; // Used to hide the console
    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    Application.CreateForm(TRXXForm1, RXXForm1);
    Application.Run;
  end else
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
        begin
          if LowerCase(ParamStr(I)) = 'all' then
          begin
            for K := Low(RXToPack) to High(RXToPack) do
            begin
              Tick := GetTickCount;
              fRXXPacker.Pack(RXToPack[K], fPalettes);
              writeln(RXInfo[RXToPack[K]].FileName + '.rxx packed in ' + IntToStr(GetTickCount - Tick) + ' ms');
            end;
            Exit;
          end;
          for RXType := Low(TRXType) to High(TRXType) do
            if (LowerCase(ParamStr(I)) = LowerCase(RXInfo[RXType].FileName)) then
            begin
              Tick := GetTickCount;
              fRXXPacker.Pack(RXType, fPalettes);
              writeln(RXInfo[RXType].FileName + '.rxx packed in ' + IntToStr(GetTickCount - Tick) + ' ms');
            end;
        end;
      finally
        fRXXPacker.Free;
        fPalettes.Free;
      end;
    end;
  end;
end.
