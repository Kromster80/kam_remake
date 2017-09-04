{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels <arno.garrels@gmx.de>
Description:  Delphi 2009 and 2010 do not update old projects correctly.
              This IDE plugin reads OutputDir, UnitOutputDir, SearchPath and
              Conditionals from the .dof file on project updates from .dpr if
              no .dproj file already exists and sets these in new project's
              Base configuration.
Creation:     June 2011
Version:      1.00
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2011 by Arno Garrels
              Berlin, Germany <arno.garrels@gmx.de>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to François PIETTE. Use a nice stamp and mention your name,
                 street address, EMail address and any comment you like to say.

History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsDprUpdFix;

{.$DEFINE DEBUGLOG}
{$I Include\OverbyteIcsDefs.inc}

{$IFDEF COMPILER12_UP}
  {$IFNDEF COMPILER15_UP}
    {$DEFINE DPRFIX2009}
  {$ENDIF}
  {$IFDEF COMPILER16_UP}
    {$DEFINE DPRFIXXE2}
  {$ENDIF}
{$ENDIF}

interface

uses
    SysUtils, Classes, Forms, IniFiles, TypInfo,
{$IFDEF DPRFIXXE2}
    PlatformAPI,
    CommonOptionStrs,
{$ENDIF}
    ToolsApi;

type
    TIdeNotifier = class(TNotifierObject, IOTAIDENotifier)
    private
        FLastDpr : string;
    protected
        procedure AfterCompile(Succeeded: Boolean);
        procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
        procedure FileNotification(NotifyCode: TOTAFileNotification;
            const FileName: string; var Cancel: Boolean);
    end;

procedure Register;

implementation

uses
    DCCStrs;

const
    sDofSectDir     = 'Directories';
    sDofSectCustom  = 'IcsCustom';

var
    IDENotifierIndex : Integer = -1;
    GMessageService: IOTAMessageServices = nil;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure DebugLog(const Msg: string);
begin
    GMessageService.AddTitleMessage(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Register;
var
    Services : IOTAServices;
begin
{$IF DEFINED(DPRFIX2009) or DEFINED(DPRFIXXE2)}
    Services := BorlandIDEServices as IOTAServices;
    GMessageService := (BorlandIDEServices as IOTAMessageServices);
    if (IDENotifierIndex = -1) then begin
        IDENotifierIndex := Services.AddNotifier(TIdeNotifier.Create);
    {$IFDEF DEBUGLOG}
        DebugLog('OverbyteIcsDprUpdFix Installed NotifierIndex: #' + IntToStr(IDENotifierIndex));
    {$ENDIF}
    end;
{$IFEND}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsProjectFile(const FileName: string;
  var Project : IOTAProject): Boolean;
var
    Module  : IOTAModule;
    ProjectGroup : IOTAProjectGroup;
begin
    Module := (BorlandIDEServices as IOTAModuleServices).FindModule(FileName);
    Result := Supports(Module, IOTAProject, Project) and
              not Supports(Module, IOTAProjectGroup, ProjectGroup);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIdeNotifier.AfterCompile(Succeeded: Boolean);
begin
{$IFDEF DEBUGLOG}
    DebugLog('After Compile');
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIdeNotifier.BeforeCompile(const Project: IOTAProject;
  var Cancel: Boolean);
begin
{$IFDEF DEBUGLOG}
    DebugLog('Before Compile');
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIdeNotifier.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
var
    Project               : IOTAProject;
    OptionsConfigurations : IOTAProjectOptionsConfigurations;
    BaseConfig            : IOTABuildConfiguration;
    IniValues             : TStringList;
    Values                : TStringList;
    Ini                   : TIniFile;
    I                     : Integer;
{$IFDEF DPRFIX2009}
    RS                    : array of string;
    S1                    : string;
    S2                    : string;
{$ENDIF}
    Dirty                 : Boolean;
    DofFile               : string;
{$IFDEF DPRFIXXE2}
    RS                    : array of string;
    ProjectPlatforms      : IOTAProjectPlatforms;
    DebugConfig           : IOTABuildConfiguration;
    bFlag                 : Boolean;
{$ENDIF}
begin
    try
    {$IFDEF DEBUGLOG}
        DebugLog(Format('%s: %s',
         [GetEnumName(TypeInfo(TOTAFileNotification), Ord(NotifyCode)), FileName]));
    {$ENDIF}
        case NotifyCode of
            ofnFileOpening :
                if ExtractFileExt(FileName) = '.dpr' then
                begin
                    FLastDpr := ChangeFileExt(FileName, '.dproj');
                    if FileExists(FLastDpr) then
                        FLastDpr     := '';
                end;

            ofnFileOpened :
                if FLastDpr = FileName then
                begin
                    FLastDpr      := '';
                    DofFile       := ChangeFileExt(FileName, '.dof');
                    Dirty         := False;
                    Values        := nil;
                    IniValues     := nil;
                    Ini           := nil;
                    if FileExists(DofFile) and IsProjectFile(FileName, Project) and
                      Supports(Project.ProjectOptions,
                               IOTAProjectOptionsConfigurations,
                               OptionsConfigurations) then
                    try
                        BaseConfig := OptionsConfigurations.BaseConfiguration;
                        if BaseConfig = nil then // Should never happen on UPD from .dpr
                            Exit;
                    {$IFDEF DPRFIX2009} // And D2010
                        Ini := TIniFile.Create(DofFile);
                        IniValues := TStringList.Create;
                        IniValues.Delimiter := ';';
                        IniValues.StrictDelimiter := True;
                        Values := TStringList.Create;

                        //-----------------------------------
                        IniValues.DelimitedText := Ini.ReadString(sDofSectDir, 'SearchPath', '');
                        if IniValues.Count > 0 then
                        begin
                            BaseConfig.GetValues(sUnitSearchPath, Values, False);
                            for I := IniValues.Count - 1 downto 0 do
                            begin
                                if Values.IndexOf(Trim(IniValues[I])) >= 0 then
                                    IniValues.Delete(I);
                            end;
                            if IniValues.Count > 0 then
                            begin
                                SetLength(RS, IniValues.Count);
                                for I := 0 to IniValues.Count - 1 do
                                    RS[I] := Trim(IniValues[I]);
                                BaseConfig.InsertValues(sUnitSearchPath, RS);
                                Dirty := True;
                                DebugLog('ICS UpdateFix from .dof: Base SearchPath');
                            end;
                        end;

                        //-----------------------------------
                        IniValues.DelimitedText := Ini.ReadString(sDofSectDir, 'Conditionals', '');
                        if IniValues.Count > 0 then
                        begin
                            Values.Clear;
                            BaseConfig.GetValues(sDefine, Values, False);
                            for I := IniValues.Count - 1 downto 0 do
                            begin
                                if Values.IndexOf(Trim(IniValues[I])) >= 0 then
                                    IniValues.Delete(I);
                            end;
                            if IniValues.Count > 0 then
                            begin
                                SetLength(RS, IniValues.Count);
                                for I := 0 to IniValues.Count - 1 do
                                    RS[I] := Trim(IniValues[I]);
                                BaseConfig.InsertValues(sDefine, RS);
                                Dirty := True;
                                DebugLog('ICS UpdateFix from .dof: Base Conditionals');
                            end;
                        end;

                        //-----------------------------------
                        S2 := Trim(Ini.ReadString(sDofSectDir, 'OutputDir', ''));
                        if S2 <> '' then
                        begin
                            S1 := BaseConfig.GetValue(sExeOutput, False);
                            if S1 <> S2 then
                            begin
                                BaseConfig.Value[sExeOutput] := S2;
                                Dirty := True;
                                DebugLog('ICS UpdateFix from .dof: Base OutputDir');
                            end;
                        end;

                        //-----------------------------------
                        S2 := Trim(Ini.ReadString(sDofSectDir, 'UnitOutputDir', ''));
                        if S2 <> '' then
                        begin
                            S1 := BaseConfig.GetValue(sDcuOutput, False);
                            if S1 <> S2 then
                            begin
                                BaseConfig.Value[sDcuOutput] := S2;
                                Dirty := True;
                                DebugLog('ICS UpdateFix from .dof: Base UnitOutputDir');
                            end;
                        end;

                        //-----------------------------------
                    {$ENDIF}
                    {$IFDEF DPRFIXXE2}
                        { Read values from our custom ini-section added to .dof files }
                        { This is just the bare minimum to update the ICS samples     }
                        { from .dpr / .dof with the platforms and framework type we   }
                        { want reliable.                                              }
                        if not Assigned(Ini) then
                            Ini := TIniFile.Create(DofFile);

                        if Supports(Project, IOTAProjectPlatforms, ProjectPlatforms) and
                            Ini.SectionExists(sDofSectCustom) then // It's an ICS demo .dpr
                        begin
                            if (UpperCase(Trim(Ini.ReadString(sDofSectCustom, 'FrameworkType', ''))) = sFrameworkTypeVCL) and
                               (Project.FrameworkType <> sFrameworkTypeVCL) then
                            begin
                                Project.ProjectOptions.Values['FrameworkType'] := sFrameworkTypeVCL;
                                Dirty := True;
                            end;
                            bFlag := Ini.ReadBool(sDofSectCustom, 'Win32Supported', TRUE);
                            if ProjectPlatforms.Supported[cWin32Platform] <> bFlag then
                            begin
                                ProjectPlatforms.Supported[cWin32Platform] := bFlag;
                                Dirty := True;
                            end;
                            bFlag := Ini.ReadBool(sDofSectCustom, 'Win32Enabled', TRUE);
                            if ProjectPlatforms.Enabled[cWin32Platform] <> bFlag then
                            begin
                                if bFlag and not ProjectPlatforms.Supported[cWin32Platform] then
                                    ProjectPlatforms.Supported[cWin32Platform] := TRUE;
                                ProjectPlatforms.Enabled[cWin32Platform] := bFlag;
                                Dirty := True;
                            end;
                            bFlag := Ini.ReadBool(sDofSectCustom, 'Win64Supported', TRUE);
                            if ProjectPlatforms.Supported[cWin64Platform] <> bFlag then
                            begin
                                ProjectPlatforms.Supported[cWin64Platform]   := bFlag;
                                Dirty := True;
                            end;
                            bFlag := Ini.ReadBool(sDofSectCustom, 'Win64Enabled', FALSE);
                            if ProjectPlatforms.Enabled[cWin64Platform] <> bFlag then
                            begin
                                if bFlag and not ProjectPlatforms.Supported[cWin64Platform] then
                                    ProjectPlatforms.Supported[cWin64Platform] := TRUE;
                                ProjectPlatforms.Enabled[cWin64Platform] := bFlag;
                                if bFlag then
                                begin
                                    { Enable remote debug symbols for Win64 }
                                    for I := 0 to OptionsConfigurations.ConfigurationCount -1 do
                                    begin
                                        if OptionsConfigurations.Configurations[I].Name = 'Debug' then
                                        begin
                                            DebugConfig :=
                                              OptionsConfigurations.Configurations[I].PlatformConfiguration[cWin64Platform];
                                            if Assigned(DebugConfig) then
                                                DebugConfig.Value[sRemoteDebug] := 'true';
                                            Break;
                                        end;
                                    end;
                                end;
                                Dirty := True;
                            end;
                            bFlag := Ini.ReadBool(sDofSectCustom, 'OSX32Supported', FALSE);
                            if ProjectPlatforms.Supported[cOSX32Platform] <> bFlag then
                            begin
                                ProjectPlatforms.Supported[cOSX32Platform] := bFlag;
                                Dirty := True;
                            end;
                            bFlag := Ini.ReadBool(sDofSectCustom, 'OSX32Enabled', FALSE);
                            if ProjectPlatforms.Enabled[cOSX32Platform] <> bFlag then
                            begin
                                if bFlag and not ProjectPlatforms.Supported[cOSX32Platform] then
                                    ProjectPlatforms.Supported[cOSX32Platform] := TRUE;
                                ProjectPlatforms.Enabled[cOSX32Platform] := bFlag;
                                Dirty := True;
                            end;
                            if Dirty then
                                DebugLog('ICS project updated from custom .dof section');
                        end;

                        { No internal manifest in 32-bit }
                        for I := 0 to OptionsConfigurations.ConfigurationCount -1 do
                        begin
                            DebugConfig :=
                              OptionsConfigurations.Configurations[I].PlatformConfiguration[cWin32Platform];
                            if Assigned(DebugConfig) then
                            begin
                                if LowerCase(DebugConfig.Value[sManifest_File]) <> 'none' then
                                begin
                                    DebugConfig.Value[sManifest_File] := 'None';
                                    Dirty := True;
                                end;
                            end;
                        end;

                        { No version info in 32-bit }
                        DebugConfig := BaseConfig.PlatformConfiguration[cWin32Platform];
                        if Assigned(DebugConfig) then
                        begin
                            if LowerCase(DebugConfig.Value[sVerInfo_IncludeVerInfo]) <> 'false' then
                            begin
                                DebugConfig.Value[sVerInfo_IncludeVerInfo] := 'false';
                                Dirty := True;
                            end;
                        end;

                        { Fix namespace prefixes }

                        if not Assigned(IniValues) then
                          IniValues := TStringList.Create;
                        IniValues.Delimiter := ';';
                        IniValues.StrictDelimiter := True;
                        if not Assigned(Values) then
                          Values := TStringList.Create;

                        { Check if Winapi is set in Base config, if not add it }
                        IniValues.DelimitedText := 'Winapi;System.Win';
                        BaseConfig.GetValues(sNamespace, Values, False);

                        { For some reason unit scope name 'Ics.Fmx' is added in XE2 }
                        if Values.IndexOf('Ics.Fmx') >= 0 then begin
                          SetLength(RS, 1);
                          RS[0] := 'Ics.Fmx';
                          BaseConfig.RemoveValues(sNamespace, RS);
                        end;

                        for I := IniValues.Count - 1 downto 0 do
                        begin
                            if Values.IndexOf(IniValues[I]) >= 0 then
                                IniValues.Delete(I);
                        end;

                        if IniValues.Count > 0 then
                        begin
                            SetLength(RS, IniValues.Count);
                            for I := 0 to IniValues.Count - 1 do
                                RS[I] := IniValues[I];
                            BaseConfig.InsertValues(sNamespace, RS);
                            Dirty := True;
                        end;

                    {$ENDIF}
                        if Dirty then
                            Project.Save(False, True);
                    finally
                        Ini.Free;
                        Values.Free;
                        IniValues.Free;
                    end;
                end;

        end;
    except
        FLastDpr := '';
        Application.HandleException(Self);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure RemoveIDENotifier;
var
    Services : IOTAServices;
begin
    if IDENotifierIndex > -1 then
    begin
        Services := BorlandIDEServices as IOTAServices;
        if Services <> nil then
            Services.RemoveNotifier(IDENotifierIndex);
        IDENotifierIndex := -1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
initialization

finalization
    RemoveIDENotifier;

end.

