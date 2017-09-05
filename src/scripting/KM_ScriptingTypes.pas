unit KM_ScriptingTypes;
{$I KaM_Remake.inc}
interface

type

  TKMScriptFileInfo = record
    FullFilePath: UnicodeString;
    FileName: UnicodeString;
    FileText: AnsiString;
  end;

  // Script error message
  TKMScriptErrorMessage = record
    GameMessage: UnicodeString; // Shown in game as Message box
    LogMessage: UnicodeString;  // Printed to Log (could be more detailed)
  end;

  TKMScriptErrorType = (se_InvalidParameter, se_Exception, se_PreprocessorError, se_CompileError, se_CompileWarning, se_Log);

  TKMScriptErrorEvent = procedure (aType: TKMScriptErrorType; aErrorString: UnicodeString; aDetailedErrorString: UnicodeString = '') of object;


implementation

end.

