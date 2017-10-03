unit KM_Exceptions;
interface
uses
  MadExcept, madNVAssistant;


type
  TKMExceptions = class
  private
    procedure DoException(const ExceptIntf: IMEException; var Handled: boolean);
  public
    constructor Create;
    procedure LoadTranslation;
  end;

  //MadExcept requires this to not be within an object for some reason
  procedure DoCreateAssistant(const assistant: INVAssistant; const exception: IUnknown);

var
  fExceptions: TKMExceptions;


implementation
uses
  SysUtils,
  KM_Game,
  KM_Log, KM_ResTexts, KM_CommonClasses, KM_Defaults, KM_Points;


{ TKMExceptions }
constructor TKMExceptions.Create;
begin
  inherited;
  OnAssistantCreate := DoCreateAssistant;
  RegisterExceptionHandler(DoException, stTrySyncCallAlways);
  MESettings.HttpServer := 'http://kam.hodgman.id.au/crashupload.php?rev='+GAME_REVISION;
end;


procedure TKMExceptions.LoadTranslation;
begin
  MESettings.ExceptMsg          := gResTexts[TX_ERROR_MESSAGE];
  MESettings.SendBtnCaption     := gResTexts[TX_ERROR_SEND];
  MESettings.ShowBtnCaption     := gResTexts[TX_ERROR_SHOW_DATA];
  MESettings.CloseBtnCaption    := gResTexts[TX_ERROR_TERMINATE];
  MESettings.ContinueBtnCaption := gResTexts[TX_ERROR_RESUME];
  MESettings.SendBoxTitle       := gResTexts[TX_ERROR_SENDING_TITLE];
  MESettings.SendMailMsg        := gResTexts[TX_ERROR_SENDING_DATA];
  MESettings.SendAttachMsg      := gResTexts[TX_ERROR_SENDING_DATA];
  MESettings.SendFinalizeMsg    := gResTexts[TX_ERROR_SENDING_DATA];
  MESettings.PrepareAttachMsg   := gResTexts[TX_ERROR_SENDING_CONNECTING];
  MESettings.ConnectMsg         := gResTexts[TX_ERROR_SENDING_CONNECTING];
  MESettings.SendFailureMsg     := gResTexts[TX_ERROR_SENDING_FAILED];
  MESettings.PleaseWaitTitle    := gResTexts[TX_ERROR_WAIT_TITLEBAR];
  MESettings.PleaseWaitText     := gResTexts[TX_ERROR_WAIT_MESSAGE];
end;


//We have to load the translations for the send assistant when it is created, that's the only way
procedure DoCreateAssistant(const assistant: INVAssistant; const exception: IUnknown);
begin
  if gResTexts = nil then Exit; //If the exception happens before translations are loaded
  if assistant.FormCount = 3 then //That's how we know it's the send assistant, not one of the others (which we currently don't use)
  begin
    assistant.Title := gResTexts[TX_ERROR_SEND];
    //Contact details form
    assistant.Forms[0].Message := gResTexts[TX_ERROR_SEND_CONTACT];
    assistant.Forms[0].nvLabel('Label1').Caption := gResTexts[TX_ERROR_SEND_NAME];
    assistant.Forms[0].nvLabel('Label2').Caption := gResTexts[TX_ERROR_SEND_EMAIL];
    assistant.Forms[0].nvCheckBox('MemCheck').Caption := gResTexts[TX_ERROR_SEND_REMEMBER];
    assistant.Forms[0].ContinueButton.Caption := gResTexts[TX_ERROR_SEND_NEXT];
    assistant.Forms[0].CancelButton.Caption := gResTexts[TX_ERROR_SEND_CANCEL];

    //Error details form
    assistant.Forms[1].Message := gResTexts[TX_ERROR_SEND_DETAILS];
    assistant.Forms[1].nvLabel('Label1').Caption := gResTexts[TX_ERROR_SEND_DETAILS_MESSAGE];
    assistant.Forms[1].ContinueButton.Caption := gResTexts[TX_ERROR_SEND_NEXT];
    assistant.Forms[1].CancelButton.Caption := gResTexts[TX_ERROR_SEND_CANCEL];

    //Screenshot form
    assistant.Forms[2].Message := gResTexts[TX_ERROR_SEND_SCREENSHOT];
    assistant.Forms[2].nvCheckBox('AttachCheck').Caption := gResTexts[TX_ERROR_SEND_SCREENSHOT_MESSAGE];
    assistant.Forms[2].nvLabel('Label1').Caption := gResTexts[TX_ERROR_SEND_SCREENSHOT_EDIT];
    assistant.Forms[2].ContinueButton.Caption := gResTexts[TX_ERROR_SEND_SEND];
    assistant.Forms[2].CancelButton.Caption := gResTexts[TX_ERROR_SEND_CANCEL];
  end;
end;


procedure TKMExceptions.DoException(const ExceptIntf: IMEException; var Handled: boolean);
var LogMessage, CrashFile: string;
begin
  if gLog = nil then Exit; //Could crash very early before even the log file is created

  //It's nice to know when the exception happened in our log if the user decides to play on and sends the report later
  LogMessage := 'Exception occurred: ' + ExceptIntf.ExceptClass + ': ' + ExceptIntf.ExceptMessage;
  if ExceptIntf.ExceptObject is ELocError then
    LogMessage := LogMessage + ' at location ' + TypeToString(ELocError(ExceptIntf.ExceptObject).Loc);
  gLog.AddTime(LogMessage);
  gLog.AddNoTime('================================================================================');
  gLog.AddNoTime('                                START BUG REPORT                                ');
  gLog.AddNoTime('================================================================================');
  gLog.AddNoTime(ExceptIntf.BugReport);
  gLog.AddNoTime('================================================================================');
  gLog.AddNoTime('                                 END BUG REPORT                                 ');
  gLog.AddNoTime('================================================================================');

  //Append the exception message on a new paragraph of the dialog. It might be useful to the user (e.g. file permissions wrong)
  //and sometimes people send us a screenshot of the crash report window, it would be nice to know what the error was from that.
  if gResTexts <> nil then
    ExceptIntf.ExceptMsg := gResTexts[TX_ERROR_MESSAGE]+#13#10+#13#10+LogMessage
  else
    //Still need a sensible message if gResTexts failed to load for some reason. ENG is default
    ExceptIntf.ExceptMsg := 'An error occurred in the application. Please click Send Bug Report so we can investigate this issue. Thanks for your help!'+#13#10+#13#10+LogMessage;

  //We want to add some of our own files to the report
  CrashFile := 'KaM_Crash_' + GAME_REVISION + '_' + FormatDateTime('yyyy-mm-dd_hh-nn-ss', Now) + '.zip';
  MESettings.BugReportZip := CrashFile; //Exception info also goes in the zip
  MESettings.ScreenShotZip := CrashFile; //Screenshot also goes in the zip

  if gGame <> nil then gGame.AttachCrashReport(ExceptIntf, CrashFile);

  //Do the log after fGame because fGame adds stuff to the log
  if gLog <> nil then ExceptIntf.AdditionalAttachments.Add(gLog.LogPath, '', CrashFile);

  //Do settings here not in fGame because we could crash before fGame is created
  if FileExists(ExeDir + SETTINGS_FILE) then
    ExceptIntf.AdditionalAttachments.Add(ExeDir + SETTINGS_FILE, '', CrashFile);
end;


end.
