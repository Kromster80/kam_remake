unit KM_Exceptions;

interface
uses
  SysUtils, MadExcept, madNVAssistant;

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
  KM_Log, KM_Defaults, KM_TextLibrary, KM_Points, KM_Game, KM_CommonClasses;

constructor TKMExceptions.Create;
begin
  inherited;
  OnAssistantCreate := DoCreateAssistant;
  RegisterExceptionHandler(DoException, stTrySyncCallAlways);
  MESettings.HttpServer := 'http://lewin.hodgman.id.au/kam_remake_master_server/crashupload.php?rev='+GAME_REVISION;
end;


procedure TKMExceptions.LoadTranslation;
begin
  MESettings.ExceptMsg := fTextLibrary[TX_ERROR_MESSAGE];
  MESettings.SendBtnCaption := fTextLibrary[TX_ERROR_SEND];
  MESettings.ShowBtnCaption := fTextLibrary[TX_ERROR_SHOW_DATA];
  MESettings.CloseBtnCaption := fTextLibrary[TX_ERROR_TERMINATE];
  MESettings.ContinueBtnCaption := fTextLibrary[TX_ERROR_RESUME];
  MESettings.SendBoxTitle := fTextLibrary[TX_ERROR_SENDING_TITLE];
  MESettings.SendMailMsg := fTextLibrary[TX_ERROR_SENDING_DATA];
  MESettings.SendAttachMsg := fTextLibrary[TX_ERROR_SENDING_DATA];
  MESettings.SendFinalizeMsg := fTextLibrary[TX_ERROR_SENDING_DATA];
  MESettings.PrepareAttachMsg := fTextLibrary[TX_ERROR_SENDING_CONNECTING];
  MESettings.ConnectMsg := fTextLibrary[TX_ERROR_SENDING_CONNECTING];
  MESettings.AuthMsg := fTextLibrary[TX_ERROR_SENDING_CONNECTING];
  MESettings.SendFailureMsg := fTextLibrary[TX_ERROR_SENDING_FAILED];
end;


//We have to load the translations for the send assistant when it is created, that's the only way
procedure DoCreateAssistant(const assistant: INVAssistant; const exception: IUnknown);
begin
  if fTextLibrary = nil then Exit; //If the exception happens before translations are loaded
  if assistant.FormCount = 3 then //That's how we know it's the send assistant, not one of the others (which we currently don't use)
  begin
    assistant.Title := fTextLibrary[TX_ERROR_SEND];
    //Contact details form
    assistant.Forms[0].Message := fTextLibrary[TX_ERROR_SEND_CONTACT];
    assistant.Forms[0].nvLabel('Label1').Caption := fTextLibrary[TX_ERROR_SEND_NAME];
    assistant.Forms[0].nvLabel('Label2').Caption := fTextLibrary[TX_ERROR_SEND_EMAIL];
    assistant.Forms[0].nvCheckBox('MemCheck').Caption := fTextLibrary[TX_ERROR_SEND_REMEMBER];
    assistant.Forms[0].ContinueButton.Caption := fTextLibrary[TX_ERROR_SEND_NEXT];
    assistant.Forms[0].CancelButton.Caption := fTextLibrary[TX_ERROR_SEND_CANCEL];

    //Error details form
    assistant.Forms[1].Message := fTextLibrary[TX_ERROR_SEND_DETAILS];
    assistant.Forms[1].nvLabel('Label1').Caption := fTextLibrary[TX_ERROR_SEND_DETAILS_MESSAGE];
    assistant.Forms[1].ContinueButton.Caption := fTextLibrary[TX_ERROR_SEND_NEXT];
    assistant.Forms[1].CancelButton.Caption := fTextLibrary[TX_ERROR_SEND_CANCEL];

    //Screenshot form
    assistant.Forms[2].Message := fTextLibrary[TX_ERROR_SEND_SCREENSHOT];
    assistant.Forms[2].nvCheckBox('AttachCheck').Caption := fTextLibrary[TX_ERROR_SEND_SCREENSHOT_MESSAGE];
    assistant.Forms[2].nvLabel('Label1').Caption := fTextLibrary[TX_ERROR_SEND_SCREENSHOT_EDIT];
    assistant.Forms[2].ContinueButton.Caption := fTextLibrary[TX_ERROR_SEND_SEND];
    assistant.Forms[2].CancelButton.Caption := fTextLibrary[TX_ERROR_SEND_CANCEL];
  end;
end;


procedure TKMExceptions.DoException(const ExceptIntf: IMEException; var Handled: boolean);
var LogMessage, CrashFile: string;
begin
  if fLog = nil then Exit; //Could crash very early before even the log file is created
  //It's nice to know when the exception happened in our log if the user decides to play on and sends the report later
  LogMessage := 'Exception occured: '+ExceptIntf.ExceptClass+': '+ExceptIntf.ExceptMessage;
  if ExceptIntf.ExceptObject is ELocError then
    LogMessage := LogMessage + ' at location '+TypeToString(ELocError(ExceptIntf.ExceptObject).Loc);
  fLog.AppendLog(LogMessage);


  //We want to add some of our own files to the report
  CrashFile := 'KaM_Crash_' + GAME_REVISION + '_' + FormatDateTime('yyyy-mm-dd_hh-nn-ss', Now) + '.zip';
  MESettings.BugReportZip := CrashFile; //Exception info also goes in the zip
  MESettings.ScreenShotZip := CrashFile; //Screenshot also goes in the zip

  if fGame <> nil then fGame.AttachCrashReport(ExceptIntf, CrashFile);

  //Do the log after fGame because fGame adds stuff to the log
  if fLog <> nil then ExceptIntf.AdditionalAttachments.Add(fLog.LogPath, '', CrashFile);

  //Do settings here not in fGame because we could crash before fGame is created
  if FileExists(ExeDir + SETTINGS_FILE) then
    ExceptIntf.AdditionalAttachments.Add(ExeDir + SETTINGS_FILE, '', CrashFile);
end;


end.
