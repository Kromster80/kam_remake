unit KM_InterfaceDefaults;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  Controls, Classes,
  KM_Controls;


type
  TChatMode = (cmAll, cmTeam, cmWhisper);
  TKMMenuPage = (gpMainMenu,
                    gpSinglePlayer,
                      gpCampaign,
                      gpCampSelect,
                      gpSingleMap,
                      gpLoad,
                    gpMultiplayer,
                      gpLobby,
                    gpReplays,
                    gpMapEditor,
                    gpOptions,
                    gpCredits,
                  gpLoading,
                  gpError );
  TGUIEvent = procedure (Sender: TObject; Dest: TKMMenuPage) of object;
  TGUIEventText = procedure (Dest: TKMMenuPage; aText: UnicodeString = '') of object;

  TKMUserInterfaceCommon = class
  protected
    fMyControls: TKMMasterControl;
    Panel_Main: TKMPanel;
  public
    constructor Create(aScreenX, aScreenY: Word);
    destructor Destroy; override;

    property MyControls: TKMMasterControl read fMyControls;
    procedure ExportPages(aPath: string); virtual; abstract;

    procedure KeyDown(Key: Word; Shift: TShiftState); virtual; abstract;
    procedure KeyPress(Key: Char);
    procedure KeyUp(Key: Word; Shift: TShiftState); virtual; abstract;
    //Child classes don't pass these events to controls depending on their state
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); virtual; abstract;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); virtual; abstract;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); virtual; abstract;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer); virtual;
    procedure Resize(X,Y: Word); virtual;
    procedure UpdateState(aTickCount: Cardinal); virtual; abstract;
    procedure Paint; virtual;
  end;


const
  //Options sliders
  OPT_SLIDER_MIN = 0;
  OPT_SLIDER_MAX = 20;


implementation


{ TKMUserInterface }
constructor TKMUserInterfaceCommon.Create(aScreenX, aScreenY: Word);
begin
  inherited Create;

  fMyControls := TKMMasterControl.Create;

  //Parent Panel for whole UI
  Panel_Main := TKMPanel.Create(fMyControls, 0, 0, aScreenX, aScreenY);
end;


destructor TKMUserInterfaceCommon.Destroy;
begin
  fMyControls.Free;
  inherited;
end;


procedure TKMUserInterfaceCommon.KeyPress(Key: Char);
begin
  fMyControls.KeyPress(Key);
end;


procedure TKMUserInterfaceCommon.MouseWheel(Shift: TShiftState; WheelDelta, X, Y: Integer);
begin
  fMyControls.MouseWheel(X, Y, WheelDelta);
end;


procedure TKMUserInterfaceCommon.Resize(X, Y: Word);
begin
  Panel_Main.Width := X;
  Panel_Main.Height := Y;
end;


procedure TKMUserInterfaceCommon.Paint;
begin
  fMyControls.Paint;
end;


end.
