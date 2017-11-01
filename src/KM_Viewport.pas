unit KM_Viewport;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  KM_CommonClasses, KM_Points;


type
  TKMViewport = class
  private
    fMapX, fMapY: Word;
    fTopHill: Single;
    fPosition: TKMPointF;
    fScrolling: Boolean;
    fScrollStarted: Cardinal;
    fViewportClip: TPoint;
    fViewRect: TKMRect;
    fZoom: Single;
    fPanTo, fPanFrom: TKMPointF;
    fPanImmidiately : Boolean;
    fPanDuration, fPanProgress: Cardinal;
    function GetPosition: TKMPointF;
    procedure SetPosition(Value: TKMPointF);
    procedure SetZoom(aZoom: Single);
  public
    ScrollKeyLeft, ScrollKeyRight, ScrollKeyUp, ScrollKeyDown, ZoomKeyIn, ZoomKeyOut: boolean;
    constructor Create(aWidth, aHeight: Integer);

    property Position: TKMPointF read GetPosition write SetPosition;
    property Scrolling: Boolean read fScrolling;
    property ViewportClip: TPoint read fViewportClip;
    property ViewRect: TKMRect read fViewRect;
    property Zoom: Single read fZoom write SetZoom;

    procedure ResetZoom;
    procedure Resize(NewWidth, NewHeight: Integer);
    procedure ResizeMap(aMapX, aMapY: Word; aTopHill: Single);
    function GetClip: TKMRect; //returns visible area dimensions in map space
    function GetMinimapClip: TKMRect;
    procedure ReleaseScrollKeys;
    procedure GameSpeedChanged(aFromSpeed, aToSpeed: Single);
    function MapToScreen(const aMapLoc: TKMPointF): TKMPoint;
    procedure PanTo(const aLoc: TKMPointF; aTicksCnt: Cardinal);

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    procedure UpdateStateIdle(aFrameTime: Cardinal; aInCinematic: Boolean);
  end;


implementation
uses
  Math, KromUtils,
  KM_Resource, KM_ResCursors,
  KM_Main, KM_GameApp, KM_Game, KM_Sound,
  KM_Defaults, KM_CommonUtils;


{ TKMViewport }
constructor TKMViewport.Create(aWidth, aHeight: Integer);
begin
  inherited Create;

  fMapX := 1; //Avoid division by 0
  fMapY := 1; //Avoid division by 0

  fZoom := 1;
  ReleaseScrollKeys;
  gSoundPlayer.UpdateListener(fPosition.X, fPosition.Y);
  if gScriptSounds <> nil then
    gScriptSounds.UpdateListener(fPosition.X, fPosition.Y);
  Resize(aWidth, aHeight);
end;


procedure TKMViewport.SetZoom(aZoom: Single);
begin
  fZoom := EnsureRange(aZoom, 0.01, 8);
  //Limit the zoom to within the map boundaries
  if fViewportClip.X/CELL_SIZE_PX/fZoom > fMapX then fZoom := fViewportClip.X/CELL_SIZE_PX/(fMapX-1);
  if fViewportClip.Y/CELL_SIZE_PX/fZoom > fMapY then fZoom := fViewportClip.Y/CELL_SIZE_PX/ fMapY;
  SetPosition(fPosition); //To ensure it sets the limits smoothly
end;


procedure TKMViewport.ResetZoom;
begin
  Zoom := 1;
end;


procedure TKMViewport.Resize(NewWidth, NewHeight: Integer);
begin
  fViewRect.Left   := TOOLBAR_WIDTH;
  fViewRect.Top    := 0;
  fViewRect.Right  := NewWidth;
  fViewRect.Bottom := NewHeight;

  fViewportClip.X := fViewRect.Right-fViewRect.Left;
  fViewportClip.Y := fViewRect.Bottom-fViewRect.Top;

  SetZoom(fZoom); //View size has changed and that affects Zoom restrictions
end;


procedure TKMViewport.ResizeMap(aMapX, aMapY: Word; aTopHill: Single);
begin
  fMapX := aMapX;
  fMapY := aMapY;
  fTopHill := aTopHill;
  SetPosition(fPosition); //EnsureRanges
end;


function TKMViewport.GetPosition: TKMPointF;
begin
  Result.X := EnsureRange(fPosition.X, 1, fMapX);
  Result.Y := EnsureRange(fPosition.Y, 1, fMapY);
end;


procedure TKMViewport.SetPosition(Value: TKMPointF);
var PadTop, TilesX, TilesY: Single;
begin
  PadTop := fTopHill + 0.75; //Leave place on top for highest hills + 1 unit

  TilesX := fViewportClip.X/2/CELL_SIZE_PX/fZoom;
  TilesY := fViewportClip.Y/2/CELL_SIZE_PX/fZoom;

  fPosition.X := EnsureRange(Value.X, TilesX, fMapX - TilesX - 1);
  fPosition.Y := EnsureRange(Value.Y, TilesY - PadTop, fMapY - TilesY - 1); //Top row should be visible
  gSoundPlayer.UpdateListener(fPosition.X, fPosition.Y);
  if gScriptSounds <> nil then
    gScriptSounds.UpdateListener(fPosition.X, fPosition.Y);
end;


//Acquire boundaries of area visible to user (including mountain tops from the lower tiles)
//TestViewportClipInset is for debug, allows to see if everything gets clipped correct
function TKMViewport.GetClip: TKMRect;
begin
  Result.Left   := Math.max(Round(fPosition.X-(fViewportClip.X/2-fViewRect.Left+TOOLBAR_WIDTH)/CELL_SIZE_PX/fZoom), 1);
  Result.Right  := Math.min(Round(fPosition.X+(fViewportClip.X/2+fViewRect.Left-TOOLBAR_WIDTH)/CELL_SIZE_PX/fZoom)+1, fMapX-1);
  Result.Top    := Math.max(Round(fPosition.Y-fViewportClip.Y/2/CELL_SIZE_PX/fZoom), 1);
  Result.Bottom := Math.min(Round(fPosition.Y+fViewportClip.Y/2/CELL_SIZE_PX/fZoom)+4, fMapY-1);

  if TEST_VIEW_CLIP_INSET then
    Result := KMRectGrow(Result, -5);
end;


//Same as above function but with some values changed to suit minimap
function TKMViewport.GetMinimapClip: TKMRect;
begin
  Result.Left   := Math.max(round(fPosition.X-(fViewportClip.X/2-fViewRect.Left+TOOLBAR_WIDTH)/CELL_SIZE_PX/fZoom)+1, 1);
  Result.Right  := Math.min(round(fPosition.X+(fViewportClip.X/2+fViewRect.Left-TOOLBAR_WIDTH)/CELL_SIZE_PX/fZoom)+1, fMapX);
  Result.Top    := Math.max(round(fPosition.Y-fViewportClip.Y/2/CELL_SIZE_PX/fZoom)+2, 1);
  Result.Bottom := Math.min(round(fPosition.Y+fViewportClip.Y/2/CELL_SIZE_PX/fZoom), fMapY);
end;


procedure TKMViewport.ReleaseScrollKeys;
begin
  ScrollKeyLeft  := false;
  ScrollKeyRight := false;
  ScrollKeyUp    := false;
  ScrollKeyDown  := false;
  ZoomKeyIn      := false;
  ZoomKeyOut     := false;
end;


procedure TKMViewport.GameSpeedChanged(aFromSpeed, aToSpeed: Single);
var
  Koef: Single;
begin
  if (aFromSpeed > 0) and (fPanDuration > 0) and not fPanImmidiately then
  begin
    //Update PanDuration and Progress due to new game speed
    Koef := aFromSpeed / aToSpeed;
    fPanDuration := Round((fPanDuration - fPanProgress) * Koef);
    fPanProgress := Round(fPanProgress * Koef);
    Inc(fPanDuration, fPanProgress);
  end;
end;


function TKMViewport.MapToScreen(const aMapLoc: TKMPointF): TKMPoint;
begin
  Result.X := Round((aMapLoc.X - fPosition.X) * CELL_SIZE_PX * fZoom + fViewRect.Right / 2 + TOOLBAR_WIDTH / 2);
  Result.Y := Round((aMapLoc.Y - fPosition.Y) * CELL_SIZE_PX * fZoom + fViewRect.Bottom / 2);
end;


procedure TKMViewport.PanTo(const aLoc: TKMPointF; aTicksCnt: Cardinal);
begin
  fPanTo := aLoc;
  fPanFrom := fPosition;
  fPanImmidiately := aTicksCnt = 0;
  fPanProgress := 0;
  fPanDuration := Round(aTicksCnt * gGame.GameTickDuration);
  //Panning will be skipped when duration is zero
  if fPanImmidiately then
    SetPosition(aLoc);
end;


//Here we must test each edge to see if we need to scroll in that direction
//We scroll at SCROLLSPEED per 100 ms. That constant is defined in KM_Defaults
procedure TKMViewport.UpdateStateIdle(aFrameTime: Cardinal; aInCinematic: Boolean);
const
  SCROLL_ACCEL_TIME = 400; // Time in ms that scrolling will be affected by acceleration
  SCROLL_FLEX = 4;         // Number of pixels either side of the edge of the screen which will count as scrolling
  DirectionsBitfield: array [0..15] of TKMCursor = (
    kmc_Default, kmc_Scroll6, kmc_Scroll0, kmc_Scroll7,
    kmc_Scroll2, kmc_Default, kmc_Scroll1, kmc_Default,
    kmc_Scroll4, kmc_Scroll5, kmc_Default, kmc_Default,
    kmc_Scroll3, kmc_Default, kmc_Default, kmc_Default);
var
  TimeSinceStarted: Cardinal;
  ScrollAdv, ZoomAdv: Single;
  CursorPoint: TKMPoint;
  ScreenBounds: TRect;
  I: Byte;
  MousePos: TPoint;
begin
  //Cinematics do not allow normal scrolling. The camera will be set and panned with script commands
  if aInCinematic then
  begin
    if not fPanImmidiately then
    begin
      Inc(fPanProgress, aFrameTime);
      if fPanProgress >= fPanDuration then
      begin
        SetPosition(fPanTo); //Pan ended
        fPanImmidiately := True; //Not panning
        fPanDuration := 0;
      end
      else
        SetPosition(KMLerp(fPanFrom, fPanTo, fPanProgress / fPanDuration));
    end;
    Exit;
  end;

  {$IFDEF MSWindows}
    //Don't use Mouse.CursorPos, it will throw an EOSError (code 5: access denied) in some cases when
    //the OS doesn't want us controling the mouse, or possibly when the mouse is reset in some way.
    //It happens for me in Windows 7 every time I press CTRL+ALT+DEL with the game running.
    //On Windows XP I get "call to an OS function failed" instead.
    if not Windows.GetCursorPos(MousePos) then Exit;
  {$ENDIF}
  {$IFDEF Unix}
    MousePos := Mouse.CursorPos;
  {$ENDIF}
  if not gMain.GetScreenBounds(ScreenBounds) then Exit;

  //With multiple monitors the cursor position can be outside of this screen, which makes scrolling too fast
  CursorPoint.X := EnsureRange(MousePos.X, ScreenBounds.Left, ScreenBounds.Right );
  CursorPoint.Y := EnsureRange(MousePos.Y, ScreenBounds.Top , ScreenBounds.Bottom);

  //Do not do scrolling when the form is not focused (player has switched to another application)
  if not gMain.IsFormActive or
    (not ScrollKeyLeft  and
     not ScrollKeyUp    and
     not ScrollKeyRight and
     not ScrollKeyDown  and
     not ZoomKeyIn      and
     not ZoomKeyOut     and
     not (CursorPoint.X <= ScreenBounds.Left + SCROLL_FLEX) and
     not (CursorPoint.Y <= ScreenBounds.Top + SCROLL_FLEX) and
     not (CursorPoint.X >= ScreenBounds.Right -1-SCROLL_FLEX) and
     not (CursorPoint.Y >= ScreenBounds.Bottom-1-SCROLL_FLEX)) then
  begin
    //Stop the scrolling (e.g. if the form loses focus due to other application popping up)
    ReleaseScrollKeys;
    fScrolling := False;

    if (gRes.Cursors.Cursor in [kmc_Scroll0 .. kmc_Scroll7]) then
      gRes.Cursors.Cursor := kmc_Default;

    fScrollStarted := 0;
    Exit;
  end;

  // Both advancements have minimal value > 0
  ScrollAdv := (0.5 + gGameApp.GameSettings.ScrollSpeed / 5) * aFrameTime / 100;
  ZoomAdv := (0.2 + gGameApp.GameSettings.ScrollSpeed / 20) * aFrameTime / 1000;

  if SCROLL_ACCEL then
  begin
    if fScrollStarted = 0 then
      fScrollStarted := TimeGet;
    TimeSinceStarted := GetTimeSince(fScrollStarted);
    if TimeSinceStarted < SCROLL_ACCEL_TIME then
      ScrollAdv := Mix(ScrollAdv, 0, TimeSinceStarted / SCROLL_ACCEL_TIME);
  end;

  I := 0; //That is our bitfield variable for directions, 0..12 range
  //    3 2 6  These are directions
  //    1 * 4  They are converted from bitfield to actual cursor constants, see Arr array
  //    9 8 12

  //Keys
  if ScrollKeyLeft  then fPosition.X := fPosition.X - ScrollAdv;
  if ScrollKeyUp    then fPosition.Y := fPosition.Y - ScrollAdv;
  if ScrollKeyRight then fPosition.X := fPosition.X + ScrollAdv;
  if ScrollKeyDown  then fPosition.Y := fPosition.Y + ScrollAdv;
  if ZoomKeyIn      then fZoom := fZoom + ZoomAdv;
  if ZoomKeyOut     then fZoom := fZoom - ZoomAdv;
  //Mouse
  if CursorPoint.X <= ScreenBounds.Left   + SCROLL_FLEX then begin inc(I,1); fPosition.X := fPosition.X - ScrollAdv*(1+(ScreenBounds.Left   - CursorPoint.X)/SCROLL_FLEX); end;
  if CursorPoint.Y <= ScreenBounds.Top    + SCROLL_FLEX then begin inc(I,2); fPosition.Y := fPosition.Y - ScrollAdv*(1+(ScreenBounds.Top    - CursorPoint.Y)/SCROLL_FLEX); end;
  if CursorPoint.X >= ScreenBounds.Right -1-SCROLL_FLEX then begin inc(I,4); fPosition.X := fPosition.X + ScrollAdv*(1-(ScreenBounds.Right -1-CursorPoint.X)/SCROLL_FLEX); end;
  if CursorPoint.Y >= ScreenBounds.Bottom-1-SCROLL_FLEX then begin inc(I,8); fPosition.Y := fPosition.Y + ScrollAdv*(1-(ScreenBounds.Bottom-1-CursorPoint.Y)/SCROLL_FLEX); end;

  //Now do actual the scrolling, if needed
  fScrolling := I <> 0;
  if fScrolling then
    gRes.Cursors.Cursor := DirectionsBitfield[I] //Sample cursor type from bitfield value
  else
    if (gRes.Cursors.Cursor in [kmc_Scroll0 .. kmc_Scroll7]) then
      gRes.Cursors.Cursor := kmc_Default;

  SetZoom(fZoom); //EnsureRanges
  SetPosition(fPosition); //EnsureRanges
end;


procedure TKMViewport.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.WriteA('Viewport');
  SaveStream.Write(fMapX);
  SaveStream.Write(fMapY);
  SaveStream.Write(fPosition);
  //Zoom is reset to 1 by default
end;


procedure TKMViewport.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.ReadAssert('Viewport');
  //Load map dimensions then Position so it could be fit to map
  LoadStream.Read(fMapX);
  LoadStream.Read(fMapY);
  LoadStream.Read(fPosition);

  SetPosition(fPosition); //EnsureRanges
end;


end.
