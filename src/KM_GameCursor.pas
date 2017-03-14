unit KM_GameCursor;
interface
uses
  Classes, KM_Defaults, KM_Points;


type
  TKMGameCursor = class
  private
    fMode: TKMCursorMode; //Modes used in game (building, unit, road, etc..)
    procedure Reset;
    procedure SetMode(aMode: TKMCursorMode);
  public
    Pixel: TKMPoint;      //Cursor position in screen-space
    Float: TKMPointF;     //Precise cursor position in map coords
    Cell: TKMPoint;       //Cursor position cell
    PrevCell: TKMPoint;   //Cursor previous position cell
    SState: TShiftState;  //Thats actually used to see if Left or Right mouse button is pressed

    Tag1: Byte;           //Tag to know building type, unit type etc.
    CellAdjustment: TKMPoint; //used to adjust actual Cursor Cell
    ObjectUID: Integer;   //Object found below cursor

    MapEdDir: Byte;
    MapEdShape: (hsCircle, hsSquare);
    MapEdSlope: Byte;
    MapEdSize: Byte;
    MapEdSpeed: Byte;

    constructor Create;
    property Mode: TKMCursorMode read fMode write SetMode;
  end;


var
  gGameCursor: TKMGameCursor;


implementation


{TKMGameCursor}
constructor TKMGameCursor.Create;
begin
  Reset;
end;


procedure TKMGameCursor.Reset;
begin
  CellAdjustment := KMPOINT_ZERO;
  // Actually we need reset all fields when changing mode,
  // but lets reset only CellAdjustment for now, need to do lots of tests for other fields
end;


procedure TKMGameCursor.SetMode(aMode: TKMCursorMode);
begin
  fMode := aMode;
  Reset;
end;

end.
