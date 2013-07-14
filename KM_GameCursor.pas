unit KM_GameCursor;
interface
uses Classes, KM_Defaults, KM_Points;


var
  GameCursor: record
    Float: TKMPointF;     //Precise cursor position in map coords
    Cell: TKMPoint;       //Cursor position cell
    SState: TShiftState;  //Thats actually used to see if Left or Right mouse button is pressed
    Mode: TKMCursorMode;  //Modes used in game (building, unit, road, etc..)
    Tag1: Byte;           //Tag to know building type, unit type etc.

    MapEdDir: Byte;
    MapEdShape: (hsCircle, hsSquare);
    MapEdSlope: Byte;
    MapEdSize: Byte;
    MapEdSpeed: Byte;
  end;


implementation


end.
