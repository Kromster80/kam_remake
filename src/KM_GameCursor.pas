unit KM_GameCursor;
interface
uses
  Classes, KM_Defaults, KM_Points;


var
  gGameCursor: record
    Pixel: TKMPoint;      //Cursor position in screen-space
    Float: TKMPointF;     //Precise cursor position in map coords
    Cell: TKMPoint;       //Cursor position cell
    SState: TShiftState;  //Thats actually used to see if Left or Right mouse button is pressed
    Mode: TKMCursorMode;  //Modes used in game (building, unit, road, etc..)
    Tag1: Byte;           //Tag to know building type, unit type etc.
    CellAdjustment: TKMPoint; //used to adjust actual Cursor Cell
    ObjectUID: Integer;   //Object found below cursor

    MapEdDir: Byte;
    MapEdShape: (hsCircle, hsSquare);
    MapEdSlope: Byte;
    MapEdSize: Byte;
    MapEdSpeed: Byte;
  end;


implementation


end.
