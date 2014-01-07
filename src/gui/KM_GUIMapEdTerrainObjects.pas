unit KM_GUIMapEdTerrainObjects;
{$I KaM_Remake.inc}
interface
uses
   Math, SysUtils,
   KM_Controls, KM_Defaults, KM_Pics;

type
  TKMMapEdTerrainObjects = class
  private
    //Objects in MapElem are placed sparsely, so we need to compact them
    //to use in MapEd palette
    fLastObject: Byte;
    fCountCompact: Integer;
    fCompactToMapElem: array [Byte] of Byte; //Pointers to valid MapElem's
    fMapElemToCompact: array [Byte] of Byte; //Pointers of valid MapElem's back to map objects. (reverse lookup to one above) 256 is no object.

    procedure CompactMapElements;
    procedure ObjectsChange(Sender: TObject);
    procedure ObjectsRefresh(Sender: TObject);
  protected
    Panel_Objects: TKMPanel;
    ObjectErase: TKMButtonFlat;
    ObjectBlock: TKMButtonFlat;
    ObjectsTable: array [0..8] of TKMButtonFlat;
    ObjectsScroll: TKMScrollBar;
  public
    constructor Create(aParent: TKMPanel);

    procedure Show;
    function Visible: Boolean;
    procedure Hide;
  end;


implementation
uses
  KM_Resource, KM_ResFonts, KM_ResMapElements, KM_ResTexts,
  KM_GameCursor, KM_RenderUI, KM_InterfaceGame;


{ TKMMapEdTerrainObjects }
constructor TKMMapEdTerrainObjects.Create(aParent: TKMPanel);
var
  J,K: Integer;
begin
  inherited Create;

  CompactMapElements;

  Panel_Objects := TKMPanel.Create(aParent, 0, 28, TB_WIDTH, 400);
  TKMLabel.Create(Panel_Objects, 0, PAGE_TITLE_Y, TB_WIDTH, 0, gResTexts[TX_MAPED_OBJECTS], fnt_Outline, taCenter);
  ObjectsScroll := TKMScrollBar.Create(Panel_Objects, 0, 295, TB_WIDTH, 20, sa_Horizontal, bsGame);
  ObjectsScroll.MinValue := 0;
  ObjectsScroll.MaxValue := fCountCompact div 3 - 3;
  ObjectsScroll.Position := 0;
  ObjectsScroll.OnChange := ObjectsRefresh;
  for J := 0 to 2 do for K := 0 to 2 do
  begin
    ObjectsTable[J*3+K] := TKMButtonFlat.Create(Panel_Objects, J*65, 40+K*85,64,84,1,rxTrees); //RXid=1  // 1 2
    ObjectsTable[J*3+K].Tag := J*3+K; //Store ID
    ObjectsTable[J*3+K].OnClick := ObjectsChange;
    ObjectsTable[J*3+K].OnMouseWheel := ObjectsScroll.MouseWheel;
  end;
  ObjectErase := TKMButtonFlat.Create(Panel_Objects, 0, 8,32,32,340);
  ObjectErase.Hint := gResTexts[TX_MAPED_TERRAIN_OBJECTS_REMOVE];
  ObjectErase.Tag := 255; //no object
  ObjectErase.OnClick := ObjectsChange;

  ObjectBlock := TKMButtonFlat.Create(Panel_Objects, TB_WIDTH-32, 8,32,32,254,rxTrees);
  ObjectBlock.Hint := gResTexts[TX_MAPED_TERRAIN_OBJECTS_BLOCK];
  ObjectBlock.Tag := 61; //no object
  ObjectBlock.OnClick := ObjectsChange;
end;


//Map sparse objects into a tight lookup array
procedure TKMMapEdTerrainObjects.CompactMapElements;
var
  I: Integer;
begin
  fCountCompact := 0;
  for I := 0 to gResource.MapElements.Count - 1 do
  if (MapElem[I].Anim.Count > 0) and (MapElem[I].Anim.Step[1] > 0)
  and (MapElem[I].Stump = -1) and (I <> 61) then //Hide falling trees and invisible wall (61)
  begin
    fCompactToMapElem[fCountCompact] := I; //pointer
    fMapElemToCompact[I] := fCountCompact; //Reverse lookup
    Inc(fCountCompact);
  end;
end;


procedure TKMMapEdTerrainObjects.ObjectsChange(Sender: TObject);
var
  ObjID: Integer;
begin
  ObjID := ObjectsScroll.Position * 3 + TKMButtonFlat(Sender).Tag; //0..n-1

  //Skip indexes out of range
  if not InRange(ObjID, 0, fCountCompact - 1)
  and not (TKMButtonFlat(Sender).Tag = 255)
  and not (TKMButtonFlat(Sender).Tag = 61) then
    Exit;

  GameCursor.Mode := cmObjects;
  if TKMButtonFlat(Sender).Tag = 255 then
    //Erase
    GameCursor.Tag1 := 255
  else
  if TKMButtonFlat(Sender).Tag = 61 then
    //Block
    GameCursor.Tag1 := 61
  else
    //Object
    GameCursor.Tag1 := fCompactToMapElem[ObjID]; //0..n-1

  //Remember last selected object
  fLastObject := TKMButtonFlat(Sender).Tag;

  ObjectsRefresh(nil);
end;


procedure TKMMapEdTerrainObjects.ObjectsRefresh(Sender: TObject);
var
  I: Integer;
  ObjID: Integer;
begin
  for I := 0 to 8 do
  begin
    ObjID := ObjectsScroll.Position * 3 + I;
    if ObjID < fCountCompact then
    begin
      ObjectsTable[I].TexID := MapElem[fCompactToMapElem[ObjID]].Anim.Step[1] + 1;
      ObjectsTable[I].Caption := IntToStr(fCompactToMapElem[ObjID]);
      ObjectsTable[I].Enable;
    end
    else
    begin
      ObjectsTable[I].TexID := 0;
      ObjectsTable[I].Caption := '';
      ObjectsTable[I].Disable;
    end;
    //Mark the selected one using reverse lookup
    ObjectsTable[I].Down := (GameCursor.Mode = cmObjects) and (ObjID = fMapElemToCompact[GameCursor.Tag1]);
  end;

  ObjectErase.Down := (GameCursor.Mode = cmObjects) and (GameCursor.Tag1 = 255); //or delete button
  ObjectBlock.Down := (GameCursor.Mode = cmObjects) and (GameCursor.Tag1 = 61); //or block button
end;


procedure TKMMapEdTerrainObjects.Show;
begin
  case fLastObject of
    61:   ObjectsChange(ObjectBlock);
    255:  ObjectsChange(ObjectErase);
    else  ObjectsChange(ObjectsTable[fLastObject]);
  end;
  Panel_Objects.Show;
end;


function TKMMapEdTerrainObjects.Visible: Boolean;
begin
  Result := Panel_Objects.Visible;
end;


procedure TKMMapEdTerrainObjects.Hide;
begin
  Panel_Objects.Hide;
end;


end.
