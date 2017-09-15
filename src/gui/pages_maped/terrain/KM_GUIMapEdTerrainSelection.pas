unit KM_GUIMapEdTerrainSelection;
{$I KaM_Remake.inc}
interface
uses
   Math, SysUtils,
   KM_Controls, KM_Defaults;


type
  TKMMapEdTerrainSelection = class
  private
    procedure SelectionClick(Sender: TObject);
  protected
    Panel_Selection: TKMPanel;
    Button_SelectCopy: TKMButton;
    Button_SelectPaste: TKMButton;
    Button_SelectPasteApply: TKMButton;
    Button_SelectPasteCancel: TKMButton;
    Button_SelectFlipH, Button_SelectFlipV: TKMButton;
  public
    constructor Create(aParent: TKMPanel);

    procedure Show;
    function Visible: Boolean;
    procedure Hide;
    procedure UpdateState;
  end;


implementation
uses
  KM_ResFonts, KM_ResTexts,
  KM_Game, KM_GameCursor, KM_RenderUI,
  KM_TerrainSelection,
  KM_InterfaceGame;


{ TKMMapEdTerrainSelection }
constructor TKMMapEdTerrainSelection.Create(aParent: TKMPanel);
begin
  inherited Create;

  Panel_Selection := TKMPanel.Create(aParent, 0, 28, TB_WIDTH, 400);

  TKMLabel.Create(Panel_Selection, 0, PAGE_TITLE_Y, TB_WIDTH, 0, gResTexts[TX_MAPED_COPY_TITLE], fnt_Outline, taCenter);
  Button_SelectCopy := TKMButton.Create(Panel_Selection, 10, 30, TB_WIDTH - 20, 20, gResTexts[TX_MAPED_COPY], bsGame);
  Button_SelectCopy.Hint := gResTexts[TX_MAPED_COPY_COPY_HINT];
  Button_SelectCopy.OnClick := SelectionClick;

  Button_SelectPaste := TKMButton.Create(Panel_Selection, 10, 60, TB_WIDTH - 20, 20, gResTexts[TX_MAPED_PASTE], bsGame);
  Button_SelectPaste.Hint := gResTexts[TX_MAPED_COPY_PASTE_HINT];
  Button_SelectPaste.OnClick := SelectionClick;

  Button_SelectPasteApply := TKMButton.Create(Panel_Selection, 10, 90, TB_WIDTH - 20, 20, gResTexts[TX_MAPED_PASTE_APPLY], bsGame);
  Button_SelectPasteApply.Hint := gResTexts[TX_MAPED_COPY_PASTE_HINT];
  Button_SelectPasteApply.OnClick := SelectionClick;

  Button_SelectPasteCancel := TKMButton.Create(Panel_Selection, 10, 120, TB_WIDTH - 20, 20, gResTexts[TX_MAPED_PASTE_CANCEL], bsGame);
  Button_SelectPasteCancel.Hint := gResTexts[TX_MAPED_COPY_PASTE_HINT];
  Button_SelectPasteCancel.OnClick := SelectionClick;

  Button_SelectFlipH := TKMButton.Create(Panel_Selection, 10, 180, TB_WIDTH - 20, 20, gResTexts[TX_MAPED_COPY_PASTE_HFLIP], bsGame);
  Button_SelectFlipH.Hint := gResTexts[TX_MAPED_COPY_PASTE_HFLIP_HINT];
  Button_SelectFlipH.OnClick := SelectionClick;

  Button_SelectFlipV := TKMButton.Create(Panel_Selection, 10, 210, TB_WIDTH - 20, 20, gResTexts[TX_MAPED_COPY_PASTE_VFLIP], bsGame);
  Button_SelectFlipV.Hint := gResTexts[TX_MAPED_COPY_PASTE_VFLIP_HINT];
  Button_SelectFlipV.OnClick := SelectionClick;

  with TKMLabel.Create(Panel_Selection, 8, 250, TB_WIDTH-16, 80, gResTexts[TX_MAPED_COPY_SELECT_HINT], fnt_Grey, taLeft) do
    AutoWrap := True;
end;


procedure TKMMapEdTerrainSelection.SelectionClick(Sender: TObject);
begin
  gGameCursor.Mode := cmSelection;

  if Sender = Button_SelectCopy then
  begin
    //Copy selection into cursor
    gGame.MapEditor.Selection.Selection_Copy;
    Button_SelectPaste.Enabled := gGame.MapEditor.Selection.Selection_DataInBuffer;
  end
  else
  if Sender = Button_SelectPaste then
  begin
    //Paste selection
    gGame.MapEditor.Selection.Selection_PasteBegin;

    Button_SelectPasteApply.Enable;
    Button_SelectPasteCancel.Enable;
    Button_SelectCopy.Disable;
    Button_SelectPaste.Disable;
    Button_SelectFlipH.Disable;
    Button_SelectFlipV.Disable;
  end
  else
  if Sender = Button_SelectPasteApply then
  begin
    //Apply paste
    gGame.MapEditor.Selection.Selection_PasteApply;
    gGame.MapEditor.TerrainPainter.MakeCheckpoint;

    Button_SelectPasteApply.Disable;
    Button_SelectPasteCancel.Disable;
    Button_SelectCopy.Enable;
    Button_SelectPaste.Enable;
    Button_SelectFlipH.Enable;
    Button_SelectFlipV.Enable;
  end
  else
  if Sender = Button_SelectPasteCancel then
  begin
    //Cancel pasting
    gGame.MapEditor.Selection.Selection_PasteCancel;
    Button_SelectPasteApply.Disable;
    Button_SelectPasteCancel.Disable;
    Button_SelectCopy.Enable;
    Button_SelectPaste.Enable;
    Button_SelectFlipH.Enable;
    Button_SelectFlipV.Enable;
  end
  else
  if Sender = Button_SelectFlipH then
  begin
    //Flip selected
    gGame.MapEditor.Selection.Selection_Flip(fa_Horizontal);
    gGame.MapEditor.TerrainPainter.MakeCheckpoint;
  end
  else
  if Sender = Button_SelectFlipV then
  begin
    //Flip selected
    gGame.MapEditor.Selection.Selection_Flip(fa_Vertical);
    gGame.MapEditor.TerrainPainter.MakeCheckpoint;
  end;
end;


procedure TKMMapEdTerrainSelection.Show;
begin
  gGameCursor.Mode := cmSelection;
  gGame.MapEditor.Selection.Selection_PasteCancel; //Could be leftover from last time we were visible

  Button_SelectPasteApply.Disable;
  Button_SelectPasteCancel.Disable;
  Button_SelectCopy.Enable;
  Button_SelectFlipH.Enable;
  Button_SelectFlipV.Enable;
  Button_SelectPaste.Enabled := gGame.MapEditor.Selection.Selection_DataInBuffer;

  Panel_Selection.Show;
end;


function TKMMapEdTerrainSelection.Visible: Boolean;
begin
  Result := Panel_Selection.Visible;
end;


procedure TKMMapEdTerrainSelection.Hide;
begin
  Panel_Selection.Hide;
end;


procedure TKMMapEdTerrainSelection.UpdateState;
begin
  Button_SelectPaste.Enabled := gGame.MapEditor.Selection.Selection_DataInBuffer;
end;


end.
