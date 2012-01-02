unit KM_ResourceSprites;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF WDC} PNGImage, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}

  Classes, Math, SysUtils,
  KM_CommonEvents,
  KM_ResourceCursors,
  KM_ResourceHouse,
  KM_ResourcePalettes
  {$IFDEF FPC}, zstream {$ENDIF}
  {$IFDEF WDC}, ZLib {$ENDIF}

  {$IFDEF FPC}, BGRABitmap {$ENDIF};

const
  REMAKE_MENU_SPRITES_COUNT = 17; //Number of sprites loaded from RX7 (rxMenu)
  REMAKE_GAME_SPIRTES_COUNT = 347; //Number of sprites loaded from RX9 (rxGame)


type
  TRXType = (
    rxTrees,
    rxHouses,
    rxUnits,
    rxGui,
    rxGuiMain,
    rxGuiMainH,
    rxMenu, //Remake menu elements
    rxTiles, //Tiles
    rxGame); //Remake game sprites

  TKMSprites = class
  private
    fPalettes: TKMPalettes;
    fStepProgress: TEvent;
    fStepCaption: TStringEvent;
    procedure AllocateRX(aRT: TRXType; Count: Integer = 0);
    procedure ExpandRX(aRT: TRXType);
    procedure OverloadRX(aRT: TRXType);
    procedure MakeGFX(aRT: TRXType);
    procedure MakeGFX_AlphaTest(aHouseDat: TKMHouseDatCollection; aRT: TRXType);
    procedure ClearUnusedGFX(aRT: TRXType);
    function GetRXFileName(aRX: TRXType): string;
    procedure SaveRXX(aRT: TRXType; const aFileName:string);
  public
    constructor Create(aPalettes: TKMPalettes; aStepProgress: TEvent; aStepCaption: TStringEvent);
    procedure LoadMenuResources(aCursors: TKMCursors);
    procedure LoadGameResources(aHouseDat: TKMHouseDatCollection; aTileTex: Cardinal);
    procedure PackMenuRXX(const aFileName:string); //Used in RXX packer util (unused in Remake)
    procedure PackGameRXX(const aFileName:string);

    property FileName[aRX: TRXType]: string read GetRXFileName;
    function LoadRX(aRT: TRXType): Boolean; //Exposed for Exports
    function LoadRXX(aRT: TRXType): Boolean; //Exposed for Exports
  end;


var
  RXData: array [TRXType] of record
    Qty: Integer;
    Flag: array of Byte; //Sprite is valid
    Palleted: array of Boolean; //Sprite was overloaded from KaM RX so must be converted to RGBA
    Size: array of record X,Y: Word; end;
    Pivot: array of record x,y: Integer; end;
    Data: array of array of Byte;
    RGBA: array of array of Cardinal; //Expanded image
    Mask: array of array of Byte; //Mask for team colors
    HasMask: array of Boolean; //Mask for team colors
    HasTeamColors: Boolean;
  end;

  GFXData: array [TRXType] of array of record
    TexID,AltID: Cardinal; //AltID used for team colors
    u1,v1,u2,v2: Single;
    PxWidth,PxHeight: Word;
  end;


implementation
uses KromUtils, KM_Defaults, KM_Log, KM_Render, KM_TextLibrary;


type
  TRXLocation = (rlFileRX, rlFileRXX, rlFolder); //Location of sprites, RXfiles or folder with pngs
  TRXUsage = (ruMenu, ruGame); //Where sprites are used

  TRXInfo = record
    FileName: AnsiString; //Used for logging and filenames
    TeamColors: Boolean; //sprites should be generated with color masks
    AlphaTest: Boolean; //Alphatested gradients, used only for houses yet
    LoadFrom: TRXLocation;
    Usage: TRXUsage; //Menu and Game sprites are loaded separately
    OverrideCount: Word;
    LoadingTextID: Word;
  end;


var
  //This is internal detail, that noone should know or care outside of this unit
  RXInfo: array [TRXType] of TRXInfo = (
    (
      FileName: 'Trees';
      TeamColors: False;
      AlphaTest: False;
      LoadFrom: rlFileRX;
      Usage: ruGame;
      OverrideCount: 0;
      LoadingTextID: TX_MENU_LOADING_TREES;
    ),
    (
      FileName: 'Houses';
      TeamColors: True;
      AlphaTest: True;
      LoadFrom: rlFileRX;
      Usage: ruGame;
      OverrideCount: 0;
      LoadingTextID: TX_MENU_LOADING_HOUSES;
    ),
    (
      FileName: 'Units';
      TeamColors: True;
      AlphaTest: False;
      LoadFrom: rlFileRX;
      Usage: ruGame;
      OverrideCount: 7885; //Clip to 7885 sprites until we add TPR ballista/catapult support
      LoadingTextID: TX_MENU_LOADING_UNITS;
    ),
    (
      FileName: 'GUI';
      TeamColors: True;
      AlphaTest: False;
      LoadFrom: rlFileRX;
      Usage: ruMenu;
      OverrideCount: 0; //Required for unit scrolls and icons
      LoadingTextID: 0;
    ),
    (
      FileName: 'GUIMain';
      TeamColors: False;
      AlphaTest: False;
      LoadFrom: rlFileRX;
      Usage: ruMenu;
      OverrideCount: 0;
      LoadingTextID: 0;
    ),
    (
      FileName: 'GUIMainH';
      TeamColors: False;
      AlphaTest: False;
      LoadFrom: rlFileRX;
      Usage: ruMenu;
      OverrideCount: 0;
      LoadingTextID: 0;
    ),
    (
      FileName: 'RemakeMenu';
      TeamColors: False;
      AlphaTest: False;
      LoadFrom: rlFolder;
      Usage: ruMenu;
      OverrideCount: REMAKE_MENU_SPRITES_COUNT;
      LoadingTextID: 0;
    ),
    (
      FileName: 'Tileset';
      TeamColors: False;
      AlphaTest: False;
      LoadFrom: rlFolder;
      Usage: ruGame;
      OverrideCount: 256;
      LoadingTextID: TX_MENU_LOADING_TILESET;
    ),
    (
      FileName: 'RemakeGame';
      TeamColors: True;
      AlphaTest: False;
      LoadFrom: rlFolder;
      Usage: ruGame;
      OverrideCount: REMAKE_GAME_SPIRTES_COUNT;
      LoadingTextID: TX_MENU_LOADING_ADDITIONAL_SPRITES;
    ));


constructor TKMSprites.Create(aPalettes: TKMPalettes; aStepProgress: TEvent; aStepCaption: TStringEvent);
begin
  Inherited Create;
  fPalettes := aPalettes;
  fStepProgress := aStepProgress;
  fStepCaption := aStepCaption;
end;


function TKMSprites.GetRXFileName(aRX: TRXType): string;
begin
  Result := RXInfo[aRX].FileName;
end;


procedure TKMSprites.LoadMenuResources(aCursors: TKMCursors);
var RT: TRXType;
begin
  for RT := Low(TRXType) to High(TRXType) do
  if (RXInfo[RT].Usage = ruMenu) and (RXInfo[RT].LoadFrom in [rlFileRX,rlFileRXX]) then
  begin
    fStepCaption('Reading ' + RXInfo[RT].FileName + ' ...');
    if RXInfo[RT].LoadFrom = rlFileRXX then
      LoadRXX(RT)
    else
      LoadRX(RT);
    OverloadRX(RT); //Load RX data overrides

    if RT = rxGui then
    begin
      aCursors.MakeCursors;
      aCursors.Cursor := kmc_Default;
    end;

    MakeGFX(RT);
    ClearUnusedGFX(RT);

    fStepProgress;
  end;

  for RT := Low(TRXType) to High(TRXType) do
  if (RXInfo[RT].Usage = ruMenu) and (RXInfo[RT].LoadFrom = rlFolder) then
  begin
    fStepCaption('Reading ' + RXInfo[RT].FileName + ' ...');

    AllocateRX(RT, RXInfo[RT].OverrideCount);
    OverloadRX(RT); //Load sprites from PNGs
    MakeGFX(RT);
    ClearUnusedGFX(RT);

    fStepProgress;
  end;
end;


procedure TKMSprites.LoadGameResources(aHouseDat: TKMHouseDatCollection; aTileTex: Cardinal);
var i:integer; RT: TRXType; Loaded: boolean;
begin
  for RT := Low(TRXType) to High(TRXType) do
  if (RXInfo[RT].Usage = ruGame) and (RXInfo[RT].LoadFrom in [rlFileRX,rlFileRXX]) then
  begin
    //Skip loading for performance reasons during debug
    if ((RT = rxHouses) and not MAKE_HOUSE_SPRITES) or
       ((RT = rxUnits) and not MAKE_UNIT_SPRITES) then Continue;

    fStepCaption(fTextLibrary[RXInfo[RT].LoadingTextID]);

    fLog.AppendLog('Reading ' + RXInfo[RT].FileName + '.rx');
    if RXInfo[RT].LoadFrom = rlFileRXX then
      Loaded := LoadRXX(RT)
    else
      Loaded := LoadRX(RT);
    if Loaded then
    begin
      //Load new/updated sprites, except when we are using an RXX file
      if RXInfo[RT].LoadFrom <> rlFileRXX then OverloadRX(RT);
      MakeGFX(RT);

      //Alpha_tested sprites for houses. They come after MakeGFX cos they will replace above data
      if RXInfo[RT].AlphaTest then
        MakeGFX_AlphaTest(aHouseDat, RT);

      ClearUnusedGFX(RT);
    end;
  end;

  fLog.AppendLog('Reading folder');
  for RT := Low(TRXType) to High(TRXType) do
  if (RXInfo[RT].Usage = ruGame) and (RXInfo[RT].LoadFrom = rlFolder) then
  begin
    fStepCaption(fTextLibrary[RXInfo[RT].LoadingTextID]);
    AllocateRX(RT, RXInfo[RT].OverrideCount);
    OverloadRX(RT); //Load custom PNGs
    MakeGFX(RT);
    ClearUnusedGFX(RT);
  end;

  fLog.AppendLog('Preparing MiniMap colors...');
  AllocateRX(rxTiles, RXInfo[rxTiles].OverrideCount);
  //Generate UV coords
  for i:=0 to 255 do
  with GFXData[rxTiles, i+1] do
  begin
    TexID := aTileTex;
    v1 := (i div 16  ) / 16; //There are 16 tiles across the line
    u1 := (i mod 16  ) / 16;
    v2 := (i div 16+1) / 16;
    u2 := (i mod 16+1) / 16;
    PxWidth := 32;
    PxHeight := 32;
  end;
end;


procedure TKMSprites.PackMenuRXX(const aFileName:string);
begin
  AllocateRX(rxMenu, RXInfo[rxMenu].OverrideCount);
  OverloadRX(rxMenu); //Load sprites from PNGs
  SaveRXX(rxMenu, aFileName);
end;


procedure TKMSprites.PackGameRXX(const aFileName:string);
begin
  AllocateRX(rxGame, RXInfo[rxGame].OverrideCount);
  OverloadRX(rxGame); //Load sprites from PNGs
  SaveRXX(rxGame, aFileName);
end;


procedure TKMSprites.AllocateRX(aRT: TRXType; Count: Integer = 0);
begin
  if Count>0 then
    RXData[aRT].Qty := Count;

  Count := RXData[aRT].Qty+1;
  SetLength(GFXData[aRT],         Count);
  SetLength(RXData[aRT].Flag,     Count);
  SetLength(RXData[aRT].Palleted, Count);
  SetLength(RXData[aRT].Size,     Count);
  SetLength(RXData[aRT].Pivot,    Count);
  SetLength(RXData[aRT].Data,     Count);
  SetLength(RXData[aRT].RGBA,     Count);
  SetLength(RXData[aRT].Mask,     Count);
  SetLength(RXData[aRT].HasMask,  Count);
end;


//Reading RX Data
function TKMSprites.LoadRX(aRT: TRXType): Boolean;
var
  i: Integer;
  FileName: String;
  f: file;
begin
  Result := False;

  FileName := ExeDir + 'data\gfx\res\' + RXInfo[aRT].FileName + '.rx';

  if not CheckFileExists(FileName) then
    Exit;

  assignfile(f, FileName);
  reset(f, 1);
  blockread(f, RXData[aRT].Qty, 4);

  AllocateRX(aRT);

  blockread(f, RXData[aRT].Flag[1], RXData[aRT].Qty);

  //Don't load the extra sprites, but keep them allocated
  //to avoid range check errors when game wants to use that sprite
  if (RXInfo[aRT].OverrideCount <> 0) then
    RXData[aRT].Qty := RXInfo[aRT].OverrideCount;

  for i := 1 to RXData[aRT].Qty do
    if RXData[aRT].Flag[i] = 1 then
    begin
      RXData[aRT].Palleted[i] := True;
      blockread(f, RXData[aRT].Size[i].X, 4);
      blockread(f, RXData[aRT].Pivot[i].X, 8);
      //Data part of each sprite is 8BPP palleted in KaM RX
      SetLength(RXData[aRT].Data[i], RXData[aRT].Size[i].X * RXData[aRT].Size[i].Y);
      blockread(f, RXData[aRT].Data[i, 0], RXData[aRT].Size[i].X * RXData[aRT].Size[i].Y);
    end;
  closefile(f);
  fLog.AppendLog(RXInfo[aRT].FileName + ' -', RXData[aRT].Qty);

  ExpandRX(aRT); //Only KaM's rx needs expanding
  Result := True;
end;


//Reading RXX Data
function TKMSprites.LoadRXX(aRT: TRXType): Boolean;
var
  i: Integer;
  FileName: String;
  InputStream: TFileStream;
  DecompressionStream: TDecompressionStream;
begin
  Result := False;

  FileName := ExeDir + 'data\gfx\res\' + RXInfo[aRT].FileName + '.rxx';

  if not CheckFileExists(FileName) then
    Exit;

  InputStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  DecompressionStream := TDecompressionStream.Create(InputStream);

  DecompressionStream.Read(RXData[aRT].Qty, 4);

  AllocateRX(aRT);

  DecompressionStream.Read(RXData[aRT].Flag[1], RXData[aRT].Qty);

  //Don't load the extra sprites, but keep them allocated
  //to avoid range check errors when game wants to use that sprite
  if (RXInfo[aRT].OverrideCount <> 0) then
    RXData[aRT].Qty := RXInfo[aRT].OverrideCount;

  for i := 1 to RXData[aRT].Qty do
    if RXData[aRT].Flag[i] = 1 then
    begin
      RXData[aRT].Palleted[i] := False;
      DecompressionStream.Read(RXData[aRT].Size[i].X, 4);
      DecompressionStream.Read(RXData[aRT].Pivot[i].X, 8);
      //Data part of each sprite is 32BPP RGBA in Remake RXX files
      SetLength(RXData[aRT].RGBA[i], RXData[aRT].Size[i].X * RXData[aRT].Size[i].Y);
      SetLength(RXData[aRT].Mask[i], RXData[aRT].Size[i].X * RXData[aRT].Size[i].Y);
      DecompressionStream.Read(RXData[aRT].RGBA[i, 0], 4 * RXData[aRT].Size[i].X * RXData[aRT].Size[i].Y);
      DecompressionStream.Read(RXData[aRT].HasMask[i], 1);
      if RXData[aRT].HasMask[i] then
        DecompressionStream.Read(RXData[aRT].Mask[i, 0], RXData[aRT].Size[i].X * RXData[aRT].Size[i].Y);
    end;
  DecompressionStream.Free;
  InputStream.Free;
  fLog.AppendLog(RXInfo[aRT].FileName + ' -', RXData[aRT].Qty);

  Result := True;
end;


procedure TKMSprites.SaveRXX(aRT: TRXType; const aFileName:string);
var
  i:integer;
  InputStream: TMemoryStream;
  OutputStream: TFileStream;
  CompressionStream: TCompressionStream;
begin
  InputStream := TMemoryStream.Create;

  InputStream.Write(RXData[aRT].Qty, 4);
  InputStream.Write(RXData[aRT].Flag[1], RXData[aRT].Qty);

  for i := 1 to RXData[aRT].Qty do
    if RXData[aRT].Flag[i] = 1 then
    begin
      InputStream.Write(RXData[aRT].Size[i].X, 4);
      InputStream.Write(RXData[aRT].Pivot[i].X, 8);
      InputStream.Write(RXData[aRT].RGBA[i, 0], 4 * RXData[aRT].Size[i].X * RXData[aRT].Size[i].Y);
      InputStream.Write(RXData[aRT].HasMask[i], 1);
      if RXData[aRT].HasMask[i] then
        InputStream.Write(RXData[aRT].Mask[i, 0], RXData[aRT].Size[i].X * RXData[aRT].Size[i].Y);
    end;
  OutputStream := TFileStream.Create(aFileName,fmCreate);
  CompressionStream := TCompressionStream.Create(clMax, OutputStream);
  InputStream.Position := 0;
  CompressionStream.CopyFrom(InputStream, InputStream.Size);
  CompressionStream.Free;
  OutputStream.Free;
  InputStream.Free;
end;


//Convert paletted data into RGBA and select Team color layer from it
procedure TKMSprites.ExpandRX(aRT: TRXType);
var
  i: Integer;
  X, Y: Integer;
  Palette: TKMPal;
  L: byte;
  Pixel: Integer;
  ID: Byte;
begin
  ID := Byte(aRT) + 1;

  with RXData[aRT] do
  for i:=1 to Qty do
  begin

    case ID of
      5: Palette := RX5Pal[i];
      6: Palette := RX6Pal[i];
      else Palette := DEF_PAL;
    end;

    if Flag[i] = 1 then begin
      SetLength(RGBA[i], Size[i].X*Size[i].Y);
      SetLength(Mask[i], Size[i].X*Size[i].Y);

      for y:=0 to Size[i].Y-1 do for x:=0 to Size[i].X-1 do
      begin
        Pixel := y*Size[i].X + x;
        L := Data[i, Pixel]; //0..255

        if RXInfo[aRT].TeamColors and (L in [24..30])
        and ((ID<>2) or (i>400))  //Skip the Inn Weapon Smithy and the rest
        and ((ID<>4) or InRange(i,141,154) or InRange(i,521,550)) then //Unit icons and scrolls
        begin
          RGBA[i,Pixel] := cardinal(((L-27)*42+128)*65793) OR $FF000000;
          case L of //Maybe it makes sense to convert to 8bit?
            24,30:  Mask[i,Pixel] := $60;   //7  //6
            25,29:  Mask[i,Pixel] := $90;   //11 //9
            26,28:  Mask[i,Pixel] := $C0;   //14 //12
            27:     Mask[i,Pixel] := $FF;   //16 //16
          end;
          HasMask[i] := true;
        end else
          RGBA[i,Pixel] := fPalettes[Palette].Color32(L);
      end;
    end;
  end;
end;


{ This function should parse all valid files in Sprites folder and load them
  additionaly to or replacing original sprites }
procedure TKMSprites.OverloadRX(aRT: TRXType);
var
  FileList: TStringList;
  SearchRec: TSearchRec;
  i,x,y:integer;
  RX, ID: integer;
  T: Byte;
  ft: TextFile;
  {$IFDEF WDC}
  p: Cardinal;
  po: TPNGObject;
  {$ENDIF}
  {$IFDEF FPC}
  po: TBGRABitmap;
  {$ENDIF}
begin
  RX := Byte(aRT) + 1;

  if not DirectoryExists(ExeDir + 'Sprites\') then Exit;

  FileList := TStringList.Create;
  FindFirst(ExeDir + 'Sprites\' + inttostr(RX)+'_????.png', faAnyFile AND NOT faDirectory, SearchRec);
  repeat
    FileList.Add(SearchRec.Name);
  until (FindNext(SearchRec)<>0);
  FindClose(SearchRec);

  //#_####.png - Default texture
  //#_####a.png - Flag colors areas
  //#_####.txt - Pivot info

  for i:=0 to FileList.Count-1 do begin

    ID := StrToIntDef(Copy(FileList.Strings[i], 3, 4),0); //wrong file will return 0
    if InRange(ID, 1, RXData[aRT].Qty) then begin //Replace only certain sprites

      RXData[aRT].HasMask[ID] := FileExists(ExeDir + 'Sprites\' + Copy(FileList.Strings[i], 1, 6) + 'a.png');

      {$IFDEF WDC}
      po := TPNGObject.Create;
      po.LoadFromFile(ExeDir + 'Sprites\' + FileList.Strings[i]);
      {$ENDIF}
      {$IFDEF FPC}
      po := TBGRABitmap.Create(ExeDir + 'Sprites\' + FileList.Strings[i]);
      {$ENDIF}

      RXData[aRT].Flag[ID] := 1; //Mark as used (required for saving RXX)
      RXData[aRT].Palleted[ID] := False;
      RXData[aRT].Size[ID].X := po.Width;
      RXData[aRT].Size[ID].Y := po.Height;

      SetLength(RXData[aRT].RGBA[ID], po.Width*po.Height);
      SetLength(RXData[aRT].Mask[ID], po.Width*po.Height); //Should allocate space for it's always comes along

      {$IFDEF WDC}
      case po.TransparencyMode of //There are ways to process PNG transparency
        ptmNone:
          for y:=0 to po.Height-1 do for x:=0 to po.Width-1 do
            RXData[aRT].RGBA[ID, y*po.Width+x] := cardinal(po.Pixels[x,y]) OR $FF000000;
        ptmBit:
          for y:=0 to po.Height-1 do for x:=0 to po.Width-1 do
            if po.Pixels[x,y] = po.TransparentColor then
              RXData[aRT].RGBA[ID, y*po.Width+x] := cardinal(po.Pixels[x,y]) AND $FFFFFF //avoid black edging
            else
              RXData[aRT].RGBA[ID, y*po.Width+x] := cardinal(po.Pixels[x,y]) OR $FF000000;
        ptmPartial:
          for y:=0 to po.Height-1 do for x:=0 to po.Width-1 do begin
            p := (po.AlphaScanline[y]^[x]) shl 24; //semitransparency is killed by render later-on
            RXData[aRT].RGBA[ID, y*po.Width+x] := cardinal(po.Pixels[x,y]) OR p;
          end;
        else Assert(false, 'Unknown PNG transparency mode')
      end;
      {$ENDIF}
      {$IFDEF FPC}
      for y:=0 to po.Height-1 do for x:=0 to po.Width-1 do
        RXData[aRT].RGBA[ID, y*po.Width+x] := cardinal(po.GetPixel(x,y).red) OR (cardinal(po.GetPixel(x,y).green) shl 8) OR
                                            (cardinal(po.GetPixel(x,y).blue) shl 16) OR (cardinal(po.GetPixel(x,y).alpha) shl 24);
      {$ENDIF}
      po.Free;

      //Load and process the mask if it exists
      if RXData[aRT].HasMask[ID] then
      begin
        {$IFDEF WDC}
        po := TPNGObject.Create;
        po.LoadFromFile(ExeDir + 'Sprites\' + StringReplace(FileList.Strings[i], '.png', 'a.png', [rfReplaceAll, rfIgnoreCase]));
        {$ENDIF}
        {$IFDEF FPC}
        po := TBGRABitmap.Create(ExeDir + 'Sprites\' + StringReplace(FileList.Strings[i], '.png', 'a.png', [rfReplaceAll, rfIgnoreCase]));
        {$ENDIF}

        if (RXData[aRT].Size[ID].X = po.Width) and (RXData[aRT].Size[ID].Y = po.Height) then
        begin
          //We don't handle transparency in Masks
          {$IFDEF WDC}
          for y:=0 to po.Height-1 do for x:=0 to po.Width-1 do
          if cardinal(po.Pixels[x,y] AND $FF) <> 0 then
          begin
            T := RXData[aRT].RGBA[ID, y*po.Width+x] AND $FF; //Take red component
            RXData[aRT].Mask[ID, y*po.Width+x] := Byte(255-Abs(255-T*2));
            RXData[aRT].RGBA[ID, y*po.Width+x] := T*65793 OR $FF000000;
          end;
          {$ENDIF}
          {$IFDEF FPC}
          for y:=0 to po.Height-1 do for x:=0 to po.Width-1 do
          if cardinal(po.GetPixel(x,y).red) <> 0 then
          begin
            T := RXData[aRT].RGBA[ID, y*po.Width+x] AND $FF; //Take red component
            RXData[aRT].Mask[ID, y*po.Width+x] := Byte(255-Abs(255-T*2));
            RXData[aRT].RGBA[ID, y*po.Width+x] := T*65793 OR $FF000000;
          end;
          {$ENDIF}
        end;
        po.Free;
      end;


      //Read pivots
      if FileExists(ExeDir + 'Sprites\' + Copy(FileList.Strings[i], 1, 6)+'.txt') then begin
        AssignFile(ft, ExeDir + 'Sprites\' + Copy(FileList.Strings[i], 1, 6)+'.txt');
        Reset(ft);
        ReadLn(ft, RXData[aRT].Pivot[ID].X);
        ReadLn(ft, RXData[aRT].Pivot[ID].Y);
        CloseFile(ft);
      end;

    end;
  end;

  FileList.Free;
end;


{Take RX data and make nice textures out of it.
Textures should be POT to improve performance and avoid driver bugs
In result we have GFXData filled.}
procedure TKMSprites.MakeGFX(aRT: TRXType);
var
  ci,j,i,k,LeftIndex,RightIndex,TexCount,SpanCount:integer;
  AllocatedRAM,RequiredRAM,ColorsRAM:cardinal;
  WidthPOT,HeightPOT:word;
  TD:array of cardinal;
  TA:array of cardinal;
  HasMsk:boolean;
begin
  LeftIndex:=0; AllocatedRAM:=0; RequiredRAM:=0; ColorsRAM:=0; TexCount:=0;
  repeat
    inc(LeftIndex);

    WidthPOT  := RXData[aRT].Size[LeftIndex].X;
    HeightPOT := MakePOT(RXData[aRT].Size[LeftIndex].Y);
    SpanCount := 1;

    //Pack textures with same POT height into rows to save memory
    //This also means fewer textures for GPU RAM == better performance
    while((LeftIndex+SpanCount<RXData[aRT].Qty)and //Keep packing until end of sprites
          (
            (HeightPOT=MakePOT(RXData[aRT].Size[LeftIndex+SpanCount].Y)) //Pack if HeightPOT matches
        or((HeightPOT>=MakePOT(RXData[aRT].Size[LeftIndex+SpanCount].Y))AND(WidthPOT+RXData[aRT].Size[LeftIndex+SpanCount].X<MakePOT(WidthPOT)))
          )and
          (WidthPOT+RXData[aRT].Size[LeftIndex+SpanCount].X<=MAX_TEX_RESOLUTION)) //Pack until max Tex_Resolution approached
    do begin
      inc(WidthPOT,RXData[aRT].Size[LeftIndex+SpanCount].X);
      if (aRT=rxGuiMain)and(RX5Pal[LeftIndex]<>RX5Pal[LeftIndex+SpanCount]) then break; //Don't align RX5 images for they use all different palettes
      if (aRT=rxGuiMainH)and(RX6Pal[LeftIndex]<>RX6Pal[LeftIndex+SpanCount]) then break; //Don't align RX6 images for they use all different palettes
      inc(SpanCount);
    end;

    RightIndex := LeftIndex+SpanCount-1;
    WidthPOT := MakePOT(WidthPOT);
    SetLength(TD,WidthPOT*HeightPOT+1);
    SetLength(TA,WidthPOT*HeightPOT+1);

    for i:=0 to HeightPOT-1 do begin
      ci:=0;
      for j:=LeftIndex to RightIndex do
        for k:=0 to RXData[aRT].Size[j].X-1 do begin
          if i<RXData[aRT].Size[j].Y then begin
            //CopyMemory(TD[(i-1)*WidthPOT+ci-1], RXData[aRT].RGBA[j,(i-1)*RXData[aRT].Size[j].X+k-1], )
            TD[i*WidthPOT+ci] := RXData[aRT].RGBA[j,i*RXData[aRT].Size[j].X+k];
            TA[i*WidthPOT+ci] := (RXData[aRT].Mask[j,i*RXData[aRT].Size[j].X+k] SHL 24) OR $FFFFFF;
          end;
          inc(ci);
        end;
    end;

    HasMsk:=false;
    for j:=LeftIndex to RightIndex do
      HasMsk := HasMsk or RXData[aRT].HasMask[j];

    //If we need to prepare textures for TeamColors          //special fix for iron mine logo
    if MAKE_TEAM_COLORS and RXInfo[aRT].TeamColors and (not ((aRT=rxGui)and InRange(49,LeftIndex,RightIndex))) then
    begin
      GFXData[aRT,LeftIndex].TexID := fRender.GenTexture(WidthPOT,HeightPOT,@TD[0],tf_Normal);
      //TeamColors are done through alternative plain colored texture
      if HasMsk then begin
        GFXData[aRT,LeftIndex].AltID := fRender.GenTexture(WidthPOT,HeightPOT,@TA[0],tf_AltID);
        inc(ColorsRAM,WidthPOT*HeightPOT*4);
      end;
    end
    else
      GFXData[aRT,LeftIndex].TexID := fRender.GenTexture(WidthPOT,HeightPOT,@TD[0],tf_Normal);

    SetLength(TD,0);
    SetLength(TA,0);

    k:=0;
    for j:=LeftIndex to RightIndex do begin //Hack to test AlphaTest
      GFXData[aRT,j].TexID:=GFXData[aRT,LeftIndex].TexID;
      GFXData[aRT,j].AltID:=GFXData[aRT,LeftIndex].AltID;
      GFXData[aRT,j].u1:=k/WidthPOT;
      GFXData[aRT,j].v1:=0;
      inc(k,RXData[aRT].Size[j].X);
      GFXData[aRT,j].u2:=k/WidthPOT;
      GFXData[aRT,j].v2:=RXData[aRT].Size[j].Y/HeightPOT;
      GFXData[aRT,j].PxWidth:=RXData[aRT].Size[j].X;
      GFXData[aRT,j].PxHeight:=RXData[aRT].Size[j].Y;

      inc(RequiredRAM,RXData[aRT].Size[j].X*RXData[aRT].Size[j].Y*4);
    end;

    inc(AllocatedRAM,WidthPOT*HeightPOT*4);
    inc(LeftIndex,SpanCount-1);
    inc(TexCount);

  until(LeftIndex>=RXData[aRT].Qty); // >= in case data wasn't loaded and Qty=0

  //HasTeamColors will be accessed by fRender 
  RXData[aRT].HasTeamColors := RXInfo[aRT].TeamColors;

  fLog.AppendLog(inttostr(TexCount)+' Textures created');
  fLog.AddToLog(inttostr(AllocatedRAM div 1024)+'/'+inttostr((AllocatedRAM-RequiredRAM) div 1024)+' Kbytes allocated/wasted for units GFX when using Packing');
  fLog.AddToLog(inttostr(ColorsRAM div 1024)+' KBytes for team colors');
end;



{Take RX data and make nice textures out of it.
Textures should be POT to improve performance and avoid drivers bugs
In result we have GFXData filled.}
procedure TKMSprites.MakeGFX_AlphaTest(aHouseDat: TKMHouseDatCollection; aRT: TRXType);
var
  HT:THouseType;
  ID1,ID2:integer; //RGB and A index
  i,k,Lay,StepCount:integer;
  t,tx,ty:integer;
  Alpha:byte;
  WidthPOT,HeightPOT:integer;
  TD:array of cardinal;
begin
  for HT:=Low(THouseType) to High(THouseType) do
    if aHouseDat[HT].IsValid then

      //House is rendered in two layers since Stone does not covers Wood parts in e.g. Sawmill
      for Lay:=1 to 2 do
      begin
        if Lay=1 then begin
          ID1 := aHouseDat[HT].WoodPic+1;
          ID2 := aHouseDat[HT].WoodPal+1;
          StepCount := aHouseDat[HT].WoodPicSteps;
        end else begin
          ID1 := aHouseDat[HT].StonePic+1;
          ID2 := aHouseDat[HT].StonePal+1;
          StepCount := aHouseDat[HT].StonePicSteps;
        end;

        Assert(
            (RXData[aRT].Size[ID1].X >= RXData[aRT].Size[ID2].X) and
            (RXData[aRT].Size[ID1].Y >= RXData[aRT].Size[ID2].Y),
            Format('Mismatched sprites %d:%d - %d:%d', [Byte(aRT), ID1, Byte(aRT), ID2]));

        WidthPOT  := MakePOT(RXData[aRT].Size[ID1].X);
        HeightPOT := MakePOT(RXData[aRT].Size[ID1].Y);
        SetLength(TD, WidthPOT*HeightPOT);

        //Fill in colors data
        for i := 0 to RXData[aRT].Size[ID1].Y-1 do
        for k := 0 to RXData[aRT].Size[ID1].X-1 do
          TD[i*WidthPOT+k] := RXData[aRT].RGBA[ID1, i*RXData[aRT].Size[ID1].X+k];

        //Apply mask to where colors are (yes, it needs to be done in 2 steps, since offsets can mismatch)
        tx := RXData[aRT].Pivot[ID2].x - RXData[aRT].Pivot[ID1].x;
        ty := (RXData[aRT].Pivot[ID2].y - RXData[aRT].Pivot[ID1].y)*WidthPOT;
        for i := 0 to RXData[aRT].Size[ID2].Y-1 do
        for k := 0 to RXData[aRT].Size[ID2].X-1 do
        begin
          t := i*WidthPOT+k + tx + ty; //Shift by pivot, always positive

          //Palleted means that we can use Data array
          //Otherwise, for addon sprites, we need to resort to RGBA data they provide
          if RXData[aRT].Palleted[ID2] then
            Alpha := RXData[aRT].Data[ID2,i*RXData[aRT].Size[ID2].X+k]
          else
            Alpha := RXData[aRT].RGBA[ID2,i*RXData[aRT].Size[ID2].X+k] AND $FF;

          //Steps are going in normal order 1..n, but that last step has Alpha=0
          if TD[t] <> 0 then
            if Alpha <> 0 then //Default case
              TD[t] := TD[t] AND ($FFFFFF OR (255-round(Alpha*(255/StepCount))) shl 24)
            else
              TD[t] := TD[t] AND $01FFFFFF; //Place it as last step
        end;

        GFXData[aRT,ID1].TexID := fRender.GenTexture(WidthPOT,HeightPOT,@TD[0],tf_AlphaTest);
        SetLength(TD, 0);
        GFXData[aRT,ID1].AltID := 0;
        GFXData[aRT,ID1].u1    := 0;
        GFXData[aRT,ID1].v1    := 0;
        GFXData[aRT,ID1].u2    := RXData[aRT].Size[ID1].X/WidthPOT;
        GFXData[aRT,ID1].v2    := RXData[aRT].Size[ID1].Y/HeightPOT;
        GFXData[aRT,ID1].PxWidth := RXData[aRT].Size[ID1].X;
        GFXData[aRT,ID1].PxHeight:= RXData[aRT].Size[ID1].Y;
      end;
end;


// Now we can safely dispose of RXData[ID].Data to save us RAM
procedure TKMSprites.ClearUnusedGFX(aRT: TRXType);
var
  i: Integer;
begin
  for i := 1 to RXData[aRT].Qty do
  begin
    SetLength(RXData[aRT].Data[i], 0);
    SetLength(RXData[aRT].RGBA[i], 0);
    SetLength(RXData[aRT].Mask[i], 0);
  end;
end;



end.
