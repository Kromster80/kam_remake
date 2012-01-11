unit KM_ResourceSprites;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF WDC} PNGImage, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}

  Classes, Graphics, Math, SysUtils,
  KM_CommonEvents,
  KM_ResourceCursors,
  KM_ResourceHouse,
  KM_ResourcePalettes,
  KM_RenderSetup,
  KM_TextLibrary
  {$IFDEF FPC}, zstream {$ENDIF}
  {$IFDEF WDC}, ZLib {$ENDIF}

  {$IFDEF FPC}, BGRABitmap {$ENDIF};

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

type
  TRXUsage = (ruMenu, ruGame); //Where sprites are used

  TRXInfo = record
    FileName: AnsiString; //Used for logging and filenames
    TeamColors: Boolean; //sprites should be generated with color masks
    AlphaTest: Boolean; //Alphatested gradients, used only for houses yet
    LoadsFromRXX: Boolean; //Should this RX be loaded from RXX files for releases?
    Usage: TRXUsage; //Menu and Game sprites are loaded separately
    LoadingTextID: Word;
  end;


var
  RXInfo: array [TRXType] of TRXInfo = (
    (
      FileName: 'Trees';
      TeamColors: False;
      AlphaTest: False;
      LoadsFromRXX: True;
      Usage: ruGame;
      LoadingTextID: TX_MENU_LOADING_TREES;
    ),
    (
      FileName: 'Houses';
      TeamColors: True;
      AlphaTest: True;
      LoadsFromRXX: True;
      Usage: ruGame;
      LoadingTextID: TX_MENU_LOADING_HOUSES;
    ),
    (
      FileName: 'Units';
      TeamColors: True;
      AlphaTest: False;
      LoadsFromRXX: True;
      Usage: ruGame;
      LoadingTextID: TX_MENU_LOADING_UNITS;
    ),
    (
      FileName: 'GUI';
      TeamColors: True;
      AlphaTest: False;
      LoadsFromRXX: True;
      Usage: ruMenu;
      LoadingTextID: 0;
    ),
    (
      FileName: 'GUIMain';
      TeamColors: False;
      AlphaTest: False;
      LoadsFromRXX: True;
      Usage: ruMenu;
      LoadingTextID: 0;
    ),
    (
      FileName: 'GUIMainH';
      TeamColors: False;
      AlphaTest: False;
      LoadsFromRXX: True;
      Usage: ruMenu;
      LoadingTextID: 0;
    ),
    (
      FileName: 'RemakeMenu';
      TeamColors: False;
      AlphaTest: False;
      LoadsFromRXX: True;
      Usage: ruMenu;
      LoadingTextID: 0;
    ),
    (
      FileName: 'Tileset';
      TeamColors: False;
      AlphaTest: False;
      LoadsFromRXX: False;
      Usage: ruGame;
      LoadingTextID: TX_MENU_LOADING_TILESET;
    ),
    (
      FileName: 'RemakeGame';
      TeamColors: True;
      AlphaTest: False;
      LoadsFromRXX: True;
      Usage: ruGame;
      LoadingTextID: TX_MENU_LOADING_ADDITIONAL_SPRITES;
    ));

type
  //Base class for Sprite loading
  TKMSpritePack = class
  private
    fPalettes: TKMPalettes;
    fRT: TRXType;

    procedure Allocate(aCount: Integer); //Allocate space for data that is being loaded
    procedure Expand;
  public
    constructor Create(aPalettes: TKMPalettes; aRT: TRXType);
    procedure LoadFromRXFile(const aFileName: string);
    procedure LoadFromRXXFile(const aFileName: string);
    procedure LoadFromFolder(const aFolder: string);
    procedure OverloadFromFolder(const aFolder: string);

    procedure SaveToRXXFile(const aFileName: string);

    procedure ClearData; //Release non-required data
  end;

  //Overrides for:
  //GUI: Cursors
  //Houses: AlphaTest
  //Tiles: TGA file
  //

  TKMSprites = class
  private
    fPalettes: TKMPalettes;
    fSprites: array[TRXType] of TKMSpritePack;
    fStepProgress: TEvent;
    fStepCaption: TStringEvent;
    procedure MakeGFX(aRT: TRXType);
    procedure MakeGFX_AlphaTest(aHouseDat: TKMHouseDatCollection; aRT: TRXType);
    function GetRXFileName(aRX: TRXType): string;
    procedure SaveBMPTexture(aWidth, aHeight: Integer; aIndex: Integer; const Data: TCardinalArray; aMode: TTexFormat);
  public
    constructor Create(aPalettes: TKMPalettes; aStepProgress: TEvent; aStepCaption: TStringEvent);
    destructor Destroy; override;
    procedure LoadMenuResources(aCursors: TKMCursors);
    procedure LoadGameResources(aHouseDat: TKMHouseDatCollection; aTileTex: Cardinal);

    property FileName[aRX: TRXType]: string read GetRXFileName;
    procedure LoadSprites(aRT: TRXType; aCursors: TKMCursors; aHouseDat: TKMHouseDatCollection); //Exposed for Exports
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
uses KromUtils, KM_Defaults, KM_Log;


var
    RX5Pal: array [1 .. 40] of TKMPal = (
      pal2_setup,   pal2_setup,   pal2_setup,   pal2_setup,   pal2_setup,
      pal2_setup,   pal_set2,     pal_set2,     pal_set2,     pal_map,
      pal_map,      pal_map,      pal_map,      pal_map,      pal_map,
      pal_map,      pal2_setup,   pal2_setup,   pal2_setup,   pal2_mapgold,
      pal2_mapgold, pal2_mapgold, pal2_mapgold, pal2_mapgold, pal2_setup,
      pal_map,      pal_map,      pal_map,      pal_map,      pal_map,
      pal2_setup,   pal2_setup,   pal2_setup,   pal2_setup,   pal2_setup,
      pal2_setup,   pal2_setup,   pal_lin,      pal_lin,      pal_lin
    );

    // I couldn't find matching palettes for the 17th and 18th entries
    RX6Pal: array [1 .. 20] of TKMPal = (
      pal_set,  pal_set,  pal_set,  pal_set,  pal_set,
      pal_set,  pal_set2, pal_set2, pal_set2, pal_map,
      pal_map,  pal_map,  pal_map,  pal_map,  pal_map,
      pal_map,  pal_lin,  pal_lin,  pal_lin,  pal_lin
    );


{ TKMSpritePack }
constructor TKMSpritePack.Create(aPalettes: TKMPalettes; aRT: TRXType);
begin
  inherited Create;

  fPalettes := aPalettes;
  fRT := aRT;
end;

procedure TKMSpritePack.Allocate(aCount: Integer);
begin
  RXData[fRT].Qty := aCount;

  aCount := RXData[fRT].Qty+1;
  SetLength(GFXData[fRT],         aCount);
  SetLength(RXData[fRT].Flag,     aCount);
  SetLength(RXData[fRT].Palleted, aCount);
  SetLength(RXData[fRT].Size,     aCount);
  SetLength(RXData[fRT].Pivot,    aCount);
  SetLength(RXData[fRT].Data,     aCount);
  SetLength(RXData[fRT].RGBA,     aCount);
  SetLength(RXData[fRT].Mask,     aCount);
  SetLength(RXData[fRT].HasMask,  aCount);
end;

//Convert paletted data into RGBA and select Team color layer from it
procedure TKMSpritePack.Expand;
var
  H: Integer;
  K, I: Integer;
  Palette: TKMPalData;
  L: byte;
  Pixel: Integer;
  ID: Byte;
begin
  ID := Byte(fRT) + 1;

  with RXData[fRT] do
  for H := 1 to Qty do
  begin
    //Choose proper palette
    case ID of
      5: Palette := fPalettes[RX5Pal[H]];
      6: Palette := fPalettes[RX6Pal[H]];
      else Palette := fPalettes.DefDal;
    end;

    if Flag[H] = 1 then
    begin
      SetLength(RGBA[H], Size[H].X*Size[H].Y);
      SetLength(Mask[H], Size[H].X*Size[H].Y);

      for I := 0 to Size[H].Y - 1 do
      for K := 0 to Size[H].X - 1 do
      begin
        Pixel := I*Size[H].X + K;
        L := Data[H, Pixel]; //0..255

        if RXInfo[fRT].TeamColors and (L in [24..30])
        and ((ID<>2) or (H>400))  //Skip the Inn Weapon Smithy and the rest
        and ((ID<>4) or InRange(H,141,154) or InRange(H,521,550)) then //Unit icons and scrolls
        begin
          RGBA[H,Pixel] := cardinal(((L-27)*42+128)*65793) OR $FF000000;
          case L of //Maybe it makes sense to convert to 8bit?
            24,30:  Mask[H,Pixel] := $60;   //7  //6
            25,29:  Mask[H,Pixel] := $90;   //11 //9
            26,28:  Mask[H,Pixel] := $C0;   //14 //12
            27:     Mask[H,Pixel] := $FF;   //16 //16
          end;
          HasMask[H] := True;
        end else
          RGBA[H,Pixel] := Palette.Color32(L);
      end;
    end;
  end;
end;

//Release RAM
procedure TKMSpritePack.ClearData;
var I: Integer;
begin
  for I := 1 to RXData[fRT].Qty do
  begin
    SetLength(RXData[fRT].Data[I], 0);
    SetLength(RXData[fRT].RGBA[I], 0);
    SetLength(RXData[fRT].Mask[I], 0);
  end;
end;


procedure TKMSpritePack.LoadFromRXFile(const aFileName: string);
var
  I: Integer;
  S: TMemoryStream;
begin
  if not CheckFileExists(aFileName) then
    Exit;

  S := TMemoryStream.Create;

  S.LoadFromFile(aFileName);
  S.ReadBuffer(RXData[fRT].Qty, 4);

  Allocate(RXData[fRT].Qty);

  S.ReadBuffer(RXData[fRT].Flag[1], RXData[fRT].Qty);

  for I := 1 to RXData[fRT].Qty do
    if RXData[fRT].Flag[I] = 1 then
    begin
      RXData[fRT].Palleted[I] := True;
      S.ReadBuffer(RXData[fRT].Size[I].X, 4);
      S.ReadBuffer(RXData[fRT].Pivot[I].X, 8);
      //Data part of each sprite is 8BPP palleted in KaM RX
      SetLength(RXData[fRT].Data[I], RXData[fRT].Size[I].X * RXData[fRT].Size[I].Y);
      S.ReadBuffer(RXData[fRT].Data[I,0], RXData[fRT].Size[I].X * RXData[fRT].Size[I].Y);
    end;
  S.Free;
  fLog.AppendLog(RXInfo[fRT].FileName + ' -', RXData[fRT].Qty);

  Expand; //Only KaM's rx needs expanding
end;


procedure TKMSpritePack.LoadFromRXXFile(const aFileName: string);
var
  i: Integer;
  InputStream: TFileStream;
  DecompressionStream: TDecompressionStream;
begin
  if not CheckFileExists(aFileName) then
    Exit;

  InputStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyNone);
  DecompressionStream := TDecompressionStream.Create(InputStream);

  DecompressionStream.Read(RXData[fRT].Qty, 4);

  Allocate(RXData[fRT].Qty);

  DecompressionStream.Read(RXData[fRT].Flag[1], RXData[fRT].Qty);

  for I := 1 to RXData[fRT].Qty do
    if RXData[fRT].Flag[I] = 1 then
    begin
      RXData[fRT].Palleted[I] := False;
      DecompressionStream.Read(RXData[fRT].Size[I].X, 4);
      DecompressionStream.Read(RXData[fRT].Pivot[I].X, 8);
      //Data part of each sprite is 32BPP RGBA in Remake RXX files
      SetLength(RXData[fRT].RGBA[I], RXData[fRT].Size[I].X * RXData[fRT].Size[I].Y);
      SetLength(RXData[fRT].Mask[I], RXData[fRT].Size[I].X * RXData[fRT].Size[I].Y);
      DecompressionStream.Read(RXData[fRT].RGBA[I, 0], 4 * RXData[fRT].Size[I].X * RXData[fRT].Size[I].Y);
      DecompressionStream.Read(RXData[fRT].HasMask[I], 1);
      if RXData[fRT].HasMask[I] then
        DecompressionStream.Read(RXData[fRT].Mask[I, 0], RXData[fRT].Size[I].X * RXData[fRT].Size[I].Y);
    end;
  DecompressionStream.Free;
  InputStream.Free;
  fLog.AppendLog(RXInfo[fRT].FileName + ' -', RXData[fRT].Qty);
end;


//Load sprites from folder
procedure TKMSpritePack.LoadFromFolder(const aFolder: string);
var ft: TextFile;
begin
  if not DirectoryExists(aFolder) then Exit;
  if not FileExists(aFolder + IntToStr(Byte(fRT) + 1) + '.txt') then Exit;

  AssignFile(ft, aFolder + IntToStr(Byte(fRT) + 1) + '.txt');
    Reset(ft);
    ReadLn(ft, RXData[fRT].Qty);
  CloseFile(ft);

  Allocate(RXData[fRT].Qty);

  OverloadFromFolder(aFolder);
end;


//Parse all valid files in Sprites folder and load them additionaly to or replacing original sprites
procedure TKMSpritePack.OverloadFromFolder(const aFolder: string);
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
  RX := Byte(fRT) + 1;

  if not DirectoryExists(aFolder) then Exit;

  FileList := TStringList.Create;
  FindFirst(aFolder + inttostr(RX)+'_????.png', faAnyFile AND NOT faDirectory, SearchRec);
  repeat
    FileList.Add(SearchRec.Name);
  until (FindNext(SearchRec)<>0);
  FindClose(SearchRec);

  //#_####.png - Default texture
  //#_####a.png - Flag colors areas
  //#_####.txt - Pivot info

  for i:=0 to FileList.Count-1 do
  begin

    ID := StrToIntDef(Copy(FileList.Strings[i], 3, 4),0); //wrong file will return 0
    if InRange(ID, 1, RXData[fRT].Qty) then begin //Replace only certain sprites

      RXData[fRT].HasMask[ID] := FileExists(aFolder + Copy(FileList.Strings[i], 1, 6) + 'a.png');

      {$IFDEF WDC}
      po := TPNGObject.Create;
      po.LoadFromFile(aFolder + FileList.Strings[i]);
      {$ENDIF}
      {$IFDEF FPC}
      po := TBGRABitmap.Create(aFolder + FileList.Strings[i]);
      {$ENDIF}

      RXData[fRT].Flag[ID] := 1; //Mark as used (required for saving RXX)
      RXData[fRT].Palleted[ID] := False;
      RXData[fRT].Size[ID].X := po.Width;
      RXData[fRT].Size[ID].Y := po.Height;

      SetLength(RXData[fRT].RGBA[ID], po.Width*po.Height);
      SetLength(RXData[fRT].Mask[ID], po.Width*po.Height); //Should allocate space for it's always comes along

      {$IFDEF WDC}
      case po.TransparencyMode of //There are ways to process PNG transparency
        ptmNone:
          for y:=0 to po.Height-1 do for x:=0 to po.Width-1 do
            RXData[fRT].RGBA[ID, y*po.Width+x] := cardinal(po.Pixels[x,y]) OR $FF000000;
        ptmBit:
          for y:=0 to po.Height-1 do for x:=0 to po.Width-1 do
            if po.Pixels[x,y] = po.TransparentColor then
              RXData[fRT].RGBA[ID, y*po.Width+x] := cardinal(po.Pixels[x,y]) AND $FFFFFF //avoid black edging
            else
              RXData[fRT].RGBA[ID, y*po.Width+x] := cardinal(po.Pixels[x,y]) OR $FF000000;
        ptmPartial:
          for y:=0 to po.Height-1 do for x:=0 to po.Width-1 do begin
            p := (po.AlphaScanline[y]^[x]) shl 24; //semitransparency is killed by render later-on
            RXData[fRT].RGBA[ID, y*po.Width+x] := cardinal(po.Pixels[x,y]) OR p;
          end;
        else Assert(false, 'Unknown PNG transparency mode')
      end;
      {$ENDIF}
      {$IFDEF FPC}
      for y:=0 to po.Height-1 do for x:=0 to po.Width-1 do
        RXData[fRT].RGBA[ID, y*po.Width+x] := cardinal(po.GetPixel(x,y).red) OR (cardinal(po.GetPixel(x,y).green) shl 8) OR
                                            (cardinal(po.GetPixel(x,y).blue) shl 16) OR (cardinal(po.GetPixel(x,y).alpha) shl 24);
      {$ENDIF}
      po.Free;

      //Load and process the mask if it exists
      if RXData[fRT].HasMask[ID] then
      begin
        {$IFDEF WDC}
        po := TPNGObject.Create;
        po.LoadFromFile(aFolder + StringReplace(FileList.Strings[i], '.png', 'a.png', [rfReplaceAll, rfIgnoreCase]));
        {$ENDIF}
        {$IFDEF FPC}
        po := TBGRABitmap.Create(aFolder + StringReplace(FileList.Strings[i], '.png', 'a.png', [rfReplaceAll, rfIgnoreCase]));
        {$ENDIF}

        if (RXData[fRT].Size[ID].X = po.Width) and (RXData[fRT].Size[ID].Y = po.Height) then
        begin
          //We don't handle transparency in Masks
          {$IFDEF WDC}
          for y:=0 to po.Height-1 do for x:=0 to po.Width-1 do
          if cardinal(po.Pixels[x,y] AND $FF) <> 0 then
          begin
            T := RXData[fRT].RGBA[ID, y*po.Width+x] AND $FF; //Take red component
            RXData[fRT].Mask[ID, y*po.Width+x] := Byte(255-Abs(255-T*2));
            RXData[fRT].RGBA[ID, y*po.Width+x] := T*65793 OR $FF000000;
          end;
          {$ENDIF}
          {$IFDEF FPC}
          for y:=0 to po.Height-1 do for x:=0 to po.Width-1 do
          if cardinal(po.GetPixel(x,y).red) <> 0 then
          begin
            T := RXData[fRT].RGBA[ID, y*po.Width+x] AND $FF; //Take red component
            RXData[fRT].Mask[ID, y*po.Width+x] := Byte(255-Abs(255-T*2));
            RXData[fRT].RGBA[ID, y*po.Width+x] := T*65793 OR $FF000000;
          end;
          {$ENDIF}
        end;
        po.Free;
      end;

      //Read pivots
      if FileExists(aFolder + Copy(FileList.Strings[i], 1, 6)+'.txt') then begin
        AssignFile(ft, aFolder + Copy(FileList.Strings[i], 1, 6)+'.txt');
        Reset(ft);
        ReadLn(ft, RXData[fRT].Pivot[ID].X);
        ReadLn(ft, RXData[fRT].Pivot[ID].Y);
        CloseFile(ft);
      end;
    end;
  end;

  FileList.Free;
end;


procedure TKMSpritePack.SaveToRXXFile(const aFileName: string);
var
  i:integer;
  InputStream: TMemoryStream;
  OutputStream: TFileStream;
  CompressionStream: TCompressionStream;
begin
  InputStream := TMemoryStream.Create;

  InputStream.Write(RXData[fRT].Qty, 4);
  InputStream.Write(RXData[fRT].Flag[1], RXData[fRT].Qty);

  for I := 1 to RXData[fRT].Qty do
    if RXData[fRT].Flag[I] = 1 then
    begin
      InputStream.Write(RXData[fRT].Size[I].X, 4);
      InputStream.Write(RXData[fRT].Pivot[I].X, 8);
      InputStream.Write(RXData[fRT].RGBA[I, 0], 4 * RXData[fRT].Size[I].X * RXData[fRT].Size[I].Y);
      InputStream.Write(RXData[fRT].HasMask[I], 1);
      if RXData[fRT].HasMask[I] then
        InputStream.Write(RXData[fRT].Mask[I, 0], RXData[fRT].Size[I].X * RXData[fRT].Size[I].Y);
    end;
  OutputStream := TFileStream.Create(aFileName,fmCreate);
  CompressionStream := TCompressionStream.Create(clMax, OutputStream);
  InputStream.Position := 0;
  CompressionStream.CopyFrom(InputStream, InputStream.Size);
  CompressionStream.Free;
  OutputStream.Free;
  InputStream.Free;
end;


{ TKMSprites }
constructor TKMSprites.Create(aPalettes: TKMPalettes; aStepProgress: TEvent; aStepCaption: TStringEvent);
var
  RT: TRXType;
begin
  Inherited Create;
  fPalettes := aPalettes;

  for RT := Low(TRXType) to High(TRXType) do
    fSprites[RT] := TKMSpritePack.Create(fPalettes, RT);

  fStepProgress := aStepProgress;
  fStepCaption := aStepCaption;
end;


destructor TKMSprites.Destroy;
var
  RT: TRXType;
begin
  for RT := Low(TRXType) to High(TRXType) do
    fSprites[RT].Free;

  inherited;
end;


function TKMSprites.GetRXFileName(aRX: TRXType): string;
begin
  Result := RXInfo[aRX].FileName;
end;


procedure TKMSprites.LoadMenuResources(aCursors: TKMCursors);
var
  RT: TRXType;
begin
  for RT := Low(TRXType) to High(TRXType) do
    if (RXInfo[RT].Usage = ruMenu) then
    begin
      fStepCaption('Reading ' + RXInfo[RT].FileName + ' ...');
      LoadSprites(RT, aCursors, nil);
      fStepProgress;
    end;
end;


procedure TKMSprites.LoadGameResources(aHouseDat: TKMHouseDatCollection; aTileTex: Cardinal);
var i:integer; RT: TRXType;
begin
  for RT := Low(TRXType) to High(TRXType) do
  if (RXInfo[RT].Usage = ruGame) then
  begin
    fStepCaption(fTextLibrary[RXInfo[RT].LoadingTextID]);
    fLog.AppendLog('Reading ' + RXInfo[RT].FileName + '.rx');
    LoadSprites(RT, nil, aHouseDat);
  end;

end;


//Try to load RXX first, then RX, then use Folder
procedure TKMSprites.LoadSprites(aRT: TRXType; aCursors: TKMCursors; aHouseDat: TKMHouseDatCollection);
begin
  if FileExists(ExeDir + 'data\gfx\res\' + RXInfo[aRT].FileName + '.rxx') then
  begin
    fSprites[aRT].LoadFromRXXFile(ExeDir + 'data\gfx\res\' + RXInfo[aRT].FileName + '.rxx');
    fSprites[aRT].OverloadFromFolder(ExeDir + 'Sprites\');
  end
  else
  if FileExists(ExeDir + 'data\gfx\res\' + RXInfo[aRT].FileName + '.rx') then
  begin
    fSprites[aRT].LoadFromRXFile(ExeDir + 'data\gfx\res\' + RXInfo[aRT].FileName + '.rx');
    fSprites[aRT].OverloadFromFolder(ExeDir + 'Sprites\');
  end
  else
  if DirectoryExists(ExeDir + 'Sprites\') then
    fSprites[aRT].LoadFromFolder(ExeDir + 'Sprites\');

  //todo: Replace with something
  {//Special case for Tileset for MapEd menu
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
  end;}

  //Cursors must be made before we clear the raw RGBA data
  if (aRT = rxGui) and (aCursors <> nil) then
  begin
    aCursors.MakeCursors;
    aCursors.Cursor := kmc_Default;
  end;

  MakeGFX(aRT);

  //Alpha_tested sprites for houses. They come after MakeGFX cos they will replace above data
  if RXInfo[aRT].AlphaTest and (aHouseDat <> nil) then
    MakeGFX_AlphaTest(aHouseDat, aRT);

  fSprites[aRT].ClearData;
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
  if RXData[aRT].Qty = 0 then Exit;

  LeftIndex := 0;
  AllocatedRAM := 0;
  RequiredRAM := 0;
  ColorsRAM := 0;
  TexCount := 0;
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
      GFXData[aRT,LeftIndex].TexID := fRenderSetup.GenTexture(WidthPOT,HeightPOT,@TD[0],tf_Normal);
      //TeamColors are done through alternative plain colored texture
      if HasMsk then begin
        GFXData[aRT,LeftIndex].AltID := fRenderSetup.GenTexture(WidthPOT,HeightPOT,@TA[0],tf_AltID);
        inc(ColorsRAM,WidthPOT*HeightPOT*4);
      end;
    end
    else
      GFXData[aRT,LeftIndex].TexID := fRenderSetup.GenTexture(WidthPOT,HeightPOT,@TD[0],tf_Normal);

    SaveBMPTexture(WidthPOT,HeightPOT,GFXData[aRT,LeftIndex].TexID,@TD[0],tf_Normal);
    if HasMsk then
      SaveBMPTexture(WidthPOT,HeightPOT,GFXData[aRT,LeftIndex].TexID,@TA[0],tf_AltID);


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

        GFXData[aRT,ID1].TexID := fRenderSetup.GenTexture(WidthPOT,HeightPOT,@TD[0],tf_AlphaTest);

        SaveBMPTexture(WidthPOT,HeightPOT,GFXData[aRT,ID1].TexID,@TD[0],tf_AlphaTest);

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


procedure TKMSprites.SaveBMPTexture(aWidth, aHeight: Integer; aIndex: Integer; const Data: TCardinalArray; aMode: TTexFormat);
var
  i,k: Integer;
  Bmp: TBitmap;
begin
  if WriteAllTexturesToBMP then begin
    CreateDir(ExeDir+'Export\GenTextures\');
    Bmp:=TBitmap.Create;
    Bmp.PixelFormat:=pf24bit;
    Bmp.Width:=aWidth;
    Bmp.Height:=aHeight;

    for i:=0 to aHeight-1 do for k:=0 to aWidth-1 do
      Bmp.Canvas.Pixels[k,i] := ((PCardinal(Cardinal(@Data[0])+(i*aWidth+k)*4))^) AND $FFFFFF; //Ignore alpha
    Bmp.SaveToFile(ExeDir+'Export\GenTextures\'+int2fix(aIndex,4)+'.bmp');

    //these Alphas are worth looking at
    if aMode=tf_AlphaTest then
    begin
      for i:=0 to aHeight-1 do for k:=0 to aWidth-1 do
        Bmp.Canvas.Pixels[k,i] := ((PCardinal(Cardinal(@Data[0])+(i*aWidth+k)*4))^) SHR 24 *65793;
      Bmp.SaveToFile(ExeDir+'Export\GenTextures\'+int2fix(aIndex,4)+'a.bmp');
    end;

    Bmp.Free;
  end;
end;


end.
