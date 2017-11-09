unit RXXPackerForm;
{$I ..\..\KaM_Remake.inc}
interface
uses
  Classes, Controls, Dialogs,
  ExtCtrls, Forms, Graphics, Spin, StdCtrls, SysUtils, TypInfo,
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF FPC} LResources, LCLIntf, {$ENDIF}
  KM_Defaults, KM_Log, KM_Pics, KM_ResPalettes, KM_ResSprites, KM_ResSpritesEdit;


type
  TRXXForm1 = class(TForm)
    btnPackRXX: TButton;
    ListBox1: TListBox;
    Label1: TLabel;
    procedure btnPackRXXClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fPalettes: TKMResPalettes;
  end;


var
  RXXForm1: TRXXForm1;


implementation
{$R *.dfm}
uses KM_ResHouses, KM_ResUnits, KM_Points;


procedure TRXXForm1.FormCreate(Sender: TObject);
var
  RT: TRXType;
begin
  ExeDir := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\..\');

  Caption := 'RXX Packer (' + GAME_REVISION + ')';

  //Although we don't need them in this tool, these are required to load sprites
  gLog := TKMLog.Create(ExeDir + 'RXXPacker.log');

  fPalettes := TKMResPalettes.Create;
  fPalettes.LoadPalettes(ExeDir + 'data\gfx\');

  for RT := Low(TRXType) to High(TRXType) do
    ListBox1.Items.Add(GetEnumName(TypeInfo(TRXType), Integer(RT)));

  ListBox1.ItemIndex := 0;
end;


procedure TRXXForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fPalettes);
  FreeAndNil(gLog);
end;


procedure TRXXForm1.btnPackRXXClick(Sender: TObject);
var
  DeathAnimProcessed: array of Integer;
  DeathAnimCount: Integer;

  function DeathAnimAlreadyDone(aID:Integer):Boolean;
  var I:Integer;
  begin
    Result := False;
    for I:=0 to DeathAnimCount-1 do
      if DeathAnimProcessed[I] = aID then
      begin
        Result := True;
        Exit;
      end;
  end;

var
  SpritePack: TKMSpritePackEdit;
  RT: TRXType;
  I, Step, SpriteID: Integer;
  Tick: Cardinal;
  RXName: string;
  resHouses: TKMResHouses;
  resUnits: TKMResUnits;
  UT: TUnitType;
  Dir: TKMDirection;
begin
  btnPackRXX.Enabled := False;
  Tick := GetTickCount;

  Assert(DirectoryExists(ExeDir + 'SpriteResource\'),
         'Cannot find ' + ExeDir + 'SpriteResource\ folder.'+#10#13+
         'Please make sure this folder exists.');

  for I := 0 to ListBox1.Items.Count - 1 do
  if ListBox1.Selected[I] then
  begin
    RT := TRXType(I);
    //ruCustom sprite packs do not have a main RXX file so don't need packing
    if RXInfo[RT].Usage <> ruCustom then
    begin
      RXName := ExeDir + 'SpriteResource\' + RXInfo[RT].FileName + '.rx';
      Assert((RT = rxTiles) or FileExists(RXName),
             'Cannot find ' + RXName + ' file.'+#10#13+
             'Please copy the file from your KaM\data\gfx\res\ folder.');

      SpritePack := TKMSpritePackEdit.Create(RT, fPalettes);
      try
        //Load
        if RT <> rxTiles then
        begin
          SpritePack.LoadFromRXFile(RXName);
          SpritePack.OverloadFromFolder(ExeDir + 'SpriteResource\');
        end
        else
        if DirectoryExists(ExeDir + 'SpriteResource\') then
          SpritePack.LoadFromFolder(ExeDir + 'SpriteResource\');

        //Tiles must stay the same size as they can't use pivots
        if RT <> rxTiles then
          gLog.AddTime('Trimmed ' + IntToStr(SpritePack.TrimSprites));

        //Houses need some special treatment to adapt to GL_ALPHA_TEST that we use for construction steps
        if RT = rxHouses then
        begin
          resHouses := TKMResHouses.Create;
          SpritePack.AdjoinHouseMasks(resHouses);
          SpritePack.GrowHouseMasks(resHouses);
          resHouses.Free;
        end;

        //The idea was to blur the water and make it semitrasparent, but it did not worked out as expected
        //if RT = rxTiles then
        //  SpritePack.SoftWater(nil);

        //Save
        SpritePack.SaveToRXXFile(ExeDir + 'Data\Sprites\' + RXInfo[RT].FileName + '.rxx');

        //Generate alpha shadows for the following sprite packs
        if RT in [rxHouses,rxUnits,rxGui,rxTrees] then
        begin
          if RT = rxHouses then
          begin
            SpritePack.SoftenShadows(889, 892, False); //Smooth smoke
            SpritePack.SoftenShadows(1615, 1638, False); //Smooth flame
          end;
          if RT = rxUnits then
          begin
            SpritePack.SoftenShadows(6251, 6314, False); //Smooth thought bubbles
            //Smooth all death animations for all units
            resUnits := TKMResUnits.Create;
            DeathAnimCount := 0; //We need to remember which ones we've done because units reuse them
            SetLength(DeathAnimProcessed, 1000); //Hopefully more than enough
            for UT := HUMANS_MIN to HUMANS_MAX do
              for Dir := dir_N to dir_NW do
                for Step := 1 to 30 do
                begin
                  SpriteID := resUnits[UT].UnitAnim[ua_Die,Dir].Step[Step]+1; //Sprites in units.dat are 0 indexed
                  if (SpriteID > 0)
                  and not DeathAnimAlreadyDone(SpriteID) then
                  begin
                    SpritePack.SoftenShadows(SpriteID, False);
                    DeathAnimProcessed[DeathAnimCount] := SpriteID;
                    inc(DeathAnimCount);
                  end;
                end;
            resUnits.Free;
          end;

          if RT = rxGui then
          begin
            SpritePack.SoftenShadows(105, 128); //Field plans
            SpritePack.SoftenShadows(249, 281); //House tablets only (shadow softening messes up other rxGui sprites)
            SpritePack.SoftenShadows(461, 468); //Field fences
            SpritePack.SoftenShadows(660, 660); //Woodcutter cutting point sign
          end
          else
            SpritePack.SoftenShadows;

          SpritePack.SaveToRXXFile(ExeDir + 'Data\Sprites\' + RXInfo[RT].FileName + '_a.rxx');
        end;
      finally
        SpritePack.Free;
      end;
    end;

    ListBox1.Selected[I] := False;
    ListBox1.Refresh;
  end;

  Label1.Caption := IntToStr(GetTickCount - Tick) + ' ms';
  btnPackRXX.Enabled := True;
end;


{$IFDEF FPC}
initialization
  {$i RXXPackerForm.lrs}
{$ENDIF}


end.
