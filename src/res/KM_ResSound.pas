unit KM_ResSound;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, TypInfo,
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  KromUtils, KM_Defaults;

type
  TAttackNotification = (an_Citizens, an_Town, an_Troops);

  TSoundFX = (
    sfx_None=0,
    sfx_CornCut,
    sfx_Dig,
    sfx_Pave,
    sfx_MineStone,
    sfx_CornSow,
    sfx_ChopTree,
    sfx_housebuild,
    sfx_placemarker,
    sfx_Click,
    sfx_mill,
    sfx_saw,
    sfx_wineStep,
    sfx_wineDrain,
    sfx_metallurgists,
    sfx_coalDown,
    sfx_Pig1,sfx_Pig2,sfx_Pig3,sfx_Pig4,
    sfx_Mine,
    sfx_unknown21, //Pig?
    sfx_Leather,
    sfx_BakerSlap,
    sfx_CoalMineThud,
    sfx_ButcherCut,
    sfx_SausageString,
    sfx_QuarryClink,
    sfx_TreeDown,
    sfx_WoodcutterDig,
    sfx_CantPlace,
    sfx_MessageOpen,
    sfx_MessageClose,
    sfx_MessageNotice,
    //Usage of melee sounds can be found in Docs\Melee sounds in KaM.csv
    sfx_Melee34, sfx_Melee35, sfx_Melee36, sfx_Melee37, sfx_Melee38,
    sfx_Melee39, sfx_Melee40, sfx_Melee41, sfx_Melee42, sfx_Melee43,
    sfx_Melee44, sfx_Melee45, sfx_Melee46, sfx_Melee47, sfx_Melee48,
    sfx_Melee49, sfx_Melee50, sfx_Melee51, sfx_Melee52, sfx_Melee53,
    sfx_Melee54, sfx_Melee55, sfx_Melee56, sfx_Melee57,
    sfx_BowDraw,
    sfx_ArrowHit,
    sfx_CrossbowShoot,  //60
    sfx_CrossbowDraw,
    sfx_BowShoot,       //62
    sfx_BlacksmithBang,
    sfx_BlacksmithFire,
    sfx_CarpenterHammer, //65
    sfx_Horse1,sfx_Horse2,sfx_Horse3,sfx_Horse4,
    sfx_RockThrow,
    sfx_HouseDestroy,
    sfx_SchoolDing,
    //Below are TPR sounds ...
    sfx_SlingerShoot,
    sfx_BalistaShoot,
    sfx_CatapultShoot,
    sfx_unknown76,
    sfx_CatapultReload,
    sfx_SiegeBuildingSmash);

  TSoundFXNew = (
    sfxn_ButtonClick,
    sfxn_Trade,
    sfxn_MPChatMessage,
    sfxn_MPChatTeam,
    sfxn_MPChatSystem,
    sfxn_MPChatOpen,
    sfxn_MPChatClose,
    sfxn_Victory,
    sfxn_Defeat,
    sfxn_Beacon,
    sfxn_Error,
    sfxn_Peacetime);

  //Sounds to play on different warrior orders
  TWarriorSpeech = (
    sp_Select, sp_Eat, sp_RotLeft, sp_RotRight, sp_Split,
    sp_Join, sp_Halt, sp_Move, sp_Attack, sp_Formation,
    sp_Death, sp_BattleCry, sp_StormAttack);

  TWAVHeaderEx = record
    RIFFHeader: array [1..4] of AnsiChar;
    FileSize: Integer;
    WAVEHeader: array [1..4] of AnsiChar;
    FormatHeader: array [1..4] of AnsiChar;
    FormatHeaderSize: Integer;
    FormatCode: Word;
    ChannelNumber: Word;
    SampleRate: Integer;
    BytesPerSecond: Integer;
    BytesPerSample: Word;
    BitsPerSample: Word;
    DATAHeader: array [1..4] of AnsiChar; //Extension
    DataSize: Integer; //Extension
  end;

  TKMSoundData = record
    Head: TWAVHeaderEx;
    Data: array of byte;
    Foot: array of byte;
    IsLoaded: boolean;
  end;

  TKMResSounds = class
  private
    fLocaleString: AnsiString; //Locale used to access warrior sounds

    fWarriorUseBackup: array[WARRIOR_MIN..WARRIOR_MAX] of boolean;

    procedure LoadSoundsDAT;
    procedure ScanWarriorSounds;
    function LoadWarriorSoundsFromFile(const aFile: string): Boolean;
    procedure SaveWarriorSoundsToFile(const aFile: string);
  public
    fWavesCount: integer;
    fWaves: array of TKMSoundData;

    NotificationSoundCount: array[TAttackNotification] of byte;
    WarriorSoundCount: array[WARRIOR_MIN..WARRIOR_MAX, TWarriorSpeech] of byte;

    constructor Create(const aLocale, aFallback, aDefault: AnsiString);

    function FileOfCitizen(aUnitType: TUnitType; aSound: TWarriorSpeech): UnicodeString;
    function FileOfNewSFX(aSFX: TSoundFXNew): UnicodeString;
    function FileOfNotification(aSound: TAttackNotification; aNumber: Byte): UnicodeString;
    function FileOfWarrior(aUnitType: TUnitType; aSound: TWarriorSpeech; aNumber: Byte): UnicodeString;

    procedure ExportSounds;
  end;


implementation
uses
  KM_CommonClasses;


const
  WarriorSFXFolder: array[WARRIOR_MIN..WARRIOR_MAX] of string = (
    'militia', 'axeman', 'swordman', 'bowman', 'crossbowman',
    'lanceman', 'pikeman', 'cavalry', 'knights', 'barbarian',
    'rebel', 'rogue', 'warrior', 'vagabond');

  //TPR warriors reuse TSK voices in some languages, so if the specific ones don't exist use these
  WarriorSFXFolderBackup: array[WARRIOR_MIN..WARRIOR_MAX] of string = (
    '', '', '', '', '',
    '', '', '', '', '',
    'bowman', 'lanceman', 'barbarian', 'cavalry');

  WarriorSFX: array[TWarriorSpeech] of string = (
    'select', 'eat', 'left', 'right', 'halve',
    'join', 'halt', 'send', 'attack', 'format',
    'death', 'battle', 'storm');

  AttackNotifications: array[TAttackNotification] of string = ('citiz', 'town', 'units');

  CitizenSFX: array[CITIZEN_MIN..CITIZEN_MAX] of record
    WarriorVoice: TUnitType;
    SelectID, DeathID: byte;
  end = (
    (WarriorVoice: ut_Militia;      SelectID:3; DeathID:1), //ut_Serf
    (WarriorVoice: ut_AxeFighter;   SelectID:0; DeathID:0), //ut_Woodcutter
    (WarriorVoice: ut_Bowman;       SelectID:2; DeathID:1), //ut_Miner
    (WarriorVoice: ut_Swordsman;    SelectID:0; DeathID:2), //ut_AnimalBreeder
    (WarriorVoice: ut_Militia;      SelectID:1; DeathID:2), //ut_Farmer
    (WarriorVoice: ut_Arbaletman;   SelectID:1; DeathID:0), //ut_Lamberjack
    (WarriorVoice: ut_Pikeman;      SelectID:1; DeathID:0), //ut_Baker
    (WarriorVoice: ut_HorseScout;   SelectID:0; DeathID:2), //ut_Butcher
    (WarriorVoice: ut_Horseman;     SelectID:2; DeathID:0), //ut_Fisher
    (WarriorVoice: ut_Cavalry;      SelectID:1; DeathID:1), //ut_Worker
    (WarriorVoice: ut_Hallebardman; SelectID:1; DeathID:1), //ut_StoneCutter
    (WarriorVoice: ut_Cavalry;      SelectID:3; DeathID:4), //ut_Smith
    (WarriorVoice: ut_Hallebardman; SelectID:3; DeathID:2), //ut_Metallurgist
    (WarriorVoice: ut_Bowman;       SelectID:3; DeathID:0)  //ut_Recruit
    );

  NewSFXFolder = 'Sounds'+PathDelim;
  NewSFXFile: array [TSoundFXNew] of string = (
    'UI'+PathDelim+'ButtonClick.wav',
    'Buildings'+PathDelim+'MarketPlace'+PathDelim+'Trade.wav',
    'Chat'+PathDelim+'ChatArrive.wav',
    'Chat'+PathDelim+'ChatTeam.wav',
    'Chat'+PathDelim+'ChatSystem.wav',
    'Chat'+PathDelim+'ChatOpen.wav',
    'Chat'+PathDelim+'ChatClose.wav',
    'Misc'+PathDelim+'Victory.wav',
    'Misc'+PathDelim+'Defeat.wav',
    'UI'+PathDelim+'Beacon.wav',
    'UI'+PathDelim+'Error.wav',
    'Misc'+PathDelim+'PeaceTime.wav');


{ TKMResSounds }
constructor TKMResSounds.Create(const aLocale, aFallback, aDefault: AnsiString);
begin
  inherited Create;

  if SKIP_SOUND then Exit;

  if DirectoryExists(ExeDir + 'data' + PathDelim + 'sfx' + PathDelim + 'speech.' + UnicodeString(aLocale) + PathDelim) then
    fLocaleString := aLocale
  else
    if DirectoryExists(ExeDir + 'data' + PathDelim + 'sfx' + PathDelim + 'speech.' + UnicodeString(aFallback) + PathDelim) then
      fLocaleString := aFallback//Use fallback local when primary doesn't exist
    else
      fLocaleString := aDefault; //Use English voices when no language specific voices exist

  LoadSoundsDAT;
  ScanWarriorSounds;
end;


procedure TKMResSounds.LoadSoundsDAT;
var
  S: TMemoryStream;
  Head:record Size,Count:word; end;
  Tab1:array[1..200]of integer;
  Tab2:array[1..200]of smallint;
  i,Tmp:integer;
begin
  if not FileExists(ExeDir + 'data' + PathDelim + 'sfx' + PathDelim + 'sounds.dat') then Exit;

  S := TMemoryStream.Create;
  S.LoadFromFile(ExeDir + 'data' + PathDelim + 'sfx' + PathDelim + 'sounds.dat');
  S.Read(Head, 4);
  S.Read(Tab1, Head.Count*4); //Read Count*4bytes into Tab1(WaveSizes)
  S.Read(Tab2, Head.Count*2); //Read Count*2bytes into Tab2(No idea what is it)

  fWavesCount := Head.Count;
  SetLength(fWaves, fWavesCount+1);

  for i:=1 to Head.Count do begin
    S.Read(Tmp, 4); //Always '1' for existing waves
    if Tab1[i]<>0 then begin
      S.Read(fWaves[i].Head, SizeOf(fWaves[i].Head));
      SetLength(fWaves[i].Data, fWaves[i].Head.DataSize);
      S.Read(fWaves[i].Data[0], fWaves[i].Head.DataSize);
      SetLength(fWaves[i].Foot, Tab1[i]-SizeOf(fWaves[i].Head)-fWaves[i].Head.DataSize);
      S.Read(fWaves[i].Foot[0], Tab1[i]-SizeOf(fWaves[i].Head)-fWaves[i].Head.DataSize);
    end;
    fWaves[i].IsLoaded := True;
  end;

  {BlockRead(f,c,20);
  //Packed record
  //SampleRate,Volume,a,b:integer;
  //i,j,k,l,Index:word;
  BlockRead(f,Props[1],26*Head.Count);}

  S.Free;
end;


procedure TKMResSounds.ExportSounds;
var
  I: Integer;
  S: TMemoryStream;
begin
  ForceDirectories(ExeDir + 'Export'+PathDelim+'SoundsDat'+PathDelim);

  for I := 1 to fWavesCount do
  if Length(fWaves[I].Data) > 0 then
  begin
    S := TMemoryStream.Create;
    S.Write(fWaves[I].Head, SizeOf(fWaves[I].Head));
    S.Write(fWaves[I].Data[0], Length(fWaves[I].Data));
    S.Write(fWaves[I].Foot[0], Length(fWaves[I].Foot));
    S.SaveToFile(ExeDir + 'Export'+PathDelim+'SoundsDat'+PathDelim+'sound_' + int2fix(I, 3) + '_' +
                 GetEnumName(TypeInfo(TSoundFX), I) + '.wav');
    S.Free;
  end;
end;


function TKMResSounds.FileOfCitizen(aUnitType: TUnitType; aSound: TWarriorSpeech): UnicodeString;
var SoundID: Byte;
begin
  if not (aUnitType in [CITIZEN_MIN..CITIZEN_MAX]) then Exit;

  if aSound = sp_Death then
    SoundID := CitizenSFX[aUnitType].DeathID
  else
    SoundID := CitizenSFX[aUnitType].SelectID;

  Result := FileOfWarrior(CitizenSFX[aUnitType].WarriorVoice, aSound, SoundID);
end;


function TKMResSounds.FileOfWarrior(aUnitType: TUnitType; aSound: TWarriorSpeech; aNumber: Byte): UnicodeString;
var
  S: UnicodeString;
begin
  S := ExeDir + 'data' + PathDelim + 'sfx' + PathDelim + 'speech.' + UnicodeString(fLocaleString) + PathDelim;
  if fWarriorUseBackup[aUnitType] then
    S := S + WarriorSFXFolderBackup[aUnitType]
  else
    S := S + WarriorSFXFolder[aUnitType];
  S := S + PathDelim + WarriorSFX[aSound] + IntToStr(aNumber);
  //All our files are WAV now. Don't accept SND files because TPR uses SND in a different
  //format which can cause OpenAL to crash if someone installs KMR over TPR folder (e.g. Steam)
  Result := S+'.wav';
end;


function TKMResSounds.FileOfNewSFX(aSFX: TSoundFXNew): UnicodeString;
begin
  Result := ExeDir + NewSFXFolder + NewSFXFile[aSFX];
end;


function TKMResSounds.FileOfNotification(aSound: TAttackNotification; aNumber: Byte): UnicodeString;
var
  S: UnicodeString;
begin
  S := ExeDir + 'data'+PathDelim+'sfx'+PathDelim+'speech.'+UnicodeString(fLocaleString)+ PathDelim + AttackNotifications[aSound] + int2fix(aNumber,2);
  Result := S+'.wav';
end;


//Scan and count the number of warrior sounds
procedure TKMResSounds.ScanWarriorSounds;
var
  I: Integer;
  U: TUnitType;
  WS: TWarriorSpeech;
  AN: TAttackNotification;
  SpeechPath: string;
begin
  SpeechPath := ExeDir + 'data' + PathDelim + 'sfx' + PathDelim + 'speech.' + UnicodeString(fLocaleString) + PathDelim;

  //Reset counts from previous locale/unsuccessful load
  FillChar(WarriorSoundCount, SizeOf(WarriorSoundCount), #0);
  FillChar(NotificationSoundCount, SizeOf(NotificationSoundCount), #0);
  FillChar(fWarriorUseBackup, SizeOf(fWarriorUseBackup), #0);

  if not DirectoryExists(SpeechPath) then Exit;

  //Try to load counts from DAT,
  //otherwise we will rescan all the WAV files and write a new DAT
  if LoadWarriorSoundsFromFile(SpeechPath + 'count.dat') then
    Exit;

  //First inspect folders, if the prefered ones don't exist use the backups
  for U := WARRIOR_MIN to WARRIOR_MAX do
    if not DirectoryExists(SpeechPath + WarriorSFXFolder[U] + PathDelim) then
      fWarriorUseBackup[U] := True;

  //If the folder exists it is likely all the sounds are there
  for U := WARRIOR_MIN to WARRIOR_MAX do
    for WS := Low(TWarriorSpeech) to High(TWarriorSpeech) do
      for I := 0 to 255 do
        if not FileExists(FileOfWarrior(U, WS, I)) then
        begin
          WarriorSoundCount[U, WS] := I;
          Break;
        end;

  //Scan warning messages (e.g. under attack)
  for AN := Low(TAttackNotification) to High(TAttackNotification) do
    for I := 0 to 255 do
      if not FileExists(FileOfNotification(AN, I)) then
      begin
        NotificationSoundCount[AN] := I;
        Break;
      end;

  //Save counts to DAT file for faster access next time
  SaveWarriorSoundsToFile(SpeechPath + 'count.dat');
end;


function TKMResSounds.LoadWarriorSoundsFromFile(const aFile: string): Boolean;
var
  S: AnsiString;
  MS: TKMemoryStream;
begin
  Result := False;
  if not FileExists(aFile) then Exit;

  MS := TKMemoryStream.Create;
  try
    MS.LoadFromFile(aFile);
    MS.ReadA(S);
    if S = GAME_REVISION then
    begin
      MS.Read(WarriorSoundCount, SizeOf(WarriorSoundCount));
      MS.Read(fWarriorUseBackup, SizeOf(fWarriorUseBackup));
      MS.Read(NotificationSoundCount, SizeOf(NotificationSoundCount));
      Result := True;
    end;
  finally
    MS.Free;
  end;
end;


procedure TKMResSounds.SaveWarriorSoundsToFile(const aFile: string);
var
  MS: TKMemoryStream;
begin
  MS := TKMemoryStream.Create;
  try
    MS.WriteA(GAME_REVISION);
    MS.Write(WarriorSoundCount, SizeOf(WarriorSoundCount));
    MS.Write(fWarriorUseBackup, SizeOf(fWarriorUseBackup));
    MS.Write(NotificationSoundCount, SizeOf(NotificationSoundCount));
    MS.SaveToFile(aFile);
  finally
    MS.Free;
  end;
end;


end.
