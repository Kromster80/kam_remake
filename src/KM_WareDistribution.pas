unit KM_WareDistribution;
{$I KaM_Remake.inc}
interface
uses
  KM_ResWares, KM_ResHouses,
  KM_CommonClasses;


const
  //These have been adjusted slightly from the old KaM defaults.
  //The number means how many items should be in houses input max, and also affects delivery priority.
  DistributionDefaults: array [1..4, 1..4] of Byte = (
    (5, 5, 0, 0),
    (5, 3, 4, 4),
    (3, 4, 0, 0),
    (4, 5, 3, 0)
  );


type
  TKMWareDistribution = class
  private
    fWareDistribution: array [1..4, 1..4] of Byte;
    procedure SetWareDistribution(aWare: TWareType; aHouse: THouseType; aValue: Byte);
    function GetWareDistribution(aWare: TWareType; aHouse: THouseType): Byte;
  public
    Changed: Boolean;
    constructor Create;
    property WareDistribution[aWare: TWareType; aHouse: THouseType]: Byte read GetWareDistribution write SetWareDistribution; default;
    procedure LoadFromStr(aString: String);
    function PackToStr: String;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;


implementation
uses
  SysUtils, Math;

{TKMWareDistribution}
constructor TKMWareDistribution.Create;
var I, K: Integer;
begin
  for I := 1 to 4 do for K := 1 to 4 do
    fWareDistribution[I, K] := DistributionDefaults[I, K];  // Load default ratios
end;


procedure TKMWareDistribution.SetWareDistribution(aWare: TWareType; aHouse: THouseType; aValue: Byte);
begin
  case aWare of
    wt_Steel: if aHouse = ht_WeaponSmithy   then fWareDistribution[1,1] := aValue else
              if aHouse = ht_ArmorSmithy    then fWareDistribution[1,2] := aValue;
    wt_Coal:  if aHouse = ht_IronSmithy     then fWareDistribution[2,1] := aValue else
              if aHouse = ht_Metallurgists  then fWareDistribution[2,2] := aValue else
              if aHouse = ht_WeaponSmithy   then fWareDistribution[2,3] := aValue else
              if aHouse = ht_ArmorSmithy    then fWareDistribution[2,4] := aValue;
    wt_Wood:  if aHouse = ht_ArmorWorkshop  then fWareDistribution[3,1] := aValue else
              if aHouse = ht_WeaponWorkshop then fWareDistribution[3,2] := aValue;
    wt_Corn:  if aHouse = ht_Mill           then fWareDistribution[4,1] := aValue else
              if aHouse = ht_Swine          then fWareDistribution[4,2] := aValue else
              if aHouse = ht_Stables        then fWareDistribution[4,3] := aValue;
    else      raise Exception.Create('Unexpected resource at SetWareDistribution');
  end;
  Changed := True;
end;


function TKMWareDistribution.GetWareDistribution(aWare: TWareType; aHouse: THouseType): Byte;
begin
  Result := 5; //Default should be 5, for house/resource combinations that don't have a setting (on a side note this should be the only place the resourse limit is defined)
  case aWare of
    wt_Steel: if aHouse = ht_WeaponSmithy   then Result := fWareDistribution[1,1] else
              if aHouse = ht_ArmorSmithy    then Result := fWareDistribution[1,2];
    wt_Coal:  if aHouse = ht_IronSmithy     then Result := fWareDistribution[2,1] else
              if aHouse = ht_Metallurgists  then Result := fWareDistribution[2,2] else
              if aHouse = ht_WeaponSmithy   then Result := fWareDistribution[2,3] else
              if aHouse = ht_ArmorSmithy    then Result := fWareDistribution[2,4];
    wt_Wood:  if aHouse = ht_ArmorWorkshop  then Result := fWareDistribution[3,1] else
              if aHouse = ht_WeaponWorkshop then Result := fWareDistribution[3,2];
    wt_Corn:  if aHouse = ht_Mill           then Result := fWareDistribution[4,1] else
              if aHouse = ht_Swine          then Result := fWareDistribution[4,2] else
              if aHouse = ht_Stables        then Result := fWareDistribution[4,3];
    else      //Handled in 1st row to avoid repeating in if .. else lines
  end;
end;


procedure TKMWareDistribution.LoadFromStr(aString: String);
  function IsValid: Boolean;
  var I: Integer;
  begin
    Result := Length(aString) = 16; // Ware distribution string length should be equal to 16
    if Result then
      for I := 1 to 16 do
        begin
          Result := Result and InRange(Ord(aString[I]), 48, 53); //In ware distribution string only digits from 0 to 5 are allowed'
        end;

  end;
var I, J: Integer;
begin
  aString := Trim(aString); // Trim possible spaces
  //Distribution format is string of 16 digits, each digit should be between 0 and 5
  if IsValid then
    for I := 1 to 16 do
      fWareDistribution[1+((I-1) div 4), 1+((I-1) mod 4)] := StrToInt(aString[I])
  else
    for I := 1 to 4 do
      for J := 1 to 4 do
        fWareDistribution[I, J] := DistributionDefaults[I, J];
end;


function TKMWareDistribution.PackToStr: String;
var I, J: Integer;
begin
  Result := '';
  for I := 1 to 4 do
    for J := 1 to 4 do
      Result := Result + IntToStr(fWareDistribution[I, J]);
end;


procedure TKMWareDistribution.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fWareDistribution, SizeOf(fWareDistribution));
end;


procedure TKMWareDistribution.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(fWareDistribution, SizeOf(fWareDistribution));
end;


end.
