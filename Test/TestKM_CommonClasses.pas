unit TestKM_CommonClasses;
interface
uses
  TestFramework, KM_CommonClasses, Classes, SysUtils, Math, KM_NetworkTypes,
  KM_Points;

type
  // Test methods for class TKMPointList
  TestTKMPointList = class(TTestCase)
  strict private
    FKMPointList: TKMPointList;
  private
    procedure FillDefaults;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestClear;
    procedure TestAdd;
    procedure TestRemove;
    procedure TestInsert;
    procedure TestGetRandom;
    procedure TestGetClosest;
    procedure TestInverse;
    procedure TestGetBounds;
    procedure TestSaveToStream;
    procedure TestLoadFromStream;
  end;

  // Test methods for class TKMPointTagList
  TestTKMPointTagList = class(TTestCase)
  strict private
    FKMPointTagList: TKMPointTagList;
  private
    procedure FillDefaults;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestClear;
    procedure TestAdd;
    procedure TestRemove;
    procedure TestSaveToStream;
    procedure TestLoadFromStream;
  end;

  // Test methods for class TKMPointDirList
  TestTKMPointDirList = class(TTestCase)
  strict private
    FKMPointDirList: TKMPointDirList;
  private
    procedure FillDefaults;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestClear;
    procedure TestAdd;
    procedure TestGetRandom;
    procedure TestLoadFromStream;
    procedure TestSaveToStream;
  end;

implementation
uses KM_CommonUtils;

procedure TestTKMPointList.SetUp;
begin
  SetKaMSeed(4);
  FKMPointList := TKMPointList.Create;
end;

procedure TestTKMPointList.TearDown;
begin
  FKMPointList.Free;
  FKMPointList := nil;
end;

procedure TestTKMPointList.FillDefaults;
var I: Integer;
begin
  for I := 0 to 255 do
    FKMPointList.Add(KMPoint(I, I));
end;

procedure TestTKMPointList.TestClear;
begin
  FKMPointList.Clear;
  Check(FKMPointList.Count = 0);

  FillDefaults;
  FKMPointList.Clear;
  Check(FKMPointList.Count = 0);
end;

procedure TestTKMPointList.TestAdd;
var
  I: Integer;
  aLoc: TKMPoint;
begin
  for I := 0 to 255 do
  begin
    aLoc := KMPoint(Random(65535), Random(65535));
    FKMPointList.Add(aLoc);
    CheckEquals(I + 1, FKMPointList.Count);
    CheckEquals(aLoc.X, FKMPointList[I].X);
    CheckEquals(aLoc.Y, FKMPointList[I].Y);
  end;
end;

procedure TestTKMPointList.TestRemove;
var
  OldCount: Integer;
  ReturnValue: Integer;
  aLoc: TKMPoint;
begin
  //Test with no values
  ReturnValue := FKMPointList.Remove(KMPoint(7,8));
  Check(ReturnValue = -1);
  Check(FKMPointList.Count = 0);

  //Test single value
  aLoc := KMPoint(7,8);
  FKMPointList.Add(aLoc);
  ReturnValue := FKMPointList.Remove(aLoc);
  Check(ReturnValue = 0);
  Check(FKMPointList.Count = 0);

  //Test missing entry
  FKMPointList.Clear;
  FillDefaults;
  OldCount := FKMPointList.Count;
  ReturnValue := FKMPointList.Remove(KMPoint(7,8));
  Check(ReturnValue = -1);
  Check(FKMPointList.Count = OldCount);
end;

procedure TestTKMPointList.TestInsert;
var
  NewLoc, LastLoc: TKMPoint;
  OldCount: Integer;
begin
  //Insert into clear list
  NewLoc := KMPoint(7,8);
  FKMPointList.Insert(0, NewLoc);
  Check(KMSamePoint(FKMPointList[FKMPointList.Count - 1], NewLoc));
  Check(FKMPointList.Count = 1);

  //Insert 0th element
  FKMPointList.Clear;
  FillDefaults;
  OldCount := FKMPointList.Count;
  LastLoc := FKMPointList[OldCount - 1];
  NewLoc := KMPoint(7,8);
  FKMPointList.Insert(0, NewLoc);
  Check(KMSamePoint(FKMPointList[0], NewLoc));
  Check(FKMPointList.Count = OldCount + 1);
  Check(KMSamePoint(LastLoc, FKMPointList[FKMPointList.Count - 1]), 'Last entry corrupt');

  //Insert Nth element
  FKMPointList.Clear;
  FillDefaults;
  OldCount := FKMPointList.Count;
  LastLoc := FKMPointList[OldCount - 1];
  NewLoc := KMPoint(7,8);
  FKMPointList.Insert(FKMPointList.Count, NewLoc);
  Check(KMSamePoint(FKMPointList[FKMPointList.Count - 1], NewLoc));
  Check(FKMPointList.Count = OldCount + 1);
  Check(KMSamePoint(LastLoc, FKMPointList[FKMPointList.Count - 2]), 'Last entry corrupt');
end;

procedure TestTKMPointList.TestGetRandom;
var
  I, OldCount, K: Integer;
  ReturnValue: Boolean;
  aLoc, Point: TKMPoint;
begin
  //Empty list
  ReturnValue := FKMPointList.GetRandom(Point);
  Check(not ReturnValue);

  //Single value
  aLoc := KMPoint(7,8);
  FKMPointList.Insert(0, aLoc);
  ReturnValue := FKMPointList.GetRandom(Point);
  Check(ReturnValue);
  Check(KMSamePoint(Point, aLoc));

  //Filled list
  FillDefaults;
  OldCount := FKMPointList.Count;
  for I := 0 to OldCount - 1 do
  begin
    ReturnValue := FKMPointList.GetRandom(Point);
    Check(ReturnValue);
    K := FKMPointList.Remove(Point);
    Check(K <> -1);
  end;
  Check(FKMPointList.Count = 0);
end;

procedure TestTKMPointList.TestGetClosest;
var
  ReturnValue: Boolean;
  Point: TKMPoint;
  aLoc: TKMPoint;
begin
  //Empty list
  ReturnValue := FKMPointList.GetClosest(KMPoint(1,1), Point);
  Check(ReturnValue = False);

  //Filled list
  FillDefaults;
  ReturnValue := FKMPointList.GetClosest(aLoc, Point);
  Check(ReturnValue);
end;

procedure TestTKMPointList.TestInverse;
begin
  //Invert empty list
  FKMPointList.Inverse;
  Check(FKMPointList.Count = 0, 'Empty fail');

  //Odd list
  FKMPointList.Add(KMPoint(1,1));
  FKMPointList.Add(KMPoint(2,2));
  FKMPointList.Add(KMPoint(3,3));
  FKMPointList.Inverse;
  Check(KMSamePoint(FKMPointList[0], KMPoint(3,3)));
  Check(KMSamePoint(FKMPointList[1], KMPoint(2,2)));
  Check(KMSamePoint(FKMPointList[2], KMPoint(1,1)));

  //Even list
  FKMPointList.Clear;
  FKMPointList.Add(KMPoint(1,1));
  FKMPointList.Add(KMPoint(2,2));
  FKMPointList.Add(KMPoint(3,3));
  FKMPointList.Add(KMPoint(4,4));
  FKMPointList.Inverse;
  Check(KMSamePoint(FKMPointList[0], KMPoint(4,4)));
  Check(KMSamePoint(FKMPointList[1], KMPoint(3,3)));
  Check(KMSamePoint(FKMPointList[2], KMPoint(2,2)));
  Check(KMSamePoint(FKMPointList[3], KMPoint(1,1)));
end;

procedure TestTKMPointList.TestGetBounds;
var
  ReturnValue: Boolean;
  B: TKMRect;
begin
  ReturnValue := FKMPointList.GetBounds(B);
  Check(not ReturnValue);

  FillDefaults;
  ReturnValue := FKMPointList.GetBounds(B);
  Check(ReturnValue);
  //todo: Check(KMSameRect(KMRect(FKMPointList[0],), FKMPointList[0]));
end;

procedure TestTKMPointList.TestSaveToStream;
var
  SaveStream: TKMemoryStream;
begin
  //Empty list
  SaveStream := TKMemoryStream.Create;
  FKMPointList.SaveToStream(SaveStream);
  SaveStream.Position := 0;
  FKMPointList.LoadFromStream(SaveStream);
  Check(FKMPointList.Count = 0);
  SaveStream.Free;

  //Single entry list
  FKMPointList.Add(KMPoint(7,8));
  SaveStream := TKMemoryStream.Create;
  FKMPointList.SaveToStream(SaveStream);
  SaveStream.Position := 0;
  FKMPointList.Clear;
  FKMPointList.LoadFromStream(SaveStream);
  Check(FKMPointList.Count = 1);
  Check(KMSamePoint(FKMPointList[0], KMPoint(7,8)));
  SaveStream.Free;

  //Filled list
  FKMPointList.Clear;
  FillDefaults;
  SaveStream := TKMemoryStream.Create;
  FKMPointList.SaveToStream(SaveStream);
  SaveStream.Position := 0;
  FKMPointList.Clear;
  FKMPointList.LoadFromStream(SaveStream);
  Check(FKMPointList.Count = 256);
  Check(KMSamePoint(FKMPointList[0], KMPoint(0,0)));
  Check(KMSamePoint(FKMPointList[255], KMPoint(255,255)));
  SaveStream.Free;
end;

procedure TestTKMPointList.TestLoadFromStream;
begin
  //
end;

procedure TestTKMPointTagList.SetUp;
begin
  FKMPointTagList := TKMPointTagList.Create;
end;

procedure TestTKMPointTagList.TearDown;
begin
  FKMPointTagList.Free;
  FKMPointTagList := nil;
end;

procedure TestTKMPointTagList.FillDefaults;
var I: Integer;
begin
  for I := 0 to 255 do
    FKMPointTagList.Add(KMPoint(I, I), I, 255-I);
end;

procedure TestTKMPointTagList.TestClear;
begin
  //Clear empty
  FKMPointTagList.Clear;
  Check(FKMPointTagList.Count = 0);

  //Clear filled
  FillDefaults;
  FKMPointTagList.Clear;
  Check(FKMPointTagList.Count = 0);
end;

procedure TestTKMPointTagList.TestAdd;
var
  I: Integer;
  aLoc: TKMPoint;
  aTag: Cardinal;
  aTag2: Cardinal;
begin
  //Add values
  for I := 0 to 255 do
  begin
    aLoc := KMPoint(Random(65535), Random(65535));
    aTag := I;
    aTag2 := 255-I;
    FKMPointTagList.Add(aLoc, aTag, aTag2);
    CheckEquals(I + 1, FKMPointTagList.Count);
    Check(KMSamePoint(FKMPointTagList[I], aLoc));
    Check(FKMPointTagList.Tag[I] = aTag);
    Check(FKMPointTagList.Tag2[I] = aTag2);
  end;
end;

procedure TestTKMPointTagList.TestRemove;
var
  OldCount: Integer;
  ReturnValue: Integer;
  aLoc: TKMPoint;
begin
  //Test with no values
  ReturnValue := FKMPointTagList.Remove(KMPoint(7,8));
  Check(ReturnValue = -1);
  Check(FKMPointTagList.Count = 0);

  //Test single value
  aLoc := KMPoint(7,8);
  FKMPointTagList.Add(aLoc, 1, 1);
  ReturnValue := FKMPointTagList.Remove(aLoc);
  Check(ReturnValue = 0);
  Check(FKMPointTagList.Count = 0);

  //Test missing entry
  FKMPointTagList.Clear;
  FillDefaults;
  OldCount := FKMPointTagList.Count;
  ReturnValue := FKMPointTagList.Remove(KMPoint(7,8));
  Check(ReturnValue = -1);
  Check(FKMPointTagList.Count = OldCount);

  //Test removed second to last
  FKMPointTagList.Clear;
  FKMPointTagList.Add(KMPoint(0,0), 0, 0);
  FKMPointTagList.Add(KMPoint(1,1), 1, 1);
  FKMPointTagList.Add(KMPoint(2,2), 2, 2);
  FKMPointTagList.Add(KMPoint(3,3), 3, 3);
  ReturnValue := FKMPointTagList.Remove(KMPoint(2,2));
  Check(ReturnValue = 2);
  Check(FKMPointTagList.Count = 3);
  Check(FKMPointTagList.Tag[2] = 3);
end;

procedure TestTKMPointTagList.TestSaveToStream;
var
  SaveStream: TKMemoryStream;
begin
  //Empty list
  SaveStream := TKMemoryStream.Create;
  FKMPointTagList.SaveToStream(SaveStream);
  SaveStream.Position := 0;
  FKMPointTagList.LoadFromStream(SaveStream);
  Check(FKMPointTagList.Count = 0);
  SaveStream.Free;

  //Single entry list
  FKMPointTagList.Add(KMPoint(7,8), 1, 2);
  SaveStream := TKMemoryStream.Create;
  FKMPointTagList.SaveToStream(SaveStream);
  SaveStream.Position := 0;
  FKMPointTagList.Clear;
  FKMPointTagList.LoadFromStream(SaveStream);
  Check(FKMPointTagList.Count = 1);
  Check(KMSamePoint(FKMPointTagList[0], KMPoint(7,8)));
  Check(FKMPointTagList.Tag[0] = 1);
  Check(FKMPointTagList.Tag2[0] = 2);
  SaveStream.Free;

  //Filled list
  FKMPointTagList.Clear;
  FillDefaults;
  SaveStream := TKMemoryStream.Create;
  FKMPointTagList.SaveToStream(SaveStream);
  SaveStream.Position := 0;
  FKMPointTagList.Clear;
  FKMPointTagList.LoadFromStream(SaveStream);
  Check(FKMPointTagList.Count = 256);
  Check(KMSamePoint(FKMPointTagList[0], KMPoint(0,0)));
  Check(FKMPointTagList.Tag[0] = 0);
  Check(FKMPointTagList.Tag2[0] = 255);
  Check(KMSamePoint(FKMPointTagList[255], KMPoint(255,255)));
  Check(FKMPointTagList.Tag[255] = 255);
  Check(FKMPointTagList.Tag2[255] = 0);
  SaveStream.Free;
end;

procedure TestTKMPointTagList.TestLoadFromStream;
begin
  //
end;

procedure TestTKMPointDirList.SetUp;
begin
  FKMPointDirList := TKMPointDirList.Create;
end;

procedure TestTKMPointDirList.TearDown;
begin
  FKMPointDirList.Free;
  FKMPointDirList := nil;
end;

procedure TestTKMPointDirList.FillDefaults;
var I: Integer;
begin
  for I := 0 to 255 do
    FKMPointDirList.Add(KMPointDir(I, I, dir_N));
end;

procedure TestTKMPointDirList.TestClear;
begin
  FKMPointDirList.Clear;
  Check(FKMPointDirList.Count = 0);

  FillDefaults;
  FKMPointDirList.Clear;
  Check(FKMPointDirList.Count = 0);
end;

procedure TestTKMPointDirList.TestAdd;
var
  I: Integer;
  aLoc: TKMPointDir;
begin
  //Add values
  for I := 0 to 255 do
  begin
    aLoc := KMPointDir(Random(65535), Random(65535), TKMDirection(Random(9)));
    FKMPointDirList.Add(aLoc);
    Check(FKMPointDirList.Count = I + 1);
    Check(KMSamePointDir(FKMPointDirList[I], aLoc));
  end;
end;

procedure TestTKMPointDirList.TestGetRandom;
var
  ReturnValue: Boolean;
  aLoc, Point: TKMPointDir;
begin
  //Empty list
  ReturnValue := FKMPointDirList.GetRandom(Point);
  Check(not ReturnValue);

  //Single value
  aLoc := KMPointDir(Random(65535), Random(65535), TKMDirection(Random(9)));
  FKMPointDirList.Add(aLoc);
  ReturnValue := FKMPointDirList.GetRandom(Point);
  Check(ReturnValue);
  Check(KMSamePointDir(aLoc, Point));
end;

procedure TestTKMPointDirList.TestSaveToStream;
var
  SaveStream: TKMemoryStream;
begin
  //Empty list
  SaveStream := TKMemoryStream.Create;
  FKMPointDirList.SaveToStream(SaveStream);
  SaveStream.Position := 0;
  FKMPointDirList.LoadFromStream(SaveStream);
  Check(FKMPointDirList.Count = 0);
  SaveStream.Free;

  //Single entry list
  FKMPointDirList.Add(KMPointDir(7, 8, dir_SW));
  SaveStream := TKMemoryStream.Create;
  FKMPointDirList.SaveToStream(SaveStream);
  SaveStream.Position := 0;
  FKMPointDirList.Clear;
  FKMPointDirList.LoadFromStream(SaveStream);
  Check(FKMPointDirList.Count = 1);
  Check(KMSamePointDir(FKMPointDirList[0], KMPointDir(7, 8, dir_SW)));
  SaveStream.Free;

  //Filled list
  FKMPointDirList.Clear;
  FillDefaults;
  SaveStream := TKMemoryStream.Create;
  FKMPointDirList.SaveToStream(SaveStream);
  SaveStream.Position := 0;
  FKMPointDirList.Clear;
  FKMPointDirList.LoadFromStream(SaveStream);
  Check(FKMPointDirList.Count = 256);
  Check(KMSamePointDir(FKMPointDirList[0], KMPointDir(0,0, dir_N)));
  Check(KMSamePointDir(FKMPointDirList[255], KMPointDir(255,255, dir_N)));
  SaveStream.Free;
end;

procedure TestTKMPointDirList.TestLoadFromStream;
begin
  //
end;

initialization
  // Register any test cases with the test runner
  RegisterTest('CommonClasses', TestTKMPointList.Suite);
  RegisterTest('CommonClasses', TestTKMPointTagList.Suite);
  RegisterTest('CommonClasses', TestTKMPointDirList.Suite);
end.

