unit KM_Units;

interface

uses
  windows, math, classes, OpenGL, dglOpenGL, KromOGLUtils, KM_Terrain,
  KM_Global_Data, KM_Defaults, KM_Classes, KM_Houses;

type
TUnitType = (ut_Serf=1, //Transports goods
ut_VFarmer=5, //Villagers
ut_WHorseScout=22); //Warriors
                                           //Shoot, Run
  TUnitActionType = (ua_Walk=1, ua_Work=2, ua_Spec=3, ua_Die=4, ua_Work1=5,
                     ua_Work2=6, ua_WorkEnd=7, ua_Eat=8, ua_WalkArm=9, ua_WalkTool=10,
                     ua_WalkBooty=11, ua_WalkTool2=12, ua_WalkBooty2=13);
  TUnitActionTypeSet = set of TUnitActionType;

  TKMUnit = class;

  TUnitAction = class(TObject)
  private
    fActionType: TUnitActionType;
  public
    constructor Create(aActionType: TUnitActionType);
    procedure Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean); virtual; abstract;
    property ActionType: TUnitActionType read fActionType;
  end;

      TMoveUnitAction = class(TUnitAction)
      private
        fDestPos: TPosF;
      public
        constructor Create(PosX, PosY:integer; const aActionType:TUnitActionType=ua_Walk);
        procedure Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean); override;
      end;

      TStayUnitAction = class(TUnitAction)
      private
      TimeToStay:integer;
      StayStill:boolean;
      public
        constructor Create(aTime:integer; aActionType:TUnitActionType; const aStayStill:boolean=true);
        procedure Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean); override;
      end;

  TDeliverJob = class
  public
  FromX,FromY:integer;
  ToX,ToY:integer;
  fResource:TResourceType;
  Phase:integer;
  constructor Create(x1,y1,x2,y2:integer; Res:TResourceType);
  end;

  TMiningJob = class
  public
  HomeX,HomeY:integer;
  PlaceX,PlaceY:integer;
  fResource:TResourceType;
  Phase:integer;
  constructor Create(x1,y1,x2,y2:integer; Res:TResourceType);
  end;

  TKMUnit = class(TObject)
  private
    fCurrentAction: TUnitAction;
    fPosition: TPosF;
    fLastUpdateTime: Cardinal;
    fOwner: string;
    fUnitType:TUnitType;
    Direction: integer;
    AnimStep: integer;
    fVisible:boolean;
  public
    constructor Create(const aOwner: string; PosX, PosY:integer; aUnitType:TUnitType);
    destructor Destroy; override;
    function GetSupportedActions: TUnitActionTypeSet; virtual; abstract;
    function HitTest(X, Y: Integer): Boolean;
    procedure SetAction(aAction: TUnitAction);
    procedure UpdateState; virtual; abstract;
    procedure Paint; virtual; abstract;
  end;

  TKMFarmer = class(TKMUnit)
  public
    fHome:TPoint;
    MiningJob: TMiningJob;
    function GetSupportedActions: TUnitActionTypeSet; override;
    function FindHome():boolean;
    procedure UpdateState; override;
    function InitiateMining():boolean;
    procedure PerformMining;
    procedure Paint(); override;
  end;

  TKMSerf = class(TKMUnit)
    DeliverJob: TDeliverJob;
    Carry: TResourceType;
  public
    function GetSupportedActions: TUnitActionTypeSet; override;
    procedure UpdateState; override;
    procedure Paint(); override;
    function GiveResource(Res:TResourceType):boolean;
    function TakeResource(Res:TResourceType):boolean;
    function GetActionFromQueue():boolean;
    procedure PerformDelivery;
  end;

  TKMwarrior = class(TKMUnit)
  public
    function GetSupportedActions: TUnitActionTypeSet; override;
    procedure UpdateState; override;
    procedure Paint(); override;
  end;

  TKMUnitsCollection = class(TKMList)
  private
    fSelectedUnit: TKMUnit;
  public
    procedure Add(aOwner: string; aUnitType: TUnitType; PosX, PosY:integer);
    procedure Rem(PosX,PosY:integer);
    procedure UpdateState;
    function HitTest(X, Y: Integer): TKMUnit;
    procedure Paint();
    property SelectedUnit: TKMUnit read fSelectedUnit;
  end;

implementation
uses KM_Unit1;

{ TKMFarmer }

function TKMFarmer.GetSupportedActions: TUnitActionTypeSet;
begin      //1.2.4.5.6.8.9.10.11.12.13
  Result:= [ua_Walk, ua_Work, ua_Die, ua_Work1, ua_Work2, ua_Eat..ua_WalkBooty2];
end;

function TKMFarmer.FindHome():boolean;
var KMHouse:TKMHouse;
begin
Result:=false;
KMHouse:=ControlList.FindEmptyHouse(THouseType(9));
if KMHouse<>nil then begin fHome:=KMHouse.GetPosition; Result:=true; end;
end;

procedure TKMFarmer.Paint();
var UnitType,Owner:integer; AnimAct:integer;
begin
if not fVisible then exit;
Owner:=2;//should be inherited from =Player=
UnitType:=integer(fUnitType);
AnimAct:=integer(fCurrentAction.fActionType);

case fCurrentAction.fActionType of
ua_Walk:
  begin
    fRender.RenderUnit(UnitType,       1, Direction, AnimStep, Owner, fPosition.X+0.5, fPosition.Y+1);
    fRender.RenderUnit(UnitType,       9, Direction, AnimStep, Owner, fPosition.X+0.5, fPosition.Y+1);
  end;
ua_Work..ua_Eat:
    fRender.RenderUnit(UnitType, AnimAct, Direction, AnimStep, Owner, fPosition.X+0.5, fPosition.Y+1);
ua_WalkArm .. ua_WalkBooty2:
  begin
    fRender.RenderUnit(UnitType,       1, Direction, AnimStep, Owner, fPosition.X+0.5, fPosition.Y+1);
    fRender.RenderUnit(UnitType, AnimAct, Direction, AnimStep, Owner, fPosition.X+0.5, fPosition.Y+1);
  end;
end;
end;

procedure TKMFarmer.UpdateState;
var
  TimeDelta: Cardinal;
  DoEnd: Boolean;
begin
  TimeDelta:= GetTickCount - fLastUpdateTime;
  fLastUpdateTime:= GetTickCount;
  if fCurrentAction <> nil then
    fCurrentAction.Execute(Self, TimeDelta/1000, DoEnd);
  if (DoEnd) then begin
    if (fHome.X=0)and(fHome.Y=0) then begin
      if FindHome then
        SetAction(TMoveUnitAction.Create(fHome.X,fHome.Y))
      else
        SetAction(TStayUnitAction.Create(120, ua_Walk));
    end else
    if MiningJob<>nil then
      PerformMining
    else
      InitiateMining;
  end;
end;

function TKMFarmer.InitiateMining():boolean;
begin
MiningJob:=TMiningJob.Create(4,5,5,8,rt_Corn);
MiningJob.Phase:=0;
Result:=true;
end;

procedure TKMFarmer.PerformMining;
var KMHouse:TKMHouse;
begin
case MiningJob.Phase of
0: begin
     KMHouse:=ControlList.HousesHitTest(MiningJob.HomeX,MiningJob.HomeY);
     KMHouse.SetAction(THouseAction.Create(hat_Empty));
   end;
1: fVisible:=true;
2: SetAction(TMoveUnitAction.Create(MiningJob.HomeX,MiningJob.HomeY+1));
3: SetAction(TMoveUnitAction.Create(MiningJob.PlaceX,MiningJob.PlaceY));
4: SetAction(TStayUnitAction.Create(50, ua_Work,false));
5: SetAction(TMoveUnitAction.Create(MiningJob.HomeX,MiningJob.HomeY+1,ua_WalkBooty));
6: SetAction(TMoveUnitAction.Create(MiningJob.HomeX,MiningJob.HomeY,ua_WalkBooty));
7: fVisible:=false;
8: begin
     KMHouse:=ControlList.HousesHitTest(MiningJob.HomeX,MiningJob.HomeY);
     KMHouse.AddResource(rt_Corn);
     KMHouse.SetAction(THouseAction.Create(hat_Idle));
   end;
end;
inc(MiningJob.Phase);
if MiningJob.Phase=9 then begin
MiningJob.Free;
MiningJob:=nil;
SetAction(TStayUnitAction.Create(25,ua_Walk));
end;
end;

{ TKMSerf }

function TKMSerf.GetSupportedActions: TUnitActionTypeSet;
begin
  Result:= [ua_Walk, ua_Die, ua_Eat];
end;

procedure TKMSerf.Paint();
var UnitType,Owner:integer; AnimAct:integer;
begin
  if not fVisible then exit;
  Owner:=2;//should be inherited from =Player=
  UnitType:=1;
  AnimAct:=integer(fCurrentAction.fActionType); //should correspond with UnitAction

  fRender.RenderUnit(UnitType, AnimAct, Direction, AnimStep, Owner, fPosition.X+0.5, fPosition.Y+1);

  if Carry<>rt_None then
    fRender.RenderUnitCarry(integer(Carry), Direction, AnimStep, Owner, fPosition.X+0.5, fPosition.Y+1)
  else
    fRender.RenderUnit(UnitType, 9, Direction, AnimStep, Owner, fPosition.X+0.5, fPosition.Y+1);
end;

procedure TKMSerf.UpdateState;
var
  TimeDelta: Cardinal;
  DoEnd: Boolean;
begin
  TimeDelta:= GetTickCount - fLastUpdateTime;
  fLastUpdateTime:= GetTickCount;
  if fCurrentAction <> nil then
    fCurrentAction.Execute(Self, TimeDelta/1000, DoEnd);
  if DoEnd then begin
    if DeliverJob=nil then
      if GetActionFromQueue() then
      else
        SetAction(TStayUnitAction.Create(50,ua_Walk))
    else
      PerformDelivery;
  end;
end;

function TKMSerf.GiveResource(Res:TResourceType):boolean;
begin
Result:=false;
if Carry<>rt_None then exit;
Carry:=Res;
Result:=true;
end;

function TKMSerf.TakeResource(Res:TResourceType):boolean;
begin
Carry:=rt_None;
Result:=true;
end;

function TKMSerf.GetActionFromQueue():boolean;
begin
DeliverJob:=TDeliverJob.Create(4,4,8,5,rt_Flour);
DeliverJob.Phase:=0;
Result:=true;
end;

procedure TKMSerf.PerformDelivery;
var KMHouse:TKMHouse;
begin
case DeliverJob.Phase of
0: SetAction(TMoveUnitAction.Create(DeliverJob.FromX,DeliverJob.FromY+1));
1: SetAction(TMoveUnitAction.Create(DeliverJob.FromX,DeliverJob.FromY));
2: fVisible:=false;
3: SetAction(TStayUnitAction.Create(5,ua_Walk));
4: begin
     KMHouse:=ControlList.HousesHitTest(DeliverJob.FromX,DeliverJob.FromY);
     if (KMHouse <> nil) and KMHouse.RemResource(DeliverJob.fResource) then
       GiveResource(DeliverJob.fResource)
     else
     begin
       SetAction(nil);
       DeliverJob.Free;
       exit;
     end;
   end;
5: fVisible:=true;
6: SetAction(TMoveUnitAction.Create(DeliverJob.FromX,DeliverJob.FromY+1));
7: SetAction(TMoveUnitAction.Create(DeliverJob.ToX,DeliverJob.ToY+1));
8: SetAction(TMoveUnitAction.Create(DeliverJob.ToX,DeliverJob.ToY));
9: fVisible:=false;
10: SetAction(TStayUnitAction.Create(5,ua_Walk));
11: begin
     KMHouse:=ControlList.HousesHitTest(DeliverJob.ToX,DeliverJob.ToY);
     KMHouse.AddResource(Carry);
     TakeResource(Carry);
   end;
12: fVisible:=true;
13: SetAction(TMoveUnitAction.Create(DeliverJob.ToX,DeliverJob.ToY+1));
end;
inc(DeliverJob.Phase);
if DeliverJob.Phase=15 then begin
DeliverJob.Free;
DeliverJob:=nil;
SetAction(TStayUnitAction.Create(15,ua_Walk));
end;
end;

{ TKMwarrior }

function TKMwarrior.GetSupportedActions: TUnitActionTypeSet;
begin
  Result:= [ua_Walk, ua_Work1, ua_Die];
end;

procedure TKMwarrior.Paint();
var UnitType,Owner:integer; AnimAct:integer;
begin
Owner:=2;//should be inherited from =Player=
UnitType:=22;
AnimAct:=integer(fCurrentAction.fActionType); //should correspond with UnitAction
fRender.RenderUnit(UnitType, AnimAct, Direction, AnimStep, Owner, fPosition.X+0.5, fPosition.Y+1);
end;

procedure TKMwarrior.UpdateState;
var
  TimeDelta: Cardinal;
  DoEnd: Boolean;
begin
  TimeDelta:= GetTickCount - fLastUpdateTime;
  fLastUpdateTime:= GetTickCount;
  if fCurrentAction <> nil then
    fCurrentAction.Execute(Self, TimeDelta/1000, DoEnd);
  if DoEnd then
    SetAction(TStayUnitAction.Create(50,ua_Walk))
end;

{ TKMUnit }

constructor TKMUnit.Create(const aOwner: string; PosX, PosY:integer; aUnitType:TUnitType);
begin
  Inherited Create;
  fPosition.X:= PosX;
  fPosition.Y:= PosY;
  fOwner:= aOwner;
  fUnitType:=aUnitType;
  Direction:=1;
  fVisible:=true;
  SetAction(TStayUnitAction.Create(10,ua_Walk));
end;

destructor TKMUnit.Destroy;
begin
  Inherited;
  fCurrentAction.Free;
end;

function TKMUnit.HitTest(X, Y: Integer): Boolean;
begin
  Result:= (X = round(fPosition.X)) and (Y = round(fPosition.Y));
end;

procedure TKMUnit.SetAction(aAction: TUnitAction);
begin
AnimStep:=0;
  if aAction = nil then
  begin
    fCurrentAction.Free;
    fCurrentAction:= nil;
    Exit;
  end;
  if not (aAction.ActionType in GetSupportedActions) then
  begin
    aAction.Free;
    exit;
  end;
  if fCurrentAction <> aAction then
  begin
    fCurrentAction.Free;
    fCurrentAction:= aAction;
  end;
end;

{ TDeliverJob }
constructor TDeliverJob.Create(x1,y1,x2,y2:integer; Res:TResourceType);
begin
FromX:=x1; FromY:=y1;
ToX:=x2; ToY:=y2;
fResource:=Res;
Phase:=0;
end;

{ TMiningJob }
constructor TMiningJob.Create(x1,y1,x2,y2:integer; Res:TResourceType);
begin
HomeX:=x1; HomeY:=y1;
PlaceX:=x2; PlaceY:=y2;
fResource:=Res;
Phase:=0;
end;

{ TUnitAction }

constructor TUnitAction.Create(aActionType: TUnitActionType);
begin
  Inherited Create;
  fActionType:= aActionType;
end;

{ TMoveUnitAction }

constructor TMoveUnitAction.Create(PosX, PosY:integer; const aActionType:TUnitActionType=ua_Walk);
begin
  Inherited Create(aActionType);
  fDestPos.X:= PosX;
  fDestPos.Y:= PosY;
end;

procedure TMoveUnitAction.Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean);
var
  DX, DY, Distance:single;
begin
  DoEnd:= False;
  Distance:= TimeDelta * 3;//KMUnit.Speed;
  DX:= fDestPos.X - KMUnit.fPosition.X;
  DY:= fDestPos.Y - KMUnit.fPosition.Y;

  if (DX<0) and (DY<0) then KMUnit.Direction:=8;
  if (DX<0) and (DY=0) then KMUnit.Direction:=7;
  if (DX<0) and (DY>0) then KMUnit.Direction:=6;
  if (DX=0) and (DY>0) then KMUnit.Direction:=5;
  if (DX>0) and (DY>0) then KMUnit.Direction:=4;
  if (DX>0) and (DY=0) then KMUnit.Direction:=3;
  if (DX>0) and (DY<0) then KMUnit.Direction:=2;
  if (DX=0) and (DY<0) then KMUnit.Direction:=1;

  //Diagonal movement should take more time
  if (DX <> 0) and (DY <> 0) then
    Distance:=Distance / sqrt (2);

    KMUnit.fPosition.X:= KMUnit.fPosition.X + sign(DX)*min(Distance,abs(DX));
    KMUnit.fPosition.Y:= KMUnit.fPosition.Y + sign(DY)*min(Distance,abs(DY));

  if (KMUnit.fPosition.X = fDestPos.X) and (KMUnit.fPosition.Y = fDestPos.Y) then
    DoEnd:= True
  else
    inc(KMUnit.AnimStep);
end;

constructor TStayUnitAction.Create(aTime:integer; aActionType:TUnitActionType; const aStayStill:boolean=true);
begin
  Inherited Create(aActionType);
  TimeToStay:=aTime;
  StayStill:=aStayStill;
end;

procedure TStayUnitAction.Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean);
begin
  DoEnd:= False;
  if not StayStill then inc(KMUnit.AnimStep);
  dec(TimeToStay);
  if TimeToStay<=0 then
    DoEnd:=true;
end;

{ TKMUnitsCollection }

procedure TKMUnitsCollection.Add(aOwner: string; aUnitType: TUnitType; PosX, PosY:integer);
begin
  case aUnitType of
    ut_Serf: Inherited Add(TKMSerf.Create(aOwner,PosX,PosY,aUnitType));
    ut_VFarmer:  Inherited Add(TKMFarmer.Create(aOwner,PosX,PosY,aUnitType));
    ut_WHorseScout: Inherited Add(TKMwarrior.Create(aOwner,PosX,PosY,aUnitType));
  end;
end;

procedure TKMUnitsCollection.Rem(PosX,PosY:integer);
begin
  if HitTest(PosX,PosY)<>nil then Remove(HitTest(PosX,PosY));
end;

function TKMUnitsCollection.HitTest(X, Y: Integer): TKMUnit;
var
  I: Integer;
begin
  Result:= nil;
  for I := 0 to Count - 1 do
    if TKMUnit(Items[I]).HitTest(X, Y) then
    begin
      Result:= TKMUnit(Items[I]);
      Break;
    end;
  fSelectedUnit:= Result;
end;

procedure TKMUnitsCollection.Paint();
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TKMUnit(Items[I]).Paint();
end;

procedure TKMUnitsCollection.UpdateState;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TKMUnit(Items[I]).UpdateState;
end;

{function TKMUnitsCollection.GetPrice(aUnitType: TUnitType): TMoney;
begin
  case aUnitType of
    ut_Peasant:
      Result.Gold:= 100;
    ut_Warrior:
      Result.Gold:= 200;
  end;
end;}

end.
