unit KM_Units;

interface

uses
  KM_Defaults, windows, math, classes, OpenGL, dglOpenGL, KromOGLUtils, KM_Terrain,
  KM_Global_Data, KM_Classes, KM_Houses, KromUtils;

type
TUnitType = (ut_Serf=1, //Transports goods
ut_VStoneCutter=11,     //Villagers
ut_VFarmer=5,           //Villagers
ut_WHorseScout=22);     //Warriors
                                           //Shoot, Run
  TUnitActionType = (ua_Walk=1, ua_Work=2, ua_Spec=3, ua_Die=4, ua_Work1=5,
                     ua_Work2=6, ua_WorkEnd=7, ua_Eat=8, ua_WalkArm=9, ua_WalkTool=10,
                     ua_WalkBooty=11, ua_WalkTool2=12, ua_WalkBooty2=13);
  TUnitActionTypeSet = set of TUnitActionType;

  TKMUnit = class;
  TKMSerf = class;

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
        fDestPos: TKMPoint;
      public
        constructor Create(Loc:TKMPoint; const aActionType:TUnitActionType=ua_Walk);
        procedure Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean); override;
      end;

      TStayUnitAction = class(TUnitAction)
      private
      StayStill:boolean;
      TimeToStay:integer;
       public
        constructor Create(aTimeToStay:integer; aActionType:TUnitActionType; const aStayStill:boolean=true);
        procedure Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean); override;
      end;

  TDeliverJob = class
  private
  Phase:byte;
  fSerf:TKMSerf;
  fFrom,fTo:TKMPoint;
  fResource:TResourceType;
  ID:integer;
  public
  Done:boolean;
  constructor Create(aSerf:TKMSerf; aFrom,aTo:TKMPoint; Res:TResourceType; aID:integer);
  procedure Execute;
  end;

  TMiningJob = class
  private
  Phase:byte;
  fHome:TKMHouse;
  fUnit:TKMUnit;
  fPlace:TKMPoint;
  fResource:TResourceType;
  fCount:integer;
  fAction1,fAction2,fAction3:TUnitActionType;
  fTimeToWork:integer; //Number of work cycles to perform
  public
  Done:boolean;
  constructor Create(aHome:TKMHouse; aUnit:TKMUnit; aPlace:TKMPoint; aRes:TResourceType; aCount:integer; aAction1,aAction2,aAction3:TUnitActionType; aCycles:integer);
  procedure Execute;
  end;

  TKMUnit = class(TObject)
  private
    fCurrentAction: TUnitAction;
    fPosition: TKMPointF;
    fLastUpdateTime: Cardinal;
    fOwner: string;
    fUnitType:TUnitType;
    Direction: TKMDirection;
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
    fHome:TKMHouse;
    MiningJob: TMiningJob;
    function GetSupportedActions: TUnitActionTypeSet; override;
    function FindHome():boolean;
    procedure UpdateState; override;
    function InitiateMining():boolean;
    procedure Paint(); override;
  end;

  TKMStoneCutter = class(TKMUnit)
  public
    fHome:TKMHouse;
    MiningJob: TMiningJob;
    function GetSupportedActions: TUnitActionTypeSet; override;
    function FindHome():boolean;
    procedure UpdateState; override;
    function InitiateMining():boolean;
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
if KMHouse<>nil then begin fHome:=KMHouse; Result:=true; end;
end;

procedure TKMFarmer.Paint();
var UnitType,Owner:integer; AnimAct,AnimDir:integer;
begin
if not fVisible then exit;
Owner:=2;//should be inherited from =Player=
UnitType:=integer(fUnitType);
AnimAct:=integer(fCurrentAction.fActionType);
AnimDir:=integer(Direction);

case fCurrentAction.fActionType of
ua_Walk:
  begin
    fRender.RenderUnit(UnitType,       1, AnimDir, AnimStep, Owner, fPosition.X+0.5, fPosition.Y+1);
    fRender.RenderUnit(UnitType,       9, AnimDir, AnimStep, Owner, fPosition.X+0.5, fPosition.Y+1);
  end;
ua_Work..ua_Eat:
    fRender.RenderUnit(UnitType, AnimAct, AnimDir, AnimStep, Owner, fPosition.X+0.5, fPosition.Y+1);
ua_WalkArm .. ua_WalkBooty2:
  begin
    fRender.RenderUnit(UnitType,       1, AnimDir, AnimStep, Owner, fPosition.X+0.5, fPosition.Y+1);
    fRender.RenderUnit(UnitType, AnimAct, AnimDir, AnimStep, Owner, fPosition.X+0.5, fPosition.Y+1);
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
    if (fHome=nil) then begin
      if FindHome then
        SetAction(TMoveUnitAction.Create(fHome.GetPosition))
      else
        SetAction(TStayUnitAction.Create(120, ua_Walk));
    end else
    if (MiningJob<>nil)and(not MiningJob.Done) then
      MiningJob.Execute
    else
      InitiateMining;
  end;
end;

function TKMFarmer.InitiateMining():boolean;
var aPlace:TKMPoint;
begin
aPlace:=KMPoint(5,8);
if MiningJob<>nil then MiningJob.Free;
MiningJob:=TMiningJob.Create(fHome,Self,aPlace,rt_Corn,1,ua_Walk,ua_Work,ua_WalkBooty,4);
MiningJob.Phase:=0;
Result:=true;
end;

{TKMStoneCutter}

function TKMStoneCutter.GetSupportedActions: TUnitActionTypeSet;
begin      //1.2.4.5.6.8.9.10.11.12.13
  Result:= [ua_Walk, ua_Work, ua_Die, ua_Work1, ua_Eat..ua_WalkBooty];
end;

function TKMStoneCutter.FindHome():boolean;
var KMHouse:TKMHouse;
begin
Result:=false;
KMHouse:=ControlList.FindEmptyHouse(THouseType(15));
if KMHouse<>nil then begin fHome:=KMHouse; Result:=true; end;
end;

procedure TKMStoneCutter.Paint();
var UnitType,Owner:integer; AnimAct,AnimDir:integer;
begin
if not fVisible then exit;
Owner:=2;//should be inherited from =Player=
UnitType:=integer(fUnitType);
AnimAct:=integer(fCurrentAction.fActionType);
AnimDir:=integer(Direction);

case fCurrentAction.fActionType of
ua_Walk:
  begin
    fRender.RenderUnit(UnitType,       1, AnimDir, AnimStep, Owner, fPosition.X+0.5, fPosition.Y+1);
    fRender.RenderUnit(UnitType,       9, AnimDir, AnimStep, Owner, fPosition.X+0.5, fPosition.Y+1);
  end;
ua_Work..ua_Eat:
    fRender.RenderUnit(UnitType, AnimAct, AnimDir, AnimStep, Owner, fPosition.X+0.5, fPosition.Y+1);
ua_WalkArm .. ua_WalkBooty2:
  begin
    fRender.RenderUnit(UnitType,       1, AnimDir, AnimStep, Owner, fPosition.X+0.5, fPosition.Y+1);
    fRender.RenderUnit(UnitType, AnimAct, AnimDir, AnimStep, Owner, fPosition.X+0.5, fPosition.Y+1);
  end;
end;
end;

procedure TKMStoneCutter.UpdateState;
var
  TimeDelta: Cardinal;
  DoEnd: Boolean;
begin
  TimeDelta:= GetTickCount - fLastUpdateTime;
  fLastUpdateTime:= GetTickCount;
  if fCurrentAction <> nil then
    fCurrentAction.Execute(Self, TimeDelta/1000, DoEnd);
  if (DoEnd) then begin
    if (fHome=nil) then begin
      if FindHome then
        SetAction(TMoveUnitAction.Create(fHome.GetPosition))
      else
        SetAction(TStayUnitAction.Create(120, ua_Walk));
    end else
    if (MiningJob<>nil)and(not MiningJob.Done) then
      MiningJob.Execute
    else
      InitiateMining;
  end;
end;

function TKMStoneCutter.InitiateMining():boolean;
var aPlace:TKMPoint;
begin
aPlace:=KMPoint(8,11);
if MiningJob<>nil then MiningJob.Free;
MiningJob:=TMiningJob.Create(fHome,Self,aPlace,rt_Stone,3,ua_Walk,ua_Work,ua_WalkTool,1);
MiningJob.Phase:=0;
Result:=true;
end;

{ TKMSerf }

function TKMSerf.GetSupportedActions: TUnitActionTypeSet;
begin
  Result:= [ua_Walk, ua_Die, ua_Eat];
end;

procedure TKMSerf.Paint();
var UnitType,Owner:integer; AnimAct,AnimDir:integer;
begin
  if not fVisible then exit;
  Owner:=2;//should be inherited from =Player=
  UnitType:=1;
  AnimAct:=integer(fCurrentAction.fActionType); //should correspond with UnitAction
  AnimDir:=integer(Direction);

  fRender.RenderUnit(UnitType, AnimAct, AnimDir, AnimStep, Owner, fPosition.X+0.5, fPosition.Y+1);

  if Carry<>rt_None then
    fRender.RenderUnitCarry(integer(Carry), AnimDir, AnimStep, Owner, fPosition.X+0.5, fPosition.Y+1)
  else
    fRender.RenderUnit(UnitType, 9, AnimDir, AnimStep, Owner, fPosition.X+0.5, fPosition.Y+1);
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
    if (DeliverJob<>nil)and(not DeliverJob.Done) then
      DeliverJob.Execute
    else
    if GetActionFromQueue() then
      else
        SetAction(TStayUnitAction.Create(10,ua_Walk))
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
if DeliverJob<>nil then DeliverJob.Free;
DeliverJob:=ControlList.JobList.AskForJob(Self);
if DeliverJob<>nil then Result:=true else Result:=false;
end;

{ TKMwarrior }

function TKMwarrior.GetSupportedActions: TUnitActionTypeSet;
begin
  Result:= [ua_Walk, ua_Work1, ua_Die];
end;

procedure TKMwarrior.Paint();
var UnitType,Owner:integer; AnimAct,AnimDir:integer;
begin
Owner:=2;//should be inherited from =Player=
UnitType:=22;
AnimAct:=integer(fCurrentAction.fActionType); //should correspond with UnitAction
AnimDir:=integer(Direction);
fRender.RenderUnit(UnitType, AnimAct, AnimDir, AnimStep, Owner, fPosition.X+0.5, fPosition.Y+1);
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
  Direction:=dir_N;
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
constructor TDeliverJob.Create(aSerf:TKMSerf; aFrom,aTo:TKMPoint; Res:TResourceType; aID:integer);
begin
fSerf:=aSerf;
fFrom:=aFrom;
fTo:=aTo;
fResource:=Res;
Phase:=0;
ID:=aID;
Done:=false;
end;

procedure TDeliverJob.Execute;
var KMHouse:TKMHouse;
begin
with fSerf do
case Phase of
0: SetAction(TMoveUnitAction.Create(KMPointY1(fFrom)));
1: SetAction(TMoveUnitAction.Create(fFrom));
2: fVisible:=false;
3: SetAction(TStayUnitAction.Create(5,ua_Walk));
4: begin
     KMHouse:=ControlList.HousesHitTest(fFrom.X,fFrom.Y);
     if (KMHouse <> nil) and KMHouse.RemResource(fResource) then
       GiveResource(fResource)
     else
     begin
       SetAction(nil);
       DeliverJob.Free;
       exit;
     end;
   end;
5: fVisible:=true;
6: SetAction(TMoveUnitAction.Create(KMPointY1(DeliverJob.fFrom)));
7: SetAction(TMoveUnitAction.Create(KMPointY1(DeliverJob.fTo)));
8: SetAction(TMoveUnitAction.Create(DeliverJob.fTo));
9: fVisible:=false;
10: SetAction(TStayUnitAction.Create(5,ua_Walk));
11: begin
     KMHouse:=ControlList.HousesHitTest(DeliverJob.fTo.X,DeliverJob.fTo.Y);
     KMHouse.AddResource(Carry);
     TakeResource(Carry);
   end;
12: fVisible:=true;
13: SetAction(TMoveUnitAction.Create(KMPointY1(DeliverJob.fTo)));
14: ControlList.JobList.CloseJob(DeliverJob.ID);
15: Done:=true;
end;
inc(Phase);
end;

{ TMiningJob }
constructor TMiningJob.Create(aHome:TKMHouse; aUnit:TKMUnit; aPlace:TKMPoint; aRes:TResourceType;
                              aCount:integer; aAction1,aAction2,aAction3:TUnitActionType; aCycles:integer);
begin
fHome:=aHome;
fUnit:=aUnit;
fPlace:=aPlace;
fResource:=aRes;
fCount:=aCount;
fAction1:=aAction1;
fAction2:=aAction2;
fAction3:=aAction3;
fTimeToWork:=aCycles*max(UnitSprite[integer(fUnit.fUnitType)].Act[integer(aAction2)].Dir[1].Count,1);
Phase:=0;
Done:=false;
end;

procedure TMiningJob.Execute;
begin
with fUnit do
case Phase of
0: fHome.SetAction(hat_Empty);
1: fVisible:=true;
2: SetAction(TMoveUnitAction.Create(KMPointY1(fHome.GetPosition),fAction1));
3: if fResource=rt_Stone then SetAction(TMoveUnitAction.Create(KMPointY1(fPlace)));
4: SetAction(TMoveUnitAction.Create(fPlace,fAction1));
5: SetAction(TStayUnitAction.Create(fTimeToWork, fAction2,false));
6: SetAction(TMoveUnitAction.Create(KMPointY1(fHome.GetPosition),fAction3));
7: SetAction(TMoveUnitAction.Create(fHome.GetPosition,fAction3));
8: fVisible:=false;
9: fHome.AddResource(fResource,fCount);
10:fHome.SetAction(hat_Idle);
11:SetAction(TStayUnitAction.Create(25,ua_Walk));
12:Done:=true;
end;
inc(Phase);
end;

{ TUnitAction }

constructor TUnitAction.Create(aActionType: TUnitActionType);
begin
  Inherited Create;
  fActionType:= aActionType;
end;

{ TMoveUnitAction }

constructor TMoveUnitAction.Create(Loc:TKMPoint; const aActionType:TUnitActionType=ua_Walk);
begin
  Inherited Create(aActionType);
  fDestPos:= Loc;
end;

procedure TMoveUnitAction.Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean);
var
  DX, DY, Distance:single;
begin
  DoEnd:= False;
  TimeDelta:=0.1;
  Distance:= TimeDelta * 1;//KMUnit.Speed;
  DX:= fDestPos.X - KMUnit.fPosition.X;
  DY:= fDestPos.Y - KMUnit.fPosition.Y;

  if (DX<0) and (DY<0) then KMUnit.Direction:=dir_NW;
  if (DX<0) and (DY=0) then KMUnit.Direction:=dir_W;
  if (DX<0) and (DY>0) then KMUnit.Direction:=dir_SW;
  if (DX=0) and (DY>0) then KMUnit.Direction:=dir_S;
  if (DX>0) and (DY>0) then KMUnit.Direction:=dir_SE;
  if (DX>0) and (DY=0) then KMUnit.Direction:=dir_E;
  if (DX>0) and (DY<0) then KMUnit.Direction:=dir_NE;
  if (DX=0) and (DY<0) then KMUnit.Direction:=dir_N;

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

constructor TStayUnitAction.Create(aTimeToStay:integer; aActionType:TUnitActionType; const aStayStill:boolean=true);
begin
  Inherited Create(aActionType);
  StayStill:=aStayStill;
  TimeToStay:=aTimeToStay;
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
    ut_VStoneCutter:  Inherited Add(TKMStoneCutter.Create(aOwner,PosX,PosY,aUnitType));
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

end.
