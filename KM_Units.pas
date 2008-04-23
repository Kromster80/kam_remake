unit KM_Units;

interface

uses
  windows, math, classes, OpenGL, dglOpenGL, KromOGLUtils, KM_Terrain,
  KM_Global_Data, KM_Defaults, KM_Classes, KM_Houses;

type
  TUnitType = (ut_Worker, ut_Peasant, ut_Warrior);
  TUnitActionType = (ua_Move=0, ua_Idle=0, ua_Die=3, ua_Run, ua_Attack);
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
        constructor Create(PosX, PosY:integer);
        procedure Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean); override;
      end;

      TIdleUnitAction = class(TUnitAction)
      private
      public
        constructor Create();
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

  TKMUnit = class(TObject)
  private
    fCurrentAction: TUnitAction;
    fPosition: TPosF;
    fLastUpdateTime: Cardinal;
    fOwner: string;
    Direction: integer;
    AnimStep: integer;
  public
    constructor Create(const aOwner: string; PosX, PosY:integer);
    destructor Destroy; override;
    function GetSupportedActions: TUnitActionTypeSet; virtual; abstract;
    function HitTest(X, Y: Integer): Boolean;
//    function GiveResource(Res:TResourceType):boolean;
//    function TakeResource(Res:TResourceType):boolean;
    procedure SetAction(aAction: TUnitAction);
//    procedure GetActionFromQueue;
//    procedure PerformDelivery;
    procedure UpdateState; virtual; abstract;
    procedure Paint; virtual; abstract;
  end;

  TKMWorker = class(TKMUnit)
  public
    function GetSupportedActions: TUnitActionTypeSet; override;
    procedure UpdateState; override;
    procedure Paint(); override;
  end;

  TKMPeasant = class(TKMUnit)
    DeliverJob: TDeliverJob;
    Carry: TResourceType;
  public
    function GetSupportedActions: TUnitActionTypeSet; override;
    procedure UpdateState; override;
    procedure Paint(); override;
    function GiveResource(Res:TResourceType):boolean;
    function TakeResource(Res:TResourceType):boolean;
    procedure GetActionFromQueue;
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

{ TKMWorker }

function TKMWorker.GetSupportedActions: TUnitActionTypeSet;
begin
  Result:= [ua_Move, ua_Idle, ua_Die];
end;

procedure TKMWorker.Paint();
var UnitType,Owner:integer; AnimAct:integer;
begin
Owner:=2;//should be inherited from =Player=
UnitType:=1;

if fCurrentAction=nil then
  AnimAct:=0
else
  AnimAct:=integer(fCurrentAction.fActionType); //should correspond with UnitAction

fRender.RenderUnit(UnitType, AnimAct+1, Direction, AnimStep, Owner, fPosition.X+0.5, fPosition.Y+1);

end;

procedure TKMWorker.UpdateState;
var
  TimeDelta: Cardinal;
  DoEnd: Boolean;
begin
  TimeDelta:= GetTickCount - fLastUpdateTime;
  fLastUpdateTime:= GetTickCount;
  if fCurrentAction <> nil then
    fCurrentAction.Execute(Self, TimeDelta/1000, DoEnd);
end;

{ TKMPeasant }

function TKMPeasant.GetSupportedActions: TUnitActionTypeSet;
begin
  Result:= [ua_Move, ua_Idle, ua_Die];
end;

procedure TKMPeasant.Paint();
var UnitType,Owner:integer; AnimAct:integer;
begin
Owner:=2;//should be inherited from =Player=
UnitType:=1;

if fCurrentAction=nil then
  AnimAct:=0
else
  AnimAct:=integer(fCurrentAction.fActionType); //should correspond with UnitAction

fRender.RenderUnit(UnitType, AnimAct+1, Direction, AnimStep, Owner, fPosition.X+0.5, fPosition.Y+1);

if Carry<>rt_None then
  fRender.RenderUnitCarry(integer(Carry), Direction, AnimStep, Owner, fPosition.X+0.5, fPosition.Y+1)
else
  begin
    AnimAct:=8;
    fRender.RenderUnit(UnitType, AnimAct+1, Direction, AnimStep, Owner, fPosition.X+0.5, fPosition.Y+1);
  end;
end;

procedure TKMPeasant.UpdateState;
var
  TimeDelta: Cardinal;
  DoEnd: Boolean;
begin
  TimeDelta:= GetTickCount - fLastUpdateTime;
  fLastUpdateTime:= GetTickCount;
  if fCurrentAction <> nil then
    fCurrentAction.Execute(Self, TimeDelta/1000, DoEnd);
  if fCurrentAction=nil then
    GetActionFromQueue();
  if DoEnd then
    PerformDelivery;
end;

function TKMPeasant.GiveResource(Res:TResourceType):boolean;
begin
Result:=false;
if Carry<>rt_None then exit;
Carry:=Res;
Result:=true;
end;

function TKMPeasant.TakeResource(Res:TResourceType):boolean;
begin
Carry:=rt_None;
Result:=true;
end;

procedure TKMPeasant.GetActionFromQueue;
begin
DeliverJob:=TDeliverJob.Create(4,5,8,5,rt_Flour);
DeliverJob.Phase:=0;
end;

procedure TKMPeasant.PerformDelivery;
var KMHouse:TKMHouse;
begin
case DeliverJob.Phase of
0: SetAction(TMoveUnitAction.Create(DeliverJob.FromX,DeliverJob.FromY+1));
1: SetAction(TMoveUnitAction.Create(DeliverJob.FromX,DeliverJob.FromY));
2: begin
     KMHouse:=ControlList.HousesHitTest(DeliverJob.FromX,DeliverJob.FromY);
     if (KMHouse <> nil) and KMHouse.RemResource(DeliverJob.fResource) then
       GiveResource(DeliverJob.fResource)
     else
     begin
       SetAction(nil);
       DeliverJob.Free;
     end;
   end;
3: SetAction(TMoveUnitAction.Create(DeliverJob.FromX,DeliverJob.FromY+1));
4: SetAction(TMoveUnitAction.Create(DeliverJob.ToX,DeliverJob.ToY+1));
5: SetAction(TMoveUnitAction.Create(DeliverJob.ToX,DeliverJob.ToY));
6: begin
     KMHouse:=ControlList.HousesHitTest(DeliverJob.ToX,DeliverJob.ToY);
     KMHouse.AddResource(Carry);
     TakeResource(Carry);
   end;
7: SetAction(TMoveUnitAction.Create(DeliverJob.ToX,DeliverJob.ToY+1));
8: begin
     SetAction(TIdleUnitAction.Create());
     DeliverJob.Free;
   end;
end;
inc(DeliverJob.Phase);
end;



{ TKMwarrior }

function TKMwarrior.GetSupportedActions: TUnitActionTypeSet;
begin
  Result:= [ua_Move, ua_Idle, ua_Run, ua_Attack, ua_Die];
end;

procedure TKMwarrior.Paint();
var UnitType,Owner:integer; AnimAct:integer;
begin
Owner:=2;//should be inherited from =Player=
UnitType:=22;
if fCurrentAction=nil then
  AnimAct:=0
else
  AnimAct:=integer(fCurrentAction.fActionType); //should correspond with UnitAction
fRender.RenderUnit(UnitType, AnimAct+1, Direction, AnimStep, Owner, fPosition.X+0.5, fPosition.Y+1);
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
    SetAction(nil);
end;

{ TKMUnit }

constructor TKMUnit.Create(const aOwner: string; PosX, PosY:integer);
begin
  Inherited Create;
  fPosition.X:= PosX;
  fPosition.Y:= PosY;
  fOwner:= aOwner;
  Direction:=1;
  SetAction(TIdleUnitAction.Create());
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

{ TUnitAction }

constructor TUnitAction.Create(aActionType: TUnitActionType);
begin
  Inherited Create;
  fActionType:= aActionType;
end;

{ TMoveUnitAction }

constructor TMoveUnitAction.Create(PosX, PosY:integer);
begin
  Inherited Create(ua_Move);
  fDestPos.X:= PosX;
  fDestPos.Y:= PosY;
end;

procedure TMoveUnitAction.Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean);
var
  DX, DY, Distance:single;
begin
  DoEnd:= False;
  Distance:= TimeDelta * 1;//KMUnit.Speed;
  DX:= fDestPos.X - KMUnit.fPosition.X;
  DY:= fDestPos.Y - KMUnit.fPosition.Y;

  inc(KMUnit.AnimStep);

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

//  if DX <> 0 then
    KMUnit.fPosition.X:= KMUnit.fPosition.X + sign(DX)*min(Distance,abs(DX));

//  if DY <> 0 then
    KMUnit.fPosition.Y:= KMUnit.fPosition.Y + sign(DY)*min(Distance,abs(DY));

  if (KMUnit.fPosition.X = fDestPos.X) and (KMUnit.fPosition.Y = fDestPos.Y) then
    DoEnd:= True;
end;

constructor TIdleUnitAction.Create();
begin
  Inherited Create(ua_Idle);
end;

procedure TIdleUnitAction.Execute(KMUnit: TKMUnit; TimeDelta: single; out DoEnd: Boolean);
begin
  DoEnd:= False;
//  inc(KMUnit.AnimStep);
end;

{ TKMUnitsCollection }

procedure TKMUnitsCollection.Add(aOwner: string; aUnitType: TUnitType; PosX, PosY:integer);
begin
  case aUnitType of
    ut_Worker:  Inherited Add(TKMWorker.Create(aOwner,PosX,PosY));
    ut_Peasant: Inherited Add(TKMPeasant.Create(aOwner,PosX,PosY));
    ut_Warrior: Inherited Add(TKMwarrior.Create(aOwner,PosX,PosY));
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
