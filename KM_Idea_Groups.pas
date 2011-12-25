unit KM_Idea_Groups;
{$I KaM_Remake.inc}
interface

uses Classes,
     KM_CommonTypes, KM_Terrain, KM_Units_Warrior;

type    

  TKMGroup = class
  private

    fCommander: TKMUnitWarrior; // Index of squad commander in fMembers
    fMembers: TList; // Warriors. List of TKMUnitWarrior.
    fUnitsPerRow: Integer;

    function GetCount: Integer;
    function GetMember(Index: Integer): TKMUnitWarrior;
    procedure SetNewCommander;
    procedure SetUnitsPerRow(Qty: Integer);

  protected
  public

    constructor Create;
    destructor Destroy; override;

    function AddMember(Warrior: TKMUnitWarrior):Integer; 
    function GetPosition(PosDir: TKMPointDir): Boolean;
    //function GetPositionInGroup(Member:Integer; AllowOffMap:boolean=false): TKMPoint;

    property Commander: TKMUnitWarrior read fCommander;
    property Count: integer read GetCount;
    property Members[Index: Integer]: TKMUnitWarrior read GetMember;
    property UnitsPerRow: Integer read fUnitsPerRow write SetUnitsPerRow;

  end;    

implementation

uses Math, SysUtils, KM_Units;

{ TKMGroup }

constructor TKMGroup.Create;
begin
  inherited Create;
  fMembers := TList.Create;
  SetNewCommander; // Reset commander
  SetUnitsPerRow(1);
end;


destructor TKMGroup.Destroy;
begin
  inherited;
end;


function TKMGroup.GetCount: Integer;
begin
  Result := fMembers.Count;
end;


function TKMGroup.GetMember(Index: Integer): TKMUnitWarrior;
begin
  // Assert is not needed
  // If Index is wrong, then TList.Get raise exception
  Result := TKMUnitWarrior(fMembers.Items[Index]);
end;


procedure TKMGroup.SetNewCommander;
begin
  fCommander := nil;
  if fMembers.Count = 0 then exit;
  // Calculation of new Commander
end;


procedure TKMGroup.SetUnitsPerRow(Qty: Integer);
begin
  fUnitsPerRow := EnsureRange(Qty, 1, fMembers.Count+1);
end;


function TKMGroup.AddMember(Warrior: TKMUnitWarrior):Integer;
begin
  Result := fMembers.Add(Warrior); // No check on Warrior in group
  if (fCommander = nil) then SetNewCommander;
  //todo: Set Self to Warrior;
end;


function TKMGroup.GetPosition(PosDir: TKMPointDir): Boolean;
begin
  Result := False;
  if fCommander = nil then exit;
  PosDir.Loc := fCommander.GetPosition;
  PosDir.Dir := Word(fCommander.Direction);
  Result := true;
end;

end.