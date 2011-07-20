unit KM_Idea_Groups;
{$I KaM_Remake.inc}
interface

uses Classes,
     KM_Units_Warrior;

type    

  TKMGroup = class
  private

    fCommander: integer; // Index of squad commander in fMembers
    fMembers: TList; // Warriors. List of TKMUnitWarrior.
    fUnitsPerRow: Integer;

    function GetCount: Integer;
    function GetMember(Index: Integer): TKMUnitWarrior;
    procedure SetNewCommander;
    procedure SetUnitsPerRow(Qty: Integer);

  protected
  public

    constructor Create;

    property Count: integer read GetCount;
    property Members[Index: Integer]: TKMUnitWarrior read GetMember;
    property UnitsPerRow: Integer read fUnitsPerRow write SetUnitsPerRow;

  end;    

implementation

uses Math;

{ TKMGroup }

constructor TKMGroup.Create;
begin
  fMembers := TList.Create;
  SetNewCommander; // Reset commander
  SetUnitsPerRow(1);
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
  fCommander := -1;
  if fMembers.Count = 0 then exit;
  // Calculation of new Commander
end;

procedure TKMGroup.SetUnitsPerRow(Qty: Integer);
begin
  fUnitsPerRow := EnsureRange(Qty, 1, fMembers.Count+1);
end;

end.