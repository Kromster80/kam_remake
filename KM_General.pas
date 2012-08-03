unit KM_General;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils,
  KM_Defaults, KM_CommonClasses, KM_Points, KM_PlayerAISetup;


type
  TKMGeneral = class
  private
    fOwner: TPlayerIndex;
    fSetup: TKMPlayerAISetup;

  public
    constructor Create(aPlayer: TPlayerIndex; aSetup: TKMPlayerAISetup);
    destructor Destroy; override;

    procedure AfterMissionInit;
    procedure OwnerUpdate(aPlayer: TPlayerIndex);

    procedure UpdateState;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;


implementation
uses KM_Game, KM_Houses, KM_PlayersCollection, KM_Player, KM_Terrain, KM_Resource, KM_Utils;



{ TKMGeneral }

procedure TKMGeneral.AfterMissionInit;
begin

end;

constructor TKMGeneral.Create(aPlayer: TPlayerIndex; aSetup: TKMPlayerAISetup);
begin

end;

destructor TKMGeneral.Destroy;
begin

  inherited;
end;

procedure TKMGeneral.Load(LoadStream: TKMemoryStream);
begin

end;

procedure TKMGeneral.OwnerUpdate(aPlayer: TPlayerIndex);
begin

end;

procedure TKMGeneral.Save(SaveStream: TKMemoryStream);
begin

end;

procedure TKMGeneral.UpdateState;
begin

end;

end.

