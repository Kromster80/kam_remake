unit KM_PlayerAISetup;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, SysUtils,
    KM_CommonClasses;


type
  TKMPlayerAISetup = class
    AutoBuild: Boolean;
    AutoRepair: Boolean;
    RecruitDelay: Cardinal; //Recruits (for barracks) can only be trained after this many ticks
    RecruitFactor: Byte;
    SerfFactor: Byte;
    WorkerFactor: Byte;

    Strong: Boolean;
    Wooden: Boolean;

    constructor Create;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;


implementation
uses KM_Utils;


{ TKMPlayerAISetup }
constructor TKMPlayerAISetup.Create;
begin
  inherited;

  AutoBuild := True; //In KaM it is On by default, and most missions turn it off
  AutoRepair := False; //In KaM it is Off by default

  SerfFactor := 10; //Means 1 serf per building
  WorkerFactor := 6;
  RecruitFactor := 5; //This means the number in the barracks, watchtowers are counted seperately
  RecruitDelay := 0; //Can train at start

  Strong := (KaMRandom > 0.5);
  Wooden := (KaMRandom > 0.5);
end;


procedure TKMPlayerAISetup.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(AutoBuild);
  SaveStream.Write(AutoRepair);
  SaveStream.Write(SerfFactor);
  SaveStream.Write(RecruitDelay);
  SaveStream.Write(RecruitFactor);
  SaveStream.Write(WorkerFactor);

  SaveStream.Write(Strong);
end;


procedure TKMPlayerAISetup.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(AutoBuild);
  LoadStream.Read(AutoRepair);
  LoadStream.Read(SerfFactor);
  LoadStream.Read(RecruitDelay);
  LoadStream.Read(RecruitFactor);
  LoadStream.Read(WorkerFactor);

  LoadStream.Read(Strong);
end;


end.
