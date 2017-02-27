unit KM_NetAuthUnsecure;
{$I KaM_Remake.inc}
interface
uses
  Classes, KM_CommonClasses;

type
  TKMNetSecurity = class
  public
    class procedure GenerateChallenge(M: TKMemoryStream; aSender: Integer);
    class function SolveChallenge(M: TKMemoryStream; aSender: Integer): TKMemoryStream;
    class function ValidateSolution(M: TKMemoryStream; aSender: Integer): Boolean;
  end;

implementation


{ TKMNetSecurity }
class procedure TKMNetSecurity.GenerateChallenge(M: TKMemoryStream; aSender: Integer);
begin
  //Leave M unchanged
end;


class function TKMNetSecurity.SolveChallenge(M: TKMemoryStream; aSender: Integer): TKMemoryStream;
begin
  Result := TKMemoryStream.Create;
end;


class function TKMNetSecurity.ValidateSolution(M: TKMemoryStream; aSender: Integer): Boolean;
begin
  Result := True;
end;

end.
