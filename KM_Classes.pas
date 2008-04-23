unit KM_Classes;

interface
uses
  classes;

type
  TKMList = class(TList)
  public
    procedure Clear; override;
  end;

implementation

{ TKMList }

procedure TKMList.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TObject(Items[I]).Free;
  inherited;
end;

end.
