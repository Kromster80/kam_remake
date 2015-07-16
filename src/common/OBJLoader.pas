//------------------------------------------------------------------------
//
// Author      : Jan Horn
// Email       : jhorn@global.co.za
// Website     : http://home.global.co.za/~jhorn
// Date        : 13 May 2001
// Version     : 1.0
// Description : Wavefront OPJ loader
//
//------------------------------------------------------------------------
unit OBJLoader;
{$I KaM_Remake.inc}
interface
uses
  dglOpenGL, StrUtils, SysUtils, Unit_Vector, Classes
  {$IFDEF MSWindows} , Windows {$ENDIF};


type
  TOBJFace = Record
   Count : Integer;            // Number of vertices in faces
   vIndex : Array of Integer;  // indexes to vertices
   tIndex : Array of Integer;  // indexes to vertex textures
   nIndex : Array of Integer;  // indexes to vertex normals
  end;

  TOBJGroup = Record
   Name : String;
   Faces : Integer;            // Number of faces
   Face  : Array of TOBJFace;  // The faces in the group
   mIndex : Integer;           // index to Material
  end;

  TOBJModel = class
  private
    Vertices  : Integer;
    Normals   : Integer;
    Groups    : Integer;
    Vertex    : Array of TVector3f;
    Normal    : Array of TVector3f;
    Group     : Array of TOBJGroup;
    procedure InitModel;
    procedure ReadFaceData(S : String);
    procedure ReadVertexData(S : String);
  public
    procedure LoadFromFile(filename : String);
    procedure LoadFromText(aText: string);
    procedure DrawModel;
  end;


implementation


{------------------------------------------------------------------}
{  Initialises a model                                             }
{------------------------------------------------------------------}
procedure TOBJModel.InitModel;
begin
  Vertices  := 0;
  Normals   := 0;
  Groups    := 0;
  SetLength(Vertex, 0);
  SetLength(Normal, 0);
  SetLength(Group, 0);
end;


{------------------------------------------------------------------}
{  Gets the X, Y, Z coordinates from a String                      }
{------------------------------------------------------------------}
function GeTVector3fs(S : String) : TVector3f;
var P, P2 : Integer;
    C : TVector3f;
begin
  S := Trim(Copy(S, 3, Length(S)));
  P := Pos(' ', S);
  P2 := PosEx(' ', S, P+1);
  S := StringReplace(S, '.', {$IFDEF WDC}FormatSettings.{$ENDIF}DecimalSeparator, [rfReplaceAll]);

  C.X :=-StrToFloat(Copy(S, 1, P-1));
  C.Y :=-StrToFloat(Copy(S, P+1, P2-P-1));
  C.Z :=-StrToFloat(Copy(S, P2+1, Length(S)));
  Result :=C;
end;


{-------------------------------------------------------------------}
{  Returns the U, V texture coordinates of a texture from a String  }
{-------------------------------------------------------------------}
function GetTexCoords(S : String) : TVector2f;
var P, P2 : Integer;
begin
  P := Pos(' ', S);
  P2 := PosEx(' ', S, P+1);
  S := StringReplace(S, '.', {$IFDEF WDC}FormatSettings.{$ENDIF}DecimalSeparator, [rfReplaceAll]);

  Result.U :=StrToFloat(Copy(S, P+1, P2-P-1));
  Result.V :=StrToFloat(Copy(S, P2+1, Length(S)));
end;


{------------------------------------------------------------------}
{  Reads Vertex coords, Normals and Texture coords from a String   }
{------------------------------------------------------------------}
procedure TOBJModel.ReadVertexData(S : String);
var C : TVector3f;
begin
  case S[2] of
    ' ' : begin                      // Read the vertex coords
            C := GeTVector3fs(S);
            Inc(Vertices);
            SetLength(Vertex, Vertices+1);
            Vertex[Vertices] := C;
          end;
    'N' : begin                      // Read the vertex normals
            C := GeTVector3fs(S);
            Inc(Normals);
            SetLength(Normal, Normals+1);
            Normal[Normals] := C;
          end;
  end;
end;


{------------------------------------------------------------------}
{  Reads the faces/triangles info for the model                    }
{  Data is stored as "f f f" OR "f/t f/t /ft" OR "f/t/n .. f/t/n"  }
{------------------------------------------------------------------}
procedure TOBJModel.ReadFaceData(S : String);
var P, P2, P3 : Integer; F: TOBJFace;
begin
  P := Pos(' ', S);
  S := Trim(Copy(S, P+1, length(S)));

  Inc(Group[Groups].Faces);
  SetLength(Group[Groups].Face, Group[Groups].Faces+1);

  F.Count :=0;
  While Length(S) > 0 do
  begin
    P :=Pos('/', S);      // check for position of first /
    P3 :=Pos(' ', S);
    if P3 = 0 then      // if we reach the end
      P3 :=Length(S)+1;

    if P > 0 then              // there are normals or texture coords
    begin
      Inc(F.Count);
      SetLength(F.vIndex, F.Count);
      F.vIndex[F.Count-1] :=StrToInt(Copy(S, 1, P-1));
      P2 :=PosEx('/', S, P+1);   // check for position of second /
      if P2 > P+1 then          // there are normals AND texture coords
      begin
        SetLength(F.tIndex, F.Count);
        SetLength(F.nIndex, F.Count);
        { Change Suggested By Megaes }
        F.tIndex[F.Count-1] :=StrToInt(Copy(S, P+1, P2-P-1)); 
        F.nIndex[F.Count-1] :=StrToInt(Copy(S, P2+1, P3-P2-1));
        //F.tIndex[F.Count-1] :=StrToInt(Copy(S, P+1, P2-1));
        //F.nIndex[F.Count-1] :=StrToInt(Copy(S, P2+1, P3-1));
      end
      else
      begin
        SetLength(F.nIndex, F.Count);
        F.nIndex[F.Count-1] :=StrToInt(Copy(S, P2+1, P3-1 - P2));
      end;
    end
    else
    begin
      Inc(F.Count);
      SetLength(F.vIndex, F.Count);
      F.vIndex[F.Count-1] :=StrToInt(Copy(S, 1, P3-1));
    end;
    S :=Copy(S, P3+1, length(S));
  end;

  Group[Groups].Face[Group[Groups].Faces] := F;
end;


//Loads a Alias Wavefront .OBJ file
procedure TOBJModel.LoadFromFile(filename : String);
var
  MS: TMemoryStream;
  s: string;
begin
  if not FileExists(filename) then Exit;

  MS := TMemoryStream.Create;
  MS.LoadFromFile(filename);
  SetString(s, PAnsiChar(MS.Memory), MS.Size);
  MS.Free;
  LoadFromText(s);
end;


procedure TOBJModel.LoadFromText(aText: string);
var
  F: TStringList;
  I: Integer;
  S, S2 : String;
begin
  InitModel; //Reset everything

  F := TStringList.Create;
  F.Text := aText;

  for I := 0 to F.Count - 1 do
  begin
    S := F.Strings[I];
    if (S <> '') AND (S[1] <> '#') then
    begin
      S := Uppercase(S);
      case S[1] of
        'G' : begin
                Inc(Groups);
                SetLength(Group, Groups+1);
                S2 := Trim(Copy(S, 2, length(S)));
                Group[Groups].Name := S2;
              end;
        'V' : ReadVertexData(S);  // Read Vertex Date (coord, normal, texture)
        'F' : ReadFaceData(S);    // Read faces
      end;
    end;
  end;

  F.Free;
end;


procedure TOBJModel.DrawModel;
var I, J, K : Integer;
begin
  glPushAttrib(GL_LIGHTING_BIT);
  for I := 1 to Groups do
  for J := 1 to Group[I].Faces do
  with Group[I].Face[J] do
  begin
    case Count of
      3:    glBegin(GL_TRIANGLES);
      4:    glBegin(GL_QUADS);
      else  glBegin(GL_POLYGON);
    end;

    for K := 0 to Count - 1 do
    begin
      if Normals > 0 then
        glNormal3fv(@Normal[nIndex[K]]);
      glVertex3fv(@Vertex[vIndex[K]]);
    end;
    glEnd;
  end;
  glPopAttrib;
end;


end.