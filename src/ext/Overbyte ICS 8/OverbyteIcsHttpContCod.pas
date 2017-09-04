{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Maurizio Lotauro <Lotauro.Maurizio@dnet.it>
              Code donated to the ICS project
Creation:     July 2005
Version:      8.01
Description:  This unit contains the class used by THttpCli to handle the
              Accept-Encoding and Content-Encoding header fields.
              It also contains the THttpContentCoding class needed to implement
              a class able to handle a specific encoding.

How to create a class to handle a specific encoding
- define in a separate unit a class inherited from THttpContentCoding
- if the class name start with THttpContentCoding followed by the coding
  identifier (for example THttpContentCodingGzip) then that part is taken
  automatically as coding identifier. Otherwise you must override the
  GetCoding method. This must return the name of the coding.
- most probably you must override the Create constructor to initialize
  the structure to handle the decoding process.
  Remember to call the inherited.
  The same for Destroy.
- override the WriteBuffer method. This will called right before the
  THttpCli.TriggerDocData. The parameters are the same.
  There you could do two different think. If it is possible to decode on the
  fly then call OutputWriteBuffer passing the decoded data. Otherwise store
  the data somewhere to decode the whole at the end.
- if there is not possible to decode on the fly then you must override the
  Complete method. This will called right before the THttpCli.TriggerDocEnd.
  Like for WriteBuffer there you must call OutputWriteBuffer.
- add an inizialization section where you call
  THttpContCodHandler.RegisterContentCoding
  The first parameter is the default quality (greater than 0 and less or equal
  than 1) and the second is the class.
To use it the unit must be added to the uses clause of one unit of the project

History
Jan 08, 2006 V1.01 Maurizio fixed GetCoding
                   Francois Piette reformated the code to fit the ICS rules,
                   added compilation directives.
Mar 26, 2006 V6.00 New version 6 started
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
Oct 8, 2012, V8.01 may be more than one encoding type specified, parse them


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsHttpContCod;

interface

{$B-}                  { Enable partial boolean evaluation   }
{$T-}                  { Untyped pointers                    }
{$X+}                  { Enable extended syntax              }
{$I Include\OverbyteIcsDefs.inc}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
{$IFDEF DELPHI6_UP}
    {$WARN SYMBOL_PLATFORM   OFF}
    {$WARN SYMBOL_LIBRARY    OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{$IFDEF COMPILER2_UP}  { Not for Delphi 1                    }
    {$H+}              { Use long strings                    }
    {$J+}              { Allow typed constant to be modified }
{$ENDIF}
{$IFDEF BCB3_UP}
    {$ObjExportAll On}
{$ENDIF}

uses
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF};

type
    EHttpContentCodingException = class(Exception);

    PStream = ^TStream;

    TWriteBufferProcedure = procedure(Buffer: Pointer; Count: Integer) of object;

    THttpContentCodingClass = class of THttpContentCoding;

    THttpContentCoding = class(TObject)
    private
        FOutputWriteBuffer: TWriteBufferProcedure;
    protected
        class function GetActive: Boolean; virtual;
        function PropGetActive: Boolean;
        class function GetCoding: String; virtual;
        function PropGetCoding: String;
        property OutputWriteBuffer: TWriteBufferProcedure read FOutputWriteBuffer;
    public
        constructor Create(WriteBufferProc: TWriteBufferProcedure); virtual;
        procedure Complete; virtual;
        procedure WriteBuffer(Buffer: Pointer; Count: Integer); virtual; abstract;
        property Active: Boolean read PropGetActive;
        property Coding: String  read PropGetCoding;
    end;

    THttpCCodIdentity = class(THttpContentCoding)
    protected
        class function GetCoding: String; override;
    end;

    THttpCCodStar = class(THttpContentCoding)
    protected
        class function GetCoding: String; override;
    end;

    THttpContCodItem = class(TObject)
    private
        FCodingClass: THttpContentCodingClass;
        FEnabled : Boolean;
        FQuality : Single;
        function GetCoding: String;
        function GetEnabled: Boolean;
    public
        constructor Create(const ACodingClass : THttpContentCodingClass;
                           const AQuality     : Single);
        property Coding: String read GetCoding;
        property Enabled: Boolean read  GetEnabled
                                  write FEnabled;
        property Quality: Single  read  FQuality
                                  write FQuality;
    end;

    THttpContCodHandler = class(TObject)
    private
        FCodingList        : TList;
        FCodingParams      : TList;
        FOutputStream      : PStream;
        FInputWriteBuffer  : TWriteBufferProcedure;
        FOutputWriteBuffer : TWriteBufferProcedure;
        FHeaderText        : String;
        FRecalcHeader      : Boolean;
        FEnabled           : Boolean;
        FUseQuality        : Boolean;
        function  GetCodingItem(const Coding: String): THttpContCodItem;
        function  GetCodingEnabled(const Coding: String): Boolean;
        procedure SetCodingEnabled(const Coding: String; const Value: Boolean);
        function  GetCodingQuality(const Coding: String): Single;
        procedure SetCodingQuality(const Coding: String; const Value: Single);
        function  GetHeaderText: String;
        function  GetOutputStream: TStream;
        procedure SetUseQuality(const Value: Boolean);
        procedure ClearCodingList;
        procedure InputWriteBuffer(Buffer: Pointer; Count: Integer);
    public
        constructor Create(OutStream       : PStream;
                           WriteBufferProc : TWriteBufferProcedure);
        destructor  Destroy; override;
        class procedure RegisterContentCoding(
                            const DefaultQuality : Single;
                            ContCodClass         : THttpContentCodingClass);
        class procedure UnregisterAuthenticateClass(
                            ContCodClass: THttpContentCodingClass);
        procedure Complete;
        function  Prepare(const Encodings: String): Boolean;
        property CodingEnabled[const Coding: String] : Boolean
                                                read  GetCodingEnabled
                                                write SetCodingEnabled;
        property CodingQuality[const Coding: String] : Single
                                                read  GetCodingQuality
                                                write SetCodingQuality;
        property OutputStream  : TStream        read  GetOutputStream;
        property HeaderText    : String         read  GetHeaderText;
        property Enabled       : Boolean        read  FEnabled
                                                write FEnabled;
        property UseQuality    : Boolean        read  FUseQuality
                                                write SetUseQuality;
        property WriteBuffer   : TWriteBufferProcedure
                                                read FInputWriteBuffer;
      end;

implementation

resourcestring
    ERR_GETCODING_OVERRIDE = 'GetCoding must be overridden in %s';

type
    PContCoding = ^TContCoding;
    TContCoding = record
        ContClass : THttpContentCodingClass;
        Quality   : Single;
    end;

    TContCodingsList = class(TList)
    public
        constructor Create;
        destructor  Destroy; override;
        procedure Add(const Quality: Single; AClass: THttpContentCodingClass);
        function  FindCoding(Coding: String): THttpContentCodingClass;
        procedure Remove(AClass: THttpContentCodingClass);
    end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TContCodingsList.Create;
begin
    inherited Create;
    Add(0, THttpCCodIdentity);
    Add(0, THttpCCodStar);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TContCodingsList.Destroy;
var
    I: Integer;
begin
    for I := 0 to Count - 1 do
        Dispose(PContCoding(Items[I]));
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TContCodingsList.Add(
    const Quality : Single;
    AClass        : THttpContentCodingClass);
var
    NewRec: PContCoding;
begin
    New(NewRec);
    NewRec.Quality   := Quality;
    NewRec.ContClass := AClass;
    inherited Add(NewRec);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TContCodingsList.FindCoding(Coding: String): THttpContentCodingClass;
var
    I, J: Integer;
    EncList : TStringList;
begin
    EncList := TStringList.Create;   // V8.01 may be more than one encoding type specified
    try
        for I := Count - 1 downto 0 do begin
            EncList.CommaText := PContCoding(Items[I])^.ContClass.GetCoding;  // ie 'gzip, deflate'
            for J := 0 to EncList.Count - 1 do begin   // V8.01 test each type in turn
                if SameText(EncList [J], Coding) then begin
                    Result := PContCoding(Items[I])^.ContClass;
                    Exit;
                end;
            end;
        end;
        Result := nil;
    finally
        EncList.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TContCodingsList.Remove(AClass: THttpContentCodingClass);
var
    I : Integer;
    P : PContCoding;
begin
    for I := Count - 1 downto 0 do begin
        P := PContCoding(Items[I]);
        if P^.ContClass.InheritsFrom(AClass) then begin
            Dispose(P);
            Delete(I);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
var
  ContCodings: TContCodingsList = nil;

function GetContentCodings: TContCodingsList;
begin
    if ContCodings = nil then
        ContCodings := TContCodingsList.Create;
    Result := ContCodings;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor THttpContentCoding.Create(WriteBufferProc: TWriteBufferProcedure);
begin
    inherited Create;
    FOutputWriteBuffer := WriteBufferProc;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
class function THttpContentCoding.GetActive: Boolean;
begin
    Result := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
class function THttpContentCoding.GetCoding: String;
const
    BASE_CLASS_NAME = 'THttpContentCoding';
begin
    if Pos(BASE_CLASS_NAME, ClassName) = 1 then
        Result := Copy(ClassName, Length(BASE_CLASS_NAME) + 1, MAXINT)
    else
        raise EHttpContentCodingException.CreateFmt(ERR_GETCODING_OVERRIDE,
                                                    [ClassName]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THttpContentCoding.PropGetActive: Boolean;
begin
    Result := GetActive;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THttpContentCoding.PropGetCoding: String;
begin
    Result := GetCoding;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpContentCoding.Complete;
begin
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
class function THttpCCodIdentity.GetCoding: String;
begin
    Result := 'identity';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
class function THttpCCodStar.GetCoding: String;
begin
    Result := '*';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor THttpContCodItem.Create(
    const ACodingClass : THttpContentCodingClass;
    const AQuality     : Single);
begin
    inherited Create;
    FCodingClass := ACodingClass;
    FEnabled     := TRUE;
    FQuality     := AQuality;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THttpContCodItem.GetCoding: String;
begin
    Result := FCodingClass.GetCoding;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THttpContCodItem.GetEnabled: Boolean;
begin
    Result := FEnabled and FCodingClass.GetActive;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor THttpContCodHandler.Create(
    OutStream       : PStream;
    WriteBufferProc : TWriteBufferProcedure);
var
    I: Integer;
begin
    inherited Create;

    FOutputStream      := OutStream;
    FInputWriteBuffer  := InputWriteBuffer;
    FOutputWriteBuffer := WriteBufferProc;
    FHeaderText        := '';
    FRecalcHeader      := TRUE;
    FEnabled           := FALSE;
    FUseQuality        := FALSE;
    FCodingList        := TList.Create;
    FCodingParams      := TList.Create;
    GetContentCodings;
    for I := 0 to ContCodings.Count - 1 do begin
        FCodingParams.Add(THttpContCodItem.Create(
                              PContCoding(ContCodings.Items[I])^.ContClass,
                              PContCoding(ContCodings.Items[I])^.Quality));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor THttpContCodHandler.Destroy;
var
    I: Integer;
begin
    ClearCodingList;
    FCodingList.Free;
    if FCodingParams <> nil then begin
        for I := FCodingParams.Count - 1 downto 0 do
            THttpContCodItem(FCodingParams.Items[I]).Free;
        FCodingParams.Free;
    end;
  inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THttpContCodHandler.GetCodingItem(
    const Coding: String): THttpContCodItem;
var
    I: Integer;
begin
    for I := FCodingParams.Count - 1 downto 0 do
        if SameText(THttpContCodItem(FCodingParams.Items[I]).Coding,
                                     Coding) then begin
        Result := THttpContCodItem(FCodingParams.Items[I]);
        Exit;
    end;
    Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THttpContCodHandler.GetCodingEnabled(
    const Coding: String): Boolean;
var
    AItem: THttpContCodItem;
begin
    AItem  := GetCodingItem(Coding);
    Result := (AItem <> nil) and AItem.Enabled;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpContCodHandler.SetCodingEnabled(
    const Coding : String;
    const Value  : Boolean);
var
    AItem: THttpContCodItem;
begin
    AItem := GetCodingItem(Coding);
    if (AItem <> nil) and (AItem.Enabled <> Value) then begin
        AItem.Enabled := Value;
        FRecalcHeader := TRUE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THttpContCodHandler.GetCodingQuality(
    const Coding: String): Single;
var
    AItem: THttpContCodItem;
begin
    AItem := GetCodingItem(Coding);
    if AItem <> nil then
        Result := AItem.Quality
    else
        Result := -1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpContCodHandler.SetCodingQuality(
    const Coding : String;
    const Value  : Single);
var
    AItem: THttpContCodItem;
begin
    AItem := GetCodingItem(Coding);
    if (AItem <> nil) and (AItem.Quality <> Value) then begin
        AItem.Quality := Value;
        FRecalcHeader := TRUE;
     end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THttpContCodHandler.GetHeaderText: String;
var
    ContItem   : THttpContCodItem;
    AddQuality : Boolean;
    DecPos     : Integer;
    QualStr    : String;
    I          : Integer;
begin
    if not FEnabled then begin
        Result := '';
        Exit;
    end;

    if FRecalcHeader then begin
        if UseQuality then begin
            { Quality will be added only if there are more than one coding enabled }
            AddQuality := FCodingParams.Count > 0;
            for I := FCodingParams.Count - 1 downto 0 do begin
                if THttpContCodItem(FCodingParams.Items[I]).Enabled then begin
                    if AddQuality then
                        AddQuality := FALSE
                    else begin
                        AddQuality := TRUE;
                        Break;
                    end;
                end;
            end;
        end
        else
            AddQuality := FALSE;

        FHeaderText := '';
        for I := 0 to FCodingParams.Count - 1 do begin
            ContItem := THttpContCodItem(FCodingParams.Items[I]);
            if ContItem.Enabled then begin
                if AddQuality then begin
                    QualStr := FormatFloat('0.###', ContItem.Quality);
                    { Force the point as decimal separator }
                    if {$IFDEF COMPILER16_UP}FormatSettings.{$ENDIF}DecimalSeparator <> '.' then begin
                        DecPos := Pos({$IFDEF COMPILER16_UP}FormatSettings.{$ENDIF}DecimalSeparator, QualStr);
                        if DecPos > 0 then
                            QualStr[DecPos] := '.';
                    end;

                    if FHeaderText = '' then
                        FHeaderText := Format('%s;q=%s', [ContItem.Coding, QualStr])
                    else
                        FHeaderText := Format('%s, %s;q=%s',
                                              [FHeaderText, ContItem.Coding,
                                               QualStr]);
                end
                else if ContItem.Quality > 0 then begin
                    if FHeaderText = '' then
                        FHeaderText := ContItem.Coding
                    else
                        FHeaderText := Format('%s, %s',
                                              [FHeaderText, ContItem.Coding]);
                end;
            end;
        end;
        FRecalcHeader := FALSE;
    end;
    Result := FHeaderText;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THttpContCodHandler.GetOutputStream: TStream;
begin
    Result := FOutputStream^;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpContCodHandler.SetUseQuality(const Value: Boolean);
begin
    if FUseQuality = Value then
        Exit;
    FUseQuality   := Value;
    FRecalcHeader := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
class procedure THttpContCodHandler.RegisterContentCoding(
    const DefaultQuality : Single;
    ContCodClass         : THttpContentCodingClass);
begin
    GetContentCodings.Add(DefaultQuality, ContCodClass);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
class procedure THttpContCodHandler.UnregisterAuthenticateClass(
    ContCodClass: THttpContentCodingClass);
begin
    if ContCodings <> nil then
        ContCodings.Remove(ContCodClass);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpContCodHandler.ClearCodingList;
var
    I: Integer;
begin
    if FCodingList <> nil then begin
        for I := FCodingList.Count - 1 downto 0 do
            THttpContentCoding(FCodingList.Items[I]).Free;
        FCodingList.Clear;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpContCodHandler.InputWriteBuffer(Buffer: Pointer; Count: Integer);
begin
    if Assigned(OutputStream) then
        OutputStream.WriteBuffer(Buffer^, Count);
    FOutputWriteBuffer(Buffer, Count);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpContCodHandler.Complete;
var
    I: Integer;
begin
    for I := FCodingList.Count - 1 downto 0 do
        THttpContentCoding(FCodingList[I]).Complete;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THttpContCodHandler.Prepare(const Encodings: String): Boolean;
var
    EncList : TStringList;
    AClass  : THttpContentCodingClass;
    ContCod : THttpContentCoding;
    I       : Integer;
begin
    Result := TRUE;
    ClearCodingList;
    FInputWriteBuffer := InputWriteBuffer;
    if not FEnabled or (Encodings = '') then
        Exit;
    EncList := TStringList.Create;
    try
        EncList.CommaText := Encodings;         // unsure why a server would report more than one encoding!!!
        for I := 0 to EncList.Count - 1 do begin
            AClass := GetContentCodings.FindCoding(EncList[I]);
            if AClass = nil then begin
                Result := FALSE;
                Break;
            end;
            ContCod := AClass.Create(FInputWriteBuffer);
            FCodingList.Add(ContCod);
            FInputWriteBuffer := ContCod.WriteBuffer;
        end;
    finally
        EncList.Free;
    end;

    if not Result then begin
        ClearCodingList;
        FInputWriteBuffer := InputWriteBuffer;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
initialization


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
finalization
  ContCodings.Free;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.
