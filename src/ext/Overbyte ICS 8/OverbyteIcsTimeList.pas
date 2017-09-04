{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François Piette
Creation:     Aug 27, 2002
Description:
Version:      8.00
EMail:        francois.piette@overbyte.be         http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2002-2010 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. 
              <francois.piette@overbyte.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

History:
Dec 28, 2004 V1.01 F. Piette added DeleteItem method and replaced code where
                   needed Added RemoveItemIfAged method
Apr 04, 2009 V1.02 F. Piette cleanup code
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsTimeList;

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$H+}           { Use long strings                    }
{$J+}           { Allow typed constant to be modified }
{$I Include\OverbyteIcsDefs.inc}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
{$ENDIF}
{$IFDEF POSIX}
    Ics.Posix.WinTypes,    
{$ENDIF}
  {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF};

type
  TTimeRecFreeFct = procedure (var P : Pointer);

  TTimeRec = record
      Value    : String;
      TimeMark : TDateTime;
      Count    : Integer;
      Data     : Pointer;
      FreeFct  : TTimeRecFreeFct;
  end;
  PTimeRec = ^TTimeRec;

  TTimeListDeleteEvent = procedure (Sender: TObject; PItem : PTimeRec) of object;
  TTimeList = class(TComponent)
  private
    FData     : TList;
    FMaxItems : Integer;
    FVersion  : Integer;   // Change each time content change
    FMaxAge   : Integer;   // Seconds
    FOnChange : TNotifyEvent;
    FOnDelete : TTimeListDeleteEvent;
    function  GetCount: Integer;
    function  GetItems(Index: Integer): PTimeRec;
    procedure SetMaxItems(const Value: Integer);
    procedure SetMaxAge(const Value: Integer);
    procedure TriggerChange; virtual;
    procedure TriggerDelete(PItem : PTimeRec); virtual;
  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    function    Add(const Value : String) : PTimeRec; virtual;
    function    AddWithData(const Value : String;
                            Data        : Pointer;
                            FreeFct     : TTimeRecFreeFct) : PTimeRec; virtual;
    function    Delete(const Value: String) : Integer; virtual;
    procedure   DeleteItem(Index : Integer); virtual;
    function    IndexOf(const Value: String): Integer;
    procedure   RemoveAged; virtual;
    function    RemoveItemIfAged(Index: Integer): Boolean; virtual;
    procedure   Clear; virtual;
    property    Count : Integer read GetCount;
    property    Items[Index: Integer]: PTimeRec read GetItems; default;
    property    Version  : Integer read FVersion;
  published
    property MaxItems : Integer              read  FMaxItems write SetMaxItems;
    property MaxAge   : Integer              read  FMaxAge   write SetMaxAge;
    property OnChange : TNotifyEvent         read  FOnChange write FOnChange;
    property OnDelete : TTimeListDeleteEvent read  FOnDelete write FOnDelete;
  end;

implementation


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TTimeList.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
    FData     := TList.Create;
    FMaxItems := 100;
    FMaxAge   := 300; // Seconds
    FVersion  := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TTimeList.Destroy;
var
    I        : Integer;
begin
    if Assigned(FData) then begin
        // delete all items stored in the list
        for I := FData.Count - 1 downto 0 do
            DeleteItem(I);
        FreeAndNil(FData);
    end;
    inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTimeList.DeleteItem(Index : Integer);
var
    PItem    : PTimeRec;
begin
    PItem := FData.Items[Index];
    FData.Delete(Index);          // Remove item from list
    TriggerDelete(PItem);
    // Free data object if associated properly
    if (PItem^.Data <> nil) and Assigned(PItem^.FreeFct) then
        PItem^.FreeFct(PItem^.Data);
    Dispose(PItem);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTimeList.IndexOf(const Value: String) : Integer;
var
    PItem    : PTimeRec;
begin
    if not Assigned(FData) then begin
        Result := -1;
        Exit;
    end;
    // Search if item "Value" already exists
    Result := FData.Count - 1;  // Start with last item (most recent)
    while Result >= 0 do begin
        PItem := FData.Items[Result];
        if CompareText(PItem.Value, Value) = 0 then
            // We found existing item
            Exit;
        Dec(Result);
    end;
    // The item is not existing in the list
    Result := -1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTimeList.Add(const Value: String) : PTimeRec;
var
    I        : Integer;
    PItem    : PTimeRec;
begin
    Result := nil;
    if Value = '' then
        Exit;         // Ignore empty value

    if not Assigned(FData) then
        FData := TList.Create;

    PItem := nil; // Makes compiler happy
    // Search if item "Value" already exists
    I := FData.Count - 1;  // Start with last item (most recent)
    while I >= 0 do begin
        PItem := FData.Items[I];
        if CompareText(PItem.Value, Value) = 0 then begin
            // We found existing item
            // Remove it from where it is, to add it later at the end
            FData.Delete(I);
            break;
        end;
        Dec(I);
    end;
    if I < 0 then begin
        // The item is not existing in the list, create a new item
        New(PItem);
        PItem.Value   := Value;
        PItem.Count   := 0;
        PItem.Data    := nil;
        PItem.FreeFct := nil;
    end;

    // Remove older entry if too much items.
    if FData.Count >= FMaxItems then
        DeleteItem(0);

    // Add existing or new item at the end of the list
    FData.Add(PItem);
    // Set new TimeMark
    PItem.TimeMark := Now;
    // Increment the use count
    Inc(PItem.Count);
    // Increment version number, wrapping to 0 when largest integer is reached
    FVersion := (Cardinal(FVersion) + 1) and High(Integer);
    TriggerChange;
    Result := PItem;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTimeList.AddWithData(
    const Value : String;
    Data        : Pointer;
    FreeFct     : TTimeRecFreeFct) : PTimeRec;
begin
    Result := Add(Value);
    if Result <> nil then begin
        if (Result^.Data <> nil) and Assigned(Result^.FreeFct) then
            Result^.FreeFct(Result^.Data);
        Result^.Data    := Data;
        Result^.FreeFct := FreeFct;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTimeList.GetCount: Integer;
begin
    if not Assigned(FData) then
        Result := 0
    else
        Result := FData.Count;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTimeList.GetItems(Index: Integer): PTimeRec;
begin
    if not Assigned(FData) then
        Result := nil
    else if (Index < 0) or (Index >= FData.Count) then
        Result := nil
    else
        Result := FData.Items[Index];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTimeList.SetMaxItems(const Value: Integer);
begin
    if FMaxItems = Value then
        Exit;
    FMaxItems := Value;
    if not Assigned(FData) then
        Exit;  // No items at all
    if FData.Count > FMaxItems then begin
        // Adjust number of items in the list
        while FData.Count > FMaxItems do
            DeleteItem(0);
        // Inc version number, wrapping to 0 when largest integer is reached
        FVersion := (Cardinal(FVersion) + 1) and High(Integer);
        TriggerChange;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTimeList.TriggerChange;
begin
     if Assigned(FOnChange) then
         FOnChange(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTimeList.TriggerDelete(PItem : PTimeRec);
begin
    if Assigned(FOnDelete) then
        FOnDelete(Self, PItem);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Delete an item if it is aged
// Return TRUE if item removed
function TTimeList.RemoveItemIfAged(Index : Integer) : Boolean;
var
    TimeLimit : TDateTime;
    PItem     : PTimeRec;
begin
    Result := FALSE;
    if not Assigned(FData) then
        Exit;
    if FData.Count <= 0 then
        Exit;
    TimeLimit := Now - FMaxAge * EncodeTime(0, 0, 1, 0);
    PItem := FData.Items[Index];
    if PItem.TimeMark < TimeLimit then begin
        DeleteItem(Index);
        Result := TRUE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTimeList.RemoveAged;
var
    TimeLimit : TDateTime;
    I         : Integer;
    PItem     : PTimeRec;
    Changed   : Boolean;
begin
    if not Assigned(FData) then
        Exit;
    if FData.Count <= 0 then
        Exit;
    TimeLimit := Now - FMaxAge * EncodeTime(0, 0, 1, 0);
    Changed   := FALSE;
    I := FData.Count - 1;  // Start with last item (most recent)
    while I >= 0 do begin
        PItem := FData.Items[I];
        if PItem.TimeMark < TimeLimit then begin
            DeleteItem(I);
            Changed := TRUE;
        end;
        Dec(I);
    end;
    if Changed then begin
        // Inc version number, wrapping to 0 when largest integer is reached
        FVersion := (Cardinal(FVersion) + 1) and High(Integer);
        TriggerChange;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTimeList.SetMaxAge(const Value: Integer);
begin
    if FMaxAge = Value then
        Exit;
    FMaxAge := Value;
    RemoveAged;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTimeList.Clear;
var
    I         : Integer;
begin
    if not Assigned(FData) then
        Exit;
    if FData.Count <= 0 then
        Exit;
    I := FData.Count - 1;  // Start with last item (most recent)
    while I >= 0 do begin
        DeleteItem(I);
        Dec(I);
    end;
    // Inc version number, wrapping to 0 when largest integer is reached
    FVersion := (Cardinal(FVersion) + 1) and High(Integer);
    TriggerChange;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTimeList.Delete(const Value: String) : Integer;
begin
    if not Assigned(FData) then begin
        Result := -1;
        Exit;
    end;
    Result := IndexOf(Value);
    if Result < 0 then
        Exit;       // Not found
    DeleteItem(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
