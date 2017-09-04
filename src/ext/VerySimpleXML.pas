{ VerySimpleXML v1.1 - a lightweight, one-unit XML reader/writer
  by Dennis Spreen
  http://blog.spreendigital.de/2011/11/10/verysimplexml-a-lightweight-delphi-xml-reader-and-writer/

  (c) Copyrights 2012 Dennis D. Spreen <dennis@spreendigital.de>
  This unit is free and can be used for any needs. The introduction of
  any changes and the use of those changed library is permitted without
  limitations. Only requirement:
  This text must be present without changes in all modifications of library.

  * The contents of this file are used with permission, subject to
  * the Mozilla Public License Version 1.1 (the "License"); you may   *
  * not use this file except in compliance with the License. You may  *
  * obtain a copy of the License at                                   *
  * http:  www.mozilla.org/MPL/MPL-1.1.html                           *
  *                                                                   *
  * Software distributed under the License is distributed on an       *
  * "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or    *
  * implied. See the License for the specific language governing      *
  * rights and limitations under the License.                         *
}

{ Modified by Lewin Hodgman (lewinjh@gmail.com) for compatibility with Delphi 7
  and Lazarus, as well as changes to better suit our needs }

unit VerySimpleXML;

interface

uses
  Classes;

type
  //TXmlList owns items and frees them when they are deleted from the list
  TXmlList = class(TList)
  protected
    //This one function is enough to free all deleted/cleared/rewritten objects
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;

  TXmlNodeList = class;

  TXmlAttribute = class(TObject)
  public
    Name: String; // Attribute name
    Value: String; // Attribute value (always as String)
  end;

  TXmlAttributeList = class(TXmlList)
  public
    function Find(AttrName: String): TXmlAttribute;
    // Find an Attribute by Name (not case sensitive)
  end;

  TXmlNode = class(TObject)
  private
    FAttributes: TXmlAttributeList;
    function GetAttribute(const AttrName: String): String;
    procedure SetAttr(const AttrName: String; const Value: String);
  public
    Parent: TXmlNode; // NIL only for Root-Node
    NodeName: String; // Node name
    ChildNodes: TXmlNodeList; // Child nodes, never NIL
    Text: String; // Node text content
    Obj: TObject; // attached object
    constructor Create; virtual;
    destructor Destroy; override;
    // Find a childnode by its name
    function Find(Name: String): TXmlNode; overload;
    // Find a childnode by Name/Attribute
    function Find(Name, Attribute: String): TXmlNode; overload;
    // Find a childnode by Name/Attribute/Value
    function Find(Name, Attribute, Value: String): TXmlNode; overload;
    // Return a list of childodes with given Name
    function FindNodes(Name: String): TXmlNodeList; virtual;
    // Returns True if the Attribute exits
    function HasAttribute(const Name: String): Boolean; virtual;
    // Returns True if this child nodes exists
    function HasChild(const Name: String): Boolean; virtual;
    // Add a child node and return it
    function AddChild(const Name: String): TXmlNode; virtual;
    function InsertChild(const Name: String; Pos: Integer): TXmlNode; virtual;
    function SetText(Value: String): TXmlNode; virtual;
    function SetAttribute(const AttrName: String;
      const Value: String): TXmlNode; virtual;
    property Attribute[const AttrName: String]: String read GetAttribute
      write SetAttr; // Attributes of a Node, accessible by attribute name
  end;

  TXmlNodeList = class(TXmlList);

  TXmlOnNodeSetText = procedure (Sender: TObject; Node: TXmlNode; Text: String) of
    object;

  TXmlVerySimple = class(TObject)
  private
    Lines: TStringList;
    FOnNodeSetText: TXmlOnNodeSetText;
    FOnNodeSetName: TXmlOnNodeSetText;
    procedure Parse(Strings: TStringList);
    procedure Walk(Lines: TStringList; Prefix: String; Node: TXmlNode);
    function GetText: String;
  public
    Root: TXmlNode; // There is only one Root Node
    Header: TXmlNode; // XML declarations are stored in here as Attributes
    AllowSelfClosingTags: Boolean;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    // Load XML from a file
    procedure LoadFromFile(const FileName: String); virtual;
    // Encoding is specified in Header-Node
    procedure SaveToFile(const FileName: String); virtual;
    procedure DefaultOnNodeSetText(Sender: TObject; Node: TXmlNode; Text: String);
    procedure DefaultOnNodeSetName(Sender: TObject; Node: TXmlNode; Name: String);
    property Text: String read GetText;
    property OnNodeSetText: TXmlOnNodeSetText read FOnNodeSetText write FOnNodeSetText;
    property OnNodeSetName: TXmlOnNodeSetText read FOnNodeSetName write FOnNodeSetName;
  end;

  function XMLEscape(Value: String): String;
  function XMLUnescape(Value: String): String;


implementation

uses
  SysUtils, StrUtils;


function XMLEscape(Value: String): String;
var I: Integer;
const ESCAPED_CHARS: set of Byte = [34,38,39,60,62,160..255];
begin
  Result := '';
  for I:=1 to Length(Value) do
    if Ord(Value[I]) in ESCAPED_CHARS then
      Result := Result + '&#'+IntToStr(Ord(Value[I]))+';'
    else
      Result := Result + Value[I];
end;

function XMLUnescape(Value: String): String;
begin
  Result := StringReplace(Value, '&lt;', '<', [rfReplaceAll] );
  Result := StringReplace(Result, '&gt;', '>', [rfReplaceAll]);
  Result := StringReplace(Result, '&apos;', chr(39), [rfReplaceAll]);
  Result := StringReplace(Result, '&quot;', '"', [rfReplaceAll]);
  Result := StringReplace(Result, '&amp;', '&', [rfReplaceAll]);
end;


//We were notified that the item is deleted from the list
procedure TXmlList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if (Action = lnDeleted) then
    TObject(Ptr).Free;
end;


{ TXmlVerySimple }

procedure TXmlVerySimple.Clear;
begin
  Root.Free;
  Header.Free;
  Root := TXmlNode.Create;
  Header := TXmlNode.Create;
  Header.NodeName := '?xml'; // Default XML Header
  Header.Attribute['version'] := '1.0'; // Default XML Version
  Lines.Clear;
end;

constructor TXmlVerySimple.Create;
begin
  inherited;
  Lines := TStringList.Create;
  FOnNodeSetText := DefaultOnNodeSetText;
  FOnNodeSetName := DefaultOnNodeSetName;
  Clear;
end;

procedure TXmlVerySimple.DefaultOnNodeSetName(Sender: TObject; Node: TXmlNode;
  Name: String);
begin
  Node.NodeName := Name;
end;

procedure TXmlVerySimple.DefaultOnNodeSetText(Sender: TObject; Node: TXmlNode;
  Text: String);
begin
  Node.Text := Text;
end;

destructor TXmlVerySimple.Destroy;
begin
  Root.Free;
  Header.Free;
  Lines.Free;
  inherited;
end;

procedure TXmlVerySimple.LoadFromFile(const FileName: String);
var Strings: TStringList;
begin
  Clear;
  Strings := TStringList.Create;
  Strings.LoadFromFile(FileName);
  Parse(Strings);
  Strings.Free;
end;

procedure TXmlVerySimple.Parse(Strings: TStringList);
var
  Line: String;
  IsTag, IsText: Boolean;
  Tag, Text: String;
  Parent, Node: TXmlNode;
  Attribute: TXmlAttribute;
  ALine, Attr, AttrText: String;
  I, P: Integer;
  IsSelfClosing: Boolean;
  IsQuote: Boolean;

  // Return a text ended by StopChar, respect quotation marks
  function GetText(var Line: String; StartStr: String; StopChar: Char): String;
  var
    Chr: Char;
  begin
    while (Length(Line) > 0) and ((Line[1] <> StopChar) or (IsQuote)) do
    begin
      Chr := Line[1];
      if Chr = '"' then
        IsQuote := Not IsQuote;
      StartStr := StartStr + Chr;
      delete(Line, 1, 1);
    end;
    Result := StartStr;
  end;

begin
  if assigned(Root) then // Release previous nodes (if set)
    Root.Free;

  IsTag := False;
  IsText := False;
  IsQuote := False;
  Node := NIL;

  for I:=0 to Strings.Count-1 do
  begin
    Line := Strings[I];

    while (Length(Line) > 0) do
    begin
      if (not IsTag) and (not IsText) then
      begin
        while (Length(Line) > 0) and (Line[1] <> '<') do
          delete(Line, 1, 1);

        if Length(Line) > 0 then
        begin
          IsTag := True;
          delete(Line, 1, 1); // Delete openining tag
          Tag := '';
        end;
      end;

      if IsTag then
      begin
        Tag := GetText(Line, Tag, '>');

        if (Length(Line) > 0) and (Line[1] = '>') then
        begin
          delete(Line, 1, 1);
          IsTag := False;

          if (Length(Tag) > 0) and (Tag[1] = '/') then
            Node := Node.Parent
          else
          begin
            Parent := Node;
            IsText := True;
            IsQuote := False;

            Node := TXmlNode.Create;
            if lowercase(copy(Tag, 1, 4)) = '?xml' then // check for xml header
            begin
              Tag := TrimRight(Tag);
              if Tag[Length(Tag)]='?' then
                delete(Tag, Length(Tag), 1);
              Header.Free;
              Header := Node;
            end;

            // Self-Closing Tag
            if (Length(Tag) > 0) and (Tag[Length(Tag)] = '/') then
            begin
              IsSelfClosing := True;
              delete(Tag, Length(Tag), 1);
            end
            else
              IsSelfClosing := False;

            P := pos(' ', Tag);
            if P <> 0 then // Tag name has attributes
            begin
              ALine := Tag;
              delete(Tag, P, Length(Tag));
              delete(ALine, 1, P);

              while Length(ALine) > 0 do
              begin
                Attr := GetText(ALine, '', '='); // Get Attribute Name
                AttrText := GetText(ALine, '', ' '); // Get Attribute Value

                if Length(AttrText) > 0 then
                begin
                  delete(AttrText, 1, 1); // Remove blank

                  if AttrText[1] = '"' then // Remove start/end quotation marks
                  begin
                    delete(AttrText, 1, 1);
                    if AttrText[Length(AttrText)] = '"' then
                      delete(AttrText, Length(AttrText), 1);
                  end;
                end;

                if Length(ALine) > 0 then
                  delete(ALine, 1, 1);

                // Header node (Attr='?') does not support Attributes
                if not((Node = Header) and (Attr = '?')) then
                begin
                  Attribute := TXmlAttribute.Create;
                  Attribute.Name := Attr;
                  Attribute.Value := AttrText;
                  Node.FAttributes.Add(Attribute);
                end;
                IsQuote := False;
              end;
            end;

            FOnNodeSetName(Self, Node, Tag);
            Node.Parent := Parent;
            if assigned(Parent) then
              Parent.ChildNodes.Add(Node)
            else if Node = Header then
            begin
              IsText := False;
              Node := NIL;
            end
            else
              Root := Node;

            Text := '';
            if IsSelfClosing then
              Node := Node.Parent;
          end;
        end;
      end;

      if IsText then
      begin
        Text := GetText(Line, Text, '<');
        if (Length(Line) > 0) and (Line[1] = '<') then
        begin
          IsText := False;
          while (Length(Text) > 0) and (Text[1] = ' ') do
            delete(Text, 1, 1);
          FOnNodeSetText(Self, Node, XMLUnescape(Text));
        end;
      end;

    end;
  end;
end;

procedure TXmlVerySimple.SaveToFile(const FileName: String);
begin
  GetText;
  Lines.SaveToFile(FileName);
end;

procedure TXmlVerySimple.Walk(Lines: TStringList; Prefix: String;
  Node: TXmlNode);
var
  Attribute: TXmlAttribute;
  OriginalPrefix: String;
  S: String;
  IsSelfClosing: Boolean;
  I: Integer;
begin
  S := Prefix + '<' + Node.NodeName;
  for I:=0 to Node.FAttributes.Count-1 do
  begin
    Attribute := Node.FAttributes[I];
    S := S + ' ' + Attribute.Name + '="' + Attribute.Value + '"';
  end;

  if Node = Header then
    S := S + ' ?';

  IsSelfClosing := AllowSelfClosingTags and (Length(Node.Text) = 0) and (Node.ChildNodes.Count = 0) and
    (Node <> Header);
  if IsSelfClosing then
    S := S + ' /';

  S := S + '>';
  if Length(Node.Text) > 0 then
    S := S + XMLEscape(Node.Text);

  if (Node.ChildNodes.Count = 0) and ((Length(Node.Text) > 0) or not AllowSelfClosingTags) and
    (Node <> Header) then
  begin
    S := S + '</' + Node.NodeName + '>';
    Lines.Add(S);
  end
  else
  begin
    Lines.Add(S);
    OriginalPrefix := Prefix;
    Prefix := Prefix + '  ';
    for I:=0 to Node.ChildNodes.Count-1 do
      Walk(Lines, Prefix, Node.ChildNodes[I]);
    if (Node <> Header) and (not IsSelfClosing) then
      Lines.Add(OriginalPrefix + '</' + Node.NodeName + '>');
  end;
end;

function TXmlVerySimple.GetText: String;
begin
  Lines.Clear;

  // Create XML introduction
  Walk(Lines, '', Header);

  // Create nodes representation
  Walk(Lines, '', Root);
  Result := Lines.Text;
end;

{ TXmlNode }

function TXmlNode.AddChild(const Name: String): TXmlNode;
begin
  Result := TXmlNode.Create;
  Result.NodeName := Name;
  Result.Parent := Self;
  ChildNodes.Add(Result);
end;

constructor TXmlNode.Create;
begin
  ChildNodes := TXmlNodeList.Create;
  Parent := NIL;
  FAttributes := TXmlAttributeList.Create;
end;

destructor TXmlNode.Destroy;
begin
  FAttributes.Free;
  ChildNodes.Free;
  inherited;
end;

function TXmlNode.Find(Name: String): TXmlNode;
var
  I: Integer;
begin
  Result := NIL;
  Name := lowercase(Name);
  for I:=0 to ChildNodes.Count-1 do
    if lowercase(TXmlNode(ChildNodes[I]).NodeName) = Name then
    begin
      Result := ChildNodes[I];
      Break;
    end;
end;

function TXmlNode.Find(Name, Attribute, Value: String): TXmlNode;
var
  Node: TXmlNode;
  I: Integer;
begin
  Result := NIL;
  Name := lowercase(Name);
  for I:=0 to ChildNodes.Count-1 do
  begin
    Node := ChildNodes[I];
    if (lowercase(Node.NodeName) = Name) and (Node.HasAttribute(Attribute)) and
      (Node.Attribute[Attribute] = Value) then
    begin
      Result := Node;
      Break;
    end;
  end;
end;

function TXmlNode.Find(Name, Attribute: String): TXmlNode;
var
  Node: TXmlNode;
  I: Integer;
begin
  Result := NIL;
  Name := lowercase(Name);
  for I:=0 to ChildNodes.Count-1 do
  begin
    Node := ChildNodes[I];
    if (lowercase(Node.NodeName) = Name) and (Node.HasAttribute(Attribute)) then
    begin
      Result := Node;
      Break;
    end;
  end;
end;

function TXmlNode.FindNodes(Name: String): TXmlNodeList;
var
  Node: TXmlNode;
  I: Integer;
begin
  Result := TXmlNodeList.Create;
  Name := lowercase(Name);
  for I:=0 to ChildNodes.Count-1 do
  begin
    Node := ChildNodes[I];
    if (lowercase(Node.NodeName) = Name) then
      Result.Add(Node);
  end;
end;

function TXmlNode.GetAttribute(const AttrName: String): String;
var
  Attribute: TXmlAttribute;
begin
  Attribute := FAttributes.Find(AttrName);
  if assigned(Attribute) then
    Result := Attribute.Value
  else
    Result := '';
end;

function TXmlNode.HasAttribute(const Name: String): Boolean;
begin
  Result := assigned(FAttributes.Find(Name));
end;

function TXmlNode.HasChild(const Name: String): Boolean;
begin
  Result := assigned(Find(Name));
end;

function TXmlNode.InsertChild(const Name: String; Pos: Integer): TXmlNode;
begin
  Result := TXmlNode.Create;
  Result.NodeName := Name;
  Result.Parent := Self;
  ChildNodes.Insert(Pos, Result);
end;

procedure TXmlNode.SetAttr(const AttrName, Value: String);
begin
  SetAttribute(AttrName, Value);
end;

function TXmlNode.SetAttribute(const AttrName, Value: String): TXmlNode;
var
  Attribute: TXmlAttribute;
begin
  Attribute := FAttributes.Find(AttrName); // Search for given name
  if not assigned(Attribute) then // If attribute is not found, create one
  begin
    Attribute := TXmlAttribute.Create;
    FAttributes.Add(Attribute);
  end;
  Attribute.Name := AttrName; // this allows "name-style" rewriting
  Attribute.Value := Value;
  Result := Self;
end;

function TXmlNode.SetText(Value: String): TXmlNode;
begin
  Text := Value;
  Result := Self;
end;

{ TXmlAttributeList }

function TXmlAttributeList.Find(AttrName: String): TXmlAttribute;
var
  Attribute: TXmlAttribute;
  I: Integer;
begin
  Result := NIL;
  AttrName := lowercase(AttrName);
  for I:=0 to Count-1 do
  begin
    Attribute := Self[I];
    if lowercase(Attribute.Name) = AttrName then
    begin
      Result := Attribute;
      Break;
    end;
  end;
end;

end.
