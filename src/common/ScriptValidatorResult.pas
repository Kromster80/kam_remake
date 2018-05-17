unit ScriptValidatorResult;

interface
uses
  SysUtils, VerySimpleXML;


type
  TScriptValidatorIssue = record
    Line,
    Column:     Integer;
    Module,
    Param,
    Msg:        string;
  end;
  TScriptValidatorIssueArray = array of TScriptValidatorIssue;

  TScriptValidatorResult = class(TObject)
  strict private
    fHints,
    fWarnings,
    fErrors:   TScriptValidatorIssueArray;
    procedure Add(aLine, aColumn: Integer; aParam, aMessage: string;
                  var aDest: TScriptValidatorIssueArray); inline;
    procedure ArrayToXML(aSrc: TScriptValidatorIssueArray; var aDest: TXmlNode);
    procedure XMLToArray(aSrc: TXmlNode; var aDest: TScriptValidatorIssueArray);
    function FixText(aTest: string): string;
  public
    procedure AddHint(aLine, aColumn: Integer; aParam, aMessage: string);
    procedure AddWarning(aLine, aColumn: Integer; aParam, aMessage: string);
    procedure AddError(aLine, aColumn: Integer; aParam, aMessage: string);
    function ToXML: string;
    procedure FromXML(aXml: string);
    property Hints:    TScriptValidatorIssueArray read fHints    write fHints;
    property Warnings: TScriptValidatorIssueArray read fWarnings write fWarnings;
    property Errors:   TScriptValidatorIssueArray read fErrors   write fErrors;
  end;

implementation
uses
  Classes;

{ TSVResult }
procedure TScriptValidatorResult.Add(aLine, aColumn: Integer; aParam, aMessage: string;
                                     var aDest: TScriptValidatorIssueArray);
var
  I: Integer;
  Issue: TScriptValidatorIssue;
begin
  I := Length(aDest);
  SetLength(aDest, I + 1);
  Issue.Line   := aLine;
  Issue.Column := aColumn;
  Issue.Param  := aParam;
  Issue.Msg    := aMessage;
  aDest[I]     := Issue;
end;


procedure TScriptValidatorResult.AddHint(aLine, aColumn: Integer; aParam, aMessage: string);
begin
  Add(aLine, aColumn, aParam, aMessage, fHints);
end;


procedure TScriptValidatorResult.AddWarning(aLine, aColumn: Integer; aParam, aMessage: string);
begin
  Add(aLine, aColumn, aParam, aMessage, fWarnings);
end;


procedure TScriptValidatorResult.AddError(aLine, aColumn: Integer; aParam, aMessage: string);
begin
  Add(aLine, aColumn, aParam, aMessage, fErrors);
end;


procedure TScriptValidatorResult.ArrayToXML(aSrc: TScriptValidatorIssueArray;
                                            var aDest: TXmlNode);
var
  Node:  TXmlNode;
  Issue: TScriptValidatorIssue;
begin
  for Issue in aSrc do
  begin
    Node := aDest.AddChild('Issue');
    Node.SetAttribute('Line', IntToStr(Issue.Line));
    Node.SetAttribute('Column', IntToStr(Issue.Column));
    Node.SetAttribute('Module', Issue.Module);
    Node.SetAttribute('Param', Issue.Param);
    Node.SetAttribute('Msg', Issue.Msg);
  end;
end;


procedure TScriptValidatorResult.XMLToArray(aSrc: TXmlNode;
                                            var aDest: TScriptValidatorIssueArray);
var
  Node:  TXmlNode;
  Issue: TScriptValidatorIssue;
  Len:   Integer;
begin
  for Node in aSrc.ChildNodes do
  begin
    Len := Length(aDest);
    SetLength(aDest, Len + 1);
    Issue.Line   := StrToInt(Node.Attribute['Line']);
    Issue.Column := StrToInt(Node.Attribute['Column']);
    Issue.Module := FixText(Node.Attribute['Module']);
    Issue.Param  := FixText(Node.Attribute['Param']);
    Issue.Msg    := FixText(Node.Attribute['Msg']);
    aDest[Len]   := Issue;
  end;
end;


function TScriptValidatorResult.FixText(aTest: string): string;
begin
  Result := StringReplace(aTest, '&#39;', '"', [rfReplaceAll, rfIgnoreCase]);
end;


function TScriptValidatorResult.ToXML: string;
var
  HintNode,
  WarningNode,
  ErrorNode:   TXmlNode;
begin
  with TXmlVerySimple.Create do
  begin
    try
      Root.NodeName := 'ScriptValidatorResult';
      HintNode      := Root.AddChild('Hints');
      WarningNode   := Root.AddChild('Warnings');
      ErrorNode     := Root.AddChild('Errors');

      ArrayToXML(fHints, HintNode);
      ArrayToXML(fWarnings, WarningNode);
      ArrayToXML(fErrors, ErrorNode);

      Result := Text;
    finally
      Free;
    end;
  end;
end;


procedure TScriptValidatorResult.FromXML(aXml: string);
begin
  with TXmlVerySimple.Create do
    try
      Text := aXml;
      XMLToArray(Root.Find('Hints'), fHints);
      XMLToArray(Root.Find('Warnings'), fWarnings);
      XMLToArray(Root.Find('Errors'), fErrors);
    finally
      Free;
    end;
end;

end.
