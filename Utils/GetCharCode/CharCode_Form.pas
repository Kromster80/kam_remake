unit CharCode_Form;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TCharCode = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Edit3: TEdit;
    procedure Edit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CharCode: TCharCode;

implementation

{$R *.dfm}

procedure TCharCode.Edit1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Edit1.Clear;
end;

procedure TCharCode.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  Edit1.Clear;
end;

procedure TCharCode.Edit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Edit1.Clear;
  // Here we fix the issue where Char() doesn't always give us the character/button
  case Key of
    VK_LBUTTON: Edit1.Text := 'Left mouse button';
    VK_RBUTTON: Edit1.Text := 'Right mouse button';
    VK_CANCEL: Edit1.Text := 'Control-break';
    VK_MBUTTON: Edit1.Text := 'Middle mouse button';
    VK_BACK: Edit1.Text := 'Backspace';
    VK_TAB: Edit1.Text := 'Tab';
    VK_CLEAR: Edit1.Text := 'Clear';
    VK_RETURN: Edit1.Text := 'Enter';
    VK_SHIFT: Edit1.Text := 'Shift';
    VK_CONTROL: Edit1.Text := 'CTRL';
    VK_MENU: Edit1.Text := 'Alt';
    VK_PAUSE: Edit1.Text := 'Pause';
    VK_CAPITAL: Edit1.Text := 'Caps Lock';
    VK_ESCAPE: Edit1.Text := 'Escape';
    VK_SPACE: Edit1.Text := 'Space bar';
    VK_PRIOR: Edit1.Text := 'Page Up';
    VK_NEXT: Edit1.Text := 'Page Down';
    VK_END: Edit1.Text := 'End';
    VK_HOME: Edit1.Text := 'Home';
    VK_LEFT: Edit1.Text := 'Left Arrow';
    VK_UP: Edit1.Text := 'Up Arrow';
    VK_RIGHT: Edit1.Text := 'Right Arrow';
    VK_DOWN: Edit1.Text := 'Down Arrow';
    VK_SELECT: Edit1.Text := 'Select';
    VK_PRINT: Edit1.Text := 'Print';
    VK_EXECUTE: Edit1.Text := 'Execute';
    VK_SNAPSHOT: Edit1.Text := 'Print Screen';
    VK_INSERT: Edit1.Text := 'Insert';
    VK_DELETE: Edit1.Text := 'Delete';
    VK_HELP: Edit1.Text := 'Help';
    VK_NUMPAD0: Edit1.Text := 'Num 0';
    VK_NUMPAD1: Edit1.Text := 'Num 1';
    VK_NUMPAD2: Edit1.Text := 'Num 2';
    VK_NUMPAD3: Edit1.Text := 'Num 3';
    VK_NUMPAD4: Edit1.Text := 'Num 4';
    VK_NUMPAD5: Edit1.Text := 'Num 5';
    VK_NUMPAD6: Edit1.Text := 'Num 6';
    VK_NUMPAD7: Edit1.Text := 'Num 7';
    VK_NUMPAD8: Edit1.Text := 'Num 8';
    VK_NUMPAD9: Edit1.Text := 'Num 9';
    VK_SEPARATOR: Edit1.Text := 'Separator';
    VK_SUBTRACT: Edit1.Text := 'Num -';
    VK_DECIMAL: Edit1.Text := 'Num .';
    VK_DIVIDE: Edit1.Text := 'Num /';
    VK_F1: Edit1.Text := 'F1';
    VK_F2: Edit1.Text := 'F2';
    VK_F3: Edit1.Text := 'F3';
    VK_F4: Edit1.Text := 'F4';
    VK_F5: Edit1.Text := 'F5';
    VK_F6: Edit1.Text := 'F6';
    VK_F7: Edit1.Text := 'F7';
    VK_F8: Edit1.Text := 'F8';
    VK_F9: Edit1.Text := 'F9';
    VK_F10: Edit1.Text := 'F10';
    VK_F11: Edit1.Text := 'F11';
    VK_F12: Edit1.Text := 'F12';
    VK_F13: Edit1.Text := 'F13';
    VK_F14: Edit1.Text := 'F14';
    VK_F15: Edit1.Text := 'F15';
    VK_F16: Edit1.Text := 'F16';
    VK_F17: Edit1.Text := 'F17';
    VK_F18: Edit1.Text := 'F18';
    VK_F19: Edit1.Text := 'F19';
    VK_F20: Edit1.Text := 'F20';
    VK_F21: Edit1.Text := 'F21';
    VK_F22: Edit1.Text := 'F22';
    VK_F23: Edit1.Text := 'F23';
    VK_F24: Edit1.Text := 'F24';
    VK_NUMLOCK: Edit1.Text := 'Num Lock';
    VK_SCROLL: Edit1.Text := 'Scroll Lock';
    VK_LSHIFT: Edit1.Text := 'Left Shift';
    VK_RSHIFT: Edit1.Text := 'Right Shift';
    VK_LCONTROL: Edit1.Text := 'Left CTRL';
    VK_RCONTROL: Edit1.Text := 'Right CTRL';
    VK_LMENU: Edit1.Text := 'Left Alt';
    VK_RMENU: Edit1.Text := 'Right Alt';
    VK_PLAY: Edit1.Text := 'Play';
    VK_ZOOM: Edit1.Text := 'Zoom';
    106: Edit1.Text := 'Num *';
    107: Edit1.Text := 'Num +';
    186: Edit1.Text := ';';
    187: Edit1.Text := '=';
    188: Edit1.Text := ',';
    189: Edit1.Text := '-';
    190: Edit1.Text := '.';
    191: Edit1.Text := '/';
    192: Edit1.Text := '`';
    219: Edit1.Text := '[';
    220: Edit1.Text := '\';
    221: Edit1.Text := ']';
    222: Edit1.Text := '''';

  else
    Edit1.Text := Char(Key);
  end;
  Edit2.Text := Key.ToString();
  Edit3.Text := Key.ToHexString();
end;

end.
