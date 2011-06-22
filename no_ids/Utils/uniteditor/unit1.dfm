object Form1: TForm1
  Left = 279
  Top = 463
  Width = 644
  Height = 358
  ActiveControl = Button1
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnActivate = init
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 160
    Top = 72
    Width = 41
    Height = 13
    Caption = 'Hitpoints'
    Color = clBtnFace
    ParentColor = False
  end
  object Label2: TLabel
    Left = 160
    Top = 111
    Width = 28
    Height = 13
    Caption = 'Atack'
    Color = clBtnFace
    ParentColor = False
  end
  object Label3: TLabel
    Left = 160
    Top = 151
    Width = 89
    Height = 13
    Caption = 'AttackHorseBonus'
    Color = clBtnFace
    ParentColor = False
  end
  object Label4: TLabel
    Left = 160
    Top = 184
    Width = 11
    Height = 13
    Caption = 'x4'
    Color = clBtnFace
    ParentColor = False
  end
  object Defence: TLabel
    Left = 160
    Top = 224
    Width = 41
    Height = 13
    Caption = 'Defence'
    Color = clBtnFace
    ParentColor = False
  end
  object Speed: TLabel
    Left = 160
    Top = 264
    Width = 31
    Height = 13
    Caption = 'Speed'
    Color = clBtnFace
    ParentColor = False
  end
  object label5: TLabel
    Left = 423
    Top = 72
    Width = 11
    Height = 13
    Caption = 'x7'
    Color = clBtnFace
    ParentColor = False
  end
  object Label6: TLabel
    Left = 424
    Top = 111
    Width = 24
    Height = 13
    Caption = 'Sight'
    Color = clBtnFace
    ParentColor = False
  end
  object Label7: TLabel
    Left = 423
    Top = 151
    Width = 11
    Height = 13
    Caption = 'x9'
    Color = clBtnFace
    ParentColor = False
  end
  object Label8: TLabel
    Left = 423
    Top = 184
    Width = 17
    Height = 13
    Caption = 'x10'
    Color = clBtnFace
    ParentColor = False
  end
  object Label9: TLabel
    Left = 424
    Top = 224
    Width = 61
    Height = 13
    Caption = 'CanWalkOut'
    Color = clBtnFace
    ParentColor = False
  end
  object Label10: TLabel
    Left = 423
    Top = 264
    Width = 17
    Height = 13
    Caption = 'x11'
    Color = clBtnFace
    ParentColor = False
  end
  object Open: TButton
    Left = 24
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Open'
    TabOrder = 0
    OnClick = open_file
  end
  object Button1: TButton
    Left = 112
    Top = 16
    Width = 83
    Height = 23
    Caption = 'Save'
    TabOrder = 1
    OnClick = saveDAT
  end
  object ListBox1: TListBox
    Left = 24
    Top = 72
    Width = 124
    Height = 213
    ItemHeight = 13
    TabOrder = 2
    OnClick = showDAT
  end
  object HitPoints: TSpinEdit
    Left = 301
    Top = 72
    Width = 42
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 3
    Value = 0
    OnChange = ChangeSpinEdits
  end
  object Attack: TSpinEdit
    Left = 301
    Top = 111
    Width = 42
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 4
    Value = 0
    OnChange = ChangeSpinEdits
  end
  object AttackHorseBonus: TSpinEdit
    Left = 302
    Top = 151
    Width = 41
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 5
    Value = 0
    OnChange = ChangeSpinEdits
  end
  object x4: TSpinEdit
    Left = 302
    Top = 184
    Width = 42
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 6
    Value = 0
    OnChange = ChangeSpinEdits
  end
  object DefenceSpinEdit: TSpinEdit
    Left = 301
    Top = 224
    Width = 43
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 7
    Value = 0
    OnChange = ChangeSpinEdits
  end
  object SpeedSpinEdit: TSpinEdit
    Left = 301
    Top = 264
    Width = 42
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 8
    Value = 0
    OnChange = ChangeSpinEdits
  end
  object x7: TSpinEdit
    Left = 523
    Top = 72
    Width = 50
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 9
    Value = 0
    OnChange = ChangeSpinEdits
  end
  object Sight: TSpinEdit
    Left = 523
    Top = 111
    Width = 50
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 10
    Value = 0
    OnChange = ChangeSpinEdits
  end
  object x9: TSpinEdit
    Left = 523
    Top = 151
    Width = 50
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 11
    Value = 0
    OnChange = ChangeSpinEdits
  end
  object x10: TSpinEdit
    Left = 523
    Top = 184
    Width = 50
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 12
    Value = 0
    OnChange = ChangeSpinEdits
  end
  object CanWalkOut: TSpinEdit
    Left = 523
    Top = 224
    Width = 50
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 13
    Value = 0
    OnChange = ChangeSpinEdits
  end
  object x11: TSpinEdit
    Left = 523
    Top = 264
    Width = 50
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 14
    Value = 0
    OnChange = ChangeSpinEdits
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.dat'
    Left = 208
    Top = 16
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '../../Data/Defines/Unit.dat'
    Left = 265
    Top = 16
  end
end
