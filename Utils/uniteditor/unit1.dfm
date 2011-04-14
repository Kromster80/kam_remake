object Form1: TForm1
  Left = 231
  Top = 158
  Width = 616
  Height = 418
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object SName: TLabel
    Left = 25
    Top = 80
    Width = 50
    Height = 13
    Caption = 'Unit Name'
    Color = clBtnFace
    ParentColor = False
  end
  object UnitNumber: TLabel
    Left = 25
    Top = 47
    Width = 56
    Height = 13
    Caption = 'UnitNumber'
    Color = clBtnFace
    ParentColor = False
  end
  object Label1: TLabel
    Left = 25
    Top = 120
    Width = 41
    Height = 13
    Caption = 'Hitpoints'
    Color = clBtnFace
    ParentColor = False
  end
  object Label2: TLabel
    Left = 25
    Top = 159
    Width = 28
    Height = 13
    Caption = 'Atack'
    Color = clBtnFace
    ParentColor = False
  end
  object Label3: TLabel
    Left = 25
    Top = 199
    Width = 89
    Height = 13
    Caption = 'AttackHorseBonus'
    Color = clBtnFace
    ParentColor = False
  end
  object Label4: TLabel
    Left = 25
    Top = 232
    Width = 11
    Height = 13
    Caption = 'x4'
    Color = clBtnFace
    ParentColor = False
  end
  object Defence: TLabel
    Left = 25
    Top = 272
    Width = 41
    Height = 13
    Caption = 'Defence'
    Color = clBtnFace
    ParentColor = False
  end
  object Speed: TLabel
    Left = 25
    Top = 312
    Width = 31
    Height = 13
    Caption = 'Speed'
    Color = clBtnFace
    ParentColor = False
  end
  object label5: TLabel
    Left = 288
    Top = 120
    Width = 11
    Height = 13
    Caption = 'x7'
    Color = clBtnFace
    ParentColor = False
  end
  object Label6: TLabel
    Left = 288
    Top = 160
    Width = 24
    Height = 13
    Caption = 'Sight'
    Color = clBtnFace
    ParentColor = False
  end
  object Label7: TLabel
    Left = 288
    Top = 199
    Width = 11
    Height = 13
    Caption = 'x9'
    Color = clBtnFace
    ParentColor = False
  end
  object Label8: TLabel
    Left = 288
    Top = 232
    Width = 17
    Height = 13
    Caption = 'x10'
    Color = clBtnFace
    ParentColor = False
  end
  object Label9: TLabel
    Left = 288
    Top = 272
    Width = 61
    Height = 13
    Caption = 'CanWalkOut'
    Color = clBtnFace
    ParentColor = False
  end
  object Label10: TLabel
    Left = 288
    Top = 312
    Width = 17
    Height = 13
    Caption = 'x11'
    Color = clBtnFace
    ParentColor = False
  end
  object Label11: TLabel
    Left = 161
    Top = 82
    Width = 26
    Height = 13
    Caption = 'name'
    Color = clBtnFace
    ParentColor = False
  end
  object Open: TButton
    Left = 104
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Open'
    TabOrder = 0
    OnClick = open_file
  end
  object number: TEdit
    Left = 160
    Top = 48
    Width = 80
    Height = 21
    TabOrder = 1
    Text = '1'
    OnChange = showDAT
  end
  object HP: TEdit
    Left = 160
    Top = 120
    Width = 80
    Height = 21
    TabOrder = 2
  end
  object ATK: TEdit
    Left = 160
    Top = 160
    Width = 80
    Height = 21
    TabOrder = 3
  end
  object AHB: TEdit
    Left = 160
    Top = 199
    Width = 80
    Height = 21
    TabOrder = 4
  end
  object x4: TEdit
    Left = 160
    Top = 232
    Width = 80
    Height = 21
    TabOrder = 5
  end
  object DEF: TEdit
    Left = 160
    Top = 272
    Width = 80
    Height = 21
    TabOrder = 6
  end
  object SPD: TEdit
    Left = 161
    Top = 312
    Width = 79
    Height = 21
    TabOrder = 7
  end
  object x7: TEdit
    Left = 380
    Top = 120
    Width = 81
    Height = 21
    TabOrder = 8
  end
  object Sight: TEdit
    Left = 380
    Top = 159
    Width = 81
    Height = 21
    TabOrder = 9
  end
  object x9: TEdit
    Left = 380
    Top = 199
    Width = 81
    Height = 21
    TabOrder = 10
  end
  object x10: TEdit
    Left = 380
    Top = 232
    Width = 81
    Height = 21
    TabOrder = 11
  end
  object CVA: TEdit
    Left = 380
    Top = 272
    Width = 81
    Height = 21
    TabOrder = 12
  end
  object x11: TEdit
    Left = 380
    Top = 312
    Width = 81
    Height = 21
    TabOrder = 13
  end
  object Button1: TButton
    Left = 192
    Top = 8
    Width = 83
    Height = 23
    Caption = 'Save'
    TabOrder = 14
    OnClick = saveDAT
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.dat'
    Left = 288
    Top = 8
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '../../Data/Defines/Unit.dat'
    Left = 345
    Top = 8
  end
end
