object Form_MapSettings: TForm_MapSettings
  Left = 142
  Top = 93
  BorderStyle = bsDialog
  Caption = 'Form_MapSettings'
  ClientHeight = 329
  ClientWidth = 353
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 337
    Height = 281
    ActivePage = TabSheet2
    TabIndex = 1
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'General'
      object Label1: TLabel
        Left = 232
        Top = 11
        Width = 66
        Height = 13
        Caption = 'Map file name'
      end
      object EditMapFile: TEdit
        Left = 8
        Top = 8
        Width = 217
        Height = 21
        TabOrder = 0
        Text = 'EditMapFile'
        OnChange = EditMapFileChange
      end
      object RGHumanPlayer: TRadioGroup
        Left = 8
        Top = 40
        Width = 313
        Height = 57
        Caption = ' Set human player  '
        Columns = 3
        Items.Strings = (
          'Player1'
          'Player2'
          'Player3'
          'Player4'
          'Player5'
          'Player6')
        TabOrder = 1
        OnClick = EditMapFileChange
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Houses'
      ImageIndex = 1
      object Label2: TLabel
        Left = 8
        Top = 0
        Width = 71
        Height = 13
        Caption = 'Release house'
      end
      object Label3: TLabel
        Left = 184
        Top = 0
        Width = 59
        Height = 13
        Caption = 'Block house'
      end
      object CLBReleaseHouse: TCheckListBox
        Left = 8
        Top = 16
        Width = 137
        Height = 233
        ItemHeight = 13
        TabOrder = 0
        OnClick = EditMapFileChange
      end
      object CLBBlockHouse: TCheckListBox
        Left = 184
        Top = 16
        Width = 137
        Height = 233
        ItemHeight = 13
        TabOrder = 1
        OnClick = EditMapFileChange
      end
    end
  end
  object Button1: TButton
    Left = 272
    Top = 296
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 1
    OnClick = Button1Click
  end
end
