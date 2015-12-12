object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Pascal To Wiki Parser'
  ClientHeight = 495
  ClientWidth = 778
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    778
    495)
  PixelsPerInch = 96
  TextHeight = 13
  object txtParserOutput: TMemo
    Left = 8
    Top = 8
    Width = 762
    Height = 479
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    OnKeyPress = txtParserOutputKeyPress
  end
  object MainMenu1: TMainMenu
    Left = 312
    Top = 16
    object btnMenuFile: TMenuItem
      Caption = '&File'
      object btnMenuOpen: TMenuItem
        Caption = '&Open file'
        ShortCut = 16463
        OnClick = btnMenuOpenClick
      end
      object btnMenuSave: TMenuItem
        Caption = '&Save to file'
        ShortCut = 16467
        OnClick = btnMenuSaveClick
      end
      object menuSep1: TMenuItem
        Caption = '-'
      end
      object btnMenuExit: TMenuItem
        Caption = '&Quit'
        ShortCut = 16465
        OnClick = btnMenuExitClick
      end
    end
    object btnMenuHelp: TMenuItem
      Caption = '&Help'
      ShortCut = 16456
      OnClick = btnMenuHelpClick
    end
  end
  object OpenTxtDlg: TOpenTextFileDialog
    Left = 400
    Top = 16
  end
  object SaveTxtDlg: TSaveTextFileDialog
    Left = 504
    Top = 16
  end
end
