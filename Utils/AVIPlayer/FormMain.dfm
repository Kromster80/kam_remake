object FrmMain: TFrmMain
  Left = 192
  Top = 112
  Caption = 'AVIPlayer'
  ClientHeight = 616
  ClientWidth = 1126
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    1126
    616)
  PixelsPerInch = 96
  TextHeight = 13
  object PlayButton: TSpeedButton
    Left = 8
    Top = 528
    Width = 23
    Height = 22
    Anchors = [akLeft, akBottom]
    Enabled = False
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000130B0000130B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      33333333333333333333EEEEEEEEEEEEEEE333FFFFFFFFFFFFF3E00000000000
      00E337777777777777F3E0F77777777770E337F33333333337F3E0F333333333
      70E337F3333F333337F3E0F33303333370E337F3337FF33337F3E0F333003333
      70E337F33377FF3337F3E0F33300033370E337F333777FF337F3E0F333000033
      70E337F33377773337F3E0F33300033370E337F33377733337F3E0F333003333
      70E337F33377333337F3E0F33303333370E337F33373333337F3E0F333333333
      70E337F33333333337F3E0FFFFFFFFFFF0E337FFFFFFFFFFF7F3E00000000000
      00E33777777777777733EEEEEEEEEEEEEEE33333333333333333}
    NumGlyphs = 2
    OnClick = PlayButtonClick
  end
  object PauseButton: TSpeedButton
    Left = 32
    Top = 528
    Width = 23
    Height = 22
    Anchors = [akLeft, akBottom]
    Enabled = False
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000130B0000130B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      33333333333333333333EEEEEEEEEEEEEEE333FFFFFFFFFFFFF3E00000000000
      00E337777777777777F3E0F77777777770E337F33333333337F3E0F333333333
      70E337F33333333337F3E0F33333333370E337F333FF3FF337F3E0F330030033
      70E337F3377F77F337F3E0F33003003370E337F3377F77F337F3E0F330030033
      70E337F3377F77F337F3E0F33003003370E337F3377F77F337F3E0F330030033
      70E337F33773773337F3E0F33333333370E337F33333333337F3E0F333333333
      70E337F33333333337F3E0FFFFFFFFFFF0E337FFFFFFFFFFF7F3E00000000000
      00E33777777777777733EEEEEEEEEEEEEEE33333333333333333}
    NumGlyphs = 2
    OnClick = PauseButtonClick
  end
  object StopButton: TSpeedButton
    Left = 56
    Top = 528
    Width = 23
    Height = 22
    Anchors = [akLeft, akBottom]
    Enabled = False
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000130B0000130B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      33333333333333333333EEEEEEEEEEEEEEE333FFFFFFFFFFFFF3E00000000000
      00E337777777777777F3E0F77777777770E337F33333333337F3E0F333333333
      70E337F33333333337F3E0F33333333370E337F333FFFFF337F3E0F330000033
      70E337F3377777F337F3E0F33000003370E337F3377777F337F3E0F330000033
      70E337F3377777F337F3E0F33000003370E337F3377777F337F3E0F330000033
      70E337F33777773337F3E0F33333333370E337F33333333337F3E0F333333333
      70E337F33333333337F3E0FFFFFFFFFFF0E337FFFFFFFFFFF7F3E00000000000
      00E33777777777777733EEEEEEEEEEEEEEE33333333333333333}
    NumGlyphs = 2
    OnClick = StopButtonClick
  end
  object FramesLabel: TLabel
    Left = 232
    Top = 528
    Width = 3
    Height = 13
    Anchors = [akLeft, akBottom]
  end
  object Button1: TButton
    Left = 8
    Top = 550
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Intro.avi'
    TabOrder = 0
    OnClick = Button1Click
  end
  object edPath: TEdit
    Left = 8
    Top = 590
    Width = 489
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 1
    Text = '..\..\data\gfx\video\'
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1126
    Height = 521
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Color = clBlack
    TabOrder = 2
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 1126
      Height = 521
      Align = alClient
      Center = True
      Proportional = True
    end
  end
  object Button2: TButton
    Left = 88
    Top = 550
    Width = 89
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'KAMLOGO.avi'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button3: TButton
    Left = 184
    Top = 550
    Width = 89
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'LOST.avi'
    TabOrder = 4
    OnClick = Button1Click
  end
  object Button4: TButton
    Left = 280
    Top = 550
    Width = 89
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'M1.avi'
    TabOrder = 5
    OnClick = Button1Click
  end
  object Button5: TButton
    Left = 376
    Top = 550
    Width = 89
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'M2.avi'
    TabOrder = 6
    OnClick = Button1Click
  end
  object Button6: TButton
    Left = 472
    Top = 550
    Width = 89
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'M5.avi'
    TabOrder = 7
    OnClick = Button1Click
  end
  object Button7: TButton
    Left = 568
    Top = 550
    Width = 89
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'M6.avi'
    TabOrder = 8
    OnClick = Button1Click
  end
  object Button8: TButton
    Left = 664
    Top = 550
    Width = 89
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'M20.avi'
    TabOrder = 9
    OnClick = Button1Click
  end
  object Button9: TButton
    Left = 760
    Top = 550
    Width = 89
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'OUTRO.avi'
    TabOrder = 10
    OnClick = Button1Click
  end
  object Button10: TButton
    Left = 856
    Top = 550
    Width = 89
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'OutroNew.avi'
    TabOrder = 11
    OnClick = Button1Click
  end
  object Button11: TButton
    Left = 952
    Top = 550
    Width = 89
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Publish.avi'
    TabOrder = 12
    OnClick = Button1Click
  end
  object Button12: TButton
    Left = 1048
    Top = 550
    Width = 73
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Victory.avi'
    TabOrder = 13
    OnClick = Button1Click
  end
  object CheckBox1: TCheckBox
    Left = 504
    Top = 590
    Width = 113
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Stretch Video'
    TabOrder = 14
    OnClick = CheckBox1Click
  end
  object ProgressBar: TProgressBar
    Left = 80
    Top = 528
    Width = 150
    Height = 17
    Anchors = [akLeft, akBottom]
    Smooth = True
    TabOrder = 15
  end
  object DoubleHeightCheck: TCheckBox
    Left = 624
    Top = 590
    Width = 97
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Double Height'
    Checked = True
    State = cbChecked
    TabOrder = 16
    OnClick = DoubleHeightCheckClick
  end
  object BlackLinesCheck: TCheckBox
    Left = 728
    Top = 590
    Width = 89
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Black Lines'
    TabOrder = 17
    OnClick = BlackLinesCheckClick
  end
  object Brightness: TSpinEdit
    Left = 832
    Top = 586
    Width = 65
    Height = 22
    Anchors = [akLeft, akBottom]
    MaxLength = 4
    MaxValue = 126
    MinValue = -126
    TabOrder = 18
    Value = -25
    OnChange = BrightnessChange
  end
  object RenderTimer: TTimer
    Enabled = False
    Interval = 1
    OnTimer = RenderTimerTimer
    Left = 912
    Top = 576
  end
end
