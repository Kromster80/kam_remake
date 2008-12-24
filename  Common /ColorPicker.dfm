object Form_ColorPicker: TForm_ColorPicker
  Left = 412
  Top = 148
  BorderStyle = bsDialog
  Caption = 'Color picker'
  ClientHeight = 345
  ClientWidth = 409
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object HSImage: TImage
    Left = 4
    Top = 4
    Width = 360
    Height = 256
    OnMouseDown = HSImageMouseDown
    OnMouseMove = HSImageMouseMove
    OnMouseUp = HSImageMouseUp
  end
  object BriImage: TImage
    Left = 372
    Top = 4
    Width = 25
    Height = 256
    Proportional = True
    OnMouseDown = BriImageMouseDown
    OnMouseMove = BriImageMouseMove
    OnMouseUp = BriImageMouseUp
  end
  object Ticker: TShape
    Left = 368
    Top = 129
    Width = 33
    Height = 9
    Brush.Style = bsClear
    Enabled = False
    Pen.Width = 2
  end
  object Shape2: TShape
    Left = 4
    Top = 268
    Width = 113
    Height = 73
  end
  object Shape1: TShape
    Left = 180
    Top = 124
    Width = 9
    Height = 9
    Brush.Style = bsClear
    Enabled = False
    Pen.Style = psDot
    Pen.Width = 2
  end
  object Label1: TLabel
    Left = 178
    Top = 271
    Width = 20
    Height = 13
    Caption = 'Hue'
  end
  object Label2: TLabel
    Left = 178
    Top = 297
    Width = 48
    Height = 13
    Caption = 'Saturation'
  end
  object Label3: TLabel
    Left = 178
    Top = 323
    Width = 49
    Height = 13
    Caption = 'Brightness'
  end
  object Label4: TLabel
    Left = 290
    Top = 271
    Width = 20
    Height = 13
    Caption = 'Red'
  end
  object Label5: TLabel
    Left = 290
    Top = 297
    Width = 29
    Height = 13
    Caption = 'Green'
  end
  object Label6: TLabel
    Left = 290
    Top = 323
    Width = 21
    Height = 13
    Caption = 'Blue'
  end
  object Button1: TButton
    Left = 344
    Top = 312
    Width = 59
    Height = 25
    Caption = 'Reset'
    TabOrder = 0
    OnClick = Button1Click
  end
  object SpinR: TFloatSpinEdit
    Left = 236
    Top = 268
    Width = 49
    Height = 22
    Accuracy = 0
    Increment = 1
    MaxValue = 255
    TabOrder = 1
    OnChange = SpinRGBChange
  end
  object SpinG: TFloatSpinEdit
    Left = 236
    Top = 294
    Width = 49
    Height = 22
    Accuracy = 0
    Increment = 1
    MaxValue = 255
    TabOrder = 2
    OnChange = SpinRGBChange
  end
  object SpinB: TFloatSpinEdit
    Left = 236
    Top = 320
    Width = 49
    Height = 22
    Accuracy = 0
    Increment = 1
    MaxValue = 255
    TabOrder = 3
    OnChange = SpinRGBChange
  end
  object SpinH: TFloatSpinEdit
    Left = 124
    Top = 268
    Width = 49
    Height = 22
    Accuracy = 0
    Increment = 1
    MaxValue = 359
    TabOrder = 4
    OnChange = SpinHSBChange
  end
  object SpinS: TFloatSpinEdit
    Left = 124
    Top = 294
    Width = 49
    Height = 22
    Accuracy = 0
    Increment = 1
    MaxValue = 255
    TabOrder = 5
    OnChange = SpinHSBChange
  end
  object SpinBr: TFloatSpinEdit
    Left = 124
    Top = 320
    Width = 49
    Height = 22
    Accuracy = 0
    Increment = 1
    MaxValue = 255
    TabOrder = 6
    OnChange = SpinHSBChange
  end
  object Button2: TButton
    Left = 344
    Top = 280
    Width = 59
    Height = 25
    Caption = 'OK'
    TabOrder = 7
    OnClick = Button2Click
  end
end
