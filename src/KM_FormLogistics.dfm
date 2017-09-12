object FormLogistics: TFormLogistics
  Left = 0
  Top = 0
  Caption = 'FormLogistics'
  ClientHeight = 615
  ClientWidth = 431
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ListView: TListView
    Left = 0
    Top = 0
    Width = 431
    Height = 615
    Align = alClient
    Columns = <
      item
        Caption = 'Resurce'
        Width = 120
      end
      item
        Caption = 'From'
        Width = 120
      end
      item
        Caption = 'To'
        Width = 120
      end>
    TabOrder = 0
    ViewStyle = vsReport
    ExplicitLeft = 104
    ExplicitTop = 192
    ExplicitWidth = 250
    ExplicitHeight = 150
  end
end
