object FormLog: TFormLog
  Left = 245
  Top = 221
  Width = 233
  Height = 254
  Caption = 'Burrito Log'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object MemoLog: TMemo
    Left = 0
    Top = 0
    Width = 225
    Height = 225
    TabStop = False
    Align = alClient
    Color = clBtnFace
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
end
