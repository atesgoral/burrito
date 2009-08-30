object FormBinding: TFormBinding
  Left = 352
  Top = 179
  BorderStyle = bsDialog
  Caption = 'Add Binding'
  ClientHeight = 171
  ClientWidth = 220
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 201
    Height = 129
    TabOrder = 0
    object LabelIP: TLabel
      Left = 16
      Top = 24
      Width = 50
      Height = 13
      Caption = 'IP address'
    end
    object LabelPort: TLabel
      Left = 144
      Top = 24
      Width = 19
      Height = 13
      Caption = 'Port'
    end
    object LabelColon: TLabel
      Left = 136
      Top = 43
      Width = 9
      Height = 13
      Alignment = taCenter
      AutoSize = False
      Caption = ':'
    end
    object LabelBindingHelp: TLabel
      Left = 16
      Top = 72
      Width = 169
      Height = 41
      AutoSize = False
      Caption = '0.0.0.0 means "all IP addresses". The default port is 21.'
      WordWrap = True
    end
    object ComboBoxIP: TComboBox
      Left = 16
      Top = 40
      Width = 121
      Height = 21
      ItemHeight = 13
      MaxLength = 15
      TabOrder = 0
      OnChange = ComboBoxIPChange
      OnKeyPress = ComboBoxIPKeyPress
    end
    object EditPort: TEdit
      Left = 144
      Top = 40
      Width = 41
      Height = 21
      MaxLength = 5
      TabOrder = 1
      Text = '21'
      OnChange = EditPortChange
      OnKeyPress = EditPortKeyPress
    end
  end
  object ButtonOK: TButton
    Left = 8
    Top = 144
    Width = 73
    Height = 21
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object ButtonCancel: TButton
    Left = 136
    Top = 144
    Width = 73
    Height = 21
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
