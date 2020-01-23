object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'IGDIPlus Demo - Color Wheel'
  ClientHeight = 415
  ClientWidth = 783
  Color = 15066597
  DoubleBuffered = True
  Font.Charset = EASTEUROPE_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object img: TImage
    Left = 8
    Top = 8
    Width = 400
    Height = 400
  end
  object Label1: TLabel
    Left = 425
    Top = 23
    Width = 74
    Height = 15
    Caption = 'Change color:'
  end
  object lblCurrentColor: TLabel
    Left = 425
    Top = 67
    Width = 82
    Height = 15
    Caption = 'lblCurrentColor'
  end
  object shTriadicColor1: TShape
    Left = 425
    Top = 150
    Width = 312
    Height = 25
  end
  object lblTriadicColor1: TLabel
    Left = 425
    Top = 133
    Width = 82
    Height = 15
    Caption = 'lblTriadicColor1'
  end
  object shTriadicColor2: TShape
    Left = 425
    Top = 214
    Width = 312
    Height = 25
  end
  object lblTriadicColor2: TLabel
    Left = 425
    Top = 197
    Width = 82
    Height = 15
    Caption = 'lblTriadicColor2'
  end
  object shComplementaryColor: TShape
    Left = 425
    Top = 286
    Width = 312
    Height = 25
  end
  object lblComplementaryColor: TLabel
    Left = 425
    Top = 269
    Width = 128
    Height = 15
    Caption = 'lblComplementaryColor'
  end
  object cbColor: TColorBox
    Left = 425
    Top = 39
    Width = 145
    Height = 22
    Selected = clYellow
    Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
    DropDownCount = 16
    TabOrder = 0
    OnChange = actRefreshImageExecute
  end
  object edDegShift: TLabeledEdit
    Left = 585
    Top = 40
    Width = 50
    Height = 23
    EditLabel.Width = 48
    EditLabel.Height = 15
    EditLabel.Caption = 'Distance:'
    LabelSpacing = 2
    ReadOnly = True
    TabOrder = 1
    Text = '60'
  end
  object udDegShift: TUpDown
    Left = 635
    Top = 40
    Width = 16
    Height = 23
    Associate = edDegShift
    Min = 1
    Max = 179
    Position = 60
    TabOrder = 2
    OnClick = udDegShiftClick
  end
  object Actions: TActionList
    Left = 248
    Top = 176
    object actRefreshImage: TAction
      Caption = 'Refresh image'
      OnExecute = actRefreshImageExecute
    end
    object actEsc: TAction
      Caption = 'actEsc'
      ShortCut = 27
      OnExecute = actEscExecute
    end
  end
end
