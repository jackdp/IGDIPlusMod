unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ActnList,
  Vcl.ComCtrls,
  System.Math, System.Types,
  IGDIPlus, IGDIPlusHelpers;

const
  DEG = '°';

type
  TForm1 = class(TForm)
    img: TImage;
    cbColor: TColorBox;
    Actions: TActionList;
    actRefreshImage: TAction;
    actEsc: TAction;
    Label1: TLabel;
    lblCurrentColor: TLabel;
    shTriadicColor1: TShape;
    lblTriadicColor1: TLabel;
    shTriadicColor2: TShape;
    lblTriadicColor2: TLabel;
    shComplementaryColor: TShape;
    lblComplementaryColor: TLabel;
    edDegShift: TLabeledEdit;
    udDegShift: TUpDown;
    procedure actRefreshImageExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actEscExecute(Sender: TObject);
    procedure udDegShiftClick(Sender: TObject; Button: TUDBtnType);
  end;

var
  Form1: TForm1;


procedure DrawColorWheel(Image: TImage; const CurrentColor: TColor; bTriadic, bTetradic: Boolean; DegShift: integer;
  bDrawAxes: Boolean; bDrawDiagonals, bShowComplementaryColor: Boolean);

implementation

{$R *.dfm}


procedure TForm1.FormCreate(Sender: TObject);
begin
  actRefreshImage.Execute;
end;


procedure TForm1.udDegShiftClick(Sender: TObject; Button: TUDBtnType);
begin
  actRefreshImage.Execute;
end;

procedure TForm1.actEscExecute(Sender: TObject);
begin
  Close;
end;





{$region '            from JPLIB: JPL.Math, JPL.Colors            '}

const
  HSL_MAX_CSS_HUE = 360;
  HSL_MAX_CSS_SAT = 100;
  HSL_MAX_CSS_LUM = 100;

var
  HSLMaxHue: integer = HSL_MAX_CSS_HUE;
  HSLMaxSaturation: integer = HSL_MAX_CSS_SAT;
  HSLMaxLightness: integer = HSL_MAX_CSS_LUM;



function SinDeg(const x: Extended): Extended;
var
  rad: Extended;
begin
  rad := x * pi / 180;
  SinDeg := sin(rad);
end;

function CosDeg(const x: Extended): Extended;
var
  rad: Extended;
begin
  rad := x * pi / 180;
  CosDeg := cos(rad);
end;


procedure SetHslMaxValues(const MaxHue, MaxSat, MaxLum: integer);
begin
  HSLMaxHue := MaxHue;
  HSLMaxSaturation := MaxSat;
  HSLMaxLightness := MaxLum;
end;

procedure SetHslCssMaxValues;
begin
  SetHslMaxValues(HSL_MAX_CSS_HUE, HSL_MAX_CSS_SAT, HSL_MAX_CSS_LUM);
end;

procedure GetRgbChannels(const Color: TColor; out r, g, b: Byte);
begin
  {$IFDEF MSWINDOWS}
  r := GetRValue(Color);
  g := GetGValue(Color);
  b := GetBValue(Color);
  {$ELSE}
  r := Byte(Color);
  g := Byte(Color shr 8);
  b := Byte(Color shr 16);
  {$ENDIF}
end;

procedure ColortoHSLRange(const RGB: TColor; out H1, S1, L1: integer);
var
  br, bg, bb: Byte;
  R, G, B, D, Cmax, Cmin, H, S, L: double;
begin
  GetRgbChannels(RGB, br, bg, bb);
  R := br / 255;
  G := bg / 255;
  B := bb / 255;

  Cmax := Max(R, Max(G, B));
  Cmin := Min(R, Min(G, B));
  L := (Cmax + Cmin) / 2;
  if Cmax = Cmin then
  begin
    H := 0;
    S := 0;
  end
  else
  begin
    D := Cmax - Cmin;
    //calc L
    if L < 0.5 then S := D / (Cmax + Cmin)
    else S := D / (2 - Cmax - Cmin);
    //calc H
    if R = Cmax then H := (G - B) / D
    else if G = Cmax then H := 2 + (B - R) / D
    else H := 4 + (R - G) / D;
    H := H / 6;
    if H < 0 then H := H + 1;
  end;
  H1 := round(H * HSLMaxHue);
  S1 := round(S * HSLMaxSaturation);
  L1 := round(L * HSLMaxLightness);
end;


function HslToColor(H, S, L: double): TColor;
var
  M1, M2: double;

  function HueToColorValue(Hue: double): byte;
  var
    V: double;
  begin
    if Hue < 0 then Hue := Hue + 1
    else if Hue > 1 then Hue := Hue - 1;
    if 6 * Hue < 1 then V := M1 + (M2 - M1) * Hue * 6
    else if 2 * Hue < 1 then V := M2
    else if 3 * Hue < 2 then V := M1 + (M2 - M1) * (2 / 3 - Hue) * 6
    else V := M1;
    Result := round(255 * V)
  end;

var
  R, G, B: byte;
begin
  if S = 0 then
  begin
    R := Round(255 * L); //JP mod:  round(HSLMaxLightness * L);
    G := R;
    B := R
  end
  else
  begin
    if L <= 0.5 then M2 := L * (1 + S)
    else M2 := L + S - L * S;
    M1 := 2 * L - M2;
    R := HueToColorValue(H + 1 / 3);
    G := HueToColorValue(H);
    B := HueToColorValue(H - 1 / 3)
  end;
  Result := RGB(R, G, B)
end;

function HSLRangeToRGB(H, S, L: integer): TColor;
begin
  if S > HSLMaxSaturation then S := HSLMaxSaturation;
  if S < 0 then S := 0;
  if L > HSLMaxLightness then L := HSLMaxLightness;
  if L < 0 then L := 0;
  Result := HslToColor(H / HSLMaxHue, S / HSLMaxSaturation, L / HSLMaxLightness);
end;

function HslCssShiftHue(const AColor: TColor; const ShiftValue: integer): TColor;
var
  Hue, Sat, Lum: integer;
begin
  if ShiftValue = 0 then Exit(AColor);

  SetHslCssMaxValues;
  ColortoHSLRange(AColor, Hue, Sat, Lum);
  Hue := Hue + ShiftValue;

  if Hue > HSL_MAX_CSS_HUE then
    while Hue > HSL_MAX_CSS_HUE do Hue := Hue - HSL_MAX_CSS_HUE
  else if Hue < 0 then
    while Hue < 0 do Hue := Hue + HSL_MAX_CSS_HUE;

  Result := HSLRangeToRGB(Hue, Sat, Lum);
end;

function ComplementaryColor(const AColor: TColor): TColor;
begin
  Result := HslCssShiftHue(AColor, HSL_MAX_CSS_HUE div 2 { 180° });
end;

function TriadicColor1(const AColor: TColor; DistanceFromComplementaryColor: integer = 60): TColor;
begin
  Result := HslCssShiftHue(AColor, (HSL_MAX_CSS_HUE div 2) - DistanceFromComplementaryColor { default: 180° - 60° = 120° });
end;

function TriadicColor2(const AColor: TColor; DistanceFromComplementaryColor: integer = 60): TColor;
begin
  Result := HslCssShiftHue(AColor, (HSL_MAX_CSS_HUE div 2) + DistanceFromComplementaryColor { default: 180° + 60° = 240° });
end;

procedure GetTriadicColors(const AColor: TColor; out TriadicColor_1, TriadicColor_2: TColor; DistanceFromComplementaryColor: integer = 60);
begin
  TriadicColor_1 := TriadicColor1(AColor, DistanceFromComplementaryColor);
  TriadicColor_2 := TriadicColor2(AColor, DistanceFromComplementaryColor);
end;

function GetHueCssValue(const AColor: TColor): integer;
var
  x: integer;
begin
  SetHslCssMaxValues;
  ColortoHSLRange(AColor, Result, x, x);
end;

function HslCssToColor(const Hue, Sat, Lum: Single): TColor;
begin
  SetHslCssMaxValues;
  Result := HslToColor(Hue / HSLMaxHue, Sat / HSLMaxSaturation, Lum / HSLMaxLightness);
end;

function InvertColor(const Color: TColor): TColor;
begin
  Result := ColorToRGB(Color) xor $00FFFFFF;
end;

function RGB3(const bt: Byte): TColor;
begin
  Result := RGB(bt, bt, bt);
end;

function TetradicColor1(const AColor: TColor; DegDistance: integer = 90): TColor;
begin
  Result := HslCssShiftHue(AColor, DegDistance);
end;

function TetradicColor2(const AColor: TColor): TColor;
begin
  Result := ComplementaryColor(AColor);
end;

function TetradicColor3(const AColor: TColor; DistanceFromComplementaryColor: integer = 90): TColor;
begin
  Result := HslCssShiftHue(AColor, (HSL_MAX_CSS_HUE div 2) + DistanceFromComplementaryColor);
end;

procedure GetTetradicColors(const AColor: TColor; out TetradColor1, TetradColor2, TetradColor3: TColor; DegDistance: integer = 90);
begin
  TetradColor1 := TetradicColor1(AColor, DegDistance);
  TetradColor2 := TetradicColor2(AColor);
  TetradColor3 := TetradicColor3(AColor, DegDistance);
end;

{$hints off}
function Pad(Text: string; Len: integer; PaddingChar: Char = ' '): string;
var
  x, y, k: integer;
  s: string;
begin
  s := '';
  if Length(Text) < Len then
  begin
    x := Length(Text);
    y := Len - x;
    for k := 1 to y do
      s := s + PaddingChar;
    Text := s + Text;
  end;
  Result := Text;
end;
{$hints on}

function ColorToRgbIntStr(const Color: TColor; Padding: Byte = 3; PaddingChar: Char = '0'; Separator: string = ','): string;
var
  r, g, b: Byte;
begin
  GetRgbChannels(Color, r, g, b);
  Result :=
    Pad(IntToStr(r), Padding, PaddingChar) + Separator +
    Pad(IntToStr(g), Padding, PaddingChar) + Separator +
    Pad(IntToStr(b), Padding, PaddingChar);
end;

function ColorToHslRangeStr(const Color: TColor; AMaxHue: integer = 360; AMaxSat: integer = 100; AMaxLum: integer = 100; bShowPercent: Boolean = True;
  Padding: Byte = 0; PaddingChar: Char = ' '; Separator: string = ','; bShowDeg: Boolean = True): string;
var
  H1, S1, L1: integer;
  sH, sS, sL: string;
begin
  SetHslMaxValues(AMaxHue, AMaxSat, AMaxLum);

  ColortoHSLRange(Color, H1, S1, L1);

  sH := Pad(IntToStr(H1), Padding, PaddingChar);
  if bShowDeg then sH := sH + DEG;

  sS := Pad(IntToStr(S1), Padding, PaddingChar);
  if bShowPercent then sS := sS + '%';

  sL := Pad(IntToStr(L1), Padding, PaddingChar);
  if bShowPercent then sL := sL + '%';

  Result := sH + Separator + sS + Separator + sL;
end;

function ColorToHslCssStr(const Color: TColor; UsePercent: Boolean = True; Padding: Byte = 0; PaddingChar: Char = ' '; Separator: string = ','; bShowDeg: Boolean = True): string;
begin
  Result := ColorToHslRangeStr(Color, HSL_MAX_CSS_HUE, HSL_MAX_CSS_SAT, HSL_MAX_CSS_LUM, UsePercent, Padding, PaddingChar, Separator, bShowDeg);
end;

{$endregion from JPLIB: JPL.Math, JPL.Colors}



procedure TForm1.actRefreshImageExecute(Sender: TObject);
var
  CurrentColor, clT1, clT2, clCompl: TColor;
  DegShift: integer;
begin
  CurrentColor := cbColor.Selected;
  lblCurrentColor.Caption := 'RGB ' + ColorToRgbIntStr(CurrentColor) + sLineBreak + 'HSL ' + ColorToHslCssStr(CurrentColor);


  DegShift := udDegShift.Position;
  DrawColorWheel(img, CurrentColor, True, False, DegShift, True, True, True);


  clT1 := TriadicColor1(CurrentColor, DegShift);
  shTriadicColor1.Brush.Color := clT1;
  lblTriadicColor1.Caption := 'Triadic color 1 (RGB ' + ColorToRgbIntStr(clT1) + ' / HSL ' + ColorToHslCssStr(clT1) + '):';

  clT2 := TriadicColor2(CurrentColor, DegShift);
  shTriadicColor2.Brush.Color := clT2;
  lblTriadicColor2.Caption := 'Triadic color 2 (RGB ' + ColorToRgbIntStr(clT2) + ' / HSL ' + ColorToHslCssStr(clT2) + '):';

  clCompl := ComplementaryColor(CurrentColor);
  shComplementaryColor.Brush.Color := clCompl;
  lblComplementaryColor.Caption := 'Complementary color (RGB ' + ColorToRgbIntStr(clCompl) + ' / HSL ' + ColorToHslCssStr(clCompl) + '):';
end;


{$region '                                   DrawColorWheel                                       '}
procedure DrawColorWheel(Image: TImage; const CurrentColor: TColor; bTriadic, bTetradic: Boolean; DegShift: integer;
  bDrawAxes: Boolean; bDrawDiagonals, bShowComplementaryColor: Boolean);
var
  bmp: TBitmap;
  xWidth, xHeight, i, xPies: integer;
  gr: IGPGraphics;
  Pen: IGPPen;
  Brush: IGPBrush;
  Margin, Radius, Hue, Sweep: Single;
  halfW, halfH: Single;
  SelColHue, SelColSat, SelColLum, CompColHue: integer;
  RectF: TIGPRectF;
  AngleStart, AngleSweep, DeltaHue: Single;
  cl, clBg, clCurrent, clTriad1, clTriad2, clComplementary, clTetrad1, clTetrad2, clTetrad3: TColor;
  dxr, fi: Single;
  s: string;
  AFont: IGPFont;
  Point: TPointF;
const
  DashValues: array [0..1] of Single = (4, 4);


  {$region '     DrawHueLine     '}
  procedure DrawHueLine(const HueValue: Single; const AColor: TColor; const LineWidth: Single; bDashed: Boolean = False);
  var
    pt1, pt2: TPointF;
  begin
    gr.ResetTransform;
    gr.TranslateTransform(halfW, halfH);
    gr.ScaleTransform(1, -1);

    Pen.Color := GPColor(AColor, 220);
    Pen.Width := LineWidth;
    if bDashed then Pen.SetDashPattern(DashValues)
    else Pen.DashStyle := DashStyleSolid;
    pt1.X := 0;
    pt1.Y := 0;

    fi := 90 - HueValue;
    dxr := Radius + 0;
    pt2.X := dxr * CosDeg(fi);
    pt2.Y := dxr * SinDeg(fi);
    gr.DrawLineF(Pen, pt1, pt2);
  end;
  {$endregion DrawHueLine}


  {$region '     DrawHuePolygon     '}
  procedure DrawHuePolygon(const Arr: array of Single; const AColor: TColor; const LineWidth: Single);
  var
    Points: array of TPointF;
    i: integer;
  begin

    if Length(Arr) = 0 then Exit;
    SetLength(Points, Length(Arr));

    for i := 0 to High(Arr) do
    begin
      fi := 90 - Arr[i];
      Points[i].X := Radius * CosDeg(fi);
      Points[i].Y := Radius * SinDeg(fi);
    end;

    Pen.Color := GPColor(AColor, 255);
    Pen.Width := LineWidth;
    Pen.SetDashPattern(DashValues);

    gr.DrawPolygonF(Pen, Points);
  end;
  {$endregion DrawHuePolygon}


  {$region '     DrawHuePoint     '}
  procedure DrawHuePoint(const Hue: Single; AColor: TColor);
  var
    pt1: TPointF;
  begin
    gr.ResetTransform;
    gr.TranslateTransform(halfW, halfH);
    gr.ScaleTransform(1, -1);

    // ------ punkt œrodkowy okrêgu ---------------
    fi := 90 - Hue;
    pt1.X := Radius * CosDeg(fi);
    pt1.Y := Radius * SinDeg(fi);

    dxr := 4;
    RectF.X := pt1.X - dxr;
    RectF.Y := pt1.Y - dxr;
    RectF.Width := dxr * 2;
    RectF.Height := dxr * 2;

    Brush := nil;
    Brush := TIGPSolidBrush.Create(GPColor(AColor, 140));
    Pen.Width := 2;
    Pen.Color := GPColor(clBlack, 255);
    Pen.DashStyle := DashStyleSolid;
    gr.DrawEllipseF(Pen, RectF);
    gr.FillEllipseF(Brush, RectF);

  end;
  {$endregion DrawHuePoint}


begin
  bmp := TBitmap.Create;
  try

    xWidth := Image.Width;
    xHeight := Image.Height;
    bmp.SetSize(xWidth, xHeight);
    bmp.PixelFormat := pf32bit;
    halfW := xWidth / 2;
    halfH := xHeight / 2;
    Margin := 38;

    xPies := 360;
    clCurrent := CurrentColor;
    clComplementary := ComplementaryColor(clCurrent);
    clBg := clWhite;



    gr := TIGPGraphics.Create(bmp.Canvas.Handle);
    gr.SmoothingMode := SmoothingModeAntiAlias;
    gr.ResetTransform;


    // ------------- background ----------------
    RectF.Create(0, 0, xWidth, xHeight);
    Brush := TIGPSolidBrush.Create(GPColor(clBg));
    gr.FillRectangleF(Brush, RectF);

    // ------------ frame -------------
    RectF.Width := RectF.Width - 1;
    RectF.Height := RectF.Height - 1;
    Pen := TIGPPen.Create(GPColor($00A8A8A8), 1);
    gr.DrawRectangleF(Pen, RectF);



    // ----------- okr¹g --------------
    Pen.DashStyle := DashStyleSolid;
    Pen.Color := GPColor(clGray);
    Pen.Width := 1;
    RectF := GPInflateRectF(RectF, -Margin);
    gr.DrawEllipseF(Pen, RectF);



    // ----------- transformacja uk³adu wspó³rzêdnych -------------
    gr.TranslateTransform(halfW, halfH); // przesuniêcie pocz¹tku uk³adu wspó³rzêdnych na œrodek
    gr.RotateTransform(-90); // 0 stopni u góry



    // --------------------- Pies ---------------------------
    gr.SmoothingMode := SmoothingModeNone;
    Radius := halfW - Margin;
    DeltaHue := 360 / xPies;
    Sweep := DeltaHue;

    RectF.X := -Radius;
    RectF.Y := -Radius;
    RectF.Width := Radius * 2;
    RectF.Height := RectF.Width;

    SetHslCssMaxValues;
    ColortoHSLRange(clCurrent, SelColHue, SelColSat, SelColLum);
    CompColHue := GetHueCssValue(clComplementary);

    for i := 0 to xPies - 1 do
    begin
      Hue := i * DeltaHue;
      cl := HslCssToColor(Hue, SelColSat, SelColLum);
      Brush := nil;
      Brush := TIGPSolidBrush.Create(GPColor(cl, 255));
      AngleStart := Hue;   // k¹t pocz¹tkowy
      AngleSweep := Sweep; // rozpiêtoœæ k¹towa
      gr.FillPieF(Brush, RectF, AngleStart, AngleSweep);
    end;



    gr.SmoothingMode := SmoothingModeAntiAlias;


    // ------------------ inner circle ------------------------
    Brush := nil;
    Brush := TIGPSolidBrush.Create(GPColor(clWhite, 120));
    Pen.Color := GPColor(clGray, 100);
    dxr := Radius - (halfW / 2);

    gr.ResetTransform;
    gr.TranslateTransform(halfW, halfH);
    RectF.Create(-dxr, -dxr, dxr * 2, dxr * 2);

    gr.FillEllipseF(Brush, RectF);
    gr.DrawEllipseF(Pen, RectF);



    gr.ResetTransform;
    Pen.SetDashPattern(DashValues);

    Pen.Color := GPColor(clSilver, 150);
    // ---------------- osie ------------------
    if bDrawAxes then
    begin
      gr.DrawLineF(Pen, 0, xHeight / 2, xWidth, xHeight / 2); // X axis
      gr.DrawLineF(Pen, xWidth / 2, xHeight, xWidth / 2, 0);  // Y axis
    end;

    // ------------ przek¹tne -------------
    if bDrawDiagonals then
    begin
      gr.DrawLine(Pen, 0, 0, xWidth, xHeight);
      gr.DrawLine(Pen, xWidth, 0, 0, xHeight);
    end;



    gr.ResetTransform;
    gr.TranslateTransform(halfW, halfH);
    gr.ScaleTransform(1, -1);


    if bTriadic then
    begin
      GetTriadicColors(clCurrent, clTriad1, clTriad2, DegShift);

      DrawHueLine(SelColHue, InvertColor(clCurrent), 3);
      DrawHueLine(SelColHue + 180 - DegShift, InvertColor(clTriad1), 1, True);
      DrawHueLine(SelColHue + 180 + DegShift, InvertColor(clTriad2), 1, True);

      DrawHuePolygon([SelColHue, SelColHue + 180 - DegShift, SelColHue + 180 + DegShift], RGB3(22), 1.4);

      DrawHuePoint(SelColHue, clCurrent);
      DrawHuePoint(SelColHue + 180 - DegShift, clTriad1);
      DrawHuePoint(SelColHue + 180 + DegShift, clTriad2);
      if bShowComplementaryColor then DrawHuePoint(CompColHue, clComplementary);
    end;

    if bTetradic then
    begin
      GetTetradicColors(clCurrent, clTetrad1, clTetrad2, clTetrad3, DegShift);

      DrawHueLine(SelColHue, InvertColor(clCurrent), 3);
      DrawHueLine(SelColHue + DegShift, InvertColor(clTetrad1), 1, True);
      DrawHueLine(SelColHue + 180, InvertColor(clTetrad2), 1, True);
      DrawHueLine(SelColHue + 180 + DegShift, InvertColor(clTetrad3), 1, True);

      DrawHuePolygon([SelColHue, SelColHue + DegShift, SelColHue + 180, SelColHue + 180 + DegShift], RGB3(22), 1.4);

      DrawHuePoint(SelColHue, clCurrent);
      DrawHuePoint(SelColHue + DegShift, clTetrad1);
      DrawHuePoint(SelColHue + 180, clTetrad2);
      DrawHuePoint(SelColHue + 180 + DegShift, clTetrad3);
    end;

    gr.ResetTransform;


    //------------------------ Text -----------------------------

    Brush := nil;
    Brush := TIGPSolidBrush.Create(GPColor(clBlack));
    AFont := TIGPFont.Create('Verdana', 8, [], UnitPoint);

    s := '0' + DEG;
    Point := GPPointF(halfW, 8);
    Point := GPPointFMove(Point, -(GPTextWidthF(gr, s, AFont) / 2), 0);
    gr.DrawStringF(WideString(s), AFont, Point, Brush);

    s := '90' + DEG;
    Point := GPPointF(xWidth, xHeight / 2);
    Point := GPPointFMove(Point, -GPTextWidthF(gr, s, AFont) - 4, -GPTextHeightF(gr, s, AFont) / 2);
    gr.DrawStringF(WideString(s), AFont, Point, Brush);

    s := '180' + DEG;
    Point := GPPointF(halfW, 5);
    Point := GPPointFMove(Point, -(GPTextWidthF(gr, s, AFont) / 2), xHeight - GPTextHeightF(gr, s, AFont) - 8);
    gr.DrawStringF(WideString(s), AFont, Point, Brush);

    s := '270' + DEG;
    Point := GPPointF(0, xHeight / 2);
    Point := GPPointFMove(Point, 4, -GPTextHeightF(gr, s, AFont) / 2);
    gr.DrawStringF(WideString(s), AFont, Point, Brush);


    Image.Picture.Assign(bmp);

  finally
    bmp.Free;
  end;

end;

{$endregion DrawColorWheel}


end.
