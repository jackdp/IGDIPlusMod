unit IGDIPlusHelpers;

{$IFDEF FPC}
  {$mode delphi}
  {$I IGDIPlusAPI_FPC}
{$ENDIF}
{$IFDEF DCC}{$DEFINE HAS_SYSTEM_UITYPES}{$ENDIF}

interface

uses
  Classes, SysUtils, Graphics, Types,
  IGDIPlus
  {$IFDEF HAS_SYSTEM_UITYPES},System.UITypes{$ENDIF}
  ;


{$IFDEF FPC}
type

  TPointFHelper = record helper for Types.TPointF
  public
    class function Create(const ax, ay: Single): TPointF; overload; static; inline;
    class function Create(const apt: TPoint): TPointF; overload; static; inline;
  end;
{$ENDIF}


type
  TGPHatchStyleDynArray = array of TIGPHatchStyle;

function GPPointF(const ax, ay: Single): TPointF; overload;
function GPPointF(const apt: TPoint): TPointF; overload;
function GPPointFMove(const Point: TPointF; const dx, dy: Single): TPointF;

function GPColor(const AColor: TColor; Alpha: Byte = 255): TAlphaColor;
{$IFDEF MSWINDOWS}
function GPTextWidthF(gr: IGPGraphics; Text: string; Font: IGPFont; px: Single = 0; py: Single = 0): Single;
function GPTextHeightF(gr: IGPGraphics; Text: string; Font: IGPFont; px: Single = 0; py: Single = 0): Single;
{$ENDIF}

function GPHatchStyleToStrID(const HatchStyle: TIGPHatchStyle): string;
function GPHatchStyleToDisplayName(const HatchStyle: TIGPHatchStyle): string;
function GPTryStrIDToHatchStyle(const StrID: string; var hs: TIGPHatchStyle): Boolean;

function GPGetAllHatchStylesArray: TGPHatchStyleDynArray;



implementation

function GPGetAllHatchStylesArray: TGPHatchStyleDynArray;
var
  i: integer;
begin
  SetLength(Result, GPHatchStyleTotal);
  for i := 0 to High(Result) do
    Result[i] := TIGPHatchStyle(i);
end;

function GPHatchStyleToStrID(const HatchStyle: TIGPHatchStyle): string;
var
  s: string;
begin
  case HatchStyle of
    HatchStyleHorizontal: s := 'Horizontal';
    HatchStyleVertical: s := 'Vertical';
    HatchStyleForwardDiagonal: s := 'ForwardDiagonal';
    HatchStyleBackwardDiagonal: s := 'BackwardDiagonal';
    HatchStyleCross: s := 'Cross';
    HatchStyleDiagonalCross: s := 'DiagonalCross';
    HatchStyle05Percent: s := '05Percent';
    HatchStyle10Percent: s := '10Percent';
    HatchStyle20Percent: s := '20Percent';
    HatchStyle25Percent: s := '25Percent';
    HatchStyle30Percent: s := '30Percent';
    HatchStyle40Percent: s := '40Percent';
    HatchStyle50Percent: s := '50Percent';
    HatchStyle60Percent: s := '60Percent';
    HatchStyle70Percent: s := '70Percent';
    HatchStyle75Percent: s := '75Percent';
    HatchStyle80Percent: s := '80Percent';
    HatchStyle90Percent: s := '90Percent';
    HatchStyleLightDownwardDiagonal: s := 'LightDownwardDiagonal';
    HatchStyleLightUpwardDiagonal: s:= 'LightUpwardDiagonal';
    HatchStyleDarkDownwardDiagonal: s := 'DarkDownwardDiagonal';
    HatchStyleDarkUpwardDiagonal: s := 'DarkUpwardDiagonal';
    HatchStyleWideDownwardDiagonal: s := 'WideDownwardDiagonal';
    HatchStyleWideUpwardDiagonal: s := 'WideUpwardDiagonal';
    HatchStyleLightVertical: s := 'LightVertical';
    HatchStyleLightHorizontal: s := 'LightHorizontal';
    HatchStyleNarrowVertical: s := 'NarrowVertical';
    HatchStyleNarrowHorizontal: s := 'NarrowHorizontal';
    HatchStyleDarkVertical: s := 'DarkVertical';
    HatchStyleDarkHorizontal: s := 'DarkHorizontal';
    HatchStyleDashedDownwardDiagonal: s := 'DashedDownwardDiagonal';
    HatchStyleDashedUpwardDiagonal: s := 'DashedUpwardDiagonal';
    HatchStyleDashedHorizontal: s := 'DashedHorizontal';
    HatchStyleDashedVertical: s := 'DashedVertical';
    HatchStyleSmallConfetti: s := 'SmallConfetti';
    HatchStyleLargeConfetti: s := 'LargeConfetti';
    HatchStyleZigZag: s := 'ZigZag';
    HatchStyleWave: s := 'Wave';
    HatchStyleDiagonalBrick: s := 'DiagonalBrick';
    HatchStyleHorizontalBrick: s := 'HorizontalBrick';
    HatchStyleWeave: s := 'Weave';
    HatchStylePlaid: s := 'Plaid';
    HatchStyleDivot: s := 'Divot';
    HatchStyleDottedGrid: s := 'DottedGrid';
    HatchStyleDottedDiamond: s := 'DottedDiamond';
    HatchStyleShingle: s := 'Shingle';
    HatchStyleTrellis: s := 'Trellis';
    HatchStyleSphere: s := 'Sphere';
    HatchStyleSmallGrid: s := 'SmallGrid';
    HatchStyleSmallCheckerBoard: s := 'SmallCheckerBoard';
    HatchStyleLargeCheckerBoard: s := 'LargeCheckerBoard';
    HatchStyleOutlinedDiamond: s := 'OutlinedDiamond';
    HatchStyleSolidDiamond: s := 'SolidDiamond';
  end;
  Result := s;
end;

function GPHatchStyleToDisplayName(const HatchStyle: TIGPHatchStyle): string;
var
  s: string;
begin
  case HatchStyle of
    HatchStyleHorizontal: s := 'Horizontal';
    HatchStyleVertical: s := 'Vertical';
    HatchStyleForwardDiagonal: s := 'Forward Diagonal';
    HatchStyleBackwardDiagonal: s := 'Backward Diagonal';
    HatchStyleCross: s := 'Cross';
    HatchStyleDiagonalCross: s := 'Diagonal Cross';
    HatchStyle05Percent: s := '5%';
    HatchStyle10Percent: s := '10%';
    HatchStyle20Percent: s := '20%';
    HatchStyle25Percent: s := '25%';
    HatchStyle30Percent: s := '30%';
    HatchStyle40Percent: s := '40%';
    HatchStyle50Percent: s := '50%';
    HatchStyle60Percent: s := '60%';
    HatchStyle70Percent: s := '70%';
    HatchStyle75Percent: s := '75%';
    HatchStyle80Percent: s := '80%';
    HatchStyle90Percent: s := '90%';
    HatchStyleLightDownwardDiagonal: s := 'Light Downward Diagonal';
    HatchStyleLightUpwardDiagonal: s:= 'Light Upward Diagonal';
    HatchStyleDarkDownwardDiagonal: s := 'Dark Downward Diagonal';
    HatchStyleDarkUpwardDiagonal: s := 'Dark Upward Diagonal';
    HatchStyleWideDownwardDiagonal: s := 'Wide Downward Diagonal';
    HatchStyleWideUpwardDiagonal: s := 'Wide Upward Diagonal';
    HatchStyleLightVertical: s := 'Light Vertical';
    HatchStyleLightHorizontal: s := 'Light Horizontal';
    HatchStyleNarrowVertical: s := 'Narrow Vertical';
    HatchStyleNarrowHorizontal: s := 'Narrow Horizontal';
    HatchStyleDarkVertical: s := 'Dark Vertical';
    HatchStyleDarkHorizontal: s := 'Dark Horizontal';
    HatchStyleDashedDownwardDiagonal: s := 'Dashed Downward Diagonal';
    HatchStyleDashedUpwardDiagonal: s := 'Dashed Upward Diagonal';
    HatchStyleDashedHorizontal: s := 'Dashed Horizontal';
    HatchStyleDashedVertical: s := 'Dashed Vertical';
    HatchStyleSmallConfetti: s := 'Small Confetti';
    HatchStyleLargeConfetti: s := 'Large Confetti';
    HatchStyleZigZag: s := 'Zig Zag';
    HatchStyleWave: s := 'Wave';
    HatchStyleDiagonalBrick: s := 'Diagonal Brick';
    HatchStyleHorizontalBrick: s := 'Horizontal Brick';
    HatchStyleWeave: s := 'Weave';
    HatchStylePlaid: s := 'Plaid';
    HatchStyleDivot: s := 'Divot';
    HatchStyleDottedGrid: s := 'Dotted Grid';
    HatchStyleDottedDiamond: s := 'Dotted Diamond';
    HatchStyleShingle: s := 'Shingle';
    HatchStyleTrellis: s := 'Trellis';
    HatchStyleSphere: s := 'Sphere';
    HatchStyleSmallGrid: s := 'Small Grid';
    HatchStyleSmallCheckerBoard: s := 'Small Checker Board';
    HatchStyleLargeCheckerBoard: s := 'Large Checker Board';
    HatchStyleOutlinedDiamond: s := 'Outlined Diamond';
    HatchStyleSolidDiamond: s := 'Solid Diamond';
  end;
  Result := s;
end;


function GPTryStrIDToHatchStyle(const StrID: string; var hs: TIGPHatchStyle): Boolean;
var
  s: string;

  function TrimFromStart(const s: string; const StringToCut: string): string;
  begin
    if Copy(s, 1, Length(StringToCut)) = StringToCut then Result := Copy(s, Length(StringToCut) + 1, Length(s))
    else Result := s;
  end;

begin
  Result := False;
  s := StrID;
  s := TrimFromStart(s, 'HatchStyle');
  s := Trim(LowerCase(s));

  if s = 'horizontal' then hs := HatchStyleHorizontal
  else if s = 'vertical' then hs := HatchStyleVertical
  else if s = 'forwarddiagonal' then hs := HatchStyleForwardDiagonal
  else if s = 'backwarddiagonal' then hs := HatchStyleBackwardDiagonal
  else if s = 'cross' then hs := HatchStyleCross
  else if s = 'diagonalcross' then hs := HatchStyleDiagonalCross
  else if s = '05percent' then hs := HatchStyle05Percent
  else if s = '10percent' then hs := HatchStyle10Percent
  else if s = '20percent' then hs := HatchStyle20Percent
  else if s = '25percent' then hs := HatchStyle25Percent
  else if s = '30percent' then hs := HatchStyle30Percent
  else if s = '40percent' then hs := HatchStyle40Percent
  else if s = '50percent' then hs := HatchStyle50Percent
  else if s = '60percent' then hs := HatchStyle60Percent
  else if s = '70percent' then hs := HatchStyle70Percent
  else if s = '75percent' then hs := HatchStyle75Percent
  else if s = '80percent' then hs := HatchStyle80Percent
  else if s = '90percent' then hs := HatchStyle90Percent
  else if s = 'lightdownwarddiagonal' then hs := HatchStyleLightDownwardDiagonal
  else if s = 'lightupwarddiagonal' then hs := HatchStyleLightUpwardDiagonal
  else if s = 'darkdownwarddiagonal' then hs := HatchStyleDarkDownwardDiagonal
  else if s = 'darkupwarddiagonal' then hs := HatchStyleDarkUpwardDiagonal
  else if s = 'widedownwarddiagonal' then hs := HatchStyleWideDownwardDiagonal
  else if s = 'wideupwarddiagonal' then hs := HatchStyleWideUpwardDiagonal
  else if s = 'lightvertical' then hs := HatchStyleLightVertical
  else if s = 'lighthorizontal' then hs := HatchStyleLightHorizontal
  else if s = 'narrowvertical' then hs := HatchStyleNarrowVertical
  else if s = 'narrowhorizontal' then hs := HatchStyleNarrowHorizontal
  else if s = 'darkvertical' then hs := HatchStyleDarkVertical
  else if s = 'darkhorizontal' then hs := HatchStyleDarkHorizontal
  else if s = 'dasheddownwarddiagonal' then hs := HatchStyleDashedDownwardDiagonal
  else if s = 'dashedupwarddiagonal' then hs := HatchStyleDashedUpwardDiagonal
  else if s = 'dashedhorizontal' then hs := HatchStyleDashedHorizontal
  else if s = 'dashedvertical' then hs := HatchStyleDashedVertical
  else if s = 'smallconfetti' then hs := HatchStyleSmallConfetti
  else if s = 'largeconfetti' then hs := HatchStyleLargeConfetti
  else if s = 'zigzag' then hs := HatchStyleZigZag
  else if s = 'wave' then hs := HatchStyleWave
  else if s = 'diagonalbrick' then hs := HatchStyleDiagonalBrick
  else if s = 'horizontalbrick' then hs := HatchStyleHorizontalBrick
  else if s = 'weave' then hs := HatchStyleWeave
  else if s = 'plaid' then hs := HatchStylePlaid
  else if s = 'divot' then hs := HatchStyleDivot
  else if s = 'dottedgrid' then hs := HatchStyleDottedGrid
  else if s = 'dotteddiamond' then hs := HatchStyleDottedDiamond
  else if s = 'shingle' then hs := HatchStyleShingle
  else if s = 'trellis' then hs := HatchStyleTrellis
  else if s = 'sphere' then hs := HatchStyleSphere
  else if s = 'smallgrid' then hs := HatchStyleSmallGrid
  else if s = 'smallcheckerboard' then hs := HatchStyleSmallCheckerBoard
  else if s = 'largecheckerboard' then hs := HatchStyleLargeCheckerBoard
  else if s = 'outlineddiamond' then hs := HatchStyleOutlinedDiamond
  else if s = 'soliddiamond' then hs := HatchStyleSolidDiamond
  else Exit;

  Result := True;
end;




function GPPointF(const ax, ay: Single): TPointF;
begin
  Result.x := ax;
  Result.y := ay;
end;

function GPPointF(const apt: TPoint): TPointF;
begin
  Result.x := apt.X;
  Result.y := apt.Y;
end;

function GPPointFMove(const Point: TPointF; const dx, dy: Single): TPointF;
begin
  Result.X := Point.X + dx;
  Result.Y := Point.Y + dy;
end;

function GPColor(const AColor: TColor; Alpha: Byte): TAlphaColor;
begin
  Result := GPMakeColor(Alpha, AColor);
end;

{$IFDEF MSWINDOWS}
function GPTextWidthF(gr: IGPGraphics; Text: string; Font: IGPFont; px: Single = 0; py: Single = 0): Single;
var
  RectF: TIGPRectF;
begin
  RectF := gr.GetStringBoundingBoxF(WideString(Text), Font, GPPointF(px, py));
  Result := RectF.Width;
end;

function GPTextHeightF(gr: IGPGraphics; Text: string; Font: IGPFont; px: Single = 0; py: Single = 0): Single;
var
  RectF: TIGPRectF;
begin
  RectF := gr.GetStringBoundingBoxF(WideString(Text), Font, GPPointF(px, py));
  Result := RectF.Height;
end;
{$ENDIF}


{$IFDEF FPC}
class function TPointFHelper.Create(const ax, ay: Single): TPointF;
begin
  Result.x := ax;
  Result.y := ay;
end;

class function TPointFHelper.Create(const apt: TPoint): TPointF;
begin
  Result.x := apt.X;
  Result.y := apt.Y;
end;

{$ENDIF}


end.

