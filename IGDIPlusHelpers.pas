unit IGDIPlusHelpers;

{$IFDEF FPC}
  {$mode delphi}
  {$I IGDIPlusAPI_FPC}
{$ENDIF}
{$IFDEF DCC}{$DEFINE HAS_SYSTEM_UITYPES}{$ENDIF}

interface

uses
  Classes, SysUtils, Graphics, Types, IGDIPlus {$IFDEF HAS_SYSTEM_UITYPES},System.UITypes{$ENDIF};


{$IFDEF FPC}
type

  TPointFHelper = record helper for Types.TPointF
  public
    class function Create(const ax, ay: Single): TPointF; overload; static; inline;
    class function Create(const apt: TPoint): TPointF; overload; static; inline;
  end;
{$ENDIF}


  function GPPointF(const ax, ay: Single): TPointF; overload;
  function GPPointF(const apt: TPoint): TPointF; overload;
  function GPPointFMove(const Point: TPointF; const dx, dy: Single): TPointF;

  function GPColor(const AColor: TColor; Alpha: Byte = 255): TAlphaColor;
  function GPTextWidthF(gr: IGPGraphics; Text: string; Font: IGPFont; px: Single = 0; py: Single = 0): Single;
  function GPTextHeightF(gr: IGPGraphics; Text: string; Font: IGPFont; px: Single = 0; py: Single = 0): Single;



implementation



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

