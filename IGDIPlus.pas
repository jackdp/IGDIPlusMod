{******************************************************************************

              Copyright (C) 2008-2018 by Boian Mitov
              mitov@mitov.com
              www.mitov.com
              www.igdiplus.org
              www.openwire.org

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

******************************************************************************}

{
  IGDIPlus Wiki: http://www.mitov.com/wiki/index.php?title=Unit_IGDIPlus

  Jacek Pazera
  http://www.pazera-software.com

  My mods:

  2020.01.15
    FPC (Lazarus & CodeTyphon) support.
    Required FPC ver. 3.0.2 or newer!
    Tested with FPC ver. 3.3.1 (trunk), 3.1.1, 3.0.4, 3.0.2. FPC 3.0.0 - fail!

  2020.01.23
    Compatibility with Delphi XE2 and XE3.

}

//##HIDDENUNIT##

{$IFNDEF EMBED_IGDI_PLUS}
unit IGDIPlus;
{$ENDIF}

{$IFDEF FPC}
  {$mode Delphi}
  {$I IGDIPlusAPI_FPC.inc}
{$ENDIF}

{$IFDEF DCC}
  {$I IGDIPlusAPI_DCC.inc}
{$ENDIF}


{$ALIGN ON}
{$MINENUMSIZE 4}

{$IFDEF DCC}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

{$IFNDEF MSWINDOWS}
  {$DEFINE PURE_FMX}
{$ENDIF}

interface
uses
{$IFDEF MSWINDOWS}
  Windows, ActiveX,
{$ENDIF}
{$IFDEF DCC}
  System.UITypes, Classes, SysUtils, System.Types;
{$ELSE}
  Types,
  {$IFDEF HAS_SYSTEM_UITYPES} System.UITypes,{$ENDIF} // <-- FPC ver. 3.2 and UP (fpcsrc: packages\rtl-objpas\src\inc\system.uitypes.pp)
  Classes, SysUtils, Graphics, LCLType, LCLIntf;
{$ENDIF}


{$IFDEF FPC}

{$IFNDEF HAS_SYSTEM_UITYPES}
type
  TAlphaColor = Cardinal;
  PAlphaColor = ^TAlphaColor;
{$ENDIF}


// from GDIPAPI.pas ( by hgourvest(at)progdigy.com )
const
  { Metafile Functions }
  META_SETBKCOLOR = 513;
  META_SETBKMODE = 258;
  META_SETMAPMODE = 259;
  META_SETROP2 = 260;
  META_SETRELABS = 261;
  META_SETPOLYFILLMODE = 262;
  META_SETSTRETCHBLTMODE = 263;
  META_SETTEXTCHAREXTRA = 264;
  META_SETTEXTCOLOR = 521;
  META_SETTEXTJUSTIFICATION = 522;
  META_SETWINDOWORG = 523;
  META_SETWINDOWEXT = 524;
  META_SETVIEWPORTORG = 525;
  META_SETVIEWPORTEXT = 526;
  META_OFFSETWINDOWORG = 527;
  META_SCALEWINDOWEXT = 1040;
  META_OFFSETVIEWPORTORG = 529;
  META_SCALEVIEWPORTEXT = 1042;
  META_LINETO = 531;
  META_MOVETO = 532;
  META_EXCLUDECLIPRECT = 1045;
  META_INTERSECTCLIPRECT = 1046;
  META_ARC = 2071;
  META_ELLIPSE = 1048;
  META_FLOODFILL = 1049;
  META_PIE = 2074;
  META_RECTANGLE = 1051;
  META_ROUNDRECT = 1564;
  META_PATBLT = 1565;
  META_SAVEDC = 30;
  META_SETPIXEL = 1055;
  META_OFFSETCLIPRGN = 544;
  META_TEXTOUT = 1313;
  META_BITBLT = 2338;
  META_STRETCHBLT = 2851;
  META_POLYGON = 804;
  META_POLYLINE = 805;
  META_ESCAPE = 1574;
  META_RESTOREDC = 295;
  META_FILLREGION = 552;
  META_FRAMEREGION = 1065;
  META_INVERTREGION = 298;
  META_PAINTREGION = 299;
  META_SELECTCLIPREGION = 300;
  META_SELECTOBJECT = 301;
  META_SETTEXTALIGN = 302;
  META_CHORD = 2096;
  META_SETMAPPERFLAGS = 561;
  META_EXTTEXTOUT = 2610;
  META_SETDIBTODEV = 3379;
  META_SELECTPALETTE = 564;
  META_REALIZEPALETTE = 53;
  META_ANIMATEPALETTE = 1078;
  META_SETPALENTRIES = 55;
  META_POLYPOLYGON = 1336;
  META_RESIZEPALETTE = 313;
  META_DIBBITBLT = 2368;
  META_DIBSTRETCHBLT = 2881;
  META_DIBCREATEPATTERNBRUSH = 322;
  META_STRETCHDIB = 3907;
  META_EXTFLOODFILL = 1352;
  META_DELETEOBJECT = 496;
  META_CREATEPALETTE = 247;
  META_CREATEPATTERNBRUSH = 505;
  META_CREATEPENINDIRECT = 762;
  META_CREATEFONTINDIRECT = 763;
  META_CREATEBRUSHINDIRECT = 764;
  META_CREATEREGION = 1791;

  { Enhanced metafile record types.}
  EMR_HEADER = 1;
  EMR_POLYBEZIER = 2;
  EMR_POLYGON = 3;
  EMR_POLYLINE = 4;
  EMR_POLYBEZIERTO = 5;
  EMR_POLYLINETO = 6;
  EMR_POLYPOLYLINE = 7;
  EMR_POLYPOLYGON = 8;
  EMR_SETWINDOWEXTEX = 9;
  EMR_SETWINDOWORGEX = 10;
  EMR_SETVIEWPORTEXTEX = 11;
  EMR_SETVIEWPORTORGEX = 12;
  EMR_SETBRUSHORGEX = 13;
  EMR_EOF = 14;
  EMR_SETPIXELV = 15;
  EMR_SETMAPPERFLAGS = $10;
  EMR_SETMAPMODE = 17;
  EMR_SETBKMODE = 18;
  EMR_SETPOLYFILLMODE = 19;
  EMR_SETROP2 = 20;
  EMR_SETSTRETCHBLTMODE = 21;
  EMR_SETTEXTALIGN = 22;
  EMR_SETCOLORADJUSTMENT = 23;
  EMR_SETTEXTCOLOR = 24;
  EMR_SETBKCOLOR = 25;
  EMR_OFFSETCLIPRGN = 26;
  EMR_MOVETOEX = 27;
  EMR_SETMETARGN = 28;
  EMR_EXCLUDECLIPRECT = 29;
  EMR_INTERSECTCLIPRECT = 30;
  EMR_SCALEVIEWPORTEXTEX = 31;
  EMR_SCALEWINDOWEXTEX = 32;
  EMR_SAVEDC = 33;
  EMR_RESTOREDC = 34;
  EMR_SETWORLDTRANSFORM = 35;
  EMR_MODIFYWORLDTRANSFORM = 36;
  EMR_SELECTOBJECT = 37;
  EMR_CREATEPEN = 38;
  EMR_CREATEBRUSHINDIRECT = 39;
  EMR_DELETEOBJECT = 40;
  EMR_ANGLEARC = 41;
  EMR_ELLIPSE = 42;
  EMR_RECTANGLE = 43;
  EMR_ROUNDRECT = 44;
  EMR_ARC = 45;
  EMR_CHORD = 46;
  EMR_PIE = 47;
  EMR_SELECTPALETTE = 48;
  EMR_CREATEPALETTE = 49;
  EMR_SETPALETTEENTRIES = 50;
  EMR_RESIZEPALETTE = 51;
  EMR_REALIZEPALETTE = 52;
  EMR_EXTFLOODFILL = 53;
  EMR_LINETO = 54;
  EMR_ARCTO = 55;
  EMR_POLYDRAW = 56;
  EMR_SETARCDIRECTION = 57;
  EMR_SETMITERLIMIT = 58;
  EMR_BEGINPATH = 59;
  EMR_ENDPATH = 60;
  EMR_CLOSEFIGURE = 61;
  EMR_FILLPATH = 62;
  EMR_STROKEANDFILLPATH = 63;
  EMR_STROKEPATH = $40;
  EMR_FLATTENPATH = 65;
  EMR_WIDENPATH = 66;
  EMR_SELECTCLIPPATH = 67;
  EMR_ABORTPATH = 68;

  EMR_GDICOMMENT = 70;
  EMR_FILLRGN = 71;
  EMR_FRAMERGN = 72;
  EMR_INVERTRGN = 73;
  EMR_PAINTRGN = 74;
  EMR_EXTSELECTCLIPRGN = 75;
  EMR_BITBLT = 76;
  EMR_STRETCHBLT = 77;
  EMR_MASKBLT = 78;
  EMR_PLGBLT = 79;
  EMR_SETDIBITSTODEVICE = 80;
  EMR_STRETCHDIBITS = 81;
  EMR_EXTCREATEFONTINDIRECTW = 82;
  EMR_EXTTEXTOUTA = 83;
  EMR_EXTTEXTOUTW = 84;
  EMR_POLYBEZIER16 = 85;
  EMR_POLYGON16 = 86;
  EMR_POLYLINE16 = 87;
  EMR_POLYBEZIERTO16 = 88;
  EMR_POLYLINETO16 = 89;
  EMR_POLYPOLYLINE16 = 90;
  EMR_POLYPOLYGON16 = 91;
  EMR_POLYDRAW16 = 92;
  EMR_CREATEMONOBRUSH = 93;
  EMR_CREATEDIBPATTERNBRUSHPT = 94;
  EMR_EXTCREATEPEN = 95;
  EMR_POLYTEXTOUTA = 96;
  EMR_POLYTEXTOUTW = 97;
  EMR_SETICMMODE = 98;
  EMR_CREATECOLORSPACE = 99;
  EMR_SETCOLORSPACE = 100;
  EMR_DELETECOLORSPACE = 101;
  EMR_GLSRECORD = 102;
  EMR_GLSBOUNDEDRECORD = 103;
  EMR_PIXELFORMAT = 104;
  EMR_DRAWESCAPE = 105;
  EMR_EXTESCAPE = 106;
  EMR_STARTDOC = 107;
  EMR_SMALLTEXTOUT = 108;
  EMR_FORCEUFIMAPPING = 109;
  EMR_NAMEDESCAPE = 110;
  EMR_COLORCORRECTPALETTE = 111;
  EMR_SETICMPROFILEA = 112;
  EMR_SETICMPROFILEW = 113;
  EMR_ALPHABLEND = 114;
  EMR_ALPHADIBBLEND = 115;
  EMR_TRANSPARENTBLT = 116;
  EMR_TRANSPARENTDIB = 117;
  EMR_GRADIENTFILL = 118;
  EMR_SETLINKEDUFIS = 119;
  EMR_SETTEXTJUSTIFICATION = 120;
  EMR_MAX = 120;
  EMR_MIN = 1;
{$ENDIF} // FPC

type
  PUINT16 = ^UINT16;
  {$IFDEF FPC}PInteger = objpas.PInteger;{$ENDIF} // jacek
{$IFNDEF MSWINDOWS}
  UINT = Cardinal;
  ULONG = Cardinal;
  PROPID = ULONG;
  TPropID = PROPID;
  TCLSID = TGUID;
  PMetaHeader = Pointer;
  HRGN = Integer;
  LANGID = Integer;
  TLogFontA = Integer;
  PLogFontA = ^TLogFontA;
  TLogFontW = Integer;
  PLogFontW = ^TLogFontW;
  HDC = Integer;
  HWND = Integer;
  HFONT = Integer;
  HBITMAP = Integer;
  HPALETTE = Integer;
  HENHMETAFILE = Integer;
  HICON = Integer;
  BOOL = Integer;
  WideString = String;

  TBITMAPINFO = record
  end;

  IStream = interface
  end;

  const
    LANG_NEUTRAL = 0;
    MM_ANISOTROPIC = 0;

{$ENDIF} //MSWINDOWS

type TIGPColorNamePair = record
  Color : TAlphaColor;
  Name  : String;
end;

const GPRGBAColorNames : array [ 0..140 ] of TIGPColorNamePair =
(
  (Color:$FFF0F8FF; Name:'aclAliceBlue' ),
  (Color:$FFFAEBD7; Name:'aclAntiqueWhite' ),
  (Color:$FF00FFFF; Name:'aclAqua' ),
  (Color:$FF7FFFD4; Name:'aclAquamarine' ),
  (Color:$FFF0FFFF; Name:'aclAzure' ),
  (Color:$FFF5F5DC; Name:'aclBeige' ),
  (Color:$FFFFE4C4; Name:'aclBisque' ),
  (Color:$FF000000; Name:'aclBlack' ),
  (Color:$FFFFEBCD; Name:'aclBlanchedAlmond' ),
  (Color:$FF0000FF; Name:'aclBlue' ),
  (Color:$FF8A2BE2; Name:'aclBlueViolet' ),
  (Color:$FFA52A2A; Name:'aclBrown' ),
  (Color:$FFDEB887; Name:'aclBurlyWood' ),
  (Color:$FF5F9EA0; Name:'aclCadetBlue' ),
  (Color:$FF7FFF00; Name:'aclChartreuse' ),
  (Color:$FFD2691E; Name:'aclChocolate' ),
  (Color:$FFFF7F50; Name:'aclCoral' ),
  (Color:$FF6495ED; Name:'aclCornflowerBlue' ),
  (Color:$FFFFF8DC; Name:'aclCornsilk' ),
  (Color:$FFDC143C; Name:'aclCrimson' ),
  (Color:$FF00FFFF; Name:'aclCyan' ),
  (Color:$FF00008B; Name:'aclDarkBlue' ),
  (Color:$FF008B8B; Name:'aclDarkCyan' ),
  (Color:$FFB8860B; Name:'aclDarkGoldenrod' ),
  (Color:$FFA9A9A9; Name:'aclDarkGray' ),
  (Color:$FF006400; Name:'aclDarkGreen' ),
  (Color:$FFBDB76B; Name:'aclDarkKhaki' ),
  (Color:$FF8B008B; Name:'aclDarkMagenta' ),
  (Color:$FF556B2F; Name:'aclDarkOliveGreen' ),
  (Color:$FFFF8C00; Name:'aclDarkOrange' ),
  (Color:$FF9932CC; Name:'aclDarkOrchid' ),
  (Color:$FF8B0000; Name:'aclDarkRed' ),
  (Color:$FFE9967A; Name:'aclDarkSalmon' ),
  (Color:$FF8FBC8B; Name:'aclDarkSeaGreen' ),
  (Color:$FF483D8B; Name:'aclDarkSlateBlue' ),
  (Color:$FF2F4F4F; Name:'aclDarkSlateGray' ),
  (Color:$FF00CED1; Name:'aclDarkTurquoise' ),
  (Color:$FF9400D3; Name:'aclDarkViolet' ),
  (Color:$FFFF1493; Name:'aclDeepPink' ),
  (Color:$FF00BFFF; Name:'aclDeepSkyBlue' ),
  (Color:$FF696969; Name:'aclDimGray' ),
  (Color:$FF1E90FF; Name:'aclDodgerBlue' ),
  (Color:$FFB22222; Name:'aclFirebrick' ),
  (Color:$FFFFFAF0; Name:'aclFloralWhite' ),
  (Color:$FF228B22; Name:'aclForestGreen' ),
  (Color:$FFFF00FF; Name:'aclFuchsia' ),
  (Color:$FFDCDCDC; Name:'aclGainsboro' ),
  (Color:$FFF8F8FF; Name:'aclGhostWhite' ),
  (Color:$FFFFD700; Name:'aclGold' ),
  (Color:$FFDAA520; Name:'aclGoldenrod' ),
  (Color:$FF808080; Name:'aclGray' ),
  (Color:$FF008000; Name:'aclGreen' ),
  (Color:$FFADFF2F; Name:'aclGreenYellow' ),
  (Color:$FFF0FFF0; Name:'aclHoneydew' ),
  (Color:$FFFF69B4; Name:'aclHotPink' ),
  (Color:$FFCD5C5C; Name:'aclIndianRed' ),
  (Color:$FF4B0082; Name:'aclIndigo' ),
  (Color:$FFFFFFF0; Name:'aclIvory' ),
  (Color:$FFF0E68C; Name:'aclKhaki' ),
  (Color:$FFE6E6FA; Name:'aclLavender' ),
  (Color:$FFFFF0F5; Name:'aclLavenderBlush' ),
  (Color:$FF7CFC00; Name:'aclLawnGreen' ),
  (Color:$FFFFFACD; Name:'aclLemonChiffon' ),
  (Color:$FFADD8E6; Name:'aclLightBlue' ),
  (Color:$FFF08080; Name:'aclLightCoral' ),
  (Color:$FFE0FFFF; Name:'aclLightCyan' ),
  (Color:$FFFAFAD2; Name:'aclLightGoldenrodYellow' ),
  (Color:$FFD3D3D3; Name:'aclLightGray' ),
  (Color:$FF90EE90; Name:'aclLightGreen' ),
  (Color:$FFFFB6C1; Name:'aclLightPink' ),
  (Color:$FFFFA07A; Name:'aclLightSalmon' ),
  (Color:$FF20B2AA; Name:'aclLightSeaGreen' ),
  (Color:$FF87CEFA; Name:'aclLightSkyBlue' ),
  (Color:$FF778899; Name:'aclLightSlateGray' ),
  (Color:$FFB0C4DE; Name:'aclLightSteelBlue' ),
  (Color:$FFFFFFE0; Name:'aclLightYellow' ),
  (Color:$FF00FF00; Name:'aclLime' ),
  (Color:$FF32CD32; Name:'aclLimeGreen' ),
  (Color:$FFFAF0E6; Name:'aclLinen' ),
  (Color:$FFFF00FF; Name:'aclMagenta' ),
  (Color:$FF800000; Name:'aclMaroon' ),
  (Color:$FF66CDAA; Name:'aclMediumAquamarine' ),
  (Color:$FF0000CD; Name:'aclMediumBlue' ),
  (Color:$FFBA55D3; Name:'aclMediumOrchid' ),
  (Color:$FF9370DB; Name:'aclMediumPurple' ),
  (Color:$FF3CB371; Name:'aclMediumSeaGreen' ),
  (Color:$FF7B68EE; Name:'aclMediumSlateBlue' ),
  (Color:$FF00FA9A; Name:'aclMediumSpringGreen' ),
  (Color:$FF48D1CC; Name:'aclMediumTurquoise' ),
  (Color:$FFC71585; Name:'aclMediumVioletRed' ),
  (Color:$FF191970; Name:'aclMidnightBlue' ),
  (Color:$FFF5FFFA; Name:'aclMintCream' ),
  (Color:$FFFFE4E1; Name:'aclMistyRose' ),
  (Color:$FFFFE4B5; Name:'aclMoccasin' ),
  (Color:$FFFFDEAD; Name:'aclNavajoWhite' ),
  (Color:$FF000080; Name:'aclNavy' ),
  (Color:$FFFDF5E6; Name:'aclOldLace' ),
  (Color:$FF808000; Name:'aclOlive' ),
  (Color:$FF6B8E23; Name:'aclOliveDrab' ),
  (Color:$FFFFA500; Name:'aclOrange' ),
  (Color:$FFFF4500; Name:'aclOrangeRed' ),
  (Color:$FFDA70D6; Name:'aclOrchid' ),
  (Color:$FFEEE8AA; Name:'aclPaleGoldenrod' ),
  (Color:$FF98FB98; Name:'aclPaleGreen' ),
  (Color:$FFAFEEEE; Name:'aclPaleTurquoise' ),
  (Color:$FFDB7093; Name:'aclPaleVioletRed' ),
  (Color:$FFFFEFD5; Name:'aclPapayaWhip' ),
  (Color:$FFFFDAB9; Name:'aclPeachPuff' ),
  (Color:$FFCD853F; Name:'aclPeru' ),
  (Color:$FFFFC0CB; Name:'aclPink' ),
  (Color:$FFDDA0DD; Name:'aclPlum' ),
  (Color:$FFB0E0E6; Name:'aclPowderBlue' ),
  (Color:$FF800080; Name:'aclPurple' ),
  (Color:$FFFF0000; Name:'aclRed' ),
  (Color:$FFBC8F8F; Name:'aclRosyBrown' ),
  (Color:$FF4169E1; Name:'aclRoyalBlue' ),
  (Color:$FF8B4513; Name:'aclSaddleBrown' ),
  (Color:$FFFA8072; Name:'aclSalmon' ),
  (Color:$FFF4A460; Name:'aclSandyBrown' ),
  (Color:$FF2E8B57; Name:'aclSeaGreen' ),
  (Color:$FFFFF5EE; Name:'aclSeaShell' ),
  (Color:$FFA0522D; Name:'aclSienna' ),
  (Color:$FFC0C0C0; Name:'aclSilver' ),
  (Color:$FF87CEEB; Name:'aclSkyBlue' ),
  (Color:$FF6A5ACD; Name:'aclSlateBlue' ),
  (Color:$FF708090; Name:'aclSlateGray' ),
  (Color:$FFFFFAFA; Name:'aclSnow' ),
  (Color:$FF00FF7F; Name:'aclSpringGreen' ),
  (Color:$FF4682B4; Name:'aclSteelBlue' ),
  (Color:$FFD2B48C; Name:'aclTan' ),
  (Color:$FF008080; Name:'aclTeal' ),
  (Color:$FFD8BFD8; Name:'aclThistle' ),
  (Color:$FFFF6347; Name:'aclTomato' ),
  (Color:$00FFFFFF; Name:'aclTransparent' ),
  (Color:$FF40E0D0; Name:'aclTurquoise' ),
  (Color:$FFEE82EE; Name:'aclViolet' ),
  (Color:$FFF5DEB3; Name:'aclWheat' ),
  (Color:$FFFFFFFF; Name:'aclWhite' ),
  (Color:$FFF5F5F5; Name:'aclWhiteSmoke' ),
  (Color:$FFFFFF00; Name:'aclYellow' ),
  (Color:$FF9ACD32; Name:'aclYellowGreen' )
);

{$IFDEF CPUX64}
{$HPPEMIT '#pragma link "cbgdiplus.a"'}
{$ELSE}
{$HPPEMIT '#pragma link "cbgdiplus.lib"'}
{$ENDIF}

{$HPPEMIT '__interface _di_IGPFontFamily;' }

(**************************************************************************\
*
*   GDI+ Private Memory Management APIs
*
\**************************************************************************)

(**************************************************************************\
*
*   GDI+ Enumeration Types
*
\**************************************************************************)

//--------------------------------------------------------------------------
// Default bezier flattening tolerance in device pixels.
//--------------------------------------------------------------------------

const
  {$EXTERNALSYM FlatnessDefault}
  FlatnessDefault = 0.25;

//--------------------------------------------------------------------------
// Graphics and Container State cookies
//--------------------------------------------------------------------------
type
  TIGPGraphicsState     = Cardinal;
  TIGPGraphicsContainer = Cardinal;

//--------------------------------------------------------------------------
// Fill mode constants
//--------------------------------------------------------------------------

  TIGPFillMode = (
    FillModeAlternate,        // 0
    FillModeWinding           // 1
  );

//--------------------------------------------------------------------------
// Quality mode constants
//--------------------------------------------------------------------------

  TIGPQualityMode = (
    QualityModeInvalid   = -1,
    QualityModeDefault   =  0,
    QualityModeLow       =  1, // Best performance
    QualityModeHigh      =  2  // Best rendering quality
  );

//--------------------------------------------------------------------------
// Alpha Compositing mode constants
//--------------------------------------------------------------------------
type
  TIGPCompositingMode = (
    CompositingModeSourceOver,    // 0
    CompositingModeSourceCopy     // 1
  );

//--------------------------------------------------------------------------
// Alpha Compositing quality constants
//--------------------------------------------------------------------------
  TIGPCompositingQuality = (
    CompositingQualityInvalid          = Ord(QualityModeInvalid),
    CompositingQualityDefault          = Ord(QualityModeDefault),
    CompositingQualityHighSpeed        = Ord(QualityModeLow),
    CompositingQualityHighQuality      = Ord(QualityModeHigh),
    CompositingQualityGammaCorrected,
    CompositingQualityAssumeLinear
  );

//--------------------------------------------------------------------------
// Unit constants
//--------------------------------------------------------------------------
type
  TIGPUnit = (
    UnitWorld,      // 0 -- World coordinate (non-physical unit)
    UnitDisplay,    // 1 -- Variable -- for PageTransform only
    UnitPixel,      // 2 -- Each unit is one device pixel.
    UnitPoint,      // 3 -- Each unit is a printer's point, or 1/72 inch.
    UnitInch,       // 4 -- Each unit is 1 inch.
    UnitDocument,   // 5 -- Each unit is 1/300 inch.
    UnitMillimeter  // 6 -- Each unit is 1 millimeter.
  );

//--------------------------------------------------------------------------
// MetafileFrameUnit
//
// The frameRect for creating a metafile can be specified in any of these
// units.  There is an extra frame unit value (MetafileFrameUnitGdi) so
// that units can be supplied in the same units that GDI expects for
// frame rects -- these units are in .01 (1/100ths) millimeter units
// as defined by GDI.
//--------------------------------------------------------------------------
  TIGPMetafileFrameUnit = (
    MetafileFrameUnitPixel      = Ord(UnitPixel),
    MetafileFrameUnitPoint      = Ord(UnitPoint),
    MetafileFrameUnitInch       = Ord(UnitInch),
    MetafileFrameUnitDocument   = Ord(UnitDocument),
    MetafileFrameUnitMillimeter = Ord(UnitMillimeter),
    MetafileFrameUnitGdi        // GDI compatible .01 MM units
  );
//--------------------------------------------------------------------------
// Coordinate space identifiers
//--------------------------------------------------------------------------

  TIGPCoordinateSpace = (
    CoordinateSpaceWorld,     // 0
    CoordinateSpacePage,      // 1
    CoordinateSpaceDevice     // 2
  );

//--------------------------------------------------------------------------
// Various wrap modes for brushes
//--------------------------------------------------------------------------

  TIGPWrapMode = (
    WrapModeTile,        // 0
    WrapModeTileFlipX,   // 1
    WrapModeTileFlipY,   // 2
    WrapModeTileFlipXY,  // 3
    WrapModeClamp        // 4
  );

//--------------------------------------------------------------------------
// Various hatch styles
//--------------------------------------------------------------------------

  TIGPHatchStyle = (
    HatchStyleHorizontal,                  // = 0,
    HatchStyleVertical,                    // = 1,
    HatchStyleForwardDiagonal,             // = 2,
    HatchStyleBackwardDiagonal,            // = 3,
    HatchStyleCross,                       // = 4,
    HatchStyleDiagonalCross,               // = 5,
    HatchStyle05Percent,                   // = 6,
    HatchStyle10Percent,                   // = 7,
    HatchStyle20Percent,                   // = 8,
    HatchStyle25Percent,                   // = 9,
    HatchStyle30Percent,                   // = 10,
    HatchStyle40Percent,                   // = 11,
    HatchStyle50Percent,                   // = 12,
    HatchStyle60Percent,                   // = 13,
    HatchStyle70Percent,                   // = 14,
    HatchStyle75Percent,                   // = 15,
    HatchStyle80Percent,                   // = 16,
    HatchStyle90Percent,                   // = 17,
    HatchStyleLightDownwardDiagonal,       // = 18,
    HatchStyleLightUpwardDiagonal,         // = 19,
    HatchStyleDarkDownwardDiagonal,        // = 20,
    HatchStyleDarkUpwardDiagonal,          // = 21,
    HatchStyleWideDownwardDiagonal,        // = 22,
    HatchStyleWideUpwardDiagonal,          // = 23,
    HatchStyleLightVertical,               // = 24,
    HatchStyleLightHorizontal,             // = 25,
    HatchStyleNarrowVertical,              // = 26,
    HatchStyleNarrowHorizontal,            // = 27,
    HatchStyleDarkVertical,                // = 28,
    HatchStyleDarkHorizontal,              // = 29,
    HatchStyleDashedDownwardDiagonal,      // = 30,
    HatchStyleDashedUpwardDiagonal,        // = 31,
    HatchStyleDashedHorizontal,            // = 32,
    HatchStyleDashedVertical,              // = 33,
    HatchStyleSmallConfetti,               // = 34,
    HatchStyleLargeConfetti,               // = 35,
    HatchStyleZigZag,                      // = 36,
    HatchStyleWave,                        // = 37,
    HatchStyleDiagonalBrick,               // = 38,
    HatchStyleHorizontalBrick,             // = 39,
    HatchStyleWeave,                       // = 40,
    HatchStylePlaid,                       // = 41,
    HatchStyleDivot,                       // = 42,
    HatchStyleDottedGrid,                  // = 43,
    HatchStyleDottedDiamond,               // = 44,
    HatchStyleShingle,                     // = 45,
    HatchStyleTrellis,                     // = 46,
    HatchStyleSphere,                      // = 47,
    HatchStyleSmallGrid,                   // = 48,
    HatchStyleSmallCheckerBoard,           // = 49,
    HatchStyleLargeCheckerBoard,           // = 50,
    HatchStyleOutlinedDiamond,             // = 51,
    HatchStyleSolidDiamond                 // = 52,
  );
  
  const
    GPHatchStyleTotal = 53;

  const
    GPHatchStyleLargeGrid = HatchStyleCross; // 4
    GPHatchStyleMin       = HatchStyleHorizontal;
    GPHatchStyleMax       = HatchStyleSolidDiamond;

//--------------------------------------------------------------------------
// Dash style constants
//--------------------------------------------------------------------------

type
  TIGPDashStyle = (
    DashStyleSolid,          // 0
    DashStyleDash,           // 1
    DashStyleDot,            // 2
    DashStyleDashDot,        // 3
    DashStyleDashDotDot,     // 4
    DashStyleCustom          // 5
  );

//--------------------------------------------------------------------------
// Dash cap constants
//--------------------------------------------------------------------------
  TIGPDashCap = (
    DashCapFlat             = 0,
    DashCapRound            = 2,
    DashCapTriangle         = 3
  );

//--------------------------------------------------------------------------
// Line cap constants (only the lowest 8 bits are used).
//--------------------------------------------------------------------------
  TIGPLineCap = (
    LineCapFlat             = 0,
    LineCapSquare           = 1,
    LineCapRound            = 2,
    LineCapTriangle         = 3,

    LineCapNoAnchor         = $10, // corresponds to flat cap
    LineCapSquareAnchor     = $11, // corresponds to square cap
    LineCapRoundAnchor      = $12, // corresponds to round cap
    LineCapDiamondAnchor    = $13, // corresponds to triangle cap
    LineCapArrowAnchor      = $14, // no correspondence

    LineCapCustom           = $ff, // custom cap

    LineCapAnchorMask       = $f0  // mask to check for anchor or not.
  );

//--------------------------------------------------------------------------
// Custom Line cap type constants
//--------------------------------------------------------------------------
type
  TIGPCustomLineCapType = (
    CustomLineCapTypeDefault,
    CustomLineCapTypeAdjustableArrow
  );

//--------------------------------------------------------------------------
// Line join constants
//--------------------------------------------------------------------------

  TIGPLineJoin = (
    LineJoinMiter,
    LineJoinBevel,
    LineJoinRound,
    LineJoinMiterClipped
  );

//--------------------------------------------------------------------------
// Path point types (only the lowest 8 bits are used.)
//  The lowest 3 bits are interpreted as point type
//  The higher 5 bits are reserved for flags.
//--------------------------------------------------------------------------

  {$Z1}
  TIGPPathPointType = (
    PathPointTypeStart           = $00, // move
    PathPointTypeLine            = $01, // line
    PathPointTypeBezier          = $03, // default Bezier (= cubic Bezier)
    PathPointTypePathTypeMask    = $07, // type mask (lowest 3 bits).
    PathPointTypeDashMode        = $10, // currently in dash mode.
    PathPointTypePathMarker      = $20, // a marker for the path.
    PathPointTypeCloseSubpath    = $80, // closed flag

    // Path types used for advanced path.
    PathPointTypeBezier3         = $03  // cubic Bezier
  );
  {$Z4}

//--------------------------------------------------------------------------
// WarpMode constants
//--------------------------------------------------------------------------

  TIGPWarpMode = (
    WarpModePerspective,    // 0
    WarpModeBilinear        // 1
  );

//--------------------------------------------------------------------------
// LineGradient Mode
//--------------------------------------------------------------------------

  TIGPLinearGradientMode = (
    LinearGradientModeHorizontal,         // 0
    LinearGradientModeVertical,           // 1
    LinearGradientModeForwardDiagonal,    // 2
    LinearGradientModeBackwardDiagonal    // 3
  );

//--------------------------------------------------------------------------
// Region Comine Modes
//--------------------------------------------------------------------------

  TIGPCombineMode = (
    CombineModeReplace,     // 0
    CombineModeIntersect,   // 1
    CombineModeUnion,       // 2
    CombineModeXor,         // 3
    CombineModeExclude,     // 4
    CombineModeComplement   // 5 (Exclude From)
  );

//--------------------------------------------------------------------------
 // Image types
//--------------------------------------------------------------------------

  TIGPImageType = (
    ImageTypeUnknown,   // 0
    ImageTypeBitmap,    // 1
    ImageTypeMetafile   // 2
  );

//--------------------------------------------------------------------------
// Interpolation modes
//--------------------------------------------------------------------------
  TIGPInterpolationMode = (
    InterpolationModeInvalid          = Ord(QualityModeInvalid),
    InterpolationModeDefault          = Ord(QualityModeDefault),
    InterpolationModeLowQuality       = Ord(QualityModeLow),
    InterpolationModeHighQuality      = Ord(QualityModeHigh),
    InterpolationModeBilinear,
    InterpolationModeBicubic,
    InterpolationModeNearestNeighbor,
    InterpolationModeHighQualityBilinear,
    InterpolationModeHighQualityBicubic
  );

//--------------------------------------------------------------------------
// Pen types
//--------------------------------------------------------------------------
type
  TIGPPenAlignment = (
    PenAlignmentCenter,
    PenAlignmentInset
  );

//--------------------------------------------------------------------------
// Brush types
//--------------------------------------------------------------------------

  TIGPBrushType = (
   BrushTypeSolidColor,
   BrushTypeHatchFill,
   BrushTypeTextureFill,
   BrushTypePathGradient,
   BrushTypeLinearGradient 
  );

//--------------------------------------------------------------------------
// Pen's Fill types
//--------------------------------------------------------------------------
  TIGPPenType = (
   PenTypeSolidColor       =  Ord(BrushTypeSolidColor),
   PenTypeHatchFill        =  Ord(BrushTypeHatchFill),
   PenTypeTextureFill      =  Ord(BrushTypeTextureFill),
   PenTypePathGradient     =  Ord(BrushTypePathGradient),
   PenTypeLinearGradient   =  Ord(BrushTypeLinearGradient),
   PenTypeUnknown          = -1
  );

//--------------------------------------------------------------------------
// Matrix Order
//--------------------------------------------------------------------------
type
  TIGPMatrixOrder = (
    MatrixOrderPrepend,
    MatrixOrderAppend
  );

//--------------------------------------------------------------------------
// Generic font families
//--------------------------------------------------------------------------

  TIGPGenericFontFamily = (
    GenericFontFamilySerif,
    GenericFontFamilySansSerif,
    GenericFontFamilyMonospace
  );

//--------------------------------------------------------------------------
// FontStyle: face types and common styles
//--------------------------------------------------------------------------
type
{
  FontStyle = Integer;
  const
    FontStyleRegular    = Integer(0);
    FontStyleBold       = Integer(1);
    FontStyleItalic     = Integer(2);
    FontStyleBoldItalic = Integer(3);
    FontStyleUnderline  = Integer(4);
    FontStyleStrikeout  = Integer(8);
  Type
  TIGPFontStyle = FontStyle;
}
//---------------------------------------------------------------------------
// Smoothing Mode
//---------------------------------------------------------------------------
  TIGPSmoothingMode = (
    SmoothingModeInvalid     = Ord(QualityModeInvalid),
    SmoothingModeDefault     = Ord(QualityModeDefault),
    SmoothingModeHighSpeed   = Ord(QualityModeLow),
    SmoothingModeHighQuality = Ord(QualityModeHigh),
    SmoothingModeNone,
    SmoothingModeAntiAlias,
    SmoothingModeAntiAlias8x4 = SmoothingModeAntiAlias,
    SmoothingModeAntiAlias8x8 = 5
  );

//---------------------------------------------------------------------------
// Pixel Format Mode
//---------------------------------------------------------------------------
  TIGPPixelOffsetMode = (
    PixelOffsetModeInvalid     = Ord(QualityModeInvalid),
    PixelOffsetModeDefault     = Ord(QualityModeDefault),
    PixelOffsetModeHighSpeed   = Ord(QualityModeLow),
    PixelOffsetModeHighQuality = Ord(QualityModeHigh),
    PixelOffsetModeNone,    // No pixel offset
    PixelOffsetModeHalf     // Offset by -0.5, -0.5 for fast anti-alias perf
  );

//---------------------------------------------------------------------------
// Text Rendering Hint
//---------------------------------------------------------------------------
type
  TIGPTextRenderingHint = (
    TextRenderingHintSystemDefault,                // Glyph with system default rendering hint
    TextRenderingHintSingleBitPerPixelGridFit,     // Glyph bitmap with hinting
    TextRenderingHintSingleBitPerPixel,            // Glyph bitmap without hinting
    TextRenderingHintAntiAliasGridFit,             // Glyph anti-alias bitmap with hinting
    TextRenderingHintAntiAlias,                    // Glyph anti-alias bitmap without hinting
    TextRenderingHintClearTypeGridFit              // Glyph CT bitmap with hinting
  );

//---------------------------------------------------------------------------
// Metafile Types
//---------------------------------------------------------------------------

  TIGPMetafileType = (
    MetafileTypeInvalid,            // Invalid metafile
    MetafileTypeWmf,                // Standard WMF
    MetafileTypeWmfPlaceable,       // Placeable WMF
    MetafileTypeEmf,                // EMF (not EMF+)
    MetafileTypeEmfPlusOnly,        // EMF+ without dual, down-level records
    MetafileTypeEmfPlusDual         // EMF+ with dual, down-level records
  );

//---------------------------------------------------------------------------
// Specifies the type of EMF to record
//---------------------------------------------------------------------------
  TIGPEmfType = (
    EmfTypeEmfOnly     = Ord(MetafileTypeEmf),          // no EMF+, only EMF
    EmfTypeEmfPlusOnly = Ord(MetafileTypeEmfPlusOnly),  // no EMF, only EMF+
    EmfTypeEmfPlusDual = Ord(MetafileTypeEmfPlusDual)   // both EMF+ and EMF
  );

//---------------------------------------------------------------------------
// EMF+ Persistent object types
//---------------------------------------------------------------------------

type
  TIGPObjectType = (
    ObjectTypeInvalid,
    ObjectTypeBrush,
    ObjectTypePen,
    ObjectTypePath,
    ObjectTypeRegion,
    ObjectTypeImage,
    ObjectTypeFont,
    ObjectTypeStringFormat,
    ObjectTypeImageAttributes,
    ObjectTypeCustomLineCap
  );

const
  GPObjectTypeMax = ObjectTypeCustomLineCap;
  GPObjectTypeMin = ObjectTypeBrush;

function ObjectTypeIsValid(type_: TIGPObjectType) : Boolean;

//---------------------------------------------------------------------------
// EMF+ Records
//---------------------------------------------------------------------------

  // We have to change the WMF record numbers so that they don't conflict with
  // the EMF and EMF+ record numbers.

const
  GDIP_EMFPLUS_RECORD_BASE      = $00004000;
  {$EXTERNALSYM GDIP_EMFPLUS_RECORD_BASE}
  GDIP_WMF_RECORD_BASE          = $00010000;
  {$EXTERNALSYM GDIP_WMF_RECORD_BASE}
  
(*$HPPEMIT 'static const Shortint BCBGDIP_EMFPLUS_RECORD_BASE = 0x00004000;' *)
(*$HPPEMIT 'static const Shortint BCBGDIP_WMF_RECORD_BASE     = 0x00010000;' *)


// macros
function GP_GDIP_WMF_RECORD_TO_EMFPLUS(n: Integer) : Integer;
function GP_GDIP_EMFPLUS_RECORD_TO_WMF(n: Integer) : Integer;
function GP_GDIP_IS_WMF_RECORDTYPE(n: Integer) : Boolean;


{$EXTERNALSYM TIGPEmfPlusRecordType}

type
{$IFNDEF MSWINDOWS}
  TIGPEmfPlusRecordType = Integer;

{$ELSE}
  TIGPEmfPlusRecordType = (
   // Since we have to enumerate GDI records right along with GDI+ records,
   // We list all the GDI records here so that they can be part of the
   // same enumeration type which is used in the enumeration callback.

    WmfRecordTypeSetBkColor              = META_SETBKCOLOR or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetBkMode               = META_SETBKMODE or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetMapMode              = META_SETMAPMODE or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetROP2                 = META_SETROP2 or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetRelAbs               = META_SETRELABS or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetPolyFillMode         = META_SETPOLYFILLMODE or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetStretchBltMode       = META_SETSTRETCHBLTMODE or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetTextCharExtra        = META_SETTEXTCHAREXTRA or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetTextColor            = META_SETTEXTCOLOR or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetTextJustification    = META_SETTEXTJUSTIFICATION or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetWindowOrg            = META_SETWINDOWORG or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetWindowExt            = META_SETWINDOWEXT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetViewportOrg          = META_SETVIEWPORTORG or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetViewportExt          = META_SETVIEWPORTEXT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeOffsetWindowOrg         = META_OFFSETWINDOWORG or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeScaleWindowExt          = META_SCALEWINDOWEXT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeOffsetViewportOrg       = META_OFFSETVIEWPORTORG or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeScaleViewportExt        = META_SCALEVIEWPORTEXT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeLineTo                  = META_LINETO or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeMoveTo                  = META_MOVETO or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeExcludeClipRect         = META_EXCLUDECLIPRECT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeIntersectClipRect       = META_INTERSECTCLIPRECT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeArc                     = META_ARC or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeEllipse                 = META_ELLIPSE or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeFloodFill               = META_FLOODFILL or GDIP_WMF_RECORD_BASE,
    WmfRecordTypePie                     = META_PIE or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeRectangle               = META_RECTANGLE or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeRoundRect               = META_ROUNDRECT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypePatBlt                  = META_PATBLT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSaveDC                  = META_SAVEDC or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetPixel                = META_SETPIXEL or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeOffsetClipRgn           = META_OFFSETCLIPRGN or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeTextOut                 = META_TEXTOUT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeBitBlt                  = META_BITBLT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeStretchBlt              = META_STRETCHBLT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypePolygon                 = META_POLYGON or GDIP_WMF_RECORD_BASE,
    WmfRecordTypePolyline                = META_POLYLINE or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeEscape                  = META_ESCAPE or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeRestoreDC               = META_RESTOREDC or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeFillRegion              = META_FILLREGION or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeFrameRegion             = META_FRAMEREGION or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeInvertRegion            = META_INVERTREGION or GDIP_WMF_RECORD_BASE,
    WmfRecordTypePaintRegion             = META_PAINTREGION or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSelectClipRegion        = META_SELECTCLIPREGION or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSelectObject            = META_SELECTOBJECT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetTextAlign            = META_SETTEXTALIGN or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeDrawText                = $062F or GDIP_WMF_RECORD_BASE,  // META_DRAWTEXT
    WmfRecordTypeChord                   = META_CHORD or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetMapperFlags          = META_SETMAPPERFLAGS or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeExtTextOut              = META_EXTTEXTOUT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetDIBToDev             = META_SETDIBTODEV or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSelectPalette           = META_SELECTPALETTE or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeRealizePalette          = META_REALIZEPALETTE or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeAnimatePalette          = META_ANIMATEPALETTE or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetPalEntries           = META_SETPALENTRIES or GDIP_WMF_RECORD_BASE,
    WmfRecordTypePolyPolygon             = META_POLYPOLYGON or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeResizePalette           = META_RESIZEPALETTE or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeDIBBitBlt               = META_DIBBITBLT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeDIBStretchBlt           = META_DIBSTRETCHBLT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeDIBCreatePatternBrush   = META_DIBCREATEPATTERNBRUSH or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeStretchDIB              = META_STRETCHDIB or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeExtFloodFill            = META_EXTFLOODFILL or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeSetLayout               = $0149 or GDIP_WMF_RECORD_BASE,  // META_SETLAYOUT
    WmfRecordTypeResetDC                 = $014C or GDIP_WMF_RECORD_BASE,  // META_RESETDC
    WmfRecordTypeStartDoc                = $014D or GDIP_WMF_RECORD_BASE,  // META_STARTDOC
    WmfRecordTypeStartPage               = $004F or GDIP_WMF_RECORD_BASE,  // META_STARTPAGE
    WmfRecordTypeEndPage                 = $0050 or GDIP_WMF_RECORD_BASE,  // META_ENDPAGE
    WmfRecordTypeAbortDoc                = $0052 or GDIP_WMF_RECORD_BASE,  // META_ABORTDOC
    WmfRecordTypeEndDoc                  = $005E or GDIP_WMF_RECORD_BASE,  // META_ENDDOC
    WmfRecordTypeDeleteObject            = META_DELETEOBJECT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeCreatePalette           = META_CREATEPALETTE or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeCreateBrush             = $00F8 or GDIP_WMF_RECORD_BASE,  // META_CREATEBRUSH
    WmfRecordTypeCreatePatternBrush      = META_CREATEPATTERNBRUSH or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeCreatePenIndirect       = META_CREATEPENINDIRECT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeCreateFontIndirect      = META_CREATEFONTINDIRECT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeCreateBrushIndirect     = META_CREATEBRUSHINDIRECT or GDIP_WMF_RECORD_BASE,
    WmfRecordTypeCreateBitmapIndirect    = $02FD or GDIP_WMF_RECORD_BASE,  // META_CREATEBITMAPINDIRECT
    WmfRecordTypeCreateBitmap            = $06FE or GDIP_WMF_RECORD_BASE,  // META_CREATEBITMAP
    WmfRecordTypeCreateRegion            = META_CREATEREGION or GDIP_WMF_RECORD_BASE,

    EmfRecordTypeHeader                  = EMR_HEADER,
    EmfRecordTypePolyBezier              = EMR_POLYBEZIER,
    EmfRecordTypePolygon                 = EMR_POLYGON,
    EmfRecordTypePolyline                = EMR_POLYLINE,
    EmfRecordTypePolyBezierTo            = EMR_POLYBEZIERTO,
    EmfRecordTypePolyLineTo              = EMR_POLYLINETO,
    EmfRecordTypePolyPolyline            = EMR_POLYPOLYLINE,
    EmfRecordTypePolyPolygon             = EMR_POLYPOLYGON,
    EmfRecordTypeSetWindowExtEx          = EMR_SETWINDOWEXTEX,
    EmfRecordTypeSetWindowOrgEx          = EMR_SETWINDOWORGEX,
    EmfRecordTypeSetViewportExtEx        = EMR_SETVIEWPORTEXTEX,
    EmfRecordTypeSetViewportOrgEx        = EMR_SETVIEWPORTORGEX,
    EmfRecordTypeSetBrushOrgEx           = EMR_SETBRUSHORGEX,
    EmfRecordTypeEOF                     = EMR_EOF,
    EmfRecordTypeSetPixelV               = EMR_SETPIXELV,
    EmfRecordTypeSetMapperFlags          = EMR_SETMAPPERFLAGS,
    EmfRecordTypeSetMapMode              = EMR_SETMAPMODE,
    EmfRecordTypeSetBkMode               = EMR_SETBKMODE,
    EmfRecordTypeSetPolyFillMode         = EMR_SETPOLYFILLMODE,
    EmfRecordTypeSetROP2                 = EMR_SETROP2,
    EmfRecordTypeSetStretchBltMode       = EMR_SETSTRETCHBLTMODE,
    EmfRecordTypeSetTextAlign            = EMR_SETTEXTALIGN,
    EmfRecordTypeSetColorAdjustment      = EMR_SETCOLORADJUSTMENT,
    EmfRecordTypeSetTextColor            = EMR_SETTEXTCOLOR,
    EmfRecordTypeSetBkColor              = EMR_SETBKCOLOR,
    EmfRecordTypeOffsetClipRgn           = EMR_OFFSETCLIPRGN,
    EmfRecordTypeMoveToEx                = EMR_MOVETOEX,
    EmfRecordTypeSetMetaRgn              = EMR_SETMETARGN,
    EmfRecordTypeExcludeClipRect         = EMR_EXCLUDECLIPRECT,
    EmfRecordTypeIntersectClipRect       = EMR_INTERSECTCLIPRECT,
    EmfRecordTypeScaleViewportExtEx      = EMR_SCALEVIEWPORTEXTEX,
    EmfRecordTypeScaleWindowExtEx        = EMR_SCALEWINDOWEXTEX,
    EmfRecordTypeSaveDC                  = EMR_SAVEDC,
    EmfRecordTypeRestoreDC               = EMR_RESTOREDC,
    EmfRecordTypeSetWorldTransform       = EMR_SETWORLDTRANSFORM,
    EmfRecordTypeModifyWorldTransform    = EMR_MODIFYWORLDTRANSFORM,
    EmfRecordTypeSelectObject            = EMR_SELECTOBJECT,
    EmfRecordTypeCreatePen               = EMR_CREATEPEN,
    EmfRecordTypeCreateBrushIndirect     = EMR_CREATEBRUSHINDIRECT,
    EmfRecordTypeDeleteObject            = EMR_DELETEOBJECT,
    EmfRecordTypeAngleArc                = EMR_ANGLEARC,
    EmfRecordTypeEllipse                 = EMR_ELLIPSE,
    EmfRecordTypeRectangle               = EMR_RECTANGLE,
    EmfRecordTypeRoundRect               = EMR_ROUNDRECT,
    EmfRecordTypeArc                     = EMR_ARC,
    EmfRecordTypeChord                   = EMR_CHORD,
    EmfRecordTypePie                     = EMR_PIE,
    EmfRecordTypeSelectPalette           = EMR_SELECTPALETTE,
    EmfRecordTypeCreatePalette           = EMR_CREATEPALETTE,
    EmfRecordTypeSetPaletteEntries       = EMR_SETPALETTEENTRIES,
    EmfRecordTypeResizePalette           = EMR_RESIZEPALETTE,
    EmfRecordTypeRealizePalette          = EMR_REALIZEPALETTE,
    EmfRecordTypeExtFloodFill            = EMR_EXTFLOODFILL,
    EmfRecordTypeLineTo                  = EMR_LINETO,
    EmfRecordTypeArcTo                   = EMR_ARCTO,
    EmfRecordTypePolyDraw                = EMR_POLYDRAW,
    EmfRecordTypeSetArcDirection         = EMR_SETARCDIRECTION,
    EmfRecordTypeSetMiterLimit           = EMR_SETMITERLIMIT,
    EmfRecordTypeBeginPath               = EMR_BEGINPATH,
    EmfRecordTypeEndPath                 = EMR_ENDPATH,
    EmfRecordTypeCloseFigure             = EMR_CLOSEFIGURE,
    EmfRecordTypeFillPath                = EMR_FILLPATH,
    EmfRecordTypeStrokeAndFillPath       = EMR_STROKEANDFILLPATH,
    EmfRecordTypeStrokePath              = EMR_STROKEPATH,
    EmfRecordTypeFlattenPath             = EMR_FLATTENPATH,
    EmfRecordTypeWidenPath               = EMR_WIDENPATH,
    EmfRecordTypeSelectClipPath          = EMR_SELECTCLIPPATH,
    EmfRecordTypeAbortPath               = EMR_ABORTPATH,
    EmfRecordTypeReserved_069            = 69,  // Not Used
    EmfRecordTypeGdiComment              = EMR_GDICOMMENT,
    EmfRecordTypeFillRgn                 = EMR_FILLRGN,
    EmfRecordTypeFrameRgn                = EMR_FRAMERGN,
    EmfRecordTypeInvertRgn               = EMR_INVERTRGN,
    EmfRecordTypePaintRgn                = EMR_PAINTRGN,
    EmfRecordTypeExtSelectClipRgn        = EMR_EXTSELECTCLIPRGN,
    EmfRecordTypeBitBlt                  = EMR_BITBLT,
    EmfRecordTypeStretchBlt              = EMR_STRETCHBLT,
    EmfRecordTypeMaskBlt                 = EMR_MASKBLT,
    EmfRecordTypePlgBlt                  = EMR_PLGBLT,
    EmfRecordTypeSetDIBitsToDevice       = EMR_SETDIBITSTODEVICE,
    EmfRecordTypeStretchDIBits           = EMR_STRETCHDIBITS,
    EmfRecordTypeExtCreateFontIndirect   = EMR_EXTCREATEFONTINDIRECTW,
    EmfRecordTypeExtTextOutA             = EMR_EXTTEXTOUTA,
    EmfRecordTypeExtTextOutW             = EMR_EXTTEXTOUTW,
    EmfRecordTypePolyBezier16            = EMR_POLYBEZIER16,
    EmfRecordTypePolygon16               = EMR_POLYGON16,
    EmfRecordTypePolyline16              = EMR_POLYLINE16,
    EmfRecordTypePolyBezierTo16          = EMR_POLYBEZIERTO16,
    EmfRecordTypePolylineTo16            = EMR_POLYLINETO16,
    EmfRecordTypePolyPolyline16          = EMR_POLYPOLYLINE16,
    EmfRecordTypePolyPolygon16           = EMR_POLYPOLYGON16,
    EmfRecordTypePolyDraw16              = EMR_POLYDRAW16,
    EmfRecordTypeCreateMonoBrush         = EMR_CREATEMONOBRUSH,
    EmfRecordTypeCreateDIBPatternBrushPt = EMR_CREATEDIBPATTERNBRUSHPT,
    EmfRecordTypeExtCreatePen            = EMR_EXTCREATEPEN,
    EmfRecordTypePolyTextOutA            = EMR_POLYTEXTOUTA,
    EmfRecordTypePolyTextOutW            = EMR_POLYTEXTOUTW,
    EmfRecordTypeSetICMMode              = 98,  // EMR_SETICMMODE,
    EmfRecordTypeCreateColorSpace        = 99,  // EMR_CREATECOLORSPACE,
    EmfRecordTypeSetColorSpace           = 100, // EMR_SETCOLORSPACE,
    EmfRecordTypeDeleteColorSpace        = 101, // EMR_DELETECOLORSPACE,
    EmfRecordTypeGLSRecord               = 102, // EMR_GLSRECORD,
    EmfRecordTypeGLSBoundedRecord        = 103, // EMR_GLSBOUNDEDRECORD,
    EmfRecordTypePixelFormat             = 104, // EMR_PIXELFORMAT,
    EmfRecordTypeDrawEscape              = 105, // EMR_RESERVED_105,
    EmfRecordTypeExtEscape               = 106, // EMR_RESERVED_106,
    EmfRecordTypeStartDoc                = 107, // EMR_RESERVED_107,
    EmfRecordTypeSmallTextOut            = 108, // EMR_RESERVED_108,
    EmfRecordTypeForceUFIMapping         = 109, // EMR_RESERVED_109,
    EmfRecordTypeNamedEscape             = 110, // EMR_RESERVED_110,
    EmfRecordTypeColorCorrectPalette     = 111, // EMR_COLORCORRECTPALETTE,
    EmfRecordTypeSetICMProfileA          = 112, // EMR_SETICMPROFILEA,
    EmfRecordTypeSetICMProfileW          = 113, // EMR_SETICMPROFILEW,
    EmfRecordTypeAlphaBlend              = 114, // EMR_ALPHABLEND,
    EmfRecordTypeSetLayout               = 115, // EMR_SETLAYOUT,
    EmfRecordTypeTransparentBlt          = 116, // EMR_TRANSPARENTBLT,
    EmfRecordTypeReserved_117            = 117, // Not Used
    EmfRecordTypeGradientFill            = 118, // EMR_GRADIENTFILL,
    EmfRecordTypeSetLinkedUFIs           = 119, // EMR_RESERVED_119,
    EmfRecordTypeSetTextJustification    = 120, // EMR_RESERVED_120,
    EmfRecordTypeColorMatchToTargetW     = 121, // EMR_COLORMATCHTOTARGETW,
    EmfRecordTypeCreateColorSpaceW       = 122, // EMR_CREATECOLORSPACEW,
    EmfRecordTypeMax                     = 122,
    EmfRecordTypeMin                     = 1,

    // That is the END of the GDI EMF records.

    // Now we start the list of EMF+ records.  We leave quite
    // a bit of room here for the addition of any new GDI
    // records that may be added later.

    EmfPlusRecordTypeInvalid = GDIP_EMFPLUS_RECORD_BASE,
    EmfPlusRecordTypeHeader,
    EmfPlusRecordTypeEndOfFile,

    EmfPlusRecordTypeComment,

    EmfPlusRecordTypeGetDC,

    EmfPlusRecordTypeMultiFormatStart,
    EmfPlusRecordTypeMultiFormatSection,
    EmfPlusRecordTypeMultiFormatEnd,

    // For all persistent objects

    EmfPlusRecordTypeObject,

    // Drawing Records

    EmfPlusRecordTypeClear,
    EmfPlusRecordTypeFillRects,
    EmfPlusRecordTypeDrawRects,
    EmfPlusRecordTypeFillPolygon,
    EmfPlusRecordTypeDrawLines,
    EmfPlusRecordTypeFillEllipse,
    EmfPlusRecordTypeDrawEllipse,
    EmfPlusRecordTypeFillPie,
    EmfPlusRecordTypeDrawPie,
    EmfPlusRecordTypeDrawArc,
    EmfPlusRecordTypeFillRegion,
    EmfPlusRecordTypeFillPath,
    EmfPlusRecordTypeDrawPath,
    EmfPlusRecordTypeFillClosedCurve,
    EmfPlusRecordTypeDrawClosedCurve,
    EmfPlusRecordTypeDrawCurve,
    EmfPlusRecordTypeDrawBeziers,
    EmfPlusRecordTypeDrawImage,
    EmfPlusRecordTypeDrawImagePoints,
    EmfPlusRecordTypeDrawString,

    // Graphics State Records

    EmfPlusRecordTypeSetRenderingOrigin,
    EmfPlusRecordTypeSetAntiAliasMode,
    EmfPlusRecordTypeSetTextRenderingHint,
    EmfPlusRecordTypeSetTextContrast,
    EmfPlusRecordTypeSetInterpolationMode,
    EmfPlusRecordTypeSetPixelOffsetMode,
    EmfPlusRecordTypeSetCompositingMode,
    EmfPlusRecordTypeSetCompositingQuality,
    EmfPlusRecordTypeSave,
    EmfPlusRecordTypeRestore,
    EmfPlusRecordTypeBeginContainer,
    EmfPlusRecordTypeBeginContainerNoParams,
    EmfPlusRecordTypeEndContainer,
    EmfPlusRecordTypeSetWorldTransform,
    EmfPlusRecordTypeResetWorldTransform,
    EmfPlusRecordTypeMultiplyWorldTransform,
    EmfPlusRecordTypeTranslateWorldTransform,
    EmfPlusRecordTypeScaleWorldTransform,
    EmfPlusRecordTypeRotateWorldTransform,
    EmfPlusRecordTypeSetPageTransform,
    EmfPlusRecordTypeResetClip,
    EmfPlusRecordTypeSetClipRect,
    EmfPlusRecordTypeSetClipPath,
    EmfPlusRecordTypeSetClipRegion,
    EmfPlusRecordTypeOffsetClip,

    EmfPlusRecordTypeDrawDriverString,

    EmfPlusRecordTotal,

    EmfPlusRecordTypeMax = EmfPlusRecordTotal-1,
    EmfPlusRecordTypeMin = EmfPlusRecordTypeHeader
  );

(*$HPPEMIT 'enum TIGPEmfPlusRecordType' *)
(*$HPPEMIT '{' *)
(*$HPPEMIT '    WmfRecordTypeSetBkColor              = (META_SETBKCOLOR | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetBkMode               = (META_SETBKMODE | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetMapMode              = (META_SETMAPMODE | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetROP2                 = (META_SETROP2 | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetRelAbs               = (META_SETRELABS | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetPolyFillMode         = (META_SETPOLYFILLMODE | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetStretchBltMode       = (META_SETSTRETCHBLTMODE | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetTextCharExtra        = (META_SETTEXTCHAREXTRA | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetTextColor            = (META_SETTEXTCOLOR | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetTextJustification    = (META_SETTEXTJUSTIFICATION | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetWindowOrg            = (META_SETWINDOWORG | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetWindowExt            = (META_SETWINDOWEXT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetViewportOrg          = (META_SETVIEWPORTORG | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetViewportExt          = (META_SETVIEWPORTEXT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeOffsetWindowOrg         = (META_OFFSETWINDOWORG | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeScaleWindowExt          = (META_SCALEWINDOWEXT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeOffsetViewportOrg       = (META_OFFSETVIEWPORTORG | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeScaleViewportExt        = (META_SCALEVIEWPORTEXT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeLineTo                  = (META_LINETO | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeMoveTo                  = (META_MOVETO | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeExcludeClipRect         = (META_EXCLUDECLIPRECT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeIntersectClipRect       = (META_INTERSECTCLIPRECT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeArc                     = (META_ARC | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeEllipse                 = (META_ELLIPSE | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeFloodFill               = (META_FLOODFILL | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypePie                     = (META_PIE | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeRectangle               = (META_RECTANGLE | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeRoundRect               = (META_ROUNDRECT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypePatBlt                  = (META_PATBLT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSaveDC                  = (META_SAVEDC | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetPixel                = (META_SETPIXEL | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeOffsetClipRgn           = (META_OFFSETCLIPRGN | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeTextOut                 = (META_TEXTOUT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeBitBlt                  = (META_BITBLT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeStretchBlt              = (META_STRETCHBLT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypePolygon                 = (META_POLYGON | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypePolyline                = (META_POLYLINE | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeEscape                  = (META_ESCAPE | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeRestoreDC               = (META_RESTOREDC | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeFillRegion              = (META_FILLREGION | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeFrameRegion             = (META_FRAMEREGION | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeInvertRegion            = (META_INVERTREGION | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypePaintRegion             = (META_PAINTREGION | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSelectClipRegion        = (META_SELECTCLIPREGION | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSelectObject            = (META_SELECTOBJECT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetTextAlign            = (META_SETTEXTALIGN | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeDrawText                = (0x062F | BCBGDIP_WMF_RECORD_BASE),  // META_DRAWTEXT' *)
(*$HPPEMIT '    WmfRecordTypeChord                   = (META_CHORD | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetMapperFlags          = (META_SETMAPPERFLAGS | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeExtTextOut              = (META_EXTTEXTOUT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetDIBToDev             = (META_SETDIBTODEV | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSelectPalette           = (META_SELECTPALETTE | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeRealizePalette          = (META_REALIZEPALETTE | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeAnimatePalette          = (META_ANIMATEPALETTE | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetPalEntries           = (META_SETPALENTRIES | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypePolyPolygon             = (META_POLYPOLYGON | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeResizePalette           = (META_RESIZEPALETTE | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeDIBBitBlt               = (META_DIBBITBLT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeDIBStretchBlt           = (META_DIBSTRETCHBLT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeDIBCreatePatternBrush   = (META_DIBCREATEPATTERNBRUSH | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeStretchDIB              = (META_STRETCHDIB | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeExtFloodFill            = (META_EXTFLOODFILL | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeSetLayout               = (0x0149 | BCBGDIP_WMF_RECORD_BASE),  // META_SETLAYOUT' *)
(*$HPPEMIT '    WmfRecordTypeResetDC                 = (0x014C | BCBGDIP_WMF_RECORD_BASE),  // META_RESETDC' *)
(*$HPPEMIT '    WmfRecordTypeStartDoc                = (0x014D | BCBGDIP_WMF_RECORD_BASE),  // META_STARTDOC' *)
(*$HPPEMIT '    WmfRecordTypeStartPage               = (0x004F | BCBGDIP_WMF_RECORD_BASE),  // META_STARTPAGE' *)
(*$HPPEMIT '    WmfRecordTypeEndPage                 = (0x0050 | BCBGDIP_WMF_RECORD_BASE),  // META_ENDPAGE' *)
(*$HPPEMIT '    WmfRecordTypeAbortDoc                = (0x0052 | BCBGDIP_WMF_RECORD_BASE),  // META_ABORTDOC' *)
(*$HPPEMIT '    WmfRecordTypeEndDoc                  = (0x005E | BCBGDIP_WMF_RECORD_BASE),  // META_ENDDOC' *)
(*$HPPEMIT '    WmfRecordTypeDeleteObject            = (META_DELETEOBJECT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeCreatePalette           = (META_CREATEPALETTE | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeCreateBrush             = (0x00F8 | BCBGDIP_WMF_RECORD_BASE),  // META_CREATEBRUSH' *)
(*$HPPEMIT '    WmfRecordTypeCreatePatternBrush      = (META_CREATEPATTERNBRUSH | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeCreatePenIndirect       = (META_CREATEPENINDIRECT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeCreateFontIndirect      = (META_CREATEFONTINDIRECT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeCreateBrushIndirect     = (META_CREATEBRUSHINDIRECT | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '    WmfRecordTypeCreateBitmapIndirect    = (0x02FD | BCBGDIP_WMF_RECORD_BASE),  // META_CREATEBITMAPINDIRECT' *)
(*$HPPEMIT '    WmfRecordTypeCreateBitmap            = (0x06FE | BCBGDIP_WMF_RECORD_BASE),  // META_CREATEBITMAP' *)
(*$HPPEMIT '    WmfRecordTypeCreateRegion            = (META_CREATEREGION | BCBGDIP_WMF_RECORD_BASE),' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    EmfRecordTypeHeader                  = EMR_HEADER,' *)
(*$HPPEMIT '    EmfRecordTypePolyBezier              = EMR_POLYBEZIER,' *)
(*$HPPEMIT '    EmfRecordTypePolygon                 = EMR_POLYGON,' *)
(*$HPPEMIT '    EmfRecordTypePolyline                = EMR_POLYLINE,' *)
(*$HPPEMIT '    EmfRecordTypePolyBezierTo            = EMR_POLYBEZIERTO,' *)
(*$HPPEMIT '    EmfRecordTypePolyLineTo              = EMR_POLYLINETO,' *)
(*$HPPEMIT '    EmfRecordTypePolyPolyline            = EMR_POLYPOLYLINE,' *)
(*$HPPEMIT '    EmfRecordTypePolyPolygon             = EMR_POLYPOLYGON,' *)
(*$HPPEMIT '    EmfRecordTypeSetWindowExtEx          = EMR_SETWINDOWEXTEX,' *)
(*$HPPEMIT '    EmfRecordTypeSetWindowOrgEx          = EMR_SETWINDOWORGEX,' *)
(*$HPPEMIT '    EmfRecordTypeSetViewportExtEx        = EMR_SETVIEWPORTEXTEX,' *)
(*$HPPEMIT '    EmfRecordTypeSetViewportOrgEx        = EMR_SETVIEWPORTORGEX,' *)
(*$HPPEMIT '    EmfRecordTypeSetBrushOrgEx           = EMR_SETBRUSHORGEX,' *)
(*$HPPEMIT '    EmfRecordTypeEOF                     = EMR_EOF,' *)
(*$HPPEMIT '    EmfRecordTypeSetPixelV               = EMR_SETPIXELV,' *)
(*$HPPEMIT '    EmfRecordTypeSetMapperFlags          = EMR_SETMAPPERFLAGS,' *)
(*$HPPEMIT '    EmfRecordTypeSetMapMode              = EMR_SETMAPMODE,' *)
(*$HPPEMIT '    EmfRecordTypeSetBkMode               = EMR_SETBKMODE,' *)
(*$HPPEMIT '    EmfRecordTypeSetPolyFillMode         = EMR_SETPOLYFILLMODE,' *)
(*$HPPEMIT '    EmfRecordTypeSetROP2                 = EMR_SETROP2,' *)
(*$HPPEMIT '    EmfRecordTypeSetStretchBltMode       = EMR_SETSTRETCHBLTMODE,' *)
(*$HPPEMIT '    EmfRecordTypeSetTextAlign            = EMR_SETTEXTALIGN,' *)
(*$HPPEMIT '    EmfRecordTypeSetColorAdjustment      = EMR_SETCOLORADJUSTMENT,' *)
(*$HPPEMIT '    EmfRecordTypeSetTextColor            = EMR_SETTEXTCOLOR,' *)
(*$HPPEMIT '    EmfRecordTypeSetBkColor              = EMR_SETBKCOLOR,' *)
(*$HPPEMIT '    EmfRecordTypeOffsetClipRgn           = EMR_OFFSETCLIPRGN,' *)
(*$HPPEMIT '    EmfRecordTypeMoveToEx                = EMR_MOVETOEX,' *)
(*$HPPEMIT '    EmfRecordTypeSetMetaRgn              = EMR_SETMETARGN,' *)
(*$HPPEMIT '    EmfRecordTypeExcludeClipRect         = EMR_EXCLUDECLIPRECT,' *)
(*$HPPEMIT '    EmfRecordTypeIntersectClipRect       = EMR_INTERSECTCLIPRECT,' *)
(*$HPPEMIT '    EmfRecordTypeScaleViewportExtEx      = EMR_SCALEVIEWPORTEXTEX,' *)
(*$HPPEMIT '    EmfRecordTypeScaleWindowExtEx        = EMR_SCALEWINDOWEXTEX,' *)
(*$HPPEMIT '    EmfRecordTypeSaveDC                  = EMR_SAVEDC,' *)
(*$HPPEMIT '    EmfRecordTypeRestoreDC               = EMR_RESTOREDC,' *)
(*$HPPEMIT '    EmfRecordTypeSetWorldTransform       = EMR_SETWORLDTRANSFORM,' *)
(*$HPPEMIT '    EmfRecordTypeModifyWorldTransform    = EMR_MODIFYWORLDTRANSFORM,' *)
(*$HPPEMIT '    EmfRecordTypeSelectObject            = EMR_SELECTOBJECT,' *)
(*$HPPEMIT '    EmfRecordTypeCreatePen               = EMR_CREATEPEN,' *)
(*$HPPEMIT '    EmfRecordTypeCreateBrushIndirect     = EMR_CREATEBRUSHINDIRECT,' *)
(*$HPPEMIT '    EmfRecordTypeDeleteObject            = EMR_DELETEOBJECT,' *)
(*$HPPEMIT '    EmfRecordTypeAngleArc                = EMR_ANGLEARC,' *)
(*$HPPEMIT '    EmfRecordTypeEllipse                 = EMR_ELLIPSE,' *)
(*$HPPEMIT '    EmfRecordTypeRectangle               = EMR_RECTANGLE,' *)
(*$HPPEMIT '    EmfRecordTypeRoundRect               = EMR_ROUNDRECT,' *)
(*$HPPEMIT '    EmfRecordTypeArc                     = EMR_ARC,' *)
(*$HPPEMIT '    EmfRecordTypeChord                   = EMR_CHORD,' *)
(*$HPPEMIT '    EmfRecordTypePie                     = EMR_PIE,' *)
(*$HPPEMIT '    EmfRecordTypeSelectPalette           = EMR_SELECTPALETTE,' *)
(*$HPPEMIT '    EmfRecordTypeCreatePalette           = EMR_CREATEPALETTE,' *)
(*$HPPEMIT '    EmfRecordTypeSetPaletteEntries       = EMR_SETPALETTEENTRIES,' *)
(*$HPPEMIT '    EmfRecordTypeResizePalette           = EMR_RESIZEPALETTE,' *)
(*$HPPEMIT '    EmfRecordTypeRealizePalette          = EMR_REALIZEPALETTE,' *)
(*$HPPEMIT '    EmfRecordTypeExtFloodFill            = EMR_EXTFLOODFILL,' *)
(*$HPPEMIT '    EmfRecordTypeLineTo                  = EMR_LINETO,' *)
(*$HPPEMIT '    EmfRecordTypeArcTo                   = EMR_ARCTO,' *)
(*$HPPEMIT '    EmfRecordTypePolyDraw                = EMR_POLYDRAW,' *)
(*$HPPEMIT '    EmfRecordTypeSetArcDirection         = EMR_SETARCDIRECTION,' *)
(*$HPPEMIT '    EmfRecordTypeSetMiterLimit           = EMR_SETMITERLIMIT,' *)
(*$HPPEMIT '    EmfRecordTypeBeginPath               = EMR_BEGINPATH,' *)
(*$HPPEMIT '    EmfRecordTypeEndPath                 = EMR_ENDPATH,' *)
(*$HPPEMIT '    EmfRecordTypeCloseFigure             = EMR_CLOSEFIGURE,' *)
(*$HPPEMIT '    EmfRecordTypeFillPath                = EMR_FILLPATH,' *)
(*$HPPEMIT '    EmfRecordTypeStrokeAndFillPath       = EMR_STROKEANDFILLPATH,' *)
(*$HPPEMIT '    EmfRecordTypeStrokePath              = EMR_STROKEPATH,' *)
(*$HPPEMIT '    EmfRecordTypeFlattenPath             = EMR_FLATTENPATH,' *)
(*$HPPEMIT '    EmfRecordTypeWidenPath               = EMR_WIDENPATH,' *)
(*$HPPEMIT '    EmfRecordTypeSelectClipPath          = EMR_SELECTCLIPPATH,' *)
(*$HPPEMIT '    EmfRecordTypeAbortPath               = EMR_ABORTPATH,' *)
(*$HPPEMIT '    EmfRecordTypeReserved_069            = 69,  // Not Used' *)
(*$HPPEMIT '    EmfRecordTypeGdiComment              = EMR_GDICOMMENT,' *)
(*$HPPEMIT '    EmfRecordTypeFillRgn                 = EMR_FILLRGN,' *)
(*$HPPEMIT '    EmfRecordTypeFrameRgn                = EMR_FRAMERGN,' *)
(*$HPPEMIT '    EmfRecordTypeInvertRgn               = EMR_INVERTRGN,' *)
(*$HPPEMIT '    EmfRecordTypePaintRgn                = EMR_PAINTRGN,' *)
(*$HPPEMIT '    EmfRecordTypeExtSelectClipRgn        = EMR_EXTSELECTCLIPRGN,' *)
(*$HPPEMIT '    EmfRecordTypeBitBlt                  = EMR_BITBLT,' *)
(*$HPPEMIT '    EmfRecordTypeStretchBlt              = EMR_STRETCHBLT,' *)
(*$HPPEMIT '    EmfRecordTypeMaskBlt                 = EMR_MASKBLT,' *)
(*$HPPEMIT '    EmfRecordTypePlgBlt                  = EMR_PLGBLT,' *)
(*$HPPEMIT '    EmfRecordTypeSetDIBitsToDevice       = EMR_SETDIBITSTODEVICE,' *)
(*$HPPEMIT '    EmfRecordTypeStretchDIBits           = EMR_STRETCHDIBITS,' *)
(*$HPPEMIT '    EmfRecordTypeExtCreateFontIndirect   = EMR_EXTCREATEFONTINDIRECTW,' *)
(*$HPPEMIT '    EmfRecordTypeExtTextOutA             = EMR_EXTTEXTOUTA,' *)
(*$HPPEMIT '    EmfRecordTypeExtTextOutW             = EMR_EXTTEXTOUTW,' *)
(*$HPPEMIT '    EmfRecordTypePolyBezier16            = EMR_POLYBEZIER16,' *)
(*$HPPEMIT '    EmfRecordTypePolygon16               = EMR_POLYGON16,' *)
(*$HPPEMIT '    EmfRecordTypePolyline16              = EMR_POLYLINE16,' *)
(*$HPPEMIT '    EmfRecordTypePolyBezierTo16          = EMR_POLYBEZIERTO16,' *)
(*$HPPEMIT '    EmfRecordTypePolylineTo16            = EMR_POLYLINETO16,' *)
(*$HPPEMIT '    EmfRecordTypePolyPolyline16          = EMR_POLYPOLYLINE16,' *)
(*$HPPEMIT '    EmfRecordTypePolyPolygon16           = EMR_POLYPOLYGON16,' *)
(*$HPPEMIT '    EmfRecordTypePolyDraw16              = EMR_POLYDRAW16,' *)
(*$HPPEMIT '    EmfRecordTypeCreateMonoBrush         = EMR_CREATEMONOBRUSH,' *)
(*$HPPEMIT '    EmfRecordTypeCreateDIBPatternBrushPt = EMR_CREATEDIBPATTERNBRUSHPT,' *)
(*$HPPEMIT '    EmfRecordTypeExtCreatePen            = EMR_EXTCREATEPEN,' *)
(*$HPPEMIT '    EmfRecordTypePolyTextOutA            = EMR_POLYTEXTOUTA,' *)
(*$HPPEMIT '    EmfRecordTypePolyTextOutW            = EMR_POLYTEXTOUTW,' *)
(*$HPPEMIT '    EmfRecordTypeSetICMMode              = 98,  // EMR_SETICMMODE,' *)
(*$HPPEMIT '    EmfRecordTypeCreateColorSpace        = 99,  // EMR_CREATECOLORSPACE,' *)
(*$HPPEMIT '    EmfRecordTypeSetColorSpace           = 100, // EMR_SETCOLORSPACE,' *)
(*$HPPEMIT '    EmfRecordTypeDeleteColorSpace        = 101, // EMR_DELETECOLORSPACE,' *)
(*$HPPEMIT '    EmfRecordTypeGLSRecord               = 102, // EMR_GLSRECORD,' *)
(*$HPPEMIT '    EmfRecordTypeGLSBoundedRecord        = 103, // EMR_GLSBOUNDEDRECORD,' *)
(*$HPPEMIT '    EmfRecordTypePixelFormat             = 104, // EMR_PIXELFORMAT,' *)
(*$HPPEMIT '    EmfRecordTypeDrawEscape              = 105, // EMR_RESERVED_105,' *)
(*$HPPEMIT '    EmfRecordTypeExtEscape               = 106, // EMR_RESERVED_106,' *)
(*$HPPEMIT '    EmfRecordTypeStartDoc                = 107, // EMR_RESERVED_107,' *)
(*$HPPEMIT '    EmfRecordTypeSmallTextOut            = 108, // EMR_RESERVED_108,' *)
(*$HPPEMIT '    EmfRecordTypeForceUFIMapping         = 109, // EMR_RESERVED_109,' *)
(*$HPPEMIT '    EmfRecordTypeNamedEscape             = 110, // EMR_RESERVED_110,' *)
(*$HPPEMIT '    EmfRecordTypeColorCorrectPalette     = 111, // EMR_COLORCORRECTPALETTE,' *)
(*$HPPEMIT '    EmfRecordTypeSetICMProfileA          = 112, // EMR_SETICMPROFILEA,' *)
(*$HPPEMIT '    EmfRecordTypeSetICMProfileW          = 113, // EMR_SETICMPROFILEW,' *)
(*$HPPEMIT '    EmfRecordTypeAlphaBlend              = 114, // EMR_ALPHABLEND,' *)
(*$HPPEMIT '    EmfRecordTypeSetLayout               = 115, // EMR_SETLAYOUT,' *)
(*$HPPEMIT '    EmfRecordTypeTransparentBlt          = 116, // EMR_TRANSPARENTBLT,' *)
(*$HPPEMIT '    EmfRecordTypeReserved_117            = 117, // Not Used' *)
(*$HPPEMIT '    EmfRecordTypeGradientFill            = 118, // EMR_GRADIENTFILL,' *)
(*$HPPEMIT '    EmfRecordTypeSetLinkedUFIs           = 119, // EMR_RESERVED_119,' *)
(*$HPPEMIT '    EmfRecordTypeSetTextJustification    = 120, // EMR_RESERVED_120,' *)
(*$HPPEMIT '    EmfRecordTypeColorMatchToTargetW     = 121, // EMR_COLORMATCHTOTARGETW,' *)
(*$HPPEMIT '    EmfRecordTypeCreateColorSpaceW       = 122, // EMR_CREATECOLORSPACEW,' *)
(*$HPPEMIT '    EmfRecordTypeMax                     = 122,' *)
(*$HPPEMIT '    EmfRecordTypeMin                     = 1,' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    // That is the END of the GDI EMF records.' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    // Now we start the list of EMF+ records.  We leave quite' *)
(*$HPPEMIT '    // a bit of room here for the addition of any new GDI' *)
(*$HPPEMIT '    // records that may be added later.' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    EmfPlusRecordTypeInvalid = BCBGDIP_EMFPLUS_RECORD_BASE,' *)
(*$HPPEMIT '    EmfPlusRecordTypeHeader,' *)
(*$HPPEMIT '    EmfPlusRecordTypeEndOfFile,' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    EmfPlusRecordTypeComment,' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    EmfPlusRecordTypeGetDC,' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    EmfPlusRecordTypeMultiFormatStart,' *)
(*$HPPEMIT '    EmfPlusRecordTypeMultiFormatSection,' *)
(*$HPPEMIT '    EmfPlusRecordTypeMultiFormatEnd,' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    // For all persistent objects' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    EmfPlusRecordTypeObject,' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    // Drawing Records' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    EmfPlusRecordTypeClear,' *)
(*$HPPEMIT '    EmfPlusRecordTypeFillRects,' *)
(*$HPPEMIT '    EmfPlusRecordTypeDrawRects,' *)
(*$HPPEMIT '    EmfPlusRecordTypeFillPolygon,' *)
(*$HPPEMIT '    EmfPlusRecordTypeDrawLines,' *)
(*$HPPEMIT '    EmfPlusRecordTypeFillEllipse,' *)
(*$HPPEMIT '    EmfPlusRecordTypeDrawEllipse,' *)
(*$HPPEMIT '    EmfPlusRecordTypeFillPie,' *)
(*$HPPEMIT '    EmfPlusRecordTypeDrawPie,' *)
(*$HPPEMIT '    EmfPlusRecordTypeDrawArc,' *)
(*$HPPEMIT '    EmfPlusRecordTypeFillRegion,' *)
(*$HPPEMIT '    EmfPlusRecordTypeFillPath,' *)
(*$HPPEMIT '    EmfPlusRecordTypeDrawPath,' *)
(*$HPPEMIT '    EmfPlusRecordTypeFillClosedCurve,' *)
(*$HPPEMIT '    EmfPlusRecordTypeDrawClosedCurve,' *)
(*$HPPEMIT '    EmfPlusRecordTypeDrawCurve,' *)
(*$HPPEMIT '    EmfPlusRecordTypeDrawBeziers,' *)
(*$HPPEMIT '    EmfPlusRecordTypeDrawImage,' *)
(*$HPPEMIT '    EmfPlusRecordTypeDrawImagePoints,' *)
(*$HPPEMIT '    EmfPlusRecordTypeDrawString,' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    // Graphics State Records' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    EmfPlusRecordTypeSetRenderingOrigin,' *)
(*$HPPEMIT '    EmfPlusRecordTypeSetAntiAliasMode,' *)
(*$HPPEMIT '    EmfPlusRecordTypeSetTextRenderingHint,' *)
(*$HPPEMIT '    EmfPlusRecordTypeSetTextContrast,' *)
(*$HPPEMIT '    EmfPlusRecordTypeSetInterpolationMode,' *)
(*$HPPEMIT '    EmfPlusRecordTypeSetPixelOffsetMode,' *)
(*$HPPEMIT '    EmfPlusRecordTypeSetCompositingMode,' *)
(*$HPPEMIT '    EmfPlusRecordTypeSetCompositingQuality,' *)
(*$HPPEMIT '    EmfPlusRecordTypeSave,' *)
(*$HPPEMIT '    EmfPlusRecordTypeRestore,' *)
(*$HPPEMIT '    EmfPlusRecordTypeBeginContainer,' *)
(*$HPPEMIT '    EmfPlusRecordTypeBeginContainerNoParams,' *)
(*$HPPEMIT '    EmfPlusRecordTypeEndContainer,' *)
(*$HPPEMIT '    EmfPlusRecordTypeSetWorldTransform,' *)
(*$HPPEMIT '    EmfPlusRecordTypeResetWorldTransform,' *)
(*$HPPEMIT '    EmfPlusRecordTypeMultiplyWorldTransform,' *)
(*$HPPEMIT '    EmfPlusRecordTypeTranslateWorldTransform,' *)
(*$HPPEMIT '    EmfPlusRecordTypeScaleWorldTransform,' *)
(*$HPPEMIT '    EmfPlusRecordTypeRotateWorldTransform,' *)
(*$HPPEMIT '    EmfPlusRecordTypeSetPageTransform,' *)
(*$HPPEMIT '    EmfPlusRecordTypeResetClip,' *)
(*$HPPEMIT '    EmfPlusRecordTypeSetClipRect,' *)
(*$HPPEMIT '    EmfPlusRecordTypeSetClipPath,' *)
(*$HPPEMIT '    EmfPlusRecordTypeSetClipRegion,' *)
(*$HPPEMIT '    EmfPlusRecordTypeOffsetClip,' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    EmfPlusRecordTypeDrawDriverString,' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    EmfPlusRecordTotal,' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    EmfPlusRecordTypeMax = EmfPlusRecordTotal-1,' *)
(*$HPPEMIT '    EmfPlusRecordTypeMin = EmfPlusRecordTypeHeader' *)
(*$HPPEMIT '};' *)

{$ENDIF} //MSWINDOWS

//---------------------------------------------------------------------------
// StringFormatFlags
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
// String format flags
//
//  DirectionRightToLeft          - For horizontal text, the reading order is
//                                  right to left. This value is called
//                                  the base embedding level by the Unicode
//                                  bidirectional engine.
//                                  For vertical text, columns are read from
//                                  right to left.
//                                  By default, horizontal or vertical text is
//                                  read from left to right.
//
//  DirectionVertical             - Individual lines of text are vertical. In
//                                  each line, characters progress from top to
//                                  bottom.
//                                  By default, lines of text are horizontal,
//                                  each new line below the previous line.
//
//  NoFitBlackBox                 - Allows parts of glyphs to overhang the
//                                  bounding rectangle.
//                                  By default glyphs are first aligned
//                                  inside the margines, then any glyphs which
//                                  still overhang the bounding box are
//                                  repositioned to avoid any overhang.
//                                  For example when an italic
//                                  lower case letter f in a font such as
//                                  Garamond is aligned at the far left of a
//                                  rectangle, the lower part of the f will
//                                  reach slightly further left than the left
//                                  edge of the rectangle. Setting this flag
//                                  will ensure the character aligns visually
//                                  with the lines above and below, but may
//                                  cause some pixels outside the formatting
//                                  rectangle to be clipped or painted.
//
//  DisplayFormatControl          - Causes control characters such as the
//                                  left-to-right mark to be shown in the
//                                  output with a representative glyph.
//
//  NoFontFallback                - Disables fallback to alternate fonts for
//                                  characters not supported in the requested
//                                  font. Any missing characters will be
//                                  be displayed with the fonts missing glyph,
//                                  usually an open square.
//
//  NoWrap                        - Disables wrapping of text between lines
//                                  when formatting within a rectangle.
//                                  NoWrap is implied when a point is passed
//                                  instead of a rectangle, or when the
//                                  specified rectangle has a zero line length.
//
//  NoClip                        - By default text is clipped to the
//                                  formatting rectangle. Setting NoClip
//                                  allows overhanging pixels to affect the
//                                  device outside the formatting rectangle.
//                                  Pixels at the end of the line may be
//                                  affected if the glyphs overhang their
//                                  cells, and either the NoFitBlackBox flag
//                                  has been set, or the glyph extends to far
//                                  to be fitted.
//                                  Pixels above/before the first line or
//                                  below/after the last line may be affected
//                                  if the glyphs extend beyond their cell
//                                  ascent / descent. This can occur rarely
//                                  with unusual diacritic mark combinations.

//---------------------------------------------------------------------------

type
  TIGPStringFormatFlags = Integer;
  const
    GPStringFormatFlagsDirectionRightToLeft        = $00000001;
    GPStringFormatFlagsDirectionVertical           = $00000002;
    GPStringFormatFlagsNoFitBlackBox               = $00000004;
    GPStringFormatFlagsDisplayFormatControl        = $00000020;
    GPStringFormatFlagsNoFontFallback              = $00000400;
    GPStringFormatFlagsMeasureTrailingSpaces       = $00000800;
    GPStringFormatFlagsNoWrap                      = $00001000;
    GPStringFormatFlagsLineLimit                   = $00002000;

    GPStringFormatFlagsNoClip                      = $00004000;

//---------------------------------------------------------------------------
// TIGPStringTrimming
//---------------------------------------------------------------------------

type
  TIGPStringTrimming = (
    StringTrimmingNone,
    StringTrimmingCharacter,
    StringTrimmingWord,
    StringTrimmingEllipsisCharacter,
    StringTrimmingEllipsisWord,
    StringTrimmingEllipsisPath
  );

//---------------------------------------------------------------------------
// National language digit substitution
//---------------------------------------------------------------------------

  TIGPStringDigitSubstitute = (
    StringDigitSubstituteUser,          // As NLS setting
    StringDigitSubstituteNone,
    StringDigitSubstituteNational,
    StringDigitSubstituteTraditional
  );

  PGPStringDigitSubstitute = ^TIGPStringDigitSubstitute;

//---------------------------------------------------------------------------
// Hotkey prefix interpretation
//---------------------------------------------------------------------------

  TIGPHotkeyPrefix = (
    HotkeyPrefixNone,
    HotkeyPrefixShow,
    HotkeyPrefixHide
  );

//---------------------------------------------------------------------------
// String alignment flags
//---------------------------------------------------------------------------

  TIGPStringAlignment = (
    // Left edge for left-to-right text,
    // right for right-to-left text,
    // and top for vertical
    StringAlignmentNear,
    StringAlignmentCenter,
    StringAlignmentFar
  );

//---------------------------------------------------------------------------
// DriverStringOptions
//---------------------------------------------------------------------------

  TIGPDriverStringOptions = Integer;
  const
    DriverStringOptionsCmapLookup             = 1;
    DriverStringOptionsVertical               = 2;
    DriverStringOptionsRealizedAdvance        = 4;
    DriverStringOptionsLimitSubpixel          = 8;

//---------------------------------------------------------------------------
// Flush Intention flags
//---------------------------------------------------------------------------

type
  TIGPFlushIntention = (
    FlushIntentionFlush,  // Flush all batched rendering operations
    FlushIntentionSync    // Flush all batched rendering operations
                          // and wait for them to complete
  );

//---------------------------------------------------------------------------
// Image encoder parameter related types
//---------------------------------------------------------------------------

  TIGPEncoderParameterValueType = Integer;
  const
    EncoderParameterValueTypeByte          : Integer = 1;    // 8-bit unsigned int
    EncoderParameterValueTypeASCII         : Integer = 2;    // 8-bit byte containing one 7-bit ASCII
                                                             // code. NULL terminated.
    EncoderParameterValueTypeShort         : Integer = 3;    // 16-bit unsigned int
    EncoderParameterValueTypeLong          : Integer = 4;    // 32-bit unsigned int
    EncoderParameterValueTypeRational      : Integer = 5;    // Two Longs. The first Long is the
                                                             // numerator, the second Long expresses the
                                                             // denomintor.
    EncoderParameterValueTypeLongRange     : Integer = 6;    // Two longs which specify a range of
                                                             // integer values. The first Long specifies
                                                             // the lower end and the second one
                                                             // specifies the higher end. All values
                                                             // are inclusive at both ends
    EncoderParameterValueTypeUndefined     : Integer = 7;    // 8-bit byte that can take any value
                                                             // depending on field definition
    EncoderParameterValueTypeRationalRange : Integer = 8;    // Two Rationals. The first Rational
                                                             // specifies the lower end and the second
                                                             // specifies the higher end. All values
                                                             // are inclusive at both ends
//---------------------------------------------------------------------------
// Image encoder value types
//---------------------------------------------------------------------------

type
  TIGPEncoderValue = (
    EncoderValueColorTypeCMYK,
    EncoderValueColorTypeYCCK,
    EncoderValueCompressionLZW,
    EncoderValueCompressionCCITT3,
    EncoderValueCompressionCCITT4,
    EncoderValueCompressionRle,
    EncoderValueCompressionNone,
    EncoderValueScanMethodInterlaced,
    EncoderValueScanMethodNonInterlaced,
    EncoderValueVersionGif87,
    EncoderValueVersionGif89,
    EncoderValueRenderProgressive,
    EncoderValueRenderNonProgressive,
    EncoderValueTransformRotate90,
    EncoderValueTransformRotate180,
    EncoderValueTransformRotate270,
    EncoderValueTransformFlipHorizontal,
    EncoderValueTransformFlipVertical,
    EncoderValueMultiFrame,
    EncoderValueLastFrame,
    EncoderValueFlush,
    EncoderValueFrameDimensionTime,
    EncoderValueFrameDimensionResolution,
    EncoderValueFrameDimensionPage
  );

//---------------------------------------------------------------------------
// Conversion of Emf To WMF Bits flags
//---------------------------------------------------------------------------
  TIGPEmfToWmfBitsFlags = (
    EmfToWmfBitsFlagsDefault          = $00000000,
    EmfToWmfBitsFlagsEmbedEmf         = $00000001,
    EmfToWmfBitsFlagsIncludePlaceable = $00000002,
    EmfToWmfBitsFlagsNoXORClip        = $00000004
  );
(**************************************************************************\
*
*   GDI+ Types
*
\**************************************************************************)

//--------------------------------------------------------------------------
// Callback functions
//--------------------------------------------------------------------------

type
  {$IFDEF DCC}TIGPImageAbortProc = reference to function() : Boolean;{$ENDIF}
  {$IFDEF FPC}TIGPImageAbortProc = function() : Boolean;{$ENDIF}
  TIGPDrawImageAbortProc         = TIGPImageAbortProc;
  TIGPGetThumbnailImageAbortProc = TIGPImageAbortProc;

  // Callback for EnumerateMetafile methods.  The parameters are:

  //      recordType      WMF, EMF, or EMF+ record type
  //      flags           (always 0 for WMF/EMF records)
  //      dataSize        size of the record data (in bytes), or 0 if no data
  //      data            pointer to the record data, or NULL if no data
  //      callbackData    pointer to callbackData, if any

  // This method can then call Metafile::PlayRecord to play the
  // record that was just enumerated.  If this method  returns
  // FALSE, the enumeration process is aborted.  Otherwise, it continues.

  TIGPEnumerateMetafileProc = {$IFDEF DCC}reference to {$ENDIF}function( recordType: TIGPEmfPlusRecordType; flags: UINT;
    dataSize: UINT; data: PBYTE ) : Boolean;

//--------------------------------------------------------------------------
// Primitive data types
//
// NOTE:
//  Types already defined in standard header files:
//      INT8
//      UINT8
//      INT16
//      UINT16
//      INT32
//      UINT32
//      INT64
//      UINT64
//
//  Avoid using the following types:
//      LONG - use INT
//      ULONG - use UINT
//      DWORD - use UINT32
//--------------------------------------------------------------------------

const
  { from float.h }
  FLT_MAX =  3.402823466e+38; // max value
  {$EXTERNALSYM FLT_MAX}
  FLT_MIN =  1.175494351e-38; // min positive value
  {$EXTERNALSYM FLT_MIN}

  REAL_MAX           = FLT_MAX;
  {$EXTERNALSYM REAL_MAX}
  REAL_MIN           = FLT_MIN;
  {$EXTERNALSYM REAL_MIN}
  REAL_TOLERANCE     = (FLT_MIN * 100);
  {$EXTERNALSYM REAL_TOLERANCE}
  REAL_EPSILON       = 1.192092896e-07;        // FLT_EPSILON
  {$EXTERNALSYM REAL_EPSILON}

//--------------------------------------------------------------------------
// Status return values from GDI+ methods
//--------------------------------------------------------------------------
type
  TIGPStatus = (
    Ok,
    GenericError,
    InvalidParameter,
    OutOfMemory,
    ObjectBusy,
    InsufficientBuffer,
    NotImplemented,
    Win32Error,
    WrongState,
    Aborted,
    FileNotFound,
    ValueOverflow,
    AccessDenied,
    UnknownImageFormat,
    FontFamilyNotFound,
    FontStyleNotFound,
    NotTrueTypeFont,
    UnsupportedGdiplusVersion,
    GdiplusNotInitialized,
    PropertyNotFound,
    PropertyNotSupported,
    ProfileNotFound
  );

  type EGPException = class(Exception);
  
(**************************************************************************\
*
*   GDI+ base memory allocation class
*
\**************************************************************************)

{
type
  TGdiplusBase = class( TInterfacedObject )
  protected
    class procedure ErrorCheck( AStatus : TIGPStatus );

  public
    class function NewInstance() : TObject; override;
    procedure FreeInstance(); override;

  end;
}
//--------------------------------------------------------------------------
// Represents a dimension in a 2D coordinate system (floating-point coordinates)
//--------------------------------------------------------------------------

type
  PGPSizeF = ^TIGPSizeF;
  TIGPSizeF = packed record
    Width  : Single;
    Height : Single;

  public
    constructor Create( _Width, _Height : Single ); overload;
    constructor Create( ASize : Single); overload;

  end;


//--------------------------------------------------------------------------
// Represents a dimension in a 2D coordinate system (integer coordinates)
//--------------------------------------------------------------------------

type
  PGPSize = ^TIGPSize;
  TIGPSize = packed record
    Width  : Integer;
    Height : Integer;

  public
    constructor Create( _Width, _Height : Integer ); overload;
    constructor Create( ASize : Integer ); overload;

  end;

//--------------------------------------------------------------------------
// Represents a location in a 2D coordinate system (integer coordinates)
//--------------------------------------------------------------------------

type
  PGPPoint = ^TPoint;

  TIGPPointHelper = record helper for TPoint
    class function Create( XY: Integer ) : TPoint; overload; static; inline;

  end;

//--------------------------------------------------------------------------
// Represents a location in a 2D coordinate system (floating-point coordinates)
//--------------------------------------------------------------------------

type
  PGPPointF = ^TPointF;

  TIGPPointFHelper = record helper for TPointF
    class function Create( XY: Single ) : TPointF; overload; static; inline;

  end;

//--------------------------------------------------------------------------
// Represents a rectangle in a 2D coordinate system (floating-point coordinates)
//--------------------------------------------------------------------------

type
  PGPRect = ^TIGPRect;
  TIGPRect = packed record
    X     : Integer;
    Y     : Integer;
    Width : Integer;
    Height: Integer;

  private
    function  GetPosition() : TPoint;
    procedure SetPosition( const AValue : TPoint );
    function  GetSize() : TIGPSize;
    procedure SetSize( const AValue : TIGPSize );

    function  GetCenter() : TPoint;
    procedure SetCenter( const AValue : TPoint );
    function  GetEndPoint() : TPoint;
    procedure SetEndPoint( const AValue : TPoint );

  public
    constructor Create( _x, _y, _width, _height : Integer ); overload;
    constructor Create( const location: TPoint; const size: TIGPSize ); overload;
    constructor Create( const Rect: TRect ); overload;

  public
    function  AsRect() : TRect; inline;

    function  HitTest( AX, AY : Integer ) : Boolean; overload;
    function  HitTest( const APoint : TPoint ) : Boolean; overload;
    function  HitTest( const APoint : TPointF ) : Boolean; overload;
    function  Intersects( const ARect : TIGPRect ) : Boolean;

  public
    property Position : TPoint    read GetPosition write SetPosition;
    property Size     : TIGPSize  read GetSize     write SetSize;

    property Center   : TPoint    read GetCenter   write SetCenter;
    property EndPoint : TPoint    read GetEndPoint write SetEndPoint;

  end;

  PGPRectF = ^TIGPRectF;
  TIGPRectF = packed record
    X     : Single;
    Y     : Single;
    Width : Single;
    Height: Single;

  private
    function  GetPosition() : TPointF;
    procedure SetPosition( const AValue : TPointF );
    function  GetSize() : TIGPSizeF;
    procedure SetSize( const AValue : TIGPSizeF );

    function  GetCenter() : TPointF;
    procedure SetCenter( const AValue : TPointF );
    function  GetEndPoint() : TPointF;
    procedure SetEndPoint( const AValue : TPointF );

  public
    constructor Create( _x, _y, _width, _height : Single ); overload;
    constructor Create( const location: TPointF; const size: TIGPSizeF); overload;
    constructor Create( const Rect: TRect ); overload;
    constructor Create( const Rect: TIGPRect ); overload;

  public
    function AsRect() : TRectF; inline;

    function  HitTest( AX, AY : Single ) : Boolean; overload;
    function  HitTest( const APoint : TPointF ) : Boolean; overload;
    function  HitTest( const APoint : TPoint ) : Boolean; overload;
    function  Intersects( const ARect : TIGPRectF ) : Boolean;

  public
    property Position : TPointF read GetPosition  write SetPosition;
    property Size     : TIGPSizeF  read GetSize      write SetSize;

    property Center   : TPointF read GetCenter    write SetCenter;
    property EndPoint : TPointF read GetEndPoint  write SetEndPoint;

  end;

  function GPInflateRect( const ARect: TIGPRect; CX, CY: Integer ) : TIGPRect; overload; inline;
  function GPInflateRect( const ARect: TIGPRect; Change: Integer ) : TIGPRect; overload; inline;
  function GPInflateRectF( const ARect: TIGPRectF; CX, CY: Single ) : TIGPRectF; overload; inline;
  function GPInflateRectF( const ARect: TIGPRectF; Change: Single ) : TIGPRectF; overload; inline;
  function GPIntersectRect( const ARect1 : TIGPRect; const ARect2 : TIGPRect ) : TIGPRect; inline;
  function GPCheckIntersectRect( const ARect1 : TIGPRect; const ARect2 : TIGPRect ) : Boolean; inline;
  function GPEqualRect( const ARect1 : TIGPRect; const ARect2 : TIGPRect ) : Boolean; inline;

type
  PGPCharacterRange = ^TIGPCharacterRange;
  TIGPCharacterRange = packed record
    First  : Integer;
    Length : Integer;
  end;

  function MakeCharacterRange(First, Length: Integer) : TIGPCharacterRange; inline;

(**************************************************************************
*
*   GDI+ Startup and Shutdown APIs
*
**************************************************************************)
type
  TIGPDebugEventLevel = (
    DebugEventLevelFatal,
    DebugEventLevelWarning
  );

  // Callback function that GDI+ can call, on debug builds, for assertions
  // and warnings.

  TIGPDebugEventProc = procedure(level: TIGPDebugEventLevel; message: PChar); stdcall;

  // Notification functions which the user must call appropriately if
  // "SuppressBackgroundThread" (below) is set.

  TIGPNotificationHookProc = function(out token: Pointer) : TIGPStatus; stdcall;
  TIGPNotificationUnhookProc = procedure(token: Pointer); stdcall;

  // Input structure for GdiplusStartup

{$IFDEF MSWINDOWS}
  TIGPGdiplusStartupInput = packed record
    GdiplusVersion          : Cardinal;       // Must be 1
    DebugEventCallback      : TIGPDebugEventProc; // Ignored on free builds
    SuppressBackgroundThread: BOOL;           // FALSE unless you're prepared to call
                                              // the hook/unhook functions properly
    SuppressExternalCodecs  : BOOL;           // FALSE unless you want GDI+ only to use
  end;
                                    // its internal image codecs.
  PGPGdiplusStartupInput = ^TIGPGdiplusStartupInput;

  // Output structure for GdiplusStartup()

  TIGPGdiplusStartupOutput = packed record
    // The following 2 fields are NULL if SuppressBackgroundThread is FALSE.
    // Otherwise, they are functions which must be called appropriately to
    // replace the background thread.
    //
    // These should be called on the application's main message loop - i.e.
    // a message loop which is active for the lifetime of GDI+.
    // "NotificationHook" should be called before starting the loop,
    // and "NotificationUnhook" should be called after the loop ends.

    NotificationHook  : TIGPNotificationHookProc;
    NotificationUnhook: TIGPNotificationUnhookProc;
  end;

  PGPGdiplusStartupOutput = ^TIGPGdiplusStartupOutput;
{$ENDIF} // MSWINDOWS

  // GDI+ initialization. Must not be called from DllMain - can cause deadlock.
  //
  // Must be called before GDI+ API's or constructors are used.
  //
  // token  - may not be NULL - accepts a token to be passed in the corresponding
  //          GdiplusShutdown call.
  // input  - may not be NULL
  // output - may be NULL only if input->SuppressBackgroundThread is FALSE.

(*
  {$EXTERNALSYM GdiplusStartup}
 function GdiplusStartup(out token: ULONG; input: PGdiplusStartupInput;
   output: PGdiplusStartupOutput) : TIGPStatus; stdcall;

  // GDI+ termination. Must be called before GDI+ is unloaded.
  // Must not be called from DllMain - can cause deadlock.
  //
  // GDI+ API's may not be called after GdiplusShutdown. Pay careful attention
  // to GDI+ object destructors.

  {$EXTERNALSYM GdiplusShutdown}
  procedure GdiplusShutdown(token: ULONG); stdcall;
*)

(**************************************************************************\
*
* Copyright (c) 1998-2001, Microsoft Corp.  All Rights Reserved.
* Module Name:
*   Gdiplus Pixel Formats
* Abstract:
*   GDI+ Pixel Formats
*
\**************************************************************************)

type
  TIGPPixelFormat = Integer;

const
  GPPixelFormatIndexed     = $00010000; // Indexes into a palette
  GPPixelFormatGDI         = $00020000; // Is a GDI-supported format
  GPPixelFormatAlpha       = $00040000; // Has an alpha component
  GPPixelFormatPAlpha      = $00080000; // Pre-multiplied alpha
  GPPixelFormatExtended    = $00100000; // Extended color 16 bits/channel
  GPPixelFormatCanonical   = $00200000;

  GPPixelFormatUndefined      = 0;
  GPPixelFormatDontCare       = 0;

  GPPixelFormat1bppIndexed    = (1  or ( 1 shl 8) or GPPixelFormatIndexed or GPPixelFormatGDI);
  GPPixelFormat4bppIndexed    = (2  or ( 4 shl 8) or GPPixelFormatIndexed or GPPixelFormatGDI);
  GPPixelFormat8bppIndexed    = (3  or ( 8 shl 8) or GPPixelFormatIndexed or GPPixelFormatGDI);
  GPPixelFormat16bppGrayScale = (4  or (16 shl 8) or GPPixelFormatExtended);
  GPPixelFormat16bppRGB555    = (5  or (16 shl 8) or GPPixelFormatGDI);
  GPPixelFormat16bppRGB565    = (6  or (16 shl 8) or GPPixelFormatGDI);
  GPPixelFormat16bppARGB1555  = (7  or (16 shl 8) or GPPixelFormatAlpha or GPPixelFormatGDI);
  GPPixelFormat24bppRGB       = (8  or (24 shl 8) or GPPixelFormatGDI);
  GPPixelFormat32bppRGB       = (9  or (32 shl 8) or GPPixelFormatGDI);
  GPPixelFormat32bppARGB      = (10 or (32 shl 8) or GPPixelFormatAlpha or GPPixelFormatGDI or GPPixelFormatCanonical);
  GPPixelFormat32bppPARGB     = (11 or (32 shl 8) or GPPixelFormatAlpha or GPPixelFormatPAlpha or GPPixelFormatGDI);
  GPPixelFormat48bppRGB       = (12 or (48 shl 8) or GPPixelFormatExtended);
  GPPixelFormat64bppARGB      = (13 or (64 shl 8) or GPPixelFormatAlpha  or GPPixelFormatCanonical or GPPixelFormatExtended);
  GPPixelFormat64bppPARGB     = (14 or (64 shl 8) or GPPixelFormatAlpha  or GPPixelFormatPAlpha or GPPixelFormatExtended);
  GPPixelFormatMax            = 15;

function GetPixelFormatSize(pixfmt: TIGPPixelFormat) : Cardinal;
function IsIndexedPixelFormat(pixfmt: TIGPPixelFormat) : Boolean;
function IsAlphaPixelFormat(pixfmt: TIGPPixelFormat) : Boolean;
function IsExtendedPixelFormat(pixfmt: TIGPPixelFormat) : Boolean;

//--------------------------------------------------------------------------
// Determine if the Pixel Format is Canonical format:
//   GPPixelFormat32bppARGB
//   GPPixelFormat32bppPARGB
//   GPPixelFormat64bppARGB
//   GPPixelFormat64bppPARGB
//--------------------------------------------------------------------------

function IsCanonicalPixelFormat(pixfmt: TIGPPixelFormat) : Boolean;

type
  TIGPPaletteFlags = (
    PaletteFlagsHasAlpha    = $0001,
    PaletteFlagsGrayScale   = $0002,
    PaletteFlagsHalftone    = $0004
  );

type
  TIGPColorPalette = packed record
    Flags  : UINT ;                 // Palette flags
    Count  : UINT ;                 // Number of color entries
    Entries: array [0..0] of TAlphaColor ; // Palette color entries
  end;

  PGPColorPalette = ^TIGPColorPalette;

(**************************************************************************\
*
*   GDI+ Color Object
*
\**************************************************************************)

//----------------------------------------------------------------------------
// Color mode
//----------------------------------------------------------------------------
{
  TIGPColorMode = (
    ColorModeARGB32,
    ColorModeARGB64
  );
}
//----------------------------------------------------------------------------
// Color Channel flags 
//----------------------------------------------------------------------------

  TIGPColorChannelFlags = (
    ColorChannelFlagsC,
    ColorChannelFlagsM,
    ColorChannelFlagsY,
    ColorChannelFlagsK,
    ColorChannelFlagsLast
  );

//----------------------------------------------------------------------------
// Color
//----------------------------------------------------------------------------

  // Common color constants
const
  aclAliceBlue            = $FFF0F8FF;
  aclAntiqueWhite         = $FFFAEBD7;
  aclAqua                 = $FF00FFFF;
  aclAquamarine           = $FF7FFFD4;
  aclAzure                = $FFF0FFFF;
  aclBeige                = $FFF5F5DC;
  aclBisque               = $FFFFE4C4;
  aclBlack                = $FF000000;
  aclBlanchedAlmond       = $FFFFEBCD;
  aclBlue                 = $FF0000FF;
  aclBlueViolet           = $FF8A2BE2;
  aclBrown                = $FFA52A2A;
  aclBurlyWood            = $FFDEB887;
  aclCadetBlue            = $FF5F9EA0;
  aclChartreuse           = $FF7FFF00;
  aclChocolate            = $FFD2691E;
  aclCoral                = $FFFF7F50;
  aclCornflowerBlue       = $FF6495ED;
  aclCornsilk             = $FFFFF8DC;
  aclCrimson              = $FFDC143C;
  aclCyan                 = $FF00FFFF;
  aclDarkBlue             = $FF00008B;
  aclDarkCyan             = $FF008B8B;
  aclDarkGoldenrod        = $FFB8860B;
  aclDarkGray             = $FFA9A9A9;
  aclDarkGreen            = $FF006400;
  aclDarkKhaki            = $FFBDB76B;
  aclDarkMagenta          = $FF8B008B;
  aclDarkOliveGreen       = $FF556B2F;
  aclDarkOrange           = $FFFF8C00;
  aclDarkOrchid           = $FF9932CC;
  aclDarkRed              = $FF8B0000;
  aclDarkSalmon           = $FFE9967A;
  aclDarkSeaGreen         = $FF8FBC8B;
  aclDarkSlateBlue        = $FF483D8B;
  aclDarkSlateGray        = $FF2F4F4F;
  aclDarkTurquoise        = $FF00CED1;
  aclDarkViolet           = $FF9400D3;
  aclDeepPink             = $FFFF1493;
  aclDeepSkyBlue          = $FF00BFFF;
  aclDimGray              = $FF696969;
  aclDodgerBlue           = $FF1E90FF;
  aclFirebrick            = $FFB22222;
  aclFloralWhite          = $FFFFFAF0;
  aclForestGreen          = $FF228B22;
  aclFuchsia              = $FFFF00FF;
  aclGainsboro            = $FFDCDCDC;
  aclGhostWhite           = $FFF8F8FF;
  aclGold                 = $FFFFD700;
  aclGoldenrod            = $FFDAA520;
  aclGray                 = $FF808080;
  aclGreen                = $FF008000;
  aclGreenYellow          = $FFADFF2F;
  aclHoneydew             = $FFF0FFF0;
  aclHotPink              = $FFFF69B4;
  aclIndianRed            = $FFCD5C5C;
  aclIndigo               = $FF4B0082;
  aclIvory                = $FFFFFFF0;
  aclKhaki                = $FFF0E68C;
  aclLavender             = $FFE6E6FA;
  aclLavenderBlush        = $FFFFF0F5;
  aclLawnGreen            = $FF7CFC00;
  aclLemonChiffon         = $FFFFFACD;
  aclLightBlue            = $FFADD8E6;
  aclLightCoral           = $FFF08080;
  aclLightCyan            = $FFE0FFFF;
  aclLightGoldenrodYellow = $FFFAFAD2;
  aclLightGray            = $FFD3D3D3;
  aclLightGreen           = $FF90EE90;
  aclLightPink            = $FFFFB6C1;
  aclLightSalmon          = $FFFFA07A;
  aclLightSeaGreen        = $FF20B2AA;
  aclLightSkyBlue         = $FF87CEFA;
  aclLightSlateGray       = $FF778899;
  aclLightSteelBlue       = $FFB0C4DE;
  aclLightYellow          = $FFFFFFE0;
  aclLime                 = $FF00FF00;
  aclLimeGreen            = $FF32CD32;
  aclLinen                = $FFFAF0E6;
  aclMagenta              = $FFFF00FF;
  aclMaroon               = $FF800000;
  aclMediumAquamarine     = $FF66CDAA;
  aclMediumBlue           = $FF0000CD;
  aclMediumOrchid         = $FFBA55D3;
  aclMediumPurple         = $FF9370DB;
  aclMediumSeaGreen       = $FF3CB371;
  aclMediumSlateBlue      = $FF7B68EE;
  aclMediumSpringGreen    = $FF00FA9A;
  aclMediumTurquoise      = $FF48D1CC;
  aclMediumVioletRed      = $FFC71585;
  aclMidnightBlue         = $FF191970;
  aclMintCream            = $FFF5FFFA;
  aclMistyRose            = $FFFFE4E1;
  aclMoccasin             = $FFFFE4B5;
  aclNavajoWhite          = $FFFFDEAD;
  aclNavy                 = $FF000080;
  aclOldLace              = $FFFDF5E6;
  aclOlive                = $FF808000;
  aclOliveDrab            = $FF6B8E23;
  aclOrange               = $FFFFA500;
  aclOrangeRed            = $FFFF4500;
  aclOrchid               = $FFDA70D6;
  aclPaleGoldenrod        = $FFEEE8AA;
  aclPaleGreen            = $FF98FB98;
  aclPaleTurquoise        = $FFAFEEEE;
  aclPaleVioletRed        = $FFDB7093;
  aclPapayaWhip           = $FFFFEFD5;
  aclPeachPuff            = $FFFFDAB9;
  aclPeru                 = $FFCD853F;
  aclPink                 = $FFFFC0CB;
  aclPlum                 = $FFDDA0DD;
  aclPowderBlue           = $FFB0E0E6;
  aclPurple               = $FF800080;
  aclRed                  = $FFFF0000;
  aclRosyBrown            = $FFBC8F8F;
  aclRoyalBlue            = $FF4169E1;
  aclSaddleBrown          = $FF8B4513;
  aclSalmon               = $FFFA8072;
  aclSandyBrown           = $FFF4A460;
  aclSeaGreen             = $FF2E8B57;
  aclSeaShell             = $FFFFF5EE;
  aclSienna               = $FFA0522D;
  aclSilver               = $FFC0C0C0;
  aclSkyBlue              = $FF87CEEB;
  aclSlateBlue            = $FF6A5ACD;
  aclSlateGray            = $FF708090;
  aclSnow                 = $FFFFFAFA;
  aclSpringGreen          = $FF00FF7F;
  aclSteelBlue            = $FF4682B4;
  aclTan                  = $FFD2B48C;
  aclTeal                 = $FF008080;
  aclThistle              = $FFD8BFD8;
  aclTomato               = $FFFF6347;
  aclTransparent          = $00000000;
  aclTurquoise            = $FF40E0D0;
  aclViolet               = $FFEE82EE;
  aclWheat                = $FFF5DEB3;
  aclWhite                = $FFFFFFFF;
  aclWhiteSmoke           = $FFF5F5F5;
  aclYellow               = $FFFFFF00;
  aclYellowGreen          = $FF9ACD32;

  // Shift count and bit mask for A, R, G, B components

  function  MakeARGBColor( AAlpha : Byte; AColor : TAlphaColor ) : TAlphaColor;
  function  MakeColor( AAlpha : Byte; AColor : TColor ) : TAlphaColor; overload;
  function  GPMakeGrayScaleColor( AAlpha : Byte; AColor : Byte ) : TAlphaColor;
  function  MakeColor( AColor : TColor ) : TAlphaColor; overload;
  function  MakeColor(r, g, b: Byte) : TAlphaColor; overload;
  function  MakeColor(a, r, g, b: Byte) : TAlphaColor; overload;
  function  GPMakeColor( AAlpha : Byte; AColor : TColor ) : TAlphaColor; overload;
  function  GPMakeColor( AColor : TColor ) : TAlphaColor; overload;
  function  GPMakeColor(r, g, b: Byte) : TAlphaColor; overload;
  function  GPMakeColor(a, r, g, b: Byte) : TAlphaColor; overload;
  function  GetAlpha(color: TAlphaColor) : BYTE;
  function  GetRed(color: TAlphaColor) : BYTE;
  function  GetGreen(color: TAlphaColor) : BYTE;
  function  GetBlue(color: TAlphaColor) : BYTE;
  function  RGBToBGR(color: TAlphaColor) : TAlphaColor;
{$IFDEF MSWINDOWS}
  function  ColorRefToARGB(rgb: COLORREF) : TAlphaColor;
  function  ARGBToColorRef(Color: TAlphaColor) : COLORREF;
{$ENDIF} //MSWINDOWS
  function  StringToRGBAColor( AValue : String ) : TAlphaColor;
  function  RGBAColorToString( AValue : TAlphaColor ) : String;
  procedure GetStandardRGBAColorNames( ANames : TStrings ); overload;
  procedure GetStandardRGBAColorNames( Proc: TGetStrProc ); overload;
  function  GPGetColor( AColor : TAlphaColor ) : TColor;
  function  HexToUInt( AValue : String ) : Cardinal;


(**************************************************************************\
*
*   GDI+ Metafile Related Structures
*
\**************************************************************************)

type
{$HPPEMIT '#pragma pack(push, 1)' }
{$HPPEMIT 'struct TIGPENHMETAHEADER3' }
(*$HPPEMIT '{' *)
{$HPPEMIT '  unsigned iType;' }
{$HPPEMIT '  unsigned nSize;' }
{$HPPEMIT '  RECT rclBounds;' }
{$HPPEMIT '  RECT rclFrame;' }
{$HPPEMIT '  unsigned dSignature;' }
{$HPPEMIT '  unsigned nVersion;' }
{$HPPEMIT '  unsigned nBytes;' }
{$HPPEMIT '  unsigned nRecords;' }
{$HPPEMIT '  Word nHandles;' }
{$HPPEMIT '  Word sReserved;' }
{$HPPEMIT '  unsigned nDescription;' }
{$HPPEMIT '  unsigned offDescription;' }
{$HPPEMIT '  unsigned nPalEntries;' }
{$HPPEMIT '  tagSIZE szlDevice;' }
{$HPPEMIT '  tagSIZE szlMillimeters;' }
(*$HPPEMIT '};' *)
{$HPPEMIT '#pragma pack(pop)' }

  {$EXTERNALSYM TIGPENHMETAHEADER3}
  TIGPENHMETAHEADER3 = packed record
    iType          : DWORD;  // Record type EMR_HEADER
    nSize          : DWORD;  // Record size in bytes.  This may be greater
                             // than the sizeof(ENHMETAHEADER).
    rclBounds      : TRect;  // Inclusive-inclusive bounds in device units
    rclFrame       : TRect;  // Inclusive-inclusive Picture Frame .01mm unit
    dSignature     : DWORD;  // Signature.  Must be ENHMETA_SIGNATURE.
    nVersion       : DWORD;  // Version number
    nBytes         : DWORD;  // Size of the metafile in bytes
    nRecords       : DWORD;  // Number of records in the metafile
    nHandles       : WORD;   // Number of handles in the handle table
                             // Handle index zero is reserved.
    sReserved      : WORD;   // Reserved.  Must be zero.
    nDescription   : DWORD;  // Number of chars in the unicode desc string
                             // This is 0 if there is no description string
    offDescription : DWORD;  // Offset to the metafile description record.
                             // This is 0 if there is no description string
    nPalEntries    : DWORD;  // Number of entries in the metafile palette.
    szlDevice      : TSize;  // Size of the reference device in pels
    szlMillimeters : TSize;  // Size of the reference device in millimeters
  end;
  PENHMETAHEADER3 = ^TIGPENHMETAHEADER3;

  // Placeable WMFs

  // Placeable Metafiles were created as a non-standard way of specifying how
  // a metafile is mapped and scaled on an output device.
  // Placeable metafiles are quite wide-spread, but not directly supported by
  // the Windows API. To playback a placeable metafile using the Windows API,
  // you will first need to strip the placeable metafile header from the file.
  // This is typically performed by copying the metafile to a temporary file
  // starting at file offset 22 (0x16). The contents of the temporary file may
  // then be used as input to the Windows GetMetaFile(), PlayMetaFile(),
  // CopyMetaFile(), etc. GDI functions.

  // Each placeable metafile begins with a 22-byte header,
  //  followed by a standard metafile:

  TPWMFRect16 = packed record
    Left   : INT16;
    Top    : INT16;
    Right  : INT16;
    Bottom : INT16;
  end;
  PPWMFRect16 = ^TPWMFRect16;

  TIGPWmfPlaceableFileHeader = packed record
    Key         : Cardinal;      // GDIP_WMF_PLACEABLEKEY
    Hmf         : INT16;       // Metafile HANDLE number (always 0)
    BoundingBox : TPWMFRect16;  // Coordinates in metafile units
    Inch        : INT16;       // Number of metafile units per inch
    Reserved    : Cardinal;      // Reserved (always 0)
    Checksum    : INT16;       // Checksum value for previous 10 WORDs
  end;
  
  PGPWmfPlaceableFileHeader = ^TIGPWmfPlaceableFileHeader;

  // Key contains a special identification value that indicates the presence
  // of a placeable metafile header and is always 0x9AC6CDD7.

  // Handle is used to stored the handle of the metafile in memory. When written
  // to disk, this field is not used and will always contains the value 0.

  // Left, Top, Right, and Bottom contain the coordinates of the upper-left
  // and lower-right corners of the image on the output device. These are
  // measured in twips.

  // A twip (meaning "twentieth of a point") is the logical unit of measurement
  // used in Windows Metafiles. A twip is equal to 1/1440 of an inch. Thus 720
  // twips equal 1/2 inch, while 32,768 twips is 22.75 inches.

  // Inch contains the number of twips per inch used to represent the image.
  // Normally, there are 1440 twips per inch; however, this number may be
  // changed to scale the image. A value of 720 indicates that the image is
  // double its normal size, or scaled to a factor of 2:1. A value of 360
  // indicates a scale of 4:1, while a value of 2880 indicates that the image
  // is scaled down in size by a factor of two. A value of 1440 indicates
  // a 1:1 scale ratio.

  // Reserved is not used and is always set to 0.

  // Checksum contains a checksum value for the previous 10 WORDs in the header.
  // This value can be used in an attempt to detect if the metafile has become
  // corrupted. The checksum is calculated by XORing each WORD value to an
  // initial value of 0.

  // If the metafile was recorded with a reference Hdc that was a display.

const
  GDIP_EMFPLUSFLAGS_DISPLAY      = $00000001;
  {$EXTERNALSYM GDIP_EMFPLUSFLAGS_DISPLAY}

(**************************************************************************\
*
*   GDI+ Imaging GUIDs
*
\**************************************************************************)

//---------------------------------------------------------------------------
// Image file format identifiers
//---------------------------------------------------------------------------

const
  ImageFormatUndefined : TGUID = '{b96b3ca9-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatUndefined}
  ImageFormatMemoryBMP : TGUID = '{b96b3caa-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatMemoryBMP}
  ImageFormatBMP       : TGUID = '{b96b3cab-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatBMP}
  ImageFormatEMF       : TGUID = '{b96b3cac-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatEMF}
  ImageFormatWMF       : TGUID = '{b96b3cad-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatWMF}
  ImageFormatJPEG      : TGUID = '{b96b3cae-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatJPEG}
  ImageFormatPNG       : TGUID = '{b96b3caf-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatPNG}
  ImageFormatGIF       : TGUID = '{b96b3cb0-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatGIF}
  ImageFormatTIFF      : TGUID = '{b96b3cb1-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatTIFF}
  ImageFormatEXIF      : TGUID = '{b96b3cb2-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatEXIF}
  ImageFormatIcon      : TGUID = '{b96b3cb5-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatIcon}

//---------------------------------------------------------------------------
// Predefined multi-frame dimension IDs
//---------------------------------------------------------------------------

  FrameDimensionTime       : TGUID = '{6aedbd6d-3fb5-418a-83a6-7f45229dc872}';
  {$EXTERNALSYM FrameDimensionTime}
  FrameDimensionResolution : TGUID = '{84236f7b-3bd3-428f-8dab-4ea1439ca315}';
  {$EXTERNALSYM FrameDimensionResolution}
  FrameDimensionPage       : TGUID = '{7462dc86-6180-4c7e-8e3f-ee7333a7a483}';
  {$EXTERNALSYM FrameDimensionPage}

//---------------------------------------------------------------------------
// Property sets
//---------------------------------------------------------------------------

  FormatIDImageInformation : TGUID = '{e5836cbe-5eef-4f1d-acde-ae4c43b608ce}';
  {$EXTERNALSYM FormatIDImageInformation}
  FormatIDJpegAppHeaders   : TGUID = '{1c4afdcd-6177-43cf-abc7-5f51af39ee85}';
  {$EXTERNALSYM FormatIDJpegAppHeaders}

//---------------------------------------------------------------------------
// Encoder parameter sets
//---------------------------------------------------------------------------

  EncoderCompression      : TGUID = '{e09d739d-ccd4-44ee-8eba-3fbf8be4fc58}';
  {$EXTERNALSYM EncoderCompression}
  EncoderColorDepth       : TGUID = '{66087055-ad66-4c7c-9a18-38a2310b8337}';
  {$EXTERNALSYM EncoderColorDepth}
  EncoderScanMethod       : TGUID = '{3a4e2661-3109-4e56-8536-42c156e7dcfa}';
  {$EXTERNALSYM EncoderScanMethod}
  EncoderVersion          : TGUID = '{24d18c76-814a-41a4-bf53-1c219cccf797}';
  {$EXTERNALSYM EncoderVersion}
  EncoderRenderMethod     : TGUID = '{6d42c53a-229a-4825-8bb7-5c99e2b9a8b8}';
  {$EXTERNALSYM EncoderRenderMethod}
  EncoderQuality          : TGUID = '{1d5be4b5-fa4a-452d-9cdd-5db35105e7eb}';
  {$EXTERNALSYM EncoderQuality}
  EncoderTransformation   : TGUID = '{8d0eb2d1-a58e-4ea8-aa14-108074b7b6f9}';
  {$EXTERNALSYM EncoderTransformation}
  EncoderLuminanceTable   : TGUID = '{edb33bce-0266-4a77-b904-27216099e717}';
  {$EXTERNALSYM EncoderLuminanceTable}
  EncoderChrominanceTable : TGUID = '{f2e455dc-09b3-4316-8260-676ada32481c}';
  {$EXTERNALSYM EncoderChrominanceTable}
  EncoderSaveFlag         : TGUID = '{292266fc-ac40-47bf-8cfc-a85b89a655de}';
  {$EXTERNALSYM EncoderSaveFlag}

  CodecIImageBytes : TGUID = '{025d1823-6c7d-447b-bbdb-a3cbc3dfa2fc}';
  {$EXTERNALSYM CodecIImageBytes}

type
  IGPImageBytes = Interface(IUnknown)
    ['{025D1823-6C7D-447B-BBDB-A3CBC3DFA2FC}']
    // Return total number of bytes in the IStream
    function CountBytes(out pcb: UINT) : HRESULT; stdcall;
    // Locks "cb" bytes, starting from "ulOffset" in the stream, and returns the
    // pointer to the beginning of the locked memory chunk in "ppvBytes"
    function LockBytes(cb: UINT; ulOffset: ULONG; out ppvBytes: Pointer) : HRESULT; stdcall;
    // Unlocks "cb" bytes, pointed by "pvBytes", starting from "ulOffset" in the
    // stream
    function UnlockBytes(pvBytes: Pointer; cb: UINT; ulOffset: ULONG) : HRESULT; stdcall;
  end;

//--------------------------------------------------------------------------
// ImageCodecInfo structure
//--------------------------------------------------------------------------

  TIGPImageCodecInfo = packed record
{$IFDEF MSWINDOWS}
    Clsid             : TGUID;
    FormatID          : TGUID;
    CodecName         : PWCHAR;
    DllName           : PWCHAR;
    FormatDescription : PWCHAR;
    FilenameExtension : PWCHAR;
    MimeType          : PWCHAR;
    Flags             : DWORD;
    Version           : DWORD;
    SigCount          : DWORD;
    SigSize           : DWORD;
    SigPattern        : PBYTE;
    SigMask           : PBYTE;
{$ENDIF} //MSWINDOWS
  end;

  PGPImageCodecInfo = ^TIGPImageCodecInfo;

//--------------------------------------------------------------------------
// Information flags about image codecs
//--------------------------------------------------------------------------
  {$EXTERNALSYM TIGPImageCodecFlags}

  TIGPImageCodecFlags = (
    ImageCodecFlagsEncoder            = $00000001,
    ImageCodecFlagsDecoder            = $00000002,
    ImageCodecFlagsSupportBitmap      = $00000004,
    ImageCodecFlagsSupportVector      = $00000008,
    ImageCodecFlagsSeekableEncode     = $00000010,
    ImageCodecFlagsBlockingDecode     = $00000020,

    ImageCodecFlagsBuiltin            = $00010000,
    ImageCodecFlagsSystem             = $00020000,
    ImageCodecFlagsUser               = $00040000
  );

(*$HPPEMIT 'enum TIGPImageCodecFlags' *)
(*$HPPEMIT '{' *)
(*$HPPEMIT '    ImageCodecFlagsEncoder        = 0x00000001,' *)
(*$HPPEMIT '    ImageCodecFlagsDecoder        = 0x00000002,' *)
(*$HPPEMIT '    ImageCodecFlagsSupportBitmap  = 0x00000004,' *)
(*$HPPEMIT '    ImageCodecFlagsSupportVector  = 0x00000008,' *)
(*$HPPEMIT '    ImageCodecFlagsSeekableEncode = 0x00000010,' *)
(*$HPPEMIT '    ImageCodecFlagsBlockingDecode = 0x00000020,' *)
(*$HPPEMIT '' *)
(*$HPPEMIT '    ImageCodecFlagsBuiltin        = 0x00010000,' *)
(*$HPPEMIT '    ImageCodecFlagsSystem         = 0x00020000,' *)
(*$HPPEMIT '    ImageCodecFlagsUser           = 0x00040000' *)
(*$HPPEMIT '};' *)

//---------------------------------------------------------------------------
// Access modes used when calling Image::LockBits
//---------------------------------------------------------------------------

type
  TIGPImageLockMode = ( ImageLockModeRead, ImageLockModeWrite, ImageLockModeUserInputBuf );
//---------------------------------------------------------------------------
  TIGPImageLockModes = set of TIGPImageLockMode;

//---------------------------------------------------------------------------
// Information about image pixel data
//---------------------------------------------------------------------------

type
  TIGPBitmapDataRecord = packed record
    Width       : UINT;
    Height      : UINT;
    Stride      : Integer;
    PixelFormat : TIGPPixelFormat;
    Scan0       : Pointer;
    Reserved    : Pointer;
  end;

//---------------------------------------------------------------------------
// Image flags
//---------------------------------------------------------------------------
  TIGPRotateFlipType = (
    RotateNoneFlipNone = 0,
    Rotate90FlipNone   = 1,
    Rotate180FlipNone  = 2,
    Rotate270FlipNone  = 3,

    RotateNoneFlipX    = 4,
    Rotate90FlipX      = 5,
    Rotate180FlipX     = 6,
    Rotate270FlipX     = 7,

    RotateNoneFlipY    = Rotate180FlipX,
    Rotate90FlipY      = Rotate270FlipX,
    Rotate180FlipY     = RotateNoneFlipX,
    Rotate270FlipY     = Rotate90FlipX,

    RotateNoneFlipXY   = Rotate180FlipNone,
    Rotate90FlipXY     = Rotate270FlipNone,
    Rotate180FlipXY    = RotateNoneFlipNone,
    Rotate270FlipXY    = Rotate90FlipNone
  );

//---------------------------------------------------------------------------
// Encoder Parameter structure
//---------------------------------------------------------------------------

type
  TIGPEncoderParameter = packed record
    Guid           : TGUID;   // GUID of the parameter
    NumberOfValues : ULONG;   // Number of the parameter values
    DataType       : ULONG;   // Value type, like ValueTypeLONG  etc.
    Value          : Pointer; // A pointer to the parameter values
  end;
  
  PGPEncoderParameter = ^TIGPEncoderParameter;

//---------------------------------------------------------------------------
// Encoder Parameters structure
//---------------------------------------------------------------------------

  TIGPEncoderParameters = packed record
    Count     : UINT;               // Number of parameters in this structure
    Parameter : array[0..0] of TIGPEncoderParameter;  // Parameter values
  end;
  PGPEncoderParameters = ^TIGPEncoderParameters;

//---------------------------------------------------------------------------
// Property Item
//---------------------------------------------------------------------------

  TIGPPropertyItem = record // NOT PACKED !!
    id        : PROPID;  // ID of this property
    length    : ULONG;   // Length of the property value, in bytes
    DataType  : WORD;    // Type of the value, as one of TAG_TYPE_XXX
    value     : Pointer; // property value
  end;
  
  PGPPropertyItem = ^TIGPPropertyItem;

//---------------------------------------------------------------------------
// Image property types
//---------------------------------------------------------------------------

const
  GPPropertyTagTypeByte      : Integer =  1;
  GPPropertyTagTypeASCII     : Integer =  2;
  GPPropertyTagTypeShort     : Integer =  3;
  GPPropertyTagTypeLong      : Integer =  4;
  GPPropertyTagTypeRational  : Integer =  5;
  GPPropertyTagTypeUndefined : Integer =  7;
  GPPropertyTagTypeSLONG     : Integer =  9;
  GPPropertyTagTypeSRational : Integer = 10;

//---------------------------------------------------------------------------
// Image property ID tags
//---------------------------------------------------------------------------

  GPPropertyTagExifIFD            = $8769;
  GPPropertyTagGpsIFD             = $8825;

  GPPropertyTagNewSubfileType     = $00FE;
  GPPropertyTagSubfileType        = $00FF;
  GPPropertyTagImageWidth         = $0100;
  GPPropertyTagImageHeight        = $0101;
  GPPropertyTagBitsPerSample      = $0102;
  GPPropertyTagCompression        = $0103;
  GPPropertyTagPhotometricInterp  = $0106;
  GPPropertyTagThreshHolding      = $0107;
  GPPropertyTagCellWidth          = $0108;
  GPPropertyTagCellHeight         = $0109;
  GPPropertyTagFillOrder          = $010A;
  GPPropertyTagDocumentName       = $010D;
  GPPropertyTagImageDescription   = $010E;
  GPPropertyTagEquipMake          = $010F;
  GPPropertyTagEquipModel         = $0110;
  GPPropertyTagStripOffsets       = $0111;
  GPPropertyTagOrientation        = $0112;
  GPPropertyTagSamplesPerPixel    = $0115;
  GPPropertyTagRowsPerStrip       = $0116;
  GPPropertyTagStripBytesCount    = $0117;
  GPPropertyTagMinSampleValue     = $0118;
  GPPropertyTagMaxSampleValue     = $0119;
  GPPropertyTagXResolution        = $011A;   // Image resolution in width direction
  GPPropertyTagYResolution        = $011B;   // Image resolution in height direction
  GPPropertyTagPlanarConfig       = $011C;   // Image data arrangement
  GPPropertyTagPageName           = $011D;
  GPPropertyTagXPosition          = $011E;
  GPPropertyTagYPosition          = $011F;
  GPPropertyTagFreeOffset         = $0120;
  GPPropertyTagFreeByteCounts     = $0121;
  GPPropertyTagGrayResponseUnit   = $0122;
  GPPropertyTagGrayResponseCurve  = $0123;
  GPPropertyTagT4Option           = $0124;
  GPPropertyTagT6Option           = $0125;
  GPPropertyTagResolutionUnit     = $0128;   // Unit of X and Y resolution
  GPPropertyTagPageNumber         = $0129;
  GPPropertyTagTransferFuncition  = $012D;
  GPPropertyTagSoftwareUsed       = $0131;
  GPPropertyTagDateTime           = $0132;
  GPPropertyTagArtist             = $013B;
  GPPropertyTagHostComputer       = $013C;
  GPPropertyTagPredictor          = $013D;
  GPPropertyTagWhitePoint         = $013E;
  GPPropertyTagPrimaryChromaticities = $013F;
  GPPropertyTagColorMap           = $0140;
  GPPropertyTagHalftoneHints      = $0141;
  GPPropertyTagTileWidth          = $0142;
  GPPropertyTagTileLength         = $0143;
  GPPropertyTagTileOffset         = $0144;
  GPPropertyTagTileByteCounts     = $0145;
  GPPropertyTagInkSet             = $014C;
  GPPropertyTagInkNames           = $014D;
  GPPropertyTagNumberOfInks       = $014E;
  GPPropertyTagDotRange           = $0150;
  GPPropertyTagTargetPrinter      = $0151;
  GPPropertyTagExtraSamples       = $0152;
  GPPropertyTagSampleFormat       = $0153;
  GPPropertyTagSMinSampleValue    = $0154;
  GPPropertyTagSMaxSampleValue    = $0155;
  GPPropertyTagTransferRange      = $0156;

  GPPropertyTagJPEGProc               = $0200;
  GPPropertyTagJPEGInterFormat        = $0201;
  GPPropertyTagJPEGInterLength        = $0202;
  GPPropertyTagJPEGRestartInterval    = $0203;
  GPPropertyTagJPEGLosslessPredictors = $0205;
  GPPropertyTagJPEGPointTransforms    = $0206;
  GPPropertyTagJPEGQTables            = $0207;
  GPPropertyTagJPEGDCTables           = $0208;
  GPPropertyTagJPEGACTables           = $0209;

  GPPropertyTagYCbCrCoefficients  = $0211;
  GPPropertyTagYCbCrSubsampling   = $0212;
  GPPropertyTagYCbCrPositioning   = $0213;
  GPPropertyTagREFBlackWhite      = $0214;

  GPPropertyTagICCProfile         = $8773;   // This TAG is defined by ICC
                                           // for embedded ICC in TIFF
  GPPropertyTagGamma                = $0301;
  GPPropertyTagICCProfileDescriptor = $0302;
  GPPropertyTagSRGBRenderingIntent  = $0303;

  GPPropertyTagImageTitle         = $0320;
  GPPropertyTagCopyright          = $8298;

// Extra TAGs (Like Adobe Image Information tags etc.)

  GPPropertyTagResolutionXUnit           = $5001;
  GPPropertyTagResolutionYUnit           = $5002;
  GPPropertyTagResolutionXLengthUnit     = $5003;
  GPPropertyTagResolutionYLengthUnit     = $5004;
  GPPropertyTagPrintFlags                = $5005;
  GPPropertyTagPrintFlagsVersion         = $5006;
  GPPropertyTagPrintFlagsCrop            = $5007;
  GPPropertyTagPrintFlagsBleedWidth      = $5008;
  GPPropertyTagPrintFlagsBleedWidthScale = $5009;
  GPPropertyTagHalftoneLPI               = $500A;
  GPPropertyTagHalftoneLPIUnit           = $500B;
  GPPropertyTagHalftoneDegree            = $500C;
  GPPropertyTagHalftoneShape             = $500D;
  GPPropertyTagHalftoneMisc              = $500E;
  GPPropertyTagHalftoneScreen            = $500F;
  GPPropertyTagJPEGQuality               = $5010;
  GPPropertyTagGridSize                  = $5011;
  GPPropertyTagThumbnailFormat           = $5012;  // 1 = JPEG, 0 = RAW RGB
  GPPropertyTagThumbnailWidth            = $5013;
  GPPropertyTagThumbnailHeight           = $5014;
  GPPropertyTagThumbnailColorDepth       = $5015;
  GPPropertyTagThumbnailPlanes           = $5016;
  GPPropertyTagThumbnailRawBytes         = $5017;
  GPPropertyTagThumbnailSize             = $5018;
  GPPropertyTagThumbnailCompressedSize   = $5019;
  GPPropertyTagColorTransferFunction     = $501A;
  GPPropertyTagThumbnailData             = $501B;    // RAW thumbnail bits in
                                                   // JPEG format or RGB format
                                                   // depends on
                                                   // PropertyTagThumbnailFormat

  // Thumbnail related TAGs

  GPPropertyTagThumbnailImageWidth        = $5020;   // Thumbnail width
  GPPropertyTagThumbnailImageHeight       = $5021;   // Thumbnail height
  GPPropertyTagThumbnailBitsPerSample     = $5022;   // Number of bits per
                                                   // component
  GPPropertyTagThumbnailCompression       = $5023;   // Compression Scheme
  GPPropertyTagThumbnailPhotometricInterp = $5024;   // Pixel composition
  GPPropertyTagThumbnailImageDescription  = $5025;   // Image Tile
  GPPropertyTagThumbnailEquipMake         = $5026;   // Manufacturer of Image
                                                   // Input equipment
  GPPropertyTagThumbnailEquipModel        = $5027;   // Model of Image input
                                                   // equipment
  GPPropertyTagThumbnailStripOffsets    = $5028;  // Image data location
  GPPropertyTagThumbnailOrientation     = $5029;  // Orientation of image
  GPPropertyTagThumbnailSamplesPerPixel = $502A;  // Number of components
  GPPropertyTagThumbnailRowsPerStrip    = $502B;  // Number of rows per strip
  GPPropertyTagThumbnailStripBytesCount = $502C;  // Bytes per compressed
                                                // strip
  GPPropertyTagThumbnailResolutionX     = $502D;  // Resolution in width
                                                // direction
  GPPropertyTagThumbnailResolutionY     = $502E;  // Resolution in height
                                                // direction
  GPPropertyTagThumbnailPlanarConfig    = $502F;  // Image data arrangement
  GPPropertyTagThumbnailResolutionUnit  = $5030;  // Unit of X and Y
                                                // Resolution
  GPPropertyTagThumbnailTransferFunction = $5031;  // Transfer function
  GPPropertyTagThumbnailSoftwareUsed     = $5032;  // Software used
  GPPropertyTagThumbnailDateTime         = $5033;  // File change date and
                                                 // time
  GPPropertyTagThumbnailArtist          = $5034;  // Person who created the
                                                // image
  GPPropertyTagThumbnailWhitePoint      = $5035;  // White point chromaticity
  GPPropertyTagThumbnailPrimaryChromaticities = $5036;
                                                    // Chromaticities of
                                                    // primaries
  GPPropertyTagThumbnailYCbCrCoefficients = $5037; // Color space transforma-
                                                 // tion coefficients
  GPPropertyTagThumbnailYCbCrSubsampling = $5038;  // Subsampling ratio of Y
                                                 // to C
  GPPropertyTagThumbnailYCbCrPositioning = $5039;  // Y and C position
  GPPropertyTagThumbnailRefBlackWhite    = $503A;  // Pair of black and white
                                                 // reference values
  GPPropertyTagThumbnailCopyRight       = $503B;   // CopyRight holder

  GPPropertyTagLuminanceTable           = $5090;
  GPPropertyTagChrominanceTable         = $5091;

  GPPropertyTagFrameDelay               = $5100;
  GPPropertyTagLoopCount                = $5101;

  GPPropertyTagPixelUnit         = $5110;  // Unit specifier for pixel/unit
  GPPropertyTagPixelPerUnitX     = $5111;  // Pixels per unit in X
  GPPropertyTagPixelPerUnitY     = $5112;  // Pixels per unit in Y
  GPPropertyTagPaletteHistogram  = $5113;  // Palette histogram

  // EXIF specific tag

  GPPropertyTagExifExposureTime  = $829A;
  GPPropertyTagExifFNumber       = $829D;

  GPPropertyTagExifExposureProg  = $8822;
  GPPropertyTagExifSpectralSense = $8824;
  GPPropertyTagExifISOSpeed      = $8827;
  GPPropertyTagExifOECF          = $8828;

  GPPropertyTagExifVer           = $9000;
  GPPropertyTagExifDTOrig        = $9003; // Date & time of original
  GPPropertyTagExifDTDigitized   = $9004; // Date & time of digital data generation

  GPPropertyTagExifCompConfig    = $9101;
  GPPropertyTagExifCompBPP       = $9102;

  GPPropertyTagExifShutterSpeed  = $9201;
  GPPropertyTagExifAperture      = $9202;
  GPPropertyTagExifBrightness    = $9203;
  GPPropertyTagExifExposureBias  = $9204;
  GPPropertyTagExifMaxAperture   = $9205;
  GPPropertyTagExifSubjectDist   = $9206;
  GPPropertyTagExifMeteringMode  = $9207;
  GPPropertyTagExifLightSource   = $9208;
  GPPropertyTagExifFlash         = $9209;
  GPPropertyTagExifFocalLength   = $920A;
  GPPropertyTagExifMakerNote     = $927C;
  GPPropertyTagExifUserComment   = $9286;
  GPPropertyTagExifDTSubsec      = $9290;  // Date & Time subseconds
  GPPropertyTagExifDTOrigSS      = $9291;  // Date & Time original subseconds
  GPPropertyTagExifDTDigSS       = $9292;  // Date & TIme digitized subseconds

  GPPropertyTagExifFPXVer        = $A000;
  GPPropertyTagExifColorSpace    = $A001;
  GPPropertyTagExifPixXDim       = $A002;
  GPPropertyTagExifPixYDim       = $A003;
  GPPropertyTagExifRelatedWav    = $A004;  // related sound file
  GPPropertyTagExifInterop       = $A005;
  GPPropertyTagExifFlashEnergy   = $A20B;
  GPPropertyTagExifSpatialFR     = $A20C;  // Spatial Frequency Response
  GPPropertyTagExifFocalXRes     = $A20E;  // Focal Plane X Resolution
  GPPropertyTagExifFocalYRes     = $A20F;  // Focal Plane Y Resolution
  GPPropertyTagExifFocalResUnit  = $A210;  // Focal Plane Resolution Unit
  GPPropertyTagExifSubjectLoc    = $A214;
  GPPropertyTagExifExposureIndex = $A215;
  GPPropertyTagExifSensingMethod = $A217;
  GPPropertyTagExifFileSource    = $A300;
  GPPropertyTagExifSceneType     = $A301;
  GPPropertyTagExifCfaPattern    = $A302;

  GPPropertyTagGpsVer            = $0000;
  GPPropertyTagGpsLatitudeRef    = $0001;
  GPPropertyTagGpsLatitude       = $0002;
  GPPropertyTagGpsLongitudeRef   = $0003;
  GPPropertyTagGpsLongitude      = $0004;
  GPPropertyTagGpsAltitudeRef    = $0005;
  GPPropertyTagGpsAltitude       = $0006;
  GPPropertyTagGpsGpsTime        = $0007;
  GPPropertyTagGpsGpsSatellites  = $0008;
  GPPropertyTagGpsGpsStatus      = $0009;
  GPPropertyTagGpsGpsMeasureMode = $00A;
  GPPropertyTagGpsGpsDop         = $000B;  // Measurement precision
  GPPropertyTagGpsSpeedRef       = $000C;
  GPPropertyTagGpsSpeed          = $000D;
  GPPropertyTagGpsTrackRef       = $000E;
  GPPropertyTagGpsTrack          = $000F;
  GPPropertyTagGpsImgDirRef      = $0010;
  GPPropertyTagGpsImgDir         = $0011;
  GPPropertyTagGpsMapDatum       = $0012;
  GPPropertyTagGpsDestLatRef     = $0013;
  GPPropertyTagGpsDestLat        = $0014;
  GPPropertyTagGpsDestLongRef    = $0015;
  GPPropertyTagGpsDestLong       = $0016;
  GPPropertyTagGpsDestBearRef    = $0017;
  GPPropertyTagGpsDestBear       = $0018;
  GPPropertyTagGpsDestDistRef    = $0019;
  GPPropertyTagGpsDestDist       = $001A;

(**************************************************************************\
*
*  GDI+ Color Matrix object, used with Graphics.DrawImage
*
\**************************************************************************)

//----------------------------------------------------------------------------
// Color matrix
//----------------------------------------------------------------------------

type
  TIGPColorMatrix = packed array[0..4, 0..4] of Single;
  PGPColorMatrix = ^TIGPColorMatrix;

//----------------------------------------------------------------------------
// Color Matrix flags
//----------------------------------------------------------------------------

  TIGPColorMatrixFlags = (
    ColorMatrixFlagsDefault,
    ColorMatrixFlagsSkipGrays,
    ColorMatrixFlagsAltGray
  );

//----------------------------------------------------------------------------
// Color Adjust Type
//----------------------------------------------------------------------------

  TIGPColorAdjustType = (
    ColorAdjustTypeDefault,
    ColorAdjustTypeBitmap,
    ColorAdjustTypeBrush,
    ColorAdjustTypePen,
    ColorAdjustTypeText,
    ColorAdjustTypeCount,
    ColorAdjustTypeAny      // Reserved
  );

//----------------------------------------------------------------------------
// Color Map
//----------------------------------------------------------------------------

  TIGPColorMap = packed record
    oldColor: TAlphaColor;
    newColor: TAlphaColor;
  end;
  PGPColorMap = ^TIGPColorMap;

//---------------------------------------------------------------------------
// Private GDI+ classes for internal type checking
//---------------------------------------------------------------------------

  GpGraphics            = Pointer;

  GpBrush               = Pointer;
  GpTexture             = Pointer;
  GpSolidFill           = Pointer;
  GpLineGradient        = Pointer;
  GpPathGradient        = Pointer;
  GpHatch               = Pointer;

  GpPen                 = Pointer;
  GpCustomLineCap       = Pointer;
  GpAdjustableArrowCap  = Pointer;

  GpImage               = Pointer;
  GpBitmap              = Pointer;
  GpMetafile            = Pointer;
  GpImageAttributes     = Pointer;

  GpPath                = Pointer;
  GpRegion              = Pointer;
  GpPathIterator        = Pointer;

  GpFontFamily          = Pointer;
  GpFont                = Pointer;
  GpStringFormat        = Pointer;
  GpFontCollection      = Pointer;
  GpCachedBitmap        = Pointer;
  GpMatrix              = Pointer;

  TIGPBlend = record
    Position : Single;
    Value    : Single;
  end;

  TIGPInterpolationColor = record
    Position : Single;
    Color    : TAlphaColor;
  end;

  function MakeBlend( APosition : Single; AValue : Single ) : TIGPBlend;
  function MakeInterpolationColor( APosition : Single; AColor : TAlphaColor ) : TIGPInterpolationColor;

(**************************************************************************\
*
*   GDI+ Codec Image APIs
*
\**************************************************************************)

//--------------------------------------------------------------------------
// Codec Management APIs
//--------------------------------------------------------------------------

{$IFDEF MSWINDOWS}
  function GetImageDecodersSize(out numDecoders, size: Cardinal) : TIGPStatus;
  function GetImageDecoders() : TArray<TIGPImageCodecInfo>;
  function GetImageEncodersSize(out numEncoders, size: Cardinal) : TIGPStatus;
  function GetImageEncoders() : TArray<TIGPImageCodecInfo>;

  function GetEncoderClsid( format : String; var pClsid : TCLSID ) : Boolean;
{$ENDIF}


(**************************************************************************\
*
*   Private GDI+ header file.
*
\**************************************************************************)

//---------------------------------------------------------------------------
// GDI+ classes for forward reference
//---------------------------------------------------------------------------

type
  TIGPGraphics = class;
  TIGPBitmap = class;
{$IFDEF MSWINDOWS}
  TIGPPen = class;
  TIGPBrush = class;
  TIGPMatrix = class;
  TIGPMetafile = class;
  TIGPFontFamily = class;
  TIGPGraphicsPath = class;
  TIGPRegion = class;
  TIGPImage = class;
  TIGPHatchBrush = class;
  TIGPSolidBrush = class;
  TIGPLinearGradientBrush = class;
  TIGPPathGradientBrush = class;
  TIGPFont = class;
  TIGPFontCollection = class;
  TIGPInstalledFontCollection = class;
  TIGPPrivateFontCollection = class;
  TIGPImageAttributes = class;
  TIGPCachedBitmap = class;
  TIGPCustomLineCap = class;
  TIGPStringFormat = class;
  TIGPTextureBrush = class;
  TIGPGraphicsPathIterator = class;
  TIGPAdjustableArrowCap = class;
{$ELSE}
  TIGPPen = class
  end;

  TIGPBrush = class
  end;

  TIGPMatrix = class
  end;

  TIGPMetafile = class
  end;

  TIGPFontFamily = class
  end;

  TIGPGraphicsPath = class
  end;

  TIGPRegion = class
  end;

  TIGPImage = class
  end;

  TIGPHatchBrush = class
  end;

  TIGPSolidBrush = class
  end;

  TIGPLinearGradientBrush = class
  end;

  TIGPPathGradientBrush = class
  end;

  TIGPFont = class
  end;

  TIGPFontCollection = class
  end;

  TIGPInstalledFontCollection = class
  end;

  TIGPPrivateFontCollection = class
  end;

  TIGPImageAttributes = class
  end;

  TIGPCachedBitmap = class
  end;

  TIGPCustomLineCap = class
  end;

  TIGPStringFormat = class
  end;

  TIGPTextureBrush = class
  end;

  TIGPGraphicsPathIterator = class
  end;

  TIGPAdjustableArrowCap = class
  end;

{$ENDIF}

  IGPGraphics = interface;
  IGPMatrix = interface;
  IGPFontFamily = interface;
  IGPGraphicsPath = interface;
  IGPMetafile = interface;
  IGPFontCollection = interface;
  IGPTransformable = interface;

(**************************************************************************\
*
*   GDI+ base memory allocation class
*
\**************************************************************************)

{$IFDEF MSWINDOWS}
  TIGPBase = class( TInterfacedObject )
  protected
    class procedure ErrorCheck( AStatus : TIGPStatus );

  public
    class function NewInstance() : TObject; override;
    procedure FreeInstance(); override;

  end;
{$ENDIF}

  IGPPathData = interface
    ['{1CA67396-A73B-4621-830D-989DA20EBE36}']
    function GetCount()  : Integer;
    function GetPoints( Index : Integer ) : TPointF;
    function GetTypes( Index : Integer )  : TIGPPathPointType;

    property Count     : Integer   read GetCount;
    property Points[ Index : Integer ] : TPointF        read GetPoints;
    property Types[ Index : Integer ]  : TIGPPathPointType read GetTypes;
    
  end;
  
  IGPMetafileHeader = interface
    ['{3F6AC13B-46CD-4CA6-B5DE-ACD761649161}']
    
    function GetType() : TIGPMetafileType;
    function GetMetafileSize() : UINT;
    // If IsEmfPlus, this is the EMF+ version; else it is the WMF or EMF ver
    function GetVersion() : UINT;
     // Get the EMF+ flags associated with the metafile
    function GetEmfPlusFlags() : UINT;
    function GetDpiX() : Single;
    function GetDpiY() : Single;
    function GetBounds() : TIGPRect;
    // Is it any type of WMF (standard or Placeable Metafile)?
    function IsWmf() : Boolean;
    // Is this an Placeable Metafile?
    function IsWmfPlaceable() : Boolean;
    // Is this an EMF (not an EMF+)?
    function IsEmf() : Boolean;
    // Is this an EMF or EMF+ file?
    function IsEmfOrEmfPlus() : Boolean;
    // Is this an EMF+ file?
    function IsEmfPlus() : Boolean;
    // Is this an EMF+ dual (has dual, down-level records) file?
    function IsEmfPlusDual() : Boolean;
    // Is this an EMF+ only (no dual records) file?
    function IsEmfPlusOnly() : Boolean;
    // If it's an EMF+ file, was it recorded against a display Hdc?
    function IsDisplay() : Boolean;
    // Get the WMF header of the metafile (if it is a WMF)
    function GetWmfHeader() : PMetaHeader;
    // Get the EMF header of the metafile (if it is an EMF)
    function GetEmfHeader() : PENHMETAHEADER3;
    
    property MetafileSize : UINT    read GetMetafileSize;
    property Version      : UINT    read GetVersion;
    property DpiX         : Single  read GetDpiX;
    property DpiY         : Single  read GetDpiY;
    property Bounds       : TIGPRect read GetBounds;

  end;

(**************************************************************************\
*
*   GDI+ Region, Font, Image, CustomLineCap class definitions.
*
\**************************************************************************)

  IGPRegion = interface
    ['{ECAB7D08-39D0-47AA-8247-9DD3491485EA}']
    
    function GetNativeRegion() : GpRegion;
    
    function Clone() : TIGPRegion;
    function MakeInfinite() : TIGPRegion;
    function MakeEmpty() : TIGPRegion;
    function GetDataSize() : Cardinal;
    // buffer     - where to put the data
    // bufferSize - how big the buffer is (should be at least as big as GetDataSize())
    // sizeFilled - if not NULL, this is an OUT param that says how many bytes
    //              of data were written to the buffer.
    function GetData() : TArray<Byte>;
    function Intersect(const rect: TIGPRect) : TIGPRegion; overload;
    function IntersectF(const rect: TIGPRectF) : TIGPRegion;
    function Intersect(path: IGPGraphicsPath) : TIGPRegion; overload;
    function Intersect(region: IGPRegion) : TIGPRegion; overload;
    function Union(const rect: TIGPRect) : TIGPRegion; overload;
    function UnionF(const rect: TIGPRectF) : TIGPRegion;
    function Union(path: IGPGraphicsPath) : TIGPRegion; overload;
    function Union(region: IGPRegion) : TIGPRegion; overload;
    function XorRegion(const rect: TIGPRect) : TIGPRegion; overload;
    function XorRegionF(const rect: TIGPRectF) : TIGPRegion;
    function XorRegion(path: IGPGraphicsPath) : TIGPRegion; overload;
    function XorRegion(region: IGPRegion) : TIGPRegion; overload;
    function Exclude(const rect: TIGPRect) : TIGPRegion; overload;
    function ExcludeF(const rect: TIGPRectF) : TIGPRegion;
    function Exclude(path: IGPGraphicsPath) : TIGPRegion; overload;
    function Exclude(region: IGPRegion) : TIGPRegion; overload;
    function Complement(const rect: TIGPRect) : TIGPRegion; overload;
    function ComplementF(const rect: TIGPRectF) : TIGPRegion;
    function Complement(path: IGPGraphicsPath) : TIGPRegion; overload;
    function Complement(region: IGPRegion) : TIGPRegion; overload;
    function TranslateF(dx, dy: Single) : TIGPRegion;
    function Translate(dx, dy: Integer) : TIGPRegion;
    function Transform(matrix: IGPMatrix) : TIGPRegion;
    function GetBounds( g: IGPGraphics ) : TIGPRect;
    function GetBoundsF( g: IGPGraphics ) : TIGPRectF;
    function GetHRGN(g: IGPGraphics) : HRGN;
    function IsEmpty(g: IGPGraphics) : Boolean;
    function IsInfinite(g: IGPGraphics) : Boolean ;
    function IsVisible(x, y: Integer; g: IGPGraphics = NIL) : Boolean; overload;
    function IsVisible(const point: TPoint; g: IGPGraphics = NIL) : Boolean; overload;
    function IsVisibleF(x, y: Single; g: IGPGraphics = NIL) : Boolean; overload;
    function IsVisibleF(const point: TPointF; g: IGPGraphics = NIL) : Boolean; overload;
    function IsVisible(x, y, width, height: Integer; g: IGPGraphics) : Boolean; overload;
    function IsVisible(const rect: TIGPRect; g: IGPGraphics = NIL) : Boolean; overload;
    function IsVisibleF(x, y, width, height: Single; g: IGPGraphics = NIL) : Boolean; overload;
    function IsVisibleF(const rect: TIGPRectF; g: IGPGraphics = NIL) : Boolean; overload;
    function Equals(region: IGPRegion; g: IGPGraphics) : Boolean;
    function GetRegionScansCount(matrix: IGPMatrix) : Cardinal;
    function GetRegionScansF( matrix: IGPMatrix ) : TArray<TIGPRectF>;
    function GetRegionScans( matrix: IGPMatrix ) : TArray<TIGPRect>;
    
  end;
  
{$IFDEF MSWINDOWS}
  TIGPRegion = class( TIGPBase, IGPRegion )
  protected
    FNativeRegion: GpRegion;

  protected
    function GetNativeRegion() : GpRegion;
    procedure SetNativeRegion(nativeRegion: GpRegion);
    constructor CreateGdiPlus(nativeRegion: GpRegion; Dummy : Boolean );

  public
    constructor Create(); overload;
    constructor Create( const rect: TIGPRectF ); overload;
    constructor Create( const rect: TIGPRect ); overload;
    constructor Create(path: IGPGraphicsPath); overload;
    constructor Create( const regionData: array of BYTE ); overload;
    constructor Create(hRgn: HRGN); overload;
    destructor  Destroy(); override;

  public
    class function FromHRGN(hRgn: HRGN) : TIGPRegion;

  public
    function Clone() : TIGPRegion;
    function MakeInfinite() : TIGPRegion;
    function MakeEmpty() : TIGPRegion;
    function GetDataSize() : Cardinal;
    // buffer     - where to put the data
    // bufferSize - how big the buffer is (should be at least as big as GetDataSize())
    // sizeFilled - if not NULL, this is an OUT param that says how many bytes
    //              of data were written to the buffer.
    function GetData() : TArray<Byte>;
    function Intersect(const rect: TIGPRect) : TIGPRegion; overload;
    function IntersectF(const rect: TIGPRectF) : TIGPRegion;
    function Intersect(path: IGPGraphicsPath) : TIGPRegion; overload;
    function Intersect(region: IGPRegion) : TIGPRegion; overload;
    function Union(const rect: TIGPRect) : TIGPRegion; overload;
    function UnionF(const rect: TIGPRectF) : TIGPRegion;
    function Union(path: IGPGraphicsPath) : TIGPRegion; overload;
    function Union(region: IGPRegion) : TIGPRegion; overload;
    function XorRegion(const rect: TIGPRect) : TIGPRegion; overload;
    function XorRegionF(const rect: TIGPRectF) : TIGPRegion;
    function XorRegion(path: IGPGraphicsPath) : TIGPRegion; overload;
    function XorRegion(region: IGPRegion) : TIGPRegion; overload;
    function Exclude(const rect: TIGPRect) : TIGPRegion; overload;
    function ExcludeF(const rect: TIGPRectF) : TIGPRegion;
    function Exclude(path: IGPGraphicsPath) : TIGPRegion; overload;
    function Exclude(region: IGPRegion) : TIGPRegion; overload;
    function Complement(const rect: TIGPRect) : TIGPRegion; overload;
    function ComplementF(const rect: TIGPRectF) : TIGPRegion;
    function Complement(path: IGPGraphicsPath) : TIGPRegion; overload;
    function Complement(region: IGPRegion) : TIGPRegion; overload;
    function TranslateF(dx, dy: Single) : TIGPRegion;
    function Translate(dx, dy: Integer) : TIGPRegion;
    function Transform(matrix: IGPMatrix) : TIGPRegion;
    function GetBounds( g: IGPGraphics ) : TIGPRect;
    function GetBoundsF( g: IGPGraphics ) : TIGPRectF;
    function GetHRGN(g: IGPGraphics) : HRGN;
    function IsEmpty(g: IGPGraphics) : Boolean;
    function IsInfinite(g: IGPGraphics) : Boolean ;
    function IsVisible(x, y: Integer; g: IGPGraphics = NIL) : Boolean; overload;
    function IsVisible(const point: TPoint; g: IGPGraphics = NIL) : Boolean; overload;
    function IsVisibleF(x, y: Single; g: IGPGraphics = NIL) : Boolean; overload;
    function IsVisibleF(const point: TPointF; g: IGPGraphics = NIL) : Boolean; overload;
    function IsVisible(x, y, width, height: Integer; g: IGPGraphics) : Boolean; overload;
    function IsVisible(const rect: TIGPRect; g: IGPGraphics = NIL) : Boolean; overload;
    function IsVisibleF(x, y, width, height: Single; g: IGPGraphics = NIL) : Boolean; overload;
    function IsVisibleF(const rect: TIGPRectF; g: IGPGraphics = NIL) : Boolean; overload;
    function EqualsRegion(region: IGPRegion; g: IGPGraphics) : Boolean;
    function GetRegionScansCount(matrix: IGPMatrix) : Cardinal;
    function GetRegionScansF( matrix: IGPMatrix ) : TArray<TIGPRectF>;
    function GetRegionScans( matrix: IGPMatrix ) : TArray<TIGPRect>;

    function IGPRegion.Equals = EqualsRegion;
   
  end;
{$ENDIF}

//--------------------------------------------------------------------------
// FontFamily
//--------------------------------------------------------------------------

  IGPFontFamily = interface
    ['{4678D60A-EA61-410E-B543-AD0FEA23103A}']
    function GetFamilyName(language: LANGID = 0) : String;
    function Clone() : TIGPFontFamily;
    function IsAvailable() : Boolean;
    function IsStyleAvailable(style: Integer) : Boolean;
    function GetEmHeight(style: Integer) : UINT16;
    function GetCellAscent(style: Integer) : UINT16;
    function GetCellDescent(style: Integer) : UINT16;
    function GetLineSpacing(style: Integer) : UINT16;
    function GetNativeFamily() : GpFontFamily;
    
  end;
  
{$IFDEF MSWINDOWS}
  TIGPFontFamily = class(TIGPBase, IGPFontFamily)
  protected
    FNativeFamily : GpFontFamily;
    
  protected
    constructor CreateGdiPlus(nativeFamily: GpFontFamily; Dummy : Boolean);
    
  public
    constructor Create(); overload;
    constructor Create(name: WideString; fontCollection: IGPFontCollection = NIL); overload;
    destructor  Destroy(); override;
    
  public
    class function GenericSansSerif() : TIGPFontFamily;
    class function GenericSerif() : TIGPFontFamily;
    class function GenericMonospace() : TIGPFontFamily;
    
  public
    function GetFamilyName(language: LANGID = 0) : String;
    function Clone() : TIGPFontFamily;
    function IsAvailable() : Boolean;
    function IsStyleAvailable(style: Integer) : Boolean;
    function GetEmHeight(style: Integer) : UINT16;
    function GetCellAscent(style: Integer) : UINT16;
    function GetCellDescent(style: Integer) : UINT16;
    function GetLineSpacing(style: Integer) : UINT16;
    function GetNativeFamily() : GpFontFamily;
    
  end;
{$ENDIF}

//--------------------------------------------------------------------------
// Font Collection
//--------------------------------------------------------------------------

  IGPFontCollection = interface
    ['{856E57C8-CAF2-4824-8DBB-E82DDEABF0BC}']

    function GetNativeFontCollection() : GpFontCollection;
    function GetFamilyCount: Integer;
    function GetFamilies() : TArray<IGPFontFamily>;
    
  end;
  
{$IFDEF MSWINDOWS}
  TIGPFontCollection = class(TIGPBase, IGPFontCollection)
  protected
    FNativeFontCollection: GpFontCollection;
    function GetNativeFontCollection() : GpFontCollection;
    
  public
    constructor Create();
    destructor  Destroy(); override;
    
  public
    function GetFamilyCount() : Integer;
    function GetFamilies() : TArray<IGPFontFamily>;
    
  end;

  TIGPInstalledFontCollection = class(TIGPFontCollection)
  public
    constructor Create(); reintroduce;
    destructor  Destroy(); override;
    
  end;
{$ENDIF}

  IGPPrivateFontCollection = interface
    ['{AF596B35-2851-40AD-88E1-48CEB263314E}']

    function AddFontFile(filename: WideString) : TIGPPrivateFontCollection;
    function AddMemoryFont(memory: Pointer; length: Integer) : TIGPPrivateFontCollection;
    
  end;
  
{$IFDEF MSWINDOWS}
  TIGPPrivateFontCollection = class(TIGPFontCollection, IGPPrivateFontCollection)
  public
    constructor Create(); reintroduce;
    destructor  Destroy(); override;

  public
    function AddFontFile(filename: WideString) : TIGPPrivateFontCollection;
    function AddMemoryFont(memory: Pointer; length: Integer) : TIGPPrivateFontCollection;

  end;
{$ENDIF}

//--------------------------------------------------------------------------
// TFont
//--------------------------------------------------------------------------

  IGPFont = interface
    ['{034EF8BC-9EBD-4058-8C18-FFD8873E4883}']
    function  GetNativeFont() : GpFont; 
    
    function GetLogFontA( g : IGPGraphics ) : TLogFontA;
    function GetLogFontW( g : IGPGraphics ) : TLogFontW;
    function Clone() : TIGPFont;
    function IsAvailable() : Boolean;
    function GetStyle() : Integer;
    function GetSize() : Single;
    function GetUnit() : TIGPUnit;
    function GetHeight(graphics: IGPGraphics) : Single; overload;
    function GetHeight(dpi: Single) : Single; overload;
    function GetFamily() : IGPFontFamily;

    property Style  : Integer read GetStyle;
    property Size   : Single read GetSize;
    property Units  : TIGPUnit read GetUnit;
    property Family : IGPFontFamily read GetFamily;

  end;

{$IFDEF MSWINDOWS}
  TIGPFont = class( TIGPBase, IGPFont )
  protected
    FNativeFont: GpFont;
    
  protected
    procedure SetNativeFont(Font: GpFont);
    function  GetNativeFont() : GpFont; 
    constructor CreateGdiPlus(font: GpFont; Dummy : Boolean );
    
  public
    constructor Create(hdc: HDC); overload;
    constructor Create(hdc: HDC; logfont: PLogFontA); overload;
    constructor Create(hdc: HDC; logfont: PLogFontW); overload;
    constructor Create(hdc: HDC; hfont: HFONT); overload;
    constructor Create(family: IGPFontFamily; emSize: Single;
      style: TFontStyles = [];
      unit_: TIGPUnit = UnitPoint); overload;
    constructor Create(familyName: WideString; emSize: Single;
      style: TFontStyles = []; unit_: TIGPUnit = UnitPoint;
      fontCollection: IGPFontCollection = NIL); overload;
    destructor  Destroy(); override;
    
  public
    function GetLogFontA( g : IGPGraphics ) : TLogFontA;
    function GetLogFontW( g : IGPGraphics ) : TLogFontW;
    function Clone() : TIGPFont;
    function IsAvailable() : Boolean;
    function GetStyle() : Integer;
    function GetSize() : Single;
    function GetUnit() : TIGPUnit;
    function GetHeight(graphics: IGPGraphics) : Single; overload;
    function GetHeight(dpi: Single) : Single; overload;
    function GetFamily() : IGPFontFamily;
    
  end;
{$ENDIF}

//--------------------------------------------------------------------------
// Abstract base class for Image and Metafile
//--------------------------------------------------------------------------

  IGPImage = interface
    ['{3514B659-EAB2-4A2E-80F5-7A6AD9E2A64B}']
{$IFDEF MSWINDOWS}
    function GetNativeImage() : GpImage;
    function Clone() : TIGPImage;
    function Save(filename: WideString; const clsidEncoder: TGUID; encoderParams: PGPEncoderParameters = NIL) : TIGPImage; overload;
    function Save(stream: IStream; const clsidEncoder: TGUID; encoderParams: PGPEncoderParameters = NIL) : TIGPImage; overload;
    function Save(filename: WideString; const formatName : String = 'bmp' ) : TIGPImage; overload;
    function Save(stream: IStream; const formatName : String = 'bmp' ) : TIGPImage; overload;
    function Save(stream: TStream; const clsidEncoder: TGUID; encoderParams: PGPEncoderParameters = NIL) : TIGPImage; overload;
    function Save(stream: TStream; const formatName : String = 'bmp' ) : TIGPImage; overload;
    function SaveAdd(encoderParams: PGPEncoderParameters) : TIGPImage; overload;
    function SaveAdd(newImage: IGPImage; encoderParams: PGPEncoderParameters) : TIGPImage; overload;
    function GetType() : TIGPImageType;
    function GetPhysicalDimension() : TIGPSizeF;
    function GetBounds(out srcRect: TIGPRectF; out srcUnit: TIGPUnit) : TIGPImage;
    function AsBytes( const formatName : String = 'bmp' ) : TBytes; overload;
    function AsBytes( clsidEncoder: TGUID; encoderParams: PGPEncoderParameters = NIL ) : TBytes; overload;
    function GetWidth() : Cardinal;
    function GetHeight() : Cardinal;
    function GetHorizontalResolution() : Single;
    function GetVerticalResolution() : Single;
    function GetFlags() : Cardinal;
    function GetRawFormat() : TGUID;
    function GetFormatName() : String;
    function GetPixelFormat() : TIGPPixelFormat;
    function GetPaletteSize() : Integer;
    function GetPalette(palette: PGPColorPalette; size: Integer) : TIGPImage;
    function SetPalette(palette: PGPColorPalette) : TIGPImage;
    function GetThumbnailImage(thumbWidth, thumbHeight: Cardinal; callback: TIGPGetThumbnailImageAbortProc = NIL) : TIGPImage;
    function GetFrameDimensionsCount() : Cardinal;
    function GetFrameDimensionsList() : TArray<TGUID>;
    function GetFrameCount(const dimensionID: TGUID) : Cardinal;
    function SelectActiveFrame(const dimensionID: TGUID; frameIndex: Cardinal) : TIGPImage;
    function RotateFlip(rotateFlipType: TIGPRotateFlipType) : TIGPImage;
    function GetPropertyCount() : Cardinal;
    function GetPropertyIdList() : TArray<TPropID>;
    function GetPropertyItemSize(propId: PROPID) : Cardinal;
    function GetPropertyItem(propId: PROPID; propSize: Cardinal; buffer: PGPPropertyItem) : TIGPImage;
    function GetPropertySize(out totalBufferSize, numProperties : Cardinal) : TIGPImage;
    function GetAllPropertyItems(totalBufferSize, numProperties: Cardinal; allItems: PGPPropertyItem ) : TIGPImage;
    function RemovePropertyItem(propId: TPROPID) : TIGPImage;
    function SetPropertyItem(const item: TIGPPropertyItem) : TIGPImage;
    function GetEncoderParameterListSize(const clsidEncoder: TGUID) : Cardinal;
    function GetEncoderParameterList(const clsidEncoder: TGUID; size: Cardinal; buffer: PGPEncoderParameters) : TIGPImage;

    property Width                : Cardinal        read GetWidth;
    property Height               : Cardinal        read GetHeight;
    property PixelFormat          : TIGPPixelFormat  read GetPixelFormat;
    property ImageType            : TIGPImageType    read GetType;
    property FormatName           : String          read GetFormatName;
    property FrameDimensionsCount : Cardinal        read GetFrameDimensionsCount;
    property FrameDimensionsList  : TArray<TGUID>      read GetFrameDimensionsList;
    property HorizontalResolution : Single          read GetHorizontalResolution;
    property VerticalResolution   : Single          read GetVerticalResolution;
    property RawFormat            : TGUID           read GetRawFormat;
    property PhysicalDimension    : TIGPSizeF        read GetPhysicalDimension;
    property PropertyCount        : Cardinal        read GetPropertyCount;
    property PropertyIdList       : TArray<TPropID>  read GetPropertyIdList;
{$ENDIF}
  end;

{$IFDEF MSWINDOWS}
  TIGPImage = class( TIGPBase, IGPImage )
  protected
    FNativeImage: GpImage;

  protected
    procedure SetNativeImage(nativeImage: GpImage);
    function  GetNativeImage() : GpImage;

  protected
    constructor CreateGdiPlus(nativeImage: GpImage; Dummy : Boolean);

  public
    constructor Create(filename: WideString; useEmbeddedColorManagement: Boolean = False); overload;
    constructor Create(stream: IStream; useEmbeddedColorManagement: Boolean = False); overload;
    constructor Create(stream: TStream; useEmbeddedColorManagement: Boolean = False); overload;
    constructor Create(ABytes : TBytes; useEmbeddedColorManagement: Boolean = False); overload;
    destructor  Destroy(); override;

  public
    class function FromFile(filename: WideString; useEmbeddedColorManagement: Boolean = False) : TIGPImage;
    class function FromStream(stream: IStream; useEmbeddedColorManagement: Boolean = False) : TIGPImage;

  public
    function Clone() : TIGPImage;
    function Save(filename: WideString; const clsidEncoder: TGUID; encoderParams: PGPEncoderParameters = NIL) : TIGPImage; overload;
    function Save(stream: IStream; const clsidEncoder: TGUID; encoderParams: PGPEncoderParameters = NIL) : TIGPImage; overload;
    function Save(filename: WideString; const formatName : String ) : TIGPImage; overload;
    function Save(stream: IStream; const formatName : String ) : TIGPImage; overload;
    function Save(stream: TStream; const clsidEncoder: TGUID; encoderParams: PGPEncoderParameters = NIL) : TIGPImage; overload;
    function Save(stream: TStream; const formatName : String = 'bmp' ) : TIGPImage; overload;
    function SaveAdd(encoderParams: PGPEncoderParameters) : TIGPImage; overload;
    function SaveAdd(newImage: IGPImage; encoderParams: PGPEncoderParameters) : TIGPImage; overload;
    function GetType() : TIGPImageType;
    function GetPhysicalDimension() : TIGPSizeF;
    function GetBounds(out srcRect: TIGPRectF; out srcUnit: TIGPUnit) : TIGPImage;
    function AsBytes( const formatName : String = 'bmp' ) : TBytes; overload;
    function AsBytes( clsidEncoder: TGUID; encoderParams: PGPEncoderParameters = NIL ) : TBytes; overload;
    function GetWidth() : Cardinal;
    function GetHeight() : Cardinal;
    function GetHorizontalResolution() : Single;
    function GetVerticalResolution() : Single;
    function GetFlags() : Cardinal;
    function GetRawFormat() : TGUID;
    function GetFormatName() : String;
    function GetPixelFormat() : TIGPPixelFormat;
    function GetPaletteSize() : Integer;
    function GetPalette(palette: PGPColorPalette; size: Integer) : TIGPImage;
    function SetPalette(palette: PGPColorPalette) : TIGPImage;
    function GetThumbnailImage(thumbWidth, thumbHeight: Cardinal; callback: TIGPGetThumbnailImageAbortProc = NIL) : TIGPImage;
    function GetFrameDimensionsCount() : Cardinal;
    function GetFrameDimensionsList() : TArray<TGUID>;
    function GetFrameCount(const dimensionID: TGUID) : Cardinal;
    function SelectActiveFrame(const dimensionID: TGUID; frameIndex: Cardinal) : TIGPImage;
    function RotateFlip(rotateFlipType: TIGPRotateFlipType) : TIGPImage;
    function GetPropertyCount() : Cardinal;
    function GetPropertyIdList() : TArray<TPropID>;
    function GetPropertyItemSize(propId: PROPID) : Cardinal;
    function GetPropertyItem(propId: PROPID; propSize: Cardinal; buffer: PGPPropertyItem) : TIGPImage;
    function GetPropertySize(out totalBufferSize, numProperties : Cardinal) : TIGPImage;
    function GetAllPropertyItems(totalBufferSize, numProperties: Cardinal; allItems: PGPPropertyItem ) : TIGPImage;
    function RemovePropertyItem(propId: TPROPID) : TIGPImage;
    function SetPropertyItem(const item: TIGPPropertyItem) : TIGPImage;
    function GetEncoderParameterListSize(const clsidEncoder: TGUID) : Cardinal;
    function GetEncoderParameterList(const clsidEncoder: TGUID; size: Cardinal; buffer: PGPEncoderParameters) : TIGPImage;

  end;
{$ENDIF}

  IGPBitmapData = interface
    ['{5036255F-F234-477D-8493-582198BF2CBB}']

    function GetWidth() : UINT;
    function GetHeight() : UINT;
    function GetStride() : Integer;
    function GetPixelFormat() : TIGPPixelFormat;
    function GetScan0() : Pointer;

    property Width        : UINT            read GetWidth;
    property Height       : UINT            read GetHeight;
    property Stride       : Integer         read GetStride;
    property PixelFormat  : TIGPPixelFormat read GetPixelFormat;
    property Scan0        : Pointer         read GetScan0;

  end;

  IGPBitmap = interface( IGPImage )
    ['{A242C124-6A5D-4F1F-9AC4-50A93D12E15B}']
{$IFDEF MSWINDOWS}
    function  Clone( const rect: TIGPRect; format: TIGPPixelFormat) : TIGPBitmap; overload;
    function  Clone(x, y, width, height: Integer; format: TIGPPixelFormat) : TIGPBitmap; overload;
    function  CloneF( const rect: TIGPRectF; format: TIGPPixelFormat) : TIGPBitmap; overload;
    function  CloneF(x, y, width, height: Single; format: TIGPPixelFormat) : TIGPBitmap; overload;
    function  LockBits( const rect: TIGPRect; flags: TIGPImageLockModes; format: TIGPPixelFormat ) : IGPBitmapData; overload;
    function  LockBits( flags: TIGPImageLockModes; format: TIGPPixelFormat ) : IGPBitmapData; overload;
    function  GetPixel(x, y: Integer) : TAlphaColor;
    function  SetPixel(x, y: Integer; color: TAlphaColor) : TIGPBitmap;
    procedure SetPixelProp(x, y: Integer; color: TAlphaColor);
    function  SetResolution(xdpi, ydpi: Single) : TIGPBitmap;
    function  GetHBITMAP( colorBackground: TAlphaColor ) : HBITMAP;
    function  GetHICON() : HICON;

    property Pixels[ X, Y : Integer ] : TAlphaColor read GetPixel write SetPixelProp; default;
{$ENDIF}
  end;
  
{$IFDEF MSWINDOWS}
  TIGPBitmap = class( TIGPImage, IGPBitmap )
  protected
    constructor CreateGdiPlus(nativeBitmap: GpBitmap; Dummy : Boolean );

  protected
    procedure LockBitsInternal( const rect: TIGPRect; flags: Cardinal; format: TIGPPixelFormat; var AData : TIGPBitmapDataRecord );
    function  UnlockBits(var lockedBitmapData: TIGPBitmapDataRecord) : TIGPBitmap;

  public
    constructor Create( filename : WideString; useEmbeddedColorManagement : Boolean = False ); overload;
    constructor Create( stream : IStream; useEmbeddedColorManagement : Boolean = False ); overload;

  public
    constructor Create( width, height, stride : Integer; format : TIGPPixelFormat; scan0 : PBYTE);  overload;
    constructor Create( width, height : Integer; format : TIGPPixelFormat = GPPixelFormat32bppARGB);  overload;
    constructor Create( width, height : Integer; target : TIGPGraphics); overload;

  public
//    constructor Create(surface: IDirectDrawSurface7); overload;
    constructor CreateData( var gdiBitmapInfo : TBITMAPINFO; gdiBitmapData : Pointer );
    constructor CreateHBITMAP( hbm : HBITMAP; hpal : HPALETTE );
    constructor CreateHICON( hicon : HICON );
    constructor CreateRes( hInstance : HMODULE; bitmapName : WideString );
    
  public
    function  Clone( const rect: TIGPRect; format: TIGPPixelFormat) : TIGPBitmap; overload;
    function  Clone(x, y, width, height: Integer; format: TIGPPixelFormat) : TIGPBitmap; overload;
    function  CloneF( const rect: TIGPRectF; format: TIGPPixelFormat) : TIGPBitmap; overload;
    function  CloneF(x, y, width, height: Single; format: TIGPPixelFormat) : TIGPBitmap; overload;
    function  LockBits( const rect: TIGPRect; flags: TIGPImageLockModes; format: TIGPPixelFormat ) : IGPBitmapData; overload;
    function  LockBits( flags: TIGPImageLockModes; format: TIGPPixelFormat ) : IGPBitmapData; overload;
    function  GetPixel(x, y: Integer) : TAlphaColor;
    function  SetPixel(x, y: Integer; color: TAlphaColor) : TIGPBitmap;
    procedure SetPixelProp(x, y: Integer; color: TAlphaColor);
    function  SetResolution(xdpi, ydpi: Single) : TIGPBitmap;
    function  GetHBITMAP( colorBackground: TAlphaColor ) : HBITMAP;
    function  GetHICON() : HICON;

  public
//    class function FromDirectDrawSurface7(surface: IDirectDrawSurface7) : TIGPBitmap;
    class function FromBITMAPINFO(var gdiBitmapInfo: TBITMAPINFO; gdiBitmapData: Pointer) : TIGPBitmap;
    class function FromFile(filename: WideString; useEmbeddedColorManagement: Boolean = False) : TIGPBitmap;
    class function FromStream(stream: IStream; useEmbeddedColorManagement: Boolean = False) : TIGPBitmap;
    class function FromHBITMAP(hbm: HBITMAP; hpal: HPALETTE) : TIGPBitmap;
    class function FromHICON(hicon: HICON) : TIGPBitmap;
    class function FromResource(hInstance: HMODULE; bitmapName: WideString) : TIGPBitmap;
    
  end;
{$ELSE}
  TIGPBitmap = class( TInterfacedObject, IGPBitmap )
  public
    constructor Create( width, height : Integer; format : TIGPPixelFormat = GPPixelFormat32bppARGB);  overload;

  end;
{$ENDIF}

  IGPCustomLineCap = interface
    ['{C11912FC-5FF7-44D1-A201-ABFDA33184E9}']
    function  GetNativeCap() : GpCustomLineCap;
    function  Clone() : TIGPCustomLineCap;

    function  SetStrokeCap(strokeCap: TIGPLineCap) : TIGPCustomLineCap;
    
    function  SetStrokeCaps(startCap, endCap: TIGPLineCap) : TIGPCustomLineCap;
    function  GetStrokeCaps(out startCap, endCap: TIGPLineCap) : TIGPCustomLineCap;

    function  SetStrokeJoin(lineJoin: TIGPLineJoin) : TIGPCustomLineCap;
    procedure SetStrokeJoinProp(lineJoin: TIGPLineJoin);
    function  GetStrokeJoin() : TIGPLineJoin;
    
    function  SetBaseCap(baseCap: TIGPLineCap) : TIGPCustomLineCap;
    procedure SetBaseCapProp(baseCap: TIGPLineCap);
    function  GetBaseCap() : TIGPLineCap;

    function  SetBaseInset(inset: Single) : TIGPCustomLineCap;
    procedure SetBaseInsetProp(inset: Single);
    function  GetBaseInset() : Single;
    
    function  SetWidthScale(widthScale: Single) : TIGPCustomLineCap;
    procedure SetWidthScaleProp(widthScale: Single);
    function  GetWidthScale() : Single;

    property StrokeJoin : TIGPLineJoin read GetStrokeJoin  write SetStrokeJoinProp;
    property BaseCap    : TIGPLineCap  read GetBaseCap     write SetBaseCapProp;
    property BaseInset  : Single      read GetBaseInset   write SetBaseInsetProp;
    property WidthScale : Single      read GetWidthScale  write SetWidthScaleProp;
    
  end;

{$IFDEF MSWINDOWS}
  TIGPCustomLineCap = class( TIGPBase, IGPCustomLineCap )
  protected
    FNativeCap : GpCustomLineCap;

  protected
    function  GetNativeCap() : GpCustomLineCap; 
    procedure SetNativeCap(nativeCap: GpCustomLineCap);
    constructor CreateGdiPlus(nativeCap: GpCustomLineCap; Dummy : Boolean);

  public
    constructor Create(); overload;
    constructor Create(fillPath, strokePath: IGPGraphicsPath;
      baseCap: TIGPLineCap = LineCapFlat;
      baseInset: Single = 0); overload;
    destructor  Destroy(); override;

  public
    function  Clone() : TIGPCustomLineCap;
    function  SetStrokeCap(strokeCap: TIGPLineCap) : TIGPCustomLineCap;
    
    function  SetStrokeCaps(startCap, endCap: TIGPLineCap) : TIGPCustomLineCap;
    function  GetStrokeCaps(out startCap, endCap: TIGPLineCap) : TIGPCustomLineCap;

    function  SetStrokeJoin(lineJoin: TIGPLineJoin) : TIGPCustomLineCap;
    procedure SetStrokeJoinProp(lineJoin: TIGPLineJoin);
    function  GetStrokeJoin() : TIGPLineJoin;
    
    function  SetBaseCap(baseCap: TIGPLineCap) : TIGPCustomLineCap;
    procedure SetBaseCapProp(baseCap: TIGPLineCap);
    function  GetBaseCap() : TIGPLineCap;

    function  SetBaseInset(inset: Single) : TIGPCustomLineCap;
    procedure SetBaseInsetProp(inset: Single);
    function  GetBaseInset() : Single;
    
    function  SetWidthScale(widthScale: Single) : TIGPCustomLineCap;
    procedure SetWidthScaleProp(widthScale: Single);
    function  GetWidthScale() : Single;

  end;
{$ENDIF}

  IGPCachedBitmap = interface
    ['{96A926BE-354E-4A88-B4B3-0DB3A648D181}']
    
    function GetNativeCachedBitmap() : GpCachedBitmap;
    
  end;

{$IFDEF MSWINDOWS}
  TIGPCachedBitmap = class(TIGPBase, IGPCachedBitmap)
  protected
    FNativeCachedBitmap: GpCachedBitmap;

  protected
    function GetNativeCachedBitmap() : GpCachedBitmap;
    
  public
    constructor Create(bitmap: IGPBitmap; graphics: IGPGraphics); reintroduce;
    destructor  Destroy(); override;
    
  end;
{$ENDIF}

(**************************************************************************\
*
*   GDI+ Image Attributes used with Graphics.DrawImage
*
* There are 5 possible sets of color adjustments:
*          ColorAdjustDefault,
*          ColorAdjustBitmap,
*          ColorAdjustBrush,
*          ColorAdjustPen,
*          ColorAdjustText,
*
* Bitmaps, Brushes, Pens, and Text will all use any color adjustments
* that have been set into the default ImageAttributes until their own
* color adjustments have been set.  So as soon as any "Set" method is
* called for Bitmaps, Brushes, Pens, or Text, then they start from
* scratch with only the color adjustments that have been set for them.
* Calling Reset removes any individual color adjustments for a type
* and makes it revert back to using all the default color adjustments
* (if any).  The SetToIdentity method is a way to force a type to
* have no color adjustments at all, regardless of what previous adjustments
* have been set for the defaults or for that type.
*
\**************************************************************************)

  IGPImageAttributes = interface
    ['{330BD1E0-00B5-4399-BAB7-990DE03CC7F4}']
    
    function GetNativeImageAttr() : GpImageAttributes;
    
    function Clone() : TIGPImageAttributes;
    function SetToIdentity(type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function Reset(type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function SetColorMatrix(const colorMatrix: TIGPColorMatrix;
      mode: TIGPColorMatrixFlags = ColorMatrixFlagsDefault;
      type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function ClearColorMatrix(type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function SetColorMatrices(const colorMatrix: TIGPColorMatrix; const grayMatrix: TIGPColorMatrix;
      mode: TIGPColorMatrixFlags  = ColorMatrixFlagsDefault;
      type_: TIGPColorAdjustType  = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function ClearColorMatrices(Type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function SetThreshold(threshold: Single; type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function ClearThreshold(type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function SetGamma(gamma: Single; type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function ClearGamma( type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function SetNoOp(type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function ClearNoOp(Type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function SetColorKey(colorLow, colorHigh: TAlphaColor; type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function ClearColorKey(type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function SetOutputChannel(channelFlags: TIGPColorChannelFlags; type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function ClearOutputChannel(type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function SetOutputChannelColorProfile(colorProfileFilename: WideString;
      type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function ClearOutputChannelColorProfile(type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function SetRemapTable(mapSize: Cardinal; map: PGPColorMap; type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function ClearRemapTable(type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function SetBrushRemapTable(mapSize: Cardinal; map: PGPColorMap) : TIGPImageAttributes;
    function ClearBrushRemapTable() : TIGPImageAttributes;
    function SetWrapMode(wrap: TIGPWrapMode; color: TAlphaColor = aclBlack; clamp: Boolean = False) : TIGPImageAttributes;
    // The flags of the palette are ignored.
    function GetAdjustedPalette(colorPalette: PGPColorPalette; colorAdjustType: TIGPColorAdjustType) : TIGPImageAttributes;
    
  end;
  
{$IFDEF MSWINDOWS}
  TIGPImageAttributes = class(TIGPBase, IGPImageAttributes)
  protected
    FNativeImageAttr: GpImageAttributes;
    
  protected
    function GetNativeImageAttr() : GpImageAttributes;

  protected
    procedure SetNativeImageAttr(nativeImageAttr: GpImageAttributes);
    constructor CreateGdiPlus(imageAttr: GpImageAttributes; Dummy : Boolean );
    
  public
    constructor Create();
    destructor  Destroy(); override;
    
  public
    function Clone() : TIGPImageAttributes;
    function SetToIdentity(type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function Reset(type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function SetColorMatrix(const colorMatrix: TIGPColorMatrix;
      mode: TIGPColorMatrixFlags = ColorMatrixFlagsDefault;
      type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function ClearColorMatrix(type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function SetColorMatrices(const colorMatrix: TIGPColorMatrix; const grayMatrix: TIGPColorMatrix;
      mode: TIGPColorMatrixFlags  = ColorMatrixFlagsDefault;
      type_: TIGPColorAdjustType  = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function ClearColorMatrices(Type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function SetThreshold(threshold: Single; type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function ClearThreshold(type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function SetGamma(gamma: Single; type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function ClearGamma( type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function SetNoOp(type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function ClearNoOp(Type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function SetColorKey(colorLow, colorHigh: TAlphaColor; type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function ClearColorKey(type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function SetOutputChannel(channelFlags: TIGPColorChannelFlags; type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function ClearOutputChannel(type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function SetOutputChannelColorProfile(colorProfileFilename: WideString;
      type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function ClearOutputChannelColorProfile(type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function SetRemapTable(mapSize: Cardinal; map: PGPColorMap; type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function ClearRemapTable(type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
    function SetBrushRemapTable(mapSize: Cardinal; map: PGPColorMap) : TIGPImageAttributes;
    function ClearBrushRemapTable() : TIGPImageAttributes;
    function SetWrapMode(wrap: TIGPWrapMode; color: TAlphaColor = aclBlack; clamp: Boolean = False) : TIGPImageAttributes;
    // The flags of the palette are ignored.
    function GetAdjustedPalette(colorPalette: PGPColorPalette; colorAdjustType: TIGPColorAdjustType) : TIGPImageAttributes;
  end;
{$ENDIF}

(**************************************************************************\
*
*   GDI+ Matrix class
*
\**************************************************************************)

//  TMatrixArray = array[0..5] of Single;
  TIGPMatrixParams = packed record
    m11 : Single;
    m12 : Single;
    m21 : Single;
    m22 : Single;
    dx  : Single;
    dy  : Single;

  end;

  IGPMatrix = interface
    ['{EBD3DFC3-7740-496E-B074-2AD588B11137}']
    
    function GetNativeMatrix() : GpMatrix;
    function Clone() : TIGPMatrix;
    function GetElements() : TIGPMatrixParams;
    function SetElements(m11, m12, m21, m22, dx, dy: Single) : TIGPMatrix; overload;
    function SetElements( AElements : TIGPMatrixParams ) : TIGPMatrix; overload;
    procedure SetElementsProp( AElements : TIGPMatrixParams );
    function OffsetX() : Single;
    function OffsetY() : Single;
    function Reset() : TIGPMatrix;
    function Multiply(matrix: IGPMatrix; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPMatrix;                // ok
    function Translate(offsetX, offsetY: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPMatrix;      // ok
    function Scale(scaleX, scaleY: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPMatrix;            // ok
    function Rotate(angle: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPMatrix;                    // ok
    function RotateAt(angle: Single; const center: TPointF; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPMatrix; // ok
    function Shear(shearX, shearY: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPMatrix;            // ok
    function Invert() : TIGPMatrix;                                                                             // ok

    function TransformPointF( var point : TPointF ) : TIGPMatrix;
    function TransformPoint( var point : TPoint ) : TIGPMatrix;

    function TransformPointsF( var pts : array of TPointF ) : TIGPMatrix;
    function TransformPoints( var pts : array of TPoint ) : TIGPMatrix;

    function TransformVectorsF( var pts : array of TPointF ) : TIGPMatrix;
    function TransformVectors( var pts : array of TPoint ) : TIGPMatrix;

    function IsInvertible() : Boolean;
    function IsIdentity() : Boolean;
    function Equals(matrix: IGPMatrix) : Boolean;
    
    property Elements : TIGPMatrixParams read GetElements write SetElementsProp;

  end;
  
{$IFDEF MSWINDOWS}
  TIGPMatrix = class( TIGPBase, IGPMatrix )
  protected
    FNativeMatrix : GpMatrix;

  protected
    procedure SetNativeMatrix(nativeMatrix: GpMatrix);
    function  GetNativeMatrix() : GpMatrix;
    constructor CreateGdiPlus(nativeMatrix: GpMatrix; Dummy : Boolean);
    
  public
    // Default constructor is set to identity matrix.
    constructor Create(); overload;
    constructor Create(m11, m12, m21, m22, dx, dy: Single); overload;
    constructor Create(const rect: TIGPRectF; const dstplg: TPointF); overload;
    constructor Create(const rect: TIGPRect; const dstplg: TPoint); overload;
    destructor  Destroy(); override;
    
  public
    function  Clone() : TIGPMatrix;
    function  GetElements() : TIGPMatrixParams;
    function  SetElements(m11, m12, m21, m22, dx, dy: Single) : TIGPMatrix; overload;
    function  SetElements( AElements : TIGPMatrixParams ) : TIGPMatrix; overload;
    procedure SetElementsProp( AElements : TIGPMatrixParams );
    function  OffsetX() : Single;
    function  OffsetY() : Single;
    function  Reset() : TIGPMatrix;
    function  Multiply(matrix: IGPMatrix; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPMatrix;
    function  Translate(offsetX, offsetY: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPMatrix;
    function  Scale(scaleX, scaleY: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPMatrix;
    function  Rotate(angle: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPMatrix;
    function  RotateAt(angle: Single; const center: TPointF; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPMatrix;
    function  Shear(shearX, shearY: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPMatrix;
    function  Invert() : TIGPMatrix;

    function  TransformPointF( var point : TPointF ) : TIGPMatrix;
    function  TransformPoint( var point : TPoint ) : TIGPMatrix;

    function  TransformPointsF( var pts : array of TPointF ) : TIGPMatrix;
    function  TransformPoints( var pts : array of TPoint ) : TIGPMatrix;

    function  TransformVectorsF( var pts : array of TPointF ) : TIGPMatrix;
    function  TransformVectors( var pts : array of TPoint ) : TIGPMatrix;

    function  IsInvertible() : Boolean;
    function  IsIdentity() : Boolean;
    function  EqualsMatrix(matrix: IGPMatrix) : Boolean;

    function  IGPMatrix.Equals = EqualsMatrix;

  end;
{$ENDIF}

  IGPMatrixStore = interface
    ['{C43901CD-CF57-485E-9050-F28AB12A63CE}']
  end;

{$IFDEF MSWINDOWS}
  TIGPMatrixStore = class( TInterfacedObject, IGPMatrixStore )
  protected
    FTransformable : IGPTransformable;
    FMatrix        : IGPMatrix;

  public
    constructor Create( ATransformable : IGPTransformable );
    destructor  Destroy(); override;

  end;
{$ENDIF}

(**************************************************************************\
*
*   GDI+ Brush class
*
\**************************************************************************)

  IGPBrush = interface
    ['{C5A51119-107A-4EE4-8989-83659A5149A1}']
    function Clone() : TIGPBrush;
    function GetType() : TIGPBrushType;
    function GetNativeBrush() : GpBrush;

    property BrushType : TIGPBrushType read GetType;
    
  end;
  
  IGPWrapBrush = interface( IGPBrush )
    ['{774EE93A-BFAD-41B2-B68A-D40E975711EA}']
    
    function  GetWrapMode() : TIGPWrapMode;
    procedure SetWrapModeProp(wrapMode: TIGPWrapMode);

    procedure SetTransformProp(matrix: IGPMatrix);
    function  GetTransform() : IGPMatrix;

    property WrapMode   : TIGPWrapMode read GetWrapMode  write SetWrapModeProp;
    property Transform  : IGPMatrix   read GetTransform write SetTransformProp;

  end;

  IGPBlendBrush = interface( IGPWrapBrush )
    ['{3DBE75FD-74EF-48CF-8579-69B9EF730DB1}']
    function  GetBlendCount() : Integer;
    function  GetBlend() : TArray<TIGPBlend>;
    procedure SetBlendProp( const blendFactors : TArray<TIGPBlend> );

    function  GetInterpolationColorCount() : Integer;
    procedure SetInterpolationColorsProp( Colors : TArray<TIGPInterpolationColor> );
    function  GetInterpolationColors() : TArray<TIGPInterpolationColor>;

    procedure SetGammaCorrectionProp(useGammaCorrection: Boolean);
    function  GetGammaCorrection() : Boolean;

    property Blend      : TArray<TIGPBlend> read GetBlend write SetBlendProp;
    property BlendCount : Integer       read GetBlendCount;

    property InterpolationColors      : TArray<TIGPInterpolationColor>  read GetInterpolationColors write SetInterpolationColorsProp;
    property InterpolationColorCount  : Integer read GetInterpolationColorCount;

    property GammaCorrection  : Boolean read GetGammaCorrection write SetGammaCorrectionProp;

  end;

  //--------------------------------------------------------------------------
  // Abstract base class for various brush types
  //--------------------------------------------------------------------------

{$IFDEF MSWINDOWS}
  TIGPBrush = class( TIGPBase, IGPBrush )
  protected
    FNativeBrush : GpBrush;
    
  protected
    procedure SetNativeBrush( nativeBrush: GpBrush );
    function  GetNativeBrush() : GpBrush;
    constructor Create(nativeBrush: GpBrush); overload;
    
  public
    constructor Create(); overload;
    destructor  Destroy(); override;

  public
    function Clone() : TIGPBrush; virtual;
    function GetType() : TIGPBrushType;
    
  end;
{$ENDIF}

  //--------------------------------------------------------------------------
  // Solid Fill Brush Object
  //--------------------------------------------------------------------------

  IGPSolidBrush = interface( IGPBrush )
    ['{388E717D-5FFA-4262-9B07-0A72FF8CFEC8}']

    function  GetColor() : TAlphaColor;
    function  SetColor(color: TAlphaColor) : TIGPSolidBrush;
    procedure SetColorProp(color: TAlphaColor);

    property Color : TAlphaColor read GetColor write SetColorProp;

  end;

{$IFDEF MSWINDOWS}
  TIGPSolidBrush = class(TIGPBrush, IGPSolidBrush)
  protected
    function  GetColor() : TAlphaColor;
    function  SetColor(color: TAlphaColor) : TIGPSolidBrush;
    procedure SetColorProp(color: TAlphaColor);

  public
    constructor Create(color: TAlphaColor); overload;
    constructor Create(); overload;

  end;
{$ENDIF}

  IGPTransformable = interface
    ['{9EEFBE7F-9DA0-47D4-B426-75A0047CF6BE}']

    function  GetTransform() : IGPMatrix;
    function  SetTransform(matrix: IGPMatrix) : IGPTransformable;
    procedure SetTransformProp(matrix: IGPMatrix);

    function  ResetTransform() : IGPTransformable;
    function  MultiplyTransform(matrix: IGPMatrix; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function  TranslateTransform(dx, dy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function  ScaleTransform(sx, sy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function  ScaleTransformXY(s : Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function  RotateTransform(angle: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;

    property Transform  : IGPMatrix read GetTransform write SetTransformProp;
    
  end;
  
  //--------------------------------------------------------------------------
  // Texture Brush Fill Object
  //--------------------------------------------------------------------------
  IGPTextureBrush = interface( IGPWrapBrush )
    ['{F0DE6DAC-4D8D-408D-8D1A-CCCF5A70FF7A}']

    function  SetTransform(matrix: IGPMatrix) : TIGPTextureBrush;
    function  ResetTransform() : TIGPTextureBrush;
    function  MultiplyTransform(matrix: IGPMatrix; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPTextureBrush;
    function  TranslateTransform(dx, dy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPTextureBrush;
    function  ScaleTransform(sx, sy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPTextureBrush; overload;
    function  ScaleTransform(s: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPTextureBrush; overload;
    function  RotateTransform(angle: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPTextureBrush;

    function  SetWrapMode(wrapMode: TIGPWrapMode) : TIGPTextureBrush;
    
    function  GetImage() : IGPImage;
    function  SetImage( image : IGPImage ) : TIGPTextureBrush;
    procedure SetImageProp( image : IGPImage );

    property Image  : IGPImage  read GetImage write SetImageProp;

  end;

{$IFDEF MSWINDOWS}
  TIGPTextureBrush = class(TIGPBrush, IGPTextureBrush, IGPTransformable)
  public
    constructor Create(image: IGPImage; wrapMode: TIGPWrapMode = WrapModeTile); overload;
    constructor Create(image: IGPImage; wrapMode: TIGPWrapMode; const dstRect: TIGPRectF); overload;
    constructor Create(image: IGPImage; const dstRect: TIGPRectF; imageAttributes: IGPImageAttributes = NIL); overload;
    constructor Create(image: IGPImage; const dstRect: TIGPRect; imageAttributes: IGPImageAttributes = NIL); overload;
    constructor Create(image: IGPImage; wrapMode: TIGPWrapMode; const dstRect: TIGPRect); overload;
    constructor Create(image: IGPImage; wrapMode: TIGPWrapMode; dstX, dstY, dstWidth, dstHeight: Single); overload;
    constructor Create(image: IGPImage; wrapMode: TIGPWrapMode; dstX, dstY, dstWidth, dstHeight: Integer); overload;
    constructor Create(); overload;

  protected
    function  SetTransformT(matrix: IGPMatrix) : IGPTransformable;
    function  ResetTransformT() : IGPTransformable;
    function  MultiplyTransformT(matrix: IGPMatrix; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function  TranslateTransformT(dx, dy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function  ScaleTransformT(sx, sy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function  ScaleTransformXYT(s: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function  RotateTransformT(angle: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;

    function  IGPTransformable.SetTransform = SetTransformT;
    function  IGPTransformable.ResetTransform = ResetTransformT;
    function  IGPTransformable.MultiplyTransform = MultiplyTransformT;
    function  IGPTransformable.TranslateTransform = TranslateTransformT;
    function  IGPTransformable.ScaleTransform = ScaleTransformT;
    function  IGPTransformable.ScaleTransformXY = ScaleTransformXYT;
    function  IGPTransformable.RotateTransform = RotateTransformT;

  public
    function  SetTransform(matrix: IGPMatrix) : TIGPTextureBrush;
    procedure SetTransformProp(matrix: IGPMatrix);
    function  GetTransform() : IGPMatrix;
    function  ResetTransform() : TIGPTextureBrush;
    function  MultiplyTransform(matrix: IGPMatrix; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPTextureBrush;
    function  TranslateTransform(dx, dy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPTextureBrush;
    function  ScaleTransform(sx, sy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPTextureBrush; overload;
    function  ScaleTransform(s: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPTextureBrush; overload;
    function  RotateTransform(angle: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPTextureBrush;
    function  GetWrapMode() : TIGPWrapMode;
    function  SetWrapMode(wrapMode: TIGPWrapMode) : TIGPTextureBrush;
    procedure SetWrapModeProp(wrapMode: TIGPWrapMode);
    function  GetImage() : IGPImage;
    function  SetImage( image : IGPImage ) : TIGPTextureBrush;
    procedure SetImageProp( image : IGPImage );

  end;
{$ENDIF}

  //--------------------------------------------------------------------------
  // Linear Gradient Brush Object
  //--------------------------------------------------------------------------

  IGPLinearGradientBrush = interface( IGPBlendBrush )
    ['{FD7C48BB-0DD6-4F12-8786-940A0308A4C7}']
    
    function  SetLinearColors(color1, color2: TAlphaColor) : TIGPLinearGradientBrush;
    function  GetLinearColors(out color1, color2: TAlphaColor) : TIGPLinearGradientBrush;
    function  GetRectangleF() : TIGPRectF;
    function  GetRectangle() : TIGPRect;
    function  SetGammaCorrection(useGammaCorrection: Boolean) : TIGPLinearGradientBrush;
    
    function  SetBlendArrays( const blendFactors : array of Single; const blendPositions : array of Single ) : TIGPLinearGradientBrush;
    function  SetBlend( const blendFactors : array of TIGPBlend ) : TIGPLinearGradientBrush;
    function  SetInterpolationColors( const Colors : array of TIGPInterpolationColor ) : TIGPLinearGradientBrush;
    function  SetInterpolationColorArrays( const presetColors: array of TAlphaColor; const blendPositions: array of Single ) : TIGPLinearGradientBrush;
    function  SetBlendBellShape(focus: Single; scale: Single = 1.0) : TIGPLinearGradientBrush;
    function  SetBlendTriangularShape(focus: Single; scale: Single = 1.0) : TIGPLinearGradientBrush;
              
    function  SetTransform(matrix: IGPMatrix) : TIGPLinearGradientBrush; overload;
    function  ResetTransform() : TIGPLinearGradientBrush; overload;
    function  MultiplyTransform(matrix: IGPMatrix; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPLinearGradientBrush;
    function  TranslateTransform(dx, dy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPLinearGradientBrush; overload;
    function  ScaleTransform(sx, sy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPLinearGradientBrush; overload;
    function  ScaleTransform(s : Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPLinearGradientBrush; overload;
    function  RotateTransform(angle: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPLinearGradientBrush; overload;
    
    function  SetWrapMode(wrapMode: TIGPWrapMode) : TIGPLinearGradientBrush;

  end;

{$IFDEF MSWINDOWS}
  TIGPLinearGradientBrush = class(TIGPBrush, IGPLinearGradientBrush, IGPTransformable)
  public
    constructor Create(); overload;
    constructor Create( const point1, point2: TPointF; color1,
      color2: TAlphaColor); overload;
    constructor Create( const point1, point2: TPoint; color1,
      color2: TAlphaColor); overload;
    constructor Create( const rect: TIGPRectF; color1, color2: TAlphaColor;
      mode: TIGPLinearGradientMode); overload;
    constructor Create( const rect: TIGPRect; color1, color2: TAlphaColor;
      mode: TIGPLinearGradientMode); overload;
    constructor Create( const rect: TIGPRectF; color1, color2: TAlphaColor; angle: Single;
      isAngleScalable: Boolean = False); overload;
    constructor Create( const rect: TIGPRect; color1, color2: TAlphaColor; angle: Single;
      isAngleScalable: Boolean = False); overload;

  public
    function  SetLinearColors(color1, color2: TAlphaColor) : TIGPLinearGradientBrush;
    function  GetLinearColors(out color1, color2: TAlphaColor) : TIGPLinearGradientBrush;
    function  GetRectangleF() : TIGPRectF;
    function  GetRectangle() : TIGPRect;

    procedure SetGammaCorrectionProp(useGammaCorrection: Boolean);
    function  SetGammaCorrection(useGammaCorrection: Boolean) : TIGPLinearGradientBrush;
    function  GetGammaCorrection() : Boolean;

    function  GetBlendCount() : Integer;
    function  GetBlend() : TArray<TIGPBlend>;
    function  SetBlendArrays( const blendFactors : array of Single; const blendPositions : array of Single ) : TIGPLinearGradientBrush;
    function  SetBlend( const blendFactors : array of TIGPBlend ) : TIGPLinearGradientBrush;
    procedure SetBlendProp( const blendFactors : TArray<TIGPBlend> );
    function  GetInterpolationColorCount() : Integer;
    procedure SetInterpolationColorsProp( Colors : TArray<TIGPInterpolationColor> );
    function  SetInterpolationColors( const Colors : array of TIGPInterpolationColor ) : TIGPLinearGradientBrush;
    function  SetInterpolationColorArrays( const presetColors: array of TAlphaColor; const blendPositions: array of Single ) : TIGPLinearGradientBrush;
    function  GetInterpolationColors() : TArray<TIGPInterpolationColor>;
    function  SetBlendBellShape(focus: Single; scale: Single = 1.0) : TIGPLinearGradientBrush;
    function  SetBlendTriangularShape(focus: Single; scale: Single = 1.0) : TIGPLinearGradientBrush;
              
    function  SetTransform(matrix: IGPMatrix) : TIGPLinearGradientBrush;
    procedure SetTransformProp(matrix: IGPMatrix);
    function  GetTransform() : IGPMatrix;
    function  ResetTransform() : TIGPLinearGradientBrush;
    function  MultiplyTransform(matrix: IGPMatrix; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPLinearGradientBrush;
    function  TranslateTransform(dx, dy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPLinearGradientBrush;
    function  ScaleTransform(sx, sy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPLinearGradientBrush; overload;
    function  ScaleTransform(s: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPLinearGradientBrush; overload;
    function  RotateTransform(angle: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPLinearGradientBrush;

    procedure SetWrapModeProp(wrapMode: TIGPWrapMode);
    function  SetWrapMode(wrapMode: TIGPWrapMode) : TIGPLinearGradientBrush;
    function  GetWrapMode() : TIGPWrapMode;
    
  protected
    function SetTransformT(matrix: IGPMatrix) : IGPTransformable;
    function ResetTransformT() : IGPTransformable;
    function MultiplyTransformT(matrix: IGPMatrix; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function TranslateTransformT(dx, dy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function ScaleTransformT(sx, sy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function ScaleTransformXYT(s: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function RotateTransformT(angle: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;

    function IGPTransformable.SetTransform = SetTransformT;
    function IGPTransformable.ResetTransform = ResetTransformT;
    function IGPTransformable.MultiplyTransform = MultiplyTransformT;
    function IGPTransformable.TranslateTransform = TranslateTransformT;
    function IGPTransformable.ScaleTransform = ScaleTransformT;
    function  IGPTransformable.ScaleTransformXY = ScaleTransformXYT;
    function IGPTransformable.RotateTransform = RotateTransformT;

  end;
{$ENDIF}

  //--------------------------------------------------------------------------
  // Hatch Brush Object
  //--------------------------------------------------------------------------

  IGPHatchBrush = interface( IGPBrush )
    ['{302E268C-E3B3-421F-8EDD-341FEA9E21D9}']
    procedure SetHatchStyleProp( style : TIGPHatchStyle );
    function  SetHatchStyle( style : TIGPHatchStyle ) : TIGPHatchBrush;
    function  GetHatchStyle() : TIGPHatchStyle;

    procedure SetForegroundColorProp( color : TAlphaColor );
    function  SetForegroundColor( color : TAlphaColor ) : TIGPHatchBrush;
    function  GetForegroundColor() : TAlphaColor;

    procedure SetBackgroundColorProp( color : TAlphaColor );
    function  SetBackgroundColor( color : TAlphaColor ) : TIGPHatchBrush;
    function  GetBackgroundColor() : TAlphaColor;
    
    property  HatchStyle : TIGPHatchStyle read GetHatchStyle write SetHatchStyleProp;
    property  ForegroundColor : TAlphaColor read GetForegroundColor write SetForegroundColorProp;
    property  BackgroundColor : TAlphaColor read GetBackgroundColor write SetBackgroundColorProp;
    
  end;
  
{$IFDEF MSWINDOWS}
  TIGPHatchBrush = class(TIGPBrush, IGPHatchBrush)
  public
    constructor Create(); overload; // ok
    constructor Create(hatchStyle: TIGPHatchStyle; foreColor: TAlphaColor; backColor: TAlphaColor = aclBlack); overload; // ok
    
  public
    procedure SetHatchStyleProp( style : TIGPHatchStyle );
    function  SetHatchStyle( style : TIGPHatchStyle ) : TIGPHatchBrush;
    function  GetHatchStyle() : TIGPHatchStyle;

    procedure SetForegroundColorProp( color : TAlphaColor );
    function  SetForegroundColor( color : TAlphaColor ) : TIGPHatchBrush;
    function  GetForegroundColor() : TAlphaColor;

    procedure SetBackgroundColorProp( color : TAlphaColor );
    function  SetBackgroundColor( color : TAlphaColor ) : TIGPHatchBrush;
    function  GetBackgroundColor() : TAlphaColor;

  end;
{$ENDIF}

(**************************************************************************\
*
*   GDI+ Pen class
*
\**************************************************************************)

//--------------------------------------------------------------------------
// Pen class 
//--------------------------------------------------------------------------

  IGPPen = interface
    ['{3078FAF8-1E13-4FF0-A9B0-6350298958B6}']
    function  GetNativePen() : GpPen;
    function  Clone() : TIGPPen;
    
    procedure SetWidthProp(width: Single);
    function  SetWidth(width: Single) : TIGPPen;
    function  GetWidth() : Single;
    // Set/get line caps: start, end, and dash
    // Line cap and join APIs by using LineCap and LineJoin enums.
    function  SetLineCap(startCap, endCap: TIGPLineCap; dashCap: TIGPDashCap) : TIGPPen;
    
    procedure SetStartCapProp(startCap: TIGPLineCap);
    function  SetStartCap(startCap: TIGPLineCap) : TIGPPen;
    function  GetStartCap() : TIGPLineCap;
    
    procedure SetEndCapProp(endCap: TIGPLineCap);
    function  SetEndCap(endCap: TIGPLineCap) : TIGPPen;
    function  GetEndCap() : TIGPLineCap;
    
    procedure SetDashCapProp(dashCap: TIGPDashCap);
    function  SetDashCap(dashCap: TIGPDashCap) : TIGPPen;
    function  GetDashCap() : TIGPDashCap;

    procedure SetLineJoinProp(lineJoin: TIGPLineJoin);
    function  SetLineJoin(lineJoin: TIGPLineJoin) : TIGPPen;
    function  GetLineJoin() : TIGPLineJoin;

    procedure SetCustomStartCapProp(customCap: IGPCustomLineCap);    
    function  SetCustomStartCap(customCap: IGPCustomLineCap) : TIGPPen;
    function  GetCustomStartCap() : IGPCustomLineCap;
    
    procedure SetCustomEndCapProp(customCap: IGPCustomLineCap);    
    function  SetCustomEndCap(customCap: IGPCustomLineCap) : TIGPPen;
    function  GetCustomEndCap() : IGPCustomLineCap;

    procedure SetMiterLimitProp(miterLimit: Single);
    function  SetMiterLimit(miterLimit: Single) : TIGPPen;
    function  GetMiterLimit() : Single;
    
    procedure SetAlignmentProp( penAlignment: TIGPPenAlignment);
    function  SetAlignment( penAlignment: TIGPPenAlignment) : TIGPPen;
    function  GetAlignment() : TIGPPenAlignment;
              
    function  SetTransform(matrix: IGPMatrix) : TIGPPen;
    procedure SetTransformProp(matrix: IGPMatrix);
    function  GetTransform() : IGPMatrix;
    
    function  ResetTransform() : TIGPPen;
    function  MultiplyTransform(matrix: IGPMatrix; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPPen;
    function  TranslateTransform(dx, dy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPPen;
    function  ScaleTransform(sx, sy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPPen; overload;
    function  ScaleTransform(s: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPPen; overload;
    function  RotateTransform(angle: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPPen;
              
    function  GetPenType() : TIGPPenType;

    procedure SetColorProp(color: TAlphaColor);
    function  SetColor(color: TAlphaColor) : TIGPPen;
    function  GetColor() : TAlphaColor;

    procedure SetBrushProp( brush: IGPBrush);
    function  SetBrush( brush: IGPBrush) : TIGPPen;
    function  GetBrush() : IGPBrush;

    procedure SetDashStyleProp(dashStyle: TIGPDashStyle);
    function  SetDashStyle(dashStyle: TIGPDashStyle) : TIGPPen;
    function  GetDashStyle() : TIGPDashStyle;

    procedure SetDashOffsetProp( dashOffset: Single );
    function  SetDashOffset( dashOffset: Single ) : TIGPPen;
    function  GetDashOffset() : Single;
    
    function  GetDashPatternCount() : Integer;
    function  SetDashPattern( const dashArray: array of Single ) : TIGPPen;
    procedure SetDashPatternProp( dashArray: TArray<Single> );
    function  GetDashPattern() : TArray<Single>;
    
    function  GetCompoundArrayCount() : Integer;
    function  SetCompoundArray( const compoundArray: array of Single ) : TIGPPen;
    procedure SetCompoundArrayProp( compoundArray: TArray<Single> );
    function  GetCompoundArray() : TArray<Single>;
              
    property PenType        : TIGPPenType       read GetPenType;

    property Width          : Single            read GetWidth           write SetWidthProp;
    property Color          : TAlphaColor       read GetColor           write SetColorProp;
    property Brush          : IGPBrush          read GetBrush           write SetBrushProp;
    property Alignment      : TIGPPenAlignment  read GetAlignment       write SetAlignmentProp;
    property MiterLimit     : Single            read GetMiterLimit      write SetMiterLimitProp;
    property DashOffset     : Single            read GetDashOffset      write SetDashOffsetProp;

    property StartCap       : TIGPLineCap       read GetStartCap        write SetStartCapProp;
    property EndCap         : TIGPLineCap       read GetEndCap          write SetEndCapProp;
    property CustomStartCap : IGPCustomLineCap  read GetCustomStartCap  write SetCustomStartCapProp;
    property CustomEndCap   : IGPCustomLineCap  read GetCustomEndCap    write SetCustomEndCapProp;

    property DashStyle      : TIGPDashStyle     read GetDashStyle       write SetDashStyleProp;
    property DashCap        : TIGPDashCap       read GetDashCap         write SetDashCapProp;
    property DashPattern    : TArray<Single>    read GetDashPattern     write SetDashPatternProp;

    property LineJoin       : TIGPLineJoin      read GetLineJoin        write SetLineJoinProp;

    property CompoundArray  : TArray<Single>    read GetCompoundArray   write SetCompoundArrayProp;

    property Transform      : IGPMatrix         read GetTransform       write SetTransformProp;

  end;

{$IFDEF MSWINDOWS}
  TIGPPen = class( TIGPBase, IGPPen, IGPTransformable )
  protected
    FNativePen : GpPen;
    
  protected
    procedure SetNativePen(nativePen: GpPen);
    function  GetNativePen() : GpPen;
    constructor CreateGdiPlus(nativePen: GpPen; Dummy : Boolean);

  public
    constructor Create(color: TAlphaColor; width: Single = 1.0); overload;
    constructor Create( brush: IGPBrush; width: Single = 1.0); overload;
    destructor  Destroy(); override;

  public
    function  Clone() : TIGPPen;
    
    procedure SetWidthProp(width: Single);
    function  SetWidth(width: Single) : TIGPPen;
    function  GetWidth() : Single;
    // Set/get line caps: start, end, and dash
    // Line cap and join APIs by using LineCap and LineJoin enums.
    function  SetLineCap(startCap, endCap: TIGPLineCap; dashCap: TIGPDashCap) : TIGPPen;
    
    procedure SetStartCapProp(startCap: TIGPLineCap);
    function  SetStartCap(startCap: TIGPLineCap) : TIGPPen;
    function  GetStartCap() : TIGPLineCap;
    
    procedure SetEndCapProp(endCap: TIGPLineCap);
    function  SetEndCap(endCap: TIGPLineCap) : TIGPPen;
    function  GetEndCap() : TIGPLineCap;

    procedure SetDashCapProp(dashCap: TIGPDashCap);
    function  SetDashCap(dashCap: TIGPDashCap) : TIGPPen;
    function  GetDashCap() : TIGPDashCap;

    procedure SetLineJoinProp(lineJoin: TIGPLineJoin);
    function  SetLineJoin(lineJoin: TIGPLineJoin) : TIGPPen;
    function  GetLineJoin() : TIGPLineJoin;

    procedure SetCustomStartCapProp(customCap: IGPCustomLineCap);    
    function  SetCustomStartCap(customCap: IGPCustomLineCap) : TIGPPen;
    function  GetCustomStartCap() : IGPCustomLineCap;
    
    procedure SetCustomEndCapProp(customCap: IGPCustomLineCap);    
    function  SetCustomEndCap(customCap: IGPCustomLineCap) : TIGPPen;
    function  GetCustomEndCap() : IGPCustomLineCap;
    
    procedure SetMiterLimitProp(miterLimit: Single);
    function  SetMiterLimit(miterLimit: Single) : TIGPPen;
    function  GetMiterLimit() : Single;
    
    procedure SetAlignmentProp( penAlignment: TIGPPenAlignment);
    function  SetAlignment( penAlignment: TIGPPenAlignment) : TIGPPen;
    function  GetAlignment() : TIGPPenAlignment;
              
    procedure SetTransformProp(matrix: IGPMatrix);
    function  SetTransform(matrix: IGPMatrix) : TIGPPen;
    function  GetTransform() : IGPMatrix;
    
    function  ResetTransform() : TIGPPen;
    function  MultiplyTransform(matrix: IGPMatrix; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPPen;
    function  TranslateTransform(dx, dy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPPen;
    function  ScaleTransform(sx, sy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPPen; overload;
    function  ScaleTransform(s: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPPen; overload;
    function  RotateTransform(angle: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPPen;
              
    function  GetPenType() : TIGPPenType;
    
    procedure SetColorProp(color: TAlphaColor);
    function  SetColor(color: TAlphaColor) : TIGPPen;
    function  GetColor() : TAlphaColor;

    procedure SetBrushProp( brush: IGPBrush);
    function  SetBrush( brush: IGPBrush) : TIGPPen;
    function  GetBrush() : IGPBrush;
    
    procedure SetDashStyleProp(dashStyle: TIGPDashStyle);
    function  SetDashStyle( dashStyle: TIGPDashStyle ) : TIGPPen;
    function  GetDashStyle() : TIGPDashStyle;
    
    procedure SetDashOffsetProp( dashOffset: Single );
    function  SetDashOffset( dashOffset: Single ) : TIGPPen;
    function  GetDashOffset() : Single;
    
    function  GetDashPatternCount() : Integer;
    function  SetDashPattern( const dashArray: array of Single ) : TIGPPen;
    procedure SetDashPatternProp( dashArray: TArray<Single> );
    function  GetDashPattern() : TArray<Single>;

    function  GetCompoundArrayCount() : Integer;
    function  SetCompoundArray( const compoundArray: array of Single ) : TIGPPen;
    procedure SetCompoundArrayProp( compoundArray: TArray<Single> );
    function  GetCompoundArray() : TArray<Single>;
    
  protected
    function SetTransformT(matrix: IGPMatrix) : IGPTransformable;
    function ResetTransformT() : IGPTransformable;
    function MultiplyTransformT(matrix: IGPMatrix; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function TranslateTransformT(dx, dy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function ScaleTransformT(sx, sy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function ScaleTransformXYT(s: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function RotateTransformT(angle: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;

    function IGPTransformable.SetTransform = SetTransformT;
    function IGPTransformable.ResetTransform = ResetTransformT;
    function IGPTransformable.MultiplyTransform = MultiplyTransformT;
    function IGPTransformable.TranslateTransform = TranslateTransformT;
    function IGPTransformable.ScaleTransform = ScaleTransformT;
    function  IGPTransformable.ScaleTransformXY = ScaleTransformXYT;
    function IGPTransformable.RotateTransform = RotateTransformT;

  end;
{$ENDIF}

(**************************************************************************\
*
*   GDI+ StringFormat class
*
\**************************************************************************)

  IGPStringFormat = interface
    ['{F07F7F74-9E3C-4B01-BC57-B892B69B6FD3}']
    function  GetNativeFormat() : GpStringFormat;
    
    function  Clone() : TIGPStringFormat;

    function  SetFormatFlags(flags: Integer) : TIGPStringFormat;
    procedure SetFormatFlagsProp(flags: Integer);
    function  GetFormatFlags() : Integer;

    function  SetAlignment(align: TIGPStringAlignment) : TIGPStringFormat;
    procedure SetAlignmentProp(align: TIGPStringAlignment);
    function  GetAlignment() : TIGPStringAlignment;

    function  SetLineAlignment(align: TIGPStringAlignment) : TIGPStringFormat;
    procedure SetLineAlignmentProp(align: TIGPStringAlignment);
    function  GetLineAlignment() : TIGPStringAlignment;
    
    function  SetHotkeyPrefix(hotkeyPrefix: TIGPHotkeyPrefix) : TIGPStringFormat;
    procedure SetHotkeyPrefixProp(hotkeyPrefix: TIGPHotkeyPrefix);
    function  GetHotkeyPrefix() : TIGPHotkeyPrefix;
    
    function  SetTabStops(firstTabOffset: Single; const tabStops : array of Single ) : TIGPStringFormat;
    function  GetTabStopCount() : Integer;
    function  GetTabStops( out initialTabOffset : Single ) : TArray<Single>; overload;
    function  GetTabStops() : TArray<Single>; overload;
    function  GetTabStopsProp() : TArray<Single>;
    function  GetInitialTabOffset() : Single;

    function  SetDigitSubstitution(language: LANGID; substitute: TIGPStringDigitSubstitute) : TIGPStringFormat;
    function  GetDigitSubstitutionLanguage() : LANGID;
    function  GetDigitSubstitutionMethod() : TIGPStringDigitSubstitute;

    function  SetTrimming(trimming: TIGPStringTrimming) : TIGPStringFormat;
    procedure SetTrimmingProp(trimming: TIGPStringTrimming);
    function  GetTrimming() : TIGPStringTrimming;
    
    function  SetMeasurableCharacterRanges( const ranges : array of TIGPCharacterRange ) : TIGPStringFormat;
    function  GetMeasurableCharacterRangeCount() : Integer;

    property FormatFlags                : Integer  read GetFormatFlags write SetFormatFlagsProp;
    property Alignment                  : TIGPStringAlignment read GetAlignment write SetAlignmentProp;
    property LineAlignment              : TIGPStringAlignment read GetLineAlignment write SetLineAlignmentProp;
    property HotkeyPrefix               : TIGPHotkeyPrefix read GetHotkeyPrefix write SetHotkeyPrefixProp;

    property TabStopCount               : Integer         read GetTabStopCount;
    property TabStops                   : TArray<Single>  read GetTabStopsProp;
    property InitialTabOffset           : Single          read GetInitialTabOffset;

    property DigitSubstitutionLanguage  : LANGID         read GetDigitSubstitutionLanguage;
    property DigitSubstitutionMethod    : TIGPStringDigitSubstitute  read GetDigitSubstitutionMethod;

    property Trimming                   : TIGPStringTrimming         read GetTrimming write SetTrimmingProp;
    
  end;
  
{$IFDEF MSWINDOWS}
  TIGPStringFormat = class( TIGPBase, IGPStringFormat )
  protected
    FNativeFormat : GpStringFormat;

    function GetNativeFormat() : GpStringFormat;
     
  protected
    procedure Assign(source: TIGPStringFormat);
    constructor CreateGdiPlus(clonedStringFormat: GpStringFormat; Dummy : Boolean);

  public
    constructor Create(formatFlags: Integer = 0; language: LANGID = LANG_NEUTRAL); overload;
    constructor Create(format: TIGPStringFormat); overload;
    destructor  Destroy(); override;

  public
    function Clone() : TIGPStringFormat;

    function  SetFormatFlags(flags: Integer) : TIGPStringFormat;
    procedure SetFormatFlagsProp(flags: Integer);
    function  GetFormatFlags() : Integer;

    function  SetAlignment(align: TIGPStringAlignment) : TIGPStringFormat;
    procedure SetAlignmentProp(align: TIGPStringAlignment);
    function  GetAlignment() : TIGPStringAlignment;

    function  SetLineAlignment(align: TIGPStringAlignment) : TIGPStringFormat;
    procedure SetLineAlignmentProp(align: TIGPStringAlignment);
    function  GetLineAlignment() : TIGPStringAlignment;

    function  SetHotkeyPrefix(hotkeyPrefix: TIGPHotkeyPrefix) : TIGPStringFormat;
    procedure SetHotkeyPrefixProp(hotkeyPrefix: TIGPHotkeyPrefix);
    function  GetHotkeyPrefix() : TIGPHotkeyPrefix;

    function  SetTabStops( firstTabOffset: Single; const tabStops : array of Single ) : TIGPStringFormat;
    function  GetTabStopCount() : Integer;
    function  GetTabStops( out initialTabOffset : Single ) : TArray<Single>; overload;
    function  GetTabStops() : TArray<Single>; overload;
    function  GetTabStopsProp() : TArray<Single>;
    function  GetInitialTabOffset() : Single;

    function  SetDigitSubstitution(language: LANGID; substitute: TIGPStringDigitSubstitute) : TIGPStringFormat;
    function  GetDigitSubstitutionLanguage() : LANGID;
    function  GetDigitSubstitutionMethod() : TIGPStringDigitSubstitute;

    function  SetTrimming(trimming: TIGPStringTrimming) : TIGPStringFormat;
    procedure SetTrimmingProp(trimming: TIGPStringTrimming);
    function  GetTrimming() : TIGPStringTrimming;

    function  SetMeasurableCharacterRanges( const ranges : array of TIGPCharacterRange ) : TIGPStringFormat;
    function  GetMeasurableCharacterRangeCount() : Integer;

  public
    class function GenericDefault() : TIGPStringFormat;
    class function GenericTypographic() : TIGPStringFormat;

  end;
{$ENDIF}

(**************************************************************************\
*
*   GDI+ Graphics Path class
*
\**************************************************************************)

  IGPGraphicsPath = interface
    ['{E83A7063-6F55-4A3C-AC91-0B14DF5D5510}']
    function  GetNativePath() : GpPath;
    function  Clone() : TIGPGraphicsPath;
    // Reset the path object to empty (and fill mode to FillModeAlternate)
    function  Reset() : TIGPGraphicsPath;
    function  GetFillMode() : TIGPFillMode;
    function  SetFillMode(fillmode: TIGPFillMode) : TIGPGraphicsPath;
    procedure SetFillModeProp(fillmode: TIGPFillMode);
    function  GetPathData() : IGPPathData;
    function  StartFigure() : TIGPGraphicsPath;
    function  CloseFigure() : TIGPGraphicsPath;
    function  CloseAllFigures() : TIGPGraphicsPath;
    function  SetMarker() : TIGPGraphicsPath;
    function  ClearMarkers() : TIGPGraphicsPath;
    function  Reverse() : TIGPGraphicsPath;
    function  GetLastPoint() : TPointF;

    function  AddLineF(const pt1, pt2: TPointF) : TIGPGraphicsPath; overload;
    function  AddLineF(x1, y1, x2, y2: Single) : TIGPGraphicsPath; overload;
    function  AddLinesF(const points: array of TPointF) : TIGPGraphicsPath; overload;
    function  AddLine(const pt1, pt2: TPoint) : TIGPGraphicsPath; overload;
    function  AddLine(x1, y1, x2, y2: Integer) : TIGPGraphicsPath; overload;
    function  AddLines(const points: array of TPoint) : TIGPGraphicsPath; overload;

    function  AddArcF( const rect: TIGPRectF; startAngle, sweepAngle: Single) : TIGPGraphicsPath; overload;
    function  AddArcF(x, y, width, height, startAngle, sweepAngle: Single) : TIGPGraphicsPath; overload;
    function  AddArc( const rect: TIGPRect; startAngle, sweepAngle: Single) : TIGPGraphicsPath; overload;
    function  AddArc(x, y, width, height: Integer; startAngle, sweepAngle: Single) : TIGPGraphicsPath; overload;

    function  AddBezierF(const pt1, pt2, pt3, pt4: TPointF) : TIGPGraphicsPath; overload;
    function  AddBezierF(x1, y1, x2, y2, x3, y3, x4, y4: Single) : TIGPGraphicsPath; overload;
    function  AddBeziersF(const points: array of TPointF) : TIGPGraphicsPath; overload;
    function  AddBezier(const pt1, pt2, pt3, pt4: TPoint) : TIGPGraphicsPath; overload;
    function  AddBezier(x1, y1, x2, y2, x3, y3, x4, y4: Integer) : TIGPGraphicsPath; overload;
    function  AddBeziers(const points: array of TPoint) : TIGPGraphicsPath; overload;

    function  AddCurveF(const points: array of TPointF) : TIGPGraphicsPath; overload;
    function  AddCurveF(const points: array of TPointF; tension: Single) : TIGPGraphicsPath; overload;
    function  AddCurveF(const points: array of TPointF; offset, numberOfSegments: Integer; tension: Single) : TIGPGraphicsPath; overload;
    function  AddCurve(const points: array of TPoint) : TIGPGraphicsPath; overload;
    function  AddCurve(const points: array of TPoint; tension: Single) : TIGPGraphicsPath; overload;
    function  AddCurve(const points: array of TPoint; offset, numberOfSegments: Integer; tension: Single) : TIGPGraphicsPath; overload;

    function  AddClosedCurveF(const points: array of TPointF) : TIGPGraphicsPath; overload;
    function  AddClosedCurveF(const points: array of TPointF; tension: Single) : TIGPGraphicsPath; overload;
    function  AddClosedCurve(const points: array of TPoint) : TIGPGraphicsPath; overload;
    function  AddClosedCurve(const points: array of TPoint; tension: Single) : TIGPGraphicsPath; overload;

    function  AddRectangleF( const rect: TIGPRectF ) : TIGPGraphicsPath; overload;
    function  AddRectangleF(x, y, width, height: Single) : TIGPGraphicsPath; overload;
    function  AddRectangle( const rect: TIGPRect ) : TIGPGraphicsPath; overload;
    function  AddRectangle(x, y, width, height: Integer) : TIGPGraphicsPath; overload;

    function  AddRoundRectangleF( const ARect: TIGPRectF; const ACornerSize : TIGPSizeF ) : TIGPGraphicsPath;
    function  AddRoundRectangle( const ARect: TIGPRect; const ACornerSize : TIGPSize ) : TIGPGraphicsPath;

    function  AddRectanglesF( const rects: array of TIGPRectF) : TIGPGraphicsPath;
    function  AddRectangles( const rects: array of TIGPRect) : TIGPGraphicsPath;
    
    function  AddEllipseF( const rect: TIGPRectF) : TIGPGraphicsPath; overload;
    function  AddEllipseF(x, y, width, height: Single) : TIGPGraphicsPath; overload;
    function  AddEllipse( const rect: TIGPRect) : TIGPGraphicsPath; overload;
    function  AddEllipse(x, y, width, height: Integer) : TIGPGraphicsPath; overload;

    function  AddPieF( const rect: TIGPRectF; startAngle, sweepAngle: Single) : TIGPGraphicsPath; overload;
    function  AddPieF(x, y, width, height, startAngle, sweepAngle: Single) : TIGPGraphicsPath; overload;
    function  AddPie( const rect: TIGPRect; startAngle, sweepAngle: Single) : TIGPGraphicsPath; overload;
    function  AddPie(x, y, width, height: Integer; startAngle, sweepAngle: Single) : TIGPGraphicsPath; overload;

    function  AddPolygonF(const points: array of TPointF) : TIGPGraphicsPath;
    function  AddPolygon(const points: array of TPoint) : TIGPGraphicsPath;

    function  AddPath(addingPath: IGPGraphicsPath; connect: Boolean) : TIGPGraphicsPath;

    function  AddStringF(string_: WideString; font : IGPFont;
      const origin : TPointF; format : IGPStringFormat) : TIGPGraphicsPath; overload;
    function  AddStringF(string_: WideString; font : IGPFont;
      const layoutRect: TIGPRectF; format : IGPStringFormat) : TIGPGraphicsPath; overload;
    function  AddString(string_: WideString; font : IGPFont;
      const origin : TPoint; format : IGPStringFormat) : TIGPGraphicsPath; overload;
    function  AddString(string_: WideString; font : IGPFont;
      const layoutRect: TIGPRect; format : IGPStringFormat) : TIGPGraphicsPath; overload;
      
    function  AddStringF(string_: WideString; family : IGPFontFamily;
      style : Integer; emSize : Single; const origin : TPointF; format : IGPStringFormat) : TIGPGraphicsPath; overload;
    function  AddStringF(string_: WideString; family : IGPFontFamily;
      style  : Integer; emSize : Single; const layoutRect: TIGPRectF; format : IGPStringFormat) : TIGPGraphicsPath; overload;
    function  AddString(string_: WideString; family : IGPFontFamily;
      style  : Integer; emSize : Single; const origin : TPoint; format : IGPStringFormat) : TIGPGraphicsPath; overload;
    function  AddString(string_: WideString; family : IGPFontFamily;
      style  : Integer; emSize : Single; const layoutRect: TIGPRect; format : IGPStringFormat) : TIGPGraphicsPath; overload;

    function  Transform(matrix: IGPMatrix) : TIGPGraphicsPath;

    // This is not always the tightest bounds.
    function  GetBoundsF( matrix: IGPMatrix = NIL; pen: IGPPen = NIL) : TIGPRectF;
    function  GetBounds( matrix: IGPMatrix = NIL; pen: IGPPen = NIL) : TIGPRect;
    
    // Once flattened, the resultant path is made of line segments and
    // the original path information is lost.  When matrix is NULL the
    // identity matrix is assumed.
    function  Flatten(matrix: IGPMatrix = NIL; flatness: Single = FlatnessDefault) : TIGPGraphicsPath;
    function  Widen( pen: IGPPen; matrix: IGPMatrix = NIL; flatness: Single = FlatnessDefault) : TIGPGraphicsPath;
    function  Outline(matrix: IGPMatrix = NIL; flatness: Single = FlatnessDefault) : TIGPGraphicsPath;
    // Once this is called, the resultant path is made of line segments and
    // the original path information is lost.  When matrix is NULL, the
    // identity matrix is assumed.
    function  Warp( const destPoints : array of TPointF; const srcRect: TIGPRectF;
      matrix: IGPMatrix = NIL; warpMode: TIGPWarpMode = WarpModePerspective;
      flatness: Single = FlatnessDefault) : TIGPGraphicsPath;
    function  GetPointCount() : Integer;
    function  GetPathTypes(types: PBYTE; count: Integer) : TIGPGraphicsPath;
    function  GetPathPointsF() : TArray<TPointF>;
    function  GetPathPoints() : TArray<TPoint>;

    function  IsVisibleF(const point: TPointF; g: IGPGraphics = NIL) : Boolean; overload;
    function  IsVisibleF(x, y: Single; g: IGPGraphics = NIL) : Boolean; overload;
    function  IsVisible( const point: TPoint; g : IGPGraphics = NIL) : Boolean; overload;
    function  IsVisible(x, y: Integer; g: IGPGraphics = NIL) : Boolean; overload;

    function  IsOutlineVisibleF( const point: TPointF; pen: IGPPen; g: IGPGraphics = NIL) : Boolean; overload;
    function  IsOutlineVisibleF(x, y: Single; pen: IGPPen; g: IGPGraphics = NIL) : Boolean; overload;
    function  IsOutlineVisible( const point: TPoint; pen: IGPPen; g: IGPGraphics = NIL) : Boolean; overload;
    function  IsOutlineVisible(x, y: Integer; pen: IGPPen; g: IGPGraphics = NIL) : Boolean; overload;
    
    property LastPoint : TPointF    read GetLastPoint;
    property FillMode  : TIGPFillMode  read GetFillMode  write SetFillModeProp;

  end;
  
{$IFDEF MSWINDOWS}
  TIGPGraphicsPath = class( TIGPBase, IGPGraphicsPath )
  protected
    FNativePath: GpPath;
    
  protected
    procedure SetNativePath(nativePath: GpPath);
    constructor CreateGdiPlus(nativePath: GpPath; Dummy : Boolean);
  public
    constructor Create(path: IGPGraphicsPath); overload;
    constructor Create(fillMode: TIGPFillMode = FillModeAlternate); overload;
    constructor Create( const points : array of TPointF; const types : array of BYTE;
      fillMode: TIGPFillMode = FillModeAlternate ); overload;
    constructor Create( const points : array of TPoint; const types : array of BYTE;
      fillMode: TIGPFillMode = FillModeAlternate ); overload;
    destructor  Destroy(); override;
  public
    function  GetNativePath() : GpPath;
  public
    function  Clone() : TIGPGraphicsPath;
    // Reset the path object to empty (and fill mode to FillModeAlternate)
    function  Reset() : TIGPGraphicsPath;
    function  GetFillMode() : TIGPFillMode;
    function  SetFillMode(fillmode: TIGPFillMode) : TIGPGraphicsPath;
    procedure SetFillModeProp(fillmode: TIGPFillMode);
    function  GetPathData() : IGPPathData;
    function  StartFigure() : TIGPGraphicsPath;
    function  CloseFigure() : TIGPGraphicsPath;
    function  CloseAllFigures() : TIGPGraphicsPath;
    function  SetMarker() : TIGPGraphicsPath;
    function  ClearMarkers() : TIGPGraphicsPath;
    function  Reverse() : TIGPGraphicsPath;
    function  GetLastPoint() : TPointF;

    function  AddLineF(const pt1, pt2: TPointF) : TIGPGraphicsPath; overload;
    function  AddLineF(x1, y1, x2, y2: Single) : TIGPGraphicsPath; overload;
    function  AddLinesF(const points: array of TPointF) : TIGPGraphicsPath;
    function  AddLine(const pt1, pt2: TPoint) : TIGPGraphicsPath; overload;
    function  AddLine(x1, y1, x2, y2: Integer) : TIGPGraphicsPath; overload;
    function  AddLines(const points: array of TPoint) : TIGPGraphicsPath;

    function  AddArcF( const rect: TIGPRectF; startAngle, sweepAngle: Single) : TIGPGraphicsPath; overload;
    function  AddArcF(x, y, width, height, startAngle, sweepAngle: Single) : TIGPGraphicsPath; overload;
    function  AddArc( const rect: TIGPRect; startAngle, sweepAngle: Single) : TIGPGraphicsPath; overload;
    function  AddArc(x, y, width, height: Integer; startAngle, sweepAngle: Single) : TIGPGraphicsPath; overload;

    function  AddBezierF(const pt1, pt2, pt3, pt4: TPointF) : TIGPGraphicsPath; overload;
    function  AddBezierF(x1, y1, x2, y2, x3, y3, x4, y4: Single) : TIGPGraphicsPath; overload;
    function  AddBeziersF(const points: array of TPointF) : TIGPGraphicsPath;
    function  AddBezier(const pt1, pt2, pt3, pt4: TPoint) : TIGPGraphicsPath; overload;
    function  AddBezier(x1, y1, x2, y2, x3, y3, x4, y4: Integer) : TIGPGraphicsPath; overload;
    function  AddBeziers(const points: array of TPoint) : TIGPGraphicsPath;

    function  AddCurveF(const points: array of TPointF) : TIGPGraphicsPath; overload;
    function  AddCurveF(const points: array of TPointF; tension: Single) : TIGPGraphicsPath; overload;
    function  AddCurveF(const points: array of TPointF; offset, numberOfSegments: Integer; tension: Single) : TIGPGraphicsPath; overload;
    function  AddCurve(const points: array of TPoint) : TIGPGraphicsPath; overload;
    function  AddCurve(const points: array of TPoint; tension: Single) : TIGPGraphicsPath; overload;
    function  AddCurve(const points: array of TPoint; offset, numberOfSegments: Integer; tension: Single) : TIGPGraphicsPath; overload;

    function  AddClosedCurveF(const points: array of TPointF) : TIGPGraphicsPath; overload;
    function  AddClosedCurveF(const points: array of TPointF; tension: Single) : TIGPGraphicsPath; overload;
    function  AddClosedCurve(const points: array of TPoint) : TIGPGraphicsPath; overload;
    function  AddClosedCurve(const points: array of TPoint; tension: Single) : TIGPGraphicsPath; overload;

    function  AddRectangleF( const rect: TIGPRectF) : TIGPGraphicsPath; overload;
    function  AddRectangleF(x, y, width, height: Single) : TIGPGraphicsPath; overload;
    function  AddRectangle( const rect: TIGPRect) : TIGPGraphicsPath; overload;
    function  AddRectangle(x, y, width, height: Integer) : TIGPGraphicsPath; overload;

    function  AddRoundRectangleF( const ARect: TIGPRectF; const ACornerSize : TIGPSizeF ) : TIGPGraphicsPath;
    function  AddRoundRectangle( const ARect: TIGPRect; const ACornerSize : TIGPSize ) : TIGPGraphicsPath;

    function  AddRectanglesF( const rects: array of TIGPRectF ) : TIGPGraphicsPath;
    function  AddRectangles( const rects: array of TIGPRect ) : TIGPGraphicsPath;
    
    function  AddEllipseF( const rect: TIGPRectF) : TIGPGraphicsPath; overload;
    function  AddEllipseF(x, y, width, height: Single) : TIGPGraphicsPath; overload;
    function  AddEllipse( const rect: TIGPRect) : TIGPGraphicsPath; overload;
    function  AddEllipse(x, y, width, height: Integer) : TIGPGraphicsPath; overload;

    function  AddPieF( const rect: TIGPRectF; startAngle, sweepAngle: Single) : TIGPGraphicsPath; overload;
    function  AddPieF(x, y, width, height, startAngle, sweepAngle: Single) : TIGPGraphicsPath; overload;
    function  AddPie( const rect: TIGPRect; startAngle, sweepAngle: Single) : TIGPGraphicsPath; overload;
    function  AddPie(x, y, width, height: Integer; startAngle, sweepAngle: Single) : TIGPGraphicsPath; overload;

    function  AddPolygonF(const points: array of TPointF) : TIGPGraphicsPath;
    function  AddPolygon(const points: array of TPoint) : TIGPGraphicsPath;

    function  AddPath(addingPath: IGPGraphicsPath; connect: Boolean) : TIGPGraphicsPath;

    function  AddStringF(string_: WideString; font : IGPFont;
      const origin : TPointF; format : IGPStringFormat) : TIGPGraphicsPath; overload;
    function  AddStringF(string_: WideString; font : IGPFont;
      const layoutRect: TIGPRectF; format : IGPStringFormat) : TIGPGraphicsPath; overload;
    function  AddString(string_: WideString; font : IGPFont;
      const origin : TPoint; format : IGPStringFormat) : TIGPGraphicsPath; overload;
    function  AddString(string_: WideString; font : IGPFont;
      const layoutRect: TIGPRect; format : IGPStringFormat) : TIGPGraphicsPath; overload;
      
    function  AddStringF(string_: WideString; family : IGPFontFamily;
      style : Integer; emSize : Single; const origin : TPointF; format : IGPStringFormat) : TIGPGraphicsPath; overload;
    function  AddStringF(string_: WideString; family : IGPFontFamily;
      style  : Integer; emSize : Single; const layoutRect: TIGPRectF; format : IGPStringFormat) : TIGPGraphicsPath; overload;
    function  AddString(string_: WideString; family : IGPFontFamily;
      style  : Integer; emSize : Single; const origin : TPoint; format : IGPStringFormat) : TIGPGraphicsPath; overload;
    function  AddString(string_: WideString; family : IGPFontFamily;
      style  : Integer; emSize : Single; const layoutRect: TIGPRect; format : IGPStringFormat) : TIGPGraphicsPath; overload;

    function  Transform(matrix: IGPMatrix) : TIGPGraphicsPath;

    // This is not always the tightest bounds.
    function  GetBoundsF( matrix: IGPMatrix = NIL; pen: IGPPen = NIL) : TIGPRectF;
    function  GetBounds( matrix: IGPMatrix = NIL; pen: IGPPen = NIL) : TIGPRect;
    
    // Once flattened, the resultant path is made of line segments and
    // the original path information is lost.  When matrix is NULL the
    // identity matrix is assumed.
    function  Flatten(matrix: IGPMatrix = NIL; flatness: Single = FlatnessDefault) : TIGPGraphicsPath;
    function  Widen( pen: IGPPen; matrix: IGPMatrix = NIL; flatness: Single = FlatnessDefault) : TIGPGraphicsPath;
    function  Outline(matrix: IGPMatrix = NIL; flatness: Single = FlatnessDefault) : TIGPGraphicsPath;
    // Once this is called, the resultant path is made of line segments and
    // the original path information is lost.  When matrix is NULL, the
    // identity matrix is assumed.
    function  Warp( const destPoints : array of TPointF; const srcRect: TIGPRectF;
      matrix: IGPMatrix = NIL; warpMode: TIGPWarpMode = WarpModePerspective;
      flatness: Single = FlatnessDefault) : TIGPGraphicsPath;
    function  GetPointCount() : Integer;
    function  GetPathTypes(types: PBYTE; count: Integer) : TIGPGraphicsPath;
    function  GetPathPointsF() : TArray<TPointF>;
    function  GetPathPoints() : TArray<TPoint>;

    function  IsVisibleF(const point: TPointF; g: IGPGraphics = NIL) : Boolean; overload;
    function  IsVisibleF(x, y: Single; g: IGPGraphics = NIL) : Boolean; overload;
    function  IsVisible(const point: TPoint; g : IGPGraphics = NIL) : Boolean; overload;
    function  IsVisible(x, y: Integer; g: IGPGraphics = NIL) : Boolean; overload;

    function  IsOutlineVisibleF(const point: TPointF; pen: IGPPen; g: IGPGraphics = NIL) : Boolean; overload;
    function  IsOutlineVisibleF(x, y: Single; pen: IGPPen; g: IGPGraphics = NIL) : Boolean; overload;
    function  IsOutlineVisible(const point: TPoint; pen: IGPPen; g: IGPGraphics = NIL) : Boolean; overload;
    function  IsOutlineVisible(x, y: Integer; pen: IGPPen; g: IGPGraphics = NIL) : Boolean; overload;
    
  end;
{$ENDIF}

//--------------------------------------------------------------------------
// GraphisPathIterator class
//--------------------------------------------------------------------------

  IGPGraphicsPathIterator = interface
    ['{893BF228-EE25-4FE5-B8F7-20997B95749C}']
    function  NextSubpath(out startIndex, endIndex: Integer; out isClosed: Boolean) : Integer; overload;
    function  NextSubpath(path: IGPGraphicsPath; out isClosed: Boolean) : Integer; overload;
    function  NextPathType(out pathType: TIGPPathPointType; out startIndex, endIndex: Integer) : Integer;
    function  NextMarker(out startIndex, endIndex: Integer) : Integer; overload;
    function  NextMarker(path: IGPGraphicsPath) : Integer; overload;
    function  GetCount() : Integer;
    function  GetSubpathCount() : Integer;
    function  HasCurve() : Boolean;
    function  Rewind() : TIGPGraphicsPathIterator;
    function  Enumerate( out points: TArray<TPointF>; out types: TArray<Byte> ) : Integer;
    function  CopyData(points: PGPPointF; types: PBYTE; startIndex, endIndex: Integer) : Integer;
  end;
  
{$IFDEF MSWINDOWS}
  TIGPGraphicsPathIterator = class( TIGPBase, IGPGraphicsPathIterator )
  protected
    FNativeIterator: GpPathIterator;
    
  protected
    procedure SetNativeIterator(nativeIterator: GpPathIterator);
    
  public
    constructor Create(path: IGPGraphicsPath); reintroduce;
    destructor  Destroy(); override;
    
  public
    function  NextSubpath(out startIndex, endIndex: Integer; out isClosed: Boolean) : Integer; overload;
    function  NextSubpath(path: IGPGraphicsPath; out isClosed: Boolean) : Integer; overload;
    function  NextPathType(out pathType: TIGPPathPointType; out startIndex, endIndex: Integer) : Integer;
    function  NextMarker(out startIndex, endIndex: Integer) : Integer; overload;
    function  NextMarker(path: IGPGraphicsPath) : Integer; overload;
    function  GetCount() : Integer;
    function  GetSubpathCount() : Integer;
    function  HasCurve() : Boolean;
    function  Rewind() : TIGPGraphicsPathIterator;
    function  Enumerate( out points: TArray<TPointF>; out types: TArray<Byte> ) : Integer;
    function  CopyData(points: PGPPointF; types: PBYTE; startIndex, endIndex: Integer) : Integer;

  end;
{$ENDIF}

//--------------------------------------------------------------------------
// Path Gradient Brush
//--------------------------------------------------------------------------

  IGPPathGradientBrush = interface( IGPBlendBrush )
    ['{C76439FD-D91B-44B5-91EB-E670B8E94A32}']
    function  GetCenterColor() : TAlphaColor;
    function  SetCenterColor(color: TAlphaColor) : TIGPPathGradientBrush;
    procedure SetCenterColorProp(color: TAlphaColor);

    function  GetPointCount() : Integer;
    function  GetSurroundColorCount() : Integer;

    function  SetSurroundColors(const colors : array of TAlphaColor ) : TIGPPathGradientBrush;
    procedure SetSurroundColorsProp(colors : TArray<TAlphaColor> );
    function  GetSurroundColors() : TArray<TAlphaColor>;

    function  GetGraphicsPath() : IGPGraphicsPath;
    function  SetGraphicsPath(path: IGPGraphicsPath) : TIGPPathGradientBrush;
    procedure SetGraphicsPathProp(path: IGPGraphicsPath);

    procedure SetCenterPointFProp(const point: TPointF);
    function  SetCenterPointF(const point: TPointF) : TIGPPathGradientBrush;
    function  GetCenterPointF() : TPointF;

    function  SetCenterPoint(const point: TPoint) : TIGPPathGradientBrush;
    function  GetCenterPoint() : TPoint;
    
    function  GetRectangleF() : TIGPRectF;
    function  GetRectangle() : TIGPRect;
    
    function  SetGammaCorrection(useGammaCorrection: Boolean) : TIGPPathGradientBrush;
    
    function  SetBlendArrays( const blendFactors : array of Single; const blendPositions : array of Single ) : TIGPPathGradientBrush;
    function  SetBlend( const blendFactors : array of TIGPBlend ) : TIGPPathGradientBrush;
    procedure SetInterpolationColorsProp( Colors : TArray<TIGPInterpolationColor> );
    function  SetInterpolationColors( const Colors : array of TIGPInterpolationColor ) : TIGPPathGradientBrush;
    function  SetInterpolationColorArrays( const presetColors: array of TAlphaColor; const blendPositions: array of Single ) : TIGPPathGradientBrush;
    function  SetBlendBellShape(focus: Single; scale: Single = 1.0) : TIGPPathGradientBrush;
    function  SetBlendTriangularShape(focus: Single; scale: Single = 1.0) : TIGPPathGradientBrush;

    function  SetTransform(matrix: IGPMatrix) : TIGPPathGradientBrush;
    function  ResetTransform() : TIGPPathGradientBrush;
    function  MultiplyTransform(matrix: IGPMatrix; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPPathGradientBrush;
    function  TranslateTransform(dx, dy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPPathGradientBrush;
    function  ScaleTransform(sx, sy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPPathGradientBrush; overload;
    function  ScaleTransform(s: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPPathGradientBrush; overload;
    function  RotateTransform(angle: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPPathGradientBrush;
    function  GetFocusScales(out xScale, yScale: Single) : TIGPPathGradientBrush;
    function  SetFocusScales(xScale, yScale: Single) : TIGPPathGradientBrush;
    function  SetWrapMode(wrapMode: TIGPWrapMode) : TIGPPathGradientBrush;

    property CenterColor    : TAlphaColor        read GetCenterColor write SetCenterColorProp;
    property CenterPoint    : TPointF       read GetCenterPointF write SetCenterPointFProp;

    property SurroundColors : TArray<TAlphaColor>   read GetSurroundColors  write SetSurroundColorsProp;

  end;
  
{$IFDEF MSWINDOWS}
  TIGPPathGradientBrush = class(TIGPBrush, IGPPathGradientBrush, IGPTransformable)
  public
    constructor CreateF(const points: array of TPointF;
      wrapMode: TIGPWrapMode = WrapModeClamp);
    constructor Create(const points: array of TPoint;
      wrapMode: TIGPWrapMode = WrapModeClamp); overload;
    constructor Create(path: IGPGraphicsPath); overload;
    
  public
    function  GetCenterColor() : TAlphaColor;
    function  SetCenterColor(color: TAlphaColor) : TIGPPathGradientBrush;
    procedure SetCenterColorProp(color: TAlphaColor);
    
    function  GetPointCount() : Integer;
    
    function  GetSurroundColorCount() : Integer;

    function  SetSurroundColors(const colors : array of TAlphaColor ) : TIGPPathGradientBrush;
    procedure SetSurroundColorsProp(colors : TArray<TAlphaColor> );
    function  GetSurroundColors() : TArray<TAlphaColor>;

    function  GetGraphicsPath() : IGPGraphicsPath;
    function  SetGraphicsPath(path: IGPGraphicsPath) : TIGPPathGradientBrush;
    procedure SetGraphicsPathProp(path: IGPGraphicsPath);

    procedure SetCenterPointFProp(const point: TPointF);
    function  SetCenterPointF(const point: TPointF) : TIGPPathGradientBrush;
    function  GetCenterPointF() : TPointF;

    function  GetCenterPoint() : TPoint;
    function  SetCenterPoint(const point: TPoint) : TIGPPathGradientBrush;
    function  GetRectangleF() : TIGPRectF;
    function  GetRectangle() : TIGPRect;

    procedure SetGammaCorrectionProp(useGammaCorrection: Boolean);
    function  SetGammaCorrection(useGammaCorrection: Boolean) : TIGPPathGradientBrush;
    function  GetGammaCorrection() : Boolean;

    function  GetBlendCount() : Integer;
    function  GetBlend() : TArray<TIGPBlend>;
    function  SetBlendArrays( const blendFactors : array of Single; const blendPositions : array of Single ) : TIGPPathGradientBrush;
    function  SetBlend( const blendFactors : array of TIGPBlend ) : TIGPPathGradientBrush;
    procedure SetBlendProp( const blendFactors : TArray<TIGPBlend> );
    function  GetInterpolationColorCount() : Integer;
    function  SetInterpolationColors( const Colors : array of TIGPInterpolationColor ) : TIGPPathGradientBrush; overload;
    function  SetInterpolationColorArrays( const presetColors: array of TAlphaColor; const blendPositions: array of Single ) : TIGPPathGradientBrush;
    procedure SetInterpolationColorsProp( Colors : TArray<TIGPInterpolationColor> );
    function  GetInterpolationColors() : TArray<TIGPInterpolationColor>;
    function  SetBlendBellShape(focus: Single; scale: Single = 1.0) : TIGPPathGradientBrush;
    function  SetBlendTriangularShape(focus: Single; scale: Single = 1.0) : TIGPPathGradientBrush;
              
    function  GetTransform() : IGPMatrix;
    function  SetTransform(matrix: IGPMatrix) : TIGPPathGradientBrush;
    procedure SetTransformProp(matrix: IGPMatrix);
    function  ResetTransform() : TIGPPathGradientBrush;
    function  MultiplyTransform(matrix: IGPMatrix; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPPathGradientBrush;
    function  TranslateTransform(dx, dy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPPathGradientBrush;
    function  ScaleTransform(sx, sy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPPathGradientBrush; overload;
    function  ScaleTransform(s: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPPathGradientBrush; overload;
    function  RotateTransform(angle: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPPathGradientBrush;
    function  GetFocusScales(out xScale, yScale: Single) : TIGPPathGradientBrush;
    function  SetFocusScales(xScale, yScale: Single) : TIGPPathGradientBrush;
    function  GetWrapMode() : TIGPWrapMode;
    function  SetWrapMode(wrapMode: TIGPWrapMode) : TIGPPathGradientBrush;
    procedure SetWrapModeProp(wrapMode: TIGPWrapMode);
    
  protected
    function  SetTransformT(matrix: IGPMatrix) : IGPTransformable;
    function  ResetTransformT() : IGPTransformable;
    function  MultiplyTransformT(matrix: IGPMatrix; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function  TranslateTransformT(dx, dy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function  ScaleTransformT(sx, sy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function  ScaleTransformXYT(s: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function  RotateTransformT(angle: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;

    function  IGPTransformable.SetTransform = SetTransformT;
    function  IGPTransformable.ResetTransform = ResetTransformT;
    function  IGPTransformable.MultiplyTransform = MultiplyTransformT;
    function  IGPTransformable.TranslateTransform = TranslateTransformT;
    function  IGPTransformable.ScaleTransform = ScaleTransformT;
    function  IGPTransformable.ScaleTransformXY = ScaleTransformXYT;
    function  IGPTransformable.RotateTransform = RotateTransformT;

  end;
{$ENDIF}

(**************************************************************************\
*
*   GDI+ Graphics Object
*
\**************************************************************************)

  IGPGraphics = interface
    ['{95C573E8-DD62-41D4-83DE-2F32799BD922}']
{$IFDEF MSWINDOWS}
    function  GetNativeGraphics() : GpGraphics;
    function  Flush(intention: TIGPFlushIntention = FlushIntentionFlush) : TIGPGraphics;
    //------------------------------------------------------------------------
    // GDI Interop methods
    //------------------------------------------------------------------------
    // Locks the graphics until ReleaseDC is called
    function  GetHDC() : HDC;
    function  ReleaseHDC(hdc: HDC) : TIGPGraphics;
    //------------------------------------------------------------------------
    // Rendering modes
    //------------------------------------------------------------------------
    function  SetRenderingOrigin( const point : TPoint ) : TIGPGraphics;
    procedure SetRenderingOriginProp( const point : TPoint );
    function  GetRenderingOrigin() : TPoint;
    function  SetCompositingMode(compositingMode: TIGPCompositingMode) : TIGPGraphics;
    procedure SetCompositingModeProp(compositingMode: TIGPCompositingMode);
    function  GetCompositingMode() : TIGPCompositingMode;
    function  SetCompositingQuality(compositingQuality: TIGPCompositingQuality) : TIGPGraphics;
    procedure SetCompositingQualityProp(compositingQuality: TIGPCompositingQuality);
    function  GetCompositingQuality() : TIGPCompositingQuality;
    function  SetTextRenderingHint(newMode: TIGPTextRenderingHint) : TIGPGraphics;
    procedure SetTextRenderingHintProp(newMode: TIGPTextRenderingHint);
    function  GetTextRenderingHint() : TIGPTextRenderingHint;
    function  SetTextContrast(contrast: Cardinal) : TIGPGraphics; // 0..12
    procedure SetTextContrastProp(contrast: Cardinal); // 0..12
    function  GetTextContrast() : Cardinal;
    function  GetInterpolationMode() : TIGPInterpolationMode;
    function  SetInterpolationMode(interpolationMode: TIGPInterpolationMode) : TIGPGraphics;
    procedure SetInterpolationModeProp(interpolationMode: TIGPInterpolationMode);
    function  GetSmoothingMode() : TIGPSmoothingMode;
    function  SetSmoothingMode(smoothingMode: TIGPSmoothingMode) : TIGPGraphics;
    procedure SetSmoothingModeProp(smoothingMode: TIGPSmoothingMode);
    function  GetPixelOffsetMode() : TIGPPixelOffsetMode;
    function  SetPixelOffsetMode(pixelOffsetMode: TIGPPixelOffsetMode) : TIGPGraphics;
    procedure SetPixelOffsetModeProp(pixelOffsetMode: TIGPPixelOffsetMode);
    //------------------------------------------------------------------------
    // Manipulate current world transform
    //------------------------------------------------------------------------
    function  SetTransform(matrix: IGPMatrix) : TIGPGraphics;
    procedure SetTransformProp(matrix: IGPMatrix);
    function  ResetTransform() : TIGPGraphics;
    function  MultiplyTransform(matrix: IGPMatrix; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPGraphics;
    function  TranslateTransform(dx, dy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPGraphics;
    function  ScaleTransform(sx, sy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPGraphics; overload;
    function  ScaleTransform( s : Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPGraphics; overload;
    function  RotateTransform(angle: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPGraphics;
    function  GetTransform() : IGPMatrix;
    function  SetPageUnit( unit_: TIGPUnit ) : TIGPGraphics;
    procedure SetPageUnitProp( unit_: TIGPUnit );
    function  SetPageScale( scale: Single ) : TIGPGraphics;
    procedure SetPageScaleProp( scale: Single );
    function  GetPageUnit() : TIGPUnit;
    function  GetPageScale() : Single;
    function  GetDpiX() : Single;
    function  GetDpiY() : Single;
    function  TransformPoints(destSpace: TIGPCoordinateSpace; srcSpace: TIGPCoordinateSpace;
      var pts : array of TPointF ) : TIGPGraphics; overload;
    function  TransformPoints(destSpace: TIGPCoordinateSpace; srcSpace: TIGPCoordinateSpace;
      var pts : array of TPoint ) : TIGPGraphics; overload;
    //------------------------------------------------------------------------
    // GetNearestColor (for <= 8bpp surfaces).  Note: Alpha is ignored.
    //------------------------------------------------------------------------
    function  GetNearestColor( AColor : TAlphaColor ) : TAlphaColor;

    // DrawLine(s)
    function  DrawLineF( pen: IGPPen; x1, y1, x2, y2: Single) : TIGPGraphics; overload;
    function  DrawLineF( pen: IGPPen; const pt1, pt2: TPointF) : TIGPGraphics; overload;
    function  DrawLinesF( pen: IGPPen; const points : array of TPointF ) : TIGPGraphics; overload;
    function  DrawLine( pen: IGPPen; x1, y1, x2, y2: Integer) : TIGPGraphics; overload;
    function  DrawLine( pen: IGPPen; const pt1, pt2: TPoint) : TIGPGraphics; overload;
    function  DrawLines( pen: IGPPen; const points : array of TPoint ) : TIGPGraphics; overload;

    // DrawArc
    function  DrawArcF( pen: IGPPen; x, y, width, height, startAngle, sweepAngle: Single) : TIGPGraphics; overload;
    function  DrawArcF( pen: IGPPen; const rect: TIGPRectF; startAngle, sweepAngle: Single) : TIGPGraphics; overload;
    function  DrawArc( pen: IGPPen; x, y, width, height: Integer; startAngle, sweepAngle: Single) : TIGPGraphics; overload;
    function  DrawArc( pen: IGPPen; const rect: TIGPRect; startAngle, sweepAngle: Single) : TIGPGraphics; overload;

    // DrawBezier(s)
    function  DrawBezierF( pen: IGPPen; x1, y1, x2, y2, x3, y3, x4, y4: Single) : TIGPGraphics; overload;
    function  DrawBezierF( pen: IGPPen; const pt1, pt2, pt3, pt4: TPointF) : TIGPGraphics; overload;
    function  DrawBeziersF( pen: IGPPen; const points : array of TPointF ) : TIGPGraphics;
    function  DrawBezier( pen: IGPPen; x1, y1, x2, y2, x3, y3, x4, y4: Integer) : TIGPGraphics; overload;
    function  DrawBezier( pen: IGPPen; const pt1, pt2, pt3, pt4: TPoint) : TIGPGraphics; overload;
    function  DrawBeziers( pen: IGPPen; const points : array of TPoint ) : TIGPGraphics;

    // DrawRectangle(s)
    function  DrawRectangleF( pen: IGPPen; const rect: TIGPRectF ) : TIGPGraphics; overload;
    function  DrawRectangleF( pen: IGPPen; x, y, width, height: Single ) : TIGPGraphics; overload;
    function  DrawRectanglesF( pen: IGPPen; const rects: array of TIGPRectF ) : TIGPGraphics; overload;
    function  DrawRectangle( pen: IGPPen; const rect: TIGPRect ) : TIGPGraphics; overload;
    function  DrawRectangle( pen: IGPPen; x, y, width, height: Integer ) : TIGPGraphics; overload;
    function  DrawRectangles( pen: IGPPen; const rects: array of TIGPRect ) : TIGPGraphics; overload;

    function  DrawRectangleF( pen: IGPPen; brush: IGPBrush; const rect: TIGPRectF) : TIGPGraphics; overload;
    function  DrawRectangleF( pen: IGPPen; brush: IGPBrush; x, y, width, height: Single) : TIGPGraphics; overload;
    function  DrawRectanglesF( pen: IGPPen; brush: IGPBrush; const rects: array of TIGPRectF ) : TIGPGraphics; overload;
    function  DrawRectangle( pen: IGPPen; brush: IGPBrush; const rect: TIGPRect ) : TIGPGraphics; overload;
    function  DrawRectangle( pen: IGPPen; brush: IGPBrush; x, y, width, height: Integer) : TIGPGraphics; overload;
    function  DrawRectangles( pen: IGPPen; brush: IGPBrush; const rects: array of TIGPRect ) : TIGPGraphics; overload;

    // DrawRoundRectangle
    function  DrawRoundRectangleF( pen: IGPPen; const rect: TIGPRectF; const ACornerSize : TIGPSizeF) : TIGPGraphics; overload;
    function  DrawRoundRectangle( pen: IGPPen; const rect: TIGPRect; const ACornerSize : TIGPSize) : TIGPGraphics; overload;

    function  DrawRoundRectangleF( pen: IGPPen; brush: IGPBrush; const rect: TIGPRectF; const ACornerSize : TIGPSizeF) : TIGPGraphics; overload;
    function  DrawRoundRectangle( pen: IGPPen; brush: IGPBrush; const rect: TIGPRect; const ACornerSize : TIGPSize) : TIGPGraphics; overload;

    // DrawEllipse
    function  DrawEllipseF( pen: IGPPen; const rect: TIGPRectF) : TIGPGraphics; overload;
    function  DrawEllipseF( pen: IGPPen; x, y, width, height: Single) : TIGPGraphics; overload;
    function  DrawEllipsesF( pen: IGPPen; const rects: array of TIGPRectF) : TIGPGraphics; overload;
    function  DrawEllipse( pen: IGPPen; const rect: TIGPRect) : TIGPGraphics; overload;
    function  DrawEllipse( pen: IGPPen; x, y, width, height: Integer) : TIGPGraphics; overload;
    function  DrawEllipses( pen: IGPPen; const rects: array of TIGPRect ) : TIGPGraphics; overload;

    function  DrawEllipseF( pen: IGPPen; brush: IGPBrush; const rect: TIGPRectF) : TIGPGraphics; overload;
    function  DrawEllipseF( pen: IGPPen; brush: IGPBrush; x, y, width, height: Single) : TIGPGraphics; overload;
    function  DrawEllipsesF( pen: IGPPen; brush: IGPBrush; const rects: array of TIGPRectF) : TIGPGraphics; overload;
    function  DrawEllipse( pen: IGPPen; brush: IGPBrush; const rect: TIGPRect) : TIGPGraphics; overload;
    function  DrawEllipse( pen: IGPPen; brush: IGPBrush; x, y, width, height: Integer) : TIGPGraphics; overload;
    function  DrawEllipses( pen: IGPPen; brush: IGPBrush; const rects: array of TIGPRect) : TIGPGraphics; overload;
    
    // DrawPie
    function  DrawPieF( pen: IGPPen; const rect: TIGPRectF; startAngle, sweepAngle: Single) : TIGPGraphics; overload;
    function  DrawPieF( pen: IGPPen; x, y, width, height, startAngle, sweepAngle: Single) : TIGPGraphics; overload;
    function  DrawPie( pen: IGPPen; const rect: TIGPRect; startAngle, sweepAngle: Single) : TIGPGraphics; overload;
    function  DrawPie( pen: IGPPen; x, y, width, height: Integer; startAngle, sweepAngle: Single) : TIGPGraphics; overload;

    function  DrawPieF( pen: IGPPen; brush: IGPBrush; const rect: TIGPRectF; startAngle, sweepAngle: Single) : TIGPGraphics; overload;
    function  DrawPieF( pen: IGPPen; brush: IGPBrush; x, y, width, height, startAngle, sweepAngle: Single) : TIGPGraphics; overload;
    function  DrawPie( pen: IGPPen; brush: IGPBrush; const rect: TIGPRect; startAngle, sweepAngle: Single) : TIGPGraphics; overload;
    function  DrawPie( pen: IGPPen; brush: IGPBrush; x, y, width, height: Integer; startAngle, sweepAngle: Single) : TIGPGraphics; overload;
    
    // DrawPolygon
    function  DrawPolygonF( pen: IGPPen; const points: array of TPointF ) : TIGPGraphics; overload;
    function  DrawPolygon( pen: IGPPen; const points: array of TPoint ) : TIGPGraphics; overload;

    function  DrawPolygonF( pen: IGPPen; brush: IGPBrush; const points: array of TPointF ) : TIGPGraphics; overload;
    function  DrawPolygonF( pen: IGPPen; brush: IGPBrush; const points: array of TPointF; fillMode: TIGPFillMode) : TIGPGraphics; overload;
    function  DrawPolygon( pen: IGPPen; brush: IGPBrush; const points: array of TPoint ) : TIGPGraphics; overload;
    function  DrawPolygon( pen: IGPPen; brush: IGPBrush; const points: array of TPoint; fillMode: TIGPFillMode) : TIGPGraphics; overload;
    
    // DrawPath
    function  DrawPath( pen: IGPPen; path: IGPGraphicsPath) : TIGPGraphics; overload;
    function  DrawPath( pen: IGPPen; brush: IGPBrush; path: IGPGraphicsPath) : TIGPGraphics; overload;

    // DrawCurve
    function  DrawCurveF( pen: IGPPen; const points: array of TPointF ) : TIGPGraphics; overload;
    function  DrawCurveF( pen: IGPPen; const points: array of TPointF; tension: Single) : TIGPGraphics; overload;
    function  DrawCurveF( pen: IGPPen; const points: array of TPointF; offset, numberOfSegments: Integer; tension: Single = 0.5) : TIGPGraphics; overload;
    function  DrawCurve( pen: IGPPen; const points: array of TPoint ) : TIGPGraphics; overload;
    function  DrawCurve( pen: IGPPen; const points: array of TPoint; tension: Single) : TIGPGraphics; overload;
    function  DrawCurve( pen: IGPPen; const points: array of TPoint; offset, numberOfSegments: Integer; tension: Single = 0.5) : TIGPGraphics; overload;

    // DrawClosedCurve
    function  DrawClosedCurveF( pen: IGPPen; const points: array of TPointF ) : TIGPGraphics; overload;
    function  DrawClosedCurveF( pen: IGPPen; const points: array of TPointF; tension: Single) : TIGPGraphics; overload;
    function  DrawClosedCurve( pen: IGPPen; const points: array of TPoint ) : TIGPGraphics; overload;
    function  DrawClosedCurve( pen: IGPPen; const points: array of TPoint; tension: Single) : TIGPGraphics; overload;

    function  DrawClosedCurveF( pen: IGPPen; brush: IGPBrush; const points: array of TPointF ) : TIGPGraphics; overload;
    function  DrawClosedCurveF( pen: IGPPen; brush: IGPBrush; const points: array of TPointF; fillMode: TIGPFillMode; tension: Single = 0.5) : TIGPGraphics; overload;
    function  DrawClosedCurve( pen: IGPPen; brush: IGPBrush; const points: array of TPoint ) : TIGPGraphics; overload;
    function  DrawClosedCurve( pen: IGPPen; brush: IGPBrush; const points: array of TPoint; fillMode: TIGPFillMode; tension: Single = 0.5) : TIGPGraphics; overload;

    // Clear
    function  Clear( color: TAlphaColor ) : TIGPGraphics;

    // FillRectangle(s)
    function  FillRectangleF( brush: IGPBrush; const rect: TIGPRectF) : TIGPGraphics; overload;
    function  FillRectangleF( brush: IGPBrush; x, y, width, height: Single) : TIGPGraphics; overload;
    function  FillRectanglesF( brush: IGPBrush; const rects: array of TIGPRectF ) : TIGPGraphics;
    function  FillRectangle( brush: IGPBrush; const rect: TIGPRect) : TIGPGraphics; overload;
    function  FillRectangle( brush: IGPBrush; x, y, width, height: Integer) : TIGPGraphics; overload;
    function  FillRectangles( brush: IGPBrush; const rects: array of TIGPRect ) : TIGPGraphics;

    // FillRoundRectangle
    function  FillRoundRectangleF( brush: IGPBrush; const rect: TIGPRectF; const ACornerSize : TIGPSizeF) : TIGPGraphics;
    function  FillRoundRectangle( brush: IGPBrush; const rect: TIGPRect; const ACornerSize : TIGPSize) : TIGPGraphics;

    // FillPolygon
    function  FillPolygonF( brush: IGPBrush; const points: array of TPointF ) : TIGPGraphics; overload;
    function  FillPolygonF( brush: IGPBrush; const points: array of TPointF; fillMode: TIGPFillMode) : TIGPGraphics; overload;
    function  FillPolygon( brush: IGPBrush; const points: array of TPoint ) : TIGPGraphics; overload;
    function  FillPolygon( brush: IGPBrush; const points: array of TPoint; fillMode: TIGPFillMode) : TIGPGraphics; overload;

    // FillEllipse
    function  FillEllipseF( brush: IGPBrush; const rect: TIGPRectF) : TIGPGraphics; overload;
    function  FillEllipseF( brush: IGPBrush; x, y, width, height: Single) : TIGPGraphics; overload;
    function  FillEllipsesF( brush: IGPBrush; const rects: array of TIGPRectF) : TIGPGraphics;
    function  FillEllipse( brush: IGPBrush; const rect: TIGPRect) : TIGPGraphics; overload;
    function  FillEllipse( brush: IGPBrush; x, y, width, height: Integer) : TIGPGraphics; overload;
    function  FillEllipses( brush: IGPBrush; const rects: array of TIGPRect) : TIGPGraphics;

    // FillPie
    function  FillPieF( brush: IGPBrush; const rect: TIGPRectF; startAngle, sweepAngle: Single) : TIGPGraphics; overload;
    function  FillPieF( brush: IGPBrush; x, y, width, height, startAngle, sweepAngle: Single) : TIGPGraphics; overload;
    function  FillPie( brush: IGPBrush; const rect: TIGPRect; startAngle, sweepAngle: Single) : TIGPGraphics; overload;
    function  FillPie( brush: IGPBrush; x, y, width, height: Integer; startAngle, sweepAngle: Single) : TIGPGraphics; overload;

    // FillPath
    function  FillPath( brush: IGPBrush; path: IGPGraphicsPath ) : TIGPGraphics;

    // FillClosedCurve
    function  FillClosedCurveF( brush: IGPBrush; const points: array of TPointF) : TIGPGraphics; overload;
    function  FillClosedCurveF( brush: IGPBrush; const points: array of TPointF;
      fillMode: TIGPFillMode; tension: Single = 0.5 ) : TIGPGraphics; overload;
    function  FillClosedCurve( brush: IGPBrush; const points: array of TPoint) : TIGPGraphics; overload;
    function  FillClosedCurve( brush: IGPBrush; const points: array of TPoint;
      fillMode: TIGPFillMode; tension: Single = 0.5) : TIGPGraphics; overload;

    // FillRegion
    function  FillRegion( brush: IGPBrush; region: IGPRegion ) : TIGPGraphics;

    // DrawString
    function  DrawStringF(string_: WideString; font: IGPFont;
      const layoutRect: TIGPRectF; stringFormat: IGPStringFormat; brush: IGPBrush) : TIGPGraphics; overload;
    function  DrawStringF(string_: WideString; font: IGPFont;
      const layoutRect: TIGPRectF; brush: IGPBrush) : TIGPGraphics; overload;
    function  DrawStringF(string_: WideString; font: IGPFont;
      const origin: TPointF; brush: IGPBrush) : TIGPGraphics; overload;
    function  DrawStringF(string_: WideString; font: IGPFont;
      const origin: TPointF; stringFormat: IGPStringFormat; brush: IGPBrush) : TIGPGraphics; overload;

    function  DrawString(string_: WideString; font: IGPFont;
      const layoutRect: TIGPRect; stringFormat: IGPStringFormat; brush: IGPBrush) : TIGPGraphics; overload;
    function  DrawString(string_: WideString; font: IGPFont;
      const layoutRect: TIGPRect; brush: IGPBrush) : TIGPGraphics; overload;
    function  DrawString(string_: WideString; font: IGPFont;
      const origin: TPoint; brush: IGPBrush) : TIGPGraphics; overload;
    function  DrawString(string_: WideString; font: IGPFont;
      const origin: TPoint; stringFormat: IGPStringFormat; brush: IGPBrush) : TIGPGraphics; overload;
      
    // MeasureString
    function  GetStringSizeF(string_: WideString; font: IGPFont;
      stringFormat: IGPStringFormat = NIL ) : TIGPSizeF; overload;

    function  GetStringSizeF(string_: WideString; font: IGPFont;
      const layoutRectSize: TIGPSizeF; stringFormat: IGPStringFormat = NIL;
      codepointsFitted: PInteger = NIL; linesFilled: PInteger = NIL) : TIGPSizeF; overload;

    function  GetStringBoundingBoxF(string_: WideString; font: IGPFont;
      const layoutRect: TIGPRectF; stringFormat: IGPStringFormat;
      codepointsFitted: PInteger = NIL; linesFilled: PInteger = NIL) : TIGPRectF; overload;

    function  GetStringBoundingBoxF(string_: WideString; font: IGPFont;
      const origin: TPointF; stringFormat: IGPStringFormat ) : TIGPRectF; overload;

    function  GetStringBoundingBoxF(string_: WideString; font: IGPFont;
      const layoutRect: TIGPRectF ) : TIGPRectF; overload;

    function  GetStringBoundingBoxF(string_: WideString; font: IGPFont;
      const origin: TPointF ) : TIGPRectF; overload;

    function  MeasureStringF(string_: WideString; font: IGPFont;
      stringFormat: IGPStringFormat = NIL ) : TIGPSizeF; overload;

    function  MeasureStringF(string_: WideString; font: IGPFont;
      const layoutRectSize: TIGPSizeF; stringFormat: IGPStringFormat = NIL;
      codepointsFitted: PInteger = NIL; linesFilled: PInteger = NIL) : TIGPSizeF; overload;

    function  MeasureStringF(string_: WideString; font: IGPFont;
      const layoutRect: TIGPRectF; stringFormat: IGPStringFormat;
      codepointsFitted: PInteger = NIL; linesFilled: PInteger = NIL) : TIGPRectF; overload;

    function  MeasureStringF(string_: WideString; font: IGPFont;
      const origin: TPointF; stringFormat: IGPStringFormat ) : TIGPRectF; overload;

    function  MeasureStringF(string_: WideString; font: IGPFont;
      const layoutRect: TIGPRectF ) : TIGPRectF; overload;

    function  MeasureStringF(string_: WideString; font: IGPFont;
      const origin: TPointF ) : TIGPRectF; overload;
      
    // MeasureCharacterRangesF
    function  MeasureCharacterRangesF(string_: WideString; font: IGPFont;
      const layoutRect: TIGPRectF; stringFormat: IGPStringFormat = NIL ) : TArray<IGPRegion>; overload;

    function  MeasureCharacterRangesF(string_: WideString; font: IGPFont;
      const origin: TPointF; stringFormat: IGPStringFormat = NIL ) : TArray<IGPRegion>; overload;

    function  MeasureCharacterRangesF(string_: WideString; font: IGPFont;
      stringFormat: IGPStringFormat = NIL ) : TArray<IGPRegion>; overload;

    function  MeasureCharacterRangesF(string_: WideString; font: IGPFont;
      const layoutRect: TIGPRectF; const ranges : array of TIGPCharacterRange; stringFormat: IGPStringFormat = NIL ) : TArray<IGPRegion>; overload;

    function  MeasureCharacterRangesF(string_: WideString; font: IGPFont;
      const origin: TPointF; const ranges : array of TIGPCharacterRange; stringFormat: IGPStringFormat = NIL ) : TArray<IGPRegion>; overload;

    function  MeasureCharacterRangesF(string_: WideString; font: IGPFont;
      const ranges : array of TIGPCharacterRange; stringFormat: IGPStringFormat = NIL ) : TArray<IGPRegion>; overload;

    // DrawDriverString
    function  DrawDriverString(text: PUINT16; length: Integer; font: IGPFont;
      brush: IGPBrush; positions: PGPPointF; flags: Integer; matrix: IGPMatrix) : TIGPGraphics;

    // MeasureDriverString
    function  GetDriverStringBoundingBoxF(text: PUINT16; length: Integer; font: IGPFont;
       positions: PGPPointF; flags: Integer; matrix: IGPMatrix ) : TIGPRectF;

    // Draw a cached bitmap on this graphics destination offset by
    // x, y. Note this will fail with WrongState if the CachedBitmap
    // native format differs from this Graphics.
    function  DrawCachedBitmap(cb: IGPCachedBitmap;  x, y: Integer) : TIGPGraphics;

    function  DrawImageF(image: IGPImage; const point: TPointF) : TIGPGraphics; overload;
    function  DrawImageF(image: IGPImage; x, y: Single) : TIGPGraphics; overload;
    function  DrawImageF(image: IGPImage; const rect: TIGPRectF) : TIGPGraphics; overload;
    function  DrawImageF(image: IGPImage; x, y, width, height: Single) : TIGPGraphics; overload;
    function  DrawImage(image: IGPImage; const point: TPoint) : TIGPGraphics; overload;
    function  DrawImage(image: IGPImage; x, y: Integer) : TIGPGraphics; overload;
    function  DrawImage(image: IGPImage; const rect: TIGPRect) : TIGPGraphics; overload;
    function  DrawImage(image: IGPImage; x, y, width, height: Integer) : TIGPGraphics; overload;

    function  DrawImageF(image: IGPImage; const point: TPointF; Opacity : Single ) : TIGPGraphics; overload;
    function  DrawImageF(image: IGPImage; x, y: Single; Opacity : Single ) : TIGPGraphics; overload;
    function  DrawImageF(image: IGPImage; const rect: TIGPRectF; Opacity : Single ) : TIGPGraphics; overload;
    function  DrawImageF(image: IGPImage; x, y, width, height: Single; Opacity : Single ) : TIGPGraphics; overload;
    function  DrawImage(image: IGPImage; const point: TPoint; Opacity : Single ) : TIGPGraphics; overload;
    function  DrawImage(image: IGPImage; x, y: Integer; Opacity : Single ) : TIGPGraphics; overload;
    function  DrawImage(image: IGPImage; const rect: TIGPRect; Opacity : Single ) : TIGPGraphics; overload;
    function  DrawImage(image: IGPImage; x, y, width, height: Integer; Opacity : Single ) : TIGPGraphics; overload;

    // Affine Draw Image
    // destPoints.length = 3: rect => parallelogram
    //     destPoints[0] <=> top-left corner of the source rectangle
    //     destPoints[1] <=> top-right corner
    //     destPoints[2] <=> bottom-left corner
    // destPoints.length = 4: rect => quad
    //     destPoints[3] <=> bottom-right corner
    function  DrawImageF(image: IGPImage; const destPoints: array of TPointF ) : TIGPGraphics; overload;
    function  DrawImage(image: IGPImage; const destPoints: array of TPoint ) : TIGPGraphics; overload;
    function  DrawImageF(image: IGPImage; x, y, srcx, srcy, srcwidth, srcheight: Single; srcUnit: TIGPUnit) : TIGPGraphics; overload;
    function  DrawImageF(image: IGPImage; const destRect: TIGPRectF; srcx, srcy,
      srcwidth, srcheight: Single; srcUnit: TIGPUnit;
      imageAttributes: IGPImageAttributes = NIL; callback: TIGPDrawImageAbortProc = NIL) : TIGPGraphics; overload;
    function  DrawImageF(image: IGPImage; const destPoints: array of TPointF;
      srcx, srcy, srcwidth, srcheight: Single; srcUnit: TIGPUnit;
      imageAttributes: IGPImageAttributes = NIL; callback: TIGPDrawImageAbortProc = NIL) : TIGPGraphics; overload;
    function  DrawImage(image: IGPImage; x, y, srcx, srcy, srcwidth,
      srcheight: Integer; srcUnit: TIGPUnit) : TIGPGraphics; overload;
    function  DrawImage(image: IGPImage; const destRect: TIGPRect; srcx, srcy,
      srcwidth, srcheight: Integer; srcUnit: TIGPUnit;
      imageAttributes: IGPImageAttributes = NIL; callback: TIGPDrawImageAbortProc = NIL) : TIGPGraphics; overload;
    function  DrawImage(image: IGPImage; const destPoints: array of TPoint;
      srcx, srcy, srcwidth, srcheight: Integer; srcUnit: TIGPUnit;
      imageAttributes: IGPImageAttributes = NIL; callback: TIGPDrawImageAbortProc = NIL) : TIGPGraphics; overload;

    // The following methods are for playing an EMF+ to a graphics
    // via the enumeration interface.  Each record of the EMF+ is
    // sent to the callback (along with the callbackData).  Then
    // the callback can invoke the Metafile::PlayRecord method
    // to play the particular record.
    function  EnumerateMetafileF(metafile: IGPMetafile; const destPoint: TPointF;
      callback: TIGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TIGPGraphics; overload;
    function  EnumerateMetafile(metafile: IGPMetafile; const destPoint: TPoint;
       callback: TIGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TIGPGraphics; overload;
    function  EnumerateMetafileF(metafile: IGPMetafile; const destRect: TIGPRectF;
       callback: TIGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TIGPGraphics; overload;
    function  EnumerateMetafile(metafile: IGPMetafile; const destRect: TIGPRect;
       callback: TIGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TIGPGraphics; overload;
    function  EnumerateMetafileF(metafile: IGPMetafile; const destPoints: array of TPointF;
       callback: TIGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TIGPGraphics; overload;
    function  EnumerateMetafile(metafile: IGPMetafile; const destPoints: array of TPoint;
       callback: TIGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TIGPGraphics; overload;
    function  EnumerateMetafileF(metafile: IGPMetafile; const destPoint: TPointF;
       const srcRect: TIGPRectF; srcUnit: TIGPUnit; callback: TIGPEnumerateMetafileProc;
       imageAttributes: IGPImageAttributes = NIL
       ) : TIGPGraphics; overload;
    function  EnumerateMetafile(metafile : IGPMetafile; const destPoint : TPoint;
       const srcRect : TIGPRect; srcUnit : TIGPUnit; callback : TIGPEnumerateMetafileProc;
       imageAttributes : IGPImageAttributes = NIL
       ) : TIGPGraphics; overload;
    function  EnumerateMetafileF(metafile: IGPMetafile; const destRect: TIGPRectF;
       const srcRect: TIGPRectF; srcUnit: TIGPUnit; callback: TIGPEnumerateMetafileProc;
       imageAttributes: IGPImageAttributes = NIL) : TIGPGraphics; overload;
    function  EnumerateMetafile(metafile : IGPMetafile; const destRect, srcRect: TIGPRect;
       srcUnit : TIGPUnit; callback : TIGPEnumerateMetafileProc; imageAttributes : IGPImageAttributes = NIL) : TIGPGraphics; overload;
    function  EnumerateMetafileF( metafile: IGPMetafile; const destPoints: array of TPointF;
        const srcRect: TIGPRectF; srcUnit: TIGPUnit; callback: TIGPEnumerateMetafileProc;
        imageAttributes: IGPImageAttributes = NIL) : TIGPGraphics; overload;
    function  EnumerateMetafile(metafile: IGPMetafile; const destPoints: array of TPoint;
        const srcRect: TIGPRect; srcUnit: TIGPUnit; callback: TIGPEnumerateMetafileProc;
        imageAttributes: IGPImageAttributes = NIL) : TIGPGraphics; overload;

    // SetClip
    function  SetClip(g: IGPGraphics; combineMode: TIGPCombineMode = CombineModeReplace) : TIGPGraphics; overload;
    function  SetClipF( const rect: TIGPRectF; combineMode: TIGPCombineMode = CombineModeReplace) : TIGPGraphics;
    function  SetClip( const rect: TIGPRect; combineMode: TIGPCombineMode = CombineModeReplace) : TIGPGraphics; overload;
    function  SetClip(path: IGPGraphicsPath; combineMode: TIGPCombineMode = CombineModeReplace) : TIGPGraphics; overload;
    function  SetClip(region: IGPRegion; combineMode: TIGPCombineMode = CombineModeReplace) : TIGPGraphics; overload;
    // This is different than the other SetClip methods because it assumes
    // that the HRGN is already in device units, so it doesn't transform
    // the coordinates in the HRGN.
    function  SetClip(hRgn: HRGN; combineMode: TIGPCombineMode = CombineModeReplace) : TIGPGraphics; overload;

    procedure SetClipProp( region: IGPRegion );

    // IntersectClip
    function  IntersectClipF(const rect: TIGPRectF) : TIGPGraphics;
    function  IntersectClip(const rect: TIGPRect) : TIGPGraphics; overload;
    function  IntersectClip(region: IGPRegion) : TIGPGraphics; overload;
    
    // ExcludeClip
    function  ExcludeClipF(const rect: TIGPRectF) : TIGPGraphics;
    function  ExcludeClip(const rect: TIGPRect) : TIGPGraphics; overload;
    function  ExcludeClip(region: IGPRegion) : TIGPGraphics; overload;

    function  ResetClip() : TIGPGraphics;

    function  TranslateClipF(dx, dy: Single) : TIGPGraphics;
    function  TranslateClip(dx, dy: Integer) : TIGPGraphics;

    function  GetClip() : IGPRegion;

    function  GetClipBoundsF() : TIGPRectF;
    function  GetClipBounds() : TIGPRect;

    function  IsClipEmpty() : Boolean;

    function  GetVisibleClipBoundsF() : TIGPRectF;
    function  GetVisibleClipBounds() : TIGPRect;

    function  IsVisibleClipEmpty: Boolean;

    function  IsVisible(x, y: Integer) : Boolean; overload;
    function  IsVisible(const point: TPoint) : Boolean; overload;
    function  IsVisible(x, y, width, height: Integer) : Boolean; overload;
    function  IsVisible(const rect: TIGPRect) : Boolean; overload;
    function  IsVisibleF(x, y: Single) : Boolean; overload;
    function  IsVisibleF(const point: TPointF) : Boolean; overload;
    function  IsVisibleF(x, y, width, height: Single) : Boolean; overload;
    function  IsVisibleF(const rect: TIGPRectF) : Boolean; overload;

    function  Save() : TIGPGraphicsState;
    function  Restore(gstate: TIGPGraphicsState) : TIGPGraphics;

    function  BeginContainerF(const dstrect,srcrect: TIGPRectF; unit_: TIGPUnit) : TIGPGraphicsContainer; overload;
    function  BeginContainer(const dstrect, srcrect: TIGPRect; unit_: TIGPUnit) : TIGPGraphicsContainer; overload;
    function  BeginContainer() : TIGPGraphicsContainer; overload;
    function  EndContainer(state: TIGPGraphicsContainer) : TIGPGraphics;

    // Only valid when recording metafiles.
    function  AddMetafileComment( const data: array of BYTE ) : TIGPGraphics;

    property RenderingOrigin    : TPoint read GetRenderingOrigin write SetRenderingOriginProp;
    property CompositingMode    : TIGPCompositingMode read GetCompositingMode write SetCompositingModeProp;
    property CompositingQuality : TIGPCompositingQuality read GetCompositingQuality write SetCompositingQualityProp;
    property TextRenderingHint  : TIGPTextRenderingHint  read GetTextRenderingHint  write SetTextRenderingHintProp;
    property TextContrast       : Cardinal read GetTextContrast write SetTextContrastProp;
    property InterpolationMode  : TIGPInterpolationMode read GetInterpolationMode write SetInterpolationModeProp;
    property SmoothingMode      : TIGPSmoothingMode  read GetSmoothingMode write SetSmoothingModeProp;
    property PixelOffsetMode    : TIGPPixelOffsetMode  read GetPixelOffsetMode write SetPixelOffsetModeProp;

    property Transform          : IGPMatrix read GetTransform       write SetTransformProp;
    property Clip               : IGPRegion read GetClip            write SetClipProp;

    property PageUnit           : TIGPUnit read GetPageUnit  write SetPageUnitProp;
    property PageScale          : Single  read GetPageScale write SetPageScaleProp;

    property DpiX               : Single  read GetDpiX;
    property DpiY               : Single  read GetDpiY;

{$ENDIF}
  end;
  
{$IFDEF MSWINDOWS}
  TIGPGraphics = class( TIGPBase, IGPGraphics, IGPTransformable )
  protected
    FNativeGraphics: GpGraphics;

  protected
    procedure SetNativeGraphics(graphics: GpGraphics);
    function  GetNativeGraphics() : GpGraphics;
    function  GetNativePen( pen: TIGPPen) : GpPen;

  protected
    constructor CreateGdiPlus(graphics: GpGraphics; Dummy1 : Boolean; Dummy2 : Boolean );

  public
    constructor Create( ahdc: HDC ); overload;
    constructor Create( ahdc: HDC; hdevice: THandle ); overload;
    constructor Create( hwnd: HWND; icm : Boolean{ = False} ); overload;
    constructor Create( image: IGPImage ); overload;
    destructor  Destroy(); override;

  public
    class function FromHDC( ahdc: HDC ) : TIGPGraphics; overload;
    class function FromHDC( ahdc: HDC; hdevice: THandle ) : TIGPGraphics; overload;
    class function FromHWND( hwnd: HWND; icm: Boolean = False ) : TIGPGraphics;
    class function FromImage( image: IGPImage ) : TIGPGraphics;

  public
    function  Flush(intention: TIGPFlushIntention = FlushIntentionFlush) : TIGPGraphics;
    //------------------------------------------------------------------------
    // GDI Interop methods
    //------------------------------------------------------------------------
    // Locks the graphics until ReleaseDC is called
    function  GetHDC() : HDC;
    function  ReleaseHDC(ahdc: HDC) : TIGPGraphics;
    //------------------------------------------------------------------------
    // Rendering modes
    //------------------------------------------------------------------------
    function  SetRenderingOrigin( const point : TPoint ) : TIGPGraphics;
    procedure SetRenderingOriginProp( const point : TPoint );
    function  GetRenderingOrigin() : TPoint;
    function  SetCompositingMode(compositingMode: TIGPCompositingMode) : TIGPGraphics;
    procedure SetCompositingModeProp(compositingMode: TIGPCompositingMode);
    function  GetCompositingMode() : TIGPCompositingMode;
    function  SetCompositingQuality(compositingQuality: TIGPCompositingQuality) : TIGPGraphics;
    procedure SetCompositingQualityProp(compositingQuality: TIGPCompositingQuality);
    function  GetCompositingQuality() : TIGPCompositingQuality;
    function  SetTextRenderingHint(newMode: TIGPTextRenderingHint) : TIGPGraphics;
    procedure SetTextRenderingHintProp(newMode: TIGPTextRenderingHint);
    function  GetTextRenderingHint() : TIGPTextRenderingHint;
    function  SetTextContrast(contrast: Cardinal) : TIGPGraphics; // 0..12
    procedure SetTextContrastProp(contrast: Cardinal); // 0..12
    function  GetTextContrast() : Cardinal;
    function  GetInterpolationMode() : TIGPInterpolationMode;
    function  SetInterpolationMode(interpolationMode: TIGPInterpolationMode) : TIGPGraphics;
    procedure SetInterpolationModeProp(interpolationMode: TIGPInterpolationMode);
    function  GetSmoothingMode() : TIGPSmoothingMode;
    function  SetSmoothingMode(smoothingMode: TIGPSmoothingMode) : TIGPGraphics;
    procedure SetSmoothingModeProp(smoothingMode: TIGPSmoothingMode);
    function  GetPixelOffsetMode() : TIGPPixelOffsetMode;
    function  SetPixelOffsetMode(pixelOffsetMode: TIGPPixelOffsetMode) : TIGPGraphics;
    procedure SetPixelOffsetModeProp(pixelOffsetMode: TIGPPixelOffsetMode);
    //------------------------------------------------------------------------
    // Manipulate current world transform
    //------------------------------------------------------------------------
    function  SetTransform(matrix: IGPMatrix) : TIGPGraphics;
    procedure SetTransformProp(matrix: IGPMatrix);
    function  ResetTransform() : TIGPGraphics;
    function  MultiplyTransform(matrix: IGPMatrix; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPGraphics;
    function  TranslateTransform(dx, dy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPGraphics;
    function  ScaleTransform(sx, sy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPGraphics; overload;
    function  ScaleTransform(s: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPGraphics; overload;
    function  RotateTransform(angle: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPGraphics;
    function  GetTransform() : IGPMatrix;
    function  SetPageUnit( unit_: TIGPUnit ) : TIGPGraphics;
    procedure SetPageUnitProp( unit_: TIGPUnit );
    function  SetPageScale( scale: Single ) : TIGPGraphics;
    procedure SetPageScaleProp( scale: Single );
    function  GetPageUnit() : TIGPUnit;
    function  GetPageScale() : Single;
    function  GetDpiX() : Single;
    function  GetDpiY() : Single;
    function  TransformPoints(destSpace: TIGPCoordinateSpace; srcSpace: TIGPCoordinateSpace;
      var pts : array of TPointF ) : TIGPGraphics; overload;
    function  TransformPoints(destSpace: TIGPCoordinateSpace; srcSpace: TIGPCoordinateSpace;
      var pts : array of TPoint ) : TIGPGraphics; overload;
    //------------------------------------------------------------------------
    // GetNearestColor (for <= 8bpp surfaces).  Note: Alpha is ignored.
    //------------------------------------------------------------------------
    function  GetNearestColor( AColor : TAlphaColor ) : TAlphaColor;

    // DrawLine(s)
    function  DrawLineF( pen: IGPPen; x1, y1, x2, y2: Single ) : TIGPGraphics; overload;
    function  DrawLineF( pen: IGPPen; const pt1, pt2: TPointF ) : TIGPGraphics; overload;
    function  DrawLinesF( pen: IGPPen; const points : array of TPointF ) : TIGPGraphics;
    function  DrawLine( pen: IGPPen; x1, y1, x2, y2: Integer ) : TIGPGraphics; overload;
    function  DrawLine( pen: IGPPen; const pt1, pt2: TPoint ) : TIGPGraphics; overload;
    function  DrawLines( pen: IGPPen; const points : array of TPoint ) : TIGPGraphics;

    // DrawArc
    function  DrawArcF( pen: IGPPen; x, y, width, height, startAngle, sweepAngle: Single) : TIGPGraphics; overload;
    function  DrawArcF( pen: IGPPen; const rect: TIGPRectF; startAngle, sweepAngle: Single) : TIGPGraphics; overload;
    function  DrawArc( pen: IGPPen; x, y, width, height: Integer; startAngle, sweepAngle: Single) : TIGPGraphics; overload;
    function  DrawArc( pen: IGPPen; const rect: TIGPRect; startAngle, sweepAngle: Single) : TIGPGraphics; overload;

    // DrawBezier(s)
    function  DrawBezierF( pen: IGPPen; x1, y1, x2, y2, x3, y3, x4, y4: Single) : TIGPGraphics; overload;
    function  DrawBezierF( pen: IGPPen; const pt1, pt2, pt3, pt4: TPointF) : TIGPGraphics; overload;
    function  DrawBeziersF( pen: IGPPen; const points : array of TPointF ) : TIGPGraphics;
    function  DrawBezier( pen: IGPPen; x1, y1, x2, y2, x3, y3, x4, y4: Integer) : TIGPGraphics; overload;
    function  DrawBezier( pen: IGPPen; const pt1, pt2, pt3, pt4: TPoint) : TIGPGraphics; overload;
    function  DrawBeziers( pen: IGPPen; const points : array of TPoint ) : TIGPGraphics;

    // DrawRectangle(s)
    function  DrawRectangleF( pen: IGPPen; const rect: TIGPRectF ) : TIGPGraphics; overload;
    function  DrawRectangleF( pen: IGPPen; x, y, width, height: Single ) : TIGPGraphics; overload;
    function  DrawRectanglesF( pen: IGPPen; const rects: array of TIGPRectF ) : TIGPGraphics; overload;
    function  DrawRectangle( pen: IGPPen; const rect: TIGPRect ) : TIGPGraphics; overload;
    function  DrawRectangle( pen: IGPPen; x, y, width, height: Integer ) : TIGPGraphics; overload;
    function  DrawRectangles( pen: IGPPen; const rects: array of TIGPRect ) : TIGPGraphics; overload;

    function  DrawRectangleF( pen: IGPPen; brush: IGPBrush; const rect: TIGPRectF) : TIGPGraphics; overload;
    function  DrawRectangleF( pen: IGPPen; brush: IGPBrush; x, y, width, height: Single) : TIGPGraphics; overload;
    function  DrawRectanglesF( pen: IGPPen; brush: IGPBrush; const rects: array of TIGPRectF ) : TIGPGraphics; overload;
    function  DrawRectangle( pen: IGPPen; brush: IGPBrush; const rect: TIGPRect) : TIGPGraphics; overload;
    function  DrawRectangle( pen: IGPPen; brush: IGPBrush; x, y, width, height: Integer) : TIGPGraphics; overload;
    function  DrawRectangles( pen: IGPPen; brush: IGPBrush; const rects: array of TIGPRect ) : TIGPGraphics; overload;

    // DrawRoundRectangle
    function  DrawRoundRectangleF( pen: IGPPen; const rect: TIGPRectF; const ACornerSize : TIGPSizeF ) : TIGPGraphics; overload;
    function  DrawRoundRectangle( pen: IGPPen; const rect: TIGPRect; const ACornerSize : TIGPSize ) : TIGPGraphics; overload;

    function  DrawRoundRectangleF( pen: IGPPen; brush: IGPBrush; const rect: TIGPRectF; const ACornerSize : TIGPSizeF ) : TIGPGraphics; overload;
    function  DrawRoundRectangle( pen: IGPPen; brush: IGPBrush; const rect: TIGPRect; const ACornerSize : TIGPSize ) : TIGPGraphics; overload;

    // DrawEllipse
    function  DrawEllipseF( pen: IGPPen; const rect: TIGPRectF ) : TIGPGraphics; overload;
    function  DrawEllipseF( pen: IGPPen; x, y, width, height: Single ) : TIGPGraphics; overload;
    function  DrawEllipsesF( pen: IGPPen; const rects: array of TIGPRectF ) : TIGPGraphics; overload;
    function  DrawEllipse( pen: IGPPen; const rect: TIGPRect ) : TIGPGraphics; overload;
    function  DrawEllipse( pen: IGPPen; x, y, width, height: Integer ) : TIGPGraphics; overload;
    function  DrawEllipses( pen: IGPPen; const rects: array of TIGPRect ) : TIGPGraphics; overload;

    function  DrawEllipseF( pen: IGPPen; brush: IGPBrush; const rect: TIGPRectF ) : TIGPGraphics; overload;
    function  DrawEllipseF( pen: IGPPen; brush: IGPBrush; x, y, width, height: Single ) : TIGPGraphics; overload;
    function  DrawEllipsesF( pen: IGPPen; brush: IGPBrush; const rects: array of TIGPRectF ) : TIGPGraphics; overload;
    function  DrawEllipse( pen: IGPPen; brush: IGPBrush; const rect: TIGPRect ) : TIGPGraphics; overload;
    function  DrawEllipse( pen: IGPPen; brush: IGPBrush; x, y, width, height: Integer ) : TIGPGraphics; overload;
    function  DrawEllipses( pen: IGPPen; brush: IGPBrush; const rects: array of TIGPRect ) : TIGPGraphics; overload;

    // DrawPie
    function  DrawPieF( pen: IGPPen; const rect: TIGPRectF; startAngle, sweepAngle: Single ) : TIGPGraphics; overload;
    function  DrawPieF( pen: IGPPen; x, y, width, height, startAngle, sweepAngle: Single ) : TIGPGraphics; overload;
    function  DrawPie( pen: IGPPen; const rect: TIGPRect; startAngle, sweepAngle: Single ) : TIGPGraphics; overload;
    function  DrawPie( pen: IGPPen; x, y, width, height: Integer; startAngle, sweepAngle: Single ) : TIGPGraphics; overload;

    function  DrawPieF( pen: IGPPen; brush: IGPBrush; const rect: TIGPRectF; startAngle, sweepAngle: Single ) : TIGPGraphics; overload;
    function  DrawPieF( pen: IGPPen; brush: IGPBrush; x, y, width, height, startAngle, sweepAngle: Single ) : TIGPGraphics; overload;
    function  DrawPie( pen: IGPPen; brush: IGPBrush; const rect: TIGPRect; startAngle, sweepAngle: Single ) : TIGPGraphics; overload;
    function  DrawPie( pen: IGPPen; brush: IGPBrush; x, y, width, height: Integer; startAngle, sweepAngle: Single ) : TIGPGraphics; overload;

    // DrawPolygon
    function  DrawPolygonF( pen: IGPPen; const points: array of TPointF ) : TIGPGraphics; overload;
    function  DrawPolygon( pen: IGPPen; const points: array of TPoint ) : TIGPGraphics; overload;

    function  DrawPolygonF( pen: IGPPen; brush: IGPBrush; const points: array of TPointF ) : TIGPGraphics; overload;
    function  DrawPolygonF( pen: IGPPen; brush: IGPBrush; const points: array of TPointF; fillMode: TIGPFillMode ) : TIGPGraphics; overload;
    function  DrawPolygon( pen: IGPPen; brush: IGPBrush; const points: array of TPoint ) : TIGPGraphics; overload;
    function  DrawPolygon( pen: IGPPen; brush: IGPBrush; const points: array of TPoint; fillMode: TIGPFillMode ) : TIGPGraphics; overload;

    // DrawPath
    function  DrawPath( pen: IGPPen; path: IGPGraphicsPath ) : TIGPGraphics; overload;
    function  DrawPath( pen: IGPPen; brush: IGPBrush; path: IGPGraphicsPath ) : TIGPGraphics; overload;

    // DrawCurve
    function  DrawCurveF( pen: IGPPen; const points: array of TPointF ) : TIGPGraphics; overload;
    function  DrawCurveF( pen: IGPPen; const points: array of TPointF; tension: Single) : TIGPGraphics; overload;
    function  DrawCurveF( pen: IGPPen; const points: array of TPointF; offset,
      numberOfSegments: Integer; tension: Single = 0.5) : TIGPGraphics; overload;
    function  DrawCurve( pen: IGPPen; const points: array of TPoint ) : TIGPGraphics; overload;
    function  DrawCurve( pen: IGPPen; const points: array of TPoint; tension: Single) : TIGPGraphics; overload;
    function  DrawCurve( pen: IGPPen; const points: array of TPoint; offset, numberOfSegments: Integer;
      tension: Single = 0.5) : TIGPGraphics; overload;

    // DrawClosedCurve
    function  DrawClosedCurveF( pen: IGPPen; const points: array of TPointF ) : TIGPGraphics; overload;
    function  DrawClosedCurveF( pen: IGPPen; const points: array of TPointF; tension: Single ) : TIGPGraphics; overload;
    function  DrawClosedCurve( pen: IGPPen; const points: array of TPoint ) : TIGPGraphics; overload;
    function  DrawClosedCurve( pen: IGPPen; const points: array of TPoint; tension: Single ) : TIGPGraphics; overload;

    function  DrawClosedCurveF( pen: IGPPen; brush: IGPBrush; const points: array of TPointF ) : TIGPGraphics; overload;
    function  DrawClosedCurveF( pen: IGPPen; brush: IGPBrush; const points: array of TPointF; fillMode: TIGPFillMode; tension: Single = 0.5 ) : TIGPGraphics; overload;
    function  DrawClosedCurve( pen: IGPPen; brush: IGPBrush; const points: array of TPoint ) : TIGPGraphics; overload;
    function  DrawClosedCurve( pen: IGPPen; brush: IGPBrush; const points: array of TPoint; fillMode: TIGPFillMode; tension: Single = 0.5 ) : TIGPGraphics; overload;
    
    // Clear
    function  Clear( color: TAlphaColor ) : TIGPGraphics;

    // FillRectangle(s)
    function  FillRectangleF( brush: IGPBrush; const rect: TIGPRectF ) : TIGPGraphics; overload;
    function  FillRectangleF( brush: IGPBrush; x, y, width, height: Single ) : TIGPGraphics; overload;
    function  FillRectanglesF( brush: IGPBrush; const rects: array of TIGPRectF ) : TIGPGraphics;
    function  FillRectangle( brush: IGPBrush; const rect: TIGPRect ) : TIGPGraphics; overload;
    function  FillRectangle( brush: IGPBrush; x, y, width, height: Integer ) : TIGPGraphics; overload;
    function  FillRectangles( brush: IGPBrush; const rects: array of TIGPRect ) : TIGPGraphics;

    // FillRoundRectangle
    function  FillRoundRectangleF( brush: IGPBrush; const rect: TIGPRectF; const ACornerSize : TIGPSizeF ) : TIGPGraphics;
    function  FillRoundRectangle( brush: IGPBrush; const rect: TIGPRect; const ACornerSize : TIGPSize ) : TIGPGraphics;

    // FillPolygon
    function  FillPolygonF( brush: IGPBrush; const points: array of TPointF ) : TIGPGraphics; overload;
    function  FillPolygonF( brush: IGPBrush; const points: array of TPointF; fillMode: TIGPFillMode) : TIGPGraphics; overload;
    function  FillPolygon( brush: IGPBrush; const points: array of TPoint ) : TIGPGraphics; overload;
    function  FillPolygon( brush: IGPBrush; const points: array of TPoint; fillMode: TIGPFillMode) : TIGPGraphics; overload;

    // FillEllipse
    function  FillEllipseF( brush: IGPBrush; const rect: TIGPRectF ) : TIGPGraphics; overload;
    function  FillEllipseF( brush: IGPBrush; x, y, width, height: Single ) : TIGPGraphics; overload;
    function  FillEllipsesF( brush: IGPBrush; const rects: array of TIGPRectF ) : TIGPGraphics;
    function  FillEllipse( brush: IGPBrush; const rect: TIGPRect ) : TIGPGraphics; overload;
    function  FillEllipse( brush: IGPBrush; x, y, width, height: Integer ) : TIGPGraphics; overload;
    function  FillEllipses( brush: IGPBrush; const rects: array of TIGPRect ) : TIGPGraphics;

    // FillPie
    function  FillPieF( brush: IGPBrush; const rect: TIGPRectF; startAngle, sweepAngle: Single ) : TIGPGraphics; overload;
    function  FillPieF( brush: IGPBrush; x, y, width, height, startAngle, sweepAngle: Single ) : TIGPGraphics; overload;
    function  FillPie( brush: IGPBrush; const rect: TIGPRect; startAngle, sweepAngle: Single ) : TIGPGraphics; overload;
    function  FillPie( brush: IGPBrush; x, y, width, height: Integer; startAngle, sweepAngle: Single ) : TIGPGraphics; overload;

    // FillPath
    function  FillPath( brush: IGPBrush; path: IGPGraphicsPath) : TIGPGraphics;

    // FillClosedCurve
    function  FillClosedCurveF( brush: IGPBrush; const points: array of TPointF ) : TIGPGraphics; overload;
    function  FillClosedCurveF( brush: IGPBrush; const points: array of TPointF;
      fillMode: TIGPFillMode; tension: Single = 0.5 ) : TIGPGraphics; overload;
    function  FillClosedCurve( brush: IGPBrush; const points: array of TPoint ) : TIGPGraphics; overload;
    function  FillClosedCurve( brush: IGPBrush; const points: array of TPoint;
      fillMode: TIGPFillMode; tension: Single = 0.5 ) : TIGPGraphics; overload;

    // FillRegion
    function  FillRegion( brush: IGPBrush; region: IGPRegion ) : TIGPGraphics;

    // DrawString
    function  DrawStringF(string_: WideString; font: IGPFont;
      const layoutRect: TIGPRectF; stringFormat: IGPStringFormat; brush: IGPBrush) : TIGPGraphics; overload;
    function  DrawStringF(string_: WideString; font: IGPFont;
      const layoutRect: TIGPRectF; brush: IGPBrush) : TIGPGraphics; overload;
    function  DrawStringF(string_: WideString; font: IGPFont;
      const origin: TPointF; brush: IGPBrush) : TIGPGraphics; overload;
    function  DrawStringF(string_: WideString; font: IGPFont;
      const origin: TPointF; stringFormat: IGPStringFormat; brush: IGPBrush) : TIGPGraphics; overload;

    function  DrawString(string_: WideString; font: IGPFont;
      const layoutRect: TIGPRect; stringFormat: IGPStringFormat; brush: IGPBrush) : TIGPGraphics; overload;
    function  DrawString(string_: WideString; font: IGPFont;
      const layoutRect: TIGPRect; brush: IGPBrush) : TIGPGraphics; overload;
    function  DrawString(string_: WideString; font: IGPFont;
      const origin: TPoint; brush: IGPBrush) : TIGPGraphics; overload;
    function  DrawString(string_: WideString; font: IGPFont;
      const origin: TPoint; stringFormat: IGPStringFormat; brush: IGPBrush) : TIGPGraphics; overload;

{
    function FillString(string_: WideString; font: IGPFont;
      const layoutRect: TIGPRectF; stringFormat: IGPStringFormat; brush: IGPBrush) : TIGPGraphics; overload;
    function FillString(string_: WideString; font: IGPFont;
      const origin: TPointF; brush: IGPBrush) : TIGPGraphics; overload;
    function FillString(string_: WideString; font: IGPFont;
      const origin: TPointF; stringFormat: IGPStringFormat; brush: IGPBrush) : TIGPGraphics; overload;
}
    // MeasureString
    function  GetStringSizeF(string_: WideString; font: IGPFont;
      stringFormat: IGPStringFormat = NIL) : TIGPSizeF; overload;

    function  GetStringSizeF(string_: WideString; font: IGPFont;
      const layoutRectSize: TIGPSizeF; stringFormat: IGPStringFormat = NIL;
      codepointsFitted: PInteger = NIL; linesFilled: PInteger = NIL) : TIGPSizeF; overload;

    function  GetStringBoundingBoxF(
      string_: WideString;
      font: IGPFont;
      const layoutRect: TIGPRectF;
      stringFormat: IGPStringFormat;
      codepointsFitted: PInteger = NIL;
      linesFilled: PInteger = NIL
    ) : TIGPRectF; overload;

    function  GetStringBoundingBoxF(
      string_: WideString;
      font: IGPFont;
      const origin: TPointF;
      stringFormat: IGPStringFormat
    ) : TIGPRectF; overload;

    function  GetStringBoundingBoxF(
      string_: WideString;
      font: IGPFont;
      const layoutRect: TIGPRectF
    ) : TIGPRectF; overload;

    function  GetStringBoundingBoxF(
      string_: WideString;
      font: IGPFont;
      const origin: TPointF
    ) : TIGPRectF; overload;



    function  MeasureStringF(string_: WideString; font: IGPFont;
      stringFormat: IGPStringFormat = NIL ) : TIGPSizeF; overload;

    function  MeasureStringF(string_: WideString; font: IGPFont;
      const layoutRectSize: TIGPSizeF; stringFormat: IGPStringFormat = NIL;
      codepointsFitted: PInteger = NIL; linesFilled: PInteger = NIL) : TIGPSizeF; overload;

    function  MeasureStringF(string_: WideString; font: IGPFont;
      const layoutRect: TIGPRectF; stringFormat: IGPStringFormat;
      codepointsFitted: PInteger = NIL; linesFilled: PInteger = NIL) : TIGPRectF; overload;

    function  MeasureStringF(string_: WideString; font: IGPFont;
      const origin: TPointF; stringFormat: IGPStringFormat ) : TIGPRectF; overload;

    function  MeasureStringF(string_: WideString; font: IGPFont;
      const layoutRect: TIGPRectF ) : TIGPRectF; overload;

    function  MeasureStringF(string_: WideString; font: IGPFont;
      const origin: TPointF ) : TIGPRectF; overload;
      
    // MeasureCharacterRangesF
    function  MeasureCharacterRangesF(string_: WideString; font: IGPFont;
      const layoutRect: TIGPRectF; stringFormat: IGPStringFormat = NIL ) : TArray<IGPRegion>; overload;

    function  MeasureCharacterRangesF(string_: WideString; font: IGPFont;
      const origin: TPointF; stringFormat: IGPStringFormat = NIL ) : TArray<IGPRegion>; overload;

    function  MeasureCharacterRangesF(string_: WideString; font: IGPFont;
      stringFormat: IGPStringFormat = NIL ) : TArray<IGPRegion>; overload;

    function  MeasureCharacterRangesF(string_: WideString; font: IGPFont;
      const layoutRect: TIGPRectF; const ranges : array of TIGPCharacterRange; stringFormat: IGPStringFormat = NIL ) : TArray<IGPRegion>; overload;

    function  MeasureCharacterRangesF(string_: WideString; font: IGPFont;
      const origin: TPointF; const ranges : array of TIGPCharacterRange; stringFormat: IGPStringFormat = NIL ) : TArray<IGPRegion>; overload;

    function  MeasureCharacterRangesF(string_: WideString; font: IGPFont;
      const ranges : array of TIGPCharacterRange; stringFormat: IGPStringFormat = NIL ) : TArray<IGPRegion>; overload;

    // DrawDriverString
    function  DrawDriverString(text: PUINT16; length: Integer; font: IGPFont;
      brush: IGPBrush; positions: PGPPointF; flags: Integer; matrix: IGPMatrix) : TIGPGraphics;

    // MeasureDriverString
    function  GetDriverStringBoundingBoxF(text: PUINT16; length: Integer; font: IGPFont;
       positions: PGPPointF; flags: Integer; matrix: IGPMatrix ) : TIGPRectF;

    // Draw a cached bitmap on this graphics destination offset by
    // x, y. Note this will fail with WrongState if the CachedBitmap
    // native format differs from this Graphics.
    function  DrawCachedBitmap(cb: IGPCachedBitmap;  x, y: Integer) : TIGPGraphics;

    function  DrawImageF(image: IGPImage; const point: TPointF) : TIGPGraphics; overload;
    function  DrawImageF(image: IGPImage; x, y: Single) : TIGPGraphics; overload;
    function  DrawImageF(image: IGPImage; const rect: TIGPRectF) : TIGPGraphics; overload;
    function  DrawImageF(image: IGPImage; x, y, width, height: Single) : TIGPGraphics; overload;
    function  DrawImage(image: IGPImage; const point: TPoint) : TIGPGraphics; overload;
    function  DrawImage(image: IGPImage; x, y: Integer) : TIGPGraphics; overload;
    function  DrawImage(image: IGPImage; const rect: TIGPRect) : TIGPGraphics; overload;
    function  DrawImage(image: IGPImage; x, y, width, height: Integer) : TIGPGraphics; overload;

    function  DrawImageF(image: IGPImage; const point: TPointF; Opacity : Single ) : TIGPGraphics; overload;
    function  DrawImageF(image: IGPImage; x, y: Single; Opacity : Single ) : TIGPGraphics; overload;
    function  DrawImageF(image: IGPImage; const rect: TIGPRectF; Opacity : Single ) : TIGPGraphics; overload;
    function  DrawImageF(image: IGPImage; x, y, width, height: Single; Opacity : Single ) : TIGPGraphics; overload;
    function  DrawImage(image: IGPImage; const point: TPoint; Opacity : Single ) : TIGPGraphics; overload;
    function  DrawImage(image: IGPImage; x, y: Integer; Opacity : Single ) : TIGPGraphics; overload;
    function  DrawImage(image: IGPImage; const rect: TIGPRect; Opacity : Single ) : TIGPGraphics; overload;
    function  DrawImage(image: IGPImage; x, y, width, height: Integer; Opacity : Single ) : TIGPGraphics; overload;

    // Affine Draw Image
    // destPoints.length = 3: rect => parallelogram
    //     destPoints[0] <=> top-left corner of the source rectangle
    //     destPoints[1] <=> top-right corner
    //     destPoints[2] <=> bottom-left corner
    // destPoints.length = 4: rect => quad
    //     destPoints[3] <=> bottom-right corner
    function  DrawImageF(image: IGPImage; const destPoints: array of TPointF ) : TIGPGraphics; overload;
    function  DrawImage(image: IGPImage; const destPoints: array of TPoint ) : TIGPGraphics; overload;
    function  DrawImageF(image: IGPImage; x, y, srcx, srcy, srcwidth, srcheight: Single; srcUnit: TIGPUnit) : TIGPGraphics; overload;
    function  DrawImageF(image: IGPImage; const destRect: TIGPRectF; srcx, srcy,
      srcwidth, srcheight: Single; srcUnit: TIGPUnit;
      imageAttributes: IGPImageAttributes = NIL; callback: TIGPDrawImageAbortProc = NIL) : TIGPGraphics; overload;
    function  DrawImageF(image: IGPImage; const destPoints: array of TPointF;
      srcx, srcy, srcwidth, srcheight: Single; srcUnit: TIGPUnit;
      imageAttributes: IGPImageAttributes = NIL; callback: TIGPDrawImageAbortProc = NIL) : TIGPGraphics; overload;
    function  DrawImage(image: IGPImage; x, y, srcx, srcy, srcwidth,
      srcheight: Integer; srcUnit: TIGPUnit) : TIGPGraphics; overload;
    function  DrawImage(image: IGPImage; const destRect: TIGPRect; srcx, srcy,
      srcwidth, srcheight: Integer; srcUnit: TIGPUnit;
      imageAttributes: IGPImageAttributes = NIL; callback: TIGPDrawImageAbortProc = NIL) : TIGPGraphics; overload;
    function  DrawImage(image: IGPImage; const destPoints: array of TPoint;
      srcx, srcy, srcwidth, srcheight: Integer; srcUnit: TIGPUnit;
      imageAttributes: IGPImageAttributes = NIL; callback: TIGPDrawImageAbortProc = NIL) : TIGPGraphics; overload;

    // The following methods are for playing an EMF+ to a graphics
    // via the enumeration interface.  Each record of the EMF+ is
    // sent to the callback (along with the callbackData).  Then
    // the callback can invoke the Metafile::PlayRecord method
    // to play the particular record.
    function  EnumerateMetafileF(metafile: IGPMetafile; const destPoint: TPointF;
      callback: TIGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TIGPGraphics; overload;
    function  EnumerateMetafile(metafile: IGPMetafile; const destPoint: TPoint;
       callback: TIGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TIGPGraphics; overload;
    function  EnumerateMetafileF(metafile: IGPMetafile; const destRect: TIGPRectF;
       callback: TIGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TIGPGraphics; overload;
    function  EnumerateMetafile(metafile: IGPMetafile; const destRect: TIGPRect;
       callback: TIGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TIGPGraphics; overload;
    function  EnumerateMetafileF(metafile: IGPMetafile; const destPoints: array of TPointF;
       callback: TIGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TIGPGraphics; overload;
    function  EnumerateMetafile(metafile: IGPMetafile; const destPoints: array of TPoint;
       callback: TIGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TIGPGraphics; overload;
    function  EnumerateMetafileF(metafile: IGPMetafile; const destPoint: TPointF;
       const srcRect: TIGPRectF; srcUnit: TIGPUnit; callback: TIGPEnumerateMetafileProc;
       imageAttributes: IGPImageAttributes = NIL
       ) : TIGPGraphics; overload;
    function  EnumerateMetafile(metafile : IGPMetafile; const destPoint : TPoint;
       const srcRect : TIGPRect; srcUnit : TIGPUnit; callback : TIGPEnumerateMetafileProc;
       imageAttributes : IGPImageAttributes = NIL
       ) : TIGPGraphics; overload;
    function  EnumerateMetafileF(metafile: IGPMetafile; const destRect: TIGPRectF;
       const srcRect: TIGPRectF; srcUnit: TIGPUnit; callback: TIGPEnumerateMetafileProc;
       imageAttributes: IGPImageAttributes = NIL) : TIGPGraphics; overload;
    function  EnumerateMetafile(metafile : IGPMetafile; const destRect, srcRect: TIGPRect;
       srcUnit : TIGPUnit; callback : TIGPEnumerateMetafileProc; imageAttributes : IGPImageAttributes = NIL) : TIGPGraphics; overload;
    function  EnumerateMetafileF( metafile: IGPMetafile; const destPoints: array of TPointF;
        const srcRect: TIGPRectF; srcUnit: TIGPUnit; callback: TIGPEnumerateMetafileProc;
        imageAttributes: IGPImageAttributes = NIL) : TIGPGraphics; overload;
    function  EnumerateMetafile(metafile: IGPMetafile; const destPoints: array of TPoint;
        const srcRect: TIGPRect; srcUnit: TIGPUnit; callback: TIGPEnumerateMetafileProc;
        imageAttributes: IGPImageAttributes = NIL) : TIGPGraphics; overload;

    // SetClip
    function  SetClip(g: IGPGraphics; combineMode: TIGPCombineMode = CombineModeReplace) : TIGPGraphics; overload;
    function  SetClipF( const rect: TIGPRectF; combineMode: TIGPCombineMode = CombineModeReplace) : TIGPGraphics;
    function  SetClip( const rect: TIGPRect; combineMode: TIGPCombineMode = CombineModeReplace) : TIGPGraphics; overload;
    function  SetClip(path: IGPGraphicsPath; combineMode: TIGPCombineMode = CombineModeReplace) : TIGPGraphics; overload;
    function  SetClip(region: IGPRegion; combineMode: TIGPCombineMode = CombineModeReplace) : TIGPGraphics; overload;
    // This is different than the other SetClip methods because it assumes
    // that the HRGN is already in device units, so it doesn't transform
    // the coordinates in the HRGN.
    function  SetClip(hRgn: HRGN; combineMode: TIGPCombineMode = CombineModeReplace) : TIGPGraphics; overload;

    procedure SetClipProp( region: IGPRegion );

    // IntersectClip
    function  IntersectClipF(const rect: TIGPRectF) : TIGPGraphics;
    function  IntersectClip(const rect: TIGPRect) : TIGPGraphics; overload;
    function  IntersectClip(region: IGPRegion) : TIGPGraphics; overload;
    // ExcludeClip
    function  ExcludeClipF(const rect: TIGPRectF) : TIGPGraphics;
    function  ExcludeClip(const rect: TIGPRect) : TIGPGraphics; overload;
    function  ExcludeClip(region: IGPRegion) : TIGPGraphics; overload;

    function  ResetClip() : TIGPGraphics;

    function  TranslateClipF(dx, dy: Single) : TIGPGraphics; overload;
    function  TranslateClip(dx, dy: Integer) : TIGPGraphics; overload;

    function  GetClip() : IGPRegion;

    function  GetClipBoundsF() : TIGPRectF;
    function  GetClipBounds() : TIGPRect;

    function  IsClipEmpty() : Boolean;

    function  GetVisibleClipBoundsF() : TIGPRectF;
    function  GetVisibleClipBounds() : TIGPRect;

    function  IsVisibleClipEmpty() : Boolean;

    function  IsVisible(x, y: Integer) : Boolean; overload;
    function  IsVisible(const point: TPoint) : Boolean; overload;
    function  IsVisible(x, y, width, height: Integer) : Boolean; overload;
    function  IsVisible(const rect: TIGPRect) : Boolean; overload;
    function  IsVisibleF(x, y: Single) : Boolean; overload;
    function  IsVisibleF(const point: TPointF) : Boolean; overload;
    function  IsVisibleF(x, y, width, height: Single) : Boolean; overload;
    function  IsVisibleF(const rect: TIGPRectF) : Boolean; overload;

    function  Save() : TIGPGraphicsState;
    function  Restore( gstate: TIGPGraphicsState ) : TIGPGraphics;

    function  BeginContainerF(const dstrect,srcrect: TIGPRectF; unit_: TIGPUnit) : TIGPGraphicsContainer; overload;
    function  BeginContainer(const dstrect, srcrect: TIGPRect; unit_: TIGPUnit) : TIGPGraphicsContainer; overload;
    function  BeginContainer() : TIGPGraphicsContainer; overload;
    function  EndContainer(state: TIGPGraphicsContainer) : TIGPGraphics;

    // Only valid when recording metafiles.
    function  AddMetafileComment( const data: array of BYTE ) : TIGPGraphics;

    class function GetHalftonePalette() : HPALETTE;
                                          
  protected
    function  SetTransformT(matrix: IGPMatrix) : IGPTransformable;
    function  ResetTransformT() : IGPTransformable;
    function  MultiplyTransformT(matrix: IGPMatrix; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function  TranslateTransformT(dx, dy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function  ScaleTransformT(sx, sy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function  ScaleTransformXYT(s: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
    function  RotateTransformT(angle: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;

    function  IGPTransformable.SetTransform = SetTransformT;
    function  IGPTransformable.ResetTransform = ResetTransformT;
    function  IGPTransformable.MultiplyTransform = MultiplyTransformT;
    function  IGPTransformable.TranslateTransform = TranslateTransformT;
    function  IGPTransformable.ScaleTransform = ScaleTransformT;
    function  IGPTransformable.ScaleTransformXY = ScaleTransformXYT;
    function  IGPTransformable.RotateTransform = RotateTransformT;

  end;
{$ELSE}
  TIGPGraphics = class( TInterfacedObject, IGPGraphics )
  end;
{$ENDIF}

(**************************************************************************\
*
*   GDI+ CustomLineCap APIs
*
\**************************************************************************)

  IGPAdjustableArrowCap = interface( IGPCustomLineCap )
    ['{4E024341-42A2-499E-8EA3-884DA121AF7A}']
    function SetHeight(height: Single) : TIGPAdjustableArrowCap;
    procedure SetHeightProp(height: Single);
    function GetHeight() : Single;
    function SetWidth(width: Single) : TIGPAdjustableArrowCap;
    procedure SetWidthProp(width: Single);
    function GetWidth() : Single;
    function SetMiddleInset(middleInset: Single) : TIGPAdjustableArrowCap;
    procedure SetMiddleInsetProp(middleInset: Single);
    function GetMiddleInset() : Single;
    function SetFillState(isFilled: Boolean) : TIGPAdjustableArrowCap;
    function IsFilled() : Boolean;

    property Width        : Single read GetWidth write SetWidthProp;
    property Height       : Single read GetHeight write SetHeightProp;
    property MiddleInset  : Single read GetMiddleInset write SetMiddleInsetProp;
  end;
  
{$IFDEF MSWINDOWS}
  TIGPAdjustableArrowCap = class(TIGPCustomLineCap, IGPAdjustableArrowCap)
  public
    constructor Create(height, width: Single; isFilled: Boolean = True); 
  public
    function SetHeight(height: Single) : TIGPAdjustableArrowCap;
    procedure SetHeightProp(height: Single);
    function GetHeight() : Single;
    function SetWidth(width: Single) : TIGPAdjustableArrowCap;
    procedure SetWidthProp(width: Single);
    function GetWidth() : Single;
    function SetMiddleInset(middleInset: Single) : TIGPAdjustableArrowCap;
    procedure SetMiddleInsetProp(middleInset: Single);
    function GetMiddleInset() : Single;
    function SetFillState(isFilled: Boolean) : TIGPAdjustableArrowCap;
    function IsFilled() : Boolean;
    
  end;
{$ENDIF}

(**************************************************************************\
*
*   GDI+ Metafile class
*
\**************************************************************************)

  IGPMetafile = interface( IGPImage )
    ['{E9766E82-C370-40C9-AEDA-4A07CBC9BC92}']
    
    function GetMetafileHeader() : IGPMetafileHeader;
    // Once this method is called, the Metafile object is in an invalid state
    // and can no longer be used.  It is the responsiblity of the caller to
    // invoke DeleteEnhMetaFile to delete this hEmf.                                     
    function GetHENHMETAFILE() : HENHMETAFILE;
    // Used in conjuction with Graphics::EnumerateMetafile to play an EMF+
    // The data must be DWORD aligned if it's an EMF or EMF+.  It must be
    // WORD aligned if it's a WMF.
    function PlayRecord(recordType: TIGPEmfPlusRecordType; flags, dataSize: Cardinal; data: PBYTE) : TIGPMetafile;
    // If you're using a printer HDC for the metafile, but you want the
    // metafile rasterized at screen resolution, then use this API to set
    // the rasterization dpi of the metafile to the screen resolution,
    // e.g. 96 dpi or 120 dpi.
    function SetDownLevelRasterizationLimit(metafileRasterizationLimitDpi: Cardinal) : TIGPMetafile;
    procedure SetDownLevelRasterizationLimitProp(metafileRasterizationLimitDpi: Cardinal);
    function GetDownLevelRasterizationLimit() : Cardinal;
    function EmfToWmfBits(hemf: HENHMETAFILE; cbData16: Cardinal; pData16: PBYTE;
      iMapMode: Integer = MM_ANISOTROPIC; eFlags: TIGPEmfToWmfBitsFlags = EmfToWmfBitsFlagsDefault) : Cardinal;

    property DownLevelRasterizationLimit  : Cardinal read GetDownLevelRasterizationLimit write SetDownLevelRasterizationLimitProp;
    property Header                       : IGPMetafileHeader read GetMetafileHeader;
  end;
  
{$IFDEF MSWINDOWS}
  {$HPPEMIT 'typedef int TIGPMetafile;'}

  {$EXTERNALSYM TIGPMetafile}
  TIGPMetafile = class(TIGPImage, IGPMetafile)
  public
    // Playback a metafile from a HMETAFILE
    // If deleteWmf is True, then when the metafile is deleted,
    // the hWmf will also be deleted.  Otherwise, it won't be.
    constructor Create(hWmf: HMETAFILE; var wmfPlaceableFileHeader: TIGPWmfPlaceableFileHeader;
      deleteWmf: Boolean = False); overload;
    // Playback a metafile from a HENHMETAFILE
    // If deleteEmf is True, then when the metafile is deleted,
    // the hEmf will also be deleted.  Otherwise, it won't be.
    constructor Create(hEmf: HENHMETAFILE; deleteEmf: Boolean = False); overload;
    constructor Create(filename: WideString); overload;
    // Playback a WMF metafile from a file.
    constructor Create(filename: WideString; var wmfPlaceableFileHeader: TIGPWmfPlaceableFileHeader); overload;
    constructor Create(stream: IStream); overload;
    // Record a metafile to memory.
    constructor Create(referenceHdc: HDC; type_: TIGPEmfType = EmfTypeEmfPlusDual;
      description: PWCHAR = NIL); overload;
    // Record a metafile to memory.
    constructor Create(referenceHdc: HDC; const frameRect: TIGPRectF;
      frameUnit: TIGPMetafileFrameUnit = MetafileFrameUnitGdi;
      type_: TIGPEmfType = EmfTypeEmfPlusDual; description: PWCHAR = NIL); overload;
    // Record a metafile to memory.
    constructor Create(referenceHdc: HDC; const frameRect: TIGPRect;
      frameUnit: TIGPMetafileFrameUnit = MetafileFrameUnitGdi;
      type_: TIGPEmfType = EmfTypeEmfPlusDual; description: PWCHAR = NIL); overload;
    constructor Create(fileName: WideString;referenceHdc: HDC;
      type_: TIGPEmfType = EmfTypeEmfPlusDual; description: PWCHAR = NIL); overload;
    constructor Create(fileName: WideString; referenceHdc: HDC; const frameRect: TIGPRectF;
      frameUnit: TIGPMetafileFrameUnit = MetafileFrameUnitGdi;
      type_: TIGPEmfType = EmfTypeEmfPlusDual; description: PWCHAR = NIL); overload;
    constructor Create( fileName: WideString; referenceHdc: HDC; frameRect: TIGPRect;
      frameUnit: TIGPMetafileFrameUnit = MetafileFrameUnitGdi;
      type_: TIGPEmfType = EmfTypeEmfPlusDual; description: PWCHAR = NIL); overload;
    constructor Create(stream: IStream; referenceHdc: HDC;
      type_: TIGPEmfType = EmfTypeEmfPlusDual; description: PWCHAR = NIL); overload;
    constructor Create(stream: IStream; referenceHdc: HDC; const frameRect: TIGPRectF;
      frameUnit: TIGPMetafileFrameUnit = MetafileFrameUnitGdi;
      type_: TIGPEmfType = EmfTypeEmfPlusDual; description: PWCHAR = NIL); overload;
    constructor Create(stream : IStream; referenceHdc : HDC; frameRect : TIGPRect;
     frameUnit : TIGPMetafileFrameUnit = MetafileFrameUnitGdi;
     type_ : TIGPEmfType = EmfTypeEmfPlusDual; description : PWCHAR = NIL); overload;
    constructor Create(); overload;
  public
    class function GetMetafileHeader(hWmf: HMETAFILE; var wmfPlaceableFileHeader: TIGPWmfPlaceableFileHeader) : IGPMetafileHeader; overload;
    class function GetMetafileHeader(hEmf: HENHMETAFILE) : IGPMetafileHeader; overload;
    class function GetMetafileHeader(filename: WideString) : IGPMetafileHeader; overload;
    class function GetMetafileHeader(stream: IStream) : IGPMetafileHeader; overload;
    function GetMetafileHeader() : IGPMetafileHeader; overload;
    // Once this method is called, the Metafile object is in an invalid state
    // and can no longer be used.  It is the responsiblity of the caller to
    // invoke DeleteEnhMetaFile to delete this hEmf.                                     
    function GetHENHMETAFILE() : HENHMETAFILE;
    // Used in conjuction with Graphics::EnumerateMetafile to play an EMF+
    // The data must be DWORD aligned if it's an EMF or EMF+.  It must be
    // WORD aligned if it's a WMF.
    function PlayRecord(recordType: TIGPEmfPlusRecordType; flags, dataSize: Cardinal; data: PBYTE) : TIGPMetafile;
    // If you're using a printer HDC for the metafile, but you want the
    // metafile rasterized at screen resolution, then use this API to set
    // the rasterization dpi of the metafile to the screen resolution,
    // e.g. 96 dpi or 120 dpi.
    function SetDownLevelRasterizationLimit(metafileRasterizationLimitDpi: Cardinal) : TIGPMetafile;
    procedure SetDownLevelRasterizationLimitProp(metafileRasterizationLimitDpi: Cardinal);
    function GetDownLevelRasterizationLimit: Cardinal;
    function EmfToWmfBits(hemf: HENHMETAFILE; cbData16: Cardinal; pData16: PBYTE;
      iMapMode: Integer = MM_ANISOTROPIC; eFlags: TIGPEmfToWmfBitsFlags = EmfToWmfBitsFlagsDefault) : Cardinal;

  end;

function GetStatus(Stat: TIGPStatus) : String;

procedure StartIGDIPlus();
procedure StopIGDIPlus();
{$ENDIF} //MSWINDOWS

implementation


{$IFDEF MSWINDOWS}
uses
  Math;

type
  TIGPBitmapData = class( TIGPBase, IGPBitmapData )
  protected
    FData   : TIGPBitmapDataRecord;
    FBitmap : TIGPBitmap;

  public
    function GetWidth() : UINT;
    function GetHeight() : UINT;
    function GetStride() : Integer;
    function GetPixelFormat() : TIGPPixelFormat;
    function GetScan0() : Pointer;

  protected
    constructor Create( ABitmap : TIGPBitmap );
    destructor  Destroy(); override;

  end;

  TIGPPathData = packed class( TIGPBase, IGPPathData )
  protected
    FCount  : Integer;
    FPoints : PGPPointF;
    FTypes  : PBYTE;

  public
    function GetCount()  : Integer;
    function GetPoints( Index : Integer ) : TPointF;
    function GetTypes( Index : Integer )  : TIGPPathPointType;

  public
    constructor Create();
    destructor  Destroy(); override;

  end;

  TIGPMetafileHeader = packed class( TIGPBase, IGPMetafileHeader )
  protected
    FType         : TIGPMetafileType;
    FSize         : UINT;           // Size of the metafile (in bytes)
    FVersion      : UINT;           // EMF+, EMF, or WMF version
    FEmfPlusFlags : UINT;
    FDpiX         : Single;
    FDpiY         : Single;
    FX            : Integer;        // Bounds in device units
    FY            : Integer;
    FWidth        : Integer;
    FHeight       : Integer;
    FHeader       : record
    case Integer of
      0: (FWmfHeader: TMETAHEADER);
      1: (FEmfHeader: TIGPENHMETAHEADER3);
    end;

    FEmfPlusHeaderSize : Integer; // size of the EMF+ header in file
    FLogicalDpiX       : Integer; // Logical Dpi of reference Hdc
    FLogicalDpiY       : Integer; // usually valid only for EMF+

  public
    function GetType() : TIGPMetafileType;
    function GetMetafileSize() : UINT;
    // If IsEmfPlus, this is the EMF+ version; else it is the WMF or EMF ver
    function GetVersion() : UINT;
     // Get the EMF+ flags associated with the metafile
    function GetEmfPlusFlags() : UINT;
    function GetDpiX() : Single;
    function GetDpiY() : Single;
    function GetBounds() : TIGPRect;
    // Is it any type of WMF (standard or Placeable Metafile)?
    function IsWmf() : Boolean;
    // Is this an Placeable Metafile?
    function IsWmfPlaceable() : Boolean;
    // Is this an EMF (not an EMF+)?
    function IsEmf() : Boolean;
    // Is this an EMF or EMF+ file?
    function IsEmfOrEmfPlus() : Boolean;
    // Is this an EMF+ file?
    function IsEmfPlus() : Boolean;
    // Is this an EMF+ dual (has dual, down-level records) file?
    function IsEmfPlusDual() : Boolean;
    // Is this an EMF+ only (no dual records) file?
    function IsEmfPlusOnly() : Boolean;
    // If it's an EMF+ file, was it recorded against a display Hdc?
    function IsDisplay() : Boolean;
    // Get the WMF header of the metafile (if it is a WMF)
    function GetWmfHeader() : PMetaHeader;
    // Get the EMF header of the metafile (if it is an EMF)
    function GetEmfHeader() : PENHMETAHEADER3;

  end;

{$I IGDIPlusAPI.inc}

{$ENDIF} //MSWINDOWS

const
  AlphaShift  = 24;
  RedShift    = 16;
  GreenShift  = 8;
  BlueShift   = 0;

  AlphaMask   = $ff000000;
  RedMask     = $00ff0000;
  GreenMask   = $0000ff00;
  BlueMask    = $000000ff;

const StandardAlphaMatrix : TIGPColorMatrix =
  (
    ( 1.0, 0.0, 0.0, 0.0, 0.0 ),
    ( 0.0, 1.0, 0.0, 0.0, 0.0 ),
    ( 0.0, 0.0, 1.0, 0.0, 0.0 ),
    ( 0.0, 0.0, 0.0, 1.0, 0.0 ),
    ( 0.0, 0.0, 0.0, 0.0, 1.0 )
  );

var
  GenericSansSerifFontFamily : TIGPFontFamily = NIL;
  GenericSerifFontFamily     : TIGPFontFamily = NIL;
  GenericMonospaceFontFamily : TIGPFontFamily = NIL;

  GenericTypographicStringFormatBuffer: TIGPStringFormat = NIL;
  GenericDefaultStringFormatBuffer    : TIGPStringFormat = NIL;

{$IFDEF MSWINDOWS}
  StartupInput: TIGPGDIPlusStartupInput;
  StartupOutput: TIGPGdiplusStartupOutput;
  gdiplusBGThreadToken : Pointer;
  gdiplusToken: Pointer;
  GInitialized : Boolean = False;

(**************************************************************************\
*
* Image Attributes
*
* Abstract:
*
*   GDI+ Image Attributes used with Graphics.DrawImage
*
* There are 5 possible sets of color adjustments:
*          ColorAdjustDefault,
*          ColorAdjustBitmap,
*          ColorAdjustBrush,
*          ColorAdjustPen,
*          ColorAdjustText,
*
* Bitmaps, Brushes, Pens, and Text will all use any color adjustments
* that have been set into the default ImageAttributes until their own
* color adjustments have been set.  So as soon as any "Set" method is
* called for Bitmaps, Brushes, Pens, or Text, then they start from
* scratch with only the color adjustments that have been set for them.
* Calling Reset removes any individual color adjustments for a type
* and makes it revert back to using all the default color adjustments
* (if any).  The SetToIdentity method is a way to force a type to
* have no color adjustments at all, regardless of what previous adjustments
* have been set for the defaults or for that type.
*
\********************************************************************F******)

constructor TIGPImageAttributes.Create();
begin
  FNativeImageAttr := NIL;
  ErrorCheck( GdipCreateImageAttributes(FNativeImageAttr));
end;

destructor TIGPImageAttributes.Destroy();
begin
  GdipDisposeImageAttributes(FNativeImageAttr);
  inherited Destroy();
end;

function TIGPImageAttributes.Clone() : TIGPImageAttributes;
var clone: GpImageAttributes;
begin
  ErrorCheck( GdipCloneImageAttributes(FNativeImageAttr, clone));
  Result := TIGPImageAttributes.CreateGdiPlus(clone, False);
end;

function TIGPImageAttributes.SetToIdentity(type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesToIdentity(FNativeImageAttr, type_));
  Result := Self;
end;

function TIGPImageAttributes.Reset(type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
begin
  ErrorCheck( GdipResetImageAttributes(FNativeImageAttr, type_));
  Result := Self;
end;

function TIGPImageAttributes.SetColorMatrix(const colorMatrix: TIGPColorMatrix;
    mode: TIGPColorMatrixFlags = ColorMatrixFlagsDefault;
    type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesColorMatrix(FNativeImageAttr,
    type_, True, @colorMatrix, NIL, mode));
      
  Result := Self;
end;

function TIGPImageAttributes.ClearColorMatrix(type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesColorMatrix(FNativeImageAttr, type_,
    False, NIL, NIL, ColorMatrixFlagsDefault));

  Result := Self;
end;

    
function TIGPImageAttributes.SetColorMatrices(const colorMatrix: TIGPColorMatrix; const grayMatrix: TIGPColorMatrix;
    mode: TIGPColorMatrixFlags  = ColorMatrixFlagsDefault;
    type_: TIGPColorAdjustType  = ColorAdjustTypeDefault) : TIGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesColorMatrix(FNativeImageAttr, type_,
    True, @colorMatrix, @grayMatrix, mode));

  Result := Self;
end;

function TIGPImageAttributes.ClearColorMatrices(Type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesColorMatrix( FNativeImageAttr,
    type_, False, NIL, NIL, ColorMatrixFlagsDefault));

  Result := Self;
end;

function TIGPImageAttributes.SetThreshold(threshold: Single; type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesThreshold( FNativeImageAttr, type_,
    True, threshold));

  Result := Self;
end;

function TIGPImageAttributes.ClearThreshold(type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesThreshold(FNativeImageAttr, type_,
    False, 0.0));

  Result := Self;
end;

function TIGPImageAttributes.SetGamma(gamma: Single; type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesGamma(FNativeImageAttr, type_, True, gamma));
  Result := Self;
end;

function TIGPImageAttributes.ClearGamma( type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesGamma(FNativeImageAttr, type_, False, 0.0));
  Result := Self;
end;

function TIGPImageAttributes.SetNoOp(type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesNoOp(FNativeImageAttr, type_, True));
  Result := Self;
end;

function TIGPImageAttributes.ClearNoOp(Type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesNoOp( FNativeImageAttr, type_, False));
  Result := Self;
end;

function TIGPImageAttributes.SetColorKey(colorLow, colorHigh: TAlphaColor; type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesColorKeys(FNativeImageAttr, type_,
    True, colorLow, colorHigh));

  Result := Self;
end;

function TIGPImageAttributes.ClearColorKey(type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesColorKeys(FNativeImageAttr, type_,
    False, 0, 0));

  Result := Self;
end;

function TIGPImageAttributes.SetOutputChannel(channelFlags: TIGPColorChannelFlags; type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesOutputChannel(FNativeImageAttr,
    type_, True, channelFlags));

  Result := Self;
end;

function TIGPImageAttributes.ClearOutputChannel(type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesOutputChannel(FNativeImageAttr,
    type_, False, ColorChannelFlagsLast));

  Result := Self;
end;

function TIGPImageAttributes.SetOutputChannelColorProfile(colorProfileFilename: WideString;
    type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesOutputChannelColorProfile(FNativeImageAttr,
    type_, True, PWideChar(colorProfileFilename)));

  Result := Self;
end;

function TIGPImageAttributes.ClearOutputChannelColorProfile(type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesOutputChannelColorProfile(FNativeImageAttr,
    type_, False, NIL));

  Result := Self;
end;

function TIGPImageAttributes.SetRemapTable(mapSize: Cardinal; map: PGPColorMap; type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesRemapTable(FNativeImageAttr, type_,
    True, mapSize, map));

  Result := Self;
end;

function TIGPImageAttributes.ClearRemapTable(type_: TIGPColorAdjustType = ColorAdjustTypeDefault) : TIGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesRemapTable(FNativeImageAttr, type_,
    False, 0, NIL));

  Result := Self;
end;

function TIGPImageAttributes.SetBrushRemapTable(mapSize: Cardinal; map: PGPColorMap) : TIGPImageAttributes;
begin
  Result := SetRemapTable(mapSize, map, ColorAdjustTypeBrush);
end;

function TIGPImageAttributes.ClearBrushRemapTable() : TIGPImageAttributes;
begin
  Result := ClearRemapTable(ColorAdjustTypeBrush);
end;

function TIGPImageAttributes.SetWrapMode(wrap: TIGPWrapMode; color: TAlphaColor = aclBlack; clamp: Boolean = False) : TIGPImageAttributes;
begin
  ErrorCheck( GdipSetImageAttributesWrapMode(FNativeImageAttr, wrap, color, clamp));
  Result := Self;
end;

// The flags of the palette are ignored.

function TIGPImageAttributes.GetAdjustedPalette(colorPalette: PGPColorPalette; colorAdjustType: TIGPColorAdjustType) : TIGPImageAttributes;
begin
  ErrorCheck( GdipGetImageAttributesAdjustedPalette(FNativeImageAttr,
    colorPalette, colorAdjustType));

  Result := Self;
end;

constructor TIGPImageAttributes.CreateGdiPlus(imageAttr: GpImageAttributes; Dummy : Boolean);
begin
  SetNativeImageAttr(imageAttr);
end;

procedure TIGPImageAttributes.SetNativeImageAttr(nativeImageAttr: GpImageAttributes);
begin
  FNativeImageAttr := nativeImageAttr;
end;

function TIGPImageAttributes.GetNativeImageAttr() : GpImageAttributes;
begin
  Result := FNativeImageAttr;
end;

(**************************************************************************\
*
*   GDI+ Matrix class
*
\**************************************************************************)

// Default constructor is set to identity matrix.
constructor TIGPMatrix.Create();
var matrix: GpMatrix;
begin
  matrix := NIL;
  ErrorCheck( GdipCreateMatrix(matrix));
  SetNativeMatrix(matrix);
end;

constructor TIGPMatrix.Create(m11, m12, m21, m22, dx, dy: Single);
var matrix: GpMatrix;
begin
  matrix := NIL;
  ErrorCheck( GdipCreateMatrix2(m11, m12, m21, m22, dx, dy, matrix));
  SetNativeMatrix(matrix);
end;

constructor TIGPMatrix.Create(const rect: TIGPRectF; const dstplg: TPointF);
var matrix: GpMatrix;
begin
  matrix := NIL;
  ErrorCheck( GdipCreateMatrix3(@rect, @dstplg, matrix));
  SetNativeMatrix(matrix);
end;

constructor TIGPMatrix.Create(const rect: TIGPRect; const dstplg: TPoint);
var matrix: GpMatrix;
begin
  matrix := NIL;
  ErrorCheck( GdipCreateMatrix3I(@rect, @dstplg, matrix));
  SetNativeMatrix(matrix);
end;

destructor TIGPMatrix.Destroy();
begin
  GdipDeleteMatrix(FNativeMatrix);
end;

function TIGPMatrix.Clone() : TIGPMatrix;
var cloneMatrix: GpMatrix;
begin
  cloneMatrix := NIL;
  ErrorCheck( GdipCloneMatrix(FNativeMatrix, cloneMatrix));
  Result := TIGPMatrix.CreateGdiPlus(cloneMatrix, False);
end;

function TIGPMatrix.GetElements() : TIGPMatrixParams;
begin
  ErrorCheck( GdipGetMatrixElements(FNativeMatrix, PSingle(@Result) ));
end;

function TIGPMatrix.SetElements(m11, m12, m21, m22, dx, dy: Single) : TIGPMatrix;
begin
  ErrorCheck( GdipSetMatrixElements(FNativeMatrix,
                          m11, m12, m21, m22, dx, dy));
                            
  Result := Self;
end;

function TIGPMatrix.SetElements( AElements : TIGPMatrixParams ) : TIGPMatrix;
begin
  ErrorCheck( GdipSetMatrixElements( FNativeMatrix,
                          AElements.m11, AElements.m12, AElements.m21, AElements.m22, AElements.dx, AElements.dy ));
                            
  Result := Self;
end;

procedure TIGPMatrix.SetElementsProp( AElements : TIGPMatrixParams );
begin
  ErrorCheck( GdipSetMatrixElements( FNativeMatrix,
                          AElements.m11, AElements.m12, AElements.m21, AElements.m22, AElements.dx, AElements.dy ));
                            
end;

function TIGPMatrix.OffsetX() : Single;
begin
  Result := GetElements().dx;// [4];
end;

function TIGPMatrix.OffsetY: Single;
begin
  Result := GetElements().dy; // [5];
end;

function TIGPMatrix.Reset() : TIGPMatrix;
begin
  // set identity matrix elements
  ErrorCheck( GdipSetMatrixElements(FNativeMatrix, 1.0, 0.0, 0.0, 1.0,
              0.0, 0.0));
                
  Result := Self;
end;

function TIGPMatrix.Multiply(matrix: IGPMatrix; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPMatrix;
begin
  Assert( matrix <> NIL );

  ErrorCheck( GdipMultiplyMatrix(FNativeMatrix, matrix.GetNativeMatrix(), order));
  Result := Self;
end;

function TIGPMatrix.Translate(offsetX, offsetY: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPMatrix;
begin
  ErrorCheck( GdipTranslateMatrix(FNativeMatrix, offsetX, offsetY, order));
  Result := Self;
end;

function TIGPMatrix.Scale(scaleX, scaleY: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPMatrix;
begin
  ErrorCheck( GdipScaleMatrix(FNativeMatrix, scaleX, scaleY, order));
  Result := Self;
end;

function TIGPMatrix.Rotate(angle: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPMatrix;
begin
  ErrorCheck( GdipRotateMatrix(FNativeMatrix, angle, order));
  Result := Self;
end;

function TIGPMatrix.RotateAt(angle: Single; const center: TPointF; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPMatrix;
begin
  if(order = MatrixOrderPrepend) then
    begin
    ErrorCheck( GdipTranslateMatrix(FNativeMatrix, center.X, center.Y, order));
    ErrorCheck( GdipRotateMatrix(FNativeMatrix, angle, order));
    ErrorCheck( GdipTranslateMatrix(FNativeMatrix, -center.X, -center.Y,
                order));
    end
      
  else
    begin
    ErrorCheck( GdipTranslateMatrix(FNativeMatrix, - center.X, - center.Y, order));
    ErrorCheck( GdipRotateMatrix(FNativeMatrix, angle, order));
    ErrorCheck( GdipTranslateMatrix(FNativeMatrix, center.X, center.Y,
                order));
    end;
    
  Result := Self;
end;

function TIGPMatrix.Shear(shearX, shearY: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPMatrix;
begin
  ErrorCheck( GdipShearMatrix(FNativeMatrix, shearX, shearY, order));
  Result := Self;
end;

function TIGPMatrix.Invert() : TIGPMatrix;
begin
  ErrorCheck( GdipInvertMatrix(FNativeMatrix));
  Result := Self;
end;

function TIGPMatrix.TransformPointF( var point : TPointF ) : TIGPMatrix;
var
  pts : array [ 0..0 ] of TPointF;

begin
  pts[ 0 ] := point;
  Result := TransformPointsF( pts );
  point := pts[ 0 ]; 
end;

function TIGPMatrix.TransformPoint( var point : TPoint ) : TIGPMatrix;
var
  pts : array [ 0..0 ] of TPoint;

begin
  pts[ 0 ] := point;
  Result := TransformPoints( pts );
  point := pts[ 0 ]; 
end;
  
// float version
function TIGPMatrix.TransformPointsF( var pts : array of TPointF ) : TIGPMatrix;
begin
  ErrorCheck( GdipTransformMatrixPoints( FNativeMatrix, @pts[ 0 ], Length( pts )));
  Result := Self;
end;

function TIGPMatrix.TransformPoints( var pts : array of TPoint ) : TIGPMatrix;
begin
  ErrorCheck( GdipTransformMatrixPointsI(FNativeMatrix, @pts[ 0 ], Length( pts )));
  Result := Self;
end;

function TIGPMatrix.TransformVectorsF( var pts : array of TPointF ) : TIGPMatrix;
begin
  ErrorCheck( GdipVectorTransformMatrixPoints( FNativeMatrix, @pts[ 0 ], Length( pts )));
  Result := Self;
end;

function TIGPMatrix.TransformVectors( var pts : array of TPoint ) : TIGPMatrix;
begin
  ErrorCheck( GdipVectorTransformMatrixPointsI(FNativeMatrix, @pts[ 0 ], Length( pts )));
  Result := Self;
end;

function TIGPMatrix.IsInvertible() : Boolean;
var
  AValue : BOOL;
    
begin
  ErrorCheck( GdipIsMatrixInvertible(FNativeMatrix, AValue ));
  Result := AValue;
end;

function TIGPMatrix.IsIdentity() : Boolean;
var
  AValue : BOOL;
    
begin
  ErrorCheck( GdipIsMatrixIdentity(FNativeMatrix, AValue));
  Result := AValue;
end;

function TIGPMatrix.EqualsMatrix(matrix: IGPMatrix) : Boolean;
var
  AValue : BOOL;

begin
  ErrorCheck( GdipIsMatrixEqual(FNativeMatrix, matrix.GetNativeMatrix(), AValue ));
  Result := AValue;
end;

constructor TIGPMatrix.CreateGdiPlus(nativeMatrix: GpMatrix; Dummy : Boolean);
begin
  SetNativeMatrix(nativeMatrix);
end;

procedure TIGPMatrix.SetNativeMatrix(nativeMatrix: GpMatrix);
begin
  FNativeMatrix := nativeMatrix;
end;

function TIGPMatrix.GetNativeMatrix() : GpMatrix;
begin
  Result := FNativeMatrix;
end;

(**************************************************************************\
\**************************************************************************)

constructor TIGPMatrixStore.Create( ATransformable : IGPTransformable );
begin
  inherited Create();
  FTransformable := ATransformable;
  FMatrix := FTransformable.Transform;
end;

destructor TIGPMatrixStore.Destroy();
begin
  FTransformable.Transform := FMatrix;
  inherited;
end;

(**************************************************************************\
*
*   GDI+ StringFormat class
*
\**************************************************************************)

constructor TIGPStringFormat.Create(formatFlags: Integer = 0; language: LANGID = LANG_NEUTRAL);
begin
  FNativeFormat := NIL;
  ErrorCheck( GdipCreateStringFormat(formatFlags, language, FNativeFormat));
end;

class function TIGPStringFormat.GenericDefault: TIGPStringFormat;
begin
  if( not Assigned(GenericDefaultStringFormatBuffer)) then
  begin
    GenericDefaultStringFormatBuffer := TIGPStringFormat.Create();
    ErrorCheck( GdipStringFormatGetGenericDefault(GenericDefaultStringFormatBuffer.FNativeFormat));
  end;
  Result := GenericDefaultStringFormatBuffer;
end;

class function TIGPStringFormat.GenericTypographic: TIGPStringFormat;
begin
  if( not Assigned(GenericTypographicStringFormatBuffer)) then
  begin
    GenericTypographicStringFormatBuffer := TIGPStringFormat.Create();
    ErrorCheck( GdipStringFormatGetGenericTypographic(GenericTypographicStringFormatBuffer.FNativeFormat));
  end;
  Result := GenericTypographicStringFormatBuffer;
end;

constructor TIGPStringFormat.Create(format: TIGPStringFormat);
var gpstf: GPSTRINGFORMAT;
begin
  FNativeFormat := NIL;
  if( Assigned(format)) then
    gpstf := format.FNativeFormat

  else
    gpstf := NIL;
      
  ErrorCheck( GdipCloneStringFormat(gpstf, FNativeFormat));
end;

function TIGPStringFormat.Clone() : TIGPStringFormat;
var
  clonedStringFormat: GpStringFormat;
begin
  clonedStringFormat := NIL;
  ErrorCheck( GdipCloneStringFormat(FNativeFormat, clonedStringFormat));
  Result := TIGPStringFormat.CreateGdiPlus(clonedStringFormat, False);
end;

destructor TIGPStringFormat.Destroy();
begin
  GdipDeleteStringFormat(FNativeFormat);
end;

function TIGPStringFormat.SetFormatFlags(flags: Integer) : TIGPStringFormat;
begin
  ErrorCheck( GdipSetStringFormatFlags(FNativeFormat, flags));
  Result := Self;
end;

procedure TIGPStringFormat.SetFormatFlagsProp(flags: Integer);
begin
  ErrorCheck( GdipSetStringFormatFlags(FNativeFormat, flags));
end;

function TIGPStringFormat.GetFormatFlags() : Integer;
begin
  ErrorCheck( GdipGetStringFormatFlags(FNativeFormat, Result));
end;

function TIGPStringFormat.SetAlignment(align: TIGPStringAlignment) : TIGPStringFormat;
begin
  ErrorCheck( GdipSetStringFormatAlign(FNativeFormat, align));
  Result := Self;
end;

procedure TIGPStringFormat.SetAlignmentProp(align: TIGPStringAlignment);
begin
  ErrorCheck( GdipSetStringFormatAlign(FNativeFormat, align));
end;

function TIGPStringFormat.GetAlignment: TIGPStringAlignment;
begin
  ErrorCheck( GdipGetStringFormatAlign(FNativeFormat, Result));
end;

function TIGPStringFormat.SetLineAlignment(align: TIGPStringAlignment) : TIGPStringFormat;
begin
  ErrorCheck( GdipSetStringFormatLineAlign(FNativeFormat, align));
  Result := Self;
end;

procedure TIGPStringFormat.SetLineAlignmentProp(align: TIGPStringAlignment);
begin
  ErrorCheck( GdipSetStringFormatLineAlign(FNativeFormat, align));
end;

function TIGPStringFormat.GetLineAlignment: TIGPStringAlignment;
begin
  ErrorCheck( GdipGetStringFormatLineAlign(FNativeFormat, Result));
end;

function TIGPStringFormat.SetHotkeyPrefix(hotkeyPrefix: TIGPHotkeyPrefix) : TIGPStringFormat;
begin
  ErrorCheck( GdipSetStringFormatHotkeyPrefix(FNativeFormat, Integer(hotkeyPrefix)));
  Result := Self;
end;

procedure TIGPStringFormat.SetHotkeyPrefixProp(hotkeyPrefix: TIGPHotkeyPrefix);
begin
  ErrorCheck( GdipSetStringFormatHotkeyPrefix(FNativeFormat, Integer(hotkeyPrefix)));
end;

function TIGPStringFormat.GetHotkeyPrefix: TIGPHotkeyPrefix;
var HotkeyPrefix: Integer;
begin
  ErrorCheck( GdipGetStringFormatHotkeyPrefix(FNativeFormat, HotkeyPrefix));
  Result := TIGPHotkeyPrefix(HotkeyPrefix);
end;

function TIGPStringFormat.SetTabStops( firstTabOffset: Single; const tabStops : array of Single ) : TIGPStringFormat;
begin
  ErrorCheck( GdipSetStringFormatTabStops(FNativeFormat, firstTabOffset, Length( tabStops ), @tabStops[ 0 ]));
  Result := Self;
end;

function TIGPStringFormat.GetTabStopCount() : Integer;
begin
  ErrorCheck( GdipGetStringFormatTabStopCount(FNativeFormat, Result));
end;

function TIGPStringFormat.GetTabStops( out initialTabOffset : Single ) : TArray<Single>;
var
  count: Integer;
    
begin
  ErrorCheck( GdipGetStringFormatTabStopCount( FNativeFormat, count ));
  SetLength( Result, count );
  ErrorCheck( GdipGetStringFormatTabStops(FNativeFormat, count, @initialTabOffset, @Result[ 0 ] ));
end;

function TIGPStringFormat.GetTabStops() : TArray<Single>;
var
  initialTabOffset : Single;
    
begin
  Result := GetTabStops( initialTabOffset );
end;
  
function TIGPStringFormat.GetTabStopsProp() : TArray<Single>;
var
  initialTabOffset : Single;
    
begin
  Result := GetTabStops( initialTabOffset );
end;

function TIGPStringFormat.GetInitialTabOffset() : Single;
begin
  GetTabStops( Result );
end;

function TIGPStringFormat.SetDigitSubstitution(language: LANGID; substitute: TIGPStringDigitSubstitute) : TIGPStringFormat;
begin
  ErrorCheck( GdipSetStringFormatDigitSubstitution(FNativeFormat, language, substitute));
  Result := Self;
end;

function TIGPStringFormat.GetDigitSubstitutionLanguage() : LANGID;
begin
  ErrorCheck( GdipGetStringFormatDigitSubstitution(FNativeFormat, PUINT(@Result), NIL));
end;

function TIGPStringFormat.GetDigitSubstitutionMethod() : TIGPStringDigitSubstitute;
begin
  ErrorCheck( GdipGetStringFormatDigitSubstitution(FNativeFormat, NIL, @Result));
end;

function TIGPStringFormat.SetTrimming(trimming: TIGPStringTrimming) : TIGPStringFormat;
begin
  ErrorCheck( GdipSetStringFormatTrimming(FNativeFormat, trimming));
  Result := Self;
end;

procedure TIGPStringFormat.SetTrimmingProp(trimming: TIGPStringTrimming);
begin
  ErrorCheck( GdipSetStringFormatTrimming(FNativeFormat, trimming));
end;

function TIGPStringFormat.GetTrimming: TIGPStringTrimming;
begin
  ErrorCheck( GdipGetStringFormatTrimming(FNativeFormat, Result));
end;

function TIGPStringFormat.SetMeasurableCharacterRanges( const ranges : array of TIGPCharacterRange ) : TIGPStringFormat;
begin
  ErrorCheck( GdipSetStringFormatMeasurableCharacterRanges(FNativeFormat,
    Length( ranges ), @ranges[ 0 ] ));
  Result := Self;
end;

function TIGPStringFormat.GetMeasurableCharacterRangeCount: Integer;
begin
  ErrorCheck( GdipGetStringFormatMeasurableCharacterRangeCount(FNativeFormat, Result));
end;

procedure TIGPStringFormat.Assign(source: TIGPStringFormat);
begin
  assert(Assigned(source));
  GdipDeleteStringFormat(FNativeFormat);
  ErrorCheck( GdipCloneStringFormat(source.FNativeFormat, FNativeFormat));
end;

constructor TIGPStringFormat.CreateGdiPlus(clonedStringFormat: GpStringFormat; Dummy : Boolean);
begin
  FNativeFormat := clonedStringFormat;
end;

function TIGPStringFormat.GetNativeFormat() : GpStringFormat;
begin
  Result := FNativeFormat;
end;

// ---------------------------------------------------------------------------
//  TAdjustableArrowCap
// ---------------------------------------------------------------------------

constructor TIGPAdjustableArrowCap.Create(height, width: Single; isFilled: Boolean = True);
var cap: GpAdjustableArrowCap;
begin
  cap := NIL;
  ErrorCheck( GdipCreateAdjustableArrowCap(height, width, isFilled, cap));
  SetNativeCap(cap);
end;

function TIGPAdjustableArrowCap.SetHeight(height: Single) : TIGPAdjustableArrowCap;
begin
  ErrorCheck( GdipSetAdjustableArrowCapHeight(GpAdjustableArrowCap(FNativeCap), height));
  Result := Self;
end;

procedure TIGPAdjustableArrowCap.SetHeightProp(height: Single);
begin
  ErrorCheck( GdipSetAdjustableArrowCapHeight(GpAdjustableArrowCap(FNativeCap), height));
end;

function TIGPAdjustableArrowCap.GetHeight: Single;
begin
  ErrorCheck( GdipGetAdjustableArrowCapHeight(GpAdjustableArrowCap(FNativeCap), Result));
end;

procedure TIGPAdjustableArrowCap.SetWidthProp(width: Single);
begin
  ErrorCheck( GdipSetAdjustableArrowCapWidth(GpAdjustableArrowCap(FNativeCap), width));
end;

function TIGPAdjustableArrowCap.SetWidth(width: Single) : TIGPAdjustableArrowCap;
begin
  ErrorCheck( GdipSetAdjustableArrowCapWidth(GpAdjustableArrowCap(FNativeCap), width));
  Result := Self;
end;

function TIGPAdjustableArrowCap.GetWidth: Single;
begin
  ErrorCheck( GdipGetAdjustableArrowCapWidth(GpAdjustableArrowCap(FNativeCap), Result));
end;

procedure TIGPAdjustableArrowCap.SetMiddleInsetProp(middleInset: Single);
begin
  ErrorCheck( GdipSetAdjustableArrowCapMiddleInset(GpAdjustableArrowCap(FNativeCap), middleInset));
end;

function TIGPAdjustableArrowCap.SetMiddleInset(middleInset: Single) : TIGPAdjustableArrowCap;
begin
  ErrorCheck( GdipSetAdjustableArrowCapMiddleInset(GpAdjustableArrowCap(FNativeCap), middleInset));
  Result := Self;
end;

function TIGPAdjustableArrowCap.GetMiddleInset: Single;
begin
  ErrorCheck( GdipGetAdjustableArrowCapMiddleInset(
    GpAdjustableArrowCap(FNativeCap), Result));
end;

function TIGPAdjustableArrowCap.SetFillState(isFilled: Boolean) : TIGPAdjustableArrowCap;
begin
  ErrorCheck( GdipSetAdjustableArrowCapFillState(
    GpAdjustableArrowCap(FNativeCap), isFilled));
      
  Result := Self;
end;

function TIGPAdjustableArrowCap.IsFilled() : Boolean;
var
  AValue : BOOL;

begin
  ErrorCheck( GdipGetAdjustableArrowCapFillState(
    GpAdjustableArrowCap(FNativeCap), AValue ));

  Result := AValue;
end;

(**************************************************************************\
*
*   GDI+ Metafile class
*
\**************************************************************************)

  // Playback a metafile from a HMETAFILE
  // If deleteWmf is True, then when the metafile is deleted,
  // the hWmf will also be deleted.  Otherwise, it won't be.

constructor TIGPMetafile.Create(hWmf: HMETAFILE;
  var wmfPlaceableFileHeader: TIGPWmfPlaceableFileHeader; deleteWmf: Boolean = False);
var
  metafile: GpMetafile;
begin
  metafile := NIL;
  ErrorCheck( GdipCreateMetafileFromWmf(hWmf, deleteWmf, @wmfPlaceableFileHeader, metafile));
  SetNativeImage(metafile);
end;

  // Playback a metafile from a HENHMETAFILE
  // If deleteEmf is True, then when the metafile is deleted,
  // the hEmf will also be deleted.  Otherwise, it won't be.

constructor TIGPMetafile.Create(hEmf: HENHMETAFILE; deleteEmf: Boolean = False);
var
  metafile: GpMetafile;
begin
  metafile := NIL;
  ErrorCheck( GdipCreateMetafileFromEmf(hEmf, deleteEmf, metafile));
  SetNativeImage(metafile);
end;

constructor TIGPMetafile.Create(filename: WideString);
var
  metafile: GpMetafile;
begin
  metafile := NIL;
  ErrorCheck( GdipCreateMetafileFromFile(PWideChar(filename), metafile));
  SetNativeImage(metafile);
end;

  // Playback a WMF metafile from a file.

constructor TIGPMetafile.Create(filename: Widestring; var wmfPlaceableFileHeader: TIGPWmfPlaceableFileHeader);
var
  metafile: GpMetafile;
begin
  metafile := NIL;
  ErrorCheck( GdipCreateMetafileFromWmfFile(PWideChar(filename), @wmfPlaceableFileHeader, metafile));
  SetNativeImage(metafile);
end;

constructor TIGPMetafile.Create(stream: IStream);
var
  metafile: GpMetafile;
begin
  metafile := NIL;
  ErrorCheck( GdipCreateMetafileFromStream(stream, metafile));
  SetNativeImage(metafile);
end;

  // Record a metafile to memory.

constructor TIGPMetafile.Create(referenceHdc: HDC; type_: TIGPEmfType = EmfTypeEmfPlusDual;
  description: PWCHAR = NIL);
var
  metafile: GpMetafile;
begin
  metafile := NIL;
  ErrorCheck( GdipRecordMetafile(referenceHdc, type_, NIL, MetafileFrameUnitGdi,
     description, metafile));
  SetNativeImage(metafile);
end;

  // Record a metafile to memory.

constructor TIGPMetafile.Create(referenceHdc: HDC; const frameRect: TIGPRectF;
   frameUnit: TIGPMetafileFrameUnit = MetafileFrameUnitGdi;
   type_: TIGPEmfType = EmfTypeEmfPlusDual; description: PWCHAR = NIL);
var metafile: GpMetafile;
begin
  metafile := NIL;
  ErrorCheck( GdipRecordMetafile(referenceHdc, type_, @frameRect, frameUnit,
    description, metafile));
  SetNativeImage(metafile);
end;

  // Record a metafile to memory.

constructor TIGPMetafile.Create(referenceHdc: HDC; const frameRect: TIGPRect;
  frameUnit: TIGPMetafileFrameUnit = MetafileFrameUnitGdi;
  type_: TIGPEmfType = EmfTypeEmfPlusDual; description: PWCHAR = NIL);
var
  metafile: GpMetafile;
begin
  metafile := NIL;
  ErrorCheck( GdipRecordMetafileI(referenceHdc, type_, @frameRect, frameUnit,
    description, metafile));
  SetNativeImage(metafile);
end;

constructor TIGPMetafile.Create(fileName: WideString; referenceHdc: HDC;
  type_: TIGPEmfType = EmfTypeEmfPlusDual; description: PWCHAR = NIL);
var
  metafile: GpMetafile;
begin
  metafile := NIL;
  ErrorCheck( GdipRecordMetafileFileName(PWideChar(fileName),
    referenceHdc, type_, NIL, MetafileFrameUnitGdi, description, metafile));
  SetNativeImage(metafile);
end;

constructor TIGPMetafile.Create(fileName: WideString; referenceHdc: HDC; const frameRect: TIGPRectF;
  frameUnit: TIGPMetafileFrameUnit = MetafileFrameUnitGdi; type_: TIGPEmfType = EmfTypeEmfPlusDual;
  description: PWCHAR = NIL);
var
  metafile: GpMetafile;
begin
  metafile := NIL;
  ErrorCheck( GdipRecordMetafileFileName(PWideChar(fileName), referenceHdc,
    type_, @frameRect, frameUnit, description, metafile));
  SetNativeImage(metafile);
end;

constructor TIGPMetafile.Create(fileName: WideString; referenceHdc: HDC; frameRect: TIGPRect;
  frameUnit: TIGPMetafileFrameUnit = MetafileFrameUnitGdi; type_: TIGPEmfType = EmfTypeEmfPlusDual;
  description: PWCHAR = NIL);
var
  metafile: GpMetafile;
begin
  metafile := NIL;
  ErrorCheck( GdipRecordMetafileFileNameI(PWideChar(fileName),
    referenceHdc, type_, @frameRect, frameUnit, description, metafile));
  SetNativeImage(metafile);
end;

constructor TIGPMetafile.Create(stream: IStream; referenceHdc: HDC;
  type_: TIGPEmfType = EmfTypeEmfPlusDual; description: PWCHAR = NIL);
var
  metafile: GpMetafile;
begin
  metafile := NIL;
  ErrorCheck( GdipRecordMetafileStream(stream, referenceHdc, type_, NIL,
    MetafileFrameUnitGdi, description, metafile));
  SetNativeImage(metafile);
end;

constructor TIGPMetafile.Create(stream: IStream; referenceHdc: HDC; const frameRect: TIGPRectF;
  frameUnit: TIGPMetafileFrameUnit = MetafileFrameUnitGdi; type_: TIGPEmfType = EmfTypeEmfPlusDual;
  description: PWCHAR = NIL);
var
  metafile: GpMetafile;
begin
  metafile := NIL;
  ErrorCheck( GdipRecordMetafileStream(stream, referenceHdc, type_,
    @frameRect, frameUnit, description, metafile));
  SetNativeImage(metafile);
end;

constructor TIGPMetafile.Create(stream : IStream; referenceHdc : HDC; frameRect : TIGPRect;
  frameUnit : TIGPMetafileFrameUnit = MetafileFrameUnitGdi; type_ : TIGPEmfType = EmfTypeEmfPlusDual;
  description : PWCHAR = NIL);
var
  metafile: GpMetafile;
begin
  metafile := NIL;
  ErrorCheck( GdipRecordMetafileStreamI(stream, referenceHdc, type_,
    @frameRect, frameUnit, description, metafile));
  SetNativeImage(metafile);
end;

class function TIGPMetafile.GetMetafileHeader(hWmf: HMETAFILE; var wmfPlaceableFileHeader: TIGPWmfPlaceableFileHeader) : IGPMetafileHeader;
var
  header : TIGPMetafileHeader;

begin
  header := TIGPMetafileHeader.Create();
  ErrorCheck( GdipGetMetafileHeaderFromWmf(hWmf, @wmfPlaceableFileHeader, @header.FType ));
  Result := header;
end;

class function TIGPMetafile.GetMetafileHeader(hEmf: HENHMETAFILE) : IGPMetafileHeader;
var
  header : TIGPMetafileHeader;

begin
  header := TIGPMetafileHeader.Create();
  ErrorCheck( GdipGetMetafileHeaderFromEmf(hEmf, @header.FType ));
  Result := header;
end;

class function TIGPMetafile.GetMetafileHeader(filename: WideString) : IGPMetafileHeader;
var
  header : TIGPMetafileHeader;

begin
  header := TIGPMetafileHeader.Create();
  ErrorCheck( GdipGetMetafileHeaderFromFile(PWideChar(filename), @header.FType ));
  Result := header;
end;

class function TIGPMetafile.GetMetafileHeader(stream: IStream) : IGPMetafileHeader;
var
  header : TIGPMetafileHeader;

begin
  header := TIGPMetafileHeader.Create();
  ErrorCheck( GdipGetMetafileHeaderFromStream(stream, @header.FType ));
  Result := header;
end;

function TIGPMetafile.GetMetafileHeader() : IGPMetafileHeader;
var
  header : TIGPMetafileHeader;

begin
  header := TIGPMetafileHeader.Create();
  ErrorCheck( GdipGetMetafileHeaderFromMetafile(GpMetafile(FNativeImage),
    @header.FType ));
  Result := header;
end;

  // Once this method is called, the Metafile object is in an invalid state
  // and can no longer be used.  It is the responsiblity of the caller to
  // invoke DeleteEnhMetaFile to delete this hEmf.

function TIGPMetafile.GetHENHMETAFILE() : HENHMETAFILE;
var
  AMeta : GPMETAFILE;

begin
  AMeta := GpMetafile(FNativeImage);
  ErrorCheck( GdipGetHemfFromMetafile( AMeta, Result ));
end;

  // Used in conjuction with Graphics::EnumerateMetafile to play an EMF+
  // The data must be DWORD aligned if it's an EMF or EMF+.  It must be
  // WORD aligned if it's a WMF.

function TIGPMetafile.PlayRecord(recordType: TIGPEmfPlusRecordType; flags, dataSize: Cardinal;
  data: PBYTE) : TIGPMetafile;
begin
  ErrorCheck( GdipPlayMetafileRecord(GpMetafile(FNativeImage),
    recordType, flags, dataSize, data));

  Result := Self;
end;

  // If you're using a printer HDC for the metafile, but you want the
  // metafile rasterized at screen resolution, then use this API to set
  // the rasterization dpi of the metafile to the screen resolution,
  // e.g. 96 dpi or 120 dpi.

function TIGPMetafile.SetDownLevelRasterizationLimit(metafileRasterizationLimitDpi: Cardinal) : TIGPMetafile;
begin
  ErrorCheck( GdipSetMetafileDownLevelRasterizationLimit(
    GpMetafile(FNativeImage), metafileRasterizationLimitDpi));

  Result := Self;
end;

procedure TIGPMetafile.SetDownLevelRasterizationLimitProp(metafileRasterizationLimitDpi: Cardinal);
begin
  ErrorCheck( GdipSetMetafileDownLevelRasterizationLimit(
    GpMetafile(FNativeImage), metafileRasterizationLimitDpi));

end;

function TIGPMetafile.GetDownLevelRasterizationLimit: Cardinal;
var metafileRasterizationLimitDpi: Cardinal;
begin
  metafileRasterizationLimitDpi := 0;
  ErrorCheck( GdipGetMetafileDownLevelRasterizationLimit(
    GpMetafile(FNativeImage), metafileRasterizationLimitDpi));
  Result := metafileRasterizationLimitDpi;
end;

function TIGPMetafile.EmfToWmfBits(hemf: HENHMETAFILE; cbData16: Cardinal; pData16: PBYTE;
  iMapMode: Integer = MM_ANISOTROPIC; eFlags: TIGPEmfToWmfBitsFlags = EmfToWmfBitsFlagsDefault) : Cardinal;
begin
  Result := GdipEmfToWmfBits(hemf, cbData16, pData16, iMapMode, Integer(eFlags));
end;

constructor TIGPMetafile.Create();
begin
  SetNativeImage(NIL);
end;

(**************************************************************************\
*
*   GDI+ Codec Image APIs
*
\**************************************************************************)

//--------------------------------------------------------------------------
// Codec Management APIs
//--------------------------------------------------------------------------

function GetImageDecodersSize(out numDecoders, size: Cardinal) : TIGPStatus;
begin
  Result := GdipGetImageDecodersSize(numDecoders, size);
end;

function GetImageDecoders() : TArray<TIGPImageCodecInfo>;
var
  numDecoders, size: Cardinal;
  AStatus : TIGPStatus;
    
begin
  AStatus := GdipGetImageDecodersSize(numDecoders, size);
  if( AStatus <> Ok ) then
    raise EGPException.Create( GetStatus( AStatus ));

  SetLength( Result, numDecoders );
  AStatus := GdipGetImageDecoders( numDecoders, size, @Result[ 0 ] );
  if( AStatus <> Ok ) then
    raise EGPException.Create( GetStatus( AStatus ));

end;

function GetImageEncodersSize(out numEncoders, size: Cardinal) : TIGPStatus;
begin
  Result := GdipGetImageEncodersSize(numEncoders, size);
end;

function GetImageEncoders() : TArray<TIGPImageCodecInfo>;
var
  numEncoders, size : Cardinal;
  AStatus : TIGPStatus;

begin
  AStatus := GdipGetImageEncodersSize(numEncoders, size);
  if( AStatus <> Ok ) then
    raise EGPException.Create( GetStatus( AStatus ));

  SetLength( Result, numEncoders );
  AStatus := GdipGetImageEncoders( numEncoders, size, @Result[ 0 ] );
  if( AStatus <> Ok ) then
    raise EGPException.Create( GetStatus( AStatus ));
      
end;

function GetEncoderClsid( format : String; var pClsid : TCLSID ) : Boolean;
var
  num  : UINT; // number of image encoders
  size : UINT; // size of the image encoder array in bytes
  aImageCodecInfo : PGPImageCodecInfo;
  j    : UINT;
   
begin
  num := 0;
  size := 0;

  Result := False;

//    aImageCodecInfo := NIL;

  GetImageEncodersSize( num, size );
  if(size = 0) then
    Exit;  // Failure

  GetMem( aImageCodecInfo, size );
  if( aImageCodecInfo = NIL ) then
    Exit;  // Failure

//    GdipGetImageEncoders(numEncoders, size, @Result[ 0 ] )
  GdipGetImageEncoders(num, size, aImageCodecInfo);

  format := LowerCase( format );

  for j := 0 to num - 1 do
    begin
    if( LowerCase( PGPImageCodecInfo( PAnsiChar( aImageCodecInfo ) + j * SizeOf( TIGPImageCodecInfo )).MimeType ) = format ) then
      begin
      pClsid := PGPImageCodecInfo( PAnsiChar( aImageCodecInfo ) + j * SizeOf( TIGPImageCodecInfo )).Clsid;
      FreeMem( aImageCodecInfo, size );
      Result := True;
      Exit;
      end;
    end;

  FreeMem( aImageCodecInfo, size );
end;

(**************************************************************************\
*
*   GDI+ Region class implementation
*
\**************************************************************************)

constructor TIGPRegion.Create();
var
  region: GpRegion;
begin
  region := NIL;
  ErrorCheck( GdipCreateRegion(region) );
  SetNativeRegion(region);
end;

constructor TIGPRegion.Create( const rect: TIGPRectF );
var
  region: GpRegion;
begin
  region := NIL;
  ErrorCheck( GdipCreateRegionRect(@rect, region));
  SetNativeRegion(region);
end;

constructor TIGPRegion.Create( const rect: TIGPRect );
var
  region: GpRegion;
begin
  region := NIL;
  ErrorCheck( GdipCreateRegionRectI(@rect, region));
  SetNativeRegion(region);
end;

constructor TIGPRegion.Create(path: IGPGraphicsPath);
var
  region: GpRegion;
begin
  region := NIL;
  ErrorCheck( GdipCreateRegionPath(path.GetNativePath(), region));
  SetNativeRegion(region);
end;

constructor TIGPRegion.Create( const regionData: array of BYTE );
var
  region: GpRegion;
begin
  region := NIL;
  ErrorCheck( GdipCreateRegionRgnData( @regionData[ 0 ], Length( regionData ), region));
  SetNativeRegion(region);
end;

constructor TIGPRegion.Create(hRgn: HRGN);
var
  region: GpRegion;
begin
  region := NIL;
  ErrorCheck( GdipCreateRegionHrgn(hRgn, region));
  SetNativeRegion(region);
end;

class function TIGPRegion.FromHRGN(hRgn: HRGN) : TIGPRegion;
var
  region: GpRegion;
begin
  region := NIL;
  if (GdipCreateRegionHrgn(hRgn, region) = Ok) then
    begin
    Result := TIGPRegion.CreateGdiPlus(region, False);
    if (Result = NIL) then
      GdipDeleteRegion(region);

    exit;
    end
      
  else
    Result := NIL;
      
end;

destructor TIGPRegion.Destroy();
begin
  GdipDeleteRegion(FNativeRegion);
end;

function TIGPRegion.Clone: TIGPRegion;
var region: GpRegion;
begin
  region := NIL;
  ErrorCheck( GdipCloneRegion(FNativeRegion, region));
  Result := TIGPRegion.CreateGdiPlus(region, False);
end;

function TIGPRegion.MakeInfinite() : TIGPRegion;
begin
  ErrorCheck( GdipSetInfinite(FNativeRegion));
  Result := Self;
end;

function TIGPRegion.MakeEmpty() : TIGPRegion;
begin
  ErrorCheck( GdipSetEmpty(FNativeRegion));
  Result := Self;
end;

// Get the size of the buffer needed for the GetData method
function TIGPRegion.GetDataSize() : Cardinal;
begin
  ErrorCheck( GdipGetRegionDataSize(FNativeRegion, Result ));
end;


  // buffer     - where to put the data
  // bufferSize - how big the buffer is (should be at least as big as GetDataSize())
  // sizeFilled - if not NIL, this is an OUT param that says how many bytes
  //              of data were written to the buffer.

function TIGPRegion.GetData() : TArray<Byte>;
var
  bufferSize : Cardinal;
    
begin
  ErrorCheck( GdipGetRegionDataSize( FNativeRegion, bufferSize ));
  SetLength( Result, bufferSize ); 
  ErrorCheck( GdipGetRegionData( FNativeRegion, @Result[ 0 ], bufferSize, NIL ));
end;

function TIGPRegion.Intersect(const rect: TIGPRect) : TIGPRegion;
begin
  ErrorCheck( GdipCombineRegionRectI(FNativeRegion, @rect, CombineModeIntersect));
  Result := Self;
end;

function TIGPRegion.IntersectF(const rect: TIGPRectF) : TIGPRegion;
begin
  ErrorCheck( GdipCombineRegionRect(FNativeRegion, @rect, CombineModeIntersect));
  Result := Self;
end;

function TIGPRegion.Intersect(path: IGPGraphicsPath) : TIGPRegion;
begin
  ErrorCheck( GdipCombineRegionPath(FNativeRegion, path.GetNativePath(),
    CombineModeIntersect));
      
  Result := Self;
end;

function TIGPRegion.Intersect(region: IGPRegion) : TIGPRegion;
begin
  ErrorCheck( GdipCombineRegionRegion(FNativeRegion, region.GetNativeRegion(),
    CombineModeIntersect));
      
  Result := Self;
end;

function TIGPRegion.Union(const rect: TIGPRect) : TIGPRegion;
begin
  ErrorCheck( GdipCombineRegionRectI(FNativeRegion, @rect, CombineModeUnion));
  Result := Self;
end;

function TIGPRegion.UnionF(const rect: TIGPRectF) : TIGPRegion;
begin
  ErrorCheck( GdipCombineRegionRect(FNativeRegion, @rect, CombineModeUnion));
  Result := Self;
end;

function TIGPRegion.Union(path: IGPGraphicsPath) : TIGPRegion;
begin
  ErrorCheck( GdipCombineRegionPath(FNativeRegion, path.GetNativePath(), CombineModeUnion));
  Result := Self;
end;

function TIGPRegion.Union(region: IGPRegion) : TIGPRegion;
begin
  ErrorCheck( GdipCombineRegionRegion(FNativeRegion, region.GetNativeRegion(),
    CombineModeUnion));
      
  Result := Self;
end;

function TIGPRegion.XorRegion(const rect: TIGPRect) : TIGPRegion;
begin
  ErrorCheck( GdipCombineRegionRectI(FNativeRegion, @rect, CombineModeXor));
  Result := Self;
end;

function TIGPRegion.XorRegionF(const rect: TIGPRectF) : TIGPRegion;
begin
  ErrorCheck( GdipCombineRegionRect(FNativeRegion, @rect, CombineModeXor));
  Result := Self;
end;

function TIGPRegion.XorRegion(path: IGPGraphicsPath) : TIGPRegion;
begin
  ErrorCheck( GdipCombineRegionPath(FNativeRegion, path.GetNativePath(), CombineModeXor));
  Result := Self;
end;

function TIGPRegion.XorRegion(region: IGPRegion) : TIGPRegion;
begin
  ErrorCheck( GdipCombineRegionRegion(FNativeRegion, region.GetNativeRegion(),
    CombineModeXor));
      
  Result := Self;
end;

function TIGPRegion.Exclude(const rect: TIGPRect) : TIGPRegion;
begin
  ErrorCheck( GdipCombineRegionRectI(FNativeRegion, @rect, CombineModeExclude));
  Result := Self;
end;

function TIGPRegion.ExcludeF(const rect: TIGPRectF) : TIGPRegion;
begin
  ErrorCheck( GdipCombineRegionRect(FNativeRegion, @rect, CombineModeExclude));
  Result := Self;
end;

function TIGPRegion.Exclude(path: IGPGraphicsPath) : TIGPRegion;
begin
  ErrorCheck( GdipCombineRegionPath(FNativeRegion, path.GetNativePath(), CombineModeExclude));
  Result := Self;
end;

function TIGPRegion.Exclude(region: IGPRegion) : TIGPRegion;
begin
  ErrorCheck( GdipCombineRegionRegion(FNativeRegion,
                                             region.GetNativeRegion(),
                                                       CombineModeExclude));
                                                         
  Result := Self;
end;

function TIGPRegion.Complement(const rect: TIGPRect) : TIGPRegion;
begin
  ErrorCheck( GdipCombineRegionRectI(FNativeRegion, @rect, CombineModeComplement));
  Result := Self;
end;

function TIGPRegion.ComplementF(const rect: TIGPRectF) : TIGPRegion;
begin
  ErrorCheck( GdipCombineRegionRect(FNativeRegion, @rect, CombineModeComplement));
  Result := Self;
end;

function TIGPRegion.Complement(path: IGPGraphicsPath) : TIGPRegion;
begin
  ErrorCheck( GdipCombineRegionPath(FNativeRegion,
                                              path.GetNativePath(),
                                              CombineModeComplement));
                                                
  Result := Self;
end;

function TIGPRegion.Complement(region: IGPRegion) : TIGPRegion;
begin
  ErrorCheck( GdipCombineRegionRegion(FNativeRegion,
                                                region.GetNativeRegion(),
                                                       CombineModeComplement));
                                                         
  Result := Self;
end;

function TIGPRegion.TranslateF(dx, dy: Single) : TIGPRegion;
begin
  ErrorCheck( GdipTranslateRegion(FNativeRegion, dx, dy));
  Result := Self;
end;

function TIGPRegion.Translate(dx, dy: Integer) : TIGPRegion;
begin
  ErrorCheck( GdipTranslateRegionI(FNativeRegion, dx, dy));
  Result := Self;
end;

function TIGPRegion.Transform(matrix: IGPMatrix) : TIGPRegion;
begin
  ErrorCheck( GdipTransformRegion(FNativeRegion,
                                                   matrix.GetNativeMatrix()));
                                                     
  Result := Self;
end;

function TIGPRegion.GetBounds( g: IGPGraphics ) : TIGPRect;
begin
  ErrorCheck( GdipGetRegionBoundsI(FNativeRegion,
                                              g.GetNativeGraphics(),
                                              @Result));
end;

function TIGPRegion.GetBoundsF( g: IGPGraphics ) : TIGPRectF;
begin
  ErrorCheck( GdipGetRegionBounds(FNativeRegion,
                                              g.GetNativeGraphics(),
                                              @Result));
end;

function TIGPRegion.GetHRGN(g: IGPGraphics) : HRGN;
begin
  ErrorCheck( GdipGetRegionHRgn(FNativeRegion, g.GetNativeGraphics(), Result));
end;

function TIGPRegion.IsEmpty(g: IGPGraphics) : Boolean;
var
  booln : BOOL;

begin
  booln := False;
  ErrorCheck( GdipIsEmptyRegion(FNativeRegion, g.GetNativeGraphics(), booln));
  Result := booln;
end;

function TIGPRegion.IsInfinite(g: IGPGraphics) : Boolean ;
var booln: BOOL;
begin
  booln := False;
  ErrorCheck( GdipIsInfiniteRegion(FNativeRegion, g.GetNativeGraphics(), booln));
  Result := booln;
end;

function TIGPRegion.IsVisible(x, y: Integer; g: IGPGraphics = NIL) : Boolean;
var
  booln: BOOL;
  GPX: GpGraphics;
begin
  booln := False;
  if( Assigned(g)) then
    gpx := g.GetNativeGraphics()

  else
    gpx := NIL;
      
  ErrorCheck( GdipIsVisibleRegionPointI(FNativeRegion, X, Y, gpx, booln));
  Result := booln;
end;

function TIGPRegion.IsVisible(const point: TPoint; g: IGPGraphics = NIL) : Boolean;
var
  booln: BOOL;
  GPX: GpGraphics;
begin
  booln := False;
  if( Assigned(g)) then
    gpx := g.GetNativeGraphics()

  else
    gpx := NIL;
      
  ErrorCheck( GdipIsVisibleRegionPointI(FNativeRegion, point.X, point.Y, gpx, booln));
  Result := booln;
end;

function TIGPRegion.IsVisibleF(x, y: Single; g: IGPGraphics = NIL) : Boolean;
var
  booln: BOOL;
  GPX: GpGraphics;
begin
  booln := False;
  if( Assigned(g)) then
    gpx := g.GetNativeGraphics()

  else
    gpx := NIL;
      
  ErrorCheck( GdipIsVisibleRegionPoint(FNativeRegion, X, Y, gpx, booln));
  Result := booln;
end;

function TIGPRegion.IsVisibleF(const point: TPointF; g: IGPGraphics = NIL) : Boolean;
var
  booln: BOOL;
  GPX: GpGraphics;
begin
  booln := False;
  if( Assigned(g)) then
    gpx := g.GetNativeGraphics()

  else
    gpx := NIL;
      
  ErrorCheck( GdipIsVisibleRegionPoint(FNativeRegion, point.X, point.Y, gpx, booln));
  Result := booln;
end;

function TIGPRegion.IsVisible(x, y, width, height: Integer; g: IGPGraphics) : Boolean;
var
  booln: BOOL;
  GPX: GpGraphics;
begin
  booln := False;
  if( Assigned(g)) then
    gpx := g.GetNativeGraphics()

  else
    gpx := NIL;
      
  ErrorCheck( GdipIsVisibleRegionRectI(FNativeRegion,
                                                X,
                                                Y,
                                                Width,
                                                Height,
                                                gpx,
                                                booln));
  Result := booln;
end;

function TIGPRegion.IsVisible(const rect: TIGPRect; g: IGPGraphics = NIL) : Boolean;
var
  booln: BOOL;
  GPX: GpGraphics;
begin
  booln := False;
  if( Assigned(g)) then
    gpx := g.GetNativeGraphics()

  else
    gpx := NIL;
      
  ErrorCheck( GdipIsVisibleRegionRectI(FNativeRegion,
                                                rect.X,
                                                rect.Y,
                                                rect.Width,
                                                rect.Height,
                                                gpx,
                                                booln));
  Result := booln;
end;

function TIGPRegion.IsVisibleF(x, y, width, height: Single; g: IGPGraphics = NIL) : Boolean;
var
  booln: BOOL;
  GPX: GpGraphics;
begin
  booln := False;
  if( Assigned(g)) then
    gpx := g.GetNativeGraphics()

  else
    gpx := NIL;
      
  ErrorCheck( GdipIsVisibleRegionRect(FNativeRegion, X,
                                                  Y, Width,
                                                  Height,
                                                  gpx,
                                                  booln));
  Result := booln;
end;

function TIGPRegion.IsVisibleF(const rect: TIGPRectF; g: IGPGraphics = NIL) : Boolean;
var
  booln: BOOL;
  GPX: GpGraphics;

begin
  booln := False;
  if( Assigned(g)) then
    gpx := g.GetNativeGraphics()

  else
    gpx := NIL;
      
  ErrorCheck( GdipIsVisibleRegionRect(FNativeRegion, rect.X,
                                                  rect.Y, rect.Width,
                                                  rect.Height,
                                                  gpx,
                                                  booln));
  Result := booln;
end;

function TIGPRegion.EqualsRegion(region: IGPRegion; g: IGPGraphics) : Boolean;
var
  booln: BOOL;
begin

  booln := False;
  ErrorCheck( GdipIsEqualRegion(FNativeRegion,
                                            region.GetNativeRegion(),
                                            g.GetNativeGraphics(),
                                            booln));
  Result := booln;
end;

function TIGPRegion.GetRegionScansCount(matrix: IGPMatrix) : Cardinal;
var
  count: Cardinal;

begin
  count := 0;
  ErrorCheck( GdipGetRegionScansCount(FNativeRegion, count, matrix.GetNativeMatrix()));
  Result := count;
end;

// If rects is NIL, Result := the count of rects in the region.
// Otherwise, assume rects is big enough to hold all the region rects
// and fill them in and Result := the number of rects filled in.
// The rects are Result :=ed in the units specified by the matrix
// (which is typically a world-to-device transform).
// Note that the number of rects Result :=ed can vary, depending on the
// matrix that is used.

function TIGPRegion.GetRegionScansF( matrix: IGPMatrix ) : TArray<TIGPRectF>;
var
  count : Cardinal;

begin
  ErrorCheck( GdipGetRegionScansCount( FNativeRegion, count, matrix.GetNativeMatrix()));
  SetLength( Result, count );
  ErrorCheck( GdipGetRegionScans(FNativeRegion,
                                        @Result[ 0 ],
                                        Integer( count ),
                                        matrix.GetNativeMatrix()));
end;

function TIGPRegion.GetRegionScans( matrix: IGPMatrix ) : TArray<TIGPRect>;
var
  count : Cardinal;

begin
  ErrorCheck( GdipGetRegionScansCount( FNativeRegion, count, matrix.GetNativeMatrix()));
  SetLength( Result, count );
  ErrorCheck( GdipGetRegionScansI(FNativeRegion,
                                        @Result[ 0 ],
                                        Integer( count ),
                                        matrix.GetNativeMatrix()));
end;

constructor TIGPRegion.CreateGdiPlus(nativeRegion: GpRegion; Dummy : Boolean);
begin
  SetNativeRegion(nativeRegion);
end;

procedure TIGPRegion.SetNativeRegion(nativeRegion: GpRegion);
begin
  FNativeRegion := nativeRegion;
end;

function TIGPRegion.GetNativeRegion() : GpRegion;
begin
  Result := FNativeRegion;
end;

(**************************************************************************\
*
*   GDI+ CustomLineCap APIs
*
\**************************************************************************)

constructor TIGPCustomLineCap.Create(fillPath, strokePath: IGPGraphicsPath;
                baseCap: TIGPLineCap = LineCapFlat; baseInset: Single = 0);
var
  nativeFillPath, nativeStrokePath: GpPath;
    
begin
  FNativeCap := NIL;
  nativeFillPath := NIL;
  nativeStrokePath := NIL;

  if( Assigned(fillPath)) then
    nativeFillPath := fillPath.GetNativePath();

  if( Assigned(strokePath)) then
    nativeStrokePath := strokePath.GetNativePath();

  ErrorCheck( GdipCreateCustomLineCap(nativeFillPath, nativeStrokePath,
                  baseCap, baseInset, FNativeCap));
end;

destructor TIGPCustomLineCap.Destroy();
begin
  GdipDeleteCustomLineCap(FNativeCap);
end;

function TIGPCustomLineCap.Clone: TIGPCustomLineCap;
var newNativeLineCap: GpCustomLineCap;
begin
  newNativeLineCap := NIL;
  ErrorCheck( GdipCloneCustomLineCap(FNativeCap, newNativeLineCap));

  Result := TIGPCustomLineCap.CreateGdiPlus(newNativeLineCap, False);
  if (Result = NIL) then
    ErrorCheck( GdipDeleteCustomLineCap(newNativeLineCap));
       
end;

// This changes both the start and end cap.
function TIGPCustomLineCap.SetStrokeCap(strokeCap: TIGPLineCap) : TIGPCustomLineCap;
begin
  Result := SetStrokeCaps(strokeCap, strokeCap);
end;

function TIGPCustomLineCap.SetStrokeCaps(startCap, endCap: TIGPLineCap) : TIGPCustomLineCap;
begin
  ErrorCheck( GdipSetCustomLineCapStrokeCaps(FNativeCap, startCap, endCap));
  Result := Self;
end;

function TIGPCustomLineCap.GetStrokeCaps(out startCap, endCap: TIGPLineCap) : TIGPCustomLineCap;
begin
  ErrorCheck( GdipGetCustomLineCapStrokeCaps(FNativeCap, startCap, endCap));
  Result := Self;
end;

function TIGPCustomLineCap.SetStrokeJoin(lineJoin: TIGPLineJoin) : TIGPCustomLineCap;
begin
  ErrorCheck( GdipSetCustomLineCapStrokeJoin(FNativeCap, lineJoin));
  Result := Self;
end;

procedure TIGPCustomLineCap.SetStrokeJoinProp(lineJoin: TIGPLineJoin);
begin
  ErrorCheck( GdipSetCustomLineCapStrokeJoin(FNativeCap, lineJoin));
end;

function TIGPCustomLineCap.GetStrokeJoin() : TIGPLineJoin;
begin
  ErrorCheck( GdipGetCustomLineCapStrokeJoin(FNativeCap, Result));
end;

function TIGPCustomLineCap.SetBaseCap(baseCap: TIGPLineCap) : TIGPCustomLineCap;
begin
  ErrorCheck( GdipSetCustomLineCapBaseCap(FNativeCap, baseCap));
  Result := Self;
end;

procedure TIGPCustomLineCap.SetBaseCapProp(baseCap: TIGPLineCap);
begin
  ErrorCheck( GdipSetCustomLineCapBaseCap(FNativeCap, baseCap));
end;

function TIGPCustomLineCap.GetBaseCap() : TIGPLineCap;
begin
  ErrorCheck( GdipGetCustomLineCapBaseCap(FNativeCap, Result));
end;

function TIGPCustomLineCap.SetBaseInset(inset: Single) : TIGPCustomLineCap;
begin
  ErrorCheck( GdipSetCustomLineCapBaseInset(FNativeCap, inset));
  Result := Self;
end;

procedure TIGPCustomLineCap.SetBaseInsetProp(inset: Single);
begin
  ErrorCheck( GdipSetCustomLineCapBaseInset(FNativeCap, inset));
end;

function TIGPCustomLineCap.GetBaseInset() : Single;
begin
  ErrorCheck( GdipGetCustomLineCapBaseInset(FNativeCap, Result));
end;

function TIGPCustomLineCap.SetWidthScale(widthScale: Single) : TIGPCustomLineCap;
begin
  ErrorCheck( GdipSetCustomLineCapWidthScale(FNativeCap, widthScale));
  Result := Self;
end;

procedure TIGPCustomLineCap.SetWidthScaleProp(widthScale: Single);
begin
  ErrorCheck( GdipSetCustomLineCapWidthScale(FNativeCap, widthScale));
end;

function TIGPCustomLineCap.GetWidthScale() : Single;
begin
  ErrorCheck( GdipGetCustomLineCapWidthScale(FNativeCap, Result));
end;

constructor TIGPCustomLineCap.Create();
begin
  FNativeCap := NIL;
end;

constructor TIGPCustomLineCap.CreateGdiPlus(nativeCap: GpCustomLineCap; Dummy : Boolean);
begin
  SetNativeCap(nativeCap);
end;

procedure TIGPCustomLineCap.SetNativeCap(nativeCap: GpCustomLineCap);
begin
  FNativeCap := nativeCap;
end;

function TIGPCustomLineCap.GetNativeCap() : GpCustomLineCap;
begin
  Result := FNativeCap;
end;

(**************************************************************************
*
* CachedBitmap class definition
*
*   GDI+ CachedBitmap is a representation of an accelerated drawing
*   that has restrictions on what operations are allowed in order
*   to accelerate the drawing to the destination.
*
**************************************************************************)

constructor TIGPCachedBitmap.Create(bitmap: IGPBitmap; graphics: IGPGraphics);
begin
  FNativeCachedBitmap := NIL;
  ErrorCheck( GdipCreateCachedBitmap(
      GpBitmap(bitmap.GetNativeImage()),
      graphics.GetNativeGraphics(),
      FNativeCachedBitmap));
end;

destructor TIGPCachedBitmap.Destroy();
begin
  GdipDeleteCachedBitmap(FNativeCachedBitmap);
end;

function TIGPCachedBitmap.GetNativeCachedBitmap() : GpCachedBitmap;
begin
  Result := FNativeCachedBitmap;
end;

(**************************************************************************\
*
*   GDI+ Pen class
*
\**************************************************************************)

//--------------------------------------------------------------------------
// Pen class
//--------------------------------------------------------------------------

constructor TIGPPen.Create(color: TAlphaColor; width: Single = 1.0);
var
  unit_ : TIGPUnit;

begin
  unit_ := UnitWorld;
  FNativePen := NIL;
  ErrorCheck( GdipCreatePen1(color, width, unit_, FNativePen) );
end;

constructor TIGPPen.Create(brush: IGPBrush; width: Single = 1.0);
var
  unit_ : TIGPUnit;

begin
  unit_ := UnitWorld;
  FNativePen := NIL;
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipCreatePen2(brush.GetNativeBrush(), width, unit_, FNativePen));
end;

destructor TIGPPen.Destroy();
begin
  GdipDeletePen(FNativePen);
end;

function TIGPPen.Clone() : TIGPPen;
var clonePen: GpPen;
begin
  clonePen := NIL;
  ErrorCheck( GdipClonePen(FNativePen, clonePen));
  Result := TIGPPen.CreateGdiPlus(clonePen, False);
end;

procedure TIGPPen.SetWidthProp(width: Single);
begin
  ErrorCheck( GdipSetPenWidth(FNativePen, width) );
end;

function TIGPPen.SetWidth(width: Single) : TIGPPen;
begin
  ErrorCheck( GdipSetPenWidth(FNativePen, width) );
  Result := Self;
end;

function TIGPPen.GetWidth() : Single;
begin
  ErrorCheck( GdipGetPenWidth(FNativePen, Result));
end;
    
// Set/get line caps: start, end, and dash
// Line cap and join APIs by using LineCap and LineJoin enums.

function TIGPPen.SetLineCap(startCap, endCap: TIGPLineCap; dashCap: TIGPDashCap) : TIGPPen;
begin
  ErrorCheck( GdipSetPenLineCap197819(FNativePen, startCap, endCap, dashCap));
  Result := Self;
end;

procedure TIGPPen.SetStartCapProp(startCap: TIGPLineCap);
begin
  ErrorCheck( GdipSetPenStartCap(FNativePen, startCap));
end;

function TIGPPen.SetStartCap(startCap: TIGPLineCap) : TIGPPen;
begin
  ErrorCheck( GdipSetPenStartCap(FNativePen, startCap));
  Result := Self;
end;

procedure TIGPPen.SetEndCapProp(endCap: TIGPLineCap);
begin
  ErrorCheck( GdipSetPenEndCap(FNativePen, endCap));
end;
  
function TIGPPen.SetEndCap(endCap: TIGPLineCap) : TIGPPen;
begin
  ErrorCheck( GdipSetPenEndCap(FNativePen, endCap));
  Result := Self;
end;

procedure TIGPPen.SetDashCapProp(dashCap: TIGPDashCap);
begin
  ErrorCheck( GdipSetPenDashCap197819(FNativePen, dashCap));
end;
  
function TIGPPen.SetDashCap(dashCap: TIGPDashCap) : TIGPPen;
begin
  ErrorCheck( GdipSetPenDashCap197819(FNativePen, dashCap));
  Result := Self;
end;

function TIGPPen.GetStartCap() : TIGPLineCap;
begin
  ErrorCheck( GdipGetPenStartCap(FNativePen, Result));
end;

function TIGPPen.GetEndCap: TIGPLineCap;
begin
  ErrorCheck( GdipGetPenEndCap(FNativePen, Result));
end;

function TIGPPen.GetDashCap: TIGPDashCap;
begin
  ErrorCheck( GdipGetPenDashCap197819(FNativePen, Result));
end;

procedure TIGPPen.SetLineJoinProp(lineJoin: TIGPLineJoin);
begin
  ErrorCheck( GdipSetPenLineJoin(FNativePen, lineJoin));
end;

function TIGPPen.SetLineJoin(lineJoin: TIGPLineJoin) : TIGPPen;
begin
  ErrorCheck( GdipSetPenLineJoin(FNativePen, lineJoin));
  Result := Self;
end;

function TIGPPen.GetLineJoin() : TIGPLineJoin;
begin
  ErrorCheck( GdipGetPenLineJoin(FNativePen, Result));
end;

procedure TIGPPen.SetCustomStartCapProp(customCap: IGPCustomLineCap);
begin
  SetCustomStartCap( customCap );
end;

function TIGPPen.SetCustomStartCap(customCap: IGPCustomLineCap) : TIGPPen;
var nativeCap: GpCustomLineCap;
begin
  nativeCap := NIL;
  if( Assigned(customCap)) then
    nativeCap := customCap.GetNativeCap();
      
  ErrorCheck( GdipSetPenCustomStartCap(FNativePen, nativeCap));
  Result := Self;
end;

function TIGPPen.GetCustomStartCap() : IGPCustomLineCap;
var
  ALineCap  : TIGPCustomLineCap;
    
begin
  ALineCap := TIGPCustomLineCap.Create();
  ErrorCheck( GdipGetPenCustomStartCap(FNativePen, ALineCap.FNativeCap ));
  Result := ALineCap;
end;

procedure TIGPPen.SetCustomEndCapProp(customCap: IGPCustomLineCap);
begin
  SetCustomEndCap( customCap );
end;
      
function TIGPPen.SetCustomEndCap(customCap: IGPCustomLineCap) : TIGPPen;
var
  nativeCap: GpCustomLineCap;
    
begin
  nativeCap := NIL;
  if( Assigned(customCap)) then
    nativeCap := customCap.GetNativeCap();
      
  ErrorCheck( GdipSetPenCustomEndCap(FNativePen, nativeCap));
  Result := Self;
end;

function TIGPPen.GetCustomEndCap() : IGPCustomLineCap;
var
  ALineCap  : TIGPCustomLineCap;
    
begin
  ALineCap := TIGPCustomLineCap.Create();
  ErrorCheck( GdipGetPenCustomEndCap(FNativePen, ALineCap.FNativeCap));
  Result := ALineCap;
end;

procedure TIGPPen.SetMiterLimitProp(miterLimit: Single);
begin
  ErrorCheck( GdipSetPenMiterLimit(FNativePen, miterLimit));
end;

function TIGPPen.SetMiterLimit(miterLimit: Single) : TIGPPen;
begin
  ErrorCheck( GdipSetPenMiterLimit(FNativePen, miterLimit));
  Result := Self;
end;

function TIGPPen.GetMiterLimit() : Single;
begin
  ErrorCheck( GdipGetPenMiterLimit(FNativePen, Result));
end;

procedure TIGPPen.SetAlignmentProp( penAlignment: TIGPPenAlignment);
begin
  ErrorCheck( GdipSetPenMode(FNativePen, penAlignment));
end;

function TIGPPen.SetAlignment( penAlignment: TIGPPenAlignment) : TIGPPen;
begin
  ErrorCheck( GdipSetPenMode(FNativePen, penAlignment));
  Result := Self;
end;

function TIGPPen.GetAlignment() : TIGPPenAlignment;
begin
  ErrorCheck( GdipGetPenMode(FNativePen, Result));
end;

procedure TIGPPen.SetTransformProp(matrix: IGPMatrix);
begin
  ErrorCheck( GdipSetPenTransform(FNativePen, matrix.GetNativeMatrix()));
end;

function TIGPPen.SetTransform(matrix: IGPMatrix) : TIGPPen;
begin
  ErrorCheck( GdipSetPenTransform(FNativePen, matrix.GetNativeMatrix()));
  Result := Self;
end;

function TIGPPen.GetTransform() : IGPMatrix;
begin
  Result := TIGPMatrix.Create();
  ErrorCheck( GdipGetPenTransform(FNativePen, Result.GetNativeMatrix()));
end;

function TIGPPen.ResetTransform() : TIGPPen;
begin
  ErrorCheck( GdipResetPenTransform(FNativePen));
  Result := Self;
end;

function TIGPPen.MultiplyTransform(matrix: IGPMatrix; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPPen;
begin
  ErrorCheck( GdipMultiplyPenTransform(FNativePen, matrix.GetNativeMatrix(), order));
  Result := Self;
end;

function TIGPPen.TranslateTransform(dx, dy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPPen;
begin
  ErrorCheck( GdipTranslatePenTransform(FNativePen, dx, dy, order));
  Result := Self;
end;

function TIGPPen.ScaleTransform(sx, sy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPPen;
begin
  ErrorCheck( GdipScalePenTransform(FNativePen, sx, sy, order));
  Result := Self;
end;

function TIGPPen.ScaleTransform(s: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPPen;
begin
  Result := ScaleTransform( s, s, order );
end;

function TIGPPen.RotateTransform(angle: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPPen;
begin
  ErrorCheck( GdipRotatePenTransform(FNativePen, angle, order));
  Result := Self;
end;

function TIGPPen.SetTransformT(matrix: IGPMatrix) : IGPTransformable;
begin
  ErrorCheck( GdipSetPenTransform(FNativePen, matrix.GetNativeMatrix()));
  Result := Self;
end;

function TIGPPen.ResetTransformT() : IGPTransformable;
begin
  ErrorCheck( GdipResetPenTransform(FNativePen));
  Result := Self;
end;

function TIGPPen.MultiplyTransformT(matrix: IGPMatrix; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipMultiplyPenTransform(FNativePen, matrix.GetNativeMatrix(), order));
  Result := Self;
end;

function TIGPPen.TranslateTransformT(dx, dy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipTranslatePenTransform(FNativePen, dx, dy, order));
  Result := Self;
end;

function TIGPPen.ScaleTransformT(sx, sy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipScalePenTransform(FNativePen, sx, sy, order));
  Result := Self;
end;

function TIGPPen.ScaleTransformXYT(s: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  Result := ScaleTransformT( s, s, order );
end;

function TIGPPen.RotateTransformT(angle: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipRotatePenTransform(FNativePen, angle, order));
  Result := Self;
end;

function TIGPPen.GetPenType() : TIGPPenType;
begin
  ErrorCheck( GdipGetPenFillType(FNativePen, Result));
end;

procedure TIGPPen.SetColorProp(color: TAlphaColor);
begin
  ErrorCheck( GdipSetPenColor(FNativePen, color));
end;

function TIGPPen.SetColor(color: TAlphaColor) : TIGPPen;
begin
  ErrorCheck( GdipSetPenColor(FNativePen, color));
  Result := Self;
end;

procedure TIGPPen.SetBrushProp(brush: IGPBrush);
begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipSetPenBrushFill( FNativePen, brush.GetNativeBrush() ));
end;

function TIGPPen.SetBrush(brush: IGPBrush) : TIGPPen;
begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipSetPenBrushFill( FNativePen, brush.GetNativeBrush() ));
  Result := Self;
end;

function TIGPPen.GetColor() : TAlphaColor;
begin
  if( GetPenType() <> PenTypeSolidColor ) then
    ErrorCheck( WrongState );

  ErrorCheck( GdipGetPenColor(FNativePen, Result ));
end;

function TIGPPen.GetBrush() : IGPBrush;
var
  type_: TIGPPenType;
  Brush: TIGPBrush;
  nativeBrush: GpBrush;

begin
  type_ := GetPenType();
  brush := NIL;

  case type_ of
     PenTypeSolidColor     : brush := TIGPSolidBrush.Create();
     PenTypeHatchFill      : brush := TIGPHatchBrush.Create();
     PenTypeTextureFill    : brush := TIGPTextureBrush.Create();
     PenTypePathGradient   : brush := TIGPBrush.Create();
     PenTypeLinearGradient : brush := TIGPLinearGradientBrush.Create();
   end;

   if( brush <> NIL ) then
     begin
     ErrorCheck( GdipGetPenBrushFill( FNativePen, nativeBrush ));
     brush.SetNativeBrush(nativeBrush);
     end;

   Result := brush;
end;

function TIGPPen.GetDashStyle() : TIGPDashStyle;
begin
  ErrorCheck( GdipGetPenDashStyle(FNativePen, Result));
end;

procedure TIGPPen.SetDashStyleProp(dashStyle: TIGPDashStyle);
begin
  ErrorCheck( GdipSetPenDashStyle(FNativePen, dashStyle));
end;

function TIGPPen.SetDashStyle(dashStyle: TIGPDashStyle) : TIGPPen;
begin
  ErrorCheck( GdipSetPenDashStyle(FNativePen, dashStyle));
  Result := Self;
end;

function TIGPPen.GetDashOffset() : Single;
begin
  ErrorCheck( GdipGetPenDashOffset(FNativePen, Result));
end;

procedure TIGPPen.SetDashOffsetProp( dashOffset: Single );
begin
  ErrorCheck( GdipSetPenDashOffset(FNativePen, dashOffset));
end;

function TIGPPen.SetDashOffset(dashOffset: Single) : TIGPPen;
begin
  ErrorCheck( GdipSetPenDashOffset(FNativePen, dashOffset));
  Result := Self;
end;

function TIGPPen.SetDashPattern( const dashArray: array of Single ) : TIGPPen;
var
  ALength : Integer;
  ADashArray: array of Single;
    
begin
  ALength := Length( dashArray );
  if( ALength and 1 > 0 ) then
    begin
    Inc( ALength );
    SetLength( ADashArray, ALength );
    Move( dashArray[ 0 ], ADashArray[ 0 ], SizeOf( dashArray )); 
    ADashArray[ ALength - 1 ] := 0.0001;
    ErrorCheck( GdipSetPenDashArray(FNativePen, @ADashArray[ 0 ], ALength ));
    end

  else
    ErrorCheck( GdipSetPenDashArray(FNativePen, @dashArray[ 0 ], ALength ));

  Result := Self;
end;

procedure TIGPPen.SetDashPatternProp( dashArray: TArray<Single> );
begin
  SetDashPattern( dashArray );
end;

function TIGPPen.GetDashPatternCount() : Integer;
begin
  ErrorCheck( GdipGetPenDashCount(FNativePen, Result));
end;

function TIGPPen.GetDashPattern() : TArray<Single>;
var
  count: Integer;
    
begin
  ErrorCheck( GdipGetPenDashCount( FNativePen, count ));
  SetLength( Result, count );
  ErrorCheck( GdipGetPenDashArray( FNativePen, @Result[ 0 ], count ));
end;

function TIGPPen.SetCompoundArray( const compoundArray: array of Single ) : TIGPPen;
begin
  ErrorCheck( GdipSetPenCompoundArray(FNativePen, @compoundArray[ 0 ], Length( compoundArray )));
  Result := Self;
end;

procedure TIGPPen.SetCompoundArrayProp( compoundArray: TArray<Single> );
begin
  ErrorCheck( GdipSetPenCompoundArray(FNativePen, @compoundArray[ 0 ], Length( compoundArray )));
end;
  
function TIGPPen.GetCompoundArrayCount() : Integer;
begin
  ErrorCheck( GdipGetPenCompoundCount(FNativePen, Result));
end;

function TIGPPen.GetCompoundArray() : TArray<Single>;
var
  count : Integer;
    
begin
  ErrorCheck( GdipGetPenCompoundCount(FNativePen, count));
  SetLength( Result, count );  
  ErrorCheck( GdipGetPenCompoundArray(FNativePen, @Result[ 0 ], count));
end;

constructor TIGPPen.CreateGdiPlus(nativePen: GpPen; Dummy : Boolean);
begin
  SetNativePen(nativePen);
end;

procedure TIGPPen.SetNativePen(nativePen: GpPen);
begin
  FNativePen := nativePen;
end;

function TIGPPen.GetNativePen() : GpPen;
begin
  Result := self.FNativePen;
end;

(**************************************************************************\
*
*   GDI+ Brush class
*
\**************************************************************************)

//--------------------------------------------------------------------------
// Abstract base class for various brush types
//--------------------------------------------------------------------------

destructor TIGPBrush.Destroy();
begin
  GdipDeleteBrush( FNativeBrush );
end;

function TIGPBrush.Clone() : TIGPBrush;
var
  brush: GpBrush;
  newBrush: TIGPBrush;
begin
  brush := NIL;
  ErrorCheck( GdipCloneBrush(FNativeBrush, brush));
  newBrush := TIGPBrush.Create(brush);
  if (newBrush = NIL) then
    GdipDeleteBrush(brush);
  Result := newBrush;
end;

function TIGPBrush.GetType() : TIGPBrushType;
var type_: TIGPBrushType;
begin
  type_ := TIGPBrushType(-1);
  ErrorCheck( GdipGetBrushType(FNativeBrush, type_));
  Result := type_;
end;

constructor TIGPBrush.Create();
begin
  ErrorCheck( NotImplemented);
end;

constructor TIGPBrush.Create(nativeBrush: GpBrush);
begin
  SetNativeBrush(nativeBrush);
end;

procedure TIGPBrush.SetNativeBrush(nativeBrush: GpBrush);
begin
  FNativeBrush := nativeBrush;
end;

function TIGPBrush.GetNativeBrush() : GpBrush;
begin
  Result := FNativeBrush;
end;
  
//--------------------------------------------------------------------------
// Solid Fill Brush Object
//--------------------------------------------------------------------------

constructor TIGPSolidBrush.Create(color: TAlphaColor);
var
  brush: GpSolidFill;
begin
  brush := NIL;
  ErrorCheck( GdipCreateSolidFill(color, brush));
  SetNativeBrush(brush);
end;

constructor TIGPSolidBrush.Create();
begin
  // hide parent function
end;

function TIGPSolidBrush.GetColor() : TAlphaColor;
begin
  ErrorCheck( GdipGetSolidFillColor(GpSolidFill(FNativeBrush), Result ));
end;

function TIGPSolidBrush.SetColor(color: TAlphaColor) : TIGPSolidBrush;
begin
  ErrorCheck( GdipSetSolidFillColor(GpSolidFill(FNativeBrush), color));
  Result := Self;
end;

procedure TIGPSolidBrush.SetColorProp(color: TAlphaColor);
begin
  ErrorCheck( GdipSetSolidFillColor(GpSolidFill(FNativeBrush), color));
end;

//--------------------------------------------------------------------------
// Texture Brush Fill Object
//--------------------------------------------------------------------------

constructor TIGPTextureBrush.Create(image: IGPImage; wrapMode: TIGPWrapMode = WrapModeTile);
var texture: GpTexture;
begin
  Assert( image <> NIL );

  //texture := NIL;
  ErrorCheck( GdipCreateTexture(image.GetNativeImage(), wrapMode, texture));
  SetNativeBrush(texture);
end;

// When creating a texture brush from a metafile image, the dstRect
// is used to specify the size that the metafile image should be
// rendered at in the device units of the destination graphics.
// It is NOT used to crop the metafile image, so only the width
// and height values matter for metafiles.

constructor TIGPTextureBrush.Create(image: IGPImage; wrapMode: TIGPWrapMode; const dstRect: TIGPRectF);
var texture: GpTexture;
begin
  Assert( image <> NIL );

  texture := NIL;
  ErrorCheck( GdipCreateTexture2(image.GetNativeImage(), wrapMode, dstRect.X,
                  dstRect.Y, dstRect.Width, dstRect.Height, texture));
  SetNativeBrush(texture);
end;

constructor TIGPTextureBrush.Create(image: IGPImage; const dstRect: TIGPRectF; imageAttributes: IGPImageAttributes = NIL);
var texture: GpTexture;
    ImgAtt: GpImageAttributes;
begin
  Assert( image <> NIL );

  texture := NIL;
  if( Assigned(imageAttributes)) then
    ImgAtt := imageAttributes.GetNativeImageAttr()

  else
    ImgAtt := NIL;

  ErrorCheck( GdipCreateTextureIA(image.GetNativeImage(), ImgAtt, dstRect.X,
                  dstRect.Y, dstRect.Width, dstRect.Height, texture));
  SetNativeBrush(texture);
end;

constructor TIGPTextureBrush.Create(image: IGPImage; const dstRect: TIGPRect; imageAttributes: IGPImageAttributes = NIL);
var texture: GpTexture;
    ImgAtt: GpImageAttributes;
begin
  Assert( image <> NIL );

  texture := NIL;
  if( Assigned(imageAttributes)) then
    ImgAtt := imageAttributes.GetNativeImageAttr()

  else
    ImgAtt := NIL;

  ErrorCheck( GdipCreateTextureIAI(image.GetNativeImage(), ImgAtt, dstRect.X,
                  dstRect.Y, dstRect.Width, dstRect.Height, texture));
   SetNativeBrush(texture);
end;

constructor TIGPTextureBrush.Create(image: IGPImage; wrapMode: TIGPWrapMode; const dstRect: TIGPRect);
var texture: GpTexture;
begin
  texture := NIL;
  ErrorCheck( GdipCreateTexture2I(image.GetNativeImage(), wrapMode, dstRect.X,
                  dstRect.Y, dstRect.Width, dstRect.Height, texture));
  SetNativeBrush(texture);
end;

constructor TIGPTextureBrush.Create(image: IGPImage; wrapMode: TIGPWrapMode; dstX, dstY, dstWidth, dstHeight: Single);
var texture: GpTexture;
begin
  Assert( image <> NIL );

  texture := NIL;
  ErrorCheck( GdipCreateTexture2(image.GetNativeImage(), wrapMode, dstX, dstY,
                  dstWidth, dstHeight, texture));
  SetNativeBrush(texture);
end;

constructor TIGPTextureBrush.Create(image: IGPImage; wrapMode: TIGPWrapMode; dstX, dstY, dstWidth, dstHeight: Integer);
var texture: GpTexture;
begin
  Assert( image <> NIL );

  texture := NIL;
  ErrorCheck( GdipCreateTexture2I(image.GetNativeImage(), wrapMode, dstX, dstY,
                  dstWidth, dstHeight, texture));
  SetNativeBrush(texture);
end;

function TIGPTextureBrush.SetTransform(matrix: IGPMatrix) : TIGPTextureBrush;
begin
  ErrorCheck( GdipSetTextureTransform(GpTexture(FNativeBrush),
              matrix.GetNativeMatrix()));

  Result := Self;
end;

procedure TIGPTextureBrush.SetTransformProp(matrix: IGPMatrix);
begin
  Assert( matrix <> NIL );

  ErrorCheck( GdipSetTextureTransform(GpTexture(FNativeBrush),
              matrix.GetNativeMatrix()));

end;

function TIGPTextureBrush.GetTransform() : IGPMatrix;
begin
  Result := TIGPMatrix.Create();
  ErrorCheck( GdipGetTextureTransform(GpTexture(FNativeBrush),
              Result.GetNativeMatrix()));
end;

function TIGPTextureBrush.ResetTransform() : TIGPTextureBrush;
begin
  ErrorCheck( GdipResetTextureTransform(GpTexture(FNativeBrush)));

  Result := Self;
end;

function TIGPTextureBrush.MultiplyTransform(matrix: IGPMatrix; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPTextureBrush;
begin
  ErrorCheck( GdipMultiplyTextureTransform(GpTexture(FNativeBrush),
              matrix.GetNativeMatrix(), order));

  Result := Self;
end;

function TIGPTextureBrush.TranslateTransform(dx, dy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPTextureBrush;
begin
  ErrorCheck( GdipTranslateTextureTransform(GpTexture(FNativeBrush),
              dx, dy, order));

  Result := Self;
end;

function TIGPTextureBrush.ScaleTransform(sx, sy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPTextureBrush;
begin
  ErrorCheck( GdipScaleTextureTransform(GpTexture(FNativeBrush),
               sx, sy, order));

  Result := Self;
end;

function TIGPTextureBrush.ScaleTransform(s: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPTextureBrush;
begin
  Result := ScaleTransform( s, s, order );
end;

function TIGPTextureBrush.RotateTransform(angle: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPTextureBrush;
begin
  ErrorCheck( GdipRotateTextureTransform(GpTexture(FNativeBrush), angle, order));
  Result := Self;
end;

function TIGPTextureBrush.SetTransformT(matrix: IGPMatrix) : IGPTransformable;
begin
  ErrorCheck( GdipSetTextureTransform(GpTexture(FNativeBrush),
              matrix.GetNativeMatrix()));

  Result := Self;
end;

function TIGPTextureBrush.ResetTransformT() : IGPTransformable;
begin
  ErrorCheck( GdipResetTextureTransform(GpTexture(FNativeBrush)));

  Result := Self;
end;

function TIGPTextureBrush.MultiplyTransformT(matrix: IGPMatrix; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipMultiplyTextureTransform(GpTexture(FNativeBrush),
              matrix.GetNativeMatrix(), order));

  Result := Self;
end;

function TIGPTextureBrush.TranslateTransformT(dx, dy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipTranslateTextureTransform(GpTexture(FNativeBrush),
              dx, dy, order));

  Result := Self;
end;

function TIGPTextureBrush.ScaleTransformT(sx, sy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipScaleTextureTransform(GpTexture(FNativeBrush),
               sx, sy, order));

  Result := Self;
end;

function TIGPTextureBrush.ScaleTransformXYT(s: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  Result := ScaleTransformT( s, s, order );
end;

function TIGPTextureBrush.RotateTransformT(angle: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipRotateTextureTransform(GpTexture(FNativeBrush), angle, order));
  Result := Self;
end;

function TIGPTextureBrush.SetWrapMode(wrapMode: TIGPWrapMode) : TIGPTextureBrush;
begin
  ErrorCheck( GdipSetTextureWrapMode(GpTexture(FNativeBrush), wrapMode));
  Result := Self;
end;

procedure TIGPTextureBrush.SetWrapModeProp(wrapMode: TIGPWrapMode);
begin
  ErrorCheck( GdipSetTextureWrapMode(GpTexture(FNativeBrush), wrapMode));
end;

function TIGPTextureBrush.GetWrapMode: TIGPWrapMode;
begin
  ErrorCheck( GdipGetTextureWrapMode(GpTexture(FNativeBrush), Result));
end;

function TIGPTextureBrush.GetImage() : IGPImage;
var image: GpImage;
begin
  ErrorCheck( GdipGetTextureImage(GpTexture(FNativeBrush), image));
  Result := TIGPImage.CreateGdiPlus(image, False);
  if (Result = NIL) then
    GdipDisposeImage(image);
end;

function TIGPTextureBrush.SetImage( image : IGPImage ) : TIGPTextureBrush;
var
  texture   : GpTexture;
  wrapMode  : TIGPWrapMode;

begin
  wrapMode := GetWrapMode();
  ErrorCheck( GdipCreateTexture(image.GetNativeImage(), wrapMode, texture));
  SetNativeBrush(texture);
  Result := Self;
end;
  
procedure TIGPTextureBrush.SetImageProp( image : IGPImage );
begin
  SetImage( image );
end;
  
constructor TIGPTextureBrush.Create();
begin
  // hide parent function
end;

//--------------------------------------------------------------------------
// Linear Gradient Brush Object
//--------------------------------------------------------------------------

constructor TIGPLinearGradientBrush.Create( const point1, point2: TPointF; color1, color2: TAlphaColor);
var
  brush : GpLineGradient;
  APoint2x : TPointF;

begin
  brush := NIL;
  APoint2x := point2;
  if(( point1.X = APoint2x.X ) and ( point1.Y = APoint2x.Y )) then
    APoint2x.X := point1.X + 1;

  ErrorCheck( GdipCreateLineBrush(@point1, @APoint2x, color1, color2, WrapModeTile, brush));
  SetNativeBrush(brush);
end;

constructor TIGPLinearGradientBrush.Create( const point1, point2: TPoint; color1, color2: TAlphaColor);
var
  brush: GpLineGradient;
  APoint2x : TPoint;

begin
  brush := NIL;
  APoint2x := point2;
  if(( point1.X = APoint2x.X ) and ( point1.Y = APoint2x.Y )) then
    APoint2x.X := point1.X + 1;

  ErrorCheck( GdipCreateLineBrushI(@point1, @APoint2x, color1,
                  color2, WrapModeTile, brush));
  SetNativeBrush(brush);
end;

constructor TIGPLinearGradientBrush.Create(const rect: TIGPRectF; color1, color2: TAlphaColor; mode: TIGPLinearGradientMode);
var
  brush: GpLineGradient;
  rectX : TIGPRectF;

begin
  brush := NIL;
  rectX := rect;
  if(( rectX.Width = 0 ) and ( rectX.Height = 0 )) then
    rectX.Width := 1;

  ErrorCheck( GdipCreateLineBrushFromRect(@rectX, color1,
                  color2, mode, WrapModeTile, brush));
  SetNativeBrush(brush);
end;

constructor TIGPLinearGradientBrush.Create(const rect: TIGPRect; color1, color2: TAlphaColor; mode: TIGPLinearGradientMode);
var brush: GpLineGradient;
begin
  brush := NIL;
  ErrorCheck( GdipCreateLineBrushFromRectI(@rect, color1,
                  color2, mode, WrapModeTile, brush));
  SetNativeBrush(brush);
end;

constructor TIGPLinearGradientBrush.Create(const rect: TIGPRectF; color1, color2: TAlphaColor; angle: Single; isAngleScalable: Boolean = False);
var brush: GpLineGradient;
begin
  brush := NIL;
  ErrorCheck( GdipCreateLineBrushFromRectWithAngle(@rect, color1,
                  color2, angle, isAngleScalable, WrapModeTile, brush));
  SetNativeBrush(brush);
end;

constructor TIGPLinearGradientBrush.Create( const rect: TIGPRect; color1, color2: TAlphaColor; angle: Single; isAngleScalable: Boolean = False);
var brush: GpLineGradient;
begin
  brush := NIL;
  ErrorCheck( GdipCreateLineBrushFromRectWithAngleI(@rect, color1,
                  color2, angle, isAngleScalable, WrapModeTile, brush));
  SetNativeBrush(brush);
end;

function TIGPLinearGradientBrush.SetLinearColors(color1, color2: TAlphaColor) : TIGPLinearGradientBrush;
begin
  ErrorCheck( GdipSetLineColors(GpLineGradient(FNativeBrush),
              color1, color2));

  Result := Self;
end;

function TIGPLinearGradientBrush.GetLinearColors(out color1, color2: TAlphaColor) : TIGPLinearGradientBrush;
var colors: array[0..1] of TAlphaColor;
begin
  ErrorCheck( GdipGetLineColors(GpLineGradient(FNativeBrush), PAlphaColor(@colors)));
  color1 := colors[0];
  color2 := colors[1];

  Result := Self;
end;

function TIGPLinearGradientBrush.GetRectangleF() : TIGPRectF;
begin
  ErrorCheck( GdipGetLineRect(GpLineGradient(FNativeBrush), @Result));
end;

function TIGPLinearGradientBrush.GetRectangle() : TIGPRect;
begin
  ErrorCheck( GdipGetLineRectI(GpLineGradient(FNativeBrush), @Result));
end;

procedure TIGPLinearGradientBrush.SetGammaCorrectionProp(useGammaCorrection: Boolean);
begin
  ErrorCheck( GdipSetLineGammaCorrection(GpLineGradient(FNativeBrush),
              useGammaCorrection));
end;
  
function TIGPLinearGradientBrush.SetGammaCorrection(useGammaCorrection: Boolean) : TIGPLinearGradientBrush;
begin
  ErrorCheck( GdipSetLineGammaCorrection(GpLineGradient(FNativeBrush),
              useGammaCorrection));

  Result := Self;
end;

function TIGPLinearGradientBrush.GetGammaCorrection: Boolean;
var useGammaCorrection: BOOL;
begin
  ErrorCheck( GdipGetLineGammaCorrection(GpLineGradient(FNativeBrush),
              useGammaCorrection));
  Result := useGammaCorrection;
end;

function TIGPLinearGradientBrush.GetBlendCount: Integer;
var count: Integer;
begin
  count := 0;
  ErrorCheck( GdipGetLineBlendCount(GpLineGradient(FNativeBrush), count));
  Result := count;
end;

function TIGPLinearGradientBrush.SetBlendArrays( const blendFactors : array of Single; const blendPositions : array of Single ) : TIGPLinearGradientBrush;
begin
  ErrorCheck( GdipSetLineBlend(GpLineGradient(FNativeBrush),
                        @blendFactors[ 0 ], @blendPositions[ 0 ], Min( Length( blendFactors ), Length( blendPositions )) ));

  Result := Self;
end;

function TIGPLinearGradientBrush.SetBlend( const blendFactors : array of TIGPBlend ) : TIGPLinearGradientBrush;
var
  I : Integer;
  count : Integer;
  aFactors : array of Single;
  aPositions : array of Single;

begin
  count := Length( blendFactors ); 
  SetLength( aFactors, count );
  SetLength( aPositions, count );
  for I := 0 to count - 1 do
    begin
    aFactors[ I ] := blendFactors[ I ].Value;
    aPositions[ I ] := blendFactors[ I ].Position;
    end;

  Result := SetBlendArrays( aFactors, aPositions );
end;

procedure TIGPLinearGradientBrush.SetBlendProp( const blendFactors : TArray<TIGPBlend> );
begin
  SetBlend( blendFactors );
end;
  
function TIGPLinearGradientBrush.GetBlend() : TArray<TIGPBlend>;
var
  count : Integer;
  aFactors : array of Single;
  aPositions : array of Single;
  I : Integer;

begin
  ErrorCheck( GdipGetLineBlendCount( GetNativeBrush(), count ));

  SetLength( aFactors, count );
  SetLength( aPositions, count );

  ErrorCheck( GdipGetLineBlend(GpLineGradient(FNativeBrush), @aFactors[ 0 ],
      @aPositions[ 0 ], count));

  SetLength( Result, count );
  for I := 0 to count - 1 do
    begin
    Result[ I ].Position := aPositions[ I ];
    Result[ I ].Value := aFactors[ I ];
    end;

end;

function TIGPLinearGradientBrush.GetInterpolationColorCount() : Integer;
var count: Integer;
begin
  count := 0;
  ErrorCheck( GdipGetLinePresetBlendCount(GpLineGradient(FNativeBrush), count));
  Result := count;
end;

function TIGPLinearGradientBrush.SetInterpolationColorArrays( const presetColors: array of TAlphaColor; const blendPositions: array of Single ) : TIGPLinearGradientBrush;
begin
  ErrorCheck( GdipSetLinePresetBlend(GpLineGradient(FNativeBrush),
      PAlphaColor( @presetColors[ 0 ]), @blendPositions[ 0 ], Min( Length( presetColors ), Length( blendPositions ))));

  Result := Self;
end;

function TIGPLinearGradientBrush.SetInterpolationColors( const Colors : array of TIGPInterpolationColor ) : TIGPLinearGradientBrush;
var
  presetColors : array of TAlphaColor;
  blendPositions : array of Single;
  count : Integer;
  I : Integer;
    
begin
  count := Length( Colors );

  SetLength( presetColors, count ); 
  SetLength( blendPositions, count );
     
  for I := 0 to count - 1 do
    begin
    presetColors[ I ] := Colors[ I ].Color;
    blendPositions[ I ] := Colors[ I ].Position;
    end; 

  ErrorCheck( GdipSetLinePresetBlend(GpLineGradient(FNativeBrush),
      PAlphaColor( @presetColors[ 0 ]), @blendPositions[ 0 ], count ));

  Result := Self;
end;

procedure TIGPLinearGradientBrush.SetInterpolationColorsProp( Colors : TArray<TIGPInterpolationColor> );
begin
  SetInterpolationColors( Colors );
end;
  
function TIGPLinearGradientBrush.GetInterpolationColors() : TArray<TIGPInterpolationColor>;
var
  presetColors : array of TAlphaColor;
  blendPositions : array of Single;
  count : Integer;
  I : Integer;
    
begin
  ErrorCheck( GdipGetLinePresetBlendCount( GpLineGradient(FNativeBrush), count ));

  SetLength( presetColors, count );
  SetLength( blendPositions, count );
    
  ErrorCheck( GdipGetLinePresetBlend(GpLineGradient(FNativeBrush),
                        PAlphaColor(@presetColors[ 0 ]), @blendPositions[ 0 ], count));
                          
  for I := 0 to count - 1 do
    begin
    Result[ I ].Color := presetColors[ I ];
    Result[ I ].Position := blendPositions[ I ];
    end;

end;

function TIGPLinearGradientBrush.SetBlendBellShape(focus: Single; scale: Single = 1.0) : TIGPLinearGradientBrush;
begin
  ErrorCheck( GdipSetLineSigmaBlend(GpLineGradient(FNativeBrush), focus, scale));

  Result := Self;
end;

function TIGPLinearGradientBrush.SetBlendTriangularShape(focus: Single; scale: Single = 1.0) : TIGPLinearGradientBrush;
begin
  ErrorCheck( GdipSetLineLinearBlend(GpLineGradient(FNativeBrush), focus, scale));

  Result := Self;
end;

function TIGPLinearGradientBrush.SetTransform(matrix: IGPMatrix) : TIGPLinearGradientBrush;
begin
  ErrorCheck( GdipSetLineTransform(GpLineGradient(FNativeBrush),
                                                        matrix.GetNativeMatrix()));

  Result := Self;
end;

procedure TIGPLinearGradientBrush.SetTransformProp(matrix: IGPMatrix);
begin
  ErrorCheck( GdipSetLineTransform(GpLineGradient(FNativeBrush),
                                                        matrix.GetNativeMatrix()));

end;
  
function TIGPLinearGradientBrush.GetTransform() : IGPMatrix;
begin
  Result := TIGPMatrix.Create();
  ErrorCheck( GdipGetLineTransform(GpLineGradient(FNativeBrush),
                                                   Result.GetNativeMatrix()));
end;

function TIGPLinearGradientBrush.ResetTransform() : TIGPLinearGradientBrush;
begin
  ErrorCheck( GdipResetLineTransform(GpLineGradient(FNativeBrush)));

  Result := Self;
end;

function TIGPLinearGradientBrush.MultiplyTransform(matrix: IGPMatrix; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPLinearGradientBrush;
begin
  ErrorCheck( GdipMultiplyLineTransform(GpLineGradient(FNativeBrush),
                                                              matrix.GetNativeMatrix(),
                                                              order));

  Result := Self;
end;

function TIGPLinearGradientBrush.TranslateTransform(dx, dy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPLinearGradientBrush;
begin
  ErrorCheck( GdipTranslateLineTransform(GpLineGradient(FNativeBrush),
                                                             dx, dy, order));

  Result := Self;
end;

function TIGPLinearGradientBrush.ScaleTransform(sx, sy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPLinearGradientBrush;
begin
  ErrorCheck( GdipScaleLineTransform(GpLineGradient(FNativeBrush),
                                                           sx, sy, order));

  Result := Self;
end;

function TIGPLinearGradientBrush.ScaleTransform(s: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPLinearGradientBrush;
begin
  Result := ScaleTransform( s, s, order );
end;

function TIGPLinearGradientBrush.RotateTransform(angle: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPLinearGradientBrush;
begin
  ErrorCheck( GdipRotateLineTransform(GpLineGradient(FNativeBrush),
                                                            angle, order));

  Result := Self;
end;

function TIGPLinearGradientBrush.SetTransformT(matrix: IGPMatrix) : IGPTransformable;
begin
  ErrorCheck( GdipSetLineTransform(GpLineGradient(FNativeBrush),
                                                        matrix.GetNativeMatrix()));

  Result := Self;
end;

function TIGPLinearGradientBrush.ResetTransformT() : IGPTransformable;
begin
  ErrorCheck( GdipResetLineTransform(GpLineGradient(FNativeBrush)));

  Result := Self;
end;

function TIGPLinearGradientBrush.MultiplyTransformT(matrix: IGPMatrix; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipMultiplyLineTransform(GpLineGradient(FNativeBrush),
                                                              matrix.GetNativeMatrix(),
                                                              order));

  Result := Self;
end;

function TIGPLinearGradientBrush.TranslateTransformT(dx, dy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipTranslateLineTransform(GpLineGradient(FNativeBrush), dx, dy, order));

  Result := Self;
end;

function TIGPLinearGradientBrush.ScaleTransformT(sx, sy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipScaleLineTransform(GpLineGradient(FNativeBrush), sx, sy, order));

  Result := Self;
end;

function TIGPLinearGradientBrush.ScaleTransformXYT(s: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  Result := ScaleTransformT( s, s, order );
end;

function TIGPLinearGradientBrush.RotateTransformT(angle: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipRotateLineTransform(GpLineGradient(FNativeBrush), angle, order));

  Result := Self;
end;

procedure TIGPLinearGradientBrush.SetWrapModeProp(wrapMode: TIGPWrapMode);
begin
  ErrorCheck( GdipSetLineWrapMode(GpLineGradient(FNativeBrush), wrapMode));
end;

function TIGPLinearGradientBrush.SetWrapMode(wrapMode: TIGPWrapMode) : TIGPLinearGradientBrush;
begin
  ErrorCheck( GdipSetLineWrapMode(GpLineGradient(FNativeBrush), wrapMode));
  Result := Self;
end;

function TIGPLinearGradientBrush.GetWrapMode() : TIGPWrapMode;
begin
   ErrorCheck( GdipGetLineWrapMode(GpLineGradient(FNativeBrush), Result));
end;

constructor TIGPLinearGradientBrush.Create();
begin
  // hide parent function
end;

//--------------------------------------------------------------------------
// Hatch Brush Object
//--------------------------------------------------------------------------

constructor TIGPHatchBrush.Create();
var
  brush: GpHatch;
begin
  brush := NIL;
  ErrorCheck( GdipCreateHatchBrush(Integer(HatchStyleCross), aclWhite, aclBlack, brush));
  SetNativeBrush(brush);
end;
  
constructor TIGPHatchBrush.Create(hatchStyle: TIGPHatchStyle; foreColor: TAlphaColor; backColor: TAlphaColor = aclBlack);
var
  brush: GpHatch;
begin
  brush := NIL;
  ErrorCheck( GdipCreateHatchBrush(Integer(hatchStyle), foreColor, backColor, brush));
  SetNativeBrush(brush);
end;

procedure TIGPHatchBrush.SetHatchStyleProp( style : TIGPHatchStyle );
var
  brush: GpHatch;
begin
  brush := NIL;
  ErrorCheck( GdipCreateHatchBrush( Integer( style ), GetForegroundColor(), GetBackgroundColor(), brush));
  SetNativeBrush(brush);
end;
  
function TIGPHatchBrush.SetHatchStyle( style : TIGPHatchStyle ) : TIGPHatchBrush;
begin
  SetHatchStyleProp( style );
  Result := Self;
end;
    
function TIGPHatchBrush.GetHatchStyle() : TIGPHatchStyle;
begin
  ErrorCheck( GdipGetHatchStyle(GpHatch(FNativeBrush), Result));
end;

procedure TIGPHatchBrush.SetForegroundColorProp( color : TAlphaColor );
var
  brush: GpHatch;
begin
  brush := NIL;
  ErrorCheck( GdipCreateHatchBrush( Integer( GetHatchStyle() ), color, GetBackgroundColor(), brush));
  SetNativeBrush(brush);
end;
  
function TIGPHatchBrush.SetForegroundColor( color : TAlphaColor ) : TIGPHatchBrush;
begin
  SetForegroundColorProp( color );
  Result := Self;
end;

function TIGPHatchBrush.GetForegroundColor() : TAlphaColor;
begin
  ErrorCheck( GdipGetHatchForegroundColor(GpHatch(FNativeBrush), Result ));
end;

procedure TIGPHatchBrush.SetBackgroundColorProp( color : TAlphaColor );
var
  brush: GpHatch;
begin
  brush := NIL;
  ErrorCheck( GdipCreateHatchBrush( Integer( GetHatchStyle() ), GetForegroundColor(), color, brush));
  SetNativeBrush(brush);
end;

function TIGPHatchBrush.SetBackgroundColor( color : TAlphaColor ) : TIGPHatchBrush;
begin
  SetBackgroundColorProp( color );
  Result := Self;
end;

function TIGPHatchBrush.GetBackgroundColor() : TAlphaColor;
begin
  ErrorCheck( GdipGetHatchBackgroundColor(GpHatch(FNativeBrush), Result ));
end;

constructor TIGPImage.Create(filename: WideString;
                useEmbeddedColorManagement: Boolean = False);
begin
  FNativeImage := NIL;
  if(useEmbeddedColorManagement) then
  begin
      ErrorCheck( GdipLoadImageFromFileICM(
          PWideChar(filename),
          FNativeImage
      ));
  end
  else
  begin
      ErrorCheck( GdipLoadImageFromFile(
          PWideChar(filename),
          FNativeImage
      ));
  end;
end;

constructor TIGPImage.Create(stream: IStream; useEmbeddedColorManagement: Boolean = False);
begin
  inherited Create();

  FNativeImage := NIL;
  if(useEmbeddedColorManagement) then
    ErrorCheck( GdipLoadImageFromStreamICM(stream, FNativeImage))
      
  else
    ErrorCheck( GdipLoadImageFromStream(stream, FNativeImage));
      
end;

constructor TIGPImage.Create(stream: TStream; useEmbeddedColorManagement: Boolean = False);
var
  AStream     : IStream;
  AStream1    : IStream;
  AObjectData : HGlobal;
  cbRead      : LongInt;
{$IFDEF DCC}
  {$if CompilerVersion > 28} // Delphi XE8
    ASize1      : LargeUInt;
    ASize2      : LargeUInt;
  {$ELSE} // Delphi XE8
    ASize1      : Int64;
    ASize2      : Int64;
  {$IFEND} // Delphi XE8
{$ELSE}
  ASize1      : LargeUInt;
  ASize2      : LargeUInt;
{$ENDIF}
  ABuffer     : TBytes;

begin
  AObjectData := GlobalAlloc(GMEM_MOVEABLE, 0);
  try
    CreateStreamOnHGlobal( AObjectData, False, AStream );

    AStream1 := TStreamAdapter.Create( stream );
    AStream1.Seek( 0, STREAM_SEEK_END, ASize1 );
    AStream1.Seek( 0, STREAM_SEEK_SET, ASize2 );
    SetLength( ABuffer, ASize1 );
    AStream1.Read( @ABuffer[ 0 ], ASize1, @cbRead );
    AStream.Write( @ABuffer[ 0 ], ASize1, @cbRead );
//    AStream1.CopyTo( AStream, ASize1, cbRead, cbWritten );
    Create( AStream, useEmbeddedColorManagement );

  finally
    GlobalFree( AObjectData);
    end;

end;

constructor TIGPImage.Create(ABytes : TBytes; useEmbeddedColorManagement: Boolean = False);
var
  AStream       : IStream;
  AStream1      : IStream;
  ADelphiStream : TMemoryStream;
  AObjectData   : HGlobal;
{$IFDEF DCC}
  {$if CompilerVersion > 28} // Delphi XE8
    ASize1        : LargeUInt;
    ASize2        : LargeUInt;
  {$ELSE} // Delphi XE8
    ASize1        : Int64;
    ASize2        : Int64;
  {$IFEND} // Delphi XE8
{$ELSE}
  ASize1        : LargeUInt;
  ASize2        : LargeUInt;
{$ENDIF}
  ABuffer       : TBytes;
  cbRead        : LongInt;

begin
  if( Length( ABytes ) <> 0 ) then
    begin
    ADelphiStream := TMemoryStream.Create();
    {$IFDEF FPC}
      {$IFDEF HAS_STREAM_WRITEDATA}
      ADelphiStream.WriteData( ABytes, Length( ABytes ));
      {$ELSE}
      ADelphiStream.Write(ABytes, Length(ABytes));
      {$ENDIF}
    {$ELSE}
      {$IFDEF DELPHI_XE3_OR_UP}
      ADelphiStream.WriteData( ABytes, Length( ABytes ));
      {$ELSE}
      ADelphiStream.Write(ABytes, Length(ABytes));
      {$ENDIF}
    {$ENDIF}
    ADelphiStream.Position := 0;
    try
      AObjectData := GlobalAlloc(GMEM_MOVEABLE, 0);
      try
        CreateStreamOnHGlobal( AObjectData, False, AStream );

        AStream1 := TStreamAdapter.Create( ADelphiStream );
        AStream1.Seek( 0, STREAM_SEEK_END, ASize1 );
        AStream1.Seek( 0, STREAM_SEEK_SET, ASize2 );
        SetLength( ABuffer, ASize1 );
        AStream1.Read( @ABuffer[ 0 ], ASize1, @cbRead );
        AStream.Write( @ABuffer[ 0 ], ASize1, @cbRead );

        Create( AStream, useEmbeddedColorManagement );
      finally
        GlobalFree( AObjectData );
        end;

    finally
      {$IFDEF DCC}
        {$IFDEF DELPHI_XE4_OR_UP}
        ADelphiStream.DisposeOf();
        {$ELSE}
        ADelphiStream.Free;
        {$ENDIF}
      {$ELSE}
      //http://docwiki.embarcadero.com/Libraries/Rio/en/System.TObject.DisposeOf
      ADelphiStream.Free;
      {$ENDIF}
      end;
    end

  else
    inherited Create();

end;

class function TIGPImage.FromFile(filename: WideString;
             useEmbeddedColorManagement: Boolean = False) : TIGPImage;
begin
  Result := TIGPImage.Create(
      PWideChar(filename),
      useEmbeddedColorManagement
  );
end;

class function TIGPImage.FromStream(stream: IStream;
             useEmbeddedColorManagement: Boolean = False) : TIGPImage;
begin
  Result := TIGPImage.Create(
      stream,
      useEmbeddedColorManagement
  );
end;

destructor TIGPImage.Destroy();
begin
  GdipDisposeImage(FNativeImage);
end;

function TIGPImage.Clone: TIGPImage;
var cloneimage: GpImage;
begin
  cloneimage := NIL;
  ErrorCheck( GdipCloneImage(FNativeImage, cloneimage));
  Result := TIGPImage.CreateGdiPlus(cloneimage, False);
end;

function TIGPImage.Save(filename: WideString; const clsidEncoder: TGUID;
             encoderParams: PGPEncoderParameters = NIL) : TIGPImage;
begin
  ErrorCheck( GdipSaveImageToFile( FNativeImage,
                                   PWideChar(filename),
                                   @clsidEncoder,
                                   encoderParams));

  Result := Self;
end;

function TIGPImage.Save(stream: IStream; const clsidEncoder: TGUID;
             encoderParams: PGPEncoderParameters = NIL) : TIGPImage;
begin
  ErrorCheck( GdipSaveImageToStream( FNativeImage,
                                     stream,
                                     @clsidEncoder,
                                     encoderParams));

  Result := Self;
end;

function TIGPImage.Save(filename: WideString; const formatName : String ) : TIGPImage;
var
  pClsid  : TCLSID;

begin
  if( GetEncoderClsid( 'image/' + formatName, pClsid )) then
    Exit( Save( filename, pClsid ));

  raise EGPException.Create( 'Unknown image format' );
end;

function TIGPImage.Save(stream: IStream; const formatName : String ) : TIGPImage;
var
  pClsid  : TCLSID;

begin
  if( GetEncoderClsid( 'image/' + formatName, pClsid )) then
    Exit( Save( stream, pClsid ));

  raise EGPException.Create( 'Unknown image format' );
end;

function TIGPImage.Save(stream: TStream; const clsidEncoder: TGUID; encoderParams: PGPEncoderParameters = NIL) : TIGPImage;
var
  AStreamAdapter : IStream;

begin
  AStreamAdapter := TStreamAdapter.Create( stream );
  Result := Save( AStreamAdapter, clsidEncoder, encoderParams );
end;

function TIGPImage.Save(stream: TStream; const formatName : String = 'bmp' ) : TIGPImage;
var
  AStreamAdapter : IStream;

begin
  AStreamAdapter := TStreamAdapter.Create( stream );
  Result := Save( AStreamAdapter, formatName );
end;

function TIGPImage.SaveAdd(encoderParams: PGPEncoderParameters) : TIGPImage;
begin
  ErrorCheck( GdipSaveAdd(FNativeImage, encoderParams));
  Result := Self;
end;

function TIGPImage.SaveAdd(newImage: IGPImage;
             encoderParams: PGPEncoderParameters) : TIGPImage;
begin
  if (newImage = NIL) then
    ErrorCheck( InvalidParameter);

  ErrorCheck( GdipSaveAddImage(FNativeImage,
                                                newImage.GetNativeImage(),
                                                encoderParams));
  Result := Self;
end;

function TIGPImage.GetType() : TIGPImageType;
begin
  ErrorCheck( GdipGetImageType(FNativeImage, Result));
end;

function TIGPImage.GetPhysicalDimension() : TIGPSizeF;
begin
  ErrorCheck( GdipGetImageDimension(FNativeImage, Result.Width, Result.Height));
end;

function TIGPImage.GetBounds(out srcRect: TIGPRectF; out srcUnit: TIGPUnit) : TIGPImage;
begin
  ErrorCheck( GdipGetImageBounds(FNativeImage, @srcRect, srcUnit));
  Result := Self;
end;

function TIGPImage.AsBytes( const formatName : String = 'bmp' ) : TBytes;
var
  AStream : TMemoryStream;

begin
  AStream := TMemoryStream.Create();
  try
    Save( AStream, formatName );
    AStream.Position := 0;
    SetLength( Result, AStream.Size );
    {$IFDEF FPC}
      {$IFDEF HAS_READ_WITH_OFFSET}
      AStream.Read( Result, 0, AStream.Size );
      {$ELSE}
      AStream.Read( Result, AStream.Size );
      {$ENDIF}
    {$ELSE}
      {$IFDEF DELPHI_XE3_OR_UP}
      AStream.Read( Result, 0, AStream.Size );
      {$ELSE}
      AStream.Read( Result, AStream.Size );
      {$ENDIF}
    {$ENDIF}

  finally
    {$IFDEF DCC}
      {$IFDEF DELPHI_XE4_OR_UP}
      AStream.DisposeOf();
      {$ELSE}
      AStream.Free;
      {$ENDIF}
    {$ELSE}
    AStream.Free;
    {$ENDIF}
    end;

end;

function TIGPImage.AsBytes( clsidEncoder: TGUID; encoderParams: PGPEncoderParameters = NIL ) : TBytes;
var
  AStream : TMemoryStream;

begin
  AStream := TMemoryStream.Create();
  try
    Save( AStream, clsidEncoder, encoderParams );
    AStream.Position := 0;
    SetLength( Result, AStream.Size );


    {$IFDEF FPC}
      {$IFDEF HAS_READ_WITH_OFFSET}
      AStream.Read( Result, 0, AStream.Size );
      {$ELSE}
      AStream.Read( Result, AStream.Size );
      {$ENDIF}
    {$ELSE}
      {$IFDEF DELPHI_XE3_OR_UP}
      AStream.Read( Result, 0, AStream.Size );
      {$ELSE}
      AStream.Read( Result, AStream.Size );
      {$ENDIF}
    {$ENDIF}

  finally
    {$IFDEF DCC}
      {$IFDEF DELPHI_XE4_OR_UP}
      AStream.DisposeOf();
      {$ELSE}
      AStream.Free;
      {$ENDIF}
    {$ELSE}
    AStream.Free;
    {$ENDIF}
    end;

end;

function TIGPImage.GetWidth() : Cardinal;
var width: Cardinal;
begin
  width := 0;
  ErrorCheck( GdipGetImageWidth(FNativeImage, width));
  Result := width;
end;

function TIGPImage.GetHeight() : Cardinal;
var height: Cardinal;
begin
  height := 0;
  ErrorCheck( GdipGetImageHeight(FNativeImage, height));
  Result := height;
end;

function TIGPImage.GetHorizontalResolution() : Single;
var resolution: Single;
begin
  resolution := 0.0;
  ErrorCheck( GdipGetImageHorizontalResolution(FNativeImage, resolution));
  Result := resolution;
end;

function TIGPImage.GetVerticalResolution() : Single;
var resolution: Single;
begin
  resolution := 0.0;
  ErrorCheck( GdipGetImageVerticalResolution(FNativeImage, resolution));
  Result := resolution;
end;

function TIGPImage.GetFlags: Cardinal;
var flags: Cardinal;
begin
  flags := 0;
  ErrorCheck( GdipGetImageFlags(FNativeImage, flags));
  Result := flags;
end;

function TIGPImage.GetRawFormat() : TGUID;
begin
  ErrorCheck( GdipGetImageRawFormat(FNativeImage, @Result ));
end;

function TIGPImage.GetFormatName() : String;
var
  AFormat : TGUID;

begin
  AFormat := GetRawFormat(); 
  if(( IsEqualGUID( AFormat, ImageFormatBMP )) or ( IsEqualGUID( AFormat, ImageFormatMemoryBMP ))) then
    Result := 'bmp'

  else if( IsEqualGUID( AFormat, ImageFormatEMF )) then
    Result := 'emf'

  else if( IsEqualGUID( AFormat, ImageFormatWMF )) then
    Result := 'wmf'

  else if( IsEqualGUID( AFormat, ImageFormatJPEG )) then
    Result := 'jpeg'

  else if( IsEqualGUID( AFormat, ImageFormatPNG )) then
    Result := 'png'

  else if( IsEqualGUID( AFormat, ImageFormatGIF )) then
    Result := 'gif'

  else if( IsEqualGUID( AFormat, ImageFormatEXIF )) then
    Result := 'exif'

  else if( IsEqualGUID( AFormat, ImageFormatIcon )) then
    Result := 'icon'

end;

function TIGPImage.GetPixelFormat: TIGPPixelFormat;
begin
  ErrorCheck( GdipGetImagePixelFormat(FNativeImage, Result));
end;

function TIGPImage.GetPaletteSize: Integer;
var size: Integer;
begin
  size := 0;
  ErrorCheck( GdipGetImagePaletteSize(FNativeImage, size));
  Result := size;
end;

function TIGPImage.GetPalette(palette: PGPColorPalette; size: Integer) : TIGPImage;
begin
  ErrorCheck( GdipGetImagePalette(FNativeImage, palette, size));
  Result := Self;
end;

function TIGPImage.SetPalette(palette: PGPColorPalette) : TIGPImage;
begin
  ErrorCheck( GdipSetImagePalette(FNativeImage, palette));
  Result := Self;
end;

type
  IGPIntAbortDispatcher = interface
    ['{FA84D400-A98A-46DD-9DBC-5B7BD2853B52}']
  end;
    
  TIGPIntAbortDispatcher = class( TInterfacedObject, IGPIntAbortDispatcher )
  public
    OnCallback : TIGPImageAbortProc;

  public
    function GPCallback() : BOOL;

  end;

function TIGPIntAbortDispatcher.GPCallback() : BOOL;
begin
  if( Assigned( OnCallback )) then
    Result := OnCallback()

  else
    Result := False;

end;
  
function GLGPAbortCallback(callbackData: Pointer) : BOOL; stdcall;
begin
  if( callbackData <> NIL ) then 
    Result := TIGPIntAbortDispatcher( callbackData ).GPCallback()

  else
    Result := False;

end;
  
function TIGPImage.GetThumbnailImage(thumbWidth, thumbHeight: Cardinal;
              callback: TIGPGetThumbnailImageAbortProc = NIL) : TIGPImage;
var
  thumbimage: GpImage;
  newImage: TIGPImage;
  ADispatcher : TIGPIntAbortDispatcher;
  ADispatcherIntf : IGPIntAbortDispatcher;
    
begin
  thumbimage := NIL;
  ADispatcher := TIGPIntAbortDispatcher.Create();
  ADispatcherIntf := ADispatcher; 
  ErrorCheck( GdipGetImageThumbnail(FNativeImage,
                                              thumbWidth, thumbHeight,
                                              thumbimage,
                                              GLGPAbortCallback, ADispatcher ));

  newImage := TIGPImage.CreateGdiPlus(thumbimage, False);
  if (newImage = NIL) then
      GdipDisposeImage(thumbimage);

  Result := newImage;
end;

function TIGPImage.GetFrameDimensionsCount: Cardinal;
begin
  ErrorCheck( GdipImageGetFrameDimensionsCount(FNativeImage, Result ));
end;

function TIGPImage.GetFrameDimensionsList() : TArray<TGUID>;
var
  count: Cardinal;
    
begin
  ErrorCheck( GdipImageGetFrameDimensionsCount(FNativeImage, count));
  SetLength( Result, count );
  ErrorCheck( GdipImageGetFrameDimensionsList(FNativeImage, @Result[ 0 ], count));
end;

function TIGPImage.GetFrameCount(const dimensionID: TGUID) : Cardinal;
var count: Cardinal;
begin
  count := 0;
  ErrorCheck( GdipImageGetFrameCount(FNativeImage, @dimensionID, count));
  Result := count;
end;

function TIGPImage.SelectActiveFrame(const dimensionID: TGUID; frameIndex: Cardinal) : TIGPImage;
begin
  ErrorCheck( GdipImageSelectActiveFrame(FNativeImage,
                                                          @dimensionID,
                                                          frameIndex));
                                                            
  Result := Self;
end;

function TIGPImage.RotateFlip(rotateFlipType: TIGPRotateFlipType) : TIGPImage;
begin
  ErrorCheck( GdipImageRotateFlip(FNativeImage,
                                                   rotateFlipType));
                                                            
  Result := Self;
end;

function TIGPImage.GetPropertyCount() : Cardinal;
begin
  ErrorCheck( GdipGetPropertyCount(FNativeImage, Result));
end;

function TIGPImage.GetPropertyIdList() : TArray<TPropID>;
var
  numProperty: Cardinal;
    
begin
  ErrorCheck( GdipGetPropertyCount(FNativeImage, numProperty ));
  SetLength( Result, numProperty );
  ErrorCheck( GdipGetPropertyIdList(FNativeImage, numProperty, @Result[ 0 ] ));
end;

function TIGPImage.GetPropertyItemSize(propId: PROPID) : Cardinal;
var size: Cardinal;
begin
  size := 0;
  ErrorCheck( GdipGetPropertyItemSize(FNativeImage, propId, size));
  Result := size;
end;

function TIGPImage.GetPropertyItem(propId: PROPID; propSize: Cardinal;
             buffer: PGPPropertyItem) : TIGPImage;
begin
  ErrorCheck( GdipGetPropertyItem(FNativeImage,
                                                   propId, propSize, buffer));
                                                            
  Result := Self;
end;

function TIGPImage.GetPropertySize(out totalBufferSize, numProperties : Cardinal) : TIGPImage;
begin
  ErrorCheck( GdipGetPropertySize(FNativeImage,
                                                   totalBufferSize,
                                                   numProperties));
                                                            
  Result := Self;
end;

function TIGPImage.GetAllPropertyItems(totalBufferSize, numProperties: Cardinal;
             allItems: PGPPropertyItem) : TIGPImage;
begin
  ErrorCheck( GdipGetAllPropertyItems(FNativeImage,
                                                       totalBufferSize,
                                                       numProperties,
                                                       allItems));
                                                            
  Result := Self;
end;

function TIGPImage.RemovePropertyItem(propId: TPROPID) : TIGPImage;
begin
  ErrorCheck( GdipRemovePropertyItem(FNativeImage, propId));
  Result := Self;
end;

function TIGPImage.SetPropertyItem(const item: TIGPPropertyItem) : TIGPImage;
begin
  ErrorCheck( GdipSetPropertyItem(FNativeImage, @item));
  Result := Self;
end;

function TIGPImage.GetEncoderParameterListSize(const clsidEncoder: TGUID) : Cardinal;
var size: Cardinal;
begin
  size := 0;
  ErrorCheck( GdipGetEncoderParameterListSize(FNativeImage, @clsidEncoder, size));
  Result := size;
end;

function TIGPImage.GetEncoderParameterList(const clsidEncoder: TGUID; size: Cardinal;
             buffer: PGPEncoderParameters) : TIGPImage;
begin
  ErrorCheck( GdipGetEncoderParameterList(FNativeImage,
                                                           @clsidEncoder,
                                                           size,
                                                           buffer));
                                                             
  Result := Self;
end;

constructor TIGPImage.CreateGdiPlus(nativeImage: GpImage; Dummy : Boolean);
begin
  SetNativeImage(nativeImage);
end;

procedure TIGPImage.SetNativeImage(nativeImage: GpImage);
begin
  FNativeImage := nativeImage;
end;

function TIGPImage.GetNativeImage() : GpImage;
begin
  Result := FNativeImage;
end;

  // TIGPBitmapData

constructor TIGPBitmapData.Create( ABitmap : TIGPBitmap );
begin
  inherited Create();
  FBitmap := ABitmap;
end;

destructor TIGPBitmapData.Destroy();
begin
  FBitmap.UnlockBits( FData );
  inherited;
end;

function TIGPBitmapData.GetWidth() : UINT;
begin
  Result := FData.Width;
end;

function TIGPBitmapData.GetHeight() : UINT;
begin
  Result := FData.Height;
end;

function TIGPBitmapData.GetStride() : Integer;
begin
  Result := FData.Stride;
end;

function TIGPBitmapData.GetPixelFormat() : TIGPPixelFormat;
begin
  Result := FData.PixelFormat;
end;

function TIGPBitmapData.GetScan0() : Pointer;
begin
  Result := FData.Scan0;
end;

// TIGPBitmap

constructor TIGPBitmap.Create(filename: WideString; useEmbeddedColorManagement: Boolean = False);
var bitmap: GpBitmap;
begin
  bitmap := NIL;
  if(useEmbeddedColorManagement) then
    ErrorCheck( GdipCreateBitmapFromFileICM(PWideChar(filename), bitmap))

  else
    ErrorCheck( GdipCreateBitmapFromFile(PWideChar(filename), bitmap));

  SetNativeImage(bitmap);
end;

constructor TIGPBitmap.Create(stream: IStream; useEmbeddedColorManagement: Boolean = False);
var bitmap: GpBitmap;
begin
  bitmap := NIL;
  if(useEmbeddedColorManagement) then
    ErrorCheck( GdipCreateBitmapFromStreamICM(stream, bitmap))

  else
    ErrorCheck( GdipCreateBitmapFromStream(stream, bitmap));

  SetNativeImage(bitmap);
end;

class function TIGPBitmap.FromFile(filename: WideString; useEmbeddedColorManagement: Boolean = False) : TIGPBitmap;
begin
  Result := TIGPBitmap.Create(
      PWideChar(filename),
      useEmbeddedColorManagement
  );
end;

class function TIGPBitmap.FromStream(stream: IStream; useEmbeddedColorManagement: Boolean = False) : TIGPBitmap;
begin
  Result := TIGPBitmap.Create(
      stream,
      useEmbeddedColorManagement
  );
end;

constructor TIGPBitmap.Create(width, height, stride: Integer; format: TIGPPixelFormat; scan0: PBYTE);
var bitmap: GpBitmap;
begin
  bitmap := NIL;
  ErrorCheck( GdipCreateBitmapFromScan0(width,
                                                     height,
                                                     stride,
                                                     format,
                                                     scan0,
                                                     bitmap));

  SetNativeImage(bitmap);
end;
{$ENDIF}

constructor TIGPBitmap.Create(width, height: Integer; format: TIGPPixelFormat = GPPixelFormat32bppARGB);
{$IFDEF MSWINDOWS}
var
  bitmap: GpBitmap;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  bitmap := NIL;
  ErrorCheck( GdipCreateBitmapFromScan0(width,
                                                     height,
                                                     0,
                                                     format,
                                                     NIL,
                                                     bitmap));

  SetNativeImage(bitmap);
{$ENDIF}
end;

{$IFDEF MSWINDOWS}
constructor TIGPBitmap.Create(width, height: Integer; target: TIGPGraphics);
var bitmap: GpBitmap;
begin
  bitmap := NIL;
  ErrorCheck( GdipCreateBitmapFromGraphics(width,
                                                        height,
                                                        target.FNativeGraphics,
                                                        bitmap));

  SetNativeImage(bitmap);
end;

function TIGPBitmap.Clone( const rect: TIGPRect; format: TIGPPixelFormat) : TIGPBitmap;
begin
  Result := Clone(rect.X, rect.Y, rect.Width, rect.Height, format);
end;

function TIGPBitmap.Clone(x, y, width, height: Integer; format: TIGPPixelFormat) : TIGPBitmap;
var
  bitmap: TIGPBitmap;
  gpdstBitmap: GpBitmap;
begin
  gpdstBitmap := NIL;
  ErrorCheck( GdipCloneBitmapAreaI(
                             x,
                             y,
                             width,
                             height,
                             format,
                             GpBitmap(FNativeImage),
                             gpdstBitmap));

 bitmap := TIGPBitmap.CreateGdiPlus(gpdstBitmap, False);
 if (bitmap = NIL) then
   GdipDisposeImage(gpdstBitmap);
     
 Result := bitmap;
end;

function TIGPBitmap.CloneF( const rect: TIGPRectF; format: TIGPPixelFormat) : TIGPBitmap;
begin
  Result := CloneF(rect.X, rect.Y, rect.Width, rect.Height, format);
end;

function TIGPBitmap.CloneF(x, y, width, height: Single; format: TIGPPixelFormat) : TIGPBitmap;
var
  gpdstBitmap: GpBitmap;
begin
  gpdstBitmap := NIL;
  ErrorCheck( GdipCloneBitmapArea(
                             x,
                             y,
                             width,
                             height,
                             format,
                             GpBitmap(FNativeImage),
                             gpdstBitmap));

 Result := TIGPBitmap.CreateGdiPlus(gpdstBitmap, False);
 if (Result = NIL) then
   GdipDisposeImage(gpdstBitmap);
     
end;

procedure TIGPBitmap.LockBitsInternal( const rect: TIGPRect; flags: Cardinal; format: TIGPPixelFormat; var AData : TIGPBitmapDataRecord );
begin
  ErrorCheck( GdipBitmapLockBits(
                                  GpBitmap(FNativeImage),
                                  @rect,
                                  flags,
                                  format,
                                  @AData));
end;

function TIGPBitmap.UnlockBits(var lockedBitmapData: TIGPBitmapDataRecord) : TIGPBitmap;
begin
  ErrorCheck( GdipBitmapUnlockBits(
                                  GpBitmap(FNativeImage),
                                  @lockedBitmapData));
                                    
  Result := Self;
end;

function TIGPBitmap.LockBits( const rect: TIGPRect; flags : TIGPImageLockModes; format: TIGPPixelFormat ) : IGPBitmapData;
var
  ABitmapData : TIGPBitmapData;
  CFlags      : Cardinal;
  AMode       : TIGPImageLockMode;

begin
  ABitmapData := TIGPBitmapData.Create( Self );
  CFlags := 0;
  for AMode in flags do
    CFlags := CFlags or ( 1 shl Ord( AMode ));

  LockBitsInternal( rect, CFlags, format, ABitmapData.FData );
  Result := ABitmapData;
end;

function TIGPBitmap.LockBits( flags : TIGPImageLockModes; format: TIGPPixelFormat ) : IGPBitmapData;
var
  ABitmapData : TIGPBitmapData;
  CFlags      : Cardinal;
  AMode       : TIGPImageLockMode;

begin
  ABitmapData := TIGPBitmapData.Create( Self );
  CFlags := 0;
  for AMode in flags do
    CFlags := CFlags or ( 1 shl Ord( AMode ));

  LockBitsInternal( TIGPRect.Create( 0, 0, GetWidth(), GetHeight() ), CFlags, format, ABitmapData.FData );
  Result := ABitmapData;
end;

function TIGPBitmap.GetPixel(x, y: Integer) : TAlphaColor;
begin
  ErrorCheck( GdipBitmapGetPixel(GpBitmap(FNativeImage), x, y, Result ));
end;

function TIGPBitmap.SetPixel(x, y: Integer; color: TAlphaColor) : TIGPBitmap;
begin
  ErrorCheck( GdipBitmapSetPixel(
      GpBitmap(FNativeImage),
      x, y,
      color));
        
  Result := Self;
end;

procedure TIGPBitmap.SetPixelProp(x, y: Integer; color: TAlphaColor);
begin
  ErrorCheck( GdipBitmapSetPixel(
      GpBitmap(FNativeImage),
      x, y,
      color));
end;

function TIGPBitmap.SetResolution(xdpi, ydpi: Single) : TIGPBitmap;
begin
  ErrorCheck( GdipBitmapSetResolution(
      GpBitmap(FNativeImage),
      xdpi, ydpi));
        
  Result := Self;
end;

{
constructor TIGPBitmap.Create(surface: IDirectDrawSurface7);
var bitmap: GpBitmap;
begin
  bitmap := NIL;
  ErrorCheck( GdipCreateBitmapFromDirectDrawSurface(surface, bitmap));
  SetNativeImage(bitmap);
end;
}
constructor TIGPBitmap.CreateData(var gdiBitmapInfo: TBITMAPINFO; gdiBitmapData: Pointer);
var bitmap: GpBitmap;
begin
  bitmap := NIL;
  ErrorCheck( GdipCreateBitmapFromGdiDib(@gdiBitmapInfo, gdiBitmapData, bitmap));
  SetNativeImage(bitmap);
end;

constructor TIGPBitmap.CreateHBITMAP(hbm: HBITMAP; hpal: HPALETTE);
var bitmap: GpBitmap;
begin
  bitmap := NIL;
  ErrorCheck( GdipCreateBitmapFromHBITMAP(hbm, hpal, bitmap));
  SetNativeImage(bitmap);
end;

constructor TIGPBitmap.CreateHICON(hicon: HICON);
var bitmap: GpBitmap;
begin
  bitmap := NIL;
  ErrorCheck( GdipCreateBitmapFromHICON(hicon, bitmap));
  SetNativeImage(bitmap);
end;

constructor TIGPBitmap.CreateRes(hInstance: HMODULE; bitmapName: WideString);
var bitmap: GpBitmap;
begin
  bitmap := NIL;
  ErrorCheck( GdipCreateBitmapFromResource(hInstance, PWideChar(bitmapName), bitmap));
  SetNativeImage(bitmap);
end;

{
class function TIGPBitmap.FromDirectDrawSurface7(surface: IDirectDrawSurface7) : TIGPBitmap;
begin
  Result := TIGPBitmap.Create(surface);
end;
}
class function TIGPBitmap.FromBITMAPINFO(var gdiBitmapInfo: TBITMAPINFO; gdiBitmapData: Pointer) : TIGPBitmap;
begin
  Result := TIGPBitmap.CreateData(gdiBitmapInfo, gdiBitmapData);
end;

class function TIGPBitmap.FromHBITMAP(hbm: HBITMAP; hpal: HPALETTE) : TIGPBitmap;
begin
  Result := TIGPBitmap.CreateHBitmap(hbm, hpal);
end;

class function TIGPBitmap.FromHICON(hicon: HICON) : TIGPBitmap;
begin
  Result := TIGPBitmap.CreateHIcon(hicon);
end;

class function TIGPBitmap.FromResource(hInstance: HMODULE; bitmapName: WideString) : TIGPBitmap;
begin
  Result := TIGPBitmap.CreateRes(hInstance, PWideChar(bitmapName));
end;

function TIGPBitmap.GetHBITMAP( colorBackground: TAlphaColor ) : HBITMAP;
begin
  ErrorCheck( GdipCreateHBITMAPFromBitmap(
                                      GpBitmap(FNativeImage),
                                      Result,
                                      colorBackground));
end;

function TIGPBitmap.GetHICON() : HICON;
begin
  ErrorCheck( GdipCreateHICONFromBitmap(
                                      GpBitmap(FNativeImage),
                                      Result ));
end;

constructor TIGPBitmap.CreateGdiPlus(nativeBitmap: GpBitmap; Dummy : Boolean );
begin
  SetNativeImage(nativeBitmap);
end;

(**************************************************************************\
*
*   GDI+ Graphics Object
*
\**************************************************************************)

class function TIGPGraphics.FromHDC(ahdc: HDC) : TIGPGraphics;
begin
  Result := TIGPGraphics.Create(ahdc);
end;

class function TIGPGraphics.FromHDC(ahdc: HDC; hdevice: THandle) : TIGPGraphics;
begin
  Result := TIGPGraphics.Create(ahdc, hdevice);
end;

class function TIGPGraphics.FromHWND(hwnd: HWND; icm: Boolean = False) : TIGPGraphics;
begin
  Result := TIGPGraphics.Create(hwnd, icm);
end;

class function TIGPGraphics.FromImage(image: IGPImage) : TIGPGraphics;
begin
  Result := TIGPGraphics.Create(image);
end;

constructor TIGPGraphics.Create( ahdc : HDC );
var
  graphics: GpGraphics;

begin
  graphics:= NIL;
  ErrorCheck( GdipCreateFromHDC( ahdc, graphics));
  SetNativeGraphics(graphics);
end;

constructor TIGPGraphics.Create( ahdc : HDC; hdevice: THandle );
var
  graphics: GpGraphics;
    
begin
  graphics:= NIL;
  ErrorCheck( GdipCreateFromHDC2( ahdc, hdevice, graphics));
  SetNativeGraphics(graphics);
end;

constructor TIGPGraphics.Create(hwnd: HWND; icm: Boolean{ = False});
var
  graphics: GpGraphics;
    
begin
  graphics:= NIL;
  if( icm ) then
    ErrorCheck( GdipCreateFromHWNDICM(hwnd, graphics))

  else
    ErrorCheck( GdipCreateFromHWND(hwnd, graphics));
      
  SetNativeGraphics(graphics);
end;

constructor TIGPGraphics.Create(image: IGPImage);
var
  graphics: GpGraphics;
    
begin
  graphics:= NIL;
  if (image <> NIL) then
    ErrorCheck( GdipGetImageGraphicsContext(image.GetNativeImage(), graphics));
      
  SetNativeGraphics(graphics);
end;

destructor TIGPGraphics.Destroy();
begin
  GdipDeleteGraphics(FNativeGraphics);
end;

function TIGPGraphics.Flush(intention: TIGPFlushIntention = FlushIntentionFlush) : TIGPGraphics;
begin
  ErrorCheck( GdipFlush(FNativeGraphics, intention));
  Result := Self;
end;

  //------------------------------------------------------------------------
  // GDI Interop methods
  //------------------------------------------------------------------------

  // Locks the graphics until ReleaseDC is called

function TIGPGraphics.GetHDC() : HDC;
begin
  ErrorCheck( GdipGetDC(FNativeGraphics, Result));
end;

function TIGPGraphics.ReleaseHDC(ahdc: HDC) : TIGPGraphics;
begin
  ErrorCheck( GdipReleaseDC(FNativeGraphics, ahdc));
  Result := Self;
end;

  //------------------------------------------------------------------------
  // Rendering modes
  //------------------------------------------------------------------------

function TIGPGraphics.SetRenderingOrigin( const point : TPoint ) : TIGPGraphics;
begin
  ErrorCheck( GdipSetRenderingOrigin(FNativeGraphics, point.X, point.Y ));
  Result := Self;
end;

procedure TIGPGraphics.SetRenderingOriginProp( const point : TPoint );
begin
  ErrorCheck( GdipSetRenderingOrigin(FNativeGraphics, point.X, point.Y ));
end;

function TIGPGraphics.GetRenderingOrigin() : TPoint;
begin
  ErrorCheck( GdipGetRenderingOrigin(FNativeGraphics, Result.X, Result.Y ));
end;

function TIGPGraphics.SetCompositingMode(compositingMode: TIGPCompositingMode) : TIGPGraphics;
begin
  ErrorCheck( GdipSetCompositingMode(FNativeGraphics, compositingMode));
  Result := Self;
end;

procedure TIGPGraphics.SetCompositingModeProp(compositingMode: TIGPCompositingMode);
begin
  ErrorCheck( GdipSetCompositingMode(FNativeGraphics, compositingMode));
end;

function TIGPGraphics.GetCompositingMode() : TIGPCompositingMode;
begin
  ErrorCheck( GdipGetCompositingMode(FNativeGraphics, Result));
end;

function TIGPGraphics.SetCompositingQuality(compositingQuality: TIGPCompositingQuality) : TIGPGraphics;
begin
  ErrorCheck( GdipSetCompositingQuality( FNativeGraphics, compositingQuality));
  Result := Self;
end;

procedure TIGPGraphics.SetCompositingQualityProp(compositingQuality: TIGPCompositingQuality);
begin
  ErrorCheck( GdipSetCompositingQuality( FNativeGraphics, compositingQuality));
end;

function TIGPGraphics.GetCompositingQuality() : TIGPCompositingQuality;
begin
  ErrorCheck( GdipGetCompositingQuality(FNativeGraphics, Result));
end;

function TIGPGraphics.SetTextRenderingHint(newMode: TIGPTextRenderingHint) : TIGPGraphics;
begin
  ErrorCheck( GdipSetTextRenderingHint(FNativeGraphics, newMode));
  Result := Self;
end;

procedure TIGPGraphics.SetTextRenderingHintProp(newMode: TIGPTextRenderingHint);
begin
  ErrorCheck( GdipSetTextRenderingHint(FNativeGraphics, newMode));
end;

function TIGPGraphics.GetTextRenderingHint() : TIGPTextRenderingHint;
begin
  ErrorCheck( GdipGetTextRenderingHint(FNativeGraphics, Result));
end;

function TIGPGraphics.SetTextContrast(contrast: Cardinal) : TIGPGraphics;
begin
  ErrorCheck( GdipSetTextContrast(FNativeGraphics, contrast));
  Result := Self;
end;

procedure TIGPGraphics.SetTextContrastProp(contrast: Cardinal); // 0..12
begin
  ErrorCheck( GdipSetTextContrast(FNativeGraphics, contrast));
end;

function TIGPGraphics.GetTextContrast() : Cardinal;
begin
  ErrorCheck( GdipGetTextContrast(FNativeGraphics, Result));
end;

function TIGPGraphics.GetInterpolationMode() : TIGPInterpolationMode;
begin
  Result := InterpolationModeInvalid;
  ErrorCheck( GdipGetInterpolationMode(FNativeGraphics, Result ));
end;

function TIGPGraphics.SetInterpolationMode(interpolationMode: TIGPInterpolationMode) : TIGPGraphics;
begin
  ErrorCheck( GdipSetInterpolationMode(FNativeGraphics, interpolationMode));
  Result := Self;
end;

procedure TIGPGraphics.SetInterpolationModeProp(interpolationMode: TIGPInterpolationMode);
begin
  ErrorCheck( GdipSetInterpolationMode(FNativeGraphics, interpolationMode));
end;

function TIGPGraphics.GetSmoothingMode() : TIGPSmoothingMode;
begin
  Result := SmoothingModeInvalid;
  ErrorCheck( GdipGetSmoothingMode(FNativeGraphics, Result ));
end;

function TIGPGraphics.SetSmoothingMode(smoothingMode: TIGPSmoothingMode) : TIGPGraphics;
begin
  ErrorCheck( GdipSetSmoothingMode(FNativeGraphics, smoothingMode));
  Result := Self;
end;

procedure TIGPGraphics.SetSmoothingModeProp(smoothingMode: TIGPSmoothingMode);
begin
  ErrorCheck( GdipSetSmoothingMode(FNativeGraphics, smoothingMode));
end;

function TIGPGraphics.GetPixelOffsetMode() : TIGPPixelOffsetMode;
begin
  Result := PixelOffsetModeInvalid;
  ErrorCheck( GdipGetPixelOffsetMode(FNativeGraphics, Result ));
end;

function TIGPGraphics.SetPixelOffsetMode(pixelOffsetMode: TIGPPixelOffsetMode) : TIGPGraphics;
begin
  ErrorCheck( GdipSetPixelOffsetMode(FNativeGraphics, pixelOffsetMode));
  Result := Self;
end;

procedure TIGPGraphics.SetPixelOffsetModeProp(pixelOffsetMode: TIGPPixelOffsetMode);
begin
  ErrorCheck( GdipSetPixelOffsetMode(FNativeGraphics, pixelOffsetMode));
end;

  //------------------------------------------------------------------------
  // Manipulate current world transform
  //------------------------------------------------------------------------

function TIGPGraphics.SetTransform(matrix: IGPMatrix) : TIGPGraphics;
begin
  ErrorCheck( GdipSetWorldTransform(FNativeGraphics, matrix.GetNativeMatrix()));
  Result := Self;
end;

procedure TIGPGraphics.SetTransformProp(matrix: IGPMatrix);
begin
  ErrorCheck( GdipSetWorldTransform(FNativeGraphics, matrix.GetNativeMatrix()));
end;

function TIGPGraphics.ResetTransform() : TIGPGraphics;
begin
  ErrorCheck( GdipResetWorldTransform(FNativeGraphics));
  Result := Self;
end;

function TIGPGraphics.MultiplyTransform(matrix: IGPMatrix; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPGraphics;
begin
  ErrorCheck( GdipMultiplyWorldTransform(FNativeGraphics,
                              matrix.GetNativeMatrix(),
                              order));
                                
  Result := Self;
end;

function TIGPGraphics.TranslateTransform(dx, dy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPGraphics;
begin
  ErrorCheck( GdipTranslateWorldTransform(FNativeGraphics, dx, dy, order));
  Result := Self;
end;

function TIGPGraphics.ScaleTransform(sx, sy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPGraphics;
begin
  ErrorCheck( GdipScaleWorldTransform(FNativeGraphics, sx, sy, order));
  Result := Self;
end;

function TIGPGraphics.ScaleTransform(s: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPGraphics;
begin
  Result := ScaleTransform( s, s, order );
end;

function TIGPGraphics.RotateTransform(angle: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPGraphics;
begin
  ErrorCheck( GdipRotateWorldTransform(FNativeGraphics, angle, order));
  Result := Self;
end;

function TIGPGraphics.GetTransform() : IGPMatrix;
begin
  Result := TIGPMatrix.Create();
  ErrorCheck( GdipGetWorldTransform(FNativeGraphics,
                             Result.GetNativeMatrix()));

end;

function TIGPGraphics.SetTransformT(matrix: IGPMatrix) : IGPTransformable;
begin
  ErrorCheck( GdipSetWorldTransform(FNativeGraphics, matrix.GetNativeMatrix()));
  Result := Self;
end;

function TIGPGraphics.ResetTransformT() : IGPTransformable;
begin
  ErrorCheck( GdipResetWorldTransform(FNativeGraphics));
  Result := Self;
end;

function TIGPGraphics.MultiplyTransformT(matrix: IGPMatrix; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipMultiplyWorldTransform(FNativeGraphics,
                              matrix.GetNativeMatrix(),
                              order));
                                
  Result := Self;
end;

function TIGPGraphics.TranslateTransformT(dx, dy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipTranslateWorldTransform(FNativeGraphics,
                                 dx, dy, order));

  Result := Self;
end;

function TIGPGraphics.ScaleTransformT(sx, sy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipScaleWorldTransform(FNativeGraphics, sx, sy, order));
  Result := Self;
end;

function TIGPGraphics.ScaleTransformXYT(s: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  Result := ScaleTransformT( s, s, order );
end;

function TIGPGraphics.RotateTransformT(angle: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipRotateWorldTransform(FNativeGraphics, angle, order));
  Result := Self;
end;

function TIGPGraphics.SetPageUnit(unit_: TIGPUnit) : TIGPGraphics;
begin
  ErrorCheck( GdipSetPageUnit(FNativeGraphics, unit_));
  Result := Self;
end;

procedure TIGPGraphics.SetPageUnitProp(unit_: TIGPUnit);
begin
  ErrorCheck( GdipSetPageUnit(FNativeGraphics, unit_));
end;

function TIGPGraphics.SetPageScale(scale: Single) : TIGPGraphics;
begin
  ErrorCheck( GdipSetPageScale(FNativeGraphics, scale));
  Result := Self;
end;

procedure TIGPGraphics.SetPageScaleProp(scale: Single);
begin
  ErrorCheck( GdipSetPageScale(FNativeGraphics, scale));
end;

function TIGPGraphics.GetPageUnit() : TIGPUnit;
begin
  ErrorCheck( GdipGetPageUnit(FNativeGraphics, Result));
end;

function TIGPGraphics.GetPageScale() : Single;
begin
  ErrorCheck( GdipGetPageScale(FNativeGraphics, Result));
end;

function TIGPGraphics.GetDpiX() : Single;
begin
  ErrorCheck( GdipGetDpiX(FNativeGraphics, Result));
end;

function TIGPGraphics.GetDpiY() : Single;
begin
  ErrorCheck( GdipGetDpiY(FNativeGraphics, Result));
end;

function TIGPGraphics.TransformPoints(destSpace: TIGPCoordinateSpace;
             srcSpace: TIGPCoordinateSpace;
             var pts : array of TPointF ) : TIGPGraphics;
begin
  ErrorCheck( GdipTransformPoints(FNativeGraphics,
                           destSpace,
                           srcSpace,
                           @pts[ 0 ], Length( pts )));

  Result := Self;
end;

function TIGPGraphics.TransformPoints(destSpace: TIGPCoordinateSpace;
             srcSpace: TIGPCoordinateSpace;
             var pts : array of TPoint ) : TIGPGraphics;
begin
  ErrorCheck( GdipTransformPointsI(FNativeGraphics,
                            destSpace,
                            srcSpace,
                            @pts[ 0 ], Length( pts )));

  Result := Self;
end;

  //------------------------------------------------------------------------
  // GetNearestColor (for <= 8bpp surfaces).  Note: Alpha is ignored.
  //------------------------------------------------------------------------

function TIGPGraphics.GetNearestColor( AColor : TAlphaColor ) : TAlphaColor;
begin
  ErrorCheck( GdipGetNearestColor(FNativeGraphics, @AColor ));
  Result := AColor;
end;

function TIGPGraphics.DrawLineF( pen: IGPPen; x1, y1, x2, y2: Single) : TIGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawLine(FNativeGraphics, pen.GetNativePen(), x1, y1, x2, y2));
  Result := Self;
end;

function TIGPGraphics.DrawLineF( pen: IGPPen; const pt1, pt2: TPointF) : TIGPGraphics;
begin
  Result := DrawLineF(pen, pt1.X, pt1.Y, pt2.X, pt2.Y);
end;

function TIGPGraphics.DrawLinesF( pen: IGPPen; const points : array of TPointF ) : TIGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawLines(FNativeGraphics,
                         pen.GetNativePen(),
                         @points[ 0 ], Length( points )));
                           
  Result := Self;
end;

function TIGPGraphics.DrawLine( pen: IGPPen; x1, y1, x2, y2: Integer) : TIGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawLineI(FNativeGraphics,
                         pen.GetNativePen(),
                         x1, y1, x2, y2));
                           
  Result := Self;
end;

function TIGPGraphics.DrawLine( pen: IGPPen; const pt1, pt2: TPoint) : TIGPGraphics;
begin
  Result := DrawLine( pen, pt1.X, pt1.Y, pt2.X, pt2.Y );
end;

function TIGPGraphics.DrawLines( pen: IGPPen; const points : array of TPoint ) : TIGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawLinesI(FNativeGraphics,
                          pen.GetNativePen(),
                          @points[ 0 ],
                          Length( points )));
                            
  Result := Self;
end;

function TIGPGraphics.DrawArcF( pen: IGPPen; x, y, width, height, startAngle, sweepAngle: Single ) : TIGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawArc(FNativeGraphics,
                       pen.GetNativePen(),
                       x,
                       y,
                       width,
                       height,
                       startAngle,
                       sweepAngle));

  Result := Self;
end;

function TIGPGraphics.DrawArcF( pen: IGPPen; const rect: TIGPRectF; startAngle, sweepAngle: Single ) : TIGPGraphics;
begin
  Result := DrawArcF( pen, rect.X, rect.Y, rect.Width, rect.Height, startAngle, sweepAngle );
end;

function TIGPGraphics.DrawArc( pen: IGPPen; x, y, width, height: Integer; startAngle,
         sweepAngle: Single ) : TIGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawArcI(FNativeGraphics,
                        pen.GetNativePen(),
                        x,
                        y,
                        width,
                        height,
                        startAngle,
                        sweepAngle));

  Result := Self;
end;


function TIGPGraphics.DrawArc( pen: IGPPen; const rect: TIGPRect; startAngle, sweepAngle: Single ) : TIGPGraphics;
begin
  Result := DrawArc(pen, rect.X, rect.Y, rect.Width, rect.Height, startAngle, sweepAngle );
end;

function TIGPGraphics.DrawBezierF( pen: IGPPen; x1, y1, x2, y2, x3, y3, x4, y4: Single) : TIGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawBezier(FNativeGraphics,
                          pen.GetNativePen(), x1, y1,
                          x2, y2, x3, y3, x4, y4));

  Result := Self;
end;

function TIGPGraphics.DrawBezierF( pen: IGPPen; const pt1, pt2, pt3, pt4: TPointF) : TIGPGraphics;
begin
  Result := DrawBezierF(pen,
            pt1.X,
            pt1.Y,
            pt2.X,
            pt2.Y,
            pt3.X,
            pt3.Y,
            pt4.X,
            pt4.Y);

end;

function TIGPGraphics.DrawBeziersF( pen: IGPPen; const points : array of TPointF ) : TIGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawBeziers(FNativeGraphics,
                           pen.GetNativePen(),
                           @points[ 0 ],
                           Length( points )));
                             
  Result := Self;
end;

function TIGPGraphics.DrawBezier( pen: IGPPen; x1, y1, x2, y2, x3, y3, x4, y4: Integer) : TIGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawBezierI(FNativeGraphics,
                           pen.GetNativePen(),
                           x1,
                           y1,
                           x2,
                           y2,
                           x3,
                           y3,
                           x4,
                           y4));
                             
  Result := Self;
end;

function TIGPGraphics.DrawBezier( pen: IGPPen; const pt1, pt2, pt3, pt4: TPoint) : TIGPGraphics;
begin
  Result := DrawBezier(pen,
            pt1.X,
            pt1.Y,
            pt2.X,
            pt2.Y,
            pt3.X,
            pt3.Y,
            pt4.X,
            pt4.Y);

end;

function TIGPGraphics.DrawBeziers( pen: IGPPen; const points : array of TPoint ) : TIGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawBeziersI(FNativeGraphics,
                            pen.GetNativePen(),
                            @points[ 0 ],
                            Length( points )));

  Result := Self;
end;

function TIGPGraphics.DrawRectangleF( pen: IGPPen; const rect : TIGPRectF ) : TIGPGraphics;
var
  rectX : TIGPRectF;

begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  rectX := rect;
  if( rectX.Width < 0 ) then
    begin
    rectX.Width := -rectX.Width;
    rectX.X := rectX.X - rectX.Width;
    end;

  if( rectX.Height < 0 ) then
    begin
    rectX.Height := -rectX.Height;
    rectX.Y := rectX.Y - rectX.Height;
    end;

  Result := DrawRectangleF( pen, rectX.X, rectX.Y, rectX.Width, rectX.Height );
end;

function TIGPGraphics.DrawRectangleF( pen: IGPPen; x, y, width, height: Single) : TIGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  if( width < 0 ) then
    begin
    Width := -width;
    x := x - width;
    end;

  if( height < 0 ) then
    begin
    Height := -height;
    y := y - height;
    end;

  ErrorCheck( GdipDrawRectangle(FNativeGraphics,
                             pen.GetNativePen(), x, y,
                             width, height));
                             
  Result := Self;
end;

function TIGPGraphics.DrawRectanglesF( pen: IGPPen; const rects: array of TIGPRectF ) : TIGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawRectangles(FNativeGraphics,
                          pen.GetNativePen(),
                          @rects[ 0 ], Length( rects )));
                             
  Result := Self;
end;

function TIGPGraphics.DrawRectangle( pen: IGPPen; const rect: TIGPRect) : TIGPGraphics;
var
  rectX: TIGPRect;

begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  rectX := rect;

  if( rectX.Width < 0 ) then
    begin
    rectX.Width := -rectX.Width;
    Dec( rectX.X, rectX.Width );
    end;

  if( rectX.Height < 0 ) then
    begin
    rectX.Height := -rectX.Height;
    Dec( rectX.Y, rectX.Height );
    end;

  Result := DrawRectangle(pen, rectX.X, rectX.Y, rectX.Width, rectX.Height);
end;

function TIGPGraphics.DrawRectangle( pen: IGPPen; x, y, width, height: Integer) : TIGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  if( width < 0 ) then
    begin
    width := -width;
    Dec( x, width );
    end;

  if( height < 0 ) then
    begin
    height := -height;
    Dec( y, height );
    end;

  ErrorCheck( GdipDrawRectangleI(FNativeGraphics,
                          pen.GetNativePen(),
                          x,
                          y,
                          width,
                          height));
                             
  Result := Self;
end;

function TIGPGraphics.DrawRectangles( pen: IGPPen; const rects: array of TIGPRect) : TIGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawRectanglesI(FNativeGraphics,
                           pen.GetNativePen(),
                          @rects[ 0 ], Length( rects )));
                             
  Result := Self;
end;

function TIGPGraphics.DrawRectangleF( pen: IGPPen; brush: IGPBrush; const rect: TIGPRectF) : TIGPGraphics;
var
  rectX: TIGPRectF;

begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  rectX := rect;
  if( rectX.Width < 0 ) then
    begin
    rectX.Width := -rectX.Width;
    rectX.X := rectX.X - rectX.Width;
    end;

  if( rectX.Height < 0 ) then
    begin
    rectX.Height := -rectX.Height;
    rectX.Y := rectX.Y - rectX.Height;
    end;

  FillRectangleF( brush, rectX );
  Result := DrawRectangleF( pen, rectX );
end;

function TIGPGraphics.DrawRectangleF( pen: IGPPen; brush: IGPBrush; x, y, width, height: Single) : TIGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  if( width < 0 ) then
    begin
    Width := -width;
    x := x - width;
    end;

  if( height < 0 ) then
    begin
    Height := -height;
    y := y - height;
    end;

  FillRectangleF( brush, x, y, width, height );
  Result := DrawRectangleF( pen, x, y, width, height );
end;
  
function TIGPGraphics.DrawRectanglesF( pen: IGPPen; brush: IGPBrush; const rects: array of TIGPRectF ) : TIGPGraphics;
var
  ARect : TIGPRectF;

begin
  for ARect in rects do
    begin
    FillRectangleF( brush, ARect );
    DrawRectangleF( pen, ARect );
    end;

  Result := Self;
end;

function TIGPGraphics.DrawRectangle( pen: IGPPen; brush: IGPBrush; const rect: TIGPRect) : TIGPGraphics;
var
  rectX : TIGPRect;

begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  rectX := rect;
  if( rectX.Width < 0 ) then
    begin
    rectX.Width := -rectX.Width;
    Dec( rectX.X, rectX.Width );
    end;

  if( rectX.Height < 0 ) then
    begin
    rectX.Height := -rectX.Height;
    Dec( rectX.Y, rectX.Height );
    end;

  FillRectangle( brush, rectX );
  Result := DrawRectangle( pen, rectX );
end;

function TIGPGraphics.DrawRectangle( pen: IGPPen; brush: IGPBrush; x, y, width, height: Integer) : TIGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  if( width < 0 ) then
    begin
    Width := -width;
    Dec( x, width );
    end;

  if( height < 0 ) then
    begin
    Height := -height;
    Dec( y, height );
    end;

  FillRectangle( brush, x, y, width, height );
  Result := DrawRectangle( pen, x, y, width, height );
end;

function TIGPGraphics.DrawRectangles( pen: IGPPen; brush: IGPBrush; const rects: array of TIGPRect ) : TIGPGraphics;
var
  ARect : TIGPRect;

begin
  for ARect in rects do
    begin
    FillRectangle( brush, ARect );
    DrawRectangle( pen, ARect );
    end;

  Result := Self;
end;

function TIGPGraphics.DrawRoundRectangleF( pen: IGPPen; const rect: TIGPRectF; const ACornerSize : TIGPSizeF) : TIGPGraphics;
begin
  Result := DrawPath( pen, TIGPGraphicsPath.Create().AddRoundRectangleF( rect, ACornerSize ));
end;

function TIGPGraphics.DrawRoundRectangle( pen: IGPPen; const rect: TIGPRect; const ACornerSize : TIGPSize) : TIGPGraphics;
begin
  Result := DrawPath( pen, TIGPGraphicsPath.Create().AddRoundRectangle( rect, ACornerSize ));
end;

function TIGPGraphics.DrawRoundRectangleF( pen: IGPPen; brush: IGPBrush; const rect: TIGPRectF; const ACornerSize : TIGPSizeF) : TIGPGraphics;
begin
  Result := DrawPath( pen, brush, TIGPGraphicsPath.Create().AddRoundRectangleF( rect, ACornerSize ));
end;

function TIGPGraphics.DrawRoundRectangle( pen: IGPPen; brush: IGPBrush; const rect: TIGPRect; const ACornerSize : TIGPSize) : TIGPGraphics;
begin
  Result := DrawPath( pen, brush, TIGPGraphicsPath.Create().AddRoundRectangle( rect, ACornerSize ));
end;

function TIGPGraphics.DrawEllipseF( pen: IGPPen; const rect: TIGPRectF) : TIGPGraphics;
begin
  Result := DrawEllipseF( pen, rect.X, rect.Y, rect.Width, rect.Height);
end;

function TIGPGraphics.DrawEllipseF( pen: IGPPen; x, y, width, height: Single) : TIGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawEllipse(FNativeGraphics,
                           pen.GetNativePen(),
                           x,
                           y,
                           width,
                           height));
                             
  Result := Self;
end;

function TIGPGraphics.DrawEllipsesF( pen: IGPPen; const rects: array of TIGPRectF) : TIGPGraphics;
var
  ARect : TIGPRectF;

begin
  for ARect in rects do
    DrawEllipseF( pen, ARect );
        
  Result := Self;
end;

function TIGPGraphics.DrawEllipse( pen: IGPPen; const rect: TIGPRect) : TIGPGraphics;
begin
  Result := DrawEllipse( pen,
             rect.X,
             rect.Y,
             rect.Width,
             rect.Height);

end;

function TIGPGraphics.DrawEllipse( pen: IGPPen; x, y, width, height: Integer) : TIGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawEllipseI(FNativeGraphics,
                            pen.GetNativePen(),
                            x,
                            y,
                            width,
                            height));
                             
  Result := Self;
end;

function TIGPGraphics.DrawEllipses( pen: IGPPen; const rects: array of TIGPRect) : TIGPGraphics;
var
  ARect : TIGPRect;
    
begin
  for ARect in rects do
    DrawEllipse( pen, ARect );
        
  Result := Self;
end;

function TIGPGraphics.DrawEllipseF( pen: IGPPen; brush: IGPBrush; const rect: TIGPRectF) : TIGPGraphics;
begin
  FillEllipseF( brush, rect );
  Result := DrawEllipseF( pen, rect );
end;
  
function TIGPGraphics.DrawEllipseF( pen: IGPPen; brush: IGPBrush; x, y, width, height: Single) : TIGPGraphics;
begin
  FillEllipseF( brush, x, y, width, height );
  Result := DrawEllipseF( pen, x, y, width, height );
end;
  
function TIGPGraphics.DrawEllipsesF( pen: IGPPen; brush: IGPBrush; const rects: array of TIGPRectF) : TIGPGraphics;
var
  ARect : TIGPRectF;

begin
  for ARect in rects do
    begin
    FillEllipseF( brush, ARect );
    DrawEllipseF( pen, ARect );
    end;
      
  Result := Self;
end;
  
function TIGPGraphics.DrawEllipse( pen: IGPPen; brush: IGPBrush; const rect: TIGPRect) : TIGPGraphics;
begin
  FillEllipse( brush, rect );
  Result := DrawEllipse( pen, rect );
end;
  
function TIGPGraphics.DrawEllipse( pen: IGPPen; brush: IGPBrush; x, y, width, height: Integer) : TIGPGraphics;
begin
  FillEllipse( brush, x, y, width, height );
  Result := DrawEllipse( pen, x, y, width, height );
end;
  
function TIGPGraphics.DrawEllipses( pen: IGPPen; brush: IGPBrush; const rects: array of TIGPRect) : TIGPGraphics;
var
  ARect : TIGPRect;

begin
  for ARect in rects do
    begin
    FillEllipse( brush, ARect );
    DrawEllipse( pen, ARect );
    end;
      
  Result := Self;
end;

function TIGPGraphics.DrawPieF( pen: IGPPen; const rect: TIGPRectF; startAngle, sweepAngle: Single) : TIGPGraphics;
begin
  Result := DrawPieF( pen, rect.X, rect.Y, rect.Width, rect.Height, startAngle, sweepAngle );
end;

function TIGPGraphics.DrawPieF( pen: IGPPen; x, y, width, height, startAngle, sweepAngle: Single) : TIGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawPie(FNativeGraphics,
                       pen.GetNativePen(),
                       x,
                       y,
                       width,
                       height,
                       startAngle,
                       sweepAngle));
                             
  Result := Self;
end;

function TIGPGraphics.DrawPie( pen: IGPPen; const rect: TIGPRect; startAngle, sweepAngle: Single) : TIGPGraphics;
begin
  Result := DrawPie( pen, rect.X, rect.Y, rect.Width, rect.Height, startAngle, sweepAngle );
end;

function TIGPGraphics.DrawPie( pen: IGPPen; x, y, width, height: Integer;
               startAngle, sweepAngle: Single) : TIGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawPieI(FNativeGraphics,
                        pen.GetNativePen(),
                        x,
                        y,
                        width,
                        height,
                        startAngle,
                        sweepAngle));
                             
  Result := Self;
end;

function TIGPGraphics.DrawPieF( pen: IGPPen; brush: IGPBrush; const rect: TIGPRectF; startAngle, sweepAngle: Single) : TIGPGraphics;
begin
  FillPieF( brush, rect, startAngle, sweepAngle );
  Result := DrawPieF( pen, rect, startAngle, sweepAngle );
end;
  
function TIGPGraphics.DrawPieF( pen: IGPPen; brush: IGPBrush; x, y, width, height, startAngle, sweepAngle: Single) : TIGPGraphics;
begin
  FillPieF( brush, x, y, width, height, startAngle, sweepAngle );
  Result := DrawPieF( pen, x, y, width, height, startAngle, sweepAngle );
end;

function TIGPGraphics.DrawPie( pen: IGPPen; brush: IGPBrush; const rect: TIGPRect; startAngle, sweepAngle: Single) : TIGPGraphics;
begin
  FillPie( brush, rect, startAngle, sweepAngle );
  Result := DrawPie( pen, rect, startAngle, sweepAngle );
end;

function TIGPGraphics.DrawPie( pen: IGPPen; brush: IGPBrush; x, y, width, height: Integer; startAngle, sweepAngle: Single) : TIGPGraphics;
begin
  FillPie( brush, x, y, width, height, startAngle, sweepAngle );
  Result := DrawPie( pen, x, y, width, height, startAngle, sweepAngle );
end;

function TIGPGraphics.DrawPolygonF( pen: IGPPen; const points: array of TPointF) : TIGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawPolygon(FNativeGraphics,
                           pen.GetNativePen(),
                           @points[ 0 ],
                           Length( points )));

  Result := Self;
end;

function TIGPGraphics.DrawPolygon( pen: IGPPen; const points: array of TPoint) : TIGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawPolygonI(FNativeGraphics,
                            pen.GetNativePen(),
                           @points[ 0 ],
                           Length( points )));
                             
  Result := Self;
end;

function TIGPGraphics.DrawPolygonF( pen: IGPPen; brush: IGPBrush; const points: array of TPointF ) : TIGPGraphics;
begin
  FillPolygonF( brush, points );
  Result := DrawPolygonF( pen, points );
end;

function TIGPGraphics.DrawPolygonF( pen: IGPPen; brush: IGPBrush; const points: array of TPointF; fillMode: TIGPFillMode) : TIGPGraphics;
begin
  FillPolygonF( brush, points, fillMode );
  Result := DrawPolygonF( pen, points );
end;

function TIGPGraphics.DrawPolygon( pen: IGPPen; brush: IGPBrush; const points: array of TPoint ) : TIGPGraphics;
begin
  FillPolygon( brush, points );
  Result := DrawPolygon( pen, points );
end;

function TIGPGraphics.DrawPolygon( pen: IGPPen; brush: IGPBrush; const points: array of TPoint; fillMode: TIGPFillMode) : TIGPGraphics;
begin
  FillPolygon( brush, points, fillMode );
  Result := DrawPolygon( pen, points );
end;

function TIGPGraphics.DrawPath( pen: IGPPen; path: IGPGraphicsPath) : TIGPGraphics;
var
  nPen: GpPen;
  nPath: GpPath;
begin
  if( Assigned(pen) ) then
    nPen := pen.GetNativePen()

  else
    nPen  := NIL;

  if( Assigned(path) ) then
    nPath := path.GetNativePath()

  else
    nPath := NIL;
      
  ErrorCheck( GdipDrawPath(FNativeGraphics, nPen, nPath));
                             
  Result := Self;
end;

function TIGPGraphics.DrawPath( pen: IGPPen; brush: IGPBrush; path: IGPGraphicsPath) : TIGPGraphics;
begin
  FillPath( brush, path );
  Result := DrawPath( pen, path );
end;
  
function TIGPGraphics.DrawCurveF( pen: IGPPen; const points: array of TPointF) : TIGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawCurve(FNativeGraphics,
                         pen.GetNativePen(),
                         @points[ 0 ],
                         Length( points )));
                             
  Result := Self;
end;

function TIGPGraphics.DrawCurveF( pen: IGPPen; const points: array of TPointF; tension: Single) : TIGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawCurve2(FNativeGraphics,
                          pen.GetNativePen(),
                          @points[ 0 ],
                          Length( points ),
                          tension));
                             
  Result := Self;
end;

function TIGPGraphics.DrawCurveF( pen: IGPPen; const points: array of TPointF; offset,
         numberOfSegments: Integer; tension: Single = 0.5) : TIGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawCurve3(FNativeGraphics,
                          pen.GetNativePen(),
                          @points[ 0 ],
                          Length( points ),
                          offset,
                          numberOfSegments, tension));
                             
  Result := Self;
end;

function TIGPGraphics.DrawCurve( pen: IGPPen; const points: array of TPoint) : TIGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawCurveI(FNativeGraphics,
                          pen.GetNativePen(),
                         @points[ 0 ],
                         Length( points )));
                             
  Result := Self;
end;

function TIGPGraphics.DrawCurve( pen: IGPPen; const points: array of TPoint; tension: Single) : TIGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawCurve2I(FNativeGraphics,
                           pen.GetNativePen(),
                           @points[ 0 ],
                           Length( points ),
                           tension));
                             
  Result := Self;
end;

function TIGPGraphics.DrawCurve( pen: IGPPen; const points: array of TPoint; offset,
         numberOfSegments: Integer; tension: Single = 0.5) : TIGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawCurve3I(FNativeGraphics,
                           pen.GetNativePen(),
                           @points[ 0 ],
                           Length( points ),
                           offset,
                           numberOfSegments,
                           tension));
                             
  Result := Self;
end;

function TIGPGraphics.DrawClosedCurveF( pen: IGPPen; const points: array of TPointF) : TIGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawClosedCurve(FNativeGraphics,
                           pen.GetNativePen(),
                           @points[ 0 ],
                           Length( points )));
                             
  Result := Self;
end;

function TIGPGraphics.DrawClosedCurveF( pen: IGPPen; const points: array of TPointF;
         tension: Single) : TIGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawClosedCurve2(FNativeGraphics,
                           pen.GetNativePen(),
                           @points[ 0 ],
                           Length( points ),
                           tension));
                             
  Result := Self;
end;

function TIGPGraphics.DrawClosedCurve( pen: IGPPen; const points: array of TPoint) : TIGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawClosedCurveI(FNativeGraphics,
                           pen.GetNativePen(),
                           @points[ 0 ],
                           Length( points )));

  Result := Self;
end;

function TIGPGraphics.DrawClosedCurve( pen: IGPPen; const points: array of TPoint; tension: Single) : TIGPGraphics;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipDrawClosedCurve2I(FNativeGraphics,
                           pen.GetNativePen(),
                           @points[ 0 ],
                           Length( points ),
                           tension));
                             
  Result := Self;
end;

function TIGPGraphics.DrawClosedCurveF( pen: IGPPen; brush: IGPBrush; const points: array of TPointF ) : TIGPGraphics;
begin
  FillClosedCurveF( brush, points );
  Result := DrawClosedCurveF( pen, points );
end;

function TIGPGraphics.DrawClosedCurveF( pen: IGPPen; brush: IGPBrush; const points: array of TPointF; fillMode: TIGPFillMode; tension: Single = 0.5) : TIGPGraphics;
begin
  FillClosedCurveF( brush, points, fillMode, tension );
  Result := DrawClosedCurveF( pen, points, tension );
end;

function TIGPGraphics.DrawClosedCurve( pen: IGPPen; brush: IGPBrush; const points: array of TPoint ) : TIGPGraphics;
begin
  FillClosedCurve( brush, points );
  Result := DrawClosedCurve( pen, points );
end;

function TIGPGraphics.DrawClosedCurve( pen: IGPPen; brush: IGPBrush; const points: array of TPoint; fillMode: TIGPFillMode; tension: Single = 0.5) : TIGPGraphics;
begin
  FillClosedCurve( brush, points, fillMode, tension );
  Result := DrawClosedCurve( pen, points, tension );
end;

function TIGPGraphics.Clear(color: TAlphaColor) : TIGPGraphics;
begin
  ErrorCheck( GdipGraphicsClear(
      FNativeGraphics,
      color));
                             
  Result := Self;
end;

function TIGPGraphics.FillRectangleF(brush: IGPBrush; const rect: TIGPRectF) : TIGPGraphics;
begin
  Result := FillRectangleF(brush, rect.X, rect.Y, rect.Width, rect.Height);
end;

function TIGPGraphics.FillRectangleF(brush: IGPBrush; x, y, width, height: Single) : TIGPGraphics;
begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipFillRectangle(FNativeGraphics,
                             brush.GetNativeBrush(), x, y,
                             width, height));
                             
  Result := Self;
end;

function TIGPGraphics.FillRectanglesF(brush: IGPBrush; const rects: array of TIGPRectF) : TIGPGraphics;
begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipFillRectangles(FNativeGraphics,
                          brush.GetNativeBrush(),
                           @rects[ 0 ],
                           Length( rects )));
                             
  Result := Self;
end;

function TIGPGraphics.FillRectangle(brush: IGPBrush; const rect: TIGPRect) : TIGPGraphics;
begin
  Result := FillRectangle(brush, rect.X, rect.Y, rect.Width, rect.Height);
end;

function TIGPGraphics.FillRectangle(brush: IGPBrush; x, y, width, height: Integer) : TIGPGraphics;
begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipFillRectangleI(FNativeGraphics,
                          brush.GetNativeBrush(),
                          x,
                          y,
                          width,
                          height));
                             
  Result := Self;
end;

function TIGPGraphics.FillRectangles(brush: IGPBrush; const rects: array of TIGPRect) : TIGPGraphics;
begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipFillRectanglesI(FNativeGraphics,
                           brush.GetNativeBrush(),
                           @rects[ 0 ],
                           Length( rects )));
                             
  Result := Self;
end;

function TIGPGraphics.FillRoundRectangleF(brush: IGPBrush; const rect: TIGPRectF; const ACornerSize : TIGPSizeF) : TIGPGraphics;
begin
  Result := FillPath( brush, TIGPGraphicsPath.Create().AddRoundRectangleF( rect, ACornerSize ));
end;

function TIGPGraphics.FillRoundRectangle(brush: IGPBrush; const rect: TIGPRect; const ACornerSize : TIGPSize) : TIGPGraphics;
begin
  Result := FillPath( brush, TIGPGraphicsPath.Create().AddRoundRectangle( rect, ACornerSize ));
end;

function TIGPGraphics.FillPolygonF(brush: IGPBrush; const points: array of TPointF) : TIGPGraphics;
begin
  Result := FillPolygonF(brush, points, FillModeAlternate);
end;

function TIGPGraphics.FillPolygonF(brush: IGPBrush; const points: array of TPointF;
             fillMode: TIGPFillMode) : TIGPGraphics;
begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipFillPolygon(FNativeGraphics,
                           brush.GetNativeBrush(),
                           @points[ 0 ], Length( points ),
                           fillMode));
                             
  Result := Self;
end;

function TIGPGraphics.FillPolygon(brush: IGPBrush; const points: array of TPoint) : TIGPGraphics;
begin
  Result := FillPolygon(brush, points, FillModeAlternate);
end;

function TIGPGraphics.FillPolygon(brush: IGPBrush; const points: array of TPoint;
             fillMode: TIGPFillMode) : TIGPGraphics;
begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipFillPolygonI(FNativeGraphics,
                           brush.GetNativeBrush(),
                           @points[ 0 ], Length( points ),
                           fillMode));
                             
  Result := Self;
end;

function TIGPGraphics.FillEllipseF(brush: IGPBrush; const rect: TIGPRectF) : TIGPGraphics;
begin
  Result := FillEllipseF(brush, rect.X, rect.Y, rect.Width, rect.Height);
end;

function TIGPGraphics.FillEllipseF(brush: IGPBrush; x, y, width, height: Single) : TIGPGraphics;
begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipFillEllipse(FNativeGraphics,
                           brush.GetNativeBrush(), x, y,
                           width, height));
                             
  Result := Self;
end;

function TIGPGraphics.FillEllipsesF(brush: IGPBrush; const rects: array of TIGPRectF) : TIGPGraphics;
var
  ARect : TIGPRectF;

begin
  for ARect in rects do
    FillEllipseF( brush, ARect );
        
  Result := Self;
end;

function TIGPGraphics.FillEllipse(brush: IGPBrush; const rect: TIGPRect) : TIGPGraphics;
begin
  Result := FillEllipse(brush, rect.X, rect.Y, rect.Width, rect.Height);
end;

function TIGPGraphics.FillEllipse(brush: IGPBrush; x, y, width, height: Integer) : TIGPGraphics;
begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipFillEllipseI(FNativeGraphics,
                            brush.GetNativeBrush(),
                            x,
                            y,
                            width,
                            height));
                             
  Result := Self;
end;

function TIGPGraphics.FillEllipses(brush: IGPBrush; const rects: array of TIGPRect ) : TIGPGraphics;
var
  ARect : TIGPRect;

begin
  for ARect in rects do
    FillEllipse( brush, ARect );
        
  Result := Self;
end;

function TIGPGraphics.FillPieF(brush: IGPBrush; const rect: TIGPRectF; startAngle, sweepAngle: Single) : TIGPGraphics;
begin
  Result := FillPieF(brush, rect.X, rect.Y, rect.Width, rect.Height, startAngle, sweepAngle);
end;

function TIGPGraphics.FillPieF( brush: IGPBrush; x, y, width, height, startAngle, sweepAngle: Single ) : TIGPGraphics;
begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipFillPie(FNativeGraphics,
                       brush.GetNativeBrush(), x, y,
                       width, height, startAngle,
                       sweepAngle));
                             
  Result := Self;
end;

function TIGPGraphics.FillPie( brush: IGPBrush; const rect: TIGPRect; startAngle, sweepAngle: Single ) : TIGPGraphics;
begin
  Result := FillPie( brush, rect.X, rect.Y, rect.Width, rect.Height, startAngle, sweepAngle );
end;

function TIGPGraphics.FillPie(brush: IGPBrush; x, y, width, height: Integer; startAngle,
         sweepAngle: Single) : TIGPGraphics;
begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipFillPieI(FNativeGraphics,
                        brush.GetNativeBrush(),
                        x,
                        y,
                        width,
                        height,
                        startAngle,
                        sweepAngle));
                             
  Result := Self;
end;

function TIGPGraphics.FillPath( brush: IGPBrush; path: IGPGraphicsPath ) : TIGPGraphics;
begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipFillPath(FNativeGraphics,
                        brush.GetNativeBrush(),
                        path.GetNativePath()));
                             
  Result := Self;
end;

function TIGPGraphics.FillClosedCurveF( brush: IGPBrush; const points: array of TPointF ) : TIGPGraphics;
begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipFillClosedCurve(FNativeGraphics,
                           brush.GetNativeBrush(),
                           @points[ 0 ], Length( points )));

  Result := Self;
end;

function TIGPGraphics.FillClosedCurveF(brush: IGPBrush; const points: array of TPointF;
             fillMode: TIGPFillMode; tension: Single = 0.5) : TIGPGraphics;
begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipFillClosedCurve2(FNativeGraphics,
                            brush.GetNativeBrush(),
                            @points[ 0 ], Length( points ),
                            tension, fillMode));
                             
  Result := Self;
end;

function TIGPGraphics.FillClosedCurve(brush: IGPBrush; const points: array of TPoint) : TIGPGraphics;
begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipFillClosedCurveI(FNativeGraphics,
                           brush.GetNativeBrush(),
                           @points[ 0 ], Length( points )));
                             
  Result := Self;
end;

function TIGPGraphics.FillClosedCurve(brush: IGPBrush; const points: array of TPoint;
              fillMode: TIGPFillMode; tension: Single = 0.5) : TIGPGraphics;

begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipFillClosedCurve2I(FNativeGraphics,
                            brush.GetNativeBrush(),
                            @points[ 0 ], Length( points ),
                            tension, fillMode));
                             
  Result := Self;
end;

function TIGPGraphics.FillRegion(brush: IGPBrush; region: IGPRegion) : TIGPGraphics;
begin
  if( brush = NIL ) then
    ErrorCheck( InvalidParameter );

  ErrorCheck( GdipFillRegion(FNativeGraphics,
                          brush.GetNativeBrush(),
                          region.GetNativeRegion() ));
                             
  Result := Self;
end;


(*
function TIGPGraphics.DrawString(string_: WideString; font: IGPFont;
  const layoutRect: TIGPRectF; stringFormat: TIGPStringFormat; pen: IGPPen) : TIGPGraphics;
var
  APath : IGPGraphicsPath;
  AOldUnit : TIGPUnit;

begin
  APath := TIGPGraphicsPath.Create();
  APath.StartFigure();
  APath.AddString( string_, font.GetFamily(), font.GetStyle(), font.GetSize(), layoutRect, stringFormat );
  APath.CloseFigure();

  AOldUnit := GetPageUnit();
  SetPageUnit( UnitPoint );
  DrawPath( pen, APath );
  SetPageUnit( AOldUnit );
  Result := Self;
end;

function TIGPGraphics.DrawString(string_: WideString; font: IGPFont;
  const origin: TPointF; pen: IGPPen) : TIGPGraphics;
//  var
//    APath  : IGPGraphicsPath;
//    ABrush : TIGPSolidBrush;
//    AOldUnit : TIGPUnit;
var
  I : Integer;
  J : Integer;

begin
//    ABrush := TIGPSolidBrush.Create( MakeColor( clBlue ) );
  for I := 0 to 2 do
    for J := 0 to 2 do
      FillString( string_, font, MakePoint( origin.X - 1 + I, origin.X - 1 + J ), pen.GetBrush() );
      
{
  APath := TIGPGraphicsPath.Create();
  APath.StartFigure();
  APath.AddString( string_, font.GetFamily(), font.GetStyle(), font.GetSize(), origin, NIL );
  APath.CloseFigure();

//    AOldUnit := GetPageUnit();
//    SetPageUnit( UnitPoint );
  DrawPath( pen, APath );
//    SetPageUnit( AOldUnit );
}
  Result := Self;
end;

function TIGPGraphics.DrawString(string_: WideString; font: IGPFont;
  const origin: TPointF; stringFormat: TIGPStringFormat; pen: IGPPen) : TIGPGraphics;
var
  APath : IGPGraphicsPath;
  AOldUnit : TIGPUnit;

begin
  APath := TIGPGraphicsPath.Create();
  APath.StartFigure();
  APath.AddString( string_, font.GetFamily(), font.GetStyle(), font.GetSize(), origin, stringFormat );
  APath.CloseFigure();

  AOldUnit := GetPageUnit();
  SetPageUnit( UnitPoint );
  DrawPath( pen, APath );
  SetPageUnit( AOldUnit );
  Result := Self;
end;
*)

function TIGPGraphics.DrawStringF(
  string_: WideString;
  font: IGPFont;
  const layoutRect: TIGPRectF;
  stringFormat: IGPStringFormat;
  brush: IGPBrush) : TIGPGraphics;
var
  nFont: GpFont;
  nStringFormat: GpStringFormat;
  nBrush: GpBrush;

begin
  if( Assigned(font)) then
    nfont := font.GetNativeFont()

  else
    nfont := NIL;

  if( Assigned(stringFormat)) then
    nstringFormat := stringFormat.GetNativeFormat()
      
  else
    nstringFormat := NIL;

  if( Assigned(brush)) then
    nbrush := brush.GetNativeBrush()

  else
    nbrush := NIL;
      
  ErrorCheck( GdipDrawString(
      FNativeGraphics,
      PWideChar(string_),
      Length( string_ ),
      nfont,
      @layoutRect,
      nstringFormat,
      nbrush));
                             
  Result := Self;
end;

function TIGPGraphics.DrawStringF(
  string_: WideString;
  font: IGPFont;
  const layoutRect: TIGPRectF;
  brush: IGPBrush
  ) : TIGPGraphics;
var
  nFont: GpFont;
  nBrush: GpBrush;

begin
  if( Assigned(font)) then
    nfont := font.GetNativeFont()

  else
    nfont := NIL;

  if( Assigned(brush)) then
    nbrush := brush.GetNativeBrush()

  else
    nbrush := NIL;

  ErrorCheck( GdipDrawString(
      FNativeGraphics,
      PWideChar(string_),
      Length( string_ ),
      nfont,
      @layoutRect,
      NIL,
      nbrush));

  Result := Self;
end;

function TIGPGraphics.DrawStringF(
  string_: WideString;
  font: IGPFont;
  const origin: TPointF;
  brush: IGPBrush
  ) : TIGPGraphics;
var
  rect: TIGPRectF;
  nfont: Gpfont;
  nBrush: GpBrush;
begin
  rect.X := origin.X;
  rect.Y := origin.Y;
  rect.Width := 0.0;
  rect.Height := 0.0;
  if( Assigned(font)) then
    nfont := font.GetNativeFont()

  else
    nfont := NIL;

  if( Assigned(Brush)) then
    nBrush := Brush.GetNativeBrush()

  else
    nBrush := NIL;

  ErrorCheck( GdipDrawString(
      FNativeGraphics,
      PWideChar(string_),
      Length( string_ ),
      nfont,
      @rect,
      NIL,
      nbrush));
                             
  Result := Self;
end;

function TIGPGraphics.DrawStringF(
  string_: WideString;
  font: IGPFont;
  const origin: TPointF;
  stringFormat: IGPStringFormat;
  brush: IGPBrush
  ) : TIGPGraphics;
var
  rect: TIGPRectF;
  nFont: GpFont;
  nStringFormat: GpStringFormat;
  nBrush: GpBrush;
begin
  rect.X := origin.X;
  rect.Y := origin.Y;
  rect.Width := 0.0;
  rect.Height := 0.0;
  if( Assigned(font)) then
    nfont := font.GetNativeFont()

  else
    nfont := NIL;

  if( Assigned(stringFormat)) then
    nstringFormat := stringFormat.GetNativeFormat()

  else
    nstringFormat := NIL;

  if( Assigned(brush)) then
    nbrush := brush.GetNativeBrush()

  else
    nbrush := NIL;

  ErrorCheck( GdipDrawString(
      FNativeGraphics,
      PWideChar(string_),
      Length( string_ ),
      nfont,
      @rect,
      nstringFormat,
      nbrush));
                             
  Result := Self;
end;

//  procedure TIGPGraphics.MeasureString(string_: WideString; length: Integer; font: IGPFont;
//       const layoutRect: TIGPRectF; stringFormat: TIGPStringFormat; out boundingBox: TIGPRectF;
//       codepointsFitted: PInteger = NIL; linesFilled: PInteger = NIL);
function TIGPGraphics.GetStringBoundingBoxF(
    string_: WideString;
    font: IGPFont;
    const layoutRect: TIGPRectF;
    stringFormat: IGPStringFormat;
    codepointsFitted: PInteger = NIL;
    linesFilled: PInteger = NIL
  ):
TIGPRectF;

var
  nFont: GpFont;
  nStringFormat: GpStringFormat;
begin
  if( Assigned(font)) then
    nfont := font.GetNativeFont()

  else
    nfont := NIL;
      
  if( Assigned(stringFormat)) then
    nstringFormat := stringFormat.GetNativeFormat()

  else
    nstringFormat := NIL;
      
  ErrorCheck( GdipMeasureString(
      FNativeGraphics,
      PWideChar(string_),
      Length( string_ ),
      nfont,
      @layoutRect,
      nstringFormat,
      @Result,
      codepointsFitted,
      linesFilled ));
end;


function TIGPGraphics.DrawString(string_: WideString; font: IGPFont;
const layoutRect: TIGPRect; stringFormat: IGPStringFormat; brush: IGPBrush) : TIGPGraphics;
begin
  Result := DrawStringF( string_, font, TIGPRectF.Create( layoutRect ), stringFormat, brush );
end;

function TIGPGraphics.DrawString(string_: WideString; font: IGPFont;
      const layoutRect: TIGPRect; brush: IGPBrush) : TIGPGraphics;
begin
  Result := DrawStringF( string_, font, TIGPRectF.Create( layoutRect ), brush );
end;

{$IFDEF FPC}
// ---------------- jacek ----------------------
function jp_GetPointF(const Point: TPoint): TPointF; overload;
begin
  Result.SetLocation(Point);
end;

function jp_GetPointF(const Point: TPointF): TPointF; overload;
begin
  Result.SetLocation(Point);
end;

function jp_GetPointF(const x, y: integer): TPointF; overload;
begin
  Result.SetLocation(x, y);
end;
{$ENDIF}



function TIGPGraphics.DrawString(
  string_: WideString;
  font: IGPFont;
  const origin: TPoint;
  brush: IGPBrush
 ) : TIGPGraphics;
begin
  {$IFDEF FPC}
  Result := DrawStringF( string_, font, jp_GetPointF( origin ), brush );
  {$ELSE}
  Result := DrawStringF( string_, font, TPointF.Create( origin ), brush );
  {$ENDIF}
end;

function TIGPGraphics.DrawString(string_: WideString; font: IGPFont;
const origin: TPoint; stringFormat: IGPStringFormat; brush: IGPBrush) : TIGPGraphics;
begin
  {$IFDEF FPC}
  Result := DrawStringF( string_, font, jp_GetPointF( origin ), stringFormat, brush );
  {$ELSE}
  Result := DrawStringF( string_, font, TPointF.Create( origin ), stringFormat, brush );
  {$ENDIF}
end;

function TIGPGraphics.GetStringSizeF(string_: WideString; font: IGPFont;
    stringFormat: IGPStringFormat = NIL) : TIGPSizeF;
var
  ARect : TIGPRectF;

begin
  {$IFDEF FPC}
  ARect := GetStringBoundingBoxF( string_, font, jp_GetPointF( 0, 0 ), stringFormat );
  {$ELSE}
  ARect := GetStringBoundingBoxF( string_, font, TPointF.Create( 0, 0 ), stringFormat );
  {$ENDIF}
  Result.Width := ARect.Width;
  Result.Height := ARect.Height; 
end;

//  procedure TIGPGraphics.MeasureString(string_: WideString; length: Integer; font: IGPFont;
//       const layoutRectSize: TIGPSizeF; stringFormat: TIGPStringFormat; out size: TIGPSizeF;
//       codepointsFitted: PInteger = NIL; linesFilled: PInteger = NIL);
function TIGPGraphics.GetStringSizeF(string_: WideString; font: IGPFont;
    const layoutRectSize: TIGPSizeF; stringFormat: IGPStringFormat = NIL;
    codepointsFitted: PInteger = NIL; linesFilled: PInteger = NIL) : TIGPSizeF;
var
  layoutRect, boundingBox: TIGPRectF;
  nFont: GpFont;
  nStringFormat: GpStringFormat;
begin
  layoutRect.X := 0;
  layoutRect.Y := 0;
  layoutRect.Width := layoutRectSize.Width;
  layoutRect.Height := layoutRectSize.Height;

  if( Assigned(font)) then
    nfont := font.GetNativeFont()

  else
    nfont := NIL;

  if( Assigned(stringFormat)) then
    nstringFormat := stringFormat.GetNativeFormat()

  else
    nstringFormat := NIL;

  ErrorCheck( GdipMeasureString(
      FNativeGraphics,
      PWideChar(string_),
      Length( string_ ),
      nfont,
      @layoutRect,
      nstringFormat,
      @boundingBox,
      codepointsFitted,
      linesFilled
  ));

  Result.Width  := boundingBox.Width;
  Result.Height := boundingBox.Height;                            
end;


//  procedure TIGPGraphics.MeasureString(string_: WideString ; length: Integer; font: IGPFont;
//       const origin: TPointF; stringFormat: IGPStringFormat; out boundingBox: TIGPRectF);
function TIGPGraphics.GetStringBoundingBoxF(string_: WideString; font: IGPFont;
    const origin: TPointF; stringFormat: IGPStringFormat ) : TIGPRectF;
var
  rect: TIGPRectF;
  nFont: GpFont;
  nstringFormat: GpstringFormat;
begin
  rect.X := origin.X;
  rect.Y := origin.Y;
  rect.Width := 0.0;
  rect.Height := 0.0;

  if( Assigned(font)) then
    nfont := font.GetNativeFont()

  else
    nfont := NIL;

  if( Assigned(stringFormat)) then
    nstringFormat := stringFormat.GetNativeFormat()

  else
    nstringFormat := NIL;

  ErrorCheck( GdipMeasureString(
      FNativeGraphics,
      PWideChar(string_),
      Length( string_ ),
      nfont,
      @rect,
      nstringFormat,
      @Result,
      NIL,
      NIL
  ));
end;

    
//  procedure TIGPGraphics.MeasureString(string_: WideString; length: Integer; font: IGPFont;
//       const layoutRect: TIGPRectF; out boundingBox: TIGPRectF);
function TIGPGraphics.GetStringBoundingBoxF(string_: WideString; font: IGPFont;
    const layoutRect: TIGPRectF ) : TIGPRectF;
      
var
  nFont: GpFont;
    
begin
  if( Assigned(font)) then
    nfont := font.GetNativeFont()

  else
    nfont := NIL;
      
  ErrorCheck( GdipMeasureString(
      FNativeGraphics,
      PWideChar(string_),
      Length( string_ ),
      nfont,
      @layoutRect,
      NIL,
      @Result,
      NIL,
      NIL
  ));
end;

    
//  procedure TIGPGraphics.MeasureString(string_: WideString; length: Integer; font: IGPFont;
//       const origin: TPointF; out boundingBox: TIGPRectF);
function TIGPGraphics.GetStringBoundingBoxF(string_: WideString; font: IGPFont;
    const origin: TPointF ) : TIGPRectF;
var
  nFont: GpFont;
  rect: TIGPRectF;
begin
  if( Assigned(font)) then
    nfont := font.GetNativeFont()

  else
    nfont := NIL;
      
  rect.X := origin.X;
  rect.Y := origin.Y;
  rect.Width := 0.0;
  rect.Height := 0.0;

  ErrorCheck( GdipMeasureString(
      FNativeGraphics,
      PWideChar(string_),
      Length( string_ ),
      nfont,
      @rect,
      NIL,
      @Result,
      NIL,
      NIL
  ));
end;

function TIGPGraphics.MeasureStringF(string_: WideString; font: IGPFont;
  stringFormat: IGPStringFormat = NIL ) : TIGPSizeF;
begin
  Result := GetStringSizeF( string_, font, stringFormat );
end;

function TIGPGraphics.MeasureStringF(string_: WideString; font: IGPFont;
  const layoutRectSize: TIGPSizeF; stringFormat: IGPStringFormat = NIL;
  codepointsFitted: PInteger = NIL; linesFilled: PInteger = NIL) : TIGPSizeF;
begin
  Result := GetStringSizeF( string_, font, layoutRectSize, stringFormat, codepointsFitted, linesFilled );
end;

function TIGPGraphics.MeasureStringF(string_: WideString; font: IGPFont;
  const layoutRect: TIGPRectF; stringFormat: IGPStringFormat;
  codepointsFitted: PInteger = NIL; linesFilled: PInteger = NIL) : TIGPRectF;
begin
  Result := GetStringBoundingBoxF( string_, font, layoutRect, stringFormat, codepointsFitted, linesFilled );
end;

function TIGPGraphics.MeasureStringF(string_: WideString; font: IGPFont;
  const origin: TPointF; stringFormat: IGPStringFormat ) : TIGPRectF;
begin
  Result := GetStringBoundingBoxF( string_, font, origin, stringFormat );
end;

function TIGPGraphics.MeasureStringF(string_: WideString; font: IGPFont;
  const layoutRect: TIGPRectF ) : TIGPRectF;
begin
  Result := GetStringBoundingBoxF( string_, font, layoutRect ); 
end;

function TIGPGraphics.MeasureStringF(string_: WideString; font: IGPFont;
  const origin: TPointF ) : TIGPRectF;
begin
  Result := GetStringBoundingBoxF( string_, font, origin );
end;

function TIGPGraphics.MeasureCharacterRangesF(string_: WideString; font: IGPFont;
    const layoutRect: TIGPRectF; stringFormat: IGPStringFormat ) : TArray<IGPRegion>;
var
  nativeRegions: Pointer;
  i: Integer;
  nFont: GpFont;
  nstringFormat: GpstringFormat;
  regionCount : Integer;
  ARanges : array of TIGPCharacterRange;

type
  TArrayGpRegion = array of GpRegion;

begin
  if( Assigned(font) ) then
    nfont := font.GetNativeFont()

  else
    nfont := NIL;

  if( not Assigned(stringFormat) ) then
    begin
    stringFormat := TIGPStringFormat.Create();
    SetLength( ARanges, Length( string_ ));
    for i := 0 to Length( string_ ) - 1 do
      begin
      ARanges[ i ].First := i;
      ARanges[ i ].Length := 1;
      end;

    stringFormat.SetMeasurableCharacterRanges( ARanges );
    end;

  nstringFormat := stringFormat.GetNativeFormat();

  regionCount := stringFormat.GetMeasurableCharacterRangeCount();
  GetMem(nativeRegions, Sizeof(GpRegion)* regionCount);

  SetLength( Result, regionCount ); 

  for i := 0 to regionCount - 1 do
    begin
    Result[i] := TIGPRegion.Create();
    TArrayGpRegion(nativeRegions)[i] := Result[i].GetNativeRegion();
    end;

  ErrorCheck( GdipMeasureCharacterRanges(
      FNativeGraphics,
      PWideChar(string_),
      Length( string_ ),
      nfont,
      @layoutRect,
      nstringFormat,
      regionCount,
      nativeRegions
  ));

  FreeMem(nativeRegions, Sizeof(GpRegion)* regionCount);
end;

function TIGPGraphics.MeasureCharacterRangesF(string_: WideString; font: IGPFont;
      const origin: TPointF; stringFormat: IGPStringFormat = NIL ) : TArray<IGPRegion>;
begin
  Result := MeasureCharacterRangesF( string_, font, GetStringBoundingBoxF( string_, font, origin, stringFormat ), stringFormat );
end;

function TIGPGraphics.MeasureCharacterRangesF(string_: WideString; font: IGPFont;
      stringFormat: IGPStringFormat = NIL ) : TArray<IGPRegion>;
begin
  {$IFDEF FPC}
  Result := MeasureCharacterRangesF( string_, font, GetStringBoundingBoxF( string_, font, jp_GetPointF( 0, 0 ), stringFormat ), stringFormat );
  {$ELSE}
  Result := MeasureCharacterRangesF( string_, font, GetStringBoundingBoxF( string_, font, TPointF.Create( 0, 0 ), stringFormat ), stringFormat );
  {$ENDIF}
end;

function TIGPGraphics.MeasureCharacterRangesF(string_: WideString; font: IGPFont;
  const layoutRect: TIGPRectF; const ranges : array of TIGPCharacterRange; stringFormat: IGPStringFormat = NIL ) : TArray<IGPRegion>;
var
  nativeRegions: Pointer;
  i: Integer;
  nFont: GpFont;
  nstringFormat: GpstringFormat;
  regionCount : Integer;
  AClonedStringFormat : IGPStringFormat;

type
  TArrayGpRegion = array of GpRegion;

begin
  if( Assigned(font) ) then
    nfont := font.GetNativeFont()

  else
    nfont := NIL;

  if( Assigned(stringFormat) ) then
    AClonedStringFormat := stringFormat.Clone()

  else
    AClonedStringFormat := TIGPStringFormat.Create();

  AClonedStringFormat.SetMeasurableCharacterRanges( ranges );
  nstringFormat := AClonedStringFormat.GetNativeFormat();

  regionCount := AClonedStringFormat.GetMeasurableCharacterRangeCount();
  GetMem(nativeRegions, Sizeof(GpRegion)* regionCount);

  SetLength( Result, regionCount );

  for i := 0 to regionCount - 1 do
    begin
    Result[i] := TIGPRegion.Create();
    TArrayGpRegion(nativeRegions)[i] := Result[i].GetNativeRegion();
    end;

  ErrorCheck( GdipMeasureCharacterRanges(
      FNativeGraphics,
      PWideChar(string_),
      Length( string_ ),
      nfont,
      @layoutRect,
      nstringFormat,
      regionCount,
      nativeRegions
  ));

  FreeMem(nativeRegions, Sizeof(GpRegion)* regionCount);
end;

function TIGPGraphics.MeasureCharacterRangesF(string_: WideString; font: IGPFont;
  const origin: TPointF; const ranges : array of TIGPCharacterRange; stringFormat: IGPStringFormat = NIL ) : TArray<IGPRegion>;
begin
  Result := MeasureCharacterRangesF( string_, font, GetStringBoundingBoxF( string_, font, origin, stringFormat ), ranges, stringFormat );
end;

function TIGPGraphics.MeasureCharacterRangesF(string_: WideString; font: IGPFont;
  const ranges : array of TIGPCharacterRange; stringFormat: IGPStringFormat = NIL ) : TArray<IGPRegion>;
begin
  {$IFDEF FPC}
  Result := MeasureCharacterRangesF( string_, font, GetStringBoundingBoxF( string_, font, jp_GetPointF( 0, 0 ), stringFormat ), ranges, stringFormat );
  {$ELSE}
  Result := MeasureCharacterRangesF( string_, font, GetStringBoundingBoxF( string_, font, TPointF.Create( 0, 0 ), stringFormat ), ranges, stringFormat );
  {$ENDIF}
end;

function TIGPGraphics.DrawDriverString(text: PUINT16; length: Integer; font: IGPFont;
     brush: IGPBrush; positions: PGPPointF; flags: Integer;
     matrix: IGPMatrix) : TIGPGraphics;
var
  nfont: Gpfont;
  nbrush: Gpbrush;
  nmatrix: Gpmatrix;
begin
  if( Assigned(font)) then
    nfont := font.GetNativeFont()

  else
    nfont := NIL;

  if( Assigned(brush)) then
    nbrush := brush.GetNativeBrush()

  else
    nbrush := NIL;

  if( Assigned(matrix)) then
    nmatrix := matrix.GetNativeMatrix()

  else
    nmatrix := NIL;

  ErrorCheck( GdipDrawDriverString(
      FNativeGraphics,
      text,
      length,
      nfont,
      nbrush,
      positions,
      flags,
      nmatrix));
                             
  Result := Self;
end;

//  function TIGPGraphics.MeasureDriverString(text: PUINT16; length: Integer; font: IGPFont;
//       positions: PGPPointF; flags: Integer; matrix: IGPMatrix;
//       out boundingBox: TIGPRectF);
function TIGPGraphics.GetDriverStringBoundingBoxF(text: PUINT16; length: Integer; font: IGPFont;
     positions: PGPPointF; flags: Integer; matrix: IGPMatrix ) : TIGPRectF;
var
  nfont: Gpfont;
  nmatrix: Gpmatrix;
begin
  if( Assigned(font)) then
    nfont := font.GetNativeFont()

  else
    nfont := NIL;
      
  if( Assigned(matrix)) then
    nmatrix := matrix.GetNativeMatrix()

  else
    nmatrix := NIL;

  ErrorCheck( GdipMeasureDriverString(
      FNativeGraphics,
      text,
      length,
      nfont,
      positions,
      flags,
      nmatrix,
      @Result
  ));
end;

  // Draw a cached bitmap on this graphics destination offset by
  // x, y. Note this will fail with WrongState if the CachedBitmap
  // native format differs from this Graphics.

function TIGPGraphics.DrawCachedBitmap(cb: IGPCachedBitmap;  x, y: Integer) : TIGPGraphics;
begin
  ErrorCheck( GdipDrawCachedBitmap(
      FNativeGraphics,
      cb.GetNativeCachedBitmap(),
      x, y
  ));
                             
  Result := Self;
end;

function TIGPGraphics.DrawImageF(image: IGPImage; const point: TPointF) : TIGPGraphics;
begin
  Result := DrawImageF(image, point.X, point.Y);
end;

function TIGPGraphics.DrawImageF(image: IGPImage; x, y: Single) : TIGPGraphics;
var
 nImage: GpImage;
begin
  if( Assigned(Image)) then
    nImage := Image.GetNativeImage()

  else
    nImage := NIL;

  ErrorCheck( GdipDrawImage(FNativeGraphics, nImage, x, y));
  Result := Self;
end;

function TIGPGraphics.DrawImageF(image: IGPImage; const rect: TIGPRectF) : TIGPGraphics;
begin
  Result := DrawImageF(image, rect.X, rect.Y, rect.Width, rect.Height);
end;

function TIGPGraphics.DrawImageF(image: IGPImage; x, y, width, height: Single) : TIGPGraphics;
var
 nImage: GpImage;
begin
  if( Assigned(Image)) then
    nImage := Image.GetNativeImage()

  else
    nImage := NIL;

  ErrorCheck( GdipDrawImageRect(FNativeGraphics,
                             nImage,
                             x,
                             y,
                             width,
                             height));
                             
  Result := Self;
end;

function TIGPGraphics.DrawImage(image: IGPImage; const point: TPoint) : TIGPGraphics;
begin
  Result := DrawImage(image, point.X, point.Y);
end;

function TIGPGraphics.DrawImage(image: IGPImage; x, y: Integer) : TIGPGraphics;
var
 nImage: GpImage;
begin
  if( Assigned(Image)) then
    nImage := Image.GetNativeImage()

  else
    nImage := NIL;
      
  ErrorCheck( GdipDrawImageI(FNativeGraphics,
                          nimage,
                          x,
                          y));
                             
  Result := Self;
end;

function TIGPGraphics.DrawImage(image: IGPImage; const rect: TIGPRect) : TIGPGraphics;
begin
  Result := DrawImage(image,
           rect.X,
           rect.Y,
           rect.Width,
           rect.Height);
end;

function TIGPGraphics.DrawImage(image: IGPImage; x, y, width, height: Integer) : TIGPGraphics;
var
 nImage: GpImage;
begin
  if( Assigned(Image)) then
    nImage := Image.GetNativeImage()

  else
    nImage := NIL;

  ErrorCheck( GdipDrawImageRectI(FNativeGraphics,
                          nimage,
                          x,
                          y,
                          width,
                          height));
                             
  Result := Self;
end;

function TIGPGraphics.DrawImageF( image: IGPImage; const point: TPointF; Opacity : Single ) : TIGPGraphics;
var
  AAlphaMatrix : TIGPColorMatrix;

begin
  AAlphaMatrix := StandardAlphaMatrix;
  AAlphaMatrix[ 3, 3 ] := Opacity;
  DrawImageF( image, TIGPRectF.Create( point.X, point.Y, image.Width, image.Height ), 0, 0, image.Width, image.Height, UnitPixel, TIGPImageAttributes.Create().SetColorMatrix( AAlphaMatrix ));
  Result := Self;
end;

function TIGPGraphics.DrawImageF(image: IGPImage; x, y: Single; Opacity : Single ) : TIGPGraphics;
var
  AAlphaMatrix : TIGPColorMatrix;

begin
  AAlphaMatrix := StandardAlphaMatrix;
  AAlphaMatrix[ 3, 3 ] := Opacity;
  DrawImageF( image, TIGPRectF.Create( x, y, image.Width, image.Height ), 0, 0, image.Width, image.Height, UnitPixel, TIGPImageAttributes.Create().SetColorMatrix( AAlphaMatrix ));
  Result := Self;
end;

function TIGPGraphics.DrawImageF(image: IGPImage; const rect: TIGPRectF; Opacity : Single ) : TIGPGraphics;
var
  AAlphaMatrix : TIGPColorMatrix;

begin
  AAlphaMatrix := StandardAlphaMatrix;
  AAlphaMatrix[ 3, 3 ] := Opacity;
  DrawImageF( image, TIGPRectF.Create( rect.X, rect.Y, rect.Width, rect.Height ), 0, 0, image.Width, image.Height, UnitPixel, TIGPImageAttributes.Create().SetColorMatrix( AAlphaMatrix ));
  Result := Self;
end;

function TIGPGraphics.DrawImageF(image: IGPImage; x, y, width, height: Single; Opacity : Single ) : TIGPGraphics;
var
  AAlphaMatrix : TIGPColorMatrix;

begin
  AAlphaMatrix := StandardAlphaMatrix;
  AAlphaMatrix[ 3, 3 ] := Opacity;
  DrawImageF( image, TIGPRectF.Create( x, y, width, height ), 0, 0, image.Width, image.Height, UnitPixel, TIGPImageAttributes.Create().SetColorMatrix( AAlphaMatrix ));
  Result := Self;
end;

function TIGPGraphics.DrawImage(image: IGPImage; const point: TPoint; Opacity : Single ) : TIGPGraphics;
var
  AAlphaMatrix : TIGPColorMatrix;

begin
  AAlphaMatrix := StandardAlphaMatrix;
  AAlphaMatrix[ 3, 3 ] := Opacity;
  DrawImage( image, TIGPRect.Create( point.X, point.Y, image.Width, image.Height ), 0, 0, image.Width, image.Height, UnitPixel, TIGPImageAttributes.Create().SetColorMatrix( AAlphaMatrix ));
  Result := Self;
end;

function TIGPGraphics.DrawImage(image: IGPImage; x, y: Integer; Opacity : Single ) : TIGPGraphics;
var
  AAlphaMatrix : TIGPColorMatrix;

begin
  AAlphaMatrix := StandardAlphaMatrix;
  AAlphaMatrix[ 3, 3 ] := Opacity;
  DrawImage( image, TIGPRect.Create( x, y, image.Width, image.Height ), 0, 0, image.Width, image.Height, UnitPixel, TIGPImageAttributes.Create().SetColorMatrix( AAlphaMatrix ));
  Result := Self;
end;

function TIGPGraphics.DrawImage(image: IGPImage; const rect: TIGPRect; Opacity : Single ) : TIGPGraphics;
var
  AAlphaMatrix : TIGPColorMatrix;

begin
  AAlphaMatrix := StandardAlphaMatrix;
  AAlphaMatrix[ 3, 3 ] := Opacity;
  DrawImage( image, TIGPRect.Create( rect.X, rect.Y, rect.Width, rect.Height ), 0, 0, image.Width, image.Height, UnitPixel, TIGPImageAttributes.Create().SetColorMatrix( AAlphaMatrix ));
  Result := Self;
end;

function TIGPGraphics.DrawImage(image: IGPImage; x, y, width, height: Integer; Opacity : Single ) : TIGPGraphics;
var
  AAlphaMatrix : TIGPColorMatrix;

begin
  AAlphaMatrix := StandardAlphaMatrix;
  AAlphaMatrix[ 3, 3 ] := Opacity;
  DrawImage( image, TIGPRect.Create( x, y, width, height ), 0, 0, image.Width, image.Height, UnitPixel, TIGPImageAttributes.Create().SetColorMatrix( AAlphaMatrix ));
  Result := Self;
end;

  // Affine Draw Image
  // destPoints.length = 3: rect => parallelogram
  //     destPoints[0] <=> top-left corner of the source rectangle
  //     destPoints[1] <=> top-right corner
  //     destPoints[2] <=> bottom-left corner
  // destPoints.length = 4: rect => quad
  //     destPoints[3] <=> bottom-right corner

function TIGPGraphics.DrawImageF(image: IGPImage; const destPoints: array of TPointF ) : TIGPGraphics;
var
 nImage: GpImage;
 count : Integer;
   
begin                            
  count := Length( destPoints );
  if ((count <> 3) and (count <> 4)) then
    ErrorCheck( InvalidParameter);

  if( Assigned(Image)) then
    nImage := Image.GetNativeImage()

  else
    nImage := NIL;

  ErrorCheck( GdipDrawImagePoints(FNativeGraphics,
                           nimage,
                           @destPoints[ 0 ], count));
                           
  Result := Self;
end;

function TIGPGraphics.DrawImage(image: IGPImage; const destPoints: array of TPoint) : TIGPGraphics;
var
 nImage: GpImage;
 count : Integer;
   
begin
  count := Length( destPoints );
  if ((count <> 3) and (count <> 4)) then
    ErrorCheck( InvalidParameter);

  if( Assigned(Image)) then
    nImage := Image.GetNativeImage()

  else
    nImage := NIL;

  ErrorCheck( GdipDrawImagePointsI(FNativeGraphics,
                            nimage,
                            @destPoints[ 0 ],
                            count));
                            
  Result := Self;
end;

function TIGPGraphics.DrawImageF(image: IGPImage; x, y, srcx, srcy, srcwidth, srcheight: Single;
      srcUnit: TIGPUnit) : TIGPGraphics;
var
  nImage: GpImage;
begin
  if( Assigned(Image)) then
    nImage := Image.GetNativeImage()

  else
    nImage := NIL;
      
  ErrorCheck( GdipDrawImagePointRect(FNativeGraphics,
                              nimage,
                              x, y,
                              srcx, srcy,
                              srcwidth, srcheight, srcUnit));
                             
  Result := Self;
end;

function TIGPGraphics.DrawImageF(image: IGPImage; const destRect: TIGPRectF; srcx, srcy, srcwidth, srcheight: Single;
     srcUnit: TIGPUnit; imageAttributes: IGPImageAttributes = NIL; callback: TIGPDrawImageAbortProc = NIL) : TIGPGraphics;
var
  nImage: GpImage;
  nimageAttributes: GpimageAttributes;
  ADispatcher : TIGPIntAbortDispatcher;
  ADispatcherIntf : IGPIntAbortDispatcher;
begin
  if( Assigned(Image)) then
    nImage := Image.GetNativeImage()

  else
    nImage := NIL;
      
  if( Assigned(imageAttributes)) then
    nimageAttributes := imageAttributes.GetNativeImageAttr()

  else
    nimageAttributes := NIL;
      
  ADispatcher := TIGPIntAbortDispatcher.Create();
  ADispatcherIntf := ADispatcher; 
  ErrorCheck( GdipDrawImageRectRect(FNativeGraphics,
                             nimage,
                             destRect.X,
                             destRect.Y,
                             destRect.Width,
                             destRect.Height,
                             srcx, srcy,
                             srcwidth, srcheight,
                             srcUnit,
                             nimageAttributes,
                             GLGPAbortCallback,
                             ADispatcher ));
                             
  Result := Self;
end;

function TIGPGraphics.DrawImageF(image: IGPImage; const destPoints: array of TPointF;
     srcx, srcy, srcwidth, srcheight: Single; srcUnit: TIGPUnit;
     imageAttributes: IGPImageAttributes = NIL; callback: TIGPDrawImageAbortProc = NIL) : TIGPGraphics;
var
  nImage: GpImage;
  nimageAttributes: GpimageAttributes;
  ADispatcher : TIGPIntAbortDispatcher;
  ADispatcherIntf : IGPIntAbortDispatcher;
begin
  if( Assigned(Image)) then
    nImage := Image.GetNativeImage()

  else
    nImage := NIL;
      
  if( Assigned(imageAttributes)) then
    nimageAttributes := imageAttributes.GetNativeImageAttr()

  else
    nimageAttributes := NIL;
      
  ADispatcher := TIGPIntAbortDispatcher.Create();
  ADispatcherIntf := ADispatcher; 
  ErrorCheck( GdipDrawImagePointsRect(FNativeGraphics,
                               nimage,
                               @destPoints[ 0 ],
                               Length( destPoints ),
                               srcx, srcy,
                               srcwidth,
                               srcheight,
                               srcUnit,
                               nimageAttributes,
                               GLGPAbortCallback,
                               ADispatcher ));

  Result := Self;
end;

function TIGPGraphics.DrawImage(image: IGPImage; x, y, srcx, srcy, srcwidth, srcheight: Integer;
     srcUnit: TIGPUnit) : TIGPGraphics;
var
  nImage: GpImage;
begin
  if( Assigned(Image)) then
    nImage := Image.GetNativeImage()

  else
    nImage := NIL;
      
  ErrorCheck( GdipDrawImagePointRectI(FNativeGraphics,
                               nimage,
                               x,
                               y,
                               srcx,
                               srcy,
                               srcwidth,
                               srcheight,
                               srcUnit));
                             
  Result := Self;
end;

function TIGPGraphics.DrawImage(image: IGPImage; const destRect: TIGPRect; srcx, srcy, srcwidth,
     srcheight: Integer; srcUnit: TIGPUnit; imageAttributes: IGPImageAttributes = NIL;
     callback: TIGPDrawImageAbortProc = NIL) : TIGPGraphics;
var
  nImage: GpImage;
  nimageAttributes: GpimageAttributes;
  ADispatcher : TIGPIntAbortDispatcher;
  ADispatcherIntf : IGPIntAbortDispatcher;
begin
  if( Assigned(Image)) then
    nImage := Image.GetNativeImage()

  else
    nImage := NIL;
      
  if( Assigned(imageAttributes)) then
    nimageAttributes := imageAttributes.GetNativeImageAttr()

  else
    nimageAttributes := NIL;
      
  ADispatcher := TIGPIntAbortDispatcher.Create();
  ADispatcherIntf := ADispatcher; 
  ErrorCheck( GdipDrawImageRectRectI(FNativeGraphics,
                              nimage,
                              destRect.X,
                              destRect.Y,
                              destRect.Width,
                              destRect.Height,
                              srcx,
                              srcy,
                              srcwidth,
                              srcheight,
                              srcUnit,
                              nimageAttributes,
                              GLGPAbortCallback,
                              ADispatcher ));

  Result := Self;
end;

function TIGPGraphics.DrawImage(image: IGPImage; const destPoints: array of TPoint;
     srcx, srcy, srcwidth, srcheight: Integer; srcUnit: TIGPUnit;
     imageAttributes: IGPImageAttributes = NIL; callback: TIGPDrawImageAbortProc = NIL) : TIGPGraphics;
var
  nImage: GpImage;
  nimageAttributes: GpimageAttributes;
  ADispatcher : TIGPIntAbortDispatcher;
  ADispatcherIntf : IGPIntAbortDispatcher;
    
begin
  if( Assigned(Image)) then
    nImage := Image.GetNativeImage()

  else
    nImage := NIL;
      
  if( Assigned(imageAttributes)) then
    nimageAttributes := imageAttributes.GetNativeImageAttr()

  else
    nimageAttributes := NIL;

  ADispatcher := TIGPIntAbortDispatcher.Create();
  ADispatcherIntf := ADispatcher; 
  ErrorCheck( GdipDrawImagePointsRectI(FNativeGraphics,
                                nimage,
                                @destPoints[ 0 ],
                                Length( destPoints ),
                                srcx,
                                srcy,
                                srcwidth,
                                srcheight,
                                srcUnit,
                                nimageAttributes,
                                GLGPAbortCallback,
                                ADispatcher ));
                             
  Result := Self;
end;

type
  IGPIntDispatcher = interface
    ['{F2608F3E-119F-45BE-B73E-0CE219FC4A83}']
  end;
    
  TIGPIntDispatcher = class( TInterfacedObject, IGPIntDispatcher )
  public
    OnCallback : TIGPEnumerateMetafileProc;

  public
    function GPCallback( recordType: TIGPEmfPlusRecordType; flags: UINT;
      dataSize: UINT; data: PBYTE ) : BOOL;

  end;

function TIGPIntDispatcher.GPCallback( recordType: TIGPEmfPlusRecordType; flags: UINT; dataSize: UINT; data: PBYTE ) : BOOL;
begin
  if( Assigned( OnCallback )) then
    Result := OnCallback( recordType, flags, dataSize, data )

  else
    Result := False;

end;
  
function GLGPCallback(recordType: TIGPEmfPlusRecordType; flags: UINT;
  dataSize: UINT; data: PBYTE; callbackData: Pointer) : BOOL; stdcall;
begin
  if( callbackData <> NIL ) then 
    Result := TIGPIntDispatcher( callbackData ).GPCallback( recordType, flags, dataSize, data )

  else
    Result := False;

end;

  // The following methods are for playing an EMF+ to a graphics
  // via the enumeration interface.  Each record of the EMF+ is
  // sent to the callback (along with the callbackData).  Then
  // the callback can invoke the Metafile::PlayRecord method
  // to play the particular record.

    
function TIGPGraphics.EnumerateMetafileF(metafile: IGPMetafile; const destPoint: TPointF;
    callback: TIGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TIGPGraphics;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpimageAttributes;
  ADispatcher : TIGPIntDispatcher;
  ADispatcherIntf : IGPIntDispatcher;
    
begin
  if( Assigned(Metafile)) then
    nMetafile := GpMetafile(Metafile.GetNativeImage())

  else
    nMetafile := NIL;
      
  if( Assigned(imageAttributes)) then
    nimageAttributes := imageAttributes.GetNativeImageAttr()

  else
    nimageAttributes := NIL;
      
  ADispatcher := TIGPIntDispatcher.Create();
  ADispatcherIntf := ADispatcher; 
  ErrorCheck( GdipEnumerateMetafileDestPoint(
          FNativeGraphics,
          nmetafile,
          @destPoint,
          GLGPCallback,
          ADispatcher,
          nimageAttributes));
            
  Result := Self;
end;

    
function TIGPGraphics.EnumerateMetafile(metafile: IGPMetafile; const destPoint: TPoint;
     callback: TIGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TIGPGraphics;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpimageAttributes;
  ADispatcher : TIGPIntDispatcher;
  ADispatcherIntf : IGPIntDispatcher;
    
begin
  if( Assigned(Metafile)) then
    nMetafile := GpMetafile(Metafile.GetNativeImage())

  else
    nMetafile := NIL;
      
  if( Assigned(imageAttributes)) then
    nimageAttributes := imageAttributes.GetNativeImageAttr()

  else
    nimageAttributes := NIL;
      
  ADispatcher := TIGPIntDispatcher.Create();
  ADispatcherIntf := ADispatcher; 
  ErrorCheck( GdipEnumerateMetafileDestPointI(
          FNativeGraphics,
          nmetafile,
          @destPoint,
          GLGPCallback,
          ADispatcher,
          nimageAttributes));
            
  Result := Self;
end;


function TIGPGraphics.EnumerateMetafileF(metafile: IGPMetafile; const destRect: TIGPRectF;
     callback: TIGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TIGPGraphics;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpimageAttributes;
  ADispatcher : TIGPIntDispatcher;
  ADispatcherIntf : IGPIntDispatcher;
    
begin
  if( Assigned(Metafile)) then
    nMetafile := GpMetafile(Metafile.GetNativeImage())

  else
    nMetafile := NIL;
      
  if( Assigned(imageAttributes)) then
    nimageAttributes := imageAttributes.GetNativeImageAttr()

  else
    nimageAttributes := NIL;
      
  ADispatcher := TIGPIntDispatcher.Create();
  ADispatcherIntf := ADispatcher; 
  ErrorCheck( GdipEnumerateMetafileDestRect(
          FNativeGraphics,
          nmetafile,
          @destRect,
          GLGPCallback,
          ADispatcher,
          nimageAttributes));
            
  Result := Self;
end;


function TIGPGraphics.EnumerateMetafile(metafile: IGPMetafile; const destRect: TIGPRect;
     callback: TIGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TIGPGraphics;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpimageAttributes;
  ADispatcher : TIGPIntDispatcher;
  ADispatcherIntf : IGPIntDispatcher;
    
begin
  if( Assigned(Metafile)) then
    nMetafile := GpMetafile(Metafile.GetNativeImage())

  else
    nMetafile := NIL;
      
  if( Assigned(imageAttributes)) then
    nimageAttributes := imageAttributes.GetNativeImageAttr()

  else
    nimageAttributes := NIL;
      
  ADispatcher := TIGPIntDispatcher.Create();
  ADispatcherIntf := ADispatcher; 
  ErrorCheck( GdipEnumerateMetafileDestRectI(
          FNativeGraphics,
          nmetafile,
          @destRect,
          GLGPCallback,
          ADispatcher,
          nimageAttributes));

  Result := Self;
end;


function TIGPGraphics.EnumerateMetafileF(metafile: IGPMetafile; const destPoints: array of TPointF;
     callback: TIGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TIGPGraphics;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpimageAttributes;
  ADispatcher : TIGPIntDispatcher;
  ADispatcherIntf : IGPIntDispatcher;
    
begin
  if( Assigned(Metafile)) then
    nMetafile := GpMetafile(Metafile.GetNativeImage())

  else
    nMetafile := NIL;
      
  if( Assigned(imageAttributes)) then
    nimageAttributes := imageAttributes.GetNativeImageAttr()

  else
    nimageAttributes := NIL;
      
  ADispatcher := TIGPIntDispatcher.Create();
  ADispatcherIntf := ADispatcher; 
  ErrorCheck( GdipEnumerateMetafileDestPoints(
          FNativeGraphics,
          nmetafile,
          @destPoints[ 0 ],
          Length( destPoints ),
          GLGPCallback,
          ADispatcher,
          nimageAttributes));

  Result := Self;
end;

    
function TIGPGraphics.EnumerateMetafile(metafile: IGPMetafile; const destPoints: array of TPoint;
     callback: TIGPEnumerateMetafileProc; imageAttributes: IGPImageAttributes = NIL) : TIGPGraphics;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpimageAttributes;
  ADispatcher : TIGPIntDispatcher;
  ADispatcherIntf : IGPIntDispatcher;
    
begin
  if( Assigned(Metafile)) then
    nMetafile := GpMetafile(Metafile.GetNativeImage())

  else
    nMetafile := NIL;

  if( Assigned(imageAttributes)) then
    nimageAttributes := imageAttributes.GetNativeImageAttr()

  else
    nimageAttributes := NIL;
      
  ADispatcher := TIGPIntDispatcher.Create();
  ADispatcherIntf := ADispatcher; 
  ErrorCheck( GdipEnumerateMetafileDestPointsI(
          FNativeGraphics,
          nmetafile,
          @destPoints[ 0 ],
          Length( destPoints ),
          GLGPCallback,
          ADispatcher,
          nimageAttributes));

  Result := Self;
end;

    
function TIGPGraphics.EnumerateMetafileF(metafile: IGPMetafile; const destPoint: TPointF;
     const srcRect: TIGPRectF; srcUnit: TIGPUnit; callback: TIGPEnumerateMetafileProc;
     imageAttributes: IGPImageAttributes = NIL) : TIGPGraphics;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpimageAttributes;
  ADispatcher : TIGPIntDispatcher;
  ADispatcherIntf : IGPIntDispatcher;
    
begin
  if( Assigned(Metafile)) then
    nMetafile := GpMetafile(Metafile.GetNativeImage())

  else
    nMetafile := NIL;
      
  if( Assigned(imageAttributes)) then
    nimageAttributes := imageAttributes.GetNativeImageAttr()

  else
    nimageAttributes := NIL;
      
  ADispatcher := TIGPIntDispatcher.Create();
  ADispatcherIntf := ADispatcher; 
  ErrorCheck( GdipEnumerateMetafileSrcRectDestPoint(
          FNativeGraphics,
          nmetafile,
          @destPoint,
          @srcRect,
          srcUnit,
          GLGPCallback,
          ADispatcher,
          nimageAttributes));

  Result := Self;
end;

    
function TIGPGraphics.EnumerateMetafile(metafile : IGPMetafile; const destPoint : TPoint;
     const srcRect : TIGPRect; srcUnit : TIGPUnit; callback : TIGPEnumerateMetafileProc;
     imageAttributes : IGPImageAttributes = NIL) : TIGPGraphics;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpimageAttributes;
  ADispatcher : TIGPIntDispatcher;
  ADispatcherIntf : IGPIntDispatcher;
    
begin
  if( Assigned(Metafile)) then
    nMetafile := GpMetafile(Metafile.GetNativeImage())

  else
    nMetafile := NIL;
      
  if( Assigned(imageAttributes)) then
    nimageAttributes := imageAttributes.GetNativeImageAttr()

  else
    nimageAttributes := NIL;
      
  ADispatcher := TIGPIntDispatcher.Create();
  ADispatcherIntf := ADispatcher; 
  ErrorCheck( GdipEnumerateMetafileSrcRectDestPointI(
          FNativeGraphics,
          nmetafile,
          @destPoint,
          @srcRect,
          srcUnit,
          GLGPCallback,
          ADispatcher,
          nimageAttributes));

  Result := Self;
end;


function TIGPGraphics.EnumerateMetafileF(metafile: IGPMetafile; const destRect: TIGPRectF;
     const srcRect: TIGPRectF; srcUnit: TIGPUnit; callback: TIGPEnumerateMetafileProc;
     imageAttributes: IGPImageAttributes = NIL) : TIGPGraphics;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpimageAttributes;
  ADispatcher : TIGPIntDispatcher;
  ADispatcherIntf : IGPIntDispatcher;
    
begin
  if( Assigned(Metafile)) then
    nMetafile := GpMetafile(Metafile.GetNativeImage())

  else
    nMetafile := NIL;
      
  if( Assigned(imageAttributes)) then
    nimageAttributes := imageAttributes.GetNativeImageAttr()

  else
    nimageAttributes := NIL;
      
  ADispatcher := TIGPIntDispatcher.Create();
  ADispatcherIntf := ADispatcher; 
  ErrorCheck( GdipEnumerateMetafileSrcRectDestRect(
          FNativeGraphics,
          nmetafile,
          @destRect,
          @srcRect,
          srcUnit,
          GLGPCallback,
          ADispatcher,
          nimageAttributes));

  Result := Self;
end;


function TIGPGraphics.EnumerateMetafile(metafile : IGPMetafile; const destRect, srcRect: TIGPRect;
     srcUnit : TIGPUnit; callback : TIGPEnumerateMetafileProc; imageAttributes : IGPImageAttributes = NIL) : TIGPGraphics;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpimageAttributes;
  ADispatcher : TIGPIntDispatcher;
  ADispatcherIntf : IGPIntDispatcher;
    
begin
  if( Assigned(Metafile)) then
    nMetafile := GpMetafile(Metafile.GetNativeImage())

  else
    nMetafile := NIL;
      
  if( Assigned(imageAttributes)) then
    nimageAttributes := imageAttributes.GetNativeImageAttr()

  else
    nimageAttributes := NIL;
      
  ADispatcher := TIGPIntDispatcher.Create();
  ADispatcherIntf := ADispatcher; 
  ErrorCheck( GdipEnumerateMetafileSrcRectDestRectI(
          FNativeGraphics,
          nmetafile,
          @destRect,
          @srcRect,
          srcUnit,
          GLGPCallback,
          ADispatcher,
          nimageAttributes));

  Result := Self;
end;

    
function TIGPGraphics.EnumerateMetafileF( metafile: IGPMetafile; const destPoints: array of TPointF;
  const srcRect: TIGPRectF; srcUnit: TIGPUnit; callback: TIGPEnumerateMetafileProc;
  imageAttributes: IGPImageAttributes = NIL) : TIGPGraphics;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpimageAttributes;
  ADispatcher : TIGPIntDispatcher;
  ADispatcherIntf : IGPIntDispatcher;
    
begin
  if( Assigned(Metafile)) then
    nMetafile := GpMetafile(Metafile.GetNativeImage())

  else
    nMetafile := NIL;
      
  if( Assigned(imageAttributes)) then
    nimageAttributes := imageAttributes.GetNativeImageAttr()

  else
    nimageAttributes := NIL;
      
  ADispatcher := TIGPIntDispatcher.Create();
  ADispatcherIntf := ADispatcher; 
  ErrorCheck( GdipEnumerateMetafileSrcRectDestPoints(
          FNativeGraphics,
          nmetafile,
          @destPoints[ 0 ],
          Length( destPoints ),
          @srcRect,
          srcUnit,
          GLGPCallback,
          ADispatcher,
          nimageAttributes));

  Result := Self;
end;


function TIGPGraphics.EnumerateMetafile(metafile: IGPMetafile; const destPoints: array of TPoint;
  const srcRect: TIGPRect; srcUnit: TIGPUnit; callback: TIGPEnumerateMetafileProc;
  imageAttributes: IGPImageAttributes = NIL) : TIGPGraphics;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpimageAttributes;
  ADispatcher : TIGPIntDispatcher;
  ADispatcherIntf : IGPIntDispatcher;
    
begin
  if( Assigned(Metafile)) then
    nMetafile := GpMetafile(Metafile.GetNativeImage())

  else
    nMetafile := NIL;
      
  if( Assigned(imageAttributes)) then
    nimageAttributes := imageAttributes.GetNativeImageAttr()

  else
    nimageAttributes := NIL;
      
  ADispatcher := TIGPIntDispatcher.Create();
  ADispatcherIntf := ADispatcher; 
  ErrorCheck( GdipEnumerateMetafileSrcRectDestPointsI(
          FNativeGraphics,
          nmetafile,
          @destPoints[ 0 ],
          Length( destPoints ),
          @srcRect,
          srcUnit,
          GLGPCallback,
          ADispatcher,
          nimageAttributes));

  Result := Self;
end;
    
function TIGPGraphics.SetClip(g: IGPGraphics; combineMode: TIGPCombineMode = CombineModeReplace) : TIGPGraphics;
begin
  ErrorCheck( GdipSetClipGraphics(FNativeGraphics,
                           g.GetNativeGraphics(),
                           combineMode));

  Result := Self;
end;

function TIGPGraphics.SetClipF( const rect: TIGPRectF; combineMode: TIGPCombineMode = CombineModeReplace) : TIGPGraphics;
begin
  ErrorCheck( GdipSetClipRect(FNativeGraphics,
                           rect.X, rect.Y,
                           rect.Width, rect.Height,
                           combineMode));

  Result := Self;
end;

function TIGPGraphics.SetClip( const rect: TIGPRect; combineMode: TIGPCombineMode = CombineModeReplace) : TIGPGraphics;
begin
  ErrorCheck( GdipSetClipRectI(FNativeGraphics,
                            rect.X, rect.Y,
                            rect.Width, rect.Height,
                            combineMode));

  Result := Self;
end;

function TIGPGraphics.SetClip(path: IGPGraphicsPath; combineMode: TIGPCombineMode = CombineModeReplace) : TIGPGraphics;
begin
  ErrorCheck( GdipSetClipPath(FNativeGraphics,
                           path.GetNativePath(),
                           combineMode));

  Result := Self;
end;

function TIGPGraphics.SetClip(region: IGPRegion; combineMode: TIGPCombineMode = CombineModeReplace) : TIGPGraphics;
begin
  ErrorCheck( GdipSetClipRegion(FNativeGraphics,
                             region.GetNativeRegion(),
                             combineMode));

  Result := Self;
end;

  // This is different than the other SetClip methods because it assumes
  // that the HRGN is already in device units, so it doesn't transform
  // the coordinates in the HRGN.
    
function TIGPGraphics.SetClip(hRgn: HRGN; combineMode: TIGPCombineMode = CombineModeReplace) : TIGPGraphics;
begin
  ErrorCheck( GdipSetClipHrgn(FNativeGraphics, hRgn,
                           combineMode));

  Result := Self;
end;

procedure TIGPGraphics.SetClipProp( region: IGPRegion );
begin
  SetClip( region );
end;
  
function TIGPGraphics.IntersectClipF(const rect: TIGPRectF) : TIGPGraphics;
begin
  ErrorCheck( GdipSetClipRect(FNativeGraphics,
                           rect.X, rect.Y,
                           rect.Width, rect.Height,
                           CombineModeIntersect));

  Result := Self;
end;

function TIGPGraphics.IntersectClip(const rect: TIGPRect) : TIGPGraphics;
begin
  ErrorCheck( GdipSetClipRectI(FNativeGraphics,
                            rect.X, rect.Y,
                            rect.Width, rect.Height,
                            CombineModeIntersect));

  Result := Self;
end;

function TIGPGraphics.IntersectClip(region: IGPRegion) : TIGPGraphics;
begin
  ErrorCheck( GdipSetClipRegion(FNativeGraphics,
                             region.GetNativeRegion(),
                             CombineModeIntersect));

  Result := Self;
end;

function TIGPGraphics.ExcludeClipF(const rect: TIGPRectF) : TIGPGraphics;
begin
  ErrorCheck( GdipSetClipRect(FNativeGraphics,
                           rect.X, rect.Y,
                           rect.Width, rect.Height,
                           CombineModeExclude));

  Result := Self;
end;

function TIGPGraphics.ExcludeClip(const rect: TIGPRect) : TIGPGraphics;
begin
  ErrorCheck( GdipSetClipRectI(FNativeGraphics,
                            rect.X, rect.Y,
                            rect.Width, rect.Height,
                            CombineModeExclude));

  Result := Self;
end;

function TIGPGraphics.ExcludeClip(region: IGPRegion) : TIGPGraphics;
begin
  ErrorCheck( GdipSetClipRegion(FNativeGraphics,
                             region.GetNativeRegion(),
                             CombineModeExclude));

  Result := Self;
end;

function TIGPGraphics.ResetClip() : TIGPGraphics;
begin
  ErrorCheck( GdipResetClip(FNativeGraphics));
  Result := Self;
end;

function TIGPGraphics.TranslateClipF(dx, dy: Single) : TIGPGraphics;
begin
  ErrorCheck( GdipTranslateClip(FNativeGraphics, dx, dy));
  Result := Self;
end;

function TIGPGraphics.TranslateClip(dx, dy: Integer) : TIGPGraphics;
begin
  ErrorCheck( GdipTranslateClipI(FNativeGraphics, dx, dy));
  Result := Self;
end;

function TIGPGraphics.GetClip() : IGPRegion;
begin
  Result := TIGPRegion.Create();
  ErrorCheck( GdipGetClip(FNativeGraphics,
                       Result.GetNativeRegion() ));
end;

function TIGPGraphics.GetClipBoundsF() : TIGPRectF;
begin
  ErrorCheck( GdipGetClipBounds(FNativeGraphics, @Result));
end;

function TIGPGraphics.GetClipBounds() : TIGPRect;
begin
  ErrorCheck( GdipGetClipBoundsI(FNativeGraphics, @Result));
end;

function TIGPGraphics.IsClipEmpty: Boolean;
var booln: BOOL;
begin
  booln := False;
  ErrorCheck( GdipIsClipEmpty(FNativeGraphics, @booln));
  Result := booln;
end;

function TIGPGraphics.GetVisibleClipBoundsF() : TIGPRectF;
begin
  ErrorCheck( GdipGetVisibleClipBounds(FNativeGraphics, @Result ));
end;

function TIGPGraphics.GetVisibleClipBounds() : TIGPRect;
begin
  ErrorCheck( GdipGetVisibleClipBoundsI(FNativeGraphics, @Result ));
end;

function TIGPGraphics.IsVisibleClipEmpty: Boolean;
var
  booln: BOOL;

begin
  booln := False;
  ErrorCheck( GdipIsVisibleClipEmpty(FNativeGraphics, booln));
  Result := booln;
end;

function TIGPGraphics.IsVisible(x, y: Integer) : Boolean;
var
  booln: BOOL;
  
begin
  booln := False;
  ErrorCheck( GdipIsVisiblePointI(FNativeGraphics,
                        x,
                        y,
                        booln));

  Result := booln;
end;

function TIGPGraphics.IsVisible(const point: TPoint) : Boolean;
begin
  Result := IsVisible( point.X, point.Y );
end;

function TIGPGraphics.IsVisible(x, y, width, height: Integer) : Boolean;
var booln: BOOL;
begin
  booln := True;
  ErrorCheck( GdipIsVisibleRectI(FNativeGraphics,
                       X,
                       Y,
                       Width,
                       Height,
                       booln));
  Result := booln;
end;

function TIGPGraphics.IsVisible(const rect: TIGPRect) : Boolean;
var booln: BOOL;
begin
  booln := True;
  ErrorCheck( GdipIsVisibleRectI(FNativeGraphics,
                       rect.X,
                       rect.Y,
                       rect.Width,
                       rect.Height,
                       booln));
  Result := booln;
end;

function TIGPGraphics.IsVisibleF(x, y: Single) : Boolean;
var booln: BOOL;
begin
  booln := False;
  ErrorCheck( GdipIsVisiblePoint(FNativeGraphics,
                       X,
                       Y,
                       booln));

  Result := booln;
end;

function TIGPGraphics.IsVisibleF(const point: TPointF) : Boolean;
var booln: BOOL;
begin
  booln := False;
  ErrorCheck( GdipIsVisiblePoint(FNativeGraphics,
                       point.X,
                       point.Y,
                       booln));

  Result := booln;
end;

function TIGPGraphics.IsVisibleF(x, y, width, height: Single) : Boolean;
var booln: BOOL;
begin
  booln := False;
  ErrorCheck( GdipIsVisibleRect(FNativeGraphics,
                      X,
                      Y,
                      Width,
                      Height,
                      booln));
  Result := booln;
end;

function TIGPGraphics.IsVisibleF(const rect: TIGPRectF) : Boolean;
var booln: BOOL;
begin
  booln := False;
  ErrorCheck( GdipIsVisibleRect(FNativeGraphics,
                      rect.X,
                      rect.Y,
                      rect.Width,
                      rect.Height,
                      booln));
  Result := booln;
end;

function TIGPGraphics.Save: TIGPGraphicsState;
begin
  ErrorCheck( GdipSaveGraphics(FNativeGraphics, Result));
end;

function TIGPGraphics.Restore(gstate: TIGPGraphicsState) : TIGPGraphics;
begin
  ErrorCheck( GdipRestoreGraphics(FNativeGraphics, gstate));
  Result := Self;
end;

function TIGPGraphics.BeginContainerF(const dstrect,srcrect: TIGPRectF; unit_: TIGPUnit) : TIGPGraphicsContainer;
begin
  ErrorCheck( GdipBeginContainer(FNativeGraphics, @dstrect,
                       @srcrect, unit_, Result));
end;

function TIGPGraphics.BeginContainer(const dstrect, srcrect: TIGPRect; unit_: TIGPUnit) : TIGPGraphicsContainer;
begin
  ErrorCheck( GdipBeginContainerI(FNativeGraphics, @dstrect,
                        @srcrect, unit_, Result));
end;

function TIGPGraphics.BeginContainer: TIGPGraphicsContainer;
begin
  ErrorCheck( GdipBeginContainer2(FNativeGraphics, Result));
end;

function TIGPGraphics.EndContainer(state: TIGPGraphicsContainer) : TIGPGraphics;
begin
  ErrorCheck( GdipEndContainer(FNativeGraphics, state));
  Result := Self;
end;

  // Only valid when recording metafiles.

function TIGPGraphics.AddMetafileComment( const data: array of BYTE ) : TIGPGraphics;
begin                 
  ErrorCheck( GdipComment(FNativeGraphics, Length( data ), @data[ 0 ] ));
  Result := Self;
end;

class function TIGPGraphics.GetHalftonePalette() : HPALETTE;
begin
  Result := GdipCreateHalftonePalette();
end;

constructor TIGPGraphics.CreateGdiPlus(graphics: GpGraphics; Dummy1 : Boolean; Dummy2 : Boolean );
begin
  SetNativeGraphics(graphics);
end;

procedure TIGPGraphics.SetNativeGraphics(graphics: GpGraphics);
begin
  self.FNativeGraphics := graphics;
end;

function TIGPGraphics.GetNativeGraphics: GpGraphics;
begin
  Result := self.FNativeGraphics;
end;

function TIGPGraphics.GetNativePen( pen: TIGPPen) : GpPen;
begin
  Result := pen.FNativePen;
end;

(**************************************************************************\
*
*   GDI+ Font Family class
*
\**************************************************************************)

constructor TIGPFontFamily.Create();
begin
  FNativeFamily := NIL;
end;

constructor TIGPFontFamily.Create(name: WideString; fontCollection: IGPFontCollection = NIL);
var nfontCollection: GpfontCollection;
begin
  FNativeFamily := NIL;
  if( Assigned(fontCollection)) then
    nfontCollection := fontCollection.GetNativeFontCollection()

  else
    nfontCollection := NIL;
      
  ErrorCheck( GdipCreateFontFamilyFromName(PWideChar(name), nfontCollection, FNativeFamily));
end;

destructor TIGPFontFamily.Destroy();
begin
  GdipDeleteFontFamily (FNativeFamily);
end;

class function TIGPFontFamily.GenericSansSerif: TIGPFontFamily;
var
  nFontFamily: GpFontFamily;
begin
  if (GenericSansSerifFontFamily <> NIL) then
  begin
    Result := GenericSansSerifFontFamily;
    exit;
  end;
  GenericSansSerifFontFamily := TIGPFontFamily.Create();
  ErrorCheck( GdipGetGenericFontFamilySansSerif(nFontFamily));
  GenericSansSerifFontFamily.FNativeFamily := nFontFamily;
  Result := GenericSansSerifFontFamily;
end;

class function TIGPFontFamily.GenericSerif: TIGPFontFamily;
var nFontFamily: GpFontFamily;
begin
  if (GenericSerifFontFamily <> NIL) then
  begin
    Result := GenericSerifFontFamily;
    exit;
  end;

  GenericSerifFontFamily := TIGPFontFamily.Create();// (GenericSerifFontFamilyBuffer);
  ErrorCheck( GdipGetGenericFontFamilySerif(nFontFamily));
  GenericSerifFontFamily.FNativeFamily := nFontFamily;
  Result := GenericSerifFontFamily;
end;

class function TIGPFontFamily.GenericMonospace: TIGPFontFamily;
var nFontFamily: GpFontFamily;
begin
  if (GenericMonospaceFontFamily <> NIL) then
  begin
    Result := GenericMonospaceFontFamily;
    exit;
  end;
  GenericMonospaceFontFamily := TIGPFontFamily.Create();// (GenericMonospaceFontFamilyBuffer);
  ErrorCheck( GdipGetGenericFontFamilyMonospace(nFontFamily));
  GenericMonospaceFontFamily.FNativeFamily := nFontFamily;
  Result := GenericMonospaceFontFamily;
end;

function TIGPFontFamily.GetFamilyName(language: LANGID = 0) : String;
var
  str: array[0..LF_FACESIZE - 1] of WideChar;
    
begin
  ErrorCheck( GdipGetFamilyName(FNativeFamily, PWideChar(@str), language));
  Result := str;
end;

function TIGPFontFamily.Clone() : TIGPFontFamily;
var
  clonedFamily: GpFontFamily;
begin
  clonedFamily := NIL;
  ErrorCheck( GdipCloneFontFamily (FNativeFamily, clonedFamily));
  Result := TIGPFontFamily.CreateGdiPlus(clonedFamily, False);
end;

function TIGPFontFamily.IsAvailable() : Boolean;
begin
  Result := (FNativeFamily <> NIL);
end;

function TIGPFontFamily.IsStyleAvailable(style: Integer) : Boolean;
var
  StyleAvailable: BOOL;
  AGPStatus: TIGPStatus;
begin
  AGPStatus := GdipIsStyleAvailable(FNativeFamily, style, StyleAvailable);
  if (AGPStatus <> Ok) then
    StyleAvailable := False;
      
  Result := StyleAvailable;
end;

function TIGPFontFamily.GetEmHeight(style: Integer) : UINT16;
begin
  ErrorCheck( GdipGetEmHeight(FNativeFamily, style, Result));
end;

function TIGPFontFamily.GetCellAscent(style: Integer) : UINT16;
begin
  ErrorCheck( GdipGetCellAscent(FNativeFamily, style, Result));
end;

function TIGPFontFamily.GetCellDescent(style: Integer) : UINT16;
begin
  ErrorCheck( GdipGetCellDescent(FNativeFamily, style, Result));
end;

function TIGPFontFamily.GetLineSpacing(style: Integer) : UINT16;
begin
  ErrorCheck( GdipGetLineSpacing(FNativeFamily, style, Result));
end;

constructor TIGPFontFamily.CreateGdiPlus(nativeFamily: GpFontFamily; Dummy : Boolean);
begin
  FNativeFamily := nativeFamily;
end;

function TIGPFontFamily.GetNativeFamily() : GpFontFamily;
begin
  Result := FNativeFamily;
end;
  
(**************************************************************************\
*
*   GDI+ Font class
*
\**************************************************************************)

constructor TIGPFont.Create(hdc: HDC);
var font: GpFont;
begin
  font := NIL;
  ErrorCheck( GdipCreateFontFromDC(hdc, font));
  SetNativeFont(font);
end;

constructor TIGPFont.Create(hdc: HDC; logfont: PLogFontA);
var font: GpFont;
begin
  font := NIL;
  if( Assigned(logfont)) then
    ErrorCheck( GdipCreateFontFromLogfontA(hdc, logfont, font))
      
  else
    ErrorCheck( GdipCreateFontFromDC(hdc, font));

  SetNativeFont(font);
end;

constructor TIGPFont.Create(hdc: HDC; logfont: PLogFontW);
var font: GpFont;
begin
  font := NIL;
  if( Assigned(logfont)) then
    ErrorCheck( GdipCreateFontFromLogfontW(hdc, logfont, font))
      
  else
    ErrorCheck( GdipCreateFontFromDC(hdc, font));
      
  SetNativeFont(font);
end;

constructor TIGPFont.Create(hdc: HDC; hfont: HFONT);
var
  font: GpFont;
  lf: LOGFONTA;
begin
  font := NIL;
  if Boolean(hfont) then
    begin
    if( Boolean(GetObjectA(hfont, sizeof(LOGFONTA), @lf))) then
      ErrorCheck( GdipCreateFontFromLogfontA(hdc, @lf, font))

    else
      ErrorCheck( GdipCreateFontFromDC(hdc, font));
        
    end
      
  else
    ErrorCheck( GdipCreateFontFromDC(hdc, font));

  SetNativeFont(font);
end;

constructor TIGPFont.Create(family: IGPFontFamily; emSize: Single;
    style: TFontStyles; unit_: TIGPUnit );
var
  font: GpFont;
  nFontFamily: GpFontFamily;
begin
  font := NIL;
  if( Assigned(Family)) then
    nFontFamily := Family.GetNativeFamily()

  else
    nFontFamily := NIL;
      
  ErrorCheck( GdipCreateFont(nFontFamily, emSize, PInteger(@style)^, Integer(unit_), font));
  SetNativeFont(font);
end;

constructor TIGPFont.Create(familyName: WideString; emSize: Single;
    style: TFontStyles; unit_: TIGPUnit;
    fontCollection: IGPFontCollection);
var
  family: IGPFontFamily;
  nativeFamily: GpFontFamily;
begin
  FNativeFont := NIL;
  family := TIGPFontFamily.Create(familyName, fontCollection);
  nativeFamily := family.GetNativeFamily();

  if ( GdipCreateFont(nativeFamily,
                          emSize,
                          PInteger(@style)^,
                          Integer(unit_),
                          FNativeFont) <> Ok) then
    begin
      nativeFamily := TIGPFontFamily.GenericSansSerif.FNativeFamily;

    ErrorCheck( GdipCreateFont(
          nativeFamily,
          emSize,
          PInteger(@style)^,
          Integer(unit_),
          FNativeFont));
    end;
      
end;

function TIGPFont.GetLogFontA( g: IGPGraphics ) : TLogFontA;
var
  nGraphics : GpGraphics;
    
begin
  if( Assigned(g)) then
    nGraphics := g.GetNativeGraphics()

  else
    nGraphics := NIL;
      
  ErrorCheck( GdipGetLogFontA(FNativeFont, nGraphics, Result ));
end;

function TIGPFont.GetLogFontW(g: IGPGraphics ) : TLogFontW;
var
  nGraphics: GpGraphics;
    
begin
  if( Assigned(g)) then
    nGraphics := g.GetNativeGraphics()

  else
    nGraphics := NIL;
      
  ErrorCheck( GdipGetLogFontW(FNativeFont, nGraphics, Result ));
end;

function TIGPFont.Clone() : TIGPFont;
var cloneFont: GpFont;
begin
  cloneFont := NIL;
  ErrorCheck( GdipCloneFont(FNativeFont, cloneFont));
  Result := TIGPFont.CreateGdiPlus(cloneFont, False);
end;

destructor TIGPFont.Destroy();
begin
  GdipDeleteFont(FNativeFont);
end;

function TIGPFont.IsAvailable: Boolean;
begin
  Result := (FNativeFont <> NIL);
end;

function TIGPFont.GetStyle: Integer;
begin
  ErrorCheck( GdipGetFontStyle(FNativeFont, Result));
end;

function TIGPFont.GetSize: Single;
begin
  ErrorCheck( GdipGetFontSize(FNativeFont, Result));
end;

function TIGPFont.GetUnit: TIGPUnit;
begin
  ErrorCheck( GdipGetFontUnit(FNativeFont, Result));
end;

function TIGPFont.GetHeight(graphics: IGPGraphics) : Single;
var ngraphics: Gpgraphics;
begin
  if( Assigned(graphics)) then
    ngraphics := graphics.GetNativeGraphics()

  else
    ngraphics := NIL;
      
  ErrorCheck( GdipGetFontHeight(FNativeFont, ngraphics, Result));
end;

function TIGPFont.GetHeight(dpi: Single) : Single;
begin
  ErrorCheck( GdipGetFontHeightGivenDPI(FNativeFont, dpi, Result));
end;

function TIGPFont.GetFamily() : IGPFontFamily;
var
  nFamily: GpFontFamily;

begin
  ErrorCheck( GdipGetFamily(FNativeFont, nFamily) );
  Result := TIGPFontFamily.CreateGdiPlus( nFamily, False );
end;

constructor TIGPFont.CreateGdiPlus(font: GpFont; Dummy : Boolean);
begin
  SetNativeFont(font);
end;

procedure TIGPFont.SetNativeFont(Font: GpFont);
begin
  FNativeFont := Font;
end;

function TIGPFont.GetNativeFont() : GpFont;
begin
  Result := FNativeFont; 
end;

(**************************************************************************\
*
*   Font collections (Installed and Private)
*
\**************************************************************************)

constructor TIGPFontCollection.Create();
begin
  FNativeFontCollection := NIL;
end;

destructor TIGPFontCollection.Destroy();
begin
  inherited Destroy();
end;

function TIGPFontCollection.GetFamilyCount() : Integer;
var
  numFound: Integer;
begin
  numFound := 0;
  ErrorCheck( GdipGetFontCollectionFamilyCount(FNativeFontCollection, numFound));
  Result := numFound;
end;

function TIGPFontCollection.GetFamilies() : TArray<IGPFontFamily>;
var
  nativeFamilyList: Pointer;
  count : Integer;
  numFound: Integer;
  i: Integer;

type
  ArrGpFontFamily = array of GpFontFamily;

begin
  ErrorCheck( GdipGetFontCollectionFamilyCount( FNativeFontCollection, count )); 
  getMem(nativeFamilyList, count * SizeOf(GpFontFamily));
  try
    ErrorCheck( GdipGetFontCollectionFamilyList(
        FNativeFontCollection,
        count,
        nativeFamilyList,
        numFound
      ));

    SetLength( Result, numFound ); 
    for i := 0 to numFound - 1 do
      Result[ i ] := TIGPFontFamily.CreateGdiPlus( ArrGpFontFamily(nativeFamilyList)[i], False );
//         GdipCloneFontFamily(ArrGpFontFamily(nativeFamilyList)[i], gpfamilies[i].FNativeFamily);
         
  finally
    Freemem(nativeFamilyList, count * SizeOf(GpFontFamily));
  end
end;

function TIGPFontCollection.GetNativeFontCollection() : GpFontCollection;
begin
  Result := FNativeFontCollection;
end;
{
procedure TIGPFontCollection.GetFamilies(numSought: Integer; out gpfamilies: array of TIGPFontFamily;
    out numFound: Integer);
var
  nativeFamilyList: Pointer;
  i: Integer;

type
  ArrGpFontFamily = array of GpFontFamily;

begin
  if ((numSought <= 0) or (length(gpfamilies) = 0)) then
    ErrorCheck( InvalidParameter);

  numFound := 0;

  getMem(nativeFamilyList, numSought * SizeOf(GpFontFamily));
  try
    if nativeFamilyList = NIL then
      ErrorCheck( OutOfMemory);

    ErrorCheck( GdipGetFontCollectionFamilyList(
        FNativeFontCollection,
        numSought,
        nativeFamilyList,
        numFound
      ));

    for i := 0 to numFound - 1 do
       GdipCloneFontFamily(ArrGpFontFamily(nativeFamilyList)[i], gpfamilies[i].FNativeFamily);

  finally
    Freemem(nativeFamilyList, numSought * SizeOf(GpFontFamily));
  end;

end;
}
constructor TIGPInstalledFontCollection.Create();
begin
  FNativeFontCollection := NIL;
  ErrorCheck( GdipNewInstalledFontCollection(FNativeFontCollection));
end;

destructor TIGPInstalledFontCollection.Destroy();
begin
  inherited Destroy();
end;

constructor TIGPPrivateFontCollection.Create();
begin
  FNativeFontCollection := NIL;
  ErrorCheck( GdipNewPrivateFontCollection(FNativeFontCollection));
end;

destructor TIGPPrivateFontCollection.Destroy();
begin
  GdipDeletePrivateFontCollection(FNativeFontCollection);
  inherited Destroy();
end;

function TIGPPrivateFontCollection.AddFontFile(filename: WideString) : TIGPPrivateFontCollection;
begin
  ErrorCheck( GdipPrivateAddFontFile(FNativeFontCollection, PWideChar(filename)));
  Result := Self;
end;

function TIGPPrivateFontCollection.AddMemoryFont(memory: Pointer; length: Integer) : TIGPPrivateFontCollection;
begin
  ErrorCheck( GdipPrivateAddMemoryFont(
      FNativeFontCollection,
      memory,
      length));
        
  Result := Self;
end;

(**************************************************************************\
*
*   GDI+ Graphics Path class
*
\**************************************************************************)

constructor TIGPGraphicsPath.Create(fillMode: TIGPFillMode = FillModeAlternate);
begin
  FNativePath := NIL;
  ErrorCheck( GdipCreatePath(fillMode, FNativePath));
end;

constructor TIGPGraphicsPath.Create( const points : array of TPointF; const types : array of BYTE;
    fillMode: TIGPFillMode = FillModeAlternate );
begin
  FNativePath := NIL;
  Assert( Length( points ) = Length( types ));
  ErrorCheck( GdipCreatePath2( @points[ 0 ], @types[ 0 ], Min( Length( points ), Length( types )), fillMode, FNativePath));
end;

constructor TIGPGraphicsPath.Create( const points : array of TPoint; const types : array of BYTE;
    fillMode: TIGPFillMode = FillModeAlternate );
begin
  FNativePath := NIL;
  Assert( Length( points ) = Length( types ));
  ErrorCheck( GdipCreatePath2I( @points[ 0 ], @types[ 0 ], Min( Length( points ), Length( types )), fillMode, FNativePath));
end;

destructor TIGPGraphicsPath.Destroy();
begin
  GdipDeletePath(FNativePath);
end;

function TIGPGraphicsPath.Clone: TIGPGraphicsPath;
var
  clonepath: GpPath;
begin
  clonepath := NIL;
  ErrorCheck( GdipClonePath(FNativePath, clonepath));
  Result := TIGPGraphicsPath.CreateGdiPlus(clonepath, False);
end;

  // Reset the path object to empty (and fill mode to FillModeAlternate)

function TIGPGraphicsPath.Reset() : TIGPGraphicsPath;
begin
  ErrorCheck( GdipResetPath(FNativePath));
  Result := Self;
end;

function TIGPGraphicsPath.GetFillMode() : TIGPFillMode;
var FMode: TIGPFillMode;
begin
  FMode := FillModeAlternate;
  ErrorCheck( GdipGetPathFillMode(FNativePath, Result));
  Result := FMode;
end;

function TIGPGraphicsPath.SetFillMode(fillmode: TIGPFillMode) : TIGPGraphicsPath;
begin
  ErrorCheck( GdipSetPathFillMode(FNativePath, fillmode));
  Result := Self;
end;

procedure TIGPGraphicsPath.SetFillModeProp(fillmode: TIGPFillMode);
begin
  ErrorCheck( GdipSetPathFillMode(FNativePath, fillmode));
end;

function TIGPGraphicsPath.GetPathData() : IGPPathData;
var
  count: Integer;
  pathData : TIGPPathData;
    
begin
  pathData := TIGPPathData.Create();
  Result := pathData;
  count := GetPointCount();
  if ((count <= 0) or ((pathData.FCount > 0) and (pathData.FCount < Count))) then
    begin
    pathData.FCount := 0;
    if( Assigned(pathData.FPoints)) then
      begin
      FreeMem(pathData.FPoints);
      pathData.FPoints := NIL;
      end;

    if( Assigned(pathData.FTypes)) then
      begin
      FreeMem(pathData.FTypes);
      pathData.FTypes := NIL;
      end;
    end;

  if (pathData.FCount = 0) then
    begin
    getmem(pathData.FPoints, SizeOf(TPointF) * count);
    if (pathData.FPoints = NIL) then
      ErrorCheck( OutOfMemory);

    Getmem(pathData.FTypes, count);
    if (pathData.FTypes = NIL) then
      begin
      freemem(pathData.FPoints);
      pathData.FPoints := NIL;
      ErrorCheck( OutOfMemory);
      end;
        
    pathData.FCount := count;
    end;

  ErrorCheck( GdipGetPathData(FNativePath, @pathData.FCount));
end;

function TIGPGraphicsPath.StartFigure() : TIGPGraphicsPath;
begin
  ErrorCheck( GdipStartPathFigure(FNativePath));
  Result := Self;
end;

function TIGPGraphicsPath.CloseFigure() : TIGPGraphicsPath;
begin
  ErrorCheck( GdipClosePathFigure(FNativePath));
  Result := Self;
end;

function TIGPGraphicsPath.CloseAllFigures() : TIGPGraphicsPath;
begin
  ErrorCheck( GdipClosePathFigures(FNativePath));
  Result := Self;
end;

function TIGPGraphicsPath.SetMarker() : TIGPGraphicsPath;
begin
  ErrorCheck( GdipSetPathMarker(FNativePath));
  Result := Self;
end;

function TIGPGraphicsPath.ClearMarkers() : TIGPGraphicsPath;
begin
  ErrorCheck( GdipClearPathMarkers(FNativePath));
  Result := Self;
end;

function TIGPGraphicsPath.Reverse() : TIGPGraphicsPath;
begin
  ErrorCheck( GdipReversePath(FNativePath));
  Result := Self;
end;

function TIGPGraphicsPath.GetLastPoint() : TPointF;
begin
  ErrorCheck( GdipGetPathLastPoint(FNativePath,
                                          @Result ));
end;

function TIGPGraphicsPath.AddLineF(const pt1, pt2: TPointF) : TIGPGraphicsPath;
begin
  Result := AddLineF(pt1.X, pt1.Y, pt2.X, pt2.Y);
end;

function TIGPGraphicsPath.AddLineF(x1, y1, x2, y2: Single) : TIGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathLine(FNativePath, x1, y1, x2, y2));
  Result := Self;
end;

function TIGPGraphicsPath.AddLinesF( const points: array of TPointF) : TIGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathLine2(FNativePath, @points[ 0 ], Length( points )));
  Result := Self;
end;

function TIGPGraphicsPath.AddLine(const pt1, pt2: TPoint) : TIGPGraphicsPath;
begin
  Result := AddLine(pt1.X, pt1.Y, pt2.X, pt2.Y);
end;

function TIGPGraphicsPath.AddLine(x1, y1, x2, y2: Integer) : TIGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathLineI(FNativePath, x1, y1, x2, y2));
  Result := Self;
end;

function TIGPGraphicsPath.AddLines(const points: array of TPoint) : TIGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathLine2I(FNativePath, @points[ 0 ], Length( points )));
  Result := Self;
end;

function TIGPGraphicsPath.AddArcF(const rect: TIGPRectF; startAngle, sweepAngle: Single) : TIGPGraphicsPath;
begin
  Result := AddArcF(rect.X, rect.Y, rect.Width, rect.Height,
                startAngle, sweepAngle);
end;

function TIGPGraphicsPath.AddArcF(x, y, width, height, startAngle, sweepAngle: Single) : TIGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathArc(FNativePath, x, y, width, height, startAngle, sweepAngle));
  Result := Self;
end;

function TIGPGraphicsPath.AddArc(const rect: TIGPRect; startAngle, sweepAngle: Single) : TIGPGraphicsPath;
begin
  Result := AddArc(rect.X, rect.Y, rect.Width, rect.Height, startAngle, sweepAngle);
end;

function TIGPGraphicsPath.AddArc(x, y, width, height: Integer; startAngle, sweepAngle: Single) : TIGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathArcI(FNativePath, x, y, width, height, startAngle, sweepAngle));
  Result := Self;
end;

function TIGPGraphicsPath.AddBezierF(const pt1, pt2, pt3, pt4: TPointF) : TIGPGraphicsPath;
begin
  Result := AddBezierF(pt1.X, pt1.Y, pt2.X, pt2.Y, pt3.X, pt3.Y, pt4.X, pt4.Y);
end;

function TIGPGraphicsPath.AddBezierF(x1, y1, x2, y2, x3, y3, x4, y4: Single) : TIGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathBezier(FNativePath, x1, y1, x2, y2, x3, y3, x4, y4));
  Result := Self;
end;

function TIGPGraphicsPath.AddBeziersF(const points: array of TPointF) : TIGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathBeziers(FNativePath, @points[ 0 ], Length( points )));
  Result := Self;
end;

function TIGPGraphicsPath.AddBezier(const pt1, pt2, pt3, pt4: TPoint) : TIGPGraphicsPath;
begin
  Result := AddBezier(pt1.X, pt1.Y, pt2.X, pt2.Y, pt3.X, pt3.Y, pt4.X, pt4.Y);
end;

function TIGPGraphicsPath.AddBezier(x1, y1, x2, y2, x3, y3, x4, y4: Integer) : TIGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathBezierI(FNativePath, x1, y1, x2, y2, x3, y3, x4, y4));
  Result := Self;
end;

function TIGPGraphicsPath.AddBeziers(const points: array of TPoint) : TIGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathBeziersI(FNativePath, @points[ 0 ], Length( points )));
  Result := Self;
end;

function TIGPGraphicsPath.AddCurveF( const points: array of TPointF) : TIGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathCurve(FNativePath, @points[ 0 ], Length( points )));
  Result := Self;
end;

function TIGPGraphicsPath.AddCurveF( const points: array of TPointF;
  tension: Single) : TIGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathCurve2(FNativePath, @points[ 0 ], Length( points ), tension));
  Result := Self;
end;

function TIGPGraphicsPath.AddCurveF( const points: array of TPointF; offset,
  numberOfSegments: Integer; tension: Single) : TIGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathCurve3(FNativePath, @points[ 0 ], Length( points ), offset,
                        numberOfSegments, tension));
  Result := Self;
end;

function TIGPGraphicsPath.AddCurve( const points: array of TPoint) : TIGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathCurveI(FNativePath, @points[ 0 ], Length( points )));
  Result := Self;
end;

function TIGPGraphicsPath.AddCurve( const points: array of TPoint; tension: Single) : TIGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathCurve2I(FNativePath, @points[ 0 ], Length( points ), tension));
  Result := Self;
end;

function TIGPGraphicsPath.AddCurve( const points: array of TPoint; offset,
  numberOfSegments: Integer; tension: Single) : TIGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathCurve3I(FNativePath, @points[ 0 ], Length( points ), offset,
    numberOfSegments, tension));
      
  Result := Self;
end;

function TIGPGraphicsPath.AddClosedCurveF(const points: array of TPointF) : TIGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathClosedCurve(FNativePath, @points[ 0 ], Length( points )));
  Result := Self;
end;

function TIGPGraphicsPath.AddClosedCurveF(const points: array of TPointF; tension: Single) : TIGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathClosedCurve2(FNativePath, @points[ 0 ], Length( points ), tension));
  Result := Self;
end;

function TIGPGraphicsPath.AddClosedCurve(const points: array of TPoint) : TIGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathClosedCurveI(FNativePath, @points[ 0 ], Length( points )));
  Result := Self;
end;

function TIGPGraphicsPath.AddClosedCurve(const points: array of TPoint; tension: Single) : TIGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathClosedCurve2I(FNativePath, @points[ 0 ], Length( points ), tension));
  Result := Self;
end;

function TIGPGraphicsPath.AddRectangleF(const rect: TIGPRectF) : TIGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathRectangle(FNativePath,
                                          rect.X,
                                          rect.Y,
                                          rect.Width,
                                          rect.Height));

  Result := Self;
end;

function TIGPGraphicsPath.AddRectangleF(x, y, width, height: Single) : TIGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathRectangle(FNativePath,
                                          x,
                                          y,
                                          width,
                                          height));

  Result := Self;
end;

function TIGPGraphicsPath.AddRectanglesF(const rects: array of TIGPRectF) : TIGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathRectangles(FNativePath,
                                           @rects[ 0 ],
                                           Length( rects )));
                                             
  Result := Self;
end;

function TIGPGraphicsPath.AddRectangle(const rect: TIGPRect) : TIGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathRectangleI(FNativePath,
                                          rect.X,
                                          rect.Y,
                                          rect.Width,
                                          rect.Height));

  Result := Self;
end;

function TIGPGraphicsPath.AddRectangle(x, y, width, height: Integer) : TIGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathRectangleI(FNativePath,
                                          x,
                                          y,
                                          width,
                                          height));

  Result := Self;
end;

function TIGPGraphicsPath.AddRectangles(const rects: array of TIGPRect) : TIGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathRectanglesI(FNativePath,
                                           @rects[ 0 ],
                                           Length( rects )));

  Result := Self;
end;

function TIGPGraphicsPath.AddEllipseF(const rect: TIGPRectF) : TIGPGraphicsPath;
begin
  Result := AddEllipseF(rect.X, rect.Y, rect.Width, rect.Height);
end;

function TIGPGraphicsPath.AddEllipseF(x, y, width, height: Single) : TIGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathEllipse(FNativePath,
                                        x,
                                        y,
                                        width,
                                        height));

  Result := Self;
end;

function TIGPGraphicsPath.AddEllipse(const rect: TIGPRect) : TIGPGraphicsPath;
begin
  Result := AddEllipse(rect.X, rect.Y, rect.Width, rect.Height);
end;

function TIGPGraphicsPath.AddEllipse(x, y, width, height: Integer) : TIGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathEllipseI(FNativePath,
                                        x,
                                        y,
                                        width,
                                        height));

  Result := Self;
end;

function TIGPGraphicsPath.AddRoundRectangleF( const ARect: TIGPRectF; const ACornerSize : TIGPSizeF ) : TIGPGraphicsPath;
var
  ARectRight : Single;
  ACornerSizeX : TIGPSizeF;

begin
  Result := Self;
  if(( ARect.Width = 0 ) or ( ARect.Height = 0 )) then
    Exit;

  ACornerSizeX := ACornerSize;
  if( ACornerSizeX.Width < 0 ) then
    ACornerSizeX.Width := 0;

  if( ACornerSizeX.Height < 0 ) then
    ACornerSizeX.Height := 0;

  if(( ACornerSizeX.Width = 0 ) or ( ACornerSizeX.Height = 0 )) then
    begin
    AddRectangleF( ARect );
    Exit;
    end;

  ACornerSizeX.Width := ACornerSizeX.Width * 2;
  ACornerSizeX.Height := ACornerSizeX.Height * 2;
  ARectRight := ARect.X + ARect.Width;
  if( ACornerSizeX.Width > ARect.Width ) then
    ACornerSizeX.Width := ARect.Width;

  if( ACornerSizeX.Height > ARect.Height ) then
    ACornerSizeX.Height := ARect.Height;

  StartFigure();
  AddArcF( ARect.X, ARect.Y, ACornerSizeX.Width, ACornerSizeX.Height, 180, 90);
  AddArcF( ARectRight - ACornerSizeX.Width, ARect.Y, ACornerSizeX.Width, ACornerSizeX.Height, 270, 90);
  AddArcF( ARectRight - ACornerSizeX.Width, ARect.Y + ARect.Height - ACornerSizeX.Height, ACornerSizeX.Width, ACornerSizeX.Height, 0, 90);
  AddArcF(ARect.X, ARect.Y + ARect.Height - ACornerSizeX.Height, ACornerSizeX.Width, ACornerSizeX.Height, 90, 90);
  CloseFigure();
end;

function TIGPGraphicsPath.AddRoundRectangle( const ARect: TIGPRect; const ACornerSize : TIGPSize ) : TIGPGraphicsPath;
var
  ARectRight : Integer;
  ACornerSizeX : TIGPSize;

begin
  Result := Self;
  if(( ARect.Width = 0 ) or ( ARect.Height = 0 )) then
    Exit;

  ACornerSizeX := ACornerSize;
  ACornerSizeX.Width := ACornerSizeX.Width * 2;
  ACornerSizeX.Height := ACornerSizeX.Height * 2;
  ARectRight := ARect.X + ARect.Width;
  if( ACornerSizeX.Width > ARect.Width ) then
    ACornerSizeX.Width := ARect.Width;

  if( ACornerSizeX.Height > ARect.Height ) then
    ACornerSizeX.Height := ARect.Height;

  StartFigure();
  AddArc( ARect.X, ARect.Y, ACornerSizeX.Width, ACornerSizeX.Height, 180, 90);
  AddArc( ARectRight - ACornerSizeX.Width, ARect.Y, ACornerSizeX.Width, ACornerSizeX.Height, 270, 90);
  AddArc( ARectRight - ACornerSizeX.Width, ARect.Y + ARect.Height - ACornerSizeX.Height, ACornerSizeX.Width, ACornerSizeX.Height, 0, 90);
  AddArc(ARect.X, ARect.Y + ARect.Height - ACornerSizeX.Height, ACornerSizeX.Width, ACornerSizeX.Height, 90, 90);
  CloseFigure();
end;

function TIGPGraphicsPath.AddPieF(const rect: TIGPRectF; startAngle, sweepAngle: Single) : TIGPGraphicsPath;
begin
  AddPieF(rect.X, rect.Y, rect.Width, rect.Height, startAngle,
                sweepAngle);
                  
  Result := Self;
end;

function TIGPGraphicsPath.AddPieF(x, y, width, height, startAngle, sweepAngle: Single) : TIGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathPie(FNativePath, x, y, width,
                                      height, startAngle,
                                      sweepAngle));
                                        
  Result := Self;
end;

function TIGPGraphicsPath.AddPie(const rect: TIGPRect; startAngle, sweepAngle: Single) : TIGPGraphicsPath;
begin
  Result := AddPie(rect.X,
                rect.Y,
                rect.Width,
                rect.Height,
                startAngle,
                sweepAngle);
end;

function TIGPGraphicsPath.AddPie(x, y, width, height: Integer; startAngle, sweepAngle: Single) : TIGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathPieI(FNativePath,
                                      x,
                                      y,
                                      width,
                                      height,
                                      startAngle,
                                      sweepAngle));
                  
  Result := Self;
end;

function TIGPGraphicsPath.AddPolygonF(const points: array of TPointF) : TIGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathPolygon(FNativePath, @points[ 0 ], Length( points )));
  Result := Self;
end;

function TIGPGraphicsPath.AddPolygon(const points: array of TPoint) : TIGPGraphicsPath;
begin
  ErrorCheck( GdipAddPathPolygonI(FNativePath, @points[ 0 ], Length( points )));
  Result := Self;
end;

function TIGPGraphicsPath.AddPath(addingPath: IGPGraphicsPath; connect: Boolean) : TIGPGraphicsPath;
var
  nativePath2: GpPath;
begin
  nativePath2 := NIL;
  if( Assigned(addingPath)) then
    nativePath2 := addingPath.GetNativePath();
      
  ErrorCheck( GdipAddPathPath(FNativePath, nativePath2, connect));
  Result := Self;
end;

function TIGPGraphicsPath.AddStringF(string_: WideString; font : IGPFont;
    const origin : TPointF; format : IGPStringFormat) : TIGPGraphicsPath;
begin
  Result := AddStringF( string_, font.Family, font.Style, font.Size, origin, format );
end;

function TIGPGraphicsPath.AddStringF(string_: WideString; font : IGPFont;
    const layoutRect: TIGPRectF; format : IGPStringFormat) : TIGPGraphicsPath;
begin
  Result := AddStringF( string_, font.Family, font.Style, font.Size, layoutRect, format );
end;

function TIGPGraphicsPath.AddString(string_: WideString; font : IGPFont;
    const origin : TPoint; format : IGPStringFormat) : TIGPGraphicsPath;
begin
  Result := AddString( string_, font.Family, font.Style, font.Size, origin, format );
end;
  
function TIGPGraphicsPath.AddString(string_: WideString; font : IGPFont;
    const layoutRect: TIGPRect; format : IGPStringFormat) : TIGPGraphicsPath;
begin
  Result := AddString( string_, font.Family, font.Style, font.Size, layoutRect, format );
end;

function TIGPGraphicsPath.AddStringF(
    string_: WideString;
    family : IGPFontFamily;
    style  : Integer;
    emSize : Single;  // World units
    const origin : TPointF;
    format : IGPStringFormat) : TIGPGraphicsPath;
var
  rect : TIGPRectF;
  gpff : GPFONTFAMILY;
  gpsf : GPSTRINGFORMAT;
    
begin
  rect.X := origin.X;
  rect.Y := origin.Y;
  rect.Width := 0.0;
  rect.Height := 0.0;

  gpff := NIL;
  gpsf := NIL;
  if( Assigned(family)) then
    gpff := family.GetNativeFamily();

  if( Assigned(format)) then
    gpsf := format.GetNativeFormat();
      
  ErrorCheck( GdipAddPathString(FNativePath, PWideChar(string_), Length( string_ ), gpff,
        style, emSize, @rect, gpsf));
          
  Result := Self;
end;

function TIGPGraphicsPath.AddStringF(
    string_: WideString;
    family : IGPFontFamily;
    style  : Integer;
    emSize : Single;  // World units
    const layoutRect: TIGPRectF;
    format : IGPStringFormat) : TIGPGraphicsPath;
var
  gpff : GPFONTFAMILY;
  gpsf : GPSTRINGFORMAT;
begin
  gpff := NIL;
  gpsf := NIL;
  if( Assigned(family)) then
    gpff := family.GetNativeFamily();

  if( Assigned(format)) then
    gpsf := format.GetNativeFormat();
      
  ErrorCheck( GdipAddPathString( FNativePath, PWideChar(string_), Length( string_ ), gpff,
        style, emSize, @layoutRect, gpsf));
          
  Result := Self;
end;

function TIGPGraphicsPath.AddString(
    string_: WideString;
    family : IGPFontFamily;
    style  : Integer;
    emSize : Single;  // World units
    const origin : TPoint;
    format : IGPStringFormat) : TIGPGraphicsPath;
var
  rect : TIGPRect;
  gpff : GPFONTFAMILY;
  gpsf : GPSTRINGFORMAT;
    
begin
  rect.X := origin.X;
  rect.Y := origin.Y;
  rect.Width := 0;
  rect.Height := 0;
  gpff := NIL;
  gpsf := NIL;
  if( Assigned(family)) then
    gpff := family.GetNativeFamily();

  if( Assigned(format)) then
    gpsf := format.GetNativeFormat();
      
  ErrorCheck( GdipAddPathStringI(FNativePath, PWideChar(string_), Length( string_ ), gpff,
        style, emSize, @rect, gpsf));
          
  Result := Self;
end;

function TIGPGraphicsPath.AddString(
    string_: WideString;
    family : IGPFontFamily;
    style  : Integer;
    emSize : Single;  // World units
    const layoutRect: TIGPRect;
    format : IGPStringFormat) : TIGPGraphicsPath;
var
  gpff : GPFONTFAMILY;
  gpsf : GPSTRINGFORMAT;
begin
  gpff := NIL;
  gpsf := NIL;
  if( Assigned(family)) then
    gpff := family.GetNativeFamily();
      
  if( Assigned(format)) then
    gpsf := format.GetNativeFormat();

  ErrorCheck( GdipAddPathStringI( FNativePath, PWideChar(string_), Length( string_ ), gpff,
        style, emSize, @layoutRect, gpsf));
          
  Result := Self;
end;

function TIGPGraphicsPath.Transform(matrix: IGPMatrix) : TIGPGraphicsPath;
begin
  if( Assigned(matrix)) then
    ErrorCheck( GdipTransformPath(FNativePath, matrix.GetNativeMatrix()));

  Result := Self;
end;

  // This is not always the tightest bounds.

function TIGPGraphicsPath.GetBoundsF( matrix: IGPMatrix = NIL; pen: IGPPen = NIL) : TIGPRectF;
var
  nativeMatrix: GpMatrix;
  nativePen: GpPen;

begin
  nativeMatrix := NIL;
  nativePen    := NIL;
  if( Assigned(matrix)) then
    nativeMatrix := matrix.GetNativeMatrix();

  if( Assigned(pen)) then
    nativePen := pen.GetNativePen();

  ErrorCheck( GdipGetPathWorldBounds(FNativePath, @Result, nativeMatrix, nativePen));
end;

function TIGPGraphicsPath.GetBounds( matrix: IGPMatrix = NIL; pen: IGPPen = NIL) : TIGPRect;
var
  nativeMatrix: GpMatrix;
  nativePen: GpPen;
    
begin
  nativeMatrix := NIL;
  nativePen    := NIL;
  if( Assigned(matrix)) then
    nativeMatrix := matrix.GetNativeMatrix();
      
  if( Assigned(pen)) then
    nativePen := pen.GetNativePen();

  ErrorCheck( GdipGetPathWorldBoundsI(FNativePath, @Result, nativeMatrix, nativePen));
end;

  // Once flattened, the resultant path is made of line segments and
  // the original path information is lost.  When matrix is NIL the
  // identity matrix is assumed.

function TIGPGraphicsPath.Flatten(matrix: IGPMatrix = NIL; flatness: Single = FlatnessDefault) : TIGPGraphicsPath;
var nativeMatrix: GpMatrix;
begin
  nativeMatrix := NIL;
  if( Assigned(matrix)) then
    nativeMatrix := matrix.GetNativeMatrix();
      
  ErrorCheck( GdipFlattenPath(FNativePath, nativeMatrix, flatness));
  Result := Self;
end;

function TIGPGraphicsPath.Widen( pen: IGPPen; matrix: IGPMatrix = NIL; flatness: Single = FlatnessDefault) : TIGPGraphicsPath;
var nativeMatrix: GpMatrix;
begin
  if( pen = NIL ) then
    ErrorCheck( InvalidParameter );

  nativeMatrix := NIL;
  if( Assigned(matrix)) then
    nativeMatrix := matrix.GetNativeMatrix();
      
  ErrorCheck( GdipWidenPath(FNativePath, pen.GetNativePen(), nativeMatrix, flatness));
  Result := Self;
end;

function TIGPGraphicsPath.Outline(matrix: IGPMatrix = NIL; flatness: Single = FlatnessDefault) : TIGPGraphicsPath;
var nativeMatrix: GpMatrix;
begin
  nativeMatrix := NIL;
  if( Assigned(matrix)) then
    nativeMatrix := matrix.GetNativeMatrix();
      
  ErrorCheck( GdipWindingModeOutline(FNativePath, nativeMatrix, flatness));
  Result := Self;
end;

  // Once this is called, the resultant path is made of line segments and
  // the original path information is lost.  When matrix is NIL, the
  // identity matrix is assumed.

function TIGPGraphicsPath.Warp( const destPoints : array of TPointF; const srcRect: TIGPRectF;
          matrix: IGPMatrix = NIL; warpMode: TIGPWarpMode = WarpModePerspective;
          flatness: Single = FlatnessDefault) : TIGPGraphicsPath;
var nativeMatrix: GpMatrix;
begin
  nativeMatrix := NIL;
  if( Assigned(matrix)) then
    nativeMatrix := matrix.GetNativeMatrix();
      
  ErrorCheck( GdipWarpPath(FNativePath, nativeMatrix, @destPoints[ 0 ],
              Length( destPoints ), srcRect.X, srcRect.Y, srcRect.Width, srcRect.Height,
              warpMode, flatness));
                
  Result := Self;
end;

function TIGPGraphicsPath.GetPointCount() : Integer;
var count: Integer;
begin
  count := 0;
  ErrorCheck( GdipGetPointCount(FNativePath, count));
  Result := count;
end;

function TIGPGraphicsPath.GetPathTypes(types: PBYTE; count: Integer) : TIGPGraphicsPath;
begin
  ErrorCheck( GdipGetPathTypes(FNativePath, types, count));
  Result := Self;
end;

function TIGPGraphicsPath.GetPathPointsF() : TArray<TPointF>;
var
  count : Integer;

begin
  ErrorCheck( GdipGetPointCount( FNativePath, count ));
  SetLength( Result, count );
  ErrorCheck( GdipGetPathPoints(FNativePath, @Result[ 0 ], count ));
end;

function TIGPGraphicsPath.GetPathPoints() : TArray<TPoint>;
var
  count : Integer;

begin
  ErrorCheck( GdipGetPointCount( FNativePath, count ));
  SetLength( Result, count );
  ErrorCheck( GdipGetPathPointsI(FNativePath, @Result[ 0 ], count ));
end;

function TIGPGraphicsPath.IsVisibleF(const point: TPointF; g: IGPGraphics = NIL) : Boolean;
begin
  Result := IsVisibleF(point.X, point.Y, g);
end;

function TIGPGraphicsPath.IsVisibleF(x, y: Single; g: IGPGraphics = NIL) : Boolean;
var
  booln: BOOL;
  nativeGraphics: GpGraphics;
    
begin
  booln := False;
  nativeGraphics := NIL;
  if( Assigned(g)) then
    nativeGraphics := g.GetNativeGraphics();
      
  ErrorCheck( GdipIsVisiblePathPoint(FNativePath, x, y, nativeGraphics, booln));
  Result := booln;
end;

function TIGPGraphicsPath.IsVisible(const point: TPoint; g : IGPGraphics = NIL) : Boolean;
begin
  Result := IsVisible(point.X, point.Y, g);
end;

function TIGPGraphicsPath.IsVisible(x, y: Integer; g: IGPGraphics = NIL) : Boolean;
var
  booln: BOOL;
  nativeGraphics: GpGraphics;

begin
  booln := False;
  nativeGraphics := NIL;
  if( Assigned(g)) then
    nativeGraphics := g.GetNativeGraphics();
      
  ErrorCheck( GdipIsVisiblePathPointI(FNativePath, x, y, nativeGraphics, booln));
  Result := booln;
end;

function TIGPGraphicsPath.IsOutlineVisibleF(const point: TPointF; pen: IGPPen; g: IGPGraphics = NIL) : Boolean;
begin
  Result := IsOutlineVisibleF(point.X, point.Y, pen, g);
end;

function TIGPGraphicsPath.IsOutlineVisibleF(x, y: Single; pen: IGPPen; g: IGPGraphics = NIL) : Boolean;
var
  booln: BOOL;
  nativeGraphics: GpGraphics;
  nativePen: GpPen;
begin
  booln := False;
  nativeGraphics := NIL;
  nativePen := NIL;
  if( Assigned(g)) then
    nativeGraphics := g.GetNativeGraphics();

  if( Assigned(pen)) then
    nativePen := pen.GetNativePen();
      
  ErrorCheck( GdipIsOutlineVisiblePathPoint(FNativePath, x, y, nativePen, nativeGraphics, booln));
  Result := booln;
end;

function TIGPGraphicsPath.IsOutlineVisible(const point: TPoint; pen: IGPPen; g: IGPGraphics = NIL) : Boolean;
begin
  Result := IsOutlineVisible(point.X, point.Y, pen, g);
end;

function TIGPGraphicsPath.IsOutlineVisible(x, y: Integer; pen: IGPPen; g: IGPGraphics = NIL) : Boolean;
var
  booln: BOOL;
  nativeGraphics: GpGraphics;
  nativePen: GpPen;
begin
  booln := False;
  nativeGraphics := NIL;
  nativePen := NIL;
  if( Assigned(g)) then
    nativeGraphics := g.GetNativeGraphics();

  if( Assigned(pen)) then
    nativePen := pen.GetNativePen();

  ErrorCheck( GdipIsOutlineVisiblePathPointI(FNativePath, x, y, nativePen, nativeGraphics, booln));
  Result := booln;
end;

constructor TIGPGraphicsPath.Create(path: IGPGraphicsPath);
var clonepath: GpPath;
begin
  clonepath := NIL;
  ErrorCheck( GdipClonePath(path.GetNativePath(), clonepath));
  SetNativePath(clonepath);
end;

constructor TIGPGraphicsPath.CreateGdiPlus(nativePath: GpPath; Dummy : Boolean);
begin
  SetNativePath(nativePath);
end;

procedure TIGPGraphicsPath.SetNativePath(nativePath: GpPath);
begin
  self.FNativePath := nativePath;
end;

function TIGPGraphicsPath.GetNativePath() : GpPath;
begin
  Result := self.FNativePath;
end;

//--------------------------------------------------------------------------
// GraphisPathIterator class
//--------------------------------------------------------------------------

constructor TIGPGraphicsPathIterator.Create(path: IGPGraphicsPath);
var
  nativePath: GpPath;
  iter: GpPathIterator;
begin
  nativePath := NIL;
  if( Assigned(path)) then
    nativePath := path.GetNativePath();
      
  iter := NIL;
  ErrorCheck( GdipCreatePathIter(iter, nativePath));
  SetNativeIterator(iter);
end;

destructor TIGPGraphicsPathIterator.Destroy();
begin
  GdipDeletePathIter(FNativeIterator);
end;


function TIGPGraphicsPathIterator.NextSubpath(out startIndex, endIndex: Integer; out isClosed: Boolean) : Integer;
var
  ABool : BOOL;

begin
  ErrorCheck( GdipPathIterNextSubpath(FNativeIterator, Result, startIndex, endIndex, ABool ));
  isClosed := ABool;
end;

function TIGPGraphicsPathIterator.NextSubpath(path: IGPGraphicsPath; out isClosed: Boolean) : Integer;
var
  nativePath: GpPath;
  resultCount: Integer;
  AValue : BOOL;
    
begin
  nativePath := NIL;
  if( Assigned(path)) then
    nativePath := path.GetNativePath();
      
  ErrorCheck( GdipPathIterNextSubpathPath(FNativeIterator, resultCount,
    nativePath, AValue));

  isClosed := AValue;
  Result := resultCount;
end;

function TIGPGraphicsPathIterator.NextPathType(out pathType: TIGPPathPointType; out startIndex, endIndex: Integer) : Integer;
var
  resultCount: Integer;
begin
  ErrorCheck( GdipPathIterNextPathType(FNativeIterator, resultCount, PByte(@pathType),
     startIndex, endIndex));
  Result := resultCount;
end;

function TIGPGraphicsPathIterator.NextMarker(out startIndex, endIndex: Integer) : Integer;
begin
  ErrorCheck( GdipPathIterNextMarker(FNativeIterator, Result, startIndex, endIndex));
end;

function TIGPGraphicsPathIterator.NextMarker(path: IGPGraphicsPath) : Integer;
var nativePath: GpPath;
begin
  nativePath := NIL;
  if( Assigned(path)) then
    nativePath := path.GetNativePath();
      
  ErrorCheck( GdipPathIterNextMarkerPath(FNativeIterator, Result, nativePath));
end;

function TIGPGraphicsPathIterator.GetCount: Integer;
begin
  ErrorCheck( GdipPathIterGetCount(FNativeIterator, Result));
end;

function TIGPGraphicsPathIterator.GetSubpathCount: Integer;
begin
  ErrorCheck( GdipPathIterGetSubpathCount(FNativeIterator, Result));
end;

function TIGPGraphicsPathIterator.HasCurve: Boolean;
var
  AValue : BOOL;
    
begin
  ErrorCheck( GdipPathIterHasCurve(FNativeIterator, AValue ));
  Result := AValue;
end;

function TIGPGraphicsPathIterator.Rewind() : TIGPGraphicsPathIterator;
begin
  ErrorCheck( GdipPathIterRewind(FNativeIterator));
  Result := Self;
end;

function TIGPGraphicsPathIterator.Enumerate( out points: TArray<TPointF>; out types: TArray<Byte> ) : Integer;
var
  ACount : Integer;
    
begin
  ACount := GetCount();
  SetLength( points, ACount );
  SetLength( types, ACount );
  ErrorCheck( GdipPathIterEnumerate( FNativeIterator, Result, @points[ 0 ], @types[ 0 ], ACount ));
end;

function TIGPGraphicsPathIterator.CopyData(points: PGPPointF; types: PBYTE;
  startIndex, endIndex: Integer) : Integer;
begin
  ErrorCheck( GdipPathIterCopyData(FNativeIterator, Result, points, types,
    startIndex, endIndex));
end;

procedure TIGPGraphicsPathIterator.SetNativeIterator(nativeIterator: GpPathIterator);
begin
  self.FNativeIterator := nativeIterator;
end;

//--------------------------------------------------------------------------
// Path Gradient Brush
//--------------------------------------------------------------------------

constructor TIGPPathGradientBrush.CreateF(const points: array of TPointF; wrapMode: TIGPWrapMode = WrapModeClamp);
var brush: GpPathGradient;
begin
  brush := NIL;
  ErrorCheck( GdipCreatePathGradient(@points[ 0 ], Length( points ), wrapMode, brush));
  SetNativeBrush(brush);
end;

constructor TIGPPathGradientBrush.Create(const points: array of TPoint; wrapMode: TIGPWrapMode = WrapModeClamp);
var brush: GpPathGradient;
begin
  brush := NIL;
  ErrorCheck( GdipCreatePathGradientI(@points[ 0 ], Length( points ), wrapMode, brush));
  SetNativeBrush(brush);
end;

constructor TIGPPathGradientBrush.Create(path: IGPGraphicsPath);
var brush: GpPathGradient;
begin
  ErrorCheck( GdipCreatePathGradientFromPath( path.GetNativePath(), brush));
  SetNativeBrush(brush);
end;

function TIGPPathGradientBrush.GetCenterColor() : TAlphaColor;
begin
  ErrorCheck( GdipGetPathGradientCenterColor(GpPathGradient(GetNativeBrush()), Result ));
end;

procedure TIGPPathGradientBrush.SetCenterColorProp(color: TAlphaColor);
begin
  ErrorCheck( GdipSetPathGradientCenterColor(GpPathGradient(GetNativeBrush()),color));
end;

function TIGPPathGradientBrush.SetCenterColor(color: TAlphaColor) : TIGPPathGradientBrush;
begin
  ErrorCheck( GdipSetPathGradientCenterColor(GpPathGradient(GetNativeBrush()),color));
  Result := Self;
end;

function TIGPPathGradientBrush.GetPointCount() : Integer;
begin
  ErrorCheck( GdipGetPathGradientPointCount(GpPathGradient(GetNativeBrush()), Result));
end;

function TIGPPathGradientBrush.GetSurroundColorCount() : Integer;
begin
  ErrorCheck( GdipGetPathGradientSurroundColorCount(GpPathGradient(GetNativeBrush()), Result));
end;

function TIGPPathGradientBrush.GetSurroundColors() : TArray<TAlphaColor>;
var
  count1: Integer;
    
begin
  ErrorCheck( GdipGetPathGradientSurroundColorCount(GpPathGradient(GetNativeBrush()), count1));

  if( count1 <= 0 ) then
  begin
    ErrorCheck( InsufficientBuffer);
    exit;
  end;

  SetLength( Result, count1 );

  ErrorCheck( GdipGetPathGradientSurroundColorsWithCount(GpPathGradient(GetNativeBrush()), @Result[ 0 ], count1));
end;

procedure TIGPPathGradientBrush.SetSurroundColorsProp(colors : TArray<TAlphaColor> );
begin
  SetSurroundColors( colors );
end;

function TIGPPathGradientBrush.SetSurroundColors(const colors: array of TAlphaColor ) : TIGPPathGradientBrush;
var
  count1: Integer;
  count: Integer;

begin
  Result := Self;
  count1 := GetPointCount();
  count := Length( colors );

  if((count > count1) or (count1 <= 0)) then
  begin
    ErrorCheck( InvalidParameter);
    exit;
  end;

  count1 := count;

  ErrorCheck( GdipSetPathGradientSurroundColorsWithCount(
              GpPathGradient(GetNativeBrush()), @colors[ 0 ], count1));

end;

function TIGPPathGradientBrush.GetGraphicsPath() : IGPGraphicsPath;
begin
  Result := TIGPGraphicsPath.Create();
  ErrorCheck( GdipGetPathGradientPath(GpPathGradient(GetNativeBrush()), Result.GetNativePath()));
end;

function TIGPPathGradientBrush.SetGraphicsPath(path: IGPGraphicsPath) : TIGPPathGradientBrush;
begin
  Result := Self;
  if(path = NIL) then
    ErrorCheck( InvalidParameter);

  ErrorCheck( GdipSetPathGradientPath(GpPathGradient(GetNativeBrush()), path.GetNativePath() ));
end;

procedure TIGPPathGradientBrush.SetGraphicsPathProp(path: IGPGraphicsPath);
begin
  SetGraphicsPath( path );
end;

function TIGPPathGradientBrush.GetCenterPointF() : TPointF;
begin
  ErrorCheck( GdipGetPathGradientCenterPoint(GpPathGradient(GetNativeBrush()), @Result));
end;

function TIGPPathGradientBrush.GetCenterPoint() : TPoint;
begin
  ErrorCheck( GdipGetPathGradientCenterPointI(GpPathGradient(GetNativeBrush()), @Result));
end;

procedure TIGPPathGradientBrush.SetCenterPointFProp(const point: TPointF);
begin
  ErrorCheck( GdipSetPathGradientCenterPoint(GpPathGradient(GetNativeBrush()), @point));
end;

function TIGPPathGradientBrush.SetCenterPointF(const point: TPointF) : TIGPPathGradientBrush;
begin
  ErrorCheck( GdipSetPathGradientCenterPoint(GpPathGradient(GetNativeBrush()), @point));
  Result := Self;
end;

function TIGPPathGradientBrush.SetCenterPoint(const point: TPoint) : TIGPPathGradientBrush;
begin
  ErrorCheck( GdipSetPathGradientCenterPointI(GpPathGradient(GetNativeBrush()), @point));
  Result := Self;
end;

function TIGPPathGradientBrush.GetRectangleF() : TIGPRectF;
begin
  ErrorCheck( GdipGetPathGradientRect(GpPathGradient(GetNativeBrush()), @Result));
end;

function TIGPPathGradientBrush.GetRectangle() : TIGPRect;
begin
  ErrorCheck( GdipGetPathGradientRectI(GpPathGradient(GetNativeBrush()), @Result));
end;

procedure TIGPPathGradientBrush.SetGammaCorrectionProp(useGammaCorrection: Boolean);
begin
  ErrorCheck( GdipSetPathGradientGammaCorrection(GpPathGradient(GetNativeBrush()),
    useGammaCorrection));
end;

function TIGPPathGradientBrush.SetGammaCorrection(useGammaCorrection: Boolean) : TIGPPathGradientBrush;
begin
  ErrorCheck( GdipSetPathGradientGammaCorrection(GpPathGradient(GetNativeBrush()),
    useGammaCorrection));

  Result := Self;
end;

function TIGPPathGradientBrush.GetGammaCorrection() : Boolean;
var
  AValue : BOOL;
    
begin
  ErrorCheck( GdipGetPathGradientGammaCorrection(GpPathGradient(GetNativeBrush()), AValue ));
  Result := AValue;
end;

function TIGPPathGradientBrush.GetBlendCount() : Integer;
begin
  ErrorCheck( GdipGetPathGradientBlendCount(GpPathGradient(GetNativeBrush()), Result));
end;

function TIGPPathGradientBrush.GetBlend() : TArray<TIGPBlend>;
var
  count : Integer;
  aFactors : array of Single;
  aPositions : array of Single;
  I : Integer;

begin
  ErrorCheck( GdipGetPathGradientBlendCount( GetNativeBrush(), count ));

  SetLength( aFactors, count );
  SetLength( aPositions, count );

  ErrorCheck( GdipGetPathGradientBlend(
                    GpPathGradient(GetNativeBrush()),
                    @aFactors[ 0 ], @aPositions[ 0 ], count));
                      
  SetLength( Result, count );
  for I := 0 to count - 1 do
    begin
    Result[ I ].Position := aPositions[ I ];
    Result[ I ].Value := aFactors[ I ];
    end;

end;

function TIGPPathGradientBrush.SetBlendArrays( const blendFactors : array of Single; const blendPositions : array of Single ) : TIGPPathGradientBrush;
begin
  ErrorCheck( GdipSetPathGradientBlend(
                    GpPathGradient(GetNativeBrush()),
                    @blendFactors[ 0 ], @blendPositions[ 0 ], Min( Length( blendFactors ), Length( blendPositions )) ));

  Result := Self;
end;

function TIGPPathGradientBrush.SetBlend( const blendFactors : array of TIGPBlend ) : TIGPPathGradientBrush;
var
  I : Integer;
  count : Integer;
  aFactors : array of Single;
  aPositions : array of Single;

begin
  count := Length( blendFactors ); 
  SetLength( aFactors, count );
  SetLength( aPositions, count );
  for I := 0 to count - 1 do
    begin
    aFactors[ I ] := blendFactors[ I ].Value;
    aPositions[ I ] := blendFactors[ I ].Position;
    end;

  SetBlendArrays( aFactors, aPositions );
  Result := Self;
end;

procedure TIGPPathGradientBrush.SetBlendProp( const blendFactors : TArray<TIGPBlend> );
begin
  SetBlend( blendFactors );
end;
  
function TIGPPathGradientBrush.GetInterpolationColorCount() : Integer;
begin
  ErrorCheck( GdipGetPathGradientPresetBlendCount(GpPathGradient(GetNativeBrush()), Result));
end;

function TIGPPathGradientBrush.SetInterpolationColors( const Colors : array of TIGPInterpolationColor ) : TIGPPathGradientBrush;
var
  presetColors : array of TAlphaColor;
  blendPositions : array of Single;
  count : Integer;
  I : Integer;
    
begin
  count := Length( Colors );

  SetLength( presetColors, count ); 
  SetLength( blendPositions, count );
     
  for I := 0 to count - 1 do
    begin
    presetColors[ I ] := Colors[ I ].Color;
    blendPositions[ I ] := Colors[ I ].Position;
    end; 

  ErrorCheck( GdipSetPathGradientPresetBlend(GpPathGradient(GetNativeBrush()),
      PAlphaColor( @presetColors[ 0 ]), @blendPositions[ 0 ], count ));

  Result := Self;
end;

procedure TIGPPathGradientBrush.SetInterpolationColorsProp( Colors : TArray<TIGPInterpolationColor> );
begin
  SetInterpolationColors( Colors );
end;
  
function TIGPPathGradientBrush.SetInterpolationColorArrays( const presetColors: array of TAlphaColor; const blendPositions: array of Single ) : TIGPPathGradientBrush;
begin
  Assert( Length( presetColors ) = Length( blendPositions ));
  ErrorCheck( GdipSetPathGradientPresetBlend(GpPathGradient(GetNativeBrush()),
                        PAlphaColor( @presetColors[ 0 ]), @blendPositions[ 0 ], Min( Length( presetColors ), Length( blendPositions ))));
                          
  Result := Self;
end;

function TIGPPathGradientBrush.GetInterpolationColors() : TArray<TIGPInterpolationColor>;
var
  presetColors : array of TAlphaColor;
  blendPositions : array of Single;
  count : Integer;
  I : Integer;

begin
  ErrorCheck( GdipGetPathGradientPresetBlendCount( GetNativeBrush(), count ));
    
  SetLength( presetColors, count );
  SetLength( blendPositions, count );

  ErrorCheck( GdipGetPathGradientPresetBlend(GetNativeBrush(), PAlphaColor(@presetColors[ 0 ]), @blendPositions[ 0 ], count));

  for I := 0 to count - 1 do
    begin
    Result[ I ].Color := presetColors[ I ];
    Result[ I ].Position := blendPositions[ I ];
    end;
      
end;

function TIGPPathGradientBrush.SetBlendBellShape(focus: Single; scale: Single = 1.0) : TIGPPathGradientBrush;
begin
  ErrorCheck( GdipSetPathGradientSigmaBlend(GpPathGradient(GetNativeBrush()), focus, scale));
  Result := Self;
end;

function TIGPPathGradientBrush.SetBlendTriangularShape(focus: Single; scale: Single = 1.0) : TIGPPathGradientBrush;
begin
  ErrorCheck( GdipSetPathGradientLinearBlend(GpPathGradient(GetNativeBrush()), focus, scale));
  Result := Self;
end;

function TIGPPathGradientBrush.GetTransform() : IGPMatrix;
begin
  Result := TIGPMatrix.Create();
  ErrorCheck( GdipGetPathGradientTransform(GpPathGradient(GetNativeBrush()),
                    Result.GetNativeMatrix()));
end;

function TIGPPathGradientBrush.SetTransform(matrix: IGPMatrix) : TIGPPathGradientBrush;
begin
  ErrorCheck( GdipSetPathGradientTransform(
                    GpPathGradient(GetNativeBrush()),
                    matrix.GetNativeMatrix()));

  Result := Self;
end;

procedure TIGPPathGradientBrush.SetTransformProp(matrix: IGPMatrix);
begin
  ErrorCheck( GdipSetPathGradientTransform(
                    GpPathGradient(GetNativeBrush()),
                    matrix.GetNativeMatrix()));

end;

function TIGPPathGradientBrush.ResetTransform() : TIGPPathGradientBrush;
begin
  ErrorCheck( GdipResetPathGradientTransform(
                    GpPathGradient(GetNativeBrush())));

  Result := Self;
end;

function TIGPPathGradientBrush.MultiplyTransform(matrix: IGPMatrix; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPPathGradientBrush;
begin
  ErrorCheck( GdipMultiplyPathGradientTransform(
                    GpPathGradient(GetNativeBrush()),
                    matrix.GetNativeMatrix(),
                    order));

  Result := Self;
end;

function TIGPPathGradientBrush.TranslateTransform(dx, dy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPPathGradientBrush;
begin
  ErrorCheck( GdipTranslatePathGradientTransform(
                    GpPathGradient(GetNativeBrush()),
                    dx, dy, order));
                      
  Result := Self;
end;

function TIGPPathGradientBrush.ScaleTransform(sx, sy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPPathGradientBrush;
begin
  ErrorCheck( GdipScalePathGradientTransform(
                    GpPathGradient(GetNativeBrush()),
                    sx, sy, order));
                      
  Result := Self;
end;

function TIGPPathGradientBrush.ScaleTransform(s: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPPathGradientBrush;
begin
  Result := ScaleTransform( s, s, order );
end;

function TIGPPathGradientBrush.RotateTransform(angle: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : TIGPPathGradientBrush;
begin
  ErrorCheck( GdipRotatePathGradientTransform(
                    GpPathGradient(GetNativeBrush()),
                    angle, order));
                      
  Result := Self;
end;

function TIGPPathGradientBrush.SetTransformT(matrix: IGPMatrix) : IGPTransformable;
begin
  ErrorCheck( GdipSetPathGradientTransform(
                    GpPathGradient(GetNativeBrush()),
                    matrix.GetNativeMatrix()));
                      
  Result := Self;
end;

function TIGPPathGradientBrush.ResetTransformT() : IGPTransformable;
begin
  ErrorCheck( GdipResetPathGradientTransform(
                    GpPathGradient(GetNativeBrush())));

  Result := Self;
end;

function TIGPPathGradientBrush.MultiplyTransformT(matrix: IGPMatrix; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipMultiplyPathGradientTransform(
                    GpPathGradient(GetNativeBrush()),
                    matrix.GetNativeMatrix(),
                    order));

  Result := Self;
end;

function TIGPPathGradientBrush.TranslateTransformT(dx, dy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipTranslatePathGradientTransform(
                    GpPathGradient(GetNativeBrush()),
                    dx, dy, order));
                      
  Result := Self;
end;

function TIGPPathGradientBrush.ScaleTransformT(sx, sy: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipScalePathGradientTransform(
                    GpPathGradient(GetNativeBrush()),
                    sx, sy, order));
                      
  Result := Self;
end;

function TIGPPathGradientBrush.ScaleTransformXYT(s: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  Result := ScaleTransformT( s, s, order );
end;

function TIGPPathGradientBrush.RotateTransformT(angle: Single; order: TIGPMatrixOrder = MatrixOrderPrepend) : IGPTransformable;
begin
  ErrorCheck( GdipRotatePathGradientTransform(
                    GpPathGradient(GetNativeBrush()),
                    angle, order));
                      
  Result := Self;
end;

function TIGPPathGradientBrush.GetFocusScales(out xScale, yScale: Single) : TIGPPathGradientBrush;
begin
  ErrorCheck( GdipGetPathGradientFocusScales(
                    GpPathGradient(GetNativeBrush()), xScale, yScale));
                      
  Result := Self;
end;

function TIGPPathGradientBrush.SetFocusScales(xScale, yScale: Single) : TIGPPathGradientBrush;
begin
  ErrorCheck( GdipSetPathGradientFocusScales(
                    GpPathGradient(GetNativeBrush()), xScale, yScale));
                      
  Result := Self;
end;

function TIGPPathGradientBrush.GetWrapMode() : TIGPWrapMode;
begin
  ErrorCheck( GdipGetPathGradientWrapMode(GpPathGradient(GetNativeBrush()), Result));
end;

function TIGPPathGradientBrush.SetWrapMode(wrapMode: TIGPWrapMode) : TIGPPathGradientBrush;
begin
  ErrorCheck( GdipSetPathGradientWrapMode(
                    GpPathGradient(GetNativeBrush()), wrapMode));

  Result := Self;
end;

procedure TIGPPathGradientBrush.SetWrapModeProp(wrapMode: TIGPWrapMode);
begin
  SetWrapMode( wrapMode );
end;

// -----------------------------------------------------------------------------
// TIGPBase class
// -----------------------------------------------------------------------------

class function TIGPBase.NewInstance() : TObject;
begin
  Result := InitInstance(GdipAlloc(ULONG(instanceSize)));
  TIGPBase(Result).FRefCount := 1;
end;

procedure TIGPBase.FreeInstance();
begin
  CleanupInstance();
  GdipFree(Self);
end;

{$IFDEF FPC}
// https://stackoverflow.com/questions/8950513/what-does-at-returnaddress-mean-in-delphi
// ZeosDBO->ZCompatibility.pas: Result := nil
function ReturnAddress: Pointer;
begin
  Result := nil;
end;
{$ENDIF}
class procedure TIGPBase.ErrorCheck( AStatus : TIGPStatus );
begin
  if( AStatus <> Ok ) then
    raise EGPException.Create( GetStatus( AStatus )) at ReturnAddress;

end;

// -----------------------------------------------------------------------------
// PathData class
// -----------------------------------------------------------------------------

constructor TIGPPathData.Create();
begin
  FCount := 0;
  FPoints := NIL;
  FTypes := NIL;
end;

destructor TIGPPathData.Destroy();
begin
  if( FPoints <> NIL ) then
    FreeMem(FPoints);

  if( FTypes <> NIL ) then
    FreeMem(FTypes);

end;

function TIGPPathData.GetCount()  : Integer;
begin
  Result := FCount;
end;

function TIGPPathData.GetPoints( Index : Integer  ) : TPointF;
begin
  Result := PGPPointF( PChar( FPoints ) + Index * SizeOf( TPointF ) )^;
end;

function TIGPPathData.GetTypes( Index : Integer ) : TIGPPathPointType;
begin
  Result := TIGPPathPointType( PByte( PChar( FTypes ) + Index )^ );
end;

// -----------------------------------------------------------------------------
// MetafileHeader class
// -----------------------------------------------------------------------------

function TIGPMetafileHeader.GetType() : TIGPMetafileType;
begin
  Result := FType;
end;

function TIGPMetafileHeader.GetMetafileSize() : UINT;
begin
  Result := FSize;
end;

function TIGPMetafileHeader.GetVersion() : UINT;
begin
  Result := FVersion;
end;

function TIGPMetafileHeader.GetEmfPlusFlags() : UINT;
begin
  Result := FEmfPlusFlags;
end;

function TIGPMetafileHeader.GetDpiX() : Single;
begin
  Result := FDpiX;
end;

function TIGPMetafileHeader.GetDpiY() : Single;
begin
  Result := FDpiY;
end;

function TIGPMetafileHeader.GetBounds() : TIGPRect;
begin
  Result.X      := FX;
  Result.Y      := FY;
  Result.Width  := FWidth;
  Result.Height := FHeight;
end;

function TIGPMetafileHeader.IsWmf() : Boolean;
begin
  Result :=  ((FType = MetafileTypeWmf) or (FType = MetafileTypeWmfPlaceable));
end;

function TIGPMetafileHeader.IsWmfPlaceable() : Boolean;
begin
  Result := (FType = MetafileTypeWmfPlaceable);
end;

function TIGPMetafileHeader.IsEmf() : Boolean;
begin
  Result := (FType = MetafileTypeEmf);
end;

function TIGPMetafileHeader.IsEmfOrEmfPlus() : Boolean;
begin
  Result := (FType >= MetafileTypeEmf);
end;

function TIGPMetafileHeader.IsEmfPlus() : Boolean;
begin
  Result := (FType >= MetafileTypeEmfPlusOnly)
end;

function TIGPMetafileHeader.IsEmfPlusDual() : Boolean;
begin
  Result := (FType = MetafileTypeEmfPlusDual)
end;

function TIGPMetafileHeader.IsEmfPlusOnly() : Boolean;
begin
  Result := (FType = MetafileTypeEmfPlusOnly)
end;

function TIGPMetafileHeader.IsDisplay() : Boolean;
begin
  Result := (IsEmfPlus and ((FEmfPlusFlags and GDIP_EMFPLUSFLAGS_DISPLAY) <> 0));
end;

function TIGPMetafileHeader.GetWmfHeader() : PMetaHeader;
begin
  if( IsWmf ) then
    Result :=  @FHeader.FWmfHeader

  else
    Result := NIL;

end;

function TIGPMetafileHeader.GetEmfHeader() : PENHMETAHEADER3;
begin
  if( IsEmfOrEmfPlus ) then
    Result := @FHeader.FEmfHeader

  else
    Result := NIL;

end;

{$ENDIF} //MSWINDOWS

// -----------------------------------------------------------------------------
// macros
// -----------------------------------------------------------------------------

function ObjectTypeIsValid(type_: TIGPObjectType) : Boolean;
begin
  Result :=  ((type_ >= GPObjectTypeMin) and (type_ <= GPObjectTypeMax));
end;

function GP_GDIP_WMF_RECORD_TO_EMFPLUS(n: Integer) : Integer;
begin
  Result := (n or GDIP_WMF_RECORD_BASE);
end;

function GP_GDIP_EMFPLUS_RECORD_TO_WMF(n: Integer) : Integer;
begin
  Result := n and (not GDIP_WMF_RECORD_BASE);
end;

function GP_GDIP_IS_WMF_RECORDTYPE(n: Integer) : Boolean;
begin
  Result := ((n and GDIP_WMF_RECORD_BASE) <> 0);
end;


//--------------------------------------------------------------------------
// TPointF Util
//--------------------------------------------------------------------------

class function TIGPPointFHelper.Create( XY: Single ) : TPointF;
begin
  Result.X := XY;
  Result.Y := XY;
end;

//--------------------------------------------------------------------------
// TPoint Util
//--------------------------------------------------------------------------

class function TIGPPointHelper.Create( XY: Integer ) : TPoint;
begin
  Result.X := XY;
  Result.Y := XY;
end;

//--------------------------------------------------------------------------
// TIGPSize Util
//--------------------------------------------------------------------------

constructor TIGPSizeF.Create( _Width, _Height : Single );
begin
  Width := _Width;
  Height := _Height;
end;

constructor TIGPSizeF.Create( ASize : Single);
begin
  Width := ASize;
  Height := ASize;
end;

constructor TIGPSize.Create( _Width, _Height : Integer );
begin
  Width := _Width;
  Height := _Height;
end;

constructor TIGPSize.Create( ASize : Integer );
begin
  Width := ASize;
  Height := ASize;
end;

//--------------------------------------------------------------------------
// TCharacterRange Util
//--------------------------------------------------------------------------

function MakeCharacterRange(First, Length: Integer) : TIGPCharacterRange;
begin
  Result.First  := First;
  Result.Length := Length;
end;

// -----------------------------------------------------------------------------
// RectF class
// -----------------------------------------------------------------------------

constructor TIGPRectF.Create( _x, _y, _width, _height : Single );
begin
  X      := _x;
  Y      := _y;
  Width  := _width;
  Height := _height;
end;

constructor TIGPRectF.Create( const location: TPointF; const size: TIGPSizeF );
begin
  X      := location.X;
  Y      := location.Y;
  Width  := size.Width;
  Height := size.Height;
end;

constructor TIGPRectF.Create( const Rect: TRect );
begin
  X := Rect.Left;
  Y := Rect.Top;
  Width := Rect.Right - Rect.Left - 1;
  Height:= Rect.Bottom - Rect.Top - 1;
end;

constructor TIGPRectF.Create( const Rect: TIGPRect );
begin
  X := Rect.X;
  Y := Rect.Y;
  Width := Rect.Width;
  Height:= Rect.Height;
end;

function TIGPRectF.GetPosition() : TPointF;
begin
  Result.X := X;
  Result.Y := Y;
end;

procedure TIGPRectF.SetPosition( const AValue : TPointF );
begin
  X := AValue.X;
  Y := AValue.Y;
end;

function TIGPRectF.GetSize() : TIGPSizeF;
begin
  Result.Width := Width;
  Result.Height := Height;
end;

procedure TIGPRectF.SetSize( const AValue : TIGPSizeF );
begin
  Width := AValue.Width;
  Height := AValue.Height;
end;

function TIGPRectF.GetCenter() : TPointF;
begin
  Result.X := X + Width / 2;
  Result.Y := Y + Height / 2;
end;

procedure TIGPRectF.SetCenter( const AValue : TPointF );
begin
  X := AValue.X - Width / 2;
  Y := AValue.Y - Height / 2;
end;

function TIGPRectF.GetEndPoint() : TPointF;
begin
  Result.X := X + Width;
  Result.Y := Y + Height;
end;

procedure TIGPRectF.SetEndPoint( const AValue : TPointF );
begin
  Width := AValue.X - X;
  Height := AValue.Y - Y;
end;

function TIGPRectF.AsRect() : TRectF;
begin
  Result.Left := X;
  Result.Top := Y;
  Result.Right := X + Width;
  Result.Bottom := Y + Height;
end;

function TIGPRectF.HitTest( AX, AY : Single ) : Boolean;
begin
  if( AX < X ) then
    Exit( False );

  if( AY < Y ) then
    Exit( False );

  if( AX > X + Width ) then
    Exit( False );

  if( AY > Y + Height ) then
    Exit( False );

  Exit( True );
end;

function TIGPRectF.HitTest( const APoint : TPointF ) : Boolean;
begin
  if( APoint.X < X ) then
    Exit( False );

  if( APoint.Y < Y ) then
    Exit( False );

  if( APoint.X > X + Width ) then
    Exit( False );

  if( APoint.Y > Y + Height ) then
    Exit( False );

  Exit( True );
end;

function TIGPRectF.HitTest( const APoint : TPoint ) : Boolean;
begin
  if( APoint.X < X ) then
    Exit( False );

  if( APoint.Y < Y ) then
    Exit( False );

  if( APoint.X > X + Width ) then
    Exit( False );

  if( APoint.Y > Y + Height ) then
    Exit( False );

  Exit( True );
end;

function TIGPRectF.Intersects( const ARect : TIGPRectF ) : Boolean;
begin
  if( X + Width < ARect.X ) then
    Exit( False );

  if( Y + Height < ARect.Y ) then
    Exit( False );

  if( ARect.X + ARect.Width < X ) then
    Exit( False );

  if( ARect.Y + ARect.Height < Y ) then
    Exit( False );

  Exit( True );
end;

// -----------------------------------------------------------------------------
// Rect class
// -----------------------------------------------------------------------------

constructor TIGPRect.Create( _x, _y, _width, _height : Integer );
begin
  X      := _x;
  Y      := _y;
  Width  := _width;
  Height := _height;
end;

constructor TIGPRect.Create( const location: TPoint; const size: TIGPSize );
begin
  X      := location.X;
  Y      := location.Y;
  Width  := size.Width;
  Height := size.Height;
end;

constructor TIGPRect.Create( const Rect: TRect );
begin
  X := rect.Left;
  Y := Rect.Top;
  Width := Rect.Right-Rect.Left;
  Height:= Rect.Bottom-Rect.Top;
end;

function TIGPRect.GetPosition() : TPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

procedure TIGPRect.SetPosition( const AValue : TPoint );
begin
  X := AValue.X;
  Y := AValue.Y;
end;

function TIGPRect.GetSize() : TIGPSize;
begin
  Result.Width := Width;
  Result.Height := Height;
end;

procedure TIGPRect.SetSize( const AValue : TIGPSize );
begin
  Width := AValue.Width;
  Height := AValue.Height;
end;

function TIGPRect.GetCenter() : TPoint;
begin
  Result.X := X + Width div 2;
  Result.Y := Y + Height div 2;
end;

procedure TIGPRect.SetCenter( const AValue : TPoint );
begin
  X := AValue.X - Width div 2;
  Y := AValue.Y - Height div 2;
end;

function TIGPRect.GetEndPoint() : TPoint;
begin
  Result.X := X + Width;
  Result.Y := Y + Height;
end;

procedure TIGPRect.SetEndPoint( const AValue : TPoint );
begin
  Width := AValue.X - X;
  Height := AValue.Y - Y;
end;

function TIGPRect.AsRect() : TRect;
begin
  Result.Left := X;
  Result.Top := Y;
  Result.Right := X + Width;
  Result.Bottom := Y + Height;
end;

function TIGPRect.HitTest( AX, AY : Integer ) : Boolean;
begin
  if( AX < X ) then
    Exit( False );

  if( AY < Y ) then
    Exit( False );

  if( AX > X + Width ) then
    Exit( False );

  if( AY > Y + Height ) then
    Exit( False );

  Exit( True );
end;

function TIGPRect.HitTest( const APoint : TPoint ) : Boolean;
begin
  if( APoint.X < X ) then
    Exit( False );

  if( APoint.Y < Y ) then
    Exit( False );

  if( APoint.X > X + Width ) then
    Exit( False );

  if( APoint.Y > Y + Height ) then
    Exit( False );

  Exit( True );
end;

function TIGPRect.HitTest( const APoint : TPointF ) : Boolean;
begin
  if( APoint.X < X ) then
    Exit( False );

  if( APoint.Y < Y ) then
    Exit( False );

  if( APoint.X > X + Width ) then
    Exit( False );

  if( APoint.Y > Y + Height ) then
    Exit( False );

  Exit( True );
end;

function TIGPRect.Intersects( const ARect : TIGPRect ) : Boolean;
begin
  if( X + Width < ARect.X ) then
    Exit( False );

  if( Y + Height < ARect.Y ) then
    Exit( False );

  if( ARect.X + ARect.Width < X ) then
    Exit( False );

  if( ARect.Y + ARect.Height < Y ) then
    Exit( False );

  Exit( True );
end;

function GPInflateRect( const ARect: TIGPRect; CX, CY: Integer ) : TIGPRect;
begin
  Result := ARect;

  Dec( Result.X, CX );
  Dec( Result.Y, CY );
  Inc( Result.Width, CX * 2 );
  Inc( Result.Height, CY * 2 );
end;

function GPInflateRect( const ARect: TIGPRect; Change : Integer ) : TIGPRect;
begin
  Result := ARect;

  Dec( Result.X, Change );
  Dec( Result.Y, Change );
  Inc( Result.Width, Change * 2 );
  Inc( Result.Height, Change * 2 );
end;

function GPInflateRectF( const ARect: TIGPRectF; CX, CY: Single ) : TIGPRectF;
begin
  Result.X := ARect.X - CX;
  Result.Y := ARect.Y - CY;
  Result.Width := ARect.Width + CX * 2;
  Result.Height := ARect.Height + CY * 2;
end;

function GPInflateRectF( const ARect: TIGPRectF; Change : Single ) : TIGPRectF;
begin
  Result.X := ARect.X - Change;
  Result.Y := ARect.Y - Change;
  Result.Width := ARect.Width + Change * 2;
  Result.Height := ARect.Height + Change * 2;
end;

function GPIntersectRect( const ARect1 : TIGPRect; const ARect2 : TIGPRect ) : TIGPRect;
var
  AIntersectRect : TRect;

begin
  IntersectRect( AIntersectRect, ARect1.AsRect(), ARect2.AsRect());
  Result := TIGPRect.Create( AIntersectRect );
end;

function GPCheckIntersectRect( const ARect1 : TIGPRect; const ARect2 : TIGPRect ) : Boolean;
var
  AIntersectRect : TRect;

begin
  Result := IntersectRect( AIntersectRect, ARect1.AsRect(), ARect2.AsRect());
end;

function GPEqualRect( const ARect1 : TIGPRect; const ARect2 : TIGPRect ) : Boolean;
begin
  Result := ( ARect1.X = ARect2.X ) and ( ARect1.Y = ARect2.Y ) and ( ARect1.Width = ARect2.Width ) and ( ARect1.Height = ARect2.Height );
end;

function GetPixelFormatSize(pixfmt: TIGPPixelFormat) : Cardinal;
begin
Result := (pixfmt shr 8) and $ff;
end;

function IsIndexedPixelFormat(pixfmt: TIGPPixelFormat) : Boolean;
begin
Result := (pixfmt and GPPixelFormatIndexed) <> 0;
end;

function IsAlphaPixelFormat(pixfmt: TIGPPixelFormat) : Boolean;
begin
Result := (pixfmt and GPPixelFormatAlpha) <> 0;
end;

function IsExtendedPixelFormat(pixfmt: TIGPPixelFormat) : Boolean;
begin
Result := (pixfmt and GPPixelFormatExtended) <> 0;
end;

function IsCanonicalPixelFormat(pixfmt: TIGPPixelFormat) : Boolean;
begin
  Result := (pixfmt and GPPixelFormatCanonical) <> 0;
end;

function ColorToRGB(Color: TColor): Longint;
begin
{$IFDEF MSWINDOWS}
  if( Color < 0 ) then
    Result := GetSysColor(Color and $000000FF)

  else
{$ENDIF}
    Result := Color;

end;

{$IFNDEF MSWINDOWS}
function GetRValue(rgb: DWORD): Byte;
begin
  Result := Byte(rgb);
end;

function GetGValue(rgb: DWORD): Byte;
begin
  Result := Byte(rgb shr 8);
end;

function GetBValue(rgb: DWORD): Byte;
begin
  Result := Byte(rgb shr 16);
end;

function RGB(r, g, b: Byte): TAlphaColor;
begin
  Result := (r or (g shl 8) or (b shl 16));
end;
{$ENDIF}

function MakeARGBColor( AAlpha : Byte; AColor : TAlphaColor ) : TAlphaColor; //overload;
begin
  Result := ( AColor and not AlphaMask ) or (DWORD(AAlpha) shl AlphaShift);
end;

function GPMakeGrayScaleColor( AAlpha : Byte; AColor : Byte ) : TAlphaColor;
begin
  Result := GPMakeColor( AAlpha, AColor, AColor, AColor );
end;

function MakeColor( AColor : TColor ) : TAlphaColor; overload;
begin
  Result := MakeColor( 255, AColor );
end;

function MakeColor( AAlpha : Byte; AColor : TColor ) : TAlphaColor; overload;
begin
  AColor := ColorToRGB( AColor );
  Result := MakeColor( AAlpha, GetRValue( AColor ), GetGValue( AColor ), GetBValue( AColor ));
end;

function GPGetColor( AColor : TAlphaColor ) : TColor;
begin
  Result := RGB( GetRed( AColor ), GetGreen( AColor ), GetBlue( AColor ));
end;

function MakeColor(r, g, b: Byte) : TAlphaColor; overload;
begin
  Result := GPMakeColor(255, r, g, b);
end;

function MakeColor(a, r, g, b: Byte) : TAlphaColor; overload;
begin
  Result := ((DWORD(b) shl BlueShift  ) or
             (DWORD(g) shl GreenShift ) or
             (DWORD(r) shl RedShift   ) or
             (DWORD(a) shl AlphaShift ));
end;

function GPMakeColor( AColor : TColor ) : TAlphaColor; overload;
begin
  Result := GPMakeColor( 255, AColor );
end;

function GPMakeColor( AAlpha : Byte; AColor : TColor ) : TAlphaColor; overload;
begin
  AColor := ColorToRGB( AColor );
  Result := MakeColor( AAlpha, GetRValue( AColor ), GetGValue( AColor ), GetBValue( AColor ));
end;

function GPMakeColor(r, g, b: Byte) : TAlphaColor; overload;
begin
  Result := GPMakeColor(255, r, g, b);
end;

function GPMakeColor(a, r, g, b: Byte) : TAlphaColor; overload;
begin
  Result := ((DWORD(b) shl  BlueShift) or
             (DWORD(g) shl GreenShift) or
             (DWORD(r) shl   RedShift) or
             (DWORD(a) shl AlphaShift));
end;

function GetAlpha(color: TAlphaColor) : BYTE;
begin
  Result := BYTE(color shr AlphaShift);
end;

function GetRed(color: TAlphaColor) : BYTE;
begin
  Result := BYTE(color shr RedShift);
end;

function GetGreen(color: TAlphaColor) : BYTE;
begin
  Result := BYTE(color shr GreenShift);
end;

function GetBlue(color: TAlphaColor) : BYTE;
begin
  Result := BYTE(color shr BlueShift);
end;

{$IFDEF MSWINDOWS}
function ColorRefToARGB(rgb: COLORREF) : TAlphaColor;
begin
  Result := GPMakeColor(255, GetRValue(rgb), GetGValue(rgb), GetBValue(rgb));
end;

function ARGBToColorRef(Color: TAlphaColor) : COLORREF;
begin
  Result := RGB(GetRed(Color), GetGreen(Color), GetBlue(Color));
end;
{$ENDIF}

function RGBToBGR(color: TAlphaColor) : TAlphaColor;
begin
  Result := GPMakeColor( GetAlpha( color ), GetRValue(color), GetGValue(color), GetBValue(color) );
end;

function MakeBlend( APosition : Single; AValue : Single ) : TIGPBlend;
begin
  Result.Position := APosition;
  Result.Value := AValue;
end;

function MakeInterpolationColor( APosition : Single; AColor : TAlphaColor ) : TIGPInterpolationColor;
begin
  Result.Position := APosition;
  Result.Color := AColor;
end;

function GetStatus(Stat: TIGPStatus) : String;
begin
  case Stat of
    Ok                        : Result := 'Ok';
    GenericError              : Result := 'GenericError';
    InvalidParameter          : Result := 'InvalidParameter';
    OutOfMemory               : Result := 'OutOfMemory';
    ObjectBusy                : Result := 'ObjectBusy';
    InsufficientBuffer        : Result := 'InsufficientBuffer';
    NotImplemented            : Result := 'NotImplemented';
    Win32Error                : Result := 'Win32Error';
    WrongState                : Result := 'WrongState';
    Aborted                   : Result := 'Aborted';
    FileNotFound              : Result := 'FileNotFound';
    ValueOverflow             : Result := 'ValueOverflow';
    AccessDenied              : Result := 'AccessDenied';
    UnknownImageFormat        : Result := 'UnknownImageFormat';
    FontFamilyNotFound        : Result := 'FontFamilyNotFound';
    FontStyleNotFound         : Result := 'FontStyleNotFound';
    NotTrueTypeFont           : Result := 'NotTrueTypeFont';
    UnsupportedGdiplusVersion : Result := 'UnsupportedGdiplusVersion';
    GdiplusNotInitialized     : Result := 'GdiplusNotInitialized';
    PropertyNotFound          : Result := 'PropertyNotFound';
    PropertyNotSupported      : Result := 'PropertyNotSupported';
    ProfileNotFound           : Result := 'ProfileNotFound';
  else
    Result := '<UnKnown>';
  end;
end;

procedure GetStandardRGBAColorNames( ANames : TStrings );
var
  I : Integer;

begin
  for I := 0 to SizeOf( GPRGBAColorNames ) div SizeOf( GPRGBAColorNames[ 0 ] ) - 1 do
    ANames.Add( GPRGBAColorNames[ I ].Name );
    
end;

procedure GetStandardRGBAColorNames( Proc: TGetStrProc );
var
  I : Integer;

begin
  for I := 0 to SizeOf( GPRGBAColorNames ) div SizeOf( GPRGBAColorNames[ 0 ] ) - 1 do
    Proc( GPRGBAColorNames[ I ].Name );

end;

function HexToUInt( AValue : String ) : Cardinal;
var
  I   : Integer;
  Tmp : Byte;

begin
  Result := 0;
{$IFDEF DCC}
  {$IF CompilerVersion <= 31} // Delphi RX 10.1 Berlin
    Tmp := 0;
  {$IFEND}
{$ELSE}
  Tmp := 0;
{$ENDIF}
  AValue := UpperCase( AValue );
  for I := 1 to Length( AValue ) do
    begin
    if(( I = 1 ) and ( AValue[ 1 ] = '$' )) then
      Continue;

    case( AValue[ I ] ) of
      '0' : Tmp := 0;
      '1' : Tmp := 1;
      '2' : Tmp := 2;
      '3' : Tmp := 3;
      '4' : Tmp := 4;
      '5' : Tmp := 5;
      '6' : Tmp := 6;
      '7' : Tmp := 7;
      '8' : Tmp := 8;
      '9' : Tmp := 9;
      'A' : Tmp := 10;
      'B' : Tmp := 11;
      'C' : Tmp := 12;
      'D' : Tmp := 13;
      'E' : Tmp := 14;
      'F' : Tmp := 15;

      else
        Break;

      end;

    Result := Result * 16;
    Result := Result + Tmp;
    end;

end;

function StringToRGBAColor( AValue : String ) : TAlphaColor;
var
  I : Integer;

begin
  AValue := Trim( AValue );
  Result := aclBlack;
  if( Length( AValue ) < 1 ) then
    Exit;

  if( AValue[ 1 ] = '$' ) then
    begin
    Result := HexToUInt( AValue );
//    HexToInt( PChar( @AValue[ 2 ] ), PAnsiChar( @Result ), 8 );
    Exit;
    end

  else
    for I := 0 to SizeOf( GPRGBAColorNames ) div SizeOf( GPRGBAColorNames[ 0 ] ) - 1 do
      if( GPRGBAColorNames[ I ].Name = AValue ) then
        Exit( GPRGBAColorNames[ I ].Color );

  Result := TAlphaColor( StrToInt64Def( AValue, Int64( aclBlack )));
end;

function RGBAColorToString( AValue : TAlphaColor ) : String;
var
  I : Integer;

begin
  for I := 0 to SizeOf( GPRGBAColorNames ) div SizeOf( GPRGBAColorNames[ 0 ] ) - 1 do
    if( GPRGBAColorNames[ I ].Color = AValue ) then
      Exit( GPRGBAColorNames[ I ].Name );

  Result := '$' + IntToHex( AValue, 8 );
end;

{$IFDEF MSWINDOWS}
procedure StartIGDIPlus();
begin
  if( GInitialized ) then
    Exit;

  GInitialized := True;
  // Initialize StartupInput structure
  StartupInput.DebugEventCallback := NIL;
  StartupInput.SuppressBackgroundThread := True;
  StartupInput.SuppressExternalCodecs   := False;
  StartupInput.GdiplusVersion := 1;
  // Initialize GDI+
  GdiplusStartup( gdiplusToken, @StartupInput, @StartupOutput);
  if( Assigned( StartupOutput.NotificationHook )) then
    StartupOutput.NotificationHook( gdiplusBGThreadToken );

end;

procedure StopIGDIPlus();
begin
  if( not GInitialized ) then
    Exit;

  GInitialized := False;
  if( Assigned(GenericSansSerifFontFamily)) then
    GenericSansSerifFontFamily.
    {$IFDEF DCC}
      {$IFDEF DELPHI_XE4_OR_UP}
      DisposeOf()
      {$ELSE}
      Free
      {$ENDIF}
    {$ELSE}Free
    {$ENDIF};

  if( Assigned(GenericSerifFontFamily)) then
    GenericSerifFontFamily.
    {$IFDEF DCC}
      {$IFDEF DELPHI_XE4_OR_UP}
      DisposeOf()
      {$ELSE}
      Free
      {$ENDIF}
    {$ELSE}Free
    {$ENDIF};

  if( Assigned(GenericMonospaceFontFamily)) then
    GenericMonospaceFontFamily.
    {$IFDEF DCC}
      {$IFDEF DELPHI_XE4_OR_UP}
      DisposeOf()
      {$ELSE}
      Free
      {$ENDIF}
    {$ELSE}Free
    {$ENDIF};

  if( Assigned(GenericTypographicStringFormatBuffer)) then
    GenericTypographicStringFormatBuffer.
    {$IFDEF DCC}
      {$IFDEF DELPHI_XE4_OR_UP}
      DisposeOf()
      {$ELSE}
      Free
      {$ENDIF}
    {$ELSE}Free
    {$ENDIF};

  if( Assigned(GenericDefaultStringFormatBuffer)) then
    GenericDefaultStringFormatBuffer.
    {$IFDEF DCC}
      {$IFDEF DELPHI_XE4_OR_UP}
      DisposeOf()
      {$ELSE}
      Free
      {$ENDIF}
    {$ELSE}Free
    {$ENDIF};

  // Close GDI +
  if( Assigned( StartupOutput.NotificationUnhook )) then
    StartupOutput.NotificationUnhook( gdiplusBGThreadToken );

  GdiplusShutdown(gdiplusToken);
end;

{$IFNDEF NO_IGDI_SELFINIT}
initialization
  StartIGDIPlus();

finalization
  StopIGDIPlus();

{$ENDIF}

{$ENDIF}
end.
