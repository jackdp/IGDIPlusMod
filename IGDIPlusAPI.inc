const WINGDIPDLL = 'gdiplus.dll';

type
  GpStatus          = TIGPStatus;
  GpFillMode        = TIGPFillMode;
  GpUnit            = TIGPUnit;
  GpCoordinateSpace = TIGPCoordinateSpace;
  GpPointF          = PGPPointF;
  GpPoint           = PGPPoint;
  GpRectF           = PGPRectF;
  GpRect            = PGPRect;
  GpSizeF           = PGPSizeF;
  PGPBitmapDataRecord = ^TIGPBitmapDataRecord;

  TIGPImageAbort = function ( callbackData: Pointer ) : BOOL; stdcall;
  TIGPDrawImageAbort         = TIGPImageAbort;
  TIGPGetThumbnailImageAbort = TIGPImageAbort;

  GpEnumerateMetafileProc = function(recordType: TIGPEmfPlusRecordType; flags: UINT;
    dataSize: UINT; data: PBYTE; callbackData: pointer): BOOL; stdcall;

  // GDI+ initialization. Must not be called from DllMain - can cause deadlock.
  //
  // Must be called before GDI+ API's or constructors are used.
  //
  // token  - may not be NULL - accepts a token to be passed in the corresponding
  //          GdiplusShutdown call.
  // input  - may not be NULL
  // output - may be NULL only if input->SuppressBackgroundThread is FALSE.

 function GdiplusStartup( out token: Pointer; input: PGPGdiplusStartupInput;
   output: PGPGdiplusStartupOutput): TIGPStatus; stdcall; external WINGDIPDLL;
  // GDI+ termination. Must be called before GDI+ is unloaded.
  // Must not be called from DllMain - can cause deadlock.
  //
  // GDI+ API's may not be called after GdiplusShutdown. Pay careful attention
  // to GDI+ object destructors.

  procedure GdiplusShutdown(token: Pointer); stdcall; external WINGDIPDLL;
  //----------------------------------------------------------------------------
  // Memory Allocation APIs
  //----------------------------------------------------------------------------

  function GdipAlloc(size: ULONG): pointer; stdcall; external WINGDIPDLL;
  procedure GdipFree(ptr: pointer); stdcall; external WINGDIPDLL;

  function GdipCreatePath(brushMode: GPFILLMODE;
    out path: GPPATH): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreatePath2(v1: GPPOINTF; v2: PBYTE; v3: Integer; v4: GPFILLMODE;
    out path: GPPATH): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreatePath2I(v1: GPPOINT; v2: PBYTE; v3: Integer; v4: GPFILLMODE;
    out path: GPPATH): GpStatus; stdcall; external WINGDIPDLL;

  function GdipClonePath(path: GPPATH;
    out clonePath: GPPATH): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDeletePath(path: GPPATH): GpStatus; stdcall; external WINGDIPDLL;

  function GdipResetPath(path: GPPATH): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPointCount(path: GPPATH;
    out count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPathTypes(path: GPPATH; types: PBYTE;
    count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPathPoints(v1: GPPATH; points: GPPOINTF;
    count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPathPointsI(v1: GPPATH; points: GPPOINT;
             count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPathFillMode(path: GPPATH;
    var fillmode: GPFILLMODE): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetPathFillMode(path: GPPATH;
    fillmode: GPFILLMODE): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPathData(path: GPPATH;
    pathData: Pointer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipStartPathFigure(path: GPPATH): GpStatus; stdcall; external WINGDIPDLL;
  function GdipClosePathFigure(path: GPPATH): GpStatus; stdcall; external WINGDIPDLL;
  function GdipClosePathFigures(path: GPPATH): GpStatus; stdcall; external WINGDIPDLL;
  function GdipSetPathMarker(path: GPPATH): GpStatus; stdcall; external WINGDIPDLL;
  function GdipClearPathMarkers(path: GPPATH): GpStatus; stdcall; external WINGDIPDLL;
  function GdipReversePath(path: GPPATH): GpStatus; stdcall; external WINGDIPDLL;
  function GdipGetPathLastPoint(path: GPPATH;
    lastPoint: GPPOINTF): GpStatus; stdcall; external WINGDIPDLL;

  function GdipAddPathLine(path: GPPATH;
    x1, y1, x2, y2: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipAddPathLine2(path: GPPATH; points: GPPOINTF;
    count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipAddPathArc(path: GPPATH; x, y, width, height, startAngle,
    sweepAngle: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipAddPathBezier(path: GPPATH;
    x1, y1, x2, y2, x3, y3, x4, y4: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipAddPathBeziers(path: GPPATH; points: GPPOINTF;
    count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipAddPathCurve(path: GPPATH; points: GPPOINTF;
    count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipAddPathCurve2(path: GPPATH; points: GPPOINTF; count: Integer;
    tension: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipAddPathCurve3(path: GPPATH; points: GPPOINTF; count: Integer;
    offset: Integer; numberOfSegments: Integer;
    tension: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipAddPathClosedCurve(path: GPPATH; points: GPPOINTF;
    count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipAddPathClosedCurve2(path: GPPATH; points: GPPOINTF;
    count: Integer; tension: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipAddPathRectangle(path: GPPATH; x: Single; y: Single;
    width: Single; height: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipAddPathRectangles(path: GPPATH; rects: GPRECTF;
    count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipAddPathEllipse(path: GPPATH;  x: Single; y: Single;
    width: Single; height: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipAddPathPie(path: GPPATH; x: Single; y: Single; width: Single;
    height: Single; startAngle: Single; sweepAngle: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipAddPathPolygon(path: GPPATH; points: GPPOINTF;
    count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipAddPathPath(path: GPPATH; addingPath: GPPATH;
    connect: Bool): GpStatus; stdcall; external WINGDIPDLL;

  function GdipAddPathString(path: GPPATH; string_: PWCHAR; length: Integer;
    family: GPFONTFAMILY; style: Integer; emSize: Single; layoutRect: PGPRectF;
    format: GpStringFormat): GpStatus; stdcall; external WINGDIPDLL;

  function GdipAddPathStringI(path: GPPATH; string_: PWCHAR; length: Integer;
    family: GPFONTFAMILY; style: Integer; emSize: Single; layoutRect: PGPRect;
    format: GpStringFormat): GpStatus; stdcall; external WINGDIPDLL;

  function GdipAddPathLineI(path: GPPATH; x1: Integer; y1: Integer; x2: Integer;
    y2: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipAddPathLine2I(path: GPPATH; points: GPPOINT;
    count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipAddPathArcI(path: GPPATH; x: Integer; y: Integer; width: Integer;
    height: Integer; startAngle: Single; sweepAngle: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipAddPathBezierI(path: GPPATH; x1: Integer; y1: Integer;
    x2: Integer; y2: Integer; x3: Integer; y3: Integer; x4: Integer;
    y4: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipAddPathBeziersI(path: GPPATH; points: GPPOINT;
    count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipAddPathCurveI(path: GPPATH; points: GPPOINT;
    count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipAddPathCurve2I(path: GPPATH; points: GPPOINT; count: Integer;
    tension: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipAddPathCurve3I(path: GPPATH; points: GPPOINT; count: Integer;
    offset: Integer; numberOfSegments: Integer;
    tension: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipAddPathClosedCurveI(path: GPPATH; points: GPPOINT;
    count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipAddPathClosedCurve2I(path: GPPATH; points: GPPOINT;
    count: Integer; tension: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipAddPathRectangleI(path: GPPATH; x: Integer; y: Integer;
    width: Integer; height: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipAddPathRectanglesI(path: GPPATH; rects: GPRECT;
    count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipAddPathEllipseI(path: GPPATH; x: Integer; y: Integer;
    width: Integer; height: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipAddPathPieI(path: GPPATH; x: Integer; y: Integer; width: Integer;
    height: Integer; startAngle: Single; sweepAngle: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipAddPathPolygonI(path: GPPATH; points: GPPOINT;
    count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipFlattenPath(path: GPPATH; matrix: GPMATRIX;
    flatness: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipWindingModeOutline(path: GPPATH; matrix: GPMATRIX;
    flatness: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipWidenPath(nativePath: GPPATH; pen: GPPEN; matrix: GPMATRIX;
    flatness: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipWarpPath(path: GPPATH; matrix: GPMATRIX; points: GPPOINTF;
    count: Integer; srcx: Single; srcy: Single; srcwidth: Single;
    srcheight: Single; warpMode: TIGPWarpMode; flatness: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipTransformPath(path: GPPATH; matrix: GPMATRIX): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPathWorldBounds(path: GPPATH; bounds: GPRECTF;
    matrix: GPMATRIX; pen: GPPEN): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPathWorldBoundsI(path: GPPATH; bounds: GPRECT;
    matrix: GPMATRIX; pen: GPPEN): GpStatus; stdcall; external WINGDIPDLL;

  function GdipIsVisiblePathPoint(path: GPPATH; x: Single; y: Single;
    graphics: GpGraphics; out result: Bool): GpStatus; stdcall; external WINGDIPDLL;
  
  function GdipIsVisiblePathPointI(path: GPPATH; x: Integer; y: Integer;
    graphics: GpGraphics; out result: Bool): GpStatus; stdcall; external WINGDIPDLL;

  function GdipIsOutlineVisiblePathPoint(path: GPPATH; x: Single; y: Single;
    pen: GPPEN; graphics: GpGraphics; out result: Bool): GpStatus; stdcall; external WINGDIPDLL;

  function GdipIsOutlineVisiblePathPointI(path: GPPATH; x: Integer; y: Integer;
    pen: GPPEN; graphics: GpGraphics; out result: Bool): GpStatus; stdcall; external WINGDIPDLL;

//----------------------------------------------------------------------------
// PathIterator APIs 
//----------------------------------------------------------------------------

  function GdipCreatePathIter(out iterator: GpPathIterator;
    path: GPPATH): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDeletePathIter(iterator: GpPathIterator): GpStatus; stdcall; external WINGDIPDLL;

  function GdipPathIterNextSubpath(iterator: GpPathIterator;
    var resultCount: Integer; var startIndex: Integer; var endIndex: Integer;
    out isClosed: Bool): GpStatus; stdcall; external WINGDIPDLL;

  function GdipPathIterNextSubpathPath(iterator: GpPathIterator;
    var resultCount: Integer; path: GPPATH;
    out isClosed: Bool): GpStatus; stdcall; external WINGDIPDLL;

  function GdipPathIterNextPathType(iterator: GpPathIterator;
    var resultCount: Integer; pathType: PBYTE; var startIndex: Integer;
    var endIndex: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipPathIterNextMarker(iterator: GpPathIterator;
    var resultCount: Integer; var startIndex: Integer;
    var endIndex: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipPathIterNextMarkerPath(iterator: GpPathIterator;
    var resultCount: Integer; path: GPPATH): GpStatus; stdcall; external WINGDIPDLL;

  function GdipPathIterGetCount(iterator: GpPathIterator;
    out count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipPathIterGetSubpathCount(iterator: GpPathIterator;
    out count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipPathIterIsValid(iterator: GpPathIterator;
    out valid: Bool): GpStatus; stdcall; external WINGDIPDLL;

  function GdipPathIterHasCurve(iterator: GpPathIterator;
    out hasCurve: Bool): GpStatus; stdcall; external WINGDIPDLL;

  function GdipPathIterRewind(iterator: GpPathIterator): GpStatus; stdcall; external WINGDIPDLL;

  function GdipPathIterEnumerate(iterator: GpPathIterator;
    var resultCount: Integer; points: GPPOINTF; types: PBYTE;
    count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipPathIterCopyData(iterator: GpPathIterator;
    var resultCount: Integer; points: GPPOINTF; types: PBYTE;
    startIndex: Integer; endIndex: Integer): GpStatus; stdcall; external WINGDIPDLL;

//----------------------------------------------------------------------------
// Matrix APIs
//----------------------------------------------------------------------------

  function GdipCreateMatrix(out matrix: GPMATRIX): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateMatrix2(m11: Single; m12: Single; m21: Single; m22: Single;
    dx: Single; dy: Single; out matrix: GPMATRIX): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateMatrix3(rect: GPRECTF; dstplg: GPPOINTF;
    out matrix: GPMATRIX): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateMatrix3I(rect: GPRECT; dstplg: GPPOINT;
    out matrix: GPMATRIX): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCloneMatrix(matrix: GPMATRIX;
    out cloneMatrix: GPMATRIX): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDeleteMatrix(matrix: GPMATRIX): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetMatrixElements(matrix: GPMATRIX; m11: Single; m12: Single;
    m21: Single; m22: Single; dx: Single; dy: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipMultiplyMatrix(matrix: GPMATRIX; matrix2: GPMATRIX;
    order: TIGPMatrixOrder): GpStatus; stdcall; external WINGDIPDLL;

  function GdipTranslateMatrix(matrix: GPMATRIX; offsetX: Single;
    offsetY: Single; order: TIGPMatrixOrder): GpStatus; stdcall; external WINGDIPDLL;

  function GdipScaleMatrix(matrix: GPMATRIX; scaleX: Single; scaleY: Single;
    order: TIGPMatrixOrder): GpStatus; stdcall; external WINGDIPDLL;

  function GdipRotateMatrix(matrix: GPMATRIX; angle: Single;
    order: TIGPMatrixOrder): GpStatus; stdcall; external WINGDIPDLL;

  function GdipShearMatrix(matrix: GPMATRIX; shearX: Single; shearY: Single;
    order: TIGPMatrixOrder): GpStatus; stdcall; external WINGDIPDLL;

  function GdipInvertMatrix(matrix: GPMATRIX): GpStatus; stdcall; external WINGDIPDLL;

  function GdipTransformMatrixPoints(matrix: GPMATRIX; pts: GPPOINTF;
    count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipTransformMatrixPointsI(matrix: GPMATRIX; pts: GPPOINT;
    count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipVectorTransformMatrixPoints(matrix: GPMATRIX; pts: GPPOINTF;
    count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipVectorTransformMatrixPointsI(matrix: GPMATRIX; pts: GPPOINT;
    count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetMatrixElements(matrix: GPMATRIX;
    matrixOut: PSingle): GpStatus; stdcall; external WINGDIPDLL;

  function GdipIsMatrixInvertible(matrix: GPMATRIX;
    out result: Bool): GpStatus; stdcall; external WINGDIPDLL;

  function GdipIsMatrixIdentity(matrix: GPMATRIX;
    out result: Bool): GpStatus; stdcall; external WINGDIPDLL;

  function GdipIsMatrixEqual(matrix: GPMATRIX; matrix2: GPMATRIX;
    out result: Bool): GpStatus; stdcall; external WINGDIPDLL;

//----------------------------------------------------------------------------
// Region APIs
//----------------------------------------------------------------------------

  function GdipCreateRegion(out region: GpRegion): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateRegionRect(rect: GPRECTF;
    out region: GpRegion): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateRegionRectI(rect: GPRECT;
    out region: GpRegion): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateRegionPath(path: GPPATH;
    out region: GpRegion): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateRegionRgnData(regionData: PBYTE; size: Integer;
    out region: GpRegion): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateRegionHrgn(hRgn: HRGN;
    out region: GpRegion): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCloneRegion(region: GpRegion;
    out cloneRegion: GpRegion): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDeleteRegion(region: GpRegion): GpStatus; stdcall; external WINGDIPDLL;
  function GdipSetInfinite(region: GpRegion): GpStatus; stdcall; external WINGDIPDLL;
  function GdipSetEmpty(region: GpRegion): GpStatus; stdcall; external WINGDIPDLL;
  function GdipCombineRegionRect(region: GpRegion; rect: GPRECTF;
    combineMode: TIGPCombineMode): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCombineRegionRectI(region: GpRegion; rect: GPRECT;
    combineMode: TIGPCombineMode): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCombineRegionPath(region: GpRegion; path: GPPATH;
    combineMode: TIGPCombineMode): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCombineRegionRegion(region: GpRegion; region2: GpRegion;
    combineMode: TIGPCombineMode): GpStatus; stdcall; external WINGDIPDLL;

  function GdipTranslateRegion(region: GpRegion; dx: Single;
    dy: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipTranslateRegionI(region: GpRegion; dx: Integer;
    dy: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipTransformRegion(region: GpRegion;
    matrix: GPMATRIX): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetRegionBounds(region: GpRegion; graphics: GpGraphics;
    rect: GPRECTF): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetRegionBoundsI(region: GpRegion; graphics: GpGraphics;
    rect: GPRECT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetRegionHRgn(region: GpRegion; graphics: GpGraphics;
    out hRgn: HRGN): GpStatus; stdcall; external WINGDIPDLL;

  function GdipIsEmptyRegion(region: GpRegion; graphics: GpGraphics;
    out result: Bool): GpStatus; stdcall; external WINGDIPDLL;

  function GdipIsInfiniteRegion(region: GpRegion; graphics: GpGraphics;
    out result: Bool): GpStatus; stdcall; external WINGDIPDLL;

  function GdipIsEqualRegion(region: GpRegion; region2: GpRegion;
    graphics: GpGraphics; out result: Bool): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetRegionDataSize(region: GpRegion;
    out bufferSize: UINT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetRegionData(region: GpRegion; buffer: PBYTE;
    bufferSize: UINT; sizeFilled: PUINT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipIsVisibleRegionPoint(region: GpRegion; x: Single; y: Single;
    graphics: GpGraphics; out result: Bool): GpStatus; stdcall; external WINGDIPDLL;

  function GdipIsVisibleRegionPointI(region: GpRegion; x: Integer; y: Integer;
    graphics: GpGraphics; out result: Bool): GpStatus; stdcall; external WINGDIPDLL;

  function GdipIsVisibleRegionRect(region: GpRegion; x: Single; y: Single;
    width: Single; height: Single; graphics: GpGraphics;
    out result: Bool): GpStatus; stdcall; external WINGDIPDLL;

  function GdipIsVisibleRegionRectI(region: GpRegion; x: Integer; y: Integer;
    width: Integer; height: Integer; graphics: GpGraphics;
    out result: Bool): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetRegionScansCount(region: GpRegion; out count: UINT;
    matrix: GPMATRIX): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetRegionScans(region: GpRegion; rects: GPRECTF;
    out count: Integer; matrix: GPMATRIX): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetRegionScansI(region: GpRegion; rects: GPRECT;
    out count: Integer; matrix: GPMATRIX): GpStatus; stdcall; external WINGDIPDLL;

//----------------------------------------------------------------------------
// Brush APIs
//----------------------------------------------------------------------------

  function GdipCloneBrush(brush: GPBRUSH;
    out cloneBrush: GPBRUSH): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDeleteBrush(brush: GPBRUSH): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetBrushType(brush: GPBRUSH;
    out type_: TIGPBrushType): GpStatus; stdcall; external WINGDIPDLL;

//----------------------------------------------------------------------------
// HatchBrush APIs
//----------------------------------------------------------------------------

  function GdipCreateHatchBrush(hatchstyle: Integer; forecol: TAlphaColor;
    backcol: TAlphaColor; out brush: GPHATCH): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetHatchStyle(brush: GPHATCH;
    out hatchstyle: TIGPHatchStyle): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetHatchForegroundColor(brush: GPHATCH;
    out forecol: TAlphaColor): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetHatchBackgroundColor(brush: GPHATCH;
    out backcol: TAlphaColor): GpStatus; stdcall; external WINGDIPDLL;

//----------------------------------------------------------------------------
// TextureBrush APIs
//----------------------------------------------------------------------------


  function GdipCreateTexture(image: GpImage; wrapmode: TIGPWrapMode;
    var texture: GPTEXTURE): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateTexture2(image: GpImage; wrapmode: TIGPWrapMode;
    x: Single; y: Single; width: Single; height: Single;
    out texture: GPTEXTURE): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateTextureIA(image: GpImage;
    imageAttributes: GpImageAttributes; x: Single; y: Single; width: Single;
    height: Single; out texture: GPTEXTURE): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateTexture2I(image: GpImage; wrapmode: TIGPWrapMode; x: Integer;
    y: Integer; width: Integer; height: Integer;
    out texture: GPTEXTURE): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateTextureIAI(image: GpImage;
    imageAttributes: GpImageAttributes; x: Integer; y: Integer; width: Integer;
    height: Integer; out texture: GPTEXTURE): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetTextureTransform(brush: GPTEXTURE;
    matrix: GPMATRIX): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetTextureTransform(brush: GPTEXTURE;
    matrix: GPMATRIX): GpStatus; stdcall; external WINGDIPDLL;

  function GdipResetTextureTransform(brush: GPTEXTURE): GpStatus; stdcall; external WINGDIPDLL;

  function GdipMultiplyTextureTransform(brush: GPTEXTURE; matrix: GPMATRIX;
    order: TIGPMatrixOrder): GpStatus; stdcall; external WINGDIPDLL;

  function GdipTranslateTextureTransform(brush: GPTEXTURE; dx: Single;
    dy: Single; order: TIGPMatrixOrder): GpStatus; stdcall; external WINGDIPDLL;

  function GdipScaleTextureTransform(brush: GPTEXTURE; sx: Single; sy: Single;
    order: TIGPMatrixOrder): GpStatus; stdcall; external WINGDIPDLL;

  function GdipRotateTextureTransform(brush: GPTEXTURE; angle: Single;
    order: TIGPMatrixOrder): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetTextureWrapMode(brush: GPTEXTURE;
    wrapmode: TIGPWrapMode): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetTextureWrapMode(brush: GPTEXTURE;
    var wrapmode: TIGPWrapMode): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetTextureImage(brush: GPTEXTURE;
    out image: GpImage): GpStatus; stdcall; external WINGDIPDLL;

//----------------------------------------------------------------------------
// SolidBrush APIs
//----------------------------------------------------------------------------

  function GdipCreateSolidFill(color: TAlphaColor;
    out brush: GPSOLIDFILL): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetSolidFillColor(brush: GPSOLIDFILL;
    color: TAlphaColor): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetSolidFillColor(brush: GPSOLIDFILL;
    out color: TAlphaColor): GpStatus; stdcall; external WINGDIPDLL;

//----------------------------------------------------------------------------
// LineBrush APIs
//----------------------------------------------------------------------------

  function GdipCreateLineBrush(point1: GPPOINTF; point2: GPPOINTF; color1: TAlphaColor;
    color2: TAlphaColor; wrapMode: TIGPWrapMode;
    out lineGradient: GpLineGradient): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateLineBrushI(point1: GPPOINT; point2: GPPOINT; color1: TAlphaColor;
    color2: TAlphaColor; wrapMode: TIGPWrapMode;
    out lineGradient: GpLineGradient): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateLineBrushFromRect(rect: GPRECTF; color1: TAlphaColor;
    color2: TAlphaColor; mode: TIGPLinearGradientMode; wrapMode: TIGPWrapMode;
    out lineGradient: GpLineGradient): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateLineBrushFromRectI(rect: GPRECT; color1: TAlphaColor;
    color2: TAlphaColor; mode: TIGPLinearGradientMode; wrapMode: TIGPWrapMode;
    out lineGradient: GpLineGradient): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateLineBrushFromRectWithAngle(rect: GPRECTF; color1: TAlphaColor;
    color2: TAlphaColor; angle: Single; isAngleScalable: Bool; wrapMode: TIGPWrapMode;
    out lineGradient: GpLineGradient): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateLineBrushFromRectWithAngleI(rect: GPRECT; color1: TAlphaColor;
    color2: TAlphaColor; angle: Single; isAngleScalable: Bool; wrapMode: TIGPWrapMode;
    out lineGradient: GpLineGradient): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetLineColors(brush: GpLineGradient; color1: TAlphaColor;
    color2: TAlphaColor): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetLineColors(brush: GpLineGradient;
    colors: PAlphaColor): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetLineRect(brush: GpLineGradient;
    rect: GPRECTF): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetLineRectI(brush: GpLineGradient;
    rect: GPRECT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetLineGammaCorrection(brush: GpLineGradient;
    useGammaCorrection: Bool): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetLineGammaCorrection(brush: GpLineGradient;
    out useGammaCorrection: Bool): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetLineBlendCount(brush: GpLineGradient;
    out count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetLineBlend(brush: GpLineGradient; blend: PSingle;
    positions: PSingle; count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetLineBlend(brush: GpLineGradient; blend: PSingle;
    positions: PSingle; count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetLinePresetBlendCount(brush: GpLineGradient;
    out count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetLinePresetBlend(brush: GpLineGradient; blend: PAlphaColor;
    positions: PSingle; count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetLinePresetBlend(brush: GpLineGradient; blend: PAlphaColor;
    positions: PSingle; count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetLineSigmaBlend(brush: GpLineGradient; focus: Single;
    scale: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetLineLinearBlend(brush: GpLineGradient; focus: Single;
    scale: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetLineWrapMode(brush: GpLineGradient;
    wrapmode: TIGPWrapMode): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetLineWrapMode(brush: GpLineGradient;
    out wrapmode: TIGPWrapMode): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetLineTransform(brush: GpLineGradient;
    matrix: GPMATRIX): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetLineTransform(brush: GpLineGradient;
    matrix: GPMATRIX): GpStatus; stdcall; external WINGDIPDLL;

  function GdipResetLineTransform(brush: GpLineGradient): GpStatus; stdcall; external WINGDIPDLL;

  function GdipMultiplyLineTransform(brush: GpLineGradient; matrix: GPMATRIX;
    order: TIGPMatrixOrder): GpStatus; stdcall; external WINGDIPDLL;

  function GdipTranslateLineTransform(brush: GpLineGradient; dx: Single;
    dy: Single; order: TIGPMatrixOrder): GpStatus; stdcall; external WINGDIPDLL;

  function GdipScaleLineTransform(brush: GpLineGradient; sx: Single; sy: Single;
    order: TIGPMatrixOrder): GpStatus; stdcall; external WINGDIPDLL;

  function GdipRotateLineTransform(brush: GpLineGradient; angle: Single;
    order: TIGPMatrixOrder): GpStatus; stdcall; external WINGDIPDLL;

//----------------------------------------------------------------------------
// PathGradientBrush APIs
//----------------------------------------------------------------------------

  function GdipCreatePathGradient(points: GPPOINTF; count: Integer;
    wrapMode: TIGPWrapMode; out polyGradient: GPPATHGRADIENT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreatePathGradientI(points: GPPOINT; count: Integer;
    wrapMode: TIGPWrapMode; out polyGradient: GPPATHGRADIENT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreatePathGradientFromPath(path: GPPATH;
    out polyGradient: GPPATHGRADIENT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPathGradientCenterColor(brush: GPPATHGRADIENT;
    out colors: TAlphaColor): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetPathGradientCenterColor(brush: GPPATHGRADIENT;
    colors: TAlphaColor): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPathGradientSurroundColorsWithCount(brush: GPPATHGRADIENT;
    color: PAlphaColor; var count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetPathGradientSurroundColorsWithCount(brush: GPPATHGRADIENT;
    color: PAlphaColor; var count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPathGradientPath(brush: GPPATHGRADIENT;
    path: GPPATH): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetPathGradientPath(brush: GPPATHGRADIENT;
    path: GPPATH): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPathGradientCenterPoint(brush: GPPATHGRADIENT;
    points: GPPOINTF): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPathGradientCenterPointI(brush: GPPATHGRADIENT;
    points: GPPOINT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetPathGradientCenterPoint(brush: GPPATHGRADIENT;
    points: GPPOINTF): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetPathGradientCenterPointI(brush: GPPATHGRADIENT;
    points: GPPOINT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPathGradientRect(brush: GPPATHGRADIENT;
    rect: GPRECTF): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPathGradientRectI(brush: GPPATHGRADIENT;
    rect: GPRECT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPathGradientPointCount(brush: GPPATHGRADIENT;
    var count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPathGradientSurroundColorCount(brush: GPPATHGRADIENT;
    var count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetPathGradientGammaCorrection(brush: GPPATHGRADIENT;
    useGammaCorrection: Bool): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPathGradientGammaCorrection(brush: GPPATHGRADIENT;
    var useGammaCorrection: Bool): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPathGradientBlendCount(brush: GPPATHGRADIENT;
    var count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPathGradientBlend(brush: GPPATHGRADIENT;
    blend: PSingle; positions: PSingle; count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetPathGradientBlend(brush: GPPATHGRADIENT;
    blend: PSingle; positions: PSingle; count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPathGradientPresetBlendCount(brush: GPPATHGRADIENT;
    var count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPathGradientPresetBlend(brush: GPPATHGRADIENT;
    blend: PAlphaColor; positions: PSingle; count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetPathGradientPresetBlend(brush: GPPATHGRADIENT;
    blend: PAlphaColor; positions: PSingle; count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetPathGradientSigmaBlend(brush: GPPATHGRADIENT;
    focus: Single; scale: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetPathGradientLinearBlend(brush: GPPATHGRADIENT;
    focus: Single; scale: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPathGradientWrapMode(brush: GPPATHGRADIENT;
    var wrapmode: TIGPWrapMode): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetPathGradientWrapMode(brush: GPPATHGRADIENT;
    wrapmode: TIGPWrapMode): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPathGradientTransform(brush: GPPATHGRADIENT;
    matrix: GPMATRIX): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetPathGradientTransform(brush: GPPATHGRADIENT;
    matrix: GPMATRIX): GpStatus; stdcall; external WINGDIPDLL;

  function GdipResetPathGradientTransform(
    brush: GPPATHGRADIENT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipMultiplyPathGradientTransform(brush: GPPATHGRADIENT;
    matrix: GPMATRIX; order: TIGPMatrixOrder): GpStatus; stdcall; external WINGDIPDLL;

  function GdipTranslatePathGradientTransform(brush: GPPATHGRADIENT;
    dx: Single; dy: Single; order: TIGPMatrixOrder): GpStatus; stdcall; external WINGDIPDLL;

  function GdipScalePathGradientTransform(brush: GPPATHGRADIENT;
    sx: Single; sy: Single; order: TIGPMatrixOrder): GpStatus; stdcall; external WINGDIPDLL;

  function GdipRotatePathGradientTransform(brush: GPPATHGRADIENT;
    angle: Single; order: TIGPMatrixOrder): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPathGradientFocusScales(brush: GPPATHGRADIENT;
    var xScale: Single; var yScale: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetPathGradientFocusScales(brush: GPPATHGRADIENT;
    xScale: Single; yScale: Single): GpStatus; stdcall; external WINGDIPDLL;

//----------------------------------------------------------------------------
// Pen APIs
//----------------------------------------------------------------------------

  function GdipCreatePen1(color: TAlphaColor; width: Single; unit_: GPUNIT;
    out pen: GPPEN): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreatePen2(brush: GPBRUSH; width: Single; unit_: GPUNIT;
    out pen: GPPEN): GpStatus; stdcall; external WINGDIPDLL;

  function GdipClonePen(pen: GPPEN; out clonepen: GPPEN): GpStatus; stdcall; external WINGDIPDLL;
  function GdipDeletePen(pen: GPPEN): GpStatus; stdcall; external WINGDIPDLL;
  function GdipSetPenWidth(pen: GPPEN; width: Single): GpStatus; stdcall; external WINGDIPDLL;
  function GdipGetPenWidth(pen: GPPEN; out width: Single): GpStatus; stdcall; external WINGDIPDLL;
  function GdipSetPenUnit(pen: GPPEN; unit_: GPUNIT): GpStatus; stdcall; external WINGDIPDLL;
  function GdipGetPenUnit(pen: GPPEN; var unit_: GPUNIT): GpStatus; stdcall; external WINGDIPDLL;
  function GdipSetPenLineCap197819(pen: GPPEN; startCap: TIGPLineCap;
    endCap: TIGPLineCap; dashCap: TIGPDashCap): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetPenStartCap(pen: GPPEN;
    startCap: TIGPLineCap): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetPenEndCap(pen: GPPEN; endCap: TIGPLineCap): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetPenDashCap197819(pen: GPPEN;
    dashCap: TIGPDashCap): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPenStartCap(pen: GPPEN;
    out startCap: TIGPLineCap): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPenEndCap(pen: GPPEN;
    out endCap: TIGPLineCap): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPenDashCap197819(pen: GPPEN;
    out dashCap: TIGPDashCap): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetPenLineJoin(pen: GPPEN;
    lineJoin: TIGPLineJoin): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPenLineJoin(pen: GPPEN;
    var lineJoin: TIGPLineJoin): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetPenCustomStartCap(pen: GPPEN;
    customCap: GPCUSTOMLINECAP): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPenCustomStartCap(pen: GPPEN;
    out customCap: GPCUSTOMLINECAP): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetPenCustomEndCap(pen: GPPEN;
    customCap: GPCUSTOMLINECAP): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPenCustomEndCap(pen: GPPEN;
    out customCap: GPCUSTOMLINECAP): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetPenMiterLimit(pen: GPPEN;
    miterLimit: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPenMiterLimit(pen: GPPEN;
    out miterLimit: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetPenMode(pen: GPPEN;
    penMode: TIGPPenAlignment): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPenMode(pen: GPPEN;
    var penMode: TIGPPenAlignment): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetPenTransform(pen: GPPEN;
    matrix: GPMATRIX): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPenTransform(pen: GPPEN;
    matrix: GPMATRIX): GpStatus; stdcall; external WINGDIPDLL;

  function GdipResetPenTransform(pen: GPPEN): GpStatus; stdcall; external WINGDIPDLL;

  function GdipMultiplyPenTransform(pen: GPPEN; matrix: GPMATRIX;
    order: TIGPMatrixOrder): GpStatus; stdcall; external WINGDIPDLL;

  function GdipTranslatePenTransform(pen: GPPEN; dx: Single; dy: Single;
    order: TIGPMatrixOrder): GpStatus; stdcall; external WINGDIPDLL;

  function GdipScalePenTransform(pen: GPPEN; sx: Single; sy: Single;
    order: TIGPMatrixOrder): GpStatus; stdcall; external WINGDIPDLL;

  function GdipRotatePenTransform(pen: GPPEN; angle: Single;
    order: TIGPMatrixOrder): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetPenColor(pen: GPPEN; argb: TAlphaColor): GpStatus; stdcall; external WINGDIPDLL;
  function GdipGetPenColor(pen: GPPEN; out argb: TAlphaColor): GpStatus; stdcall; external WINGDIPDLL;
  function GdipSetPenBrushFill(pen: GPPEN; brush: GPBRUSH): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPenBrushFill(pen: GPPEN;
    out brush: GPBRUSH): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPenFillType(pen: GPPEN;
    out type_: TIGPPenType): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPenDashStyle(pen: GPPEN;
    out dashstyle: TIGPDashStyle): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetPenDashStyle(pen: GPPEN;
    dashstyle: TIGPDashStyle): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPenDashOffset(pen: GPPEN;
    out offset: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetPenDashOffset(pen: GPPEN; offset: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPenDashCount(pen: GPPEN;
    var count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetPenDashArray(pen: GPPEN; dash: PSingle;
    count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPenDashArray(pen: GPPEN; dash: PSingle;
    count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPenCompoundCount(pen: GPPEN;
    out count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetPenCompoundArray(pen: GPPEN; dash: PSingle;
    count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPenCompoundArray(pen: GPPEN; dash: PSingle;
    count: Integer): GpStatus; stdcall; external WINGDIPDLL;

//----------------------------------------------------------------------------
// CustomLineCap APIs
//----------------------------------------------------------------------------

  function GdipCreateCustomLineCap(fillPath: GPPATH; strokePath: GPPATH;
    baseCap: TIGPLineCap; baseInset: Single;
    out customCap: GPCUSTOMLINECAP): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDeleteCustomLineCap(
    customCap: GPCUSTOMLINECAP): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCloneCustomLineCap(customCap: GPCUSTOMLINECAP;
    out clonedCap: GPCUSTOMLINECAP): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetCustomLineCapType(customCap: GPCUSTOMLINECAP;
    var capType: TIGPCustomLineCapType): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetCustomLineCapStrokeCaps(customCap: GPCUSTOMLINECAP;
    startCap: TIGPLineCap; endCap: TIGPLineCap): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetCustomLineCapStrokeCaps(customCap: GPCUSTOMLINECAP;
    var startCap: TIGPLineCap; var endCap: TIGPLineCap): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetCustomLineCapStrokeJoin(customCap: GPCUSTOMLINECAP;
  lineJoin: TIGPLineJoin): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetCustomLineCapStrokeJoin(customCap: GPCUSTOMLINECAP;
  var lineJoin: TIGPLineJoin): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetCustomLineCapBaseCap(customCap: GPCUSTOMLINECAP;
  baseCap: TIGPLineCap): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetCustomLineCapBaseCap(customCap: GPCUSTOMLINECAP;
  var baseCap: TIGPLineCap): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetCustomLineCapBaseInset(customCap: GPCUSTOMLINECAP;
  inset: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetCustomLineCapBaseInset(customCap: GPCUSTOMLINECAP;
  var inset: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetCustomLineCapWidthScale(customCap: GPCUSTOMLINECAP;
  widthScale: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetCustomLineCapWidthScale(customCap: GPCUSTOMLINECAP;
  var widthScale: Single): GpStatus; stdcall; external WINGDIPDLL;

//----------------------------------------------------------------------------
// AdjustableArrowCap APIs
//----------------------------------------------------------------------------

  function GdipCreateAdjustableArrowCap(height: Single;
  width: Single;
  isFilled: Bool;
  out cap: GPADJUSTABLEARROWCAP): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetAdjustableArrowCapHeight(cap: GPADJUSTABLEARROWCAP;
  height: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetAdjustableArrowCapHeight(cap: GPADJUSTABLEARROWCAP;
  var height: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetAdjustableArrowCapWidth(cap: GPADJUSTABLEARROWCAP;
  width: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetAdjustableArrowCapWidth(cap: GPADJUSTABLEARROWCAP;
  var width: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetAdjustableArrowCapMiddleInset(cap: GPADJUSTABLEARROWCAP;
  middleInset: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetAdjustableArrowCapMiddleInset(cap: GPADJUSTABLEARROWCAP;
  var middleInset: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetAdjustableArrowCapFillState(cap: GPADJUSTABLEARROWCAP;
  fillState: Bool): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetAdjustableArrowCapFillState(cap: GPADJUSTABLEARROWCAP;
  var fillState: Bool): GpStatus; stdcall; external WINGDIPDLL;

//----------------------------------------------------------------------------
// Image APIs
//----------------------------------------------------------------------------

  function GdipLoadImageFromStream(stream: ISTREAM;
  out image: GpImage): GpStatus; stdcall; external WINGDIPDLL;

  function GdipLoadImageFromFile(filename: PWCHAR;
  out image: GpImage): GpStatus; stdcall; external WINGDIPDLL;

  function GdipLoadImageFromStreamICM(stream: ISTREAM;
  out image: GpImage): GpStatus; stdcall; external WINGDIPDLL;

  function GdipLoadImageFromFileICM(filename: PWCHAR;
  out image: GpImage): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCloneImage(image: GpImage;
  out cloneImage: GpImage): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDisposeImage(image: GpImage): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSaveImageToFile(image: GpImage;
  filename: PWCHAR;
  clsidEncoder: PGUID;
  encoderParams: PGPEncoderParameters): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSaveImageToStream(image: GpImage;
  stream: ISTREAM;
  clsidEncoder: PGUID;
  encoderParams: PGPEncoderParameters): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSaveAdd(image: GpImage;
  encoderParams: PGPEncoderParameters): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSaveAddImage(image: GpImage;
  newImage: GpImage;
  encoderParams: PGPEncoderParameters): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetImageGraphicsContext(image: GpImage;
  out graphics: GpGraphics): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetImageBounds(image: GpImage;
  srcRect: GPRECTF;
  var srcUnit: GPUNIT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetImageDimension(image: GpImage;
  var width: Single;
  var height: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetImageType(image: GpImage;
  var type_: TIGPImageType): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetImageWidth(image: GpImage;
  var width: UINT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetImageHeight(image: GpImage;
  var height: UINT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetImageHorizontalResolution(image: GpImage;
  var resolution: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetImageVerticalResolution(image: GpImage;
  var resolution: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetImageFlags(image: GpImage;
  var flags: UINT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetImageRawFormat(image: GpImage;
  format: PGUID): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetImagePixelFormat(image: GpImage;
  out format: TIGPPixelFormat): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetImageThumbnail(image: GpImage; thumbWidth: UINT;
    thumbHeight: UINT; out thumbImage: GpImage;
    callback: TIGPGetThumbnailImageAbort; callbackData: Pointer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetEncoderParameterListSize(image: GpImage;
    clsidEncoder: PGUID; out size: UINT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetEncoderParameterList(image: GpImage; clsidEncoder: PGUID;
    size: UINT; buffer: PGPEncoderParameters): GpStatus; stdcall; external WINGDIPDLL;

  function GdipImageGetFrameDimensionsCount(image: GpImage;
    var count: UINT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipImageGetFrameDimensionsList(image: GpImage; dimensionIDs: PGUID;
    count: UINT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipImageGetFrameCount(image: GpImage; dimensionID: PGUID;
    var count: UINT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipImageSelectActiveFrame(image: GpImage; dimensionID: PGUID;
    frameIndex: UINT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipImageRotateFlip(image: GpImage;
    rfType: TIGPRotateFlipType): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetImagePalette(image: GpImage; palette: PGPColorPalette;
    size: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetImagePalette(image: GpImage;
    palette: PGPColorPalette): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetImagePaletteSize(image: GpImage;
    var size: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPropertyCount(image: GpImage;
    var numOfProperty: UINT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPropertyIdList(image: GpImage; numOfProperty: UINT;
    list: PPROPID): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPropertyItemSize(image: GpImage; propId: PROPID;
    var size: UINT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPropertyItem(image: GpImage; propId: PROPID; propSize: UINT;
    buffer: PGPPropertyItem): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPropertySize(image: GpImage; var totalBufferSize: UINT;
    var numProperties: UINT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetAllPropertyItems(image: GpImage; totalBufferSize: UINT;
    numProperties: UINT; allItems: PGPPropertyItem): GpStatus; stdcall; external WINGDIPDLL;

  function GdipRemovePropertyItem(image: GpImage;
    propId: PROPID): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetPropertyItem(image: GpImage;
    item: PGPPropertyItem): GpStatus; stdcall; external WINGDIPDLL;

  function GdipImageForceValidation(image: GpImage): GpStatus; stdcall; external WINGDIPDLL;

//----------------------------------------------------------------------------
// Bitmap APIs
//----------------------------------------------------------------------------

  function GdipCreateBitmapFromStream(stream: ISTREAM;
    out bitmap: GPBITMAP): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateBitmapFromFile(filename: PWCHAR;
    out bitmap: GPBITMAP): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateBitmapFromStreamICM(stream: ISTREAM;
    out bitmap: GPBITMAP): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateBitmapFromFileICM(filename: PWCHAR;
    var bitmap: GPBITMAP): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateBitmapFromScan0(width: Integer; height: Integer;
    stride: Integer; format: TIGPPixelFormat; scan0: PBYTE;
    out bitmap: GPBITMAP): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateBitmapFromGraphics(width: Integer; height: Integer;
    target: GpGraphics; out bitmap: GPBITMAP): GpStatus; stdcall; external WINGDIPDLL;

(*
  function GdipCreateBitmapFromDirectDrawSurface(surface: IDIRECTDRAWSURFACE7;
    out bitmap: GPBITMAP): GpStatus; stdcall; external WINGDIPDLL;
*)

  function GdipCreateBitmapFromGdiDib(gdiBitmapInfo: PBitmapInfo;
    gdiBitmapData: Pointer; out bitmap: GPBITMAP): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateBitmapFromHBITMAP(hbm: HBITMAP; hpal: HPALETTE;
    out bitmap: GPBITMAP): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateHBITMAPFromBitmap(bitmap: GPBITMAP; out hbmReturn: HBITMAP;
    background: TAlphaColor): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateBitmapFromHICON(hicon: HICON;
    out bitmap: GPBITMAP): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateHICONFromBitmap(bitmap: GPBITMAP;
    out hbmReturn: HICON): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateBitmapFromResource(hInstance: HMODULE;
    lpBitmapName: PWCHAR; out bitmap: GPBITMAP): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCloneBitmapArea(x: Single; y: Single; width: Single;
    height: Single; format: TIGPPixelFormat; srcBitmap: GPBITMAP;
    out dstBitmap: GPBITMAP): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCloneBitmapAreaI(x: Integer; y: Integer; width: Integer;
    height: Integer; format: TIGPPixelFormat; srcBitmap: GPBITMAP;
    out dstBitmap: GPBITMAP): GpStatus; stdcall; external WINGDIPDLL;

  function GdipBitmapLockBits(bitmap: GPBITMAP; rect: GPRECT; flags: UINT;
    format: TIGPPixelFormat; lockedBitmapData: PGPBitmapDataRecord): GpStatus; stdcall; external WINGDIPDLL;

  function GdipBitmapUnlockBits(bitmap: GPBITMAP;
    lockedBitmapData: PGPBitmapDataRecord): GpStatus; stdcall; external WINGDIPDLL;

  function GdipBitmapGetPixel(bitmap: GPBITMAP; x: Integer; y: Integer;
    var color: TAlphaColor): GpStatus; stdcall; external WINGDIPDLL;

  function GdipBitmapSetPixel(bitmap: GPBITMAP; x: Integer; y: Integer;
    color: TAlphaColor): GpStatus; stdcall; external WINGDIPDLL;

  function GdipBitmapSetResolution(bitmap: GPBITMAP; xdpi: Single;
    ydpi: Single): GpStatus; stdcall; external WINGDIPDLL;

//----------------------------------------------------------------------------
// ImageAttributes APIs
//----------------------------------------------------------------------------

  function GdipCreateImageAttributes(
    out imageattr: GpImageAttributes): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCloneImageAttributes(imageattr: GpImageAttributes;
    out cloneImageattr: GpImageAttributes): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDisposeImageAttributes(
    imageattr: GpImageAttributes): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetImageAttributesToIdentity(imageattr: GpImageAttributes;
    type_: TIGPColorAdjustType): GpStatus; stdcall; external WINGDIPDLL;

  function GdipResetImageAttributes(imageattr: GpImageAttributes;
    type_: TIGPColorAdjustType): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetImageAttributesColorMatrix(imageattr: GpImageAttributes;
    type_: TIGPColorAdjustType; enableFlag: Bool; colorMatrix: PGPColorMatrix;
    grayMatrix: PGPColorMatrix; flags: TIGPColorMatrixFlags): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetImageAttributesThreshold(imageattr: GpImageAttributes;
    type_: TIGPColorAdjustType; enableFlag: Bool;
    threshold: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetImageAttributesGamma(imageattr: GpImageAttributes;
    type_: TIGPColorAdjustType; enableFlag: Bool; gamma: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetImageAttributesNoOp(imageattr: GpImageAttributes;
  type_: TIGPColorAdjustType; enableFlag: Bool): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetImageAttributesColorKeys(imageattr: GpImageAttributes;
    type_: TIGPColorAdjustType; enableFlag: Bool; colorLow: TAlphaColor;
    colorHigh: TAlphaColor): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetImageAttributesOutputChannel(imageattr: GpImageAttributes;
    type_: TIGPColorAdjustType; enableFlag: Bool;
    channelFlags: TIGPColorChannelFlags): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetImageAttributesOutputChannelColorProfile(imageattr: GpImageAttributes;
    type_: TIGPColorAdjustType; enableFlag: Bool;
    colorProfileFilename: PWCHAR): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetImageAttributesRemapTable(imageattr: GpImageAttributes;
    type_: TIGPColorAdjustType; enableFlag: Bool; mapSize: UINT;
    map: PGPColorMap): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetImageAttributesWrapMode(imageAttr: GpImageAttributes;
    wrap: TIGPWrapMode; argb: TAlphaColor; clamp: Bool): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetImageAttributesICMMode(imageAttr: GpImageAttributes;
    on_: Bool): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetImageAttributesAdjustedPalette(imageAttr: GpImageAttributes;
    colorPalette: PGPColorPalette;
    colorAdjustType: TIGPColorAdjustType): GpStatus; stdcall; external WINGDIPDLL;

//----------------------------------------------------------------------------
// Graphics APIs
//----------------------------------------------------------------------------

  function GdipFlush(graphics: GpGraphics;
    intention: TIGPFlushIntention): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateFromHDC(hdc: HDC;
    out graphics: GpGraphics): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateFromHDC2(hdc: HDC; hDevice: THandle;
    out graphics: GpGraphics): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateFromHWND(hwnd: HWND;
    out graphics: GpGraphics): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateFromHWNDICM(hwnd: HWND;
    out graphics: GpGraphics): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDeleteGraphics(graphics: GpGraphics): GpStatus; stdcall; external WINGDIPDLL;
  function GdipGetDC(graphics: GpGraphics; var hdc: HDC): GpStatus; stdcall; external WINGDIPDLL;
  function GdipReleaseDC(graphics: GpGraphics; hdc: HDC): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetCompositingMode(graphics: GpGraphics;
    compositingMode: TIGPCompositingMode): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetCompositingMode(graphics: GpGraphics;
    var compositingMode: TIGPCompositingMode): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetRenderingOrigin(graphics: GpGraphics; x: Integer;
    y: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetRenderingOrigin(graphics: GpGraphics; var x: Integer;
    var y: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetCompositingQuality(graphics: GpGraphics;
    compositingQuality: TIGPCompositingQuality): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetCompositingQuality(graphics: GpGraphics;
    var compositingQuality: TIGPCompositingQuality): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetSmoothingMode(graphics: GpGraphics;
    smoothingMode: TIGPSmoothingMode): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetSmoothingMode(graphics: GpGraphics;
    var smoothingMode: TIGPSmoothingMode): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetPixelOffsetMode(graphics: GpGraphics;
    pixelOffsetMode: TIGPPixelOffsetMode): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPixelOffsetMode(graphics: GpGraphics;
    var pixelOffsetMode: TIGPPixelOffsetMode): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetTextRenderingHint(graphics: GpGraphics;
    mode: TIGPTextRenderingHint): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetTextRenderingHint(graphics: GpGraphics;
    var mode: TIGPTextRenderingHint): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetTextContrast(graphics: GpGraphics;
    contrast: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetTextContrast(graphics: GpGraphics;
    var contrast: UINT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetInterpolationMode(graphics: GpGraphics;
    interpolationMode: TIGPInterpolationMode): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetInterpolationMode(graphics: GpGraphics;
    var interpolationMode: TIGPInterpolationMode): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetWorldTransform(graphics: GpGraphics;
    matrix: GPMATRIX): GpStatus; stdcall; external WINGDIPDLL;

  function GdipResetWorldTransform(graphics: GpGraphics): GpStatus; stdcall; external WINGDIPDLL;

  function GdipMultiplyWorldTransform(graphics: GpGraphics; matrix: GPMATRIX;
    order: TIGPMatrixOrder): GpStatus; stdcall; external WINGDIPDLL;

  function GdipTranslateWorldTransform(graphics: GpGraphics; dx: Single;
    dy: Single; order: TIGPMatrixOrder): GpStatus; stdcall; external WINGDIPDLL;

  function GdipScaleWorldTransform(graphics: GpGraphics; sx: Single; sy: Single;
    order: TIGPMatrixOrder): GpStatus; stdcall; external WINGDIPDLL;

  function GdipRotateWorldTransform(graphics: GpGraphics; angle: Single;
    order: TIGPMatrixOrder): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetWorldTransform(graphics: GpGraphics;
    matrix: GPMATRIX): GpStatus; stdcall; external WINGDIPDLL;

  function GdipResetPageTransform(graphics: GpGraphics): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPageUnit(graphics: GpGraphics;
    var unit_: GPUNIT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetPageScale(graphics: GpGraphics;
    var scale: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetPageUnit(graphics: GpGraphics;
    unit_: GPUNIT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetPageScale(graphics: GpGraphics;
    scale: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetDpiX(graphics: GpGraphics;
    var dpi: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetDpiY(graphics: GpGraphics;
    var dpi: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipTransformPoints(graphics: GpGraphics;
    destSpace: GPCOORDINATESPACE; srcSpace: GPCOORDINATESPACE;
    points: GPPOINTF; count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipTransformPointsI(graphics: GpGraphics;
    destSpace: GPCOORDINATESPACE; srcSpace: GPCOORDINATESPACE;
    points: GPPOINT; count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetNearestColor(graphics: GpGraphics;
    argb: PAlphaColor): GpStatus; stdcall; external WINGDIPDLL;

// Creates the Win9x Halftone Palette (even on NT) with correct Desktop colors

  function GdipCreateHalftonePalette: HPALETTE; stdcall; external WINGDIPDLL;

  function GdipDrawLine(graphics: GpGraphics; pen: GPPEN; x1: Single;
    y1: Single; x2: Single; y2: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawLineI(graphics: GpGraphics; pen: GPPEN; x1: Integer;
    y1: Integer; x2: Integer; y2: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawLines(graphics: GpGraphics; pen: GPPEN; points: GPPOINTF;
    count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawLinesI(graphics: GpGraphics; pen: GPPEN; points: GPPOINT;
    count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawArc(graphics: GpGraphics; pen: GPPEN; x: Single; y: Single;
    width: Single; height: Single; startAngle: Single;
    sweepAngle: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawArcI(graphics: GpGraphics; pen: GPPEN; x: Integer;
    y: Integer; width: Integer; height: Integer; startAngle: Single;
    sweepAngle: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawBezier(graphics: GpGraphics; pen: GPPEN; x1: Single;
    y1: Single; x2: Single; y2: Single; x3: Single; y3: Single; x4: Single;
    y4: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawBezierI(graphics: GpGraphics; pen: GPPEN; x1: Integer;
    y1: Integer; x2: Integer; y2: Integer; x3: Integer; y3: Integer;
    x4: Integer; y4: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawBeziers(graphics: GpGraphics; pen: GPPEN; points: GPPOINTF;
    count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawBeziersI(graphics: GpGraphics; pen: GPPEN; points: GPPOINT;
    count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawRectangle(graphics: GpGraphics; pen: GPPEN; x: Single;
    y: Single; width: Single; height: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawRectangleI(graphics: GpGraphics; pen: GPPEN; x: Integer;
    y: Integer; width: Integer; height: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawRectangles(graphics: GpGraphics; pen: GPPEN; rects: GPRECTF;
    count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawRectanglesI(graphics: GpGraphics; pen: GPPEN; rects: GPRECT;
    count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawEllipse(graphics: GpGraphics; pen: GPPEN; x: Single;
    y: Single; width: Single; height: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawEllipseI(graphics: GpGraphics; pen: GPPEN; x: Integer;
    y: Integer; width: Integer; height: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawPie(graphics: GpGraphics; pen: GPPEN; x: Single; y: Single;
    width: Single;  height: Single; startAngle: Single;
    sweepAngle: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawPieI(graphics: GpGraphics; pen: GPPEN; x: Integer;
    y: Integer; width: Integer; height: Integer; startAngle: Single;
    sweepAngle: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawPolygon(graphics: GpGraphics; pen: GPPEN; points: GPPOINTF;
    count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawPolygonI(graphics: GpGraphics; pen: GPPEN; points: GPPOINT;
    count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawPath(graphics: GpGraphics; pen: GPPEN;
    path: GPPATH): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawCurve(graphics: GpGraphics; pen: GPPEN; points: GPPOINTF;
    count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawCurveI(graphics: GpGraphics; pen: GPPEN; points: GPPOINT;
    count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawCurve2(graphics: GpGraphics; pen: GPPEN; points: GPPOINTF;
    count: Integer; tension: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawCurve2I(graphics: GpGraphics; pen: GPPEN; points: GPPOINT;
    count: Integer; tension: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawCurve3(graphics: GpGraphics; pen: GPPEN; points: GPPOINTF;
    count: Integer; offset: Integer; numberOfSegments: Integer;
    tension: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawCurve3I(graphics: GpGraphics; pen: GPPEN; points: GPPOINT;
    count: Integer; offset: Integer; numberOfSegments: Integer;
    tension: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawClosedCurve(graphics: GpGraphics; pen: GPPEN;
    points: GPPOINTF; count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawClosedCurveI(graphics: GpGraphics; pen: GPPEN;
    points: GPPOINT; count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawClosedCurve2(graphics: GpGraphics; pen: GPPEN;
    points: GPPOINTF; count: Integer; tension: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawClosedCurve2I(graphics: GpGraphics; pen: GPPEN;
    points: GPPOINT; count: Integer; tension: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGraphicsClear(graphics: GpGraphics;
    color: TAlphaColor): GpStatus; stdcall; external WINGDIPDLL;

  function GdipFillRectangle(graphics: GpGraphics; brush: GPBRUSH; x: Single;
    y: Single; width: Single; height: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipFillRectangleI(graphics: GpGraphics; brush: GPBRUSH; x: Integer;
    y: Integer; width: Integer; height: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipFillRectangles(graphics: GpGraphics; brush: GPBRUSH;
    rects: GPRECTF; count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipFillRectanglesI(graphics: GpGraphics; brush: GPBRUSH;
    rects: GPRECT; count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipFillPolygon(graphics: GpGraphics; brush: GPBRUSH;
    points: GPPOINTF; count: Integer; fillMode: GPFILLMODE): GpStatus; stdcall; external WINGDIPDLL;

  function GdipFillPolygonI(graphics: GpGraphics; brush: GPBRUSH;
    points: GPPOINT; count: Integer; fillMode: GPFILLMODE): GpStatus; stdcall; external WINGDIPDLL;

  function GdipFillPolygon2(graphics: GpGraphics; brush: GPBRUSH;
    points: GPPOINTF; count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipFillPolygon2I(graphics: GpGraphics; brush: GPBRUSH;
    points: GPPOINT; count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipFillEllipse(graphics: GpGraphics; brush: GPBRUSH; x: Single;
    y: Single; width: Single; height: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipFillEllipseI(graphics: GpGraphics; brush: GPBRUSH; x: Integer;
    y: Integer; width: Integer; height: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipFillPie(graphics: GpGraphics; brush: GPBRUSH; x: Single;
    y: Single; width: Single; height: Single; startAngle: Single;
    sweepAngle: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipFillPieI(graphics: GpGraphics; brush: GPBRUSH; x: Integer;
    y: Integer; width: Integer; height: Integer; startAngle: Single;
    sweepAngle: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipFillPath(graphics: GpGraphics; brush: GPBRUSH;
    path: GPPATH): GpStatus; stdcall; external WINGDIPDLL;

  function GdipFillClosedCurve(graphics: GpGraphics; brush: GPBRUSH;
    points: GPPOINTF; count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipFillClosedCurveI(graphics: GpGraphics; brush: GPBRUSH;
    points: GPPOINT; count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipFillClosedCurve2(graphics: GpGraphics; brush: GPBRUSH;
    points: GPPOINTF; count: Integer; tension: Single;
    fillMode: GPFILLMODE): GpStatus; stdcall; external WINGDIPDLL;

  function GdipFillClosedCurve2I(graphics: GpGraphics; brush: GPBRUSH;
    points: GPPOINT; count: Integer; tension: Single;
    fillMode: GPFILLMODE): GpStatus; stdcall; external WINGDIPDLL;

  function GdipFillRegion(graphics: GpGraphics; brush: GPBRUSH;
    region: GpRegion): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawImage(graphics: GpGraphics; image: GpImage; x: Single;
    y: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawImageI(graphics: GpGraphics; image: GpImage; x: Integer;
    y: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawImageRect(graphics: GpGraphics; image: GpImage; x: Single;
    y: Single; width: Single; height: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawImageRectI(graphics: GpGraphics; image: GpImage; x: Integer;
    y: Integer; width: Integer; height: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawImagePoints(graphics: GpGraphics; image: GpImage;
    dstpoints: GPPOINTF; count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawImagePointsI(graphics: GpGraphics; image: GpImage;
    dstpoints: GPPOINT; count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawImagePointRect(graphics: GpGraphics; image: GpImage;
    x: Single; y: Single; srcx: Single; srcy: Single; srcwidth: Single;
    srcheight: Single; srcUnit: GPUNIT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawImagePointRectI(graphics: GpGraphics; image: GpImage;
    x: Integer; y: Integer; srcx: Integer; srcy: Integer; srcwidth: Integer;
    srcheight: Integer; srcUnit: GPUNIT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawImageRectRect(graphics: GpGraphics; image: GpImage;
    dstx: Single; dsty: Single; dstwidth: Single; dstheight: Single;
    srcx: Single; srcy: Single; srcwidth: Single; srcheight: Single;
    srcUnit: GPUNIT; imageAttributes: GpImageAttributes;
    callback: TIGPDrawImageAbort; callbackData: Pointer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawImageRectRectI(graphics: GpGraphics; image: GpImage;
    dstx: Integer; dsty: Integer; dstwidth: Integer; dstheight: Integer;
    srcx: Integer; srcy: Integer; srcwidth: Integer; srcheight: Integer;
    srcUnit: GPUNIT; imageAttributes: GpImageAttributes;
    callback: TIGPDrawImageAbort; callbackData: Pointer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawImagePointsRect(graphics: GpGraphics; image: GpImage;
    points: GPPOINTF; count: Integer; srcx: Single; srcy: Single;
    srcwidth: Single; srcheight: Single; srcUnit: GPUNIT;
    imageAttributes: GpImageAttributes; callback: TIGPDrawImageAbort;
    callbackData: Pointer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawImagePointsRectI(graphics: GpGraphics; image: GpImage;
    points: GPPOINT; count: Integer; srcx: Integer; srcy: Integer;
    srcwidth: Integer; srcheight: Integer; srcUnit: GPUNIT;
    imageAttributes: GpImageAttributes; callback: TIGPDrawImageAbort;
    callbackData: Pointer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipEnumerateMetafileDestPoint(graphics: GpGraphics;
    metafile: GpMetafile; destPoint: PGPPointF; callback: GpEnumerateMetafileProc;
    callbackData: Pointer;
    imageAttributes: GpImageAttributes): GpStatus; stdcall; external WINGDIPDLL;

  function GdipEnumerateMetafileDestPointI(graphics: GpGraphics;
    metafile: GpMetafile; destPoint: PGPPoint; callback: GpEnumerateMetafileProc;
    callbackData: Pointer;
    imageAttributes: GpImageAttributes): GpStatus; stdcall; external WINGDIPDLL;

  function GdipEnumerateMetafileDestRect(graphics: GpGraphics;
    metafile: GpMetafile; destRect: PGPRectF; callback: GpEnumerateMetafileProc;
    callbackData: Pointer;
    imageAttributes: GpImageAttributes): GpStatus; stdcall; external WINGDIPDLL;

  function GdipEnumerateMetafileDestRectI(graphics: GpGraphics;
    metafile: GpMetafile; destRect: PGPRect; callback: GpEnumerateMetafileProc;
    callbackData: Pointer;
    imageAttributes: GpImageAttributes): GpStatus; stdcall; external WINGDIPDLL;

  function GdipEnumerateMetafileDestPoints(graphics: GpGraphics;
    metafile: GpMetafile; destPoints: PGPPointF; count: Integer;
    callback: GpEnumerateMetafileProc; callbackData: Pointer;
    imageAttributes: GpImageAttributes): GpStatus; stdcall; external WINGDIPDLL;

  function GdipEnumerateMetafileDestPointsI(graphics: GpGraphics;
    metafile: GpMetafile; destPoints: PGPPoint; count: Integer;
    callback: GpEnumerateMetafileProc; callbackData: Pointer;
    imageAttributes: GpImageAttributes): GpStatus; stdcall; external WINGDIPDLL;

  function GdipEnumerateMetafileSrcRectDestPoint(graphics: GpGraphics;
    metafile: GpMetafile; destPoint: PGPPointF; srcRect: PGPRectF; srcUnit: TIGPUnit;
    callback: GpEnumerateMetafileProc; callbackData: Pointer;
    imageAttributes: GpImageAttributes): GpStatus; stdcall; external WINGDIPDLL;

  function GdipEnumerateMetafileSrcRectDestPointI(graphics: GpGraphics;
    metafile: GpMetafile; destPoint: PGPPoint; srcRect: PGPRect; srcUnit: TIGPUnit;
    callback: GpEnumerateMetafileProc; callbackData: Pointer;
    imageAttributes: GpImageAttributes): GpStatus; stdcall; external WINGDIPDLL;

  function GdipEnumerateMetafileSrcRectDestRect(graphics: GpGraphics;
    metafile: GpMetafile; destRect: PGPRectF; srcRect: PGPRectF; srcUnit: TIGPUnit;
    callback: GpEnumerateMetafileProc; callbackData: Pointer;
    imageAttributes: GpImageAttributes): GpStatus; stdcall; external WINGDIPDLL;

  function GdipEnumerateMetafileSrcRectDestRectI(graphics: GpGraphics;
    metafile: GpMetafile; destRect: PGPRect; srcRect: PGPRect; srcUnit: TIGPUnit;
    callback: GpEnumerateMetafileProc; callbackData: Pointer;
    imageAttributes: GpImageAttributes): GpStatus; stdcall; external WINGDIPDLL;

  function GdipEnumerateMetafileSrcRectDestPoints(graphics: GpGraphics;
    metafile: GpMetafile; destPoints: PGPPointF; count: Integer; srcRect: PGPRectF;
    srcUnit: TIGPUnit; callback: GpEnumerateMetafileProc; callbackData: Pointer;
    imageAttributes: GpImageAttributes): GpStatus; stdcall; external WINGDIPDLL;

  function GdipEnumerateMetafileSrcRectDestPointsI(graphics: GpGraphics;
    metafile: GpMetafile; destPoints: PGPPoint; count: Integer; srcRect: PGPRect;
    srcUnit: TIGPUnit; callback: GpEnumerateMetafileProc; callbackData: Pointer;
    imageAttributes: GpImageAttributes): GpStatus; stdcall; external WINGDIPDLL;

  function GdipPlayMetafileRecord(metafile: GpMetafile;
    recordType: TIGPEmfPlusRecordType; flags: UINT; dataSize: UINT;
    data: PBYTE): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetClipGraphics(graphics: GpGraphics; srcgraphics: GpGraphics;
    combineMode: TIGPCombineMode): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetClipRect(graphics: GpGraphics; x: Single; y: Single;
    width: Single; height: Single; combineMode: TIGPCombineMode): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetClipRectI(graphics: GpGraphics; x: Integer; y: Integer;
    width: Integer; height: Integer;
    combineMode: TIGPCombineMode): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetClipPath(graphics: GpGraphics; path: GPPATH;
    combineMode: TIGPCombineMode): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetClipRegion(graphics: GpGraphics; region: GpRegion;
    combineMode: TIGPCombineMode): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetClipHrgn(graphics: GpGraphics; hRgn: HRGN;
    combineMode: TIGPCombineMode): GpStatus; stdcall; external WINGDIPDLL;

  function GdipResetClip(graphics: GpGraphics): GpStatus; stdcall; external WINGDIPDLL;

  function GdipTranslateClip(graphics: GpGraphics; dx: Single;
    dy: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipTranslateClipI(graphics: GpGraphics; dx: Integer;
    dy: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetClip(graphics: GpGraphics;
    region: GpRegion): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetClipBounds(graphics: GpGraphics;
    rect: GPRECTF): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetClipBoundsI(graphics: GpGraphics;
    rect: GPRECT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipIsClipEmpty(graphics: GpGraphics;
    result: PBool): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetVisibleClipBounds(graphics: GpGraphics;
    rect: GPRECTF): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetVisibleClipBoundsI(graphics: GpGraphics;
    rect: GPRECT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipIsVisibleClipEmpty(graphics: GpGraphics;
    var result: Bool): GpStatus; stdcall; external WINGDIPDLL;

  function GdipIsVisiblePoint(graphics: GpGraphics; x: Single; y: Single;
    var result: Bool): GpStatus; stdcall; external WINGDIPDLL;

  function GdipIsVisiblePointI(graphics: GpGraphics; x: Integer; y: Integer;
    var result: Bool): GpStatus; stdcall; external WINGDIPDLL;

  function GdipIsVisibleRect(graphics: GpGraphics; x: Single; y: Single;
    width: Single; height: Single; var result: Bool): GpStatus; stdcall; external WINGDIPDLL;

  function GdipIsVisibleRectI(graphics: GpGraphics; x: Integer; y: Integer;
    width: Integer; height: Integer; var result: Bool): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSaveGraphics(graphics: GpGraphics;
    var state: TIGPGraphicsState): GpStatus; stdcall; external WINGDIPDLL;

  function GdipRestoreGraphics(graphics: GpGraphics;
    state: TIGPGraphicsState): GpStatus; stdcall; external WINGDIPDLL;

  function GdipBeginContainer(graphics: GpGraphics; dstrect: GPRECTF;
    srcrect: GPRECTF; unit_: GPUNIT;
    var state: TIGPGraphicsContainer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipBeginContainerI(graphics: GpGraphics; dstrect: GPRECT;
    srcrect: GPRECT; unit_: GPUNIT;
    var state: TIGPGraphicsContainer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipBeginContainer2(graphics: GpGraphics;
    var state: TIGPGraphicsContainer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipEndContainer(graphics: GpGraphics;
    state: TIGPGraphicsContainer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetMetafileHeaderFromWmf(hWmf: HMETAFILE;
    wmfPlaceableFileHeader: PGPWmfPlaceableFileHeader;
    header: Pointer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetMetafileHeaderFromEmf(hEmf: HENHMETAFILE;
    header: Pointer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetMetafileHeaderFromFile(filename: PWCHAR;
    header: Pointer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetMetafileHeaderFromStream(stream: ISTREAM;
    header: Pointer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetMetafileHeaderFromMetafile(metafile: GpMetafile;
    header: Pointer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetHemfFromMetafile(metafile: GpMetafile;
    var hEmf: HENHMETAFILE): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateStreamOnFile(filename: PWCHAR; access: UINT;
    out stream: ISTREAM): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateMetafileFromWmf(hWmf: HMETAFILE; deleteWmf: Bool;
    wmfPlaceableFileHeader: PGPWmfPlaceableFileHeader;
    out metafile: GpMetafile): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateMetafileFromEmf(hEmf: HENHMETAFILE; deleteEmf: Bool;
    out metafile: GpMetafile): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateMetafileFromFile(file_: PWCHAR;
    out metafile: GpMetafile): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateMetafileFromWmfFile(file_: PWCHAR;
    wmfPlaceableFileHeader: PGPWmfPlaceableFileHeader;
    out metafile: GpMetafile): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateMetafileFromStream(stream: ISTREAM;
    out metafile: GpMetafile): GpStatus; stdcall; external WINGDIPDLL;

  function GdipRecordMetafile(referenceHdc: HDC; type_: TIGPEmfType;
    frameRect: GPRECTF; frameUnit: TIGPMetafileFrameUnit;
    description: PWCHAR; out metafile: GpMetafile): GpStatus; stdcall; external WINGDIPDLL;

  function GdipRecordMetafileI(referenceHdc: HDC; type_: TIGPEmfType;
    frameRect: GPRECT; frameUnit: TIGPMetafileFrameUnit; description: PWCHAR;
    out metafile: GpMetafile): GpStatus; stdcall; external WINGDIPDLL;

  function GdipRecordMetafileFileName(fileName: PWCHAR; referenceHdc: HDC;
    type_: TIGPEmfType; frameRect: GPRECTF; frameUnit: TIGPMetafileFrameUnit;
    description: PWCHAR; out metafile: GpMetafile): GpStatus; stdcall; external WINGDIPDLL;

  function GdipRecordMetafileFileNameI(fileName: PWCHAR; referenceHdc: HDC;
    type_: TIGPEmfType; frameRect: GPRECT; frameUnit: TIGPMetafileFrameUnit;
    description: PWCHAR; out metafile: GpMetafile): GpStatus; stdcall; external WINGDIPDLL;

  function GdipRecordMetafileStream(stream: ISTREAM; referenceHdc: HDC;
    type_: TIGPEmfType; frameRect: GPRECTF; frameUnit: TIGPMetafileFrameUnit;
    description: PWCHAR; out metafile: GpMetafile): GpStatus; stdcall; external WINGDIPDLL;

  function GdipRecordMetafileStreamI(stream: ISTREAM; referenceHdc: HDC;
    type_: TIGPEmfType; frameRect: GPRECT; frameUnit: TIGPMetafileFrameUnit;
    description: PWCHAR; out metafile: GpMetafile): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetMetafileDownLevelRasterizationLimit(metafile: GpMetafile;
    metafileRasterizationLimitDpi: UINT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetMetafileDownLevelRasterizationLimit(metafile: GpMetafile;
    var metafileRasterizationLimitDpi: UINT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetImageDecodersSize(out numDecoders: UINT;
    out size: UINT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetImageDecoders(numDecoders: UINT; size: UINT;
    decoders: PGPImageCodecInfo): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetImageEncodersSize(out numEncoders: UINT;
    out size: UINT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetImageEncoders(numEncoders: UINT; size: UINT;
    encoders: PGPImageCodecInfo): GpStatus; stdcall; external WINGDIPDLL;

  function GdipComment(graphics: GpGraphics; sizeData: UINT;
    data: PBYTE): GpStatus; stdcall; external WINGDIPDLL;

//----------------------------------------------------------------------------
// FontFamily APIs
//----------------------------------------------------------------------------

  function GdipCreateFontFamilyFromName(name: PWCHAR;
    fontCollection: GPFONTCOLLECTION;
    out FontFamily: GPFONTFAMILY): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDeleteFontFamily(FontFamily: GPFONTFAMILY): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCloneFontFamily(FontFamily: GPFONTFAMILY;
    out clonedFontFamily: GPFONTFAMILY): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetGenericFontFamilySansSerif(
    out nativeFamily: GPFONTFAMILY): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetGenericFontFamilySerif(
    out nativeFamily: GPFONTFAMILY): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetGenericFontFamilyMonospace(
    out nativeFamily: GPFONTFAMILY): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetFamilyName(family: GPFONTFAMILY; name: PWideChar;
    language: LANGID): GpStatus; stdcall; external WINGDIPDLL;

  function GdipIsStyleAvailable(family: GPFONTFAMILY; style: Integer;
    var IsStyleAvailable: Bool): GpStatus; stdcall; external WINGDIPDLL;

  function GdipFontCollectionEnumerable(fontCollection: GPFONTCOLLECTION;
    graphics: GpGraphics; var numFound: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipFontCollectionEnumerate(fontCollection: GPFONTCOLLECTION;
    numSought: Integer; gpfamilies: array of GPFONTFAMILY;
    var numFound: Integer; graphics: GpGraphics): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetEmHeight(family: GPFONTFAMILY; style: Integer;
    out EmHeight: UINT16): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetCellAscent(family: GPFONTFAMILY; style: Integer;
    var CellAscent: UINT16): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetCellDescent(family: GPFONTFAMILY; style: Integer;
    var CellDescent: UINT16): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetLineSpacing(family: GPFONTFAMILY; style: Integer;
    var LineSpacing: UINT16): GpStatus; stdcall; external WINGDIPDLL;

//----------------------------------------------------------------------------
// Font APIs
//----------------------------------------------------------------------------

  function GdipCreateFontFromDC(hdc: HDC; out font: GPFONT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateFontFromLogfontA(hdc: HDC; logfont: PLOGFONTA;
    out font: GPFONT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateFontFromLogfontW(hdc: HDC; logfont: PLOGFONTW;
    out font: GPFONT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCreateFont(fontFamily: GPFONTFAMILY; emSize: Single;
    style: Integer; unit_: Integer; out font: GPFONT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCloneFont(font: GPFONT;
    out cloneFont: GPFONT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDeleteFont(font: GPFONT): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetFamily(font: GPFONT;
    out family: GPFONTFAMILY): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetFontStyle(font: GPFONT;
    var style: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetFontSize(font: GPFONT; var size: Single): GpStatus; stdcall; external WINGDIPDLL;
  function GdipGetFontUnit(font: GPFONT; var unit_: TIGPUnit): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetFontHeight(font: GPFONT; graphics: GpGraphics;
    var height: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetFontHeightGivenDPI(font: GPFONT; dpi: Single;
    var height: Single): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetLogFontA(font: GPFONT; graphics: GpGraphics;
    var logfontA: LOGFONTA): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetLogFontW(font: GPFONT; graphics: GpGraphics;
    var logfontW: LOGFONTW): GpStatus; stdcall; external WINGDIPDLL;

  function GdipNewInstalledFontCollection(
    out fontCollection: GPFONTCOLLECTION): GpStatus; stdcall; external WINGDIPDLL;

  function GdipNewPrivateFontCollection(
    out fontCollection: GPFONTCOLLECTION): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDeletePrivateFontCollection(
    out fontCollection: GPFONTCOLLECTION): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetFontCollectionFamilyCount(fontCollection: GPFONTCOLLECTION;
    var numFound: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetFontCollectionFamilyList(fontCollection: GPFONTCOLLECTION;
    numSought: Integer; gpfamilies: GPFONTFAMILY;
    var numFound: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipPrivateAddFontFile(fontCollection: GPFONTCOLLECTION;
    filename: PWCHAR): GpStatus; stdcall; external WINGDIPDLL;

  function GdipPrivateAddMemoryFont(fontCollection: GPFONTCOLLECTION;
    memory: Pointer; length: Integer): GpStatus; stdcall; external WINGDIPDLL;

//----------------------------------------------------------------------------
// Text APIs
//----------------------------------------------------------------------------

  function GdipDrawString(graphics: GpGraphics; string_: PWCHAR;
    length: Integer; font: GPFONT; layoutRect: PGPRectF;
    stringFormat: GpStringFormat; brush: GPBRUSH): GpStatus; stdcall; external WINGDIPDLL;

  function GdipMeasureString(graphics: GpGraphics; string_: PWCHAR;
    length: Integer; font: GPFONT; layoutRect: PGPRectF;
    stringFormat: GpStringFormat; boundingBox: PGPRectF;
    codepointsFitted: PInteger; linesFilled: PInteger): GpStatus; stdcall; external WINGDIPDLL;

  function GdipMeasureCharacterRanges(graphics: GpGraphics; string_: PWCHAR;
    length: Integer; font: GPFONT; layoutRect: PGPRectF;
    stringFormat: GpStringFormat; regionCount: Integer;
    const regions: GpRegion): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawDriverString(graphics: GpGraphics; const text: PUINT16;
    length: Integer; const font: GPFONT; const brush: GPBRUSH;
    const positions: PGPPointF; flags: Integer;
    const matrix: GPMATRIX): GpStatus; stdcall; external WINGDIPDLL;

  function GdipMeasureDriverString(graphics: GpGraphics; text: PUINT16;
    length: Integer; font: GPFONT; positions: PGPPointF; flags: Integer;
    matrix: GPMATRIX; boundingBox: PGPRectF): GpStatus; stdcall; external WINGDIPDLL;

//----------------------------------------------------------------------------
// String format APIs
//----------------------------------------------------------------------------

  function GdipCreateStringFormat(formatAttributes: Integer; language: LANGID;
    out format: GpStringFormat): GpStatus; stdcall; external WINGDIPDLL;

  function GdipStringFormatGetGenericDefault(
    out format: GpStringFormat): GpStatus; stdcall; external WINGDIPDLL;

  function GdipStringFormatGetGenericTypographic(
    out format: GpStringFormat): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDeleteStringFormat(format: GpStringFormat): GpStatus; stdcall; external WINGDIPDLL;

  function GdipCloneStringFormat(format: GpStringFormat;
    out newFormat: GpStringFormat): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetStringFormatFlags(format: GpStringFormat;
    flags: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetStringFormatFlags(format: GpStringFormat;
    out flags: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetStringFormatAlign(format: GpStringFormat;
    align: TIGPStringAlignment): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetStringFormatAlign(format: GpStringFormat;
    out align: TIGPStringAlignment): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetStringFormatLineAlign(format: GpStringFormat;
    align: TIGPStringAlignment): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetStringFormatLineAlign(format: GpStringFormat;
    out align: TIGPStringAlignment): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetStringFormatTrimming(format: GpStringFormat;
    trimming: TIGPStringTrimming): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetStringFormatTrimming(format: GpStringFormat;
    out trimming: TIGPStringTrimming): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetStringFormatHotkeyPrefix(format: GpStringFormat;
    hotkeyPrefix: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetStringFormatHotkeyPrefix(format: GpStringFormat;
    out hotkeyPrefix: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetStringFormatTabStops(format: GpStringFormat;
    firstTabOffset: Single; count: Integer;
    tabStops: PSingle): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetStringFormatTabStops(format: GpStringFormat;
    count: Integer; firstTabOffset: PSingle;
    tabStops: PSingle): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetStringFormatTabStopCount(format: GpStringFormat;
    out count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetStringFormatDigitSubstitution(format: GpStringFormat;
    language: LANGID;
    substitute: TIGPStringDigitSubstitute): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetStringFormatDigitSubstitution(format: GpStringFormat;
    language: PUINT; substitute: PGPStringDigitSubstitute): GpStatus; stdcall; external WINGDIPDLL;

  function GdipGetStringFormatMeasurableCharacterRangeCount(format: GpStringFormat;
    out count: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipSetStringFormatMeasurableCharacterRanges(format: GpStringFormat;
    rangeCount: Integer; ranges: PGPCharacterRange): GpStatus; stdcall; external WINGDIPDLL;

//----------------------------------------------------------------------------
// Cached Bitmap APIs
//----------------------------------------------------------------------------

  function GdipCreateCachedBitmap(bitmap: GpBitmap; graphics: GpGraphics;
    out cachedBitmap: GpCachedBitmap): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDeleteCachedBitmap(
    cachedBitmap: GpCachedBitmap): GpStatus; stdcall; external WINGDIPDLL;

  function GdipDrawCachedBitmap(graphics: GpGraphics;
    cachedBitmap: GpCachedBitmap; x: Integer;
    y: Integer): GpStatus; stdcall; external WINGDIPDLL;

  function GdipEmfToWmfBits(hemf: HENHMETAFILE; cbData16: UINT; pData16: PBYTE;
    iMapMode: Integer; eFlags: Integer): UINT; stdcall; external WINGDIPDLL;

