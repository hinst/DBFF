unit MapViewer;

{$mode objfpc}{$H+}
{$DEFINE LOG_TEXTURE_CACHE}

interface

uses
  Classes,
  SysUtils,

  zgl_screen,
  zgl_window,
  zgl_textures,
  zgl_primitives_2d,
  zgl_mouse,
  zgl_keyboard,
  zgl_sprite_2d,
  zgl_fx,
  zgl_render_target,

  LogEntity,
  LogEntityFace,
  Generic2DArray,
  NiceExceptions,

  TextureCache,
  MapDataFace,
  EngineManagerFace,
  TerrainManager,
  TerrainManagerFace;

type

  { TMapView }
  // XSpeed and YSpeed are measured in tiles per second

  TMapView = class(TComponent)
  public
    constructor Create(const aOwner: TComponent); reintroduce;
  public type
    TForeachVisualCell = procedure(const aX, aY: integer; const aXD, aYD: single) of object;

    TCellMaskInfo = record
      Up, Down, Left, Right: integer;
      UpT, DownT, LeftT, RightT: TTerrainType;
      // -1 stands for no mask;
      // non-negative number stands for mask index in Terrains.Masks property
    end;

    PCellMaskInfo = ^TCellMaskInfo;

    TDrawFraming = record
      xD, yD: single;
      x, y: integer;
      cmi: PCellMaskInfo;
      typee: TTerrainType;
    end;

    PDrawFraming = ^TDrawFraming;

    TCellMaskInfoColumn = array of PCellMaskInfo;

    TCellMaskInfoMatrix = array of TCellMaskInfoColumn;

  private
    fLog: ILog;
    fViewX, fViewY: single;
    fFieldWidth, fFieldHeight: single;
    fXSpeed, fYSpeed: single;
    fMap: IMapData;
    fMatrix: TCells.TMatrix; // direct access to Map.Cells.Matrix; cached
    fMapWidth, fMapHeight: integer; // direct access to Map.Cells.Width and Map.Cells.Height; cached
    fTerrain: ITerrainManager;
    fTerrainMan: TTerrainManager; // direct access to Terrain.Reverse
    fMaskTexture: zglPTexture; // direct access to TerrainMan.Masks.Area^.Surface
    fMaskCount: integer; // direct access to TerrainMan.Masks.Count
    fMasks: TCellMaskInfoMatrix;
    fGridColor: LongWord;
    fGridAlpha: byte;
    fFramings: TFPList;
    fFramingTexture: zglPRenderTarget;
    fCellTexture: zglPRenderTarget;
    fEngine: IEngineManager;
    fCache: TMapTextureCache;
    fAllowNewCacheItem: boolean;
    procedure Initialize;
    procedure AssignDefaults;
    procedure FocusViewOnMapCenter;
    procedure DetermineFieldDemensions;
    procedure DetermineMasks;
    function GetViewXLeft: single; inline;
    function GetViewYUp: single; inline;
    procedure SetTerrain(const aTerrain: ITerrainManager);
    procedure DrawTerrainLayerSubcolor(const aX, aY: integer; const aXD, aYD: single);
    procedure CheckCellFraming(const aX, aY: integer; out aU, aD, aL, aR: TTerrainType); inline;
    function ProcessFraming(const aX, aY: integer; const aXD, aYD: single): boolean; inline;
    procedure DrawTerrainLayerSimple(const aX, aY: integer; const aXD, aYD: single);
    procedure DrawFramingCached(const aF: PDrawFraming); inline;
    procedure DrawFraming(const aF: PDrawFraming); inline;
    procedure OverlapByFrame(const aTerrainType: TTerrainType;
      const aMaskIndex: integer; const aAngle: single);
    procedure DrawBottomCellTexture(const aTexture: zglPTexture);
    procedure DrawFramings;
    procedure ReleaseFraming;
    procedure ReleaseMasks;
    procedure Finalize;
  public const
    TileWidth = 64;
    TileHeight = 64;
    DefaultXSpeed = 6;
    DefaultYSpeed = 6;
    DefaultGridColor = $FFFFFF;
    DefaultGridAlpha = 255 div 2;
  public
    property Log: ILog read fLog write fLog;
    property ViewX: single read fViewX write fViewX;
    property ViewY: single read fViewY write fViewY;
    property ViewXCorner: single read GetViewXLeft;
    property ViewYCorner: single read GetViewYUp;
    property FieldWidth: single read fFieldWidth write fFieldWidth;
    property FieldHeight: single read fFieldHeight write fFieldHeight;
    property XSpeed: single read fXSpeed write fXSpeed;
    property YSpeed: single read fYSpeed write fYSpeed;
    property Map: IMapData read fMap write fMap;
    property Matrix: TCells.TMatrix read fMatrix write fMatrix;
    property MapWidth: integer read fMapWidth;
    property MapHeight: integer read fMapHeight;
    property Terrain: ITerrainManager read fTerrain write SetTerrain;
    property TerrainMan: TTerrainManager read fTerrainMan;
    property MaskTexture: zglPTexture read fMaskTexture;
    property MaskCount: integer read fMaskCount;
    property Masks: TCellMaskInfoMatrix read fMasks;
    property GridColor: LongWord read fGridColor write fGridColor;
    property GridAlpha: byte read fGridAlpha write fGridAlpha;
    property Framings: TFPList read fFramings;
    property FramingTexture: zglPRenderTarget read fFramingTexture;
    property CellTexture: zglPRenderTarget read fCellTexture;
    property Engine: IEngineManager read fEngine;
    property Cache: TMapTextureCache read fCache;
    property AllowNewCacheItem: boolean read fAllowNewCacheItem;
      // it is necessary to call this procedure after changing the map data
    procedure Update;
    procedure ReceiveInput(const aT: double);
    procedure DrawDebugInfo;
    procedure DrawTerrainLayerSubcolors;
    procedure DrawTerrainLayerSimples;
    procedure ForeachVisibleCell(const aProcedure: TForeachVisualCell);
    procedure DrawGridLines;
    destructor Destroy; override;
  end;

implementation

uses
  Common;

function SortFraming(Item1, Item2: pointer): integer;
type
  PDrawFraming = TMapView.PDrawFraming;
var
  framing1, framing2: PDrawFraming;
begin
  framing1 := PDrawFraming(Item1);
  framing2 := PDrawFraming(Item2);
  if framing1^.typee = framing2^.typee then
    result := 0
  else if framing1^.typee > framing2^.typee then
    result := +1
  else
    result := -1;
end;

function CreateMaskInfo: TMapView.PCellMaskInfo;
begin
  New(result);
  result^.Up := -1;
  result^.Down := -1;
  result^.Left := -1;
  result^.Right := -1;
end;

function CellMaskInfoToText(const aX, aY: integer; const aInfo: TMapView.PCellMaskInfo): string;
begin
  result := IntToStr(aX) + ':' + IntToStr(aY) + ' ';
  if aInfo = nil then
    result += 'N'
  else
  begin
    if aInfo^.Up <> -1 then
      result += 'U';
    if aInfo^.Down <> -1 then
      result += 'D';
    if aInfo^.Left <> -1 then
      result += 'L';
    if aInfo^.Right <> -1 then
      result += 'R';
  end;
end;

{ TMapView }

constructor TMapView.Create(const aOwner: TComponent);
begin
  inherited Create(aOwner);
  Initialize;
end;

procedure TMapView.Initialize;
begin
  fLog := TLog.Create(GlobalLogManager, 'MapView');
  fEngine := GlobalGameManager.Engine;
  fCache := TMapTextureCache.Create;
  Cache.Engine := Engine;
  Cache.CleaningDistance := Cache.GetDefaultDistance(TileWidth);
  {$IFDEF LOG_TEXTURE_CACHE}
  Cache.Log := TLog.Create(GlobalLogManager, 'MTCache');
  {$ENDIF}
  AssignDefaults;
end;

procedure TMapView.AssignDefaults;
begin
  XSpeed := DefaultXSpeed;
  YSpeed := DefaultYSpeed;
  GridColor := DefaultGridColor;
  GridAlpha := DefaultGridAlpha;
end;

procedure TMapView.FocusViewOnMapCenter;
begin
  ViewX := FieldWidth / 2;
  ViewY := FieldHeight / 2;
end;

procedure TMapView.DetermineFieldDemensions;
begin
  FieldWidth := Map.Cells.Width * TileWidth;
  FieldHeight := Map.Cells.Height * TileHeight;
  Log.Write('Field dimensions determined: '
    + FloatToStr(FieldWidth) + ' x ' + FloatToStr(FieldHeight));
end;

procedure TMapView.DetermineMasks;
const
  DEBUG = false;
var
  U, D, L, R: TTerrainType;
  x, y: integer;
  maskInfo: PCellMaskInfo;
begin
  if DEBUG then
    Log.Write('Now determining masks...:');
  if Length(Masks) <> 0 then
    ReleaseMasks;
  SetLength(fMasks, MapWidth, MapHeight);
  AssertArgumentAssigned(Assigned(Matrix), 'Matrix');
  for x := 0 to MapWidth - 1 do
    for y := 0 to MapHeight - 1 do
    begin
      CheckCellFraming(x, y, U, D, L, R);
      maskInfo := nil;
      if (U <> -1) or (D <> -1) or (L <> -1) or (R <> -1) then
      begin
        maskInfo := CreateMaskInfo;
        if U <> -1 then
        begin
          maskInfo^.Up := random(MaskCount);
          maskInfo^.UpT := U;
        end;
        if D <> -1 then
        begin
          maskInfo^.Down := random(MaskCount);
          maskInfo^.DownT := D;
        end;
        if L <> -1 then
        begin
          maskInfo^.Left := random(MaskCount);
          maskInfo^.LeftT := L;
        end;
        if R <> -1 then
        begin
          maskInfo^.Right := random(MaskCount);
          maskInfo^.RightT := R;
        end;
        Masks[x, y] := maskInfo;
      end;
      Masks[x, y] := maskInfo;
      if DEBUG then
        Log.Write(CellMaskInfoToText(x, y, maskInfo));
    end;
end;

function TMapView.GetViewXLeft: single;
begin
  result := ViewX - single(wndWidth) / 2;
end;

function TMapView.GetViewYUp: single;
begin
  result := ViewY - single(wndHeight) / 2;
end;

procedure TMapView.SetTerrain(const aTerrain: ITerrainManager);
begin
  fTerrain := aTerrain;
  fTerrainMan := TTerrainManager(aTerrain.Reverse);
  fMaskTexture := TerrainMan.Masks.Area^.Surface;
  fMaskCount := TerrainMan.Masks.Count;
end;

procedure TMapView.DrawTerrainLayerSubcolor(const aX, aY: integer; const aXD, aYD: single);
var
  typee: TTerrainType;
  color: LongWord;
begin
  typee := Map.Cells.Matrix[aX, aY].typee;
  if typee = -1 then
    exit;
  color := Terrain.GetTypeColor(typee);
  pr2d_Rect(aXD, aYD, TileWidth+1, TileHeight+1, color, 255, PR2D_FILL);
end;

procedure TMapView.CheckCellFraming(const aX, aY: integer; out aU, aD, aL,
  aR: TTerrainType);

  procedure CleanUDLR; inline;
  begin
    aU := -1;
    aD := -1;
    aL := -1;
    aR := -1;
  end;

var
  C: TTerrainType;

  procedure CompareAndReplace(var aT: TTerrainType);
  begin
    if aT = -1 then
      exit;
    if C >= aT then
      aT := -1;
  end;

begin
  C := Matrix[aX, aY].typee;
  CleanUDLR;
  // UP
  if aY > 0 then
    aU := Matrix[aX, aY - 1].typee;
  CompareAndReplace(aU);

  // DOWN
  if aY < MapHeight - 1 then
    aD := Matrix[aX, aY + 1].typee;
  CompareAndReplace(aD);

  // LEFT
  if aX > 0 then
    aL := Matrix[aX - 1, aY].typee;
  CompareAndReplace(aL);

  // RIGHT
  if aX < MapWidth - 1 then
    aR := Matrix[aX + 1, aY].typee;
  CompareAndReplace(aR);
end;

function TMapView.ProcessFraming(const aX, aY: integer; const aXD, aYD: single
  ): boolean;
var
  mask: PCellMaskInfo;
  framing: PDrawFraming;

begin
  mask := Masks[aX, aY];
  result := Assigned(mask);
  if not result then
    exit;

  New(framing);

  framing^.xD := aXD;
  framing^.yD := aYD;
  framing^.x := aX;
  framing^.y := aY;
  framing^.cmi := mask;
  framing^.typee := Matrix[aX, aY].typee;
  Framings.Add(framing);
end;

procedure TMapView.DrawTerrainLayerSimple(const aX, aY: integer; const aXD, aYD: single);
var
  typee: TTerrainType;
  texture: zglPTexture;
  t: TTerrain;
  isAnyFraming: boolean;
begin
  typee := Matrix[aX, aY].typee;
  t := TerrainMan.Terrains[typee];
  texture := t.Texture;
  if texture = nil then
    exit;
  isAnyFraming := ProcessFraming(aX, aY, aXD, aYD);
  if not isAnyFraming then
    ssprite2d_Draw(texture, aXD, aYD, TileWidth, TileHeight, 0);
end;

procedure TMapView.DrawFramingCached(const aF: PDrawFraming);
const
  DEBUG = false;
var
  cacheItem: PMapTextureCacheItem;
begin
  if DEBUG then
    Log.Write('DrawFramingCached - method start...');
  if DEBUG then
    AssertAssigned(Cache, 'Cache');
  cacheItem := Cache.Access[aF^.x, aF^.y];
  if (cacheItem = nil) and (AllowNewCacheItem) then
  begin
    if DEBUG then
      Log.Write('Encountering cell '
        + IntToStr(aF^.x) + ':' + IntToStr(aF^.y)
        + ' for the first time...');
    cacheItem := Cache.AddNew;
    if cacheItem = nil then
    begin
      if DEBUG then
        Log.Write('Warning: cache does not returns new cache item');
      exit;
    end;
    cacheItem^.x := aF^.x;
    cacheItem^.y := aF^.y;
    DrawFraming(aF);
    if DEBUG then
      Log.Write('Adding to the cache...');
    cacheItem^.texture := Engine.DirectCopyTexture(CellTexture^.Surface);
    if DEBUG then
      Log.Write('Marking the redrawing cycle to be interrupted...');
    fAllowNewCacheItem := false;
  end;
  if Assigned(cacheItem) then
  begin
    if DEBUG then
      Log.Write('Now drawing texture from cache...');
    ssprite2d_Draw(cacheItem^.texture, aF^.xD, aF^.yD, TileWidth, TileHeight, 0);
    if DEBUG then
      Log.Write('  Drawing texture from cache: DONE.');
  end;
end;

procedure TMapView.DrawFraming(const aF: PDrawFraming);

const
  DEBUG = false;

  procedure OverlapDebugMessage(const aDirection: string;
    const aWhat: TTerrainType;
    const aMaskIndex: integer); inline;
  var
    s: string;
  begin
    if not DEBUG then exit;
    s := 'Now drawing overlap: ';
    s += aDirection + ': ';
    s += IntToStr(aWhat) + ' on ' + IntToStr(aF^.typee) + '; ';
    s += 'Mask index is ' + IntToStr(aMaskIndex);
    Log.Write(s);
  end;

  procedure OverlapDebugMessageDone;
  begin
    if not DEBUG then exit;
    Log.Write('Done');
  end;

var
  t: TTerrain;
  bottomTexture: zglPTexture;
  cmi: PCellMaskInfo;

begin
  t := TerrainMan.Terrains[aF^.typee];
  DrawBottomCellTexture(t.Texture);
  if aF^.cmi^.Up <> -1 then
  begin
    OverlapDebugMessage('Up', aF^.cmi^.UpT, aF^.cmi^.Up);
    OverlapByFrame(aF^.cmi^.UpT, aF^.cmi^.Up, 180);
    OverlapDebugMessageDone;
  end;
  if aF^.cmi^.Down <> -1 then
  begin
    OverlapDebugMessage('Down', aF^.cmi^.DownT, aF^.cmi^.Down);
    OverlapByFrame(aF^.cmi^.DownT, aF^.cmi^.Down, 0);
    OverlapDebugMessageDone;
  end;
  if aF^.cmi^.Left <> -1 then
  begin
    OverlapDebugMessage('Left', aF^.cmi^.LeftT, aF^.cmi^.Left);
    OverlapByFrame(aF^.cmi^.LeftT, aF^.cmi^.Left, 90);
    OverlapDebugMessageDone;
  end;
  if aF^.cmi^.Right <> -1 then
  begin
    OverlapDebugMessage('Right', aF^.cmi^.RightT, aF^.cmi^.Right);
    OverlapByFrame(aF^.cmi^.RightT, aF^.cmi^.Right, -90);
    OverlapDebugMessageDone;
  end;
end;

procedure TMapView.OverlapByFrame(const aTerrainType: TTerrainType;
  const aMaskIndex: integer; const aAngle: single);
const
  DEBUG = false;
var
  texture: zglPTexture;
  mask: zglPTexture;
begin
  if DEBUG then
  begin
    AssertAssigned(FramingTexture, 'FramingTexture');
    AssertAssigned(MaskTexture, 'MaskTexture');
    AssertAssigned(TerrainMan, 'TerrainMan');
  end;
  rtarget_Set(FramingTexture);
  Engine.DirectCleanTexture(FramingTexture);
  asprite2d_Draw(MaskTexture, 0, 0, TileWidth, TileHeight, aAngle, aMaskIndex);
  texture := TerrainMan.Terrains[aTerrainType].Texture;
  fx_SetBlendMode(FX_BLEND_MULT, false);
  ssprite2d_Draw(texture, 0, 0, TileWidth, TileHeight, 0);
  fx_SetBlendMode(FX_BLEND_NORMAL);
  rtarget_Set(CellTexture);
  ssprite2d_Draw(FramingTexture^.Surface, 0, 0, TileWidth, TileHeight, 0);
  rtarget_Set(nil);
end;

procedure TMapView.DrawBottomCellTexture(const aTexture: zglPTexture);
begin
  rtarget_set(CellTexture);
  Engine.DirectCleanTexture(CellTexture);
  ssprite2d_Draw(aTexture, 0, 0, TileWidth, TileHeight, 0);
  rtarget_set(nil);
end;

procedure TMapView.DrawFramings;
var
  i: integer;
begin
  fAllowNewCacheItem := true;
  for i := 0 to Framings.Count - 1 do
  begin
    DrawFramingCached(Framings[i]);
    //if false = AllowNewCacheItem then break;
  end;
  Cache.Clean(Round(ViewX / TileWidth), Round(ViewY / TileHeight));
end;

procedure TMapView.ReleaseFraming;
var
  i: integer;
  p: pointer;
  f: PDrawFraming;
begin
  for i := 0 to Framings.Count - 1 do
  begin
    p := Framings[i];
    f := PDrawFraming(p);
    Dispose(f);
  end;
  Framings.Clear;
end;

procedure TMapView.ReleaseMasks;
var
  x, y: integer;
  maskInfo: PCellMaskInfo;
begin
  for x := 0 to Length(Masks) - 1 do
    for y := 0 to Length(Masks[x]) - 1 do
      if Masks[x, y] <> nil then
      begin
        maskInfo := Masks[x, y];
        Dispose(maskInfo);
        Masks[x, y] := nil;
      end;
  SetLength(fMasks, 0, 0);
end;

procedure TMapView.Finalize;
begin
  ReleaseMasks;
  if GlobalEngineRunning then
  Cache.CleanAll(GlobalEngineRunning);
  FreeAndNil(fCache);
  FreeAndNil(fFramings);
  FreeLog(fLog);
end;

procedure TMapView.Update;
begin
  Log.Write('Now updating map...');
  fMatrix := Map.Cells.Matrix;
  fMapWidth := Map.Cells.Width;
  fMapHeight := Map.Cells.Height;
  DetermineFieldDemensions;
  DetermineMasks;
  FocusViewOnMapCenter;

  fFramings := TFPList.Create;
  Log.Write('Updating map: creating surfaces...');
  fFramingTexture := Engine.CreateRenderTarget(TileWidth, TileHeight);
  fCellTexture := Engine.CreateRenderTarget(TileWidth, TileHeight);
  Log.Write('Updating map: creating surfaces - Done.');
end;

procedure TMapView.ReceiveInput(const aT: double);
  function DeltaX: single; inline;
  begin
    result := aT / single(1000) * fXSpeed * single(TileWidth);
  end;

  function DeltaY: single; inline;
  begin
    result := aT / single(1000) * fYSpeed * single(TileHeight);
  end;

begin
  if key_Down(K_W) then
    fViewY -= DeltaY;
  if key_Down(K_S) then
    fViewY += DeltaY;
  if key_Down(K_A) then
    fViewX -= DeltaX;
  if key_Down(K_D) then
    fViewX += DeltaX;
end;

procedure TMapView.DrawDebugInfo;
begin

end;

procedure TMapView.DrawTerrainLayerSubcolors;
begin
  ForeachVisibleCell(@DrawTerrainLayerSubcolor);
end;

procedure TMapView.DrawTerrainLayerSimples;
const
  DEBUG = false;
begin
  if DEBUG then
    Log.Write('Stage 1');
  ForeachVisibleCell(@DrawTerrainLayerSimple);
  if DEBUG then
    Log.Write('Stage 2');
  Framings.Sort(@SortFraming);
  if DEBUG then
    Log.Write('Stage 3');
  DrawFramings;
  if DEBUG then
    Log.Write('Stage 4');
  ReleaseFraming;
  if DEBUG then
    Log.Write('Stage F');
end;

  { Warning: some heavy calculations are taking place in this procedure }
procedure TMapView.ForeachVisibleCell(const aProcedure: TForeachVisualCell);
var
  x, y, yy: integer;
  mcw, mch: integer;
  xD, yD, xLA, yUA, xA, yA, yDD: single;
begin
  x := 0;
  xA := 0;
  y := 0;
  yA := 0;
  xLA := ViewXCorner;
  yUA := ViewYCorner;
  while xA < xLA do
  begin
    xA += TileWidth;
    x += 1;
  end;
  if x > 0 then
  begin
    xA -= TileWidth;
    x -= 1;
  end;
  while yA < yUA do
  begin
    yA += TileHeight;
    y += 1;
  end;
  if y > 0 then
  begin
    yA -= TileHeight;
    y -= 1;
  end;
  xD := xA - xLA;
  yD := yA - yUA;
  yDD := yD; // store
  yy := y; // store
  mcw := MapWidth;
  mch := MapHeight;
  while (x < mcw) and (xD < wndWidth) do
  begin
    y := yy;
    yD := yDD;
    while (y < mch) and (yD < wndHeight) do
    begin
      aProcedure(x, y, xD, yD);
      y += 1;
      yD += TileHeight;
    end;
    x += 1;
    xD += TileWidth;
  end;
end;

procedure TMapView.DrawGridLines;
  procedure DrawVerticalLines;
  var
    xLA, xA, xD: single;
  begin
    xLA := ViewXCorner;
    xA := 0;
    while xA < xLA do xA += TileWidth;
    xD := xA - xLA;
    while (xA <= FieldWidth) and (xD < wndWidth) do
    begin
      pr2d_Line(xD, 0, xD, wndHeight, GridColor, GridAlpha);
      xA += TileWidth;
      xD += TileWidth;
    end;
  end;

  procedure DrawHorizontalLines;
  var
    yUA, yA, yD: single;
  begin
    yUA := ViewYCorner;
    yA := 0;
    while yA < yUA do yA += TileHeight;
    yD := yA - yUA;
    while (yA <= FieldHeight) and (yD < wndHeight) do
    begin
      pr2d_Line(0, yD, wndWidth, yD, GridColor, GridAlpha);
      yA += TileHeight;
      yD += TileHeight;
    end;
  end;

begin
  DrawVerticalLines;
  DrawHorizontalLines;
end;

destructor TMapView.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

end.

