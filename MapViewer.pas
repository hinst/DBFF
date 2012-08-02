unit MapViewer;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

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

  MapDataFace,
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
    end;

    PCellMaskInfo = ^TCellMaskInfo;

    TDrawFraming = record
      xD, yD: single;
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
    procedure Initialize;
    procedure AssignDefaults;
    procedure FocusViewOnMapCenter;
    procedure DetermineFieldDemensions;
    procedure DetermineMasks;
    function GetViewXLeft: single; inline;
    function GetViewYUp: single; inline;
    procedure SetTerrain(const aTerrain: ITerrainManager);
    procedure ClearDrawn;
    procedure DrawTerrainLayerSubcolor(const aX, aY: integer; const aXD, aYD: single);
    procedure CheckCellFraming(const aX, aY: integer; var aU, aD, aL, aR: boolean); inline;
    function ProcessFraming(const aX, aY: integer; const aXD, aYD: single): boolean; inline;
    procedure OptimizeFraming;
    procedure DrawTerrainLayerSimple(const aX, aY: integer; const aXD, aYD: single);
    procedure DrawFraming(const aP: PDrawFraming); inline;
    procedure DrawBorderFrame(const aTexture: zglPTexture;
      const aMaskFrame: integer; const aAngle: single;
      const aXD, aYD: single); inline;
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
      // it is necessary to call this procedure after changing the map data
    procedure Update;
    procedure ReceiveInput(const aT: single);
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

{ TMapView }

constructor TMapView.Create(const aOwner: TComponent);
begin
  inherited Create(aOwner);
  Initialize;
end;

procedure TMapView.Initialize;
begin
  fLog := TLog.Create(GlobalLogManager, 'MapView');
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
var
  U, D, L, R: boolean;
  x, y: integer;
  maskInfo: PCellMaskInfo;
begin
  Log.Write('Now determining masks...');
  if Length(Masks) <> 0 then
    ReleaseMasks;
  SetLength(fMasks, MapWidth, MapHeight);
  AssertArgumentAssigned(Assigned(Matrix), 'Matrix');
  for x := 0 to MapWidth - 1 do
    for y := 0 to MapHeight - 1 do
    begin
      CheckCellFraming(x, y, U, D, L, R);
      if U or D or L or R then
      begin
        New(maskInfo);
        if U then
          maskInfo^.Up := random(MaskCount);
        if D then
          maskInfo^.Down := random(MaskCount);
        if L then
          maskInfo^.Left := random(MaskCount);
        if R then
          maskInfo^.Right := random(MaskCount);
        Masks[x, y] := maskInfo;
      end
      else
        Masks[x, y] := nil;
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

procedure TMapView.ClearDrawn;
begin
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

procedure TMapView.CheckCellFraming(const aX, aY: integer; var aU, aD, aL,
  aR: boolean);
var
  C, U, D, L, R: TTerrainType;

  procedure CleanUDLR; inline;
  begin
    U := -1;
    D := -1;
    L := -1;
    R := -1;
  end;

begin
  c := Matrix[aX, aY].typee;
  CleanUDLR;
  // UP
  if aY > 0 then
    U := Matrix[aX, aY - 1].typee;
  aU := C > U;

  // DOWN
  if aY < MapHeight - 1 then
    D := Matrix[aX, aY + 1].typee;
  aD := C > D;

  // LEFT
  if aX > 0 then
    L := Matrix[aX - 1, aY].typee;
  aL := C > L;

  // RIGHT
  if aX < MapWidth - 1 then
    R := Matrix[aX + 1, aY].typee;
  aR := C > R;
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
  framing^.cmi := mask;
  framing^.typee := Matrix[aX, aY].typee;
  Framings.Add(framing);
end;

procedure TMapView.OptimizeFraming;
var
  i: integer;
  f: PDrawFraming;
begin
  for i := 0 to Framings.Count - 1 do
  begin
    f := PDrawFraming(Framings[i]);
  end;
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
  {
  fx_SetBlendMode( FX_BLEND_MULT, FALSE );
  ssprite2d_Draw(TerrainMan.SMask, aXD, aYD, TileWidth, TileHeight, 0);
  fx_SetBlendMode( FX_BLEND_NORMAL );
  }
end;

procedure TMapView.DrawFraming(const aP: PDrawFraming);
var
  f: PDrawFraming;
  typee: TTerrainType;
  t: TTerrain;
  texture: zglPTexture;
  MaskFrame: integer;
  xD, yD: single;
  cmi: TCellMaskInfo;
begin
  f := PDrawFraming(aP);
  typee := f^.typee;
  xD := f^.xD;
  yD := f^.yD;
  t := TerrainMan.Terrains[typee];
  texture := t.Texture;
  ssprite2d_Draw(texture, xD, yD, TileWidth, TileHeight, 0);

  cmi := f^.cmi^;
  DrawBorderFrame(texture, cmi.Up, 0, xD, yD - TileHeight);
  DrawBorderFrame(texture, cmi.Down, 180, xD, yD + TileHeight);
  DrawBorderFrame(texture, cmi.Left, -90, xD - TileWidth, yD);
  DrawBorderFrame(texture, cmi.Right, +90, xD + TileWidth, yD);
end;

procedure TMapView.DrawBorderFrame(const aTexture: zglPTexture;
  const aMaskFrame: integer; const aAngle: single;
  const aXD, aYD: single);
begin
  if aMaskFrame = - 1 then
    exit;
  rtarget_Set(FramingTexture);
  fx_SetBlendMode(FX_BLEND_MULT, FALSE);
  pr2d_Rect(
    0, 0, TileWidth, TileHeight, // XYWH
    0, 0, // color, alpha
    PR2D_FILL
  );
  fx_SetBlendMode(FX_BLEND_NORMAL);
  asprite2d_Draw(
    MaskTexture,
    0, 0, TileWidth, TileHeight, // XYWH
    aAngle, // angle
    aMaskFrame
  );
  fx_SetBlendMode(FX_BLEND_MULT, FALSE);
  ssprite2d_Draw(aTexture, 0, 0, TileWidth, TileHeight, 0);
  rtarget_Set(nil);
  fx_SetBlendMode(FX_BLEND_NORMAL);
  ssprite2d_Draw(FramingTexture^.Surface, aXD, aYD, TileWidth, TileHeight, 0);
end;

procedure TMapView.DrawFramings;
var
  i: integer;
begin
  for i := 0 to Framings.Count - 1 do
    DrawFraming(Framings[i]);
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
  FreeAndNil(fFramings);
  FreeLog(fLog);
end;

procedure TMapView.Update;
begin
  fMatrix := Map.Cells.Matrix;
  fMapWidth := Map.Cells.Width;
  fMapHeight := Map.Cells.Height;
  DetermineFieldDemensions;
  DetermineMasks;
  FocusViewOnMapCenter;

  fFramings := TFPList.Create;
  fFramingTexture := rtarget_Add(tex_CreateZero(TileWidth, TileHeight), RT_DEFAULT);
end;

procedure TMapView.ReceiveInput(const aT: single);
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

