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

  LogEntity,
  LogEntityFace,
  Generic2DArray,

  MapDataFace,
  TerrainManager,
  TerrainManagerFace;

type

  { TMapView }
  // XSpeed and YSpeed are measured in tiles per second

  TMapView = class(TComponent)
  public
    constructor Create(const aOwner: TComponent); reintroduce;
  private
    fLog: ILog;
    fViewX, fViewY: single;
    fFieldWidth, fFieldHeight: single;
    fXSpeed, fYSpeed: single;
    fMap: IMapData;
    fMatrix: TCells.TMatrix; // direct access to Map.Cells.Matrix
    fTerrain: ITerrainManager;
    fTerrainMan: TTerrainManager;
    fGridColor: LongWord;
    fGridAlpha: byte;
    fFraming: TFPList;
    procedure Initialize;
    procedure AssignDefaults;
    procedure FocusViewOnMapCenter;
    procedure DetermineFieldDemensions;
    function GetViewXLeft: single; inline;
    function GetViewYUp: single; inline;
    procedure SetTerrain(const aTerrain: ITerrainManager);
    procedure ClearDrawn;
    procedure DrawTerrainLayerSubcolor(const aX, aY: integer; const aXD, aYD: single);
    procedure ProcessFraming(const aX, aY: integer; const aXD, aYD: single);
    procedure DrawTerrainLayerSimple(const aX, aY: integer; const aXD, aYD: single);
    procedure ReleaseFraming;
    procedure Finalize;
  public type
    TForeachVisualCell = procedure(const aX, aY: integer; const aXD, aYD: single) of object;
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
    property Terrain: ITerrainManager read fTerrain write SetTerrain;
    property TerrainMan: TTerrainManager read fTerrainMan;
    property GridColor: LongWord read fGridColor write fGridColor;
    property GridAlpha: byte read fGridAlpha write fGridAlpha;
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

type
  TDrawFraming = record
    xd, yd: single;
    Up, Down, Left, Right: boolean;
    typee: TTerrainType;
  end;

  PDrawFraming = ^TDrawFraming;

function SortFraming(Item1, Item2: pointer): integer;
var
  framing1, framing2: PDrawFraming;
begin
  framing1 := PDrawFraming(Item1);
  framing2 := PDrawFraming(Item2);
  if framing1^.typee > framing2^.typee then
    result := 1
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

procedure TMapView.ProcessFraming(const aX, aY: integer; const aXD, aYD: single);
var
  c, U, D, L, R: TTerrainType;
  framing : PDrawFraming;

  procedure CleanUDLR; inline;
  begin
    U := -1;
    D := -1;
    L := -1;
    R := -1;
  end;

  function f: PDrawFraming; inline;
  begin
    if framing = nil then
      New(framing);
    result := framing;
  end;

begin
  c := Matrix[aX, aY].typee;
  CleanUDLR;
  framing := nil;

  // UP
  if aY > 0 then
    U := Matrix[aX, aY - 1].typee;
  if c <> U then
    f^.Up := true;

  // DOWN
  if aY < Map.Cells.Height - 1 then
    D := Matrix[aX, aY + 1].typee;
  if c <> D then
    f^.Down := true;

  // LEFT
  if aX > 0 then
    L := Matrix[aX - 1, aY].typee;
  if c <> L then
    f^.Left := true;

  // RIGHT
  if aX < Map.Cells.Width - 1 then
    R := Matrix[aX + 1, aY].typee;
  if c <> R then
    f^.Right := true;

  if framing = nil then exit;
  framing^.xd := aXD;
  framing^.yd := aYD;
  fFraming.Add(framing);
end;

procedure TMapView.DrawTerrainLayerSimple(const aX, aY: integer; const aXD, aYD: single);
var
  typee: TTerrainType;
  texture: zglPTexture;
  t: TTerrain;
begin
  typee := Matrix[aX, aY].typee;
  t := TerrainMan.Terrains[typee];
  texture := t.Texture;
  if texture = nil then
    exit;
  ssprite2d_Draw(TerrainMan.SMask, aXD, aYD, TileWidth, TileHeight, 0);
  ProcessFraming(aX, aY, aXD, aYD);
  {
  fx_SetBlendMode( FX_BLEND_MULT, FALSE );
  ssprite2d_Draw(texture, aXD, aYD, TileWidth, TileHeight, 0);
  fx_SetBlendMode( FX_BLEND_NORMAL );
  }
end;

procedure TMapView.ReleaseFraming;
var
  p: pointer;
  f: PDrawFraming;
begin
  for p in fFraming do
  begin
    f := PDrawFraming(p);
    Dispose(f);
  end;
end;

procedure TMapView.Finalize;
begin
  FreeLog(fLog);
end;

procedure TMapView.Update;
begin
  fMatrix := Map.Cells.Matrix;
  DetermineFieldDemensions;
  FocusViewOnMapCenter;
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
begin
  fFraming := TFPList.Create;
  ForeachVisibleCell(@DrawTerrainLayerSimple);
  fFraming.Sort(@SortFraming);
  FreeAndNil(fFraming);
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
  mcw := Map.Cells.Width;
  mch := Map.Cells.Height;
  while (x < mcw) and (xD < wndWidth) do
  begin
    y := yy;
    yD := yDD;
    while (y < mch) and (yD < wndHeight) do
    begin
      //WriteLN('calling aP');
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

