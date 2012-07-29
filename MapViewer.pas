unit MapViewer;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes,
  SysUtils,

  zgl_screen,
  zgl_window,
  zgl_primitives_2d,
  zgl_mouse,
  zgl_keyboard,

  LogEntity,
  LogEntityFace,

  MapDataFace,
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
    fTerrain: ITerrainManager;
    fGridColor: LongWord;
    fGridAlpha: byte;
    procedure Initialize;
    procedure AssignDefaults;
    procedure FocusViewOnMapCenter;
    procedure DetermineFieldDemensions;
    function GetViewXLeft: single; inline;
    function GetViewYUp: single; inline;
    procedure DrawTerrainLayerSubcolor(const aX, aY: integer; const aXD, aYD: single);
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
    property Terrain: ITerrainManager read fTerrain write fTerrain;
    property GridColor: LongWord read fGridColor write fGridColor;
    property GridAlpha: byte read fGridAlpha write fGridAlpha;
      // it is necessary to call this procedure after changing the map data
    procedure Update;
    procedure ReceiveInput(const aT: single);
    procedure DrawDebugInfo;
    procedure DrawTerrainLayerSubcolors;
    procedure ForeachVisibleCell(const aProcedure: TForeachVisualCell);
    procedure DrawGridLines;
    destructor Destroy; override;
  end;

implementation

uses
  Common;

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

procedure TMapView.DrawTerrainLayerSubcolor(const aX, aY: integer; const aXD,
  aYD: single);
var
  &type: TTerrainType;
  color: LongWord;
begin
  &type := Map.Cells.Matrix[aX, aY].&type;
  color := Terrain.GetTypeColor(&type);
  pr2d_Rect(aXD, aYD, TileWidth, TileHeight, color, 255, PR2D_FILL);
end;

procedure TMapView.Finalize;
begin
  FreeLog(fLog);
end;

procedure TMapView.Update;
begin
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

