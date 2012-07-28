unit MapViewer;

{$mode objfpc}{$H+}

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

  MapDataFace;

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
    fGridColor: LongWord;
    fGridAlpha: byte;
    procedure Initialize;
    procedure AssignDefaults;
    procedure FocusViewOnMapCenter;
    procedure DetermineFieldDemensions;
    procedure Finalize;
  public const
    TileWidth = 64;
    TileHeight = 64;
    DefaultXSpeed = 3;
    DefaultYSpeed = 3;
    DefaultGridColor = $FFFFFF;
    DefaultGridAlpha = 255 div 2;
  public
    property Log: ILog read fLog write fLog;
    property ViewX: single read fViewX write fViewX;
    property ViewY: single read fViewY write fViewY;
    property FieldWidth: single read fFieldWidth write fFieldWidth;
    property FieldHeight: single read fFieldHeight write fFieldHeight;
    property XSpeed: single read fXSpeed write fXSpeed;
    property YSpeed: single read fYSpeed write fYSpeed;
    property Map: IMapData read fMap write fMap;
    property GridColor: LongWord read fGridColor write fGridColor;
    property GridAlpha: byte read fGridAlpha write fGridAlpha;
      // it is necessary to call this procedure after changing the map data
    procedure Update;
    procedure ReceiveInput(const aT: single);
    procedure DrawDebugInfo;
    procedure DrawTerrainLayer;
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
    result := aT / 1000 * fXSpeed;
  end;

  function DeltaY: single; inline;
  begin
    result := aT / 1000 * fYSpeed;
  end;

begin
  if key_Down(K_UP) then
    fViewY -= DeltaY;
  if key_Down(K_DOWN) then
    fViewY += DeltaY;
  if key_Down(K_LEFT) then
    fViewX -= DeltaX;
  if key_Down(K_RIGHT) then
    fViewX += DeltaX;
end;

procedure TMapView.DrawDebugInfo;
begin

end;

procedure TMapView.DrawTerrainLayer;
begin

end;

procedure TMapView.DrawGridLines;
  procedure DrawVerticalLines;
  var
    x, xx: single;
  begin
    xx := ViewX - wndWidth / 2;
    x := xx;
    while x > 0 do
      x -= TileWidth;
    while xx < FieldWidth do
    begin
      x += TileWidth;
      xx += TileWidth;
      pr2d_Line(x, 0, x, wndHeight, GridColor, GridAlpha);
    end;
  end;

  procedure DrawHorizontalLines;
  var
    y, yy: single;
  begin
    yy := ViewX - wndHeight / 2;
    y := yy;
    while y > 0 do
      y -= TileHeight;
    while yy < FieldWidth do
    begin
      y += TileHeight;
      yy += TileHeight;
      pr2d_Line(0, y, wndWidth, y, GridColor, GridAlpha);
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

