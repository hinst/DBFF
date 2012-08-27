unit MapScrollManager;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  zgl_math_2d,
  zgl_window,
  zgl_keyboard,
  zgl_mouse,

  LogEntityFace,
  LogEntity,

  Common,
  MapDataFace,
  TerrainViewerFace;

type

  { TMapScrollManager }

  TMapScrollManager = class
  public
    constructor Create;
  private
    fLog: ILog;
    fMap: IMapData;
    fViewX, fViewY: single;
    fXSpeed, fYSpeed: single;
    fTileWidth, fTileHeight: integer;
    fFieldWidth, fFieldHeight: single;
    fDisplayCellInfo: boolean;
    function GetViewLeft: single;
    function GetViewTop: single;
    function GetViewRight: single;
    function GetViewBottom: single;
    procedure Initialize(const aMap: IMapData);
    procedure AssignDefaults;
    procedure DetermineFieldDimensions;
    procedure ScrollMap(const aTime: double);
    function GetCellVisible(const aX, aY: integer): boolean;
    function GetCellAtWindowPoint(const aX, aY: integer): PCell;
    function GetCellNumberAtWindowPoint(const aX, aY: integer): TCellNumber;
  public const
    DefaultXSpeed = 6; // squares per second
    DefaultYSpeed = 6;
    DefaultTileWidth = 64;
    DefaultTileHeight = 64;
  public
    property Log: ILog read fLog;
      // This property should be assigned
    property Map: IMapData read fMap write fMap;
    property ViewX: single read fViewX;
    property ViewY: single read fViewY;
    property XSpeed: single read fXSpeed write fXSpeed;
    property YSpeed: single read fYSpeed write fYSpeed;
    property TileWidth: integer read fTileWidth write fTileWidth;
    property TileHeight: integer read fTileHeight write fTileHeight;
    property FieldWidth: single read fFieldWidth;
    property FieldHeight: single read fFieldHeight;
    property DisplayCellInfo: boolean read fDisplayCellInfo write fDisplayCellInfo;
    property ViewLeft: single read GetViewLeft;
    property ViewTop: single read GetViewTop;
    property ViewRight: single read GetViewRight;
    property ViewBottom: single read GetViewBottom;
    property CellVisible[const x, y: integer]: boolean read GetCellVisible;
    procedure ReceiveInput(const aTime: double);
    procedure DrawCellInfo(const aX, aY: integer);
    procedure Update;
    procedure FocusOnMapCenter;
    property CellAtWindowPoint[const x, y: integer]: PCell read GetCellAtWindowPoint;
    property CellNumberAtWindowPoint[const x, y: integer]: TCellNumber
      read GetCellNumberAtWindowPoint;
    function GlobalToScreen(const aRect: zglPRect): zglTRect;
    function ScreenX(const aCell: TCellNumber): single;
    function ScreenY(const aCell: TCellNumber): single;
    destructor Destroy; override;
  end;

implementation

{ TMapScrollManager }

constructor TMapScrollManager.Create;
begin
  inherited Create;
  Initialize(nil);
end;

function TMapScrollManager.GetViewLeft: single;
begin
  result := ViewX - single(wndWidth) / 2;
end;

function TMapScrollManager.GetViewTop: single;
begin
  result := ViewY - single(wndHeight) / 2;
end;

function TMapScrollManager.GetViewRight: single;
begin
  result := ViewX + single(wndWidth) / 2;
end;

function TMapScrollManager.GetViewBottom: single;
begin
  result := ViewY + single(wndHeight) / 2;
end;

procedure TMapScrollManager.Initialize(const aMap: IMapData);
begin
  fLog := TLog.Create(GlobalLogManager, 'MapScroll');
  fMap := aMap;
  AssignDefaults;
end;

procedure TMapScrollManager.AssignDefaults;
begin
  XSpeed := DefaultXSpeed;
  YSpeed := DefaultYSpeed;
  TileWidth := DefaultTileWidth;
  TileHeight := DefaultTileHeight;
end;

procedure TMapScrollManager.DetermineFieldDimensions;
begin
  fFieldWidth := Map.Cells.Width * TileWidth;
  fFieldHeight := Map.Cells.Height * TileHeight;
  Log.Write('Field dimensions determined: '
    + FloatToStr(FieldWidth) + ' x ' + FloatToStr(FieldHeight));
end;

procedure TMapScrollManager.ScrollMap(const aTime: double);
  function DeltaX: single; inline;
  begin
    result := aTime / single(1000) * fXSpeed * single(TileWidth);
  end;

  function DeltaY: single; inline;
  begin
    result := aTime / single(1000) * fYSpeed * single(TileHeight);
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

function TMapScrollManager.GetCellVisible(const aX, aY: integer): boolean;
var
  x, y: single;
begin
  result := true;
  x := aX * TileWidth;
  y := aY * TileHeight;
  if x + TileWidth < ViewLeft then exit(false);
  if y + TileWidth < ViewTop then exit(false);
  if x > ViewRight then exit(false);
  if y > ViewBottom then exit(false);
end;

function TMapScrollManager.GetCellAtWindowPoint(const aX, aY: integer): PCell;
var
  number: TCellNumber;
begin
  number := CellNumberAtWindowPoint[aX, aY];
  if number.IsNegative then
    result := nil
  else
    result := @( Map.Cells.Matrix[number.X, number.Y] );
end;

function TMapScrollManager.GetCellNumberAtWindowPoint(const aX, aY: integer): TCellNumber;
var
  xv, yv: single;
  x, y: integer;
begin
  xv := GetViewLeft + aX;
  x := -1;
  while xv > 0 do
  begin
    xv -= TileWidth;
    x += 1;
  end;
  yv := GetViewTop + aY;
  y := -1;
  while yv > 0 do
  begin
    yv -= TileHeight;
    y += 1;
  end;
  if Map.Cells.CellExists[x, y] then
  begin
    result.X := x;
    result.Y := y;
  end
  else
  begin
    result.X := -1;
    result.Y := -1;
  end;
end;

procedure TMapScrollManager.ReceiveInput(const aTime: double);
begin
  ScrollMap(aTime);
  if DisplayCellInfo then
    DrawCellInfo(mouse_X, mouse_Y);
end;

procedure TMapScrollManager.DrawCellInfo(const aX, aY: integer);
var
  cell: PCell;
begin
  cell := CellAtWindowPoint[aX, aY];
  if cell = nil then exit;
end;

procedure TMapScrollManager.Update;
begin
  DetermineFieldDimensions;
end;

procedure TMapScrollManager.FocusOnMapCenter;
begin
  fViewX := FieldWidth / 2;
  fViewY := FieldHeight / 2;
end;

function TMapScrollManager.GlobalToScreen(const aRect: zglPRect): zglTRect;
begin
  result.X := aRect^.X - ViewLeft;
  result.Y := aRect^.Y - ViewTop;
  result.W := aRect^.W;
  result.H := aRect^.H;
end;

function TMapScrollManager.ScreenX(const aCell: TCellNumber): single;
begin
  result := TileWidth * aCell.X - ViewLeft;
end;

function TMapScrollManager.ScreenY(const aCell: TCellNumber): single;
begin
  result := TileHeight * aCell.Y - ViewTop;
end;

destructor TMapScrollManager.Destroy;
begin
  FreeLog(fLog);
  inherited Destroy;
end;

end.

