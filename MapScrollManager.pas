unit MapScrollManager;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  Common,
  MapDataFace,
  LogEntityFace,
  LogEntity,

  zgl_window,
  zgl_keyboard;

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
    function GetViewLeft: single;
    function GetViewTop: single;
    function GetViewRight: single;
    function GetViewBottom: single;
    procedure Initialize(const aMap: IMapData);
    procedure AssignDefaults;
    procedure DetermineFieldDimensions;
    procedure ScrollMap(const aTime: double);
    function GetCellVisible(const aX, aY: integer): boolean;
  public const
    DefaultXSpeed = 6; // squares per second
    DefaultYSpeed = 6;
    DefaultTileWidth = 64;
    DefaultTileHeight = 64;
  public
    property Log: ILog read fLog;
    property Map: IMapData read fMap write fMap;
    property ViewX: single read fViewX;
    property ViewY: single read fViewY;
    property XSpeed: single read fXSpeed write fXSpeed;
    property YSpeed: single read fYSpeed write fYSpeed;
    property TileWidth: integer read fTileWidth write fTileWidth;
    property TileHeight: integer read fTileHeight write fTileHeight;
    property FieldWidth: single read fFieldWidth;
    property FieldHeight: single read fFieldHeight;
    property ViewLeft: single read GetViewLeft;
    property ViewTop: single read GetViewTop;
    property ViewRight: single read GetViewRight;
    property ViewBottom: single read GetViewBottom;
    property CellVisible[const x, y: integer]: boolean read GetCellVisible;
    procedure ReceiveInput(const aTime: double);
    procedure Update;
    procedure FocusOnMapCenter;
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

procedure TMapScrollManager.ReceiveInput(const aTime: double);
begin
  ScrollMap(aTime);
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

destructor TMapScrollManager.Destroy;
begin
  FreeLog(fLog);
  inherited Destroy;
end;

end.

