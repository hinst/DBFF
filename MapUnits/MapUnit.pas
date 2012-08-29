unit MapUnit;

{$mode objfpc}{$H+}
//{$DEFINE DEBUG_LOG_VISIBILITY}

interface

uses
  Classes,
  SysUtils,

  zgl_math_2d,
  zgl_textures,

  NiceExceptions,
  LogEntityFace,
  LogEntity,

  Common,
  EngineManagerFace,
  MapUnitFace,
  MapDataFace,
  MapScrollManagerFace,
  TerrainManagerFaceE;

type

  { TMapUnitType }

  TMapUnitType = class
  public
      // this constructor probably should be overridden in descendants
    constructor Create; virtual;
  protected
    fTexture: zglPTexture;
    function GetEngine: IEngineManager;
    function GetStandardUnitsPath: string;
    function GetTextureFilePath: string; virtual; abstract;
    procedure Finalize;
  public
    property Texture: zglPTexture read fTexture;
    property Engine: IEngineManager read GetEngine;
    property StandardUnitsPath: string read GetStandardUnitsPath;
    property TextureFilePath: string read GetTextureFilePath;
    procedure Load; virtual;
    destructor Destroy; override;
  end;

  TMapUnitTypeClass = class of TMapUnitType;

  { TMapUnit }

  TMapUnit = class
  public
    constructor Create(const aType: TMapUnitType); virtual;
  protected
    fLog: ILog;
    fUnitType: TMapUnitType;
    fLeftTopCell: TCellNumber;
    fGraphicalRect: zglTRect;
    fLastTimeVisible: boolean;
    function GetLeftTopCell: PCellNumber;
    function GetGraphicalRect: zglPRect;
    function GetTerrainPossible(const aTerrain: PTerrain): boolean; virtual;
    function Reverse: TObject;
    function GetUnitWidth: integer; virtual; abstract;
    function GetUnitHeight: integer; virtual; abstract;
    procedure Initialize(const aType: TMapUnitType);
    procedure Finalize;
  public
    property Log: ILog read fLog;
    property UnitType: TMapUnitType read fUnitType;
    property LeftTopCell: PCellNumber read GetLeftTopCell;
    property GraphicalRect: zglPRect read GetGraphicalRect;
    property LastTimeVisible: boolean read fLastTimeVisible;
    property UnitWidth: integer read GetUnitWidth;
    property UnitHeight: integer read GetUnitHeight;
    procedure UpdateGraphicalRect(const aScroll: IMapScrollManager);
    function IsVisible(const aScroll: IMapScrollManager): boolean;
    procedure Draw(const aScroll: IMapScrollManager); virtual;
    procedure DrawTopLayer(const aScroll: IMapScrollManager); virtual;
    destructor Destroy; override;
  end;

implementation

{ TMapUnitType }

constructor TMapUnitType.Create;
begin
  inherited Create;
end;

function TMapUnitType.GetEngine: IEngineManager;
begin
  result := GlobalGameManager.Engine;
end;

function TMapUnitType.GetStandardUnitsPath: string;
begin
  result := GlobalApplicationPath + StandardUnitsRelativePath;
end;

procedure TMapUnitType.Finalize;
begin
  Engine.DisposeTexture(fTexture);
end;

procedure TMapUnitType.Load;
begin
  if TextureFilePath <> '' then
    fTexture := Engine.LoadTexture(TextureFilePath);
end;

destructor TMapUnitType.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

{ TMapUnit }

constructor TMapUnit.Create(const aType: TMapUnitType);
begin
  inherited Create;
  Initialize(aType);
end;

function TMapUnit.GetLeftTopCell: PCellNumber;
begin
  result := @fLeftTopCell;
end;

function TMapUnit.GetGraphicalRect: zglPRect;
begin
  result := @fGraphicalRect;
end;

function TMapUnit.GetTerrainPossible(const aTerrain: PTerrain): boolean;
begin
  result := false;
end;

function TMapUnit.Reverse: TObject;
begin
  result := self;
end;

procedure TMapUnit.Initialize(const aType: TMapUnitType);
begin
  fLog := TLog.Create(GlobalLogManager, self.ClassName);
  fUnitType := aType;
end;

procedure TMapUnit.Finalize;
begin
  FreeLog(fLog);
end;

procedure TMapUnit.UpdateGraphicalRect(const aScroll: IMapScrollManager);
begin
  GraphicalRect^.X := LeftTopCell^.X * aScroll.TileWidth;
  GraphicalRect^.Y := LeftTopCell^.Y * aScroll.TileHeight;
  GraphicalRect^.W := UnitWidth * aScroll.TileWidth;
  GraphicalRect^.H := UnitHeight * aScroll.TileHeight;
end;

function TMapUnit.IsVisible(const aScroll: IMapScrollManager): boolean;
var
  cell: TCellNumber;
  cells: TCellNumbers;
  visible: boolean;
begin
  cells := (self as IMapUnit).OccupatedCells;
  visible := false;
  for cell in cells do
    if aScroll.CellVisible[cell.X, cell.Y] then
    begin
      visible := true;
      break;
    end;
  {$IFDEF DEBUG_LOG_VISIBILITY}
  if visible and not LastTimeVisible then
    Log.Write('Unit is now visible');
  if not visible and LastTimeVisible then
    Log.Write('Unit is now not visible');
  {$ENDIF}
  fLastTimeVisible := visible;
  result := visible;
end;

procedure TMapUnit.Draw(const aScroll: IMapScrollManager);
begin
end;

procedure TMapUnit.DrawTopLayer(const aScroll: IMapScrollManager);
begin
end;

destructor TMapUnit.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

end.

