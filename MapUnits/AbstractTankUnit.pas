unit AbstractTankUnit;

{$DEFINE DEBUG_LOG_TANK_PATHFINDER}
{$DEFINE DEBUG_LOG_TANK_NAVIGATION}

interface

uses
  Classes,
  SysUtils,

  zgl_textures,
  zgl_sprite_2d,

  Angle360,
  LogEntityFace,
  LogEntity,

  ZenGLFCLGraphics,
  MapUnitFace,
  MapUnit,
  MapDataCells,
  MapDataFace,
  MapScrollManagerFace,
  PathFinder,

  Common,
  VehicleUnit,
  TerrainManagerFaceE
  ;

type

  { TAbstractTankType }

  TAbstractTankType = class(TVehicleType)
  public
    constructor Create; override;
  protected
    fBodyRotationSpeed: single;
    fBodySpeed: single;
    fTowerTexture: zglPTexture;
    fTowerSpeed: single;
    function GetTowerTextureFilePath: string; virtual; abstract;
    procedure Finalize;
  public
    property BodyRotationSpeed: single read fBodyRotationSpeed;
      // Cells/Second
    property BodySpeed: single read fBodySpeed;
    property TowerTexture: zglPTexture read fTowerTexture;
    property TowerSpeed: single read fTowerSpeed;
    property TowerTextureFilePath: string read GetTowerTextureFilePath;
    procedure Load; override;
    destructor Destroy; override;
  end;

  { TAstractTank }

  TAbstractTank = class(TVehicle, IMoveableMapUnit)
  public
    constructor Create(const aType: TMapUnitType); override;
  protected
    fBodyAngle: TAngle360;
    fDesiredBodyAngle: TAngle360;
    fTowerAngle: TAngle360;
    fDesiredTowerAngle: TAngle360;
    fPathFind: TPathFind;
    fPath: TCellNumberVector;
    function GetMyType: TAbstractTankType;
    function GetOccupatedCells: TCellNumberArray;
    function GetUnitWidth: integer; override;
    function GetUnitHeight: integer; override;
    function GetTerrainPossible(const aTerrain: PTerrain): boolean; override;
    procedure LogNavigation(const aText: string);
    procedure UpdateNavigate;
    procedure Initialize;
  public
    property BodyAngle: TAngle360 read fBodyAngle;
    property DesiredBodyAngle: TAngle360 read fDesiredBodyAngle;
    property TowerAngle: TAngle360 read fTowerAngle;
    property DesiredTowerAngle: TAngle360 read fDesiredTowerAngle;
    property PathFind: TPathFind read fPathFind;
    property MyType: TAbstractTankType read GetMyType;
    procedure Draw(const aScroll: IMapScrollManager); override;
    procedure Update(const aTime: double);
    {$REGION IMoveableMapUnit}
      procedure Navigate(const aCell: TCellNumber);
    {$ENDREGION}
    destructor Destroy; override;
  end;


implementation

{ TAbstractTankType }

constructor TAbstractTankType.Create;
begin
  inherited Create;
end;

procedure TAbstractTankType.Finalize;
begin
  Engine.DisposeTexture(fTowerTexture);
end;

procedure TAbstractTankType.Load;
begin
  inherited Load;
  fTowerTexture := Engine.LoadTexture(TowerTextureFilePath);
end;

destructor TAbstractTankType.Destroy;
begin
  Finalize;
  inherited Destroy;
end;


{ TAbstractTank }

constructor TAbstractTank.Create(const aType: TMapUnitType);
begin
  Assert(aType is TAbstractTankType);
  inherited Create(aType);
  Initialize;
end;

function TAbstractTank.GetMyType: TAbstractTankType;
begin
  result := UnitType as TAbstractTankType;
end;

function TAbstractTank.GetOccupatedCells: TCellNumberArray;
begin
  SetLength(result, 1);
  result[0].Assign(LeftTopCell^);
end;

function TAbstractTank.GetUnitWidth: integer;
begin
  result := 1;
end;

function TAbstractTank.GetUnitHeight: integer;
begin
  result := 1;
end;

function TAbstractTank.GetTerrainPossible(const aTerrain: PTerrain): boolean;
begin
  result := aTerrain^.Vehicleable;
end;

procedure TAbstractTank.LogNavigation(const aText: string);
begin
  {$IFDEF DEBUG_LOG_TANK_NAVIGATION}
  Log.Write(aText);
  {$ENDIF}
end;

procedure TAbstractTank.UpdateNavigate;
begin
  if Assigned(PathFind) then
  begin
    PathFind.Iterate;
    if PathFind.Ready then
    begin
      FreeAndNil(fPath);
      LogNavigation('Path found ' + ToText(PathFind.Path));
      fPath := TCellNumberVector.Create(PathFind.Path);
      FreeAndNil(fPathFind);
    end;
  end;
end;

procedure TAbstractTank.Initialize;
begin
  TowerAngle.Random;
  DesiredTowerAngle.Random;
  BodyAngle.Random;
  DesiredBodyAngle.Random;
end;

procedure TAbstractTank.Draw(const aScroll: IMapScrollManager);
begin
  if not IsVisible(aScroll) then exit;
  with aScroll do
  begin
    {$REGION DRAW_BASE}
    ssprite2d_Draw(
      MyType.Texture,
      ScreenX(LeftTopCell^),
      ScreenY(LeftTopCell^),
      TileWidth,
      TileHeight,
      BodyAngle
    );
    {$ENDREGION}
    {$REGION DRAW_TOWER}
    ssprite2d_Draw(
      MyType.TowerTexture,
      ScreenX(LeftTopCell^),
      ScreenY(LeftTopCell^),
      TileWidth,
      TileHeight,
      TowerAngle
    );
    {$ENDREGION}
  end;
end;

procedure TAbstractTank.Update(const aTime: double);
begin
  TowerAngle.MoveToDesiredAngle(DesiredTowerAngle, aTime * MyType.TowerSpeed);
  BodyAngle.MoveToDesiredAngle(DesiredBodyAngle, aTime * MyType.TowerSpeed);
  UpdateNavigate;
end;

procedure TAbstractTank.Navigate(const aCell: TCellNumber);
begin
  LogNavigation('Someone wants me to move: ' +
    LeftTopCell^.ToText + '->' + aCell.ToText);
  // clear old path information ~~~
  FreeAndNil(fPath);
  FreeAndNil(fPathFind);
  // setup new Path Finder ~~~
  fPathFind := TPathFind.Create;
  {$IFDEF DEBUG_LOG_TANK_PATHFINDER}
    PathFind.Log := TLog.Create(GlobalLogManager, 'PathFind');
  {$ENDIF}
  PathFind.Map := GlobalGameManager.Level.Map;
  PathFind.Start := LeftTopCell;
  PathFind.Destination := @aCell;
end;

destructor TAbstractTank.Destroy;
begin
  FreeAndNil(fPath);
  FreeAndNil(fPathFind);
  inherited Destroy;
end;

end.

