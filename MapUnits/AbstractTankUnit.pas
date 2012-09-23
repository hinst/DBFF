unit AbstractTankUnit;

{$DEFINE DEBUG_LOG_TANK_PATHFINDER}
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

  TAbstractTank = class(TVehicle, IMapUnit, IMoveableMapUnit)
  public
    constructor Create(const aType: TMapUnitType); override;
  protected
    fBodyAngle: TAngle360;
    fDesiredBodyAngle: TAngle360;
    fTowerAngle: TAngle360;
    fDesiredTowerAngle: TAngle360;
    function GetMyType: TAbstractTankType;
    function GetOccupatedCells: TCellNumberArray;
    function GetUnitWidth: integer; override;
    function GetUnitHeight: integer; override;
    procedure Initialize;
    procedure UpdateMove(const aTime: double); override;
    procedure SetMovementDirection(const aDeltaX, aDeltaY: integer); override;
  public
    property BodyAngle: TAngle360 read fBodyAngle;
    property DesiredBodyAngle: TAngle360 read fDesiredBodyAngle;
    property TowerAngle: TAngle360 read fTowerAngle;
    property DesiredTowerAngle: TAngle360 read fDesiredTowerAngle;
    property MyType: TAbstractTankType read GetMyType;
    procedure Draw(const aScroll: IMapScrollManager); override;
    procedure Update(const aTime: double); override;
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

procedure TAbstractTank.Initialize;
begin
  TowerAngle.Random;
  DesiredTowerAngle.Random;
  BodyAngle.Random;
  DesiredBodyAngle.Random;
end;

procedure TAbstractTank.UpdateMove(const aTime: double);
begin
  if BodyAngle.Value = DesiredBodyAngle.Value then
    inherited UpdateMove(aTime);
end;

procedure TAbstractTank.SetMovementDirection(const aDeltaX, aDeltaY: integer);
begin
  inherited SetMovementDirection(aDeltaX, aDeltaY);
  DesiredBodyAngle.Assign(
    CalculateDesiredAngleForMovement(aDeltaX, aDeltaY)
  );
end;

procedure TAbstractTank.Draw(const aScroll: IMapScrollManager);
begin
  inherited Draw(aScroll);
  if not IsVisible(aScroll) then
    exit;
  with aScroll do
  begin
    {$REGION DRAW_BASE}
    ssprite2d_Draw(
      MyType.Texture,
      ScreenX(LeftTopCell^) + DeltaX * aScroll.TileWidth,
      ScreenY(LeftTopCell^) + DeltaY * aScroll.TileHeight,
      TileWidth,
      TileHeight,
      BodyAngle
    );
    {$ENDREGION}
    {$REGION DRAW_TOWER}
    ssprite2d_Draw(
      MyType.TowerTexture,
      ScreenX(LeftTopCell^) + DeltaX * aScroll.TileWidth,
      ScreenY(LeftTopCell^) + DeltaY * aScroll.TileHeight,
      TileWidth,
      TileHeight,
      TowerAngle
    );
    {$ENDREGION}
  end;
end;

procedure TAbstractTank.Update(const aTime: double);
begin
  inherited Update(aTime);
  TowerAngle.MoveToDesiredAngle(DesiredTowerAngle, aTime * MyType.TowerSpeed);
  BodyAngle.MoveToDesiredAngle(DesiredBodyAngle, aTime * MyType.TowerSpeed);
end;

destructor TAbstractTank.Destroy;
begin
  inherited Destroy;
end;

end.

