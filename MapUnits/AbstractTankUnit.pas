unit AbstractTankUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  zgl_textures,
  zgl_sprite_2d,

  Angle360,

  MapUnitFace,
  MapUnit,
  MapDataFace,
  MapScrollManager,
  VehicleUnit
  ;

type

  { TAbstractTankType }

  TAbstractTankType = class(TVehicleType)
  public
    constructor Create; override;
  protected
    fBodySpeed: single;
    fTowerTexture: zglPTexture;
    fTowerSpeed: single;
    function GetTowerTextureFilePath: string; virtual; abstract;
    procedure Finalize;
  public
    property TowerTexture: zglPTexture read fTowerTexture;
    property TowerSpeed: single read fTowerSpeed;
    property TowerTextureFilePath: string read GetTowerTextureFilePath;
    procedure Load; override;
    destructor Destroy; override;
  end;

  { TAstractTank }

  TAbstractTank = class(TVehicle, IMapUnit)
  public
    constructor Create(const aType: TMapUnitType); override;
  protected
    fBodyAngle: TAngle360;
    fDesiredBodyAngle: TAngle360;
    fTowerAngle: TAngle360;
    fDesiredTowerAngle: TAngle360;
    function GetMyType: TAbstractTankType;
    function GetOccupatedCells: TCellNumbers;
    function GetUnitWidth: integer; override;
    function GetUnitHeight: integer; override;
    procedure Initialize;
    procedure SureDraw(const aScroll: TMapScrollManager); override;
  public
    property BodyAngle: TAngle360 read fBodyAngle;
    property DesiredBodyAngle: TAngle360 read fDesiredBodyAngle;
    property TowerAngle: TAngle360 read fTowerAngle;
    property DesiredTowerAngle: TAngle360 read fDesiredTowerAngle;
    property MyType: TAbstractTankType read GetMyType;
    procedure Update(const aTime: double);
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

function TAbstractTank.GetOccupatedCells: TCellNumbers;
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

procedure TAbstractTank.SureDraw(const aScroll: TMapScrollManager);
begin
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
end;

end.

