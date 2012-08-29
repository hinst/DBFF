unit AbstractTurretUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  zgl_textures,
  zgl_sprite_2d,

  Common,
  ZenGLFCLGraphics,
  Angle360,
  MapUnitFace,
  MapUnit,
  MapDataFace,
  MapScrollManagerFace,
  BuildingUnit;


type

  { TAbstractTurretType }

  TAbstractTurretType = class(TBuildingType)
  protected
    fTowerTexture: zglPTexture;
    function GetTopTextureFilePath: string; virtual; abstract;
    procedure Finalize;
  public
    property TextureFilePath: string read GetTextureFilePath;
    property TopTextureFilePath: string read GetTopTextureFilePath;
    property TowerTexture: zglPTexture read fTowerTexture;
    procedure Load; override;
    destructor Destroy; override;
  end;

  TAbstractTurret = class(TBuilding, IMapUnit)
  public
    constructor Create(const aType: TMapUnitType); override;
  protected
    fTowerAngle: TAngle360;
    fDesiredTowerAngle: TAngle360;
    fIdleChangeAngleTimeLeft: single;
    function GetMyType: TAbstractTurretType;
    function GetOccupatedCells: TCellNumbers;
    function GetUnitWidth: integer; override;
    function GetUnitHeight: integer; override;
    procedure Initialize;
    procedure MoveToDesiredAngle(const aTime: double);
    procedure IdleChangeAngle(const aTime: double);
  public const
    TowerSpeed = 360 / 2000;
    IdleChangeAngleTime = 5000;
  public
    property TowerAngle: TAngle360 read fTowerAngle;
    property DesiredTowerAngle: TAngle360 read fDesiredTowerAngle;
    property IdleChangeAngleTimeLeft: single read fIdleChangeAngleTimeLeft;
    property MyType: TAbstractTurretType read GetMyType;
    procedure Draw(const aScroll: IMapScrollManager); override;
    procedure Update(const aTime: double);
  end;

implementation

{ TAbstractTurretType }

procedure TAbstractTurretType.Finalize;
begin
  Engine.DisposeTexture(fTowerTexture);
end;

procedure TAbstractTurretType.Load;
begin
  inherited Load;
  fTowerTexture := Engine.LoadTexture(TopTextureFilePath);
end;

destructor TAbstractTurretType.Destroy;
begin
  Finalize;
  inherited Destroy;
end;


{TAbstractTurret}

constructor TAbstractTurret.Create(const aType: TMapUnitType);
begin
  inherited Create(aType);
  Initialize;
end;

function TAbstractTurret.GetMyType: TAbstractTurretType;
begin
  result := UnitType as TAbstractTurretType;
end;

function TAbstractTurret.GetOccupatedCells: TCellNumbers;
begin
  SetLength(result, 1);
  result[0].Assign(fLeftTopCell);
end;

function TAbstractTurret.GetUnitWidth: integer;
begin
  result := 1;
end;

function TAbstractTurret.GetUnitHeight: integer;
begin
  result := 1;
end;

procedure TAbstractTurret.Initialize;
begin
  TowerAngle.Random;
  DesiredTowerAngle.Random;
  fIdleChangeAngleTimeLeft := IdleChangeAngleTime;
end;

procedure TAbstractTurret.MoveToDesiredAngle(const aTime: double);
begin
  TowerAngle.MoveToDesiredAngle(DesiredTowerAngle, aTime*TowerSpeed);
end;

procedure TAbstractTurret.IdleChangeAngle(const aTime: double);
//{$DEFINE DEBUG_THIS_PROCEDURE}
begin
  fIdleChangeAngleTimeLeft -= aTime;
  if IdleChangeAngleTimeLeft <= 0 then
  begin
    fDesiredTowerAngle := random(360);
    {$IFDEF DEBUG_THIS_PROCEDURE}
    Log.Write('Desired angle changed (idle): ' + FloatToStr(DesiredTowerAngle));
    {$ENDIF}
    fIdleChangeAngleTimeLeft := random(IdleChangeAngleTime) + IdleChangeAngleTime;
  end;
end;
{$UNDEF DEBUG_THIS_PROCEDURE}

procedure TAbstractTurret.Draw(const aScroll: IMapScrollManager);
begin
  if not IsVisible(aScroll) then exit;
  with aScroll do
  begin
    ssprite2d_Draw(MyType.Texture,
      aScroll.ScreenX(LeftTopCell^),
      aScroll.ScreenY(LeftTopCell^),
      TileWidth,
      TileHeight,
      0);
    ssprite2d_Draw(MyType.TowerTexture,
      aScroll.ScreenX(LeftTopCell^),
      aScroll.ScreenY(LeftTopCell^),
      TileWidth,
      TileHeight,
      TowerAngle
    );
  end;
end;

procedure TAbstractTurret.Update(const aTime: double);
begin
  MoveToDesiredAngle(aTime);
  IdleChangeAngle(aTime);
end;

end.

