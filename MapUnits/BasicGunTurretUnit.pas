unit BasicGunTurretUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  zgl_textures,
  zgl_sprite_2d,

  Common,
  MapUnitFace,
  MapDataFace,
  MapScrollManager,
  BuildingUnit;

type

  { TBasicGunTurretType }

  TBasicGunTurretType = class(TBuildingType)
  protected
    fTopTexture: zglPTexture;
    function GetTextureFilePath: string;
    function GetTopTextureFilePath: string;
    procedure Finalize;
  public
    property TopTexture: zglPTexture read fTopTexture;
    property TextureFilePath: string read GetTextureFilePath;
    property TopTextureFilePath: string read GetTopTextureFilePath;
    procedure Load;
    destructor Destroy; override;
  end;

  TBasicGunTurret = class(TBuilding, IMapUnit)
  public
    constructor Create(const aType: TBuildingType); override;
  protected
    fTowerAngle: single;
    fDesiredTowerAngle: single;
    fIdleChangeAngleTimeLeft: single;
    function GetMyType: TBasicGunTurretType;
    function GetOccupatedCells: TCellNumbers;
    function GetUnitWidth: integer; override;
    function GetUnitHeight: integer; override;
    procedure Initialize;
    procedure SureDraw(const aScroll: TMapScrollManager); override;
    procedure MoveToDesiredAngle(const aTime: double);
    procedure IdleChangeAngle(const aTime: double);
  public const
    TowerSpeed = 360 / 2000;
    IdleChangeAngleTime = 5000;
  public
    property TowerAngle: single read fTowerAngle;
    property DesiredTowerAngle: single read fDesiredTowerAngle;
    property IdleChangeAngleTimeLeft: single read fIdleChangeAngleTimeLeft;
    property MyType: TBasicGunTurretType read GetMyType;
    procedure Update(const aTime: double);
  end;

implementation

constructor TBasicGunTurret.Create(const aType: TBuildingType);
begin
  inherited Create(aType);
  Initialize;
end;

function TBasicGunTurret.GetMyType: TBasicGunTurretType;
begin
  result := BuildingType as TBasicGunTurretType;
end;

function TBasicGunTurret.GetOccupatedCells: TCellNumbers;
begin
  SetLength(result, 1);
  result[0].Assign(fLeftTopCell);
end;

function TBasicGunTurret.GetUnitWidth: integer;
begin
  result := 1;
end;

function TBasicGunTurret.GetUnitHeight: integer;
begin
  result := 1;
end;

procedure TBasicGunTurret.Initialize;
begin
  fTowerAngle := random(360);
  fDesiredTowerAngle := random(360);
  fIdleChangeAngleTimeLeft := IdleChangeAngleTime;
end;

procedure TBasicGunTurret.SureDraw(const aScroll: TMapScrollManager);

  function DrawX: single;
  begin
    result := aScroll.TileWidth * fLeftTopCell.X - aScroll.ViewLeft;
  end;

  function DrawY: single;
  begin
    result := aScroll.TileHeight * fLeftTopCell.Y - aScroll.ViewTop;
  end;

begin
  with aScroll do
  begin
    ssprite2d_Draw(MyType.Texture,
      DrawX,
      DrawY,
      TileWidth,
      TileHeight,
      0);
    ssprite2d_Draw(MyType.TopTexture,
      DrawX,
      DrawY,
      TileWidth,
      TileHeight,
      TowerAngle
    );
  end;
end;

procedure TBasicGunTurret.MoveToDesiredAngle(const aTime: double);
begin
  if TowerAngle = DesiredTowerAngle then exit;
  if TowerAngle < DesiredTowerAngle then
  begin
    fTowerAngle += TowerSpeed * aTime;
    if TowerAngle >= DesiredTowerAngle then
      fTowerAngle := DesiredTowerAngle;
  end
  else
  if TowerAngle > DesiredTowerAngle then
  begin
    fTowerAngle -= TowerSpeed * aTime;
    if TowerAngle <= DesiredTowerAngle then
      fTowerAngle := DesiredTowerAngle;
  end;
end;

procedure TBasicGunTurret.IdleChangeAngle(const aTime: double);
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

procedure TBasicGunTurret.Update(const aTime: double);
begin
  MoveToDesiredAngle(aTime);
  IdleChangeAngle(aTime);
end;

{ TBasicGunTurretType }

function TBasicGunTurretType.GetTextureFilePath: string;
begin
  result := StandardUnitsPath + 'GTurretFnd.png';
end;

function TBasicGunTurretType.GetTopTextureFilePath: string;
begin
  result := StandardUnitsPath + 'GTurretTop.png';
end;

procedure TBasicGunTurretType.Finalize;
begin
  Engine.DisposeTexture(fTopTexture);
end;

procedure TBasicGunTurretType.Load;
begin
  fTexture := Engine.LoadTexture(TextureFilePath);
  fTopTexture := Engine.LoadTexture(TopTextureFilePath);
end;

destructor TBasicGunTurretType.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

end.

