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
  MapDataFace,
  MapScrollManager,
  BuildingUnit;


type

  { TAbstractTurretType }

  TAbstractTurretType = class(TBuildingType)
  protected
    fTowerTexture: zglPTexture;
    function GetTextureFilePath: string; virtual; abstract;
    function GetTopTextureFilePath: string; virtual; abstract;
    procedure Finalize;
  public
    property TextureFilePath: string read GetTextureFilePath;
    property TopTextureFilePath: string read GetTopTextureFilePath;
    property TowerTexture: zglPTexture read fTowerTexture;
    procedure Load;
    destructor Destroy; override;
  end;

  TAbstractTurret = class(TBuilding, IMapUnit)
  public
    constructor Create(const aType: TBuildingType); override;
  protected
    fTowerAngle: TAngle360;
    fDesiredTowerAngle: TAngle360;
    fIdleChangeAngleTimeLeft: single;
    function GetMyType: TAbstractTurretType;
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
    property TowerAngle: TAngle360 read fTowerAngle;
    property DesiredTowerAngle: TAngle360 read fDesiredTowerAngle;
    property IdleChangeAngleTimeLeft: single read fIdleChangeAngleTimeLeft;
    property MyType: TAbstractTurretType read GetMyType;
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
  fTexture := Engine.LoadTexture(TextureFilePath);
  fTowerTexture := Engine.LoadTexture(TopTextureFilePath);
end;

destructor TAbstractTurretType.Destroy;
begin
  Finalize;
  inherited Destroy;
end;


{TAbstractTurret}

constructor TAbstractTurret.Create(const aType: TBuildingType);
begin
  inherited Create(aType);
  Initialize;
end;

function TAbstractTurret.GetMyType: TAbstractTurretType;
begin
  result := BuildingType as TAbstractTurretType;
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
  fTowerAngle := random(360);
  fDesiredTowerAngle := random(360);
  fIdleChangeAngleTimeLeft := IdleChangeAngleTime;
end;

procedure TAbstractTurret.SureDraw(const aScroll: TMapScrollManager);

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
    ssprite2d_Draw(MyType.TowerTexture,
      DrawX,
      DrawY,
      TileWidth,
      TileHeight,
      TowerAngle
    );
  end;
end;

procedure TAbstractTurret.MoveToDesiredAngle(const aTime: double);
var
  d1, d2: shortint;
begin
  if TowerAngle = DesiredTowerAngle then exit;
  d1 := MostCloseAngleDirection(TowerAngle, DesiredTowerAngle);
  fTowerAngle.Inc( TowerSpeed * aTime * d1 );
  d2 := MostCloseAngleDirection(TowerAngle, DesiredTowerAngle);
  if d1 <> d2 then
    fTowerAngle := DesiredTowerAngle;
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

procedure TAbstractTurret.Update(const aTime: double);
begin
  MoveToDesiredAngle(aTime);
  IdleChangeAngle(aTime);
end;

end.

