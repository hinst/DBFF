unit BasicVehicleFactoryUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  zgl_textures,
  zgl_sprite_2d,

  NiceExceptions,

  Common,
  MapUnitFace,
  UnitFactoryFace,
  MapUnit,
  MapDataFace,
  MapScrollManager,
  BuildingUnit,
  UnitProduction,
  EngineManagerFace;

type

  { TBasicVehicleFactoryType }

  TBasicVehicleFactoryType = class(TBuildingType)
  protected
    fTopTexture: zglPTexture;
    function GetTextureFilePath: string; override;
    function GetTopTextureFilePath: string;
    procedure Finalize;
  public
    property HatTexture: zglPTexture read fTopTexture;
    property TopTextureFilePath: string read GetTopTextureFilePath;
    procedure Load; override;
    destructor Destroy; override;
  end;

  TBasicVehicleFactory = class(TBuilding, IMapUnit, IUnitFactory)
  public
    constructor Create(const aType: TMapUnitType); override;
  protected
    fHatAngle: single;
    fProduction: TUnitProduction;
    function GetMyType: TBasicVehicleFactoryType;
    function GetOccupatedCells: TCellNumbers;
    function GetUnitWidth: integer; override;
    function GetUnitHeight: integer; override;
    function GetProduction: TUnitProduction;
    function IsVehicleFactory: boolean;
    procedure Initialize;
    procedure Finalize;
  public const
      // one rotation per two seconds is 360 / 2000
    HatSpeed = 360 / 2000;
  public
    property HatAngle: single read fHatAngle;
    property Production: TUnitProduction read fProduction;
    property MyType: TBasicVehicleFactoryType read GetMyType;
    procedure Draw(const aScroll: TMapScrollManager);
    procedure DrawTopLayer(const aScroll: TMapScrollManager);
    procedure Update(const aTime: double);
    destructor Destroy; override;
  end;

implementation

constructor TBasicVehicleFactory.Create(const aType: TMapUnitType);
begin
  inherited Create(aType);
  Initialize;
end;

function TBasicVehicleFactory.GetMyType: TBasicVehicleFactoryType;
begin
  result := UnitType as TBasicVehicleFactoryType;
end;

function TBasicVehicleFactory.GetOccupatedCells: TCellNumbers;
begin
  SetLength(result, 6);
  result[0].Assign(fLeftTopCell);
  result[1].X := fLeftTopCell.X + 1;
  result[1].Y := fLeftTopCell.Y;
  result[2].X := fLeftTopCell.X + 2;
  result[2].Y := fLeftTopCell.Y;

  result[3].X := fLeftTopCell.X;
  result[3].Y := fLeftTopCell.Y + 1;
  result[4].X := fLeftTopCell.X + 1;
  result[4].Y := fLeftTopCell.Y + 1;
  result[5].X := fLeftTopCell.X + 2;
  result[5].Y := fLeftTopCell.Y + 1;
end;

function TBasicVehicleFactory.GetUnitWidth: integer;
begin
  result := 3;
end;

function TBasicVehicleFactory.GetUnitHeight: integer;
begin
  result := 2;
end;

function TBasicVehicleFactory.GetProduction: TUnitProduction;
begin
  result := fProduction;
end;

function TBasicVehicleFactory.IsVehicleFactory: boolean;
begin
  result := true;
end;

procedure TBasicVehicleFactory.Initialize;
begin
  fHatAngle := random(360);
  fProduction := TUnitProduction.Create(self);
end;

procedure TBasicVehicleFactory.Finalize;
begin
  FreeAndNil(fProduction);
end;

procedure TBasicVehicleFactory.Draw(const aScroll: TMapScrollManager);
begin
  if not IsVisible(aScroll) then exit;
  with aScroll do
  begin
    ssprite2d_Draw(
      MyType.Texture,
      aScroll.ScreenX(LeftTopCell^),
      aScroll.ScreenY(LeftTopCell^),
      TileWidth * 3,
      TileHeight * 2,
      0);
    ssprite2d_Draw(
      MyType.HatTexture,
      aScroll.ScreenX(LeftTopCell^) + TileWidth,
      aScroll.ScreenY(LeftTopCell^) + TileHeight / 2,
      TileWidth,
      TileHeight,
      HatAngle
    );
  end;
end;

procedure TBasicVehicleFactory.DrawTopLayer(const aScroll: TMapScrollManager);
begin
  Production.Draw(aScroll);
end;

procedure TBasicVehicleFactory.Update(const aTime: double);
begin
  fHatAngle += HatSpeed * aTime;
  Production.Update(aTime);
end;

destructor TBasicVehicleFactory.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

{ TBasicVehicleFactoryType }

function TBasicVehicleFactoryType.GetTextureFilePath: string;
begin
  result := StandardUnitsPath + 'Building_sample3.png';
end;

function TBasicVehicleFactoryType.GetTopTextureFilePath: string;
begin
  result := StandardUnitsPath + 'BuildingHat1.png';
end;

procedure TBasicVehicleFactoryType.Finalize;
begin
  Engine.DisposeTexture(fTopTexture);
end;

procedure TBasicVehicleFactoryType.Load;
begin
  inherited Load;
  fTopTexture := Engine.LoadTexture(TopTextureFilePath);
end;

destructor TBasicVehicleFactoryType.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

end.

