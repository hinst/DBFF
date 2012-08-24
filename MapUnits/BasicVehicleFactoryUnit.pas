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
  MapDataFace,
  MapScrollManager,
  BuildingUnit,
  EngineManagerFace;

type

  { TBasicVehicleFactoryType }

  TBasicVehicleFactoryType = class(TBuildingType)
  private
    fTopTexture: zglPTexture;
    function GetTextureFilePath: string;
    function GetTopTextureFilePath: string;
    procedure Finalize;
  public
    property HatTexture: zglPTexture read fTopTexture;
    property TextureFilePath: string read GetTextureFilePath;
    property TopTextureFilePath: string read GetTopTextureFilePath;
    procedure Load;
    destructor Destroy; override;
  end;

  TBasicVehicleFactory = class(TBuilding, IMapUnit)
  public
    constructor Create(const aType: TBuildingType); override;
  protected
    fHatAngle: single;
    function GetMyType: TBasicVehicleFactoryType;
    function GetOccupatedCells: TCellNumbers;
    function GetUnitWidth: integer; override;
    function GetUnitHeight: integer; override;
    procedure Initialize;
    procedure SureDraw(const aScroll: TMapScrollManager); override;
  public const
      // two rotations per two seconds is 360 / 2000
    HatSpeed = 360 / 2000;
  public
    property HatAngle: single read fHatAngle;
    property MyType: TBasicVehicleFactoryType read GetMyType;
    procedure Update(const aTime: double);
  end;

implementation

constructor TBasicVehicleFactory.Create(const aType: TBuildingType);
begin
  inherited Create(aType);
  Initialize;
end;

function TBasicVehicleFactory.GetMyType: TBasicVehicleFactoryType;
begin
  result := BuildingType as TBasicVehicleFactoryType;
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

procedure TBasicVehicleFactory.Initialize;
begin
  fHatAngle := random(360);
end;

procedure TBasicVehicleFactory.SureDraw(const aScroll: TMapScrollManager);

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
      TileWidth * 3,
      TileHeight * 2,
      0);
    ssprite2d_Draw(MyType.HatTexture,
      DrawX + TileWidth,
      DrawY + TileHeight / 2,
      TileWidth,
      TileHeight,
      HatAngle
    );
  end;
end;

procedure TBasicVehicleFactory.Update(const aTime: double);
begin
  fHatAngle += HatSpeed * aTime;
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
  fTexture := Engine.LoadTexture(TextureFilePath);
  fTopTexture := Engine.LoadTexture(TopTextureFilePath);
end;

destructor TBasicVehicleFactoryType.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

end.

