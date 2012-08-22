unit BuildingUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  zgl_textures,

  NiceExceptions,

  Common,
  BuildingUnitFaceA,
  MapUnit,
  MapUnitFace,
  MapDataFace;

type

  { TBuildingType }

  TBuildingType = class(IAbstractBuildingType)
  protected
    fTexture: zglPTexture;
    procedure Finalize;
  public
    property Texture: zglPTexture read fTexture;
    destructor Destroy; override;
  end;

  TBuildingTypeClass = class of TBuildingType;

  TVehicleFactory = class(TBuildingType)
  end;

  { TBuilding }

  TBuilding = class(TMapUnit)
  public
    constructor Create(const aType: TBuildingType);
  private
    fLeftTopCell: TCellNumber;
    fBuildingType: TBuildingType;
    function GetLeftTopCell: PCellNumber; inline;
    procedure Initialize(const aType: TBuildingType);
  public
    property LeftTopCell: PCellNumber read GetLeftTopCell;
    property BuildingType: TBuildingType read fBuildingType;
    procedure Draw; override;
  end;

implementation

{ TBuildingType }

procedure TBuildingType.Finalize;
begin
  if GlobalEngineRunning then
    GlobalGameManager.Engine.DisposeTexture(fTexture);
end;

destructor TBuildingType.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

{ TBuilding }

constructor TBuilding.Create(const aType: TBuildingType);
begin
  inherited Create;
  Initialize(aType);
end;

function TBuilding.GetLeftTopCell: PCellNumber;
begin
  result := @fLeftTopCell;
end;

procedure TBuilding.Initialize(const aType: TBuildingType);
begin
  fBuildingType := aType;
end;

procedure TBuilding.Draw;
begin
  AssertAssigned(BuildingType, 'BuildingType');
end;

end.

