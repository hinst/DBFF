unit BuildingUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  zgl_textures,

  BuildingUnitFaceA,
  MapUnit,
  MapUnitFace,
  MapDataFace;

type

  TBuildingType = class(IAbstractBuildingType)
  protected
    fTexture: zglPTexture;
  public
    property Texture: zglPTexture read fTexture;
  end;

  TVehicleFactory = class(TBuildingType)
  end;

  { TBuilding }

  TBuilding = class(TMapUnit)
  private
    fLeftTopCell: TCellNumber;
    fBuildingType: TBuildingType;
    function GetLeftTopCell: PCellNumber; inline;
  public
    property LeftTopCell: PCellNumber read GetLeftTopCell;
  end;

implementation

{ TBuilding }

function TBuilding.GetLeftTopCell: PCellNumber;
begin
  result := @fLeftTopCell;
end;

end.

