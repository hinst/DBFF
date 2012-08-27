unit BuildingUnit;

{$mode objfpc}{$H+}
//{$DEFINE DEBUG_LOG_VISIBILITY}

interface

uses
  Classes,
  SysUtils,

  zgl_textures,

  NiceExceptions,

  Common,
  BuildingUnitFaceA,
  EngineManagerFace,
  MapUnit,
  MapUnitFace,
  MapDataFace,
  MapScrollManager;

type

  { TBuildingType }

  TBuildingType = class(TMapUnitType)
  end;

  TBuildingTypeClass = class of TBuildingType;

  { TBuilding }

  TBuilding = class(TMapUnit)
  end;

  TBuildingClass = class of TBuilding;

implementation

{ TBuilding }

end.

