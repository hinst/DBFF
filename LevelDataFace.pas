unit LevelDataFace;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  MapDataFace,
  TerrainManagerFace,
  UnitManagerFace;

type
  ILevelData = interface
    function GetMap: IMapData;
    property Map: IMapData read GetMap;
    function GetTerrain: ITerrainManager;
    property Terrain: ITerrainManager read GetTerrain;
    function GetUnitManager: IUnitManager;
    property UnitManager: IUnitManager read GetUnitManager;
    procedure LoadTerrainMapFromImageFile(const aFileName: string);
  end;

implementation

end.

