unit LevelDataFace;

{$mode objfpc}{$H+}

interface

uses
  MapDataFace,
  TerrainManagerFace;

type
  ILevelData = interface
    function GetMap: IMapData;
    function GetTerrain: ITerrainManager;
    property Terrain: ITerrainManager read GetTerrain;
    property Map: IMapData read GetMap;
  end;

implementation

end.

