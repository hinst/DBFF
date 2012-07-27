unit LevelDataFace;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  MapDataFace,
  TerrainManagerFace;

type
  ILevelData = interface
    function GetMap: IMapData;
    property Map: IMapData read GetMap;
    function GetTerrain: ITerrainManager;
    property Terrain: ITerrainManager read GetTerrain;
    procedure DoSomeShit;
    procedure LoadTerrainMapFromImageFile(const aFileName: string);
  end;

implementation

end.

