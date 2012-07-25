unit LevelDataContainer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LevelDataFace,
  MapDataFace,
  MapDataContainer,
  TerrainManagerFace,
  TerrainManager;

type

  { TLevelData }

  TLevelData = class(TComponent, ILevelData)
  public
    constructor Create(const aOwner: TComponent); reintroduce;
  private
    fMap: TMapData;
    fTerrain: TTerrainManager;
    procedure Initialize;
    function GetMap: IMapData;
    function GetTerrain: ITerrainManager;
    procedure Finalize;
  public
    property Map: TMapData read fMap;
    property Terrain: TTerrainManager read fTerrain;
  end;

implementation

{ TLevelData }

constructor TLevelData.Create(const aOwner: TComponent);
begin
  inherited Create(aOwner);
  Initialize;
end;

procedure TLevelData.Initialize;
begin
  fTerrain := TTerrainManager.Create(self);
  fMap := TMapData.Create(self);
end;

function TLevelData.GetMap: IMapData;
begin
  result := fMap;
end;

function TLevelData.GetTerrain: ITerrainManager;
begin
  result := fTerrain;
end;

procedure TLevelData.Finalize;
begin
  if Assigned(Map) then
    FreeAndNil(fMap);
  if Assigned(Terrain) then
    FreeAndNil(fTerrain);
end;

end.

