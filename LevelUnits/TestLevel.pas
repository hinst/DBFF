unit TestLevel;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  SysUtils,

  DefaultLevel,
  NiceExceptions,
  LevelDataFace,
  LogEntityFace,
  LevelLoaderFace;

type

  { TTestLevel }

  TTestLevel = class(TDefaultLevel, ILevelLoader)
  private
    function GetTerrainsInfoFilePath: string;
    function GetMasksInfoFilePath: string;
    function GetTerrainMapImageFilePath: string;
  public const
    TerrainMapImageFilePath = '..' + PathDelim + 'data' + PathDelim + 'TestLevel.png';
  public
    procedure Load(const aLevel: ILevelData);
  end;

implementation

uses
  Common;

function TTestLevel.GetTerrainsInfoFilePath: string;
begin
  result := GlobalApplicationPath + StandardTerrainsRelativePath;
end;

function TTestLevel.GetMasksInfoFilePath: string;
begin
  result := GlobalApplicationPath + StandardMasksRelativePath;
end;

function TTestLevel.GetTerrainMapImageFilePath: string;
begin
  result := GlobalApplicationPath + TerrainMapImageFilePath;
end;

procedure TTestLevel.Load(const aLevel: ILevelData);
  procedure LoadTerrains;
  begin
    aLevel.Terrain.LoadTerrains(GetTerrainsInfoFilePath);
    Log.Write(aLevel.Terrain.GetTerrainsInfoAsText);
    aLevel.Terrain.LoadMasks(GetMasksInfoFilePath);
  end;

  procedure LoadTerrainMap;
  begin
    aLevel.LoadTerrainMapFromImageFile(GetTerrainMapImageFilePath);
  end;

begin
  AssertArgumentAssigned(Assigned(aLevel), 'aLevel');
  AssertAssigned(Log, 'Log');
  Log.Write('Loading test level: loading terrains...');
  LoadTerrains;
  Log.Write('Loading test level: loading terrain map...');
  LoadTerrainMap;
  Log.Write('Loading test level: loading basic building types...');
  aLevel.UnitManager.LoadBasicBuildingTypes;
  Log.Write('Loading test level - Done.');
end;

end.

