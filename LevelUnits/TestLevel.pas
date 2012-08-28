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
  Log.Write('Loading test level: loading basic vehicle types');
  aLevel.UnitManager.LoadBasicVehicleTypes;
  Log.Write('Loading test level: Creating basic vehicle factory...');
  aLevel.UnitManager.AddBasicVehicleFactory(7, 0);
  aLevel.UnitManager.AddBasicVehicleFactory(3, 4);
  Log.Write('Loading test level: Creating basic gun turret...');
  aLevel.UnitManager.AddBasicGunTurret(6, 3);
  aLevel.UnitManager.AddBasicGunTurret(3, 6);
  aLevel.UnitManager.AddBasicGunTurret(3, 7);
  aLevel.UnitManager.AddBasicGunTurret(6, 0);
  Log.Write('Loading test level: Creating basic single turret...');
  aLevel.UnitManager.AddBasicSingleTurret(6, 1);
  aLevel.UnitManager.AddBasicSingleTurret(8, 2);
  aLevel.UnitManager.AddBasicSingleTurret(9, 2);
  Log.Write('Loading test level: Creating basic tanks...');
  aLevel.UnitManager.AddBasicTank(1, 1);
  Log.Write('Loading test level - Done.');
end;

end.

