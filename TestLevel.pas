unit TestLevel;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  SysUtils,
  NiceExceptions,
  LevelDataFace,
  LogEntityFace,
  LevelLoaderFace;

type

  { TTestLevel }

  TTestLevel = class(TInterfacedObject, ILevelLoader)
  private
    fLog: ILog;
    procedure SetLog(const aLog: ILog);
    function GetTerrainsInfoFilePath: string;
    function GetMasksInfoFilePath: string;
    function GetTerrainMapImageFilePath: string;
  public const
    TerrainMapImageFilePath = '..' + PathDelim + 'data' + PathDelim + 'TestLevel.png';
  public
    procedure Load(const aLevel: ILevelData);
    property Log: ILog read fLog;
  end;

implementation

uses
  Common;

{ TTestLevel }

procedure TTestLevel.SetLog(const aLog: ILog);
begin
  fLog := aLog;
end;

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
  AssertAssigned(Assigned(Log), 'Log');
  LoadTerrains;
  LoadTerrainMap;
end;

end.

