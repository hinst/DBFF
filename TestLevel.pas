unit TestLevel;

{$mode objfpc}{$H+}

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
  public const
    TestLevelTerrainMap = '..' + PathDelim + 'data' + PathDelim + 'TestLevel.png';
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

procedure TTestLevel.Load(const aLevel: ILevelData);
begin
  aLevel.Terrain.LoadTerrains(GlobalApplicationPath + StandardTerrainsRelativePath);
  if not Assigned(Log) then
    raise EUnassigned.Create('Log');
  Log.Write(aLevel.Terrain.GetTerrainsInfoAsText);
end;

end.

