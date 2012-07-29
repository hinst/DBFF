unit GameManager;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  zgl_main,
  zgl_mouse,
  zgl_keyboard,

  LogEntityFace,
  LogEntity,
  NiceExceptions,

  EngineManager,
  LevelDataContainer,
  LevelLoaderFace,
  MapViewer,
  TestLevel;

type

  { TGameManager }

  TGameManager = class(TComponent)
  public
    constructor Create(const aOwner: TComponent); reintroduce;
    procedure StartupEngine;
  private
    fLog: ILog;
    fEngineMan: TEngineManager;
    fLevel: TLevelData;
    procedure Initialize;
    procedure Finalize;
  public
    property Log: ILog read fLog;
    property EngineMan: TEngineManager read fEngineMan;
    property Level: TLevelData read fLevel;
    procedure Load;
    procedure Draw;
    procedure Update(const aTime: double);
    procedure LoadLevel(const aLevel: ILevelLoader);
    procedure LoadTestLevel;
    destructor Destroy; override;
  end;

implementation

uses
  Common;

procedure GlobalLoad;
begin
  AssertArgumentAssigned(GlobalGameManager, 'GlobalGameManager');
  GlobalGameManager.Load;
end;

procedure GlobalDraw;
begin
  AssertArgumentAssigned(GlobalGameManager, 'GlobalGameManager');
  GlobalGameManager.Draw;
end;

procedure GlobalUpdate(DT: Double);
begin
  AssertArgumentAssigned(GlobalGameManager, 'GlobalGameManager');
  GlobalGameManager.Update(DT);
end;

{ TGameManager }

constructor TGameManager.Create(const aOwner: TComponent);
begin
  inherited Create(aOwner);
  Initialize;
end;

procedure TGameManager.StartupEngine;
begin
  EngineMan.Draw := @GlobalDraw;
  EngineMan.Load := @GlobalLoad;
  EngineMan.Update := @GlobalUpdate;
  EngineMan.Startup(TEngineManager.GetConfigFilePath);
end;

procedure TGameManager.Initialize;
begin
  fLog := TLog.Create(GlobalLogManager, 'GameManager');
  fEngineMan := TEngineManager.Create(self);
end;

procedure TGameManager.Finalize;
begin
  if Assigned(Log) then
  begin
    Log.Free;
    fLog := nil;
  end;
  if Assigned(EngineMan) then
    FreeAndNil(fEngineMan);
end;

procedure TGameManager.Load;
begin
  LoadTestLevel;
end;

procedure TGameManager.Draw;
begin
  Level.Draw;
end;

procedure TGameManager.Update(const aTime: double);
begin
  Level.ReceiveInput(aTime);
end;

procedure TGameManager.LoadLevel(const aLevel: ILevelLoader);
var
  levelLog: ILog;
begin
  AssertArgumentAssigned(Assigned(aLevel), 'aLevel');
  if Assigned(Level) then
  begin
    Log.Write('Releasing existing level data...');
    FreeAndNil(fLevel);
  end;
  fLevel := TLevelData.Create(self);
  try
    try
      levelLog := TLog.Create(GlobalLogManager, 'LevelLoader');
      aLevel.Log := levelLog;
      aLevel.Load(Level);
    finally
      levelLog.Free;
    end;
  except
    on E: Exception do
    begin
      Log.Write(logTagError, 'Activating level loader caused an exception.');
      raise;
    end;
  end;
end;

procedure TGameManager.LoadTestLevel;
var
  loader: TTestLevel;
begin
  Log.Write('Now loading test level...');
  try
    loader := TTestLevel.Create;
    LoadLevel(loader);
  finally
    loader.Free;
  end;
end;

destructor TGameManager.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

end.

