unit GameManager;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

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
  JobThread,
  SynchroThread,

  GameManagerFace,
  ZenGLFCLGraphics,
  EngineManagerFace,
  EngineManager,
  LevelDataFace,
  LevelData,
  LevelLoaderFace,
  TerrainViewer,
  TestLevel;

type

  { TGameManager }

  TGameManager = class(TComponent, IGameManager)
  public
    constructor Create(const aOwner: TComponent); reintroduce;
  private
    fLog: ILog;
    fJobThread: TJobThread;
    fEngine: TEngineManager;
    fLevel: TLevelData;
    fLevelActive: boolean;
    function GetEngine: IEngineManager;
    function GetLevel: ILevelData;
    procedure Initialize;
    procedure ReceiveDebugKeys;
    procedure Finalize;
  public
    property Log: ILog read fLog;
    property JobThread: TJobThread read fJobThread;
    property Engine: TEngineManager read fEngine;
    property Level: TLevelData read fLevel;
    property LevelActive: boolean read fLevelActive write fLevelActive;
    procedure StartupEngine;
    procedure Load;
    procedure Draw;
    procedure Update(const aTime: double);
    procedure ReceiveInput(const aTime: double);
    procedure UserLoadLevel(const aLevel: ILevelLoader);
    procedure LoadLevel(const aLevel: ILevelLoader);
    procedure LoadTestLevel;
    destructor Destroy; override;
  end;

implementation

uses
  Common;

type

  { TLoadLevelJob }

  TLoadLevelJob = class(TInterfacedObject, IThreadJob)
  public
    GameManager: TGameManager;
    Loader: ILevelLoader;
    procedure Execute(const aThread: TJobThread);
  end;

procedure GlobalLoad;
begin
  GlobalEngineRunning := true;
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

procedure GlobalFinalize;
begin
  GlobalEngineRunning := false;
end;

{ TLoadLevelJob }

procedure TLoadLevelJob.Execute(const aThread: TJobThread);
begin
  try
    try
      GameManager.LoadLevel(Loader);
    finally
      Loader.Free;
    end;
  except
    on e: Exception do
    begin
      GameManager.Log.Write(logTagError, 'An error occured while executing load level job');
      GameManager.Log.Write(GetFullExceptionInfo(e));
    end;
  end;
end;

{ TGameManager }

constructor TGameManager.Create(const aOwner: TComponent);
begin
  inherited Create(aOwner);
  Initialize;
end;

function TGameManager.GetEngine: IEngineManager;
begin
  result := fEngine;
end;

function TGameManager.GetLevel: ILevelData;
begin
  result := fLevel;
end;

procedure TGameManager.Initialize;
begin
  fLog := TLog.Create(GlobalLogManager, 'GameManager');
  fJobThread := TJobThread.Create;
  JobThread.Start;
  LevelActive := false;
  fEngine := TEngineManager.Create(self);
end;

procedure TGameManager.StartupEngine;
begin
  Engine.Draw := @GlobalDraw;
  Engine.Load := @GlobalLoad;
  Engine.Update := @GlobalUpdate;
  Engine.OnEngineFinalize := @GlobalFinalize;
  Engine.Startup;
end;

procedure TGameManager.ReceiveDebugKeys;
begin
  if key_Press(K_F1) then
    LoadTestLevel;
end;

procedure TGameManager.Finalize;
begin
  LevelActive := false;
  FreeAndNil(fEngine);
  FreeAndNil(fJobThread);
  FreeLog(fLog);
end;

procedure TGameManager.Load;
begin
  //LoadTestLevel;
end;

procedure TGameManager.Draw;
begin
  Engine.PerformBatch;
  if LevelActive then
    Level.Draw;
end;

procedure TGameManager.Update(const aTime: double);
begin
  if not GlobalEngineRunning then exit;
  ReceiveInput(aTime);
end;

procedure TGameManager.ReceiveInput(const aTime: double);
begin
  ReceiveDebugKeys;
  if LevelActive then
    Level.ReceiveInput(aTime);

  key_ClearState;
end;

procedure TGameManager.UserLoadLevel(const aLevel: ILevelLoader);
var
  job: TLoadLevelJob;
begin
  job := TLoadLevelJob.Create;
  job.GameManager := self;
  job.Loader := aLevel;
  JobThread.Add(job);
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
    levelLog := TLog.Create(GlobalLogManager, 'LevelLoader');
    aLevel.Log := levelLog;
    aLevel.Load(Level);
    LevelActive := true;
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
  loader := TTestLevel.Create;
  UserLoadLevel(loader);
end;

destructor TGameManager.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

end.

