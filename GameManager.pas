unit GameManager;

{$INTERFACES CORBA}

interface

uses
  Classes,
  SysUtils,

  zgl_mouse,
  zgl_keyboard,
  zgl_window,
  zgl_textures_png,
  zgl_textures_tga,
  zgl_render_2d,

  NiceTypes,
  NiceExceptions,
  LogEntityFace,
  LogEntity,
  JobThread,

  Common,
  GameManagerFace,
  ZenGLFCLGraphics,
  EngineManagerFace,
  EngineManager,
  LevelDataFace,
  LevelData,
  LevelLoaderFace,
  TerrainViewer,
  TestLevel,
  UserFaceManager;

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
    fUserFace: TUserFace;
    fLevelActive: boolean;
    fDraw: boolean;
    function GetEngine: IEngineManager;
    function GetLevel: ILevelData;
    procedure Initialize;
    procedure ReceiveDebugKeys;
    procedure UnsafeDraw;
    procedure Finalize;
  public
    property Log: ILog read fLog;
    property JobThread: TJobThread read fJobThread;
    property Engine: TEngineManager read fEngine;
    property Level: TLevelData read fLevel;
    property LevelActive: boolean read fLevelActive write fLevelActive;
    property UserFace: TUserFace read fUserFace;
    procedure StartupEngine;
    procedure Load;
    procedure Draw;
    procedure Update(const aTime: double);
    procedure ReceiveInput(const aTime: double);
    procedure UserLoadLevel(const aLevel: ILevelLoader);
    procedure LoadLevel(const aLevel: ILevelLoader);
    procedure AfterLoadLevel;
    procedure LoadTestLevel;
    destructor Destroy; override;
  end;

implementation

type

  TLoadLevelJob = class(TInterfacedObject, IThreadJob)
  public
    GameManager: TGameManager;
    Loader: ILevelLoader;
    procedure Execute(const aThread: TJobThread);
  end;

procedure GlobalLoad;
begin
  GlobalEngineRunning := true;
  AssertAssigned(GlobalGameManager, 'GlobalGameManager', TVariableType.Global);
  GlobalGameManager.Load;
end;

procedure GlobalDraw;
begin
  AssertAssigned(GlobalGameManager, 'GlobalGameManager', TVariableType.Global);
  GlobalGameManager.Draw;
end;

procedure GlobalUpdate(DT: Double);
begin
  AssertAssigned(GlobalGameManager, 'GlobalGameManager', TVariableType.Global);
  GlobalGameManager.Update(DT);
end;

procedure GlobalFinalize;
begin
  GlobalEngineRunning := false;
end;

{ TLoadLevelJob }

procedure TLoadLevelJob.Execute(const aThread: TJobThread);
begin
  aThread.NotUsedHere;
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
  fUserFace := TUserFace.Create;
  fDraw := true;
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
  if key_Press(K_F2) then
    Engine.TriggerShowFpsActive;
  if key_Press(K_F3) and LevelActive then
    Level.MapView.TerrainView.TriggerDisplayCellBusiness;
end;

procedure TGameManager.UnsafeDraw;
//{$DEFINE DEBUG_THIS_PROCEDURE}
begin
  {$IFDEF DEBUG_THIS_PROCEDURE}
  Log.Write('Now performing engine batch...');
  {$ENDIF}
  Engine.PerformBatch;
  batch2d_Begin;
  if LevelActive then
  begin
    {$IFDEF DEBUG_THIS_PROCEDURE}
    Log.Write('Now drawing level...');
    {$ENDIF}
    Level.Draw;
  end;
  {$IFDEF DEBUG_THIS_PROCEDURE}
  Log.Write('Engine.DrawAfter...');
  {$ENDIF}
  Engine.DrawAfter;
  {$IFDEF DEBUG_THIS_PROCEDURE}
  Log.Write('UserFace.Draw...');
  {$ENDIF}
  UserFace.Draw;
  batch2d_End;
  {$IFDEF DEBUG_THIS_PROCEDURE}
  WriteLN('Drawn.');
  {$ENDIF}
end;
{$UNDEF DEBUG_THIS_PROCEDURE}

procedure TGameManager.Finalize;
begin
  LevelActive := false;
  FreeAndNil(fLevel);
  FreeAndNil(fUserFace);
  FreeAndNil(fEngine);
  FreeAndNil(fJobThread);
  FreeLog(fLog);
end;

procedure TGameManager.Load;
begin
  //LoadTestLevel;
  wnd_ShowCursor(true);
end;

procedure TGameManager.Draw;
begin
  if not fDraw then exit;
  try
    UnsafeDraw;
  except
    on e: Exception do
    begin
      Log.Write(logTagError, 'An error occured while drawing...');
      Log.Write(GetFullExceptionInfo(e));
      fDraw := false;
    end;
  end;
end;

procedure TGameManager.Update(const aTime: double);
begin
  if not GlobalEngineRunning then exit;
  ReceiveInput(aTime);
  if LevelActive then
    Level.Update(aTime);
  UserFace.Update(aTime);
end;

procedure TGameManager.ReceiveInput(const aTime: double);
begin
  ReceiveDebugKeys;
  if LevelActive then
    Level.ReceiveInput(aTime);
  UserFace.ReceiveInput(aTime);
  key_ClearState;
  mouse_ClearState;
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
  AssertAssigned(Assigned(aLevel), 'aLevel', TVariableType.Argument);
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
  // Loading is a success
  AfterLoadLevel;
end;

procedure TGameManager.AfterLoadLevel;
begin
  AssertAssigned(UserFace, 'UserFace', TVariableType.Propertie);
  UserFace.Level := Level;
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

