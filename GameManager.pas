unit GameManager;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  LogEntityFace,
  LogEntity,
  NiceExceptions,

  EngineManager,
  LevelDataContainer,
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
    procedure Finalize;
  public
    property Log: ILog read fLog;
    property EngineMan: TEngineManager read fEngineMan;
    property Level: TLevelData read fLevel;
    procedure Load;
    procedure Draw;
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

{ TGameManager }

constructor TGameManager.Create(const aOwner: TComponent);
begin
  inherited Create(aOwner);
  fLog := TLog.Create(GlobalLogManager, 'GameManager');
end;

procedure TGameManager.StartupEngine;
begin
  fEngineMan := TEngineManager.Create(self);
  EngineMan.Draw := @GlobalDraw;
  EngineMan.Load := @GlobalLoad;
  EngineMan.Startup(TEngineManager.GetConfigFilePath);
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
  {$REGION 'Launch the test level'}
  {$ENDREGION}
end;

procedure TGameManager.Draw;
begin

end;

destructor TGameManager.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

end.

