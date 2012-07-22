unit GameManager;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  LogEntity,

  EngineManager;

type

  { TGameManager }

  TGameManager = class(TComponent)
  public
    constructor Create(const aOwner: TComponent); reintroduce;
    procedure StartupEngine;
  private
    fEngineMan: TEngineManager;
    fLog: ILog;
    procedure Finalize;
  public
    property EngineMan: TEngineManager read fEngineMan;
    property Log: ILog read fLog;
    procedure Load;
    procedure Draw;
    destructor Destroy; override;
  end;

implementation

uses
  Common;

procedure GlobalLoad;
begin
  GlobalGameManager.Load;
end;

procedure GlobalDraw;
begin
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

