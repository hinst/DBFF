unit EngineManager;

{$mode objfpc}{$H+}

{$UNDEF DEBUG_BATCH}
{$DEFINE DEBUG_WRITE_ENGINE_CONFIG}
{$DEFINE DEBUG_WRITE_ON_LOADTEXTURE}

interface

uses
  Classes,
  SysUtils,
  IniFiles,

  {$REGION ZenGL units}
  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_textures,
  zgl_render_target,
  zgl_fx,
  zgl_primitives_2d,
  zgl_sprite_2d,
  zgl_font,
  {$ENDREGION}

  NiceTypes,
  NiceExceptions,
  SynchroThread,
  LogEntityFace,
  LogEntity,

  Common,
  EngineManagerFace,
  ZenGLFCLGraphics,
  ZenGLShowMeFPS;

type

  { TEngineManager }

  TEngineManager = class(TComponent, IEngineManager)
  public
    constructor Create(const aOwner: TComponent); reintroduce;
      // also loads configuration from the configuration file
    procedure Initialize;
    procedure Startup;
    procedure Finalize;
  public type
    TUpdateProcedure = procedure(DT: Double);
  private
    fLog: ILog;
    fDraw: TProcedure;
    fLoad: TProcedure;
    fUpdate: TUpdateProcedure;
    fOnEngineFinalize: TProcedure;
    fBatch: TSynchroThread;
    fShowFpsActive: boolean;
    fFpsDrawer: TFPSDrawer;
    function GetBatch: TSynchroThread;
    procedure SetShowFpsActive(const aValue: boolean);
  public
    property Log: ILog read fLog;
    property Draw: TProcedure read fDraw write fDraw;
    property Load: TProcedure read fLoad write fLoad;
    property Update: TUpdateProcedure read fUpdate write fUpdate;
    property OnEngineFinalize: TProcedure read fOnEngineFinalize write fOnEngineFinalize;
    property Batch: TSynchroThread read fBatch;
    property FPSDrawer: TFPSDrawer read fFpsDrawer;
    property ShowFpsActive: boolean read fShowFpsActive write SetShowFpsActive;
    procedure TriggerShowFpsActive;
    procedure DrawAfter;
    class function GetConfigFilePath: string;
    function PerformBatch: boolean;
    function LoadTexture(const aFileName: string): zglPTexture;
    function LoadFont(const aFileName: string): zglPFont;
    function DirectCopyTexture(const aTexture: zglPTexture): zglPTexture;
    procedure DirectCleanTexture(const aTexture: zglPRenderTarget);
    function CreateRenderTarget(const aWidth, aHeight: integer): zglPRenderTarget;
    procedure DisposeTexture(var aTexture: zglPTexture);
    destructor Destroy; override;
  end;

  { TEngineConfig }

  TEngineConfig = object
  public
    ScreenWidth: integer;
    ScreenHeight: integer;
    FullScreen: boolean;
    VSync: boolean;
  public const
    DefaultScreenWidth = 1024;
    DefaultScreenHeight = 768;
    DefaultFullScreen = false;
    DefaultVSync = true;
  public const
    Section = 'EngineConfig';
    ScreenWidthIdent = 'Screen.Width';
    ScreenHeightIdent = 'Screen.Height';
    FullScreenIdent = 'Screen.FullScreen';
    VSyncIdent = 'Screen.VSync';
  public type
    EConfigInvalid = class(Exception);
    EUnspecifiedOption = class(EConfigInvalid);
  public
    procedure Read(const aFile: TIniFile);
    procedure SetDefaults;
    procedure Write(const aFile: TIniFile);
    procedure Write(const aLog: ILog);
  end;

implementation

{ TEngineManager }

constructor TEngineManager.Create(const aOwner: TComponent);
begin
  inherited Create(aOwner);
  Initialize;
end;

procedure TEngineManager.Initialize;
begin
  fLog := TLog.Create(GlobalLogManager, 'EngineManager');
  fBatch := TSynchroThread.Create(self);
  {$IFDEF DEBUG_BATCH}
  Batch.EnableDebug(TLog.Create(GlobalLogManager, 'Batch'));
  {$ENDIF}
end;

procedure TEngineManager.Startup;
var
  configFilePath: string;
  configFile: TIniFile;
  config: TEngineConfig;
begin
  configFilePath := GetConfigFilePath;
  Log.Write('Engine config file is: "' + configFilePath + '"');
  configFile := TIniFile.Create(configFilePath);
  config.Read(configFile);
  configFile.Free;
  {$IFDEF DEBUG_WRITE_ENGINE_CONFIG}
  config.Write(Log);
  {$ENDIF}
  Log.Write('Now starting up the engine...');
  scr_SetOptions(
    config.ScreenWidth, config.ScreenHeight, REFRESH_DEFAULT, config.FullScreen, config.VSync
  );
  // I dout if every of these procedures should be strictly required
  // Maybe it is better just not to register the handler if it is not assigned
  AssertAssigned(Load, 'Load', TVariableType.Propertie);
  AssertAssigned(Draw, 'Draw', TVariableType.Propertie);
  AssertAssigned(Update,'Update', TVariableType.Propertie);
  zgl_Reg(SYS_LOAD, Load);
  zgl_Reg(SYS_DRAW, Draw);
  zgl_Reg(SYS_UPDATE, Update);
  zgl_Reg(SYS_EXIT, OnEngineFinalize);
  wnd_SetCaption(ApplicationTitle);
  Log.Write('Initializing engine...');
  zgl_Init;
end;

procedure TEngineManager.Finalize;
begin
  FreeAndNil(fFpsDrawer);
  FreeAndNil(fBatch);
  FreeLog(fLog);
end;

function TEngineManager.GetBatch: TSynchroThread;
begin
  result := Batch;
end;

procedure TEngineManager.SetShowFpsActive(const aValue: boolean);
begin
  if aValue = true then
  begin
    if fFpsDrawer = nil then
    begin
      fFpsDrawer := TFPSDrawer.Create;
      FPSDrawer.Font := LoadFont(GlobalApplicationPath + FpsFontRelativePath);
    end;
  end;
  fShowFpsActive := aValue;
end;

procedure TEngineManager.TriggerShowFpsActive;
begin
  ShowFpsActive := not ShowFpsActive;
end;

procedure TEngineManager.DrawAfter;
begin
  if ShowFpsActive then
    FPSDrawer.Draw;
end;

class function TEngineManager.GetConfigFilePath: string;
begin
  result := GlobalConfigPath + EngineConfigFileName;
end;

function TEngineManager.PerformBatch: boolean;
begin
  result := Batch.ExecuteOne;
end;

function TEngineManager.LoadTexture(const aFileName: string): zglPTexture;
var
  job: TLoadTexture;
begin
  {$IFDEF DEBUG_WRITE_ON_LOADTEXTURE}
  Log.Write('Loading texture "' + aFileName + '"...');
  {$ENDIF}
  job := TLoadTexture.Create(aFileName);
  Batch.Execute(job);
  result := job.Result;
end;

function TEngineManager.LoadFont(const aFileName: string): zglPFont;
var
  job: TLoadFont;
begin
  Log.Write('Loading font "' + aFileName + '"...');
  job := TLoadFont.Create(aFileName);
  Batch.Execute(job);
  result := job.Result;
end;

function TEngineManager.DirectCopyTexture(const aTexture: zglPTexture
  ): zglPTexture;
var
  target: zglPRenderTarget;
begin
  target := rtarget_Add(tex_CreateZero(aTexture^.Width, aTexture^.Height), RT_DEFAULT);
  rtarget_Set(target);
  ssprite2d_Draw(aTexture, 0, 0, aTexture^.Width, aTexture^.Height, 0);
  rtarget_Set(nil);
  result := target^.Surface;
  target^.Surface := nil;
  rtarget_Del(target);
end;

procedure TEngineManager.DirectCleanTexture(const aTexture: zglPRenderTarget);
begin
  fx_SetBlendMode(FX_BLEND_MULT, FALSE);
  pr2d_Rect(
    0, 0, aTexture^.Surface^.Width, aTexture^.Surface^.Height, // XYWH
    0, 0, // color, alpha
    PR2D_FILL
  );
  fx_SetBlendMode(FX_BLEND_NORMAL);
end;

function TEngineManager.CreateRenderTarget(const aWidth, aHeight: integer
  ): zglPRenderTarget;
var
  job: TCreateRenderTarget;
begin
  job := TCreateRenderTarget.Create(aWidth, aHeight);
  Batch.Execute(job);
  result := job.Result;
end;

procedure TEngineManager.DisposeTexture(var aTexture: zglPTexture);
var
  job: ISynchroJob;
begin
  if aTexture = nil then exit;
  if GlobalEngineRunning then
  begin
    job := TDisposeTexture.Create(aTexture);
    Batch.Execute(job);
  end;
  aTexture := nil;
end;

destructor TEngineManager.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

{ TEngineConfig }

procedure TEngineConfig.Read(const aFile: TIniFile);
begin
  AssertAssigned(aFile, 'aFile', TVariableType.Argument);

  ScreenWidth := aFile.ReadInteger(Section, ScreenWidthIdent, -1);
  if ScreenWidth = -1 then
    raise EUnspecifiedOption.Create(ScreenWidthIdent);

  ScreenHeight := aFile.ReadInteger(Section, 'Screen.Height', -1);
  if ScreenHeight = -1 then
    raise EUnspecifiedOption.Create(ScreenHeightIdent);

  if not aFile.ValueExists(Section, FullScreenIdent) then
    raise EUnspecifiedOption.Create(FullScreenIdent);
  FullScreen := aFile.ReadBool(Section, FullScreenIdent, false);

  if not aFile.ValueExists(Section, VSyncIdent) then
    raise EUnspecifiedOption.Create(VSyncIdent);
  VSync := aFile.ReadBool(Section, VSyncIdent, false);
end;

procedure TEngineConfig.SetDefaults;
begin
  ScreenWidth := DefaultScreenWidth;
  ScreenHeight := DefaultScreenHeight;
  FullScreen := DefaultFullScreen;
  VSync := DefaultVSync;
end;

procedure TEngineConfig.Write(const aFile: TIniFile);
begin
  AssertAssigned(aFile, 'aFile', TVariableType.Argument);
  aFile.WriteInteger(Section, ScreenWidthIdent, ScreenWidth);
  aFile.WriteInteger(Section, ScreenHeightIdent, ScreenHeight);
  aFile.WriteBool(Section, FullScreenIdent, FullScreen);
  aFile.WriteBool(Section, VSyncIdent, VSync);
end;

procedure TEngineConfig.Write(const aLog: ILog);
  procedure W(const aIdent, aValue: string);
  begin
    aLog.Write('EC: ' + aIdent + ' = ' + aValue + ';');
    // EC stands for Engine Config
  end;
begin
  W(ScreenWidthIdent, IntToStr(ScreenWidth));
  W(ScreenHeightIdent, IntToStr(ScreenHeight));
  W(FullScreenIdent, BoolToStr(FullScreen, true));
  W(VSyncIdent, BoolToStr(VSync, true));
end;

end.

