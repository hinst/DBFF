unit EngineManager;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  IniFiles,

  {$REGION ZenGL units}
  zgl_main,
  zgl_screen,
  zgl_window,
  {$ENDREGION}

  LogEntity,
  NiceExceptions;

type

  { TEngineManager }

  TEngineManager = class(TComponent)
  public
    constructor Create(const aOwner: TComponent); reintroduce;
    procedure Startup(const aConfigFilePath: string);
  private
    fLog: ILog;
    fDraw: TProcedure;
    fLoad: TProcedure;
  public const
    Debug = true;
  public
    property Log: ILog read fLog;
    property Draw: TProcedure read fDraw write fDraw;
    property Load: TProcedure read fLoad write fLoad;
    class function GetConfigFilePath: string;
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

uses
  Common;

{ TEngineManager }

constructor TEngineManager.Create(const aOwner: TComponent);
begin
  inherited Create(aOwner);
  fLog := TLog.Create(GlobalLogManager, 'EngineManager');
end;

procedure TEngineManager.Startup(const aConfigFilePath: string);
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
  if Debug then
    config.Write(Log);
  Log.Write('Now starting up the engine...');
  scr_SetOptions(
    config.ScreenWidth, config.ScreenHeight, REFRESH_DEFAULT, config.FullScreen, config.VSync
  );
  AssertArgumentAssigned(Assigned(Load), 'Load');
  AssertArgumentAssigned(Assigned(Draw), 'Draw');
  zgl_Reg(SYS_LOAD, Load);
  zgl_Reg(SYS_DRAW, Draw);
  wnd_SetCaption(ApplicationTitle);
  Log.Write('Initializing engine...');
  zgl_Init;
end;

class function TEngineManager.GetConfigFilePath: string;
begin
  result := GlobalConfigPath + EngineConfigFileName;
end;

destructor TEngineManager.Destroy;
begin
  if Assigned(Log) then
  begin
    Log.Free;
    fLog := nil;
  end;
  inherited Destroy;
end;

{ TEngineConfig }

procedure TEngineConfig.Read(const aFile: TIniFile);
begin
  AssertArgumentAssigned(aFile, 'aFile');

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
  AssertArgumentAssigned(aFile, 'aFile');
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

