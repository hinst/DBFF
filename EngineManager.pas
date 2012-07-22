unit EngineManager;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  IniFiles,

  NiceExceptions,

  zgl_main,
  zgl_screen,
  zgl_window,

  Common;

type

  { TEngineManager }

  TEngineManager = class(TComponent)
  public
    constructor Create(const aOwner: TComponent); reintroduce;
    procedure Startup;
  public
    function GetEngineConfigFilePath: string;
  end;

  { TEngineConfig }

  TEngineConfig = object
  public
    ScreenWidth: integer;
    ScreenHeight: integer;
    FullScreen: boolean;
  public const
    DefaultScreenWidth = 1024;
    DefaultScreenHeight = 768;
    ScreenWidthIdent = 'Screen.Width';
    ScreenHeightIdent = 'Screen.Height';
    DefaultFullScreen = true;
  public type
    EConfigInvalid = class(Exception);
    EUnspecifiedOption = class(EConfigInvalid);
  public
    procedure Read(const aFile: TIniFile);
    procedure SetDefaults;
    procedure Write(const aFile: TIniFile);
  end;

implementation

{ TEngineManager }

constructor TEngineManager.Create(const aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

procedure TEngineManager.Startup;
var
  config: TIniFile;
begin
  config := TIniFile.Create(GetEngineConfigFilePath);
end;

function TEngineManager.GetEngineConfigFilePath: string;
begin
  result := GlobalConfigPath + EngineConfigFileName;
end;

{ TEngineConfig }

procedure TEngineConfig.Read(const aFile: TIniFile);
begin
  AssertArgumentAssigned(aFile, 'aFile');
  ScreenWidth := aFile.ReadInteger('', ScreenWidthIdent, -1);
  if ScreenWidth = -1 then
    raise EUnspecifiedOption.Create(ScreenWidthIdent);
  ScreenHeight := aFile.ReadInteger('', 'Screen.Height', -1);
  if ScreenHeight = -1 then
    raise EUnspecifiedOption.Create(ScreenHeightIdent);
end;

procedure TEngineConfig.SetDefaults;
begin
  ScreenWidth := DefaultScreenWidth;
  ScreenHeight := DefaultScreenHeight;
  FullScreen := DefaultFullScreen;
end;

procedure TEngineConfig.Write(const aFile: TIniFile);
begin
  AssertArgumentAssigned(aFile, 'aFile');
  aFile.WriteInteger('', ScreenWidthIdent, ScreenWidth);
  aFile.WriteInteger('', ScreenHeightIdent, ScreenHeight);
end;

end.

