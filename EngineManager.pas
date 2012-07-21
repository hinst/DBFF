unit EngineManager;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  IniFiles,

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
    procedure Read(const aFile: TIniFile);
  public const
    DefaultScreenWidth = 1024;
    ScreenWidthIdent = 'Screen.Width';
    DefaultScreenHeight = 768;
  public type
    EConfigInvalid = class(Exception);
    EUnspecifiedOption = class(EConfigInvalid);
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
  ScreenWidth := aFile.ReadInteger('', 'Screen.Width', -1);
  if ScreenWidth = -1 then
    raise EUnspecifiedOption.Create(ScreenWidthIdent);
end;

end.

