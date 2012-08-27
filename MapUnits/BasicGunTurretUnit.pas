unit BasicGunTurretUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  zgl_textures,

  Common,
  AbstractTurretUnit;

type

  { TBasicGunTurretType }

  TBasicGunTurretType = class(TAbstractTurretType)
  public
    constructor Create; override;
  protected
    function GetTextureFilePath: string; override;
    function GetTopTextureFilePath: string; override;
  end;

  TBasicGunTurret = class(TAbstractTurret)
  end;

implementation

{ TBasicGunTurretType }

constructor TBasicGunTurretType.Create;
begin
  inherited Create;
end;

function TBasicGunTurretType.GetTextureFilePath: string;
begin
  result := StandardUnitsPath + 'GTurretFnd.png';
end;

function TBasicGunTurretType.GetTopTextureFilePath: string;
begin
  result := StandardUnitsPath + 'GTurretTop.png';
end;

end.

