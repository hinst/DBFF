unit BasicSingleTurretUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  AbstractTurretUnit;

type

  { TBasicSingleTurretType }

  TBasicSingleTurretType = class(TAbstractTurretType)
  protected
    function GetTextureFilePath: string; override;
    function GetTopTextureFilePath: string; override;
  end;

  TBasicSingleTurret = class(TAbstractTurret)

  end;

implementation

{ TBasicSingleTurretType }

function TBasicSingleTurretType.GetTextureFilePath: string;
begin
  result := StandardUnitsPath + 'GTurretFnd.png';
end;

function TBasicSingleTurretType.GetTopTextureFilePath: string;
begin
  result := StandardUnitsPath + 'RTurretTop.png';
end;

end.

