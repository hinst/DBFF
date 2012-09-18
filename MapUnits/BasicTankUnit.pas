unit BasicTankUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  MapUnitFace,
  AbstractTankUnit;

type

  { TBasicTankType }

  TBasicTankType = class(TAbstractTankType)
  public
    constructor Create; override;
  protected
    function GetTextureFilePath: string; override;
    function GetTowerTextureFilePath: string; override;
    procedure Initialize;
    procedure AssignDefaults;
  end;

  TBasicTank = class(TAbstractTank, IMapUnit)
  end;

implementation

{ TBasicTankType }

constructor TBasicTankType.Create;
begin
  inherited Create;
  Initialize;
end;

function TBasicTankType.GetTextureFilePath: string;
begin
  result := StandardUnitsPath + 'BasicTankBase.png';
end;

function TBasicTankType.GetTowerTextureFilePath: string;
begin
  result := StandardUnitsPath + 'GTurretTop.png';
end;

procedure TBasicTankType.Initialize;
begin
  AssignDefaults;
end;

procedure TBasicTankType.AssignDefaults;
begin
  fBodyRotationSpeed := 360 / 4000;
  fTowerSpeed := 360 / 2000;
end;

end.

