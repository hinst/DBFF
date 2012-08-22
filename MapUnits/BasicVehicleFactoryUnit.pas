unit BasicVehicleFactoryUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  Common,
  BuildingUnit,
  EngineManagerFace;

type

  { TBasicVehicleFactoryType }

  TBasicVehicleFactoryType = class(TBuildingType)
  private
    function GetTextureName: string;
  public
    procedure Load(const aEngine: IEngineManager);
  end;

  TBasicVehicleFactory = class(TBuilding)
  end;

implementation

{ TBasicVehicleFactoryType }

function TBasicVehicleFactoryType.GetTextureName: string;
begin
  result := GlobalApplicationPath + StandardDataRelativePath
    + 'Units' + DirectorySeparator + 'Building_sample.png';
end;

procedure TBasicVehicleFactoryType.Load(const aEngine: IEngineManager);
begin
  fTexture := aEngine.LoadTexture(GetTextureName);
end;

end.

