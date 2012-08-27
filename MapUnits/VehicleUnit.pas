unit VehicleUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  zgl_textures,

  MapUnit;

type
  TVehicleType = class(TMapUnitType)
  end;

  TVehicleTypeClass = class of TVehicleType;

  { TVehicle }

  TVehicle = class(TMapUnit)
  end;

  TVehicleClass = class of TVehicle;

implementation

end.

