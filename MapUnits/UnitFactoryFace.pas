unit UnitFactoryFace;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes,
  SysUtils,

  UnitProduction;

type
  IUnitFactory = interface ['IUnitFactory']
    function GetProduction: TUnitProduction;
    property Production: TUnitProduction read GetProduction;
    function IsVehicleFactory: boolean;
  end;

implementation

end.

