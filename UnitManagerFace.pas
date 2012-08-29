unit UnitManagerFace;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  NiceInterfaces,

  BuildingUnitFaceA;

type

  { IUnitManager }

  IUnitManager = interface(IReversible) ['IUnitManager']
    {$REGION Buildings}
    procedure LoadBasicBuildingTypes;
    procedure AddBasicVehicleFactory(const aX, aY: integer);
    procedure AddBasicGunTurret(const aX, aY: integer);
    procedure AddBasicSingleTurret(const aX, aY: integer);
    {$ENDREGION}
    {$REGION Vehicles}
    procedure LoadBasicVehicleTypes;
    procedure AddBasicTank(const aX, aY: integer);
    {$ENDREGION}
  end;

implementation

end.

