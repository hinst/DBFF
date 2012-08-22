unit UnitManagerFace;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  BuildingUnitFaceA;

type

  { IUnitManager }

  IUnitManager = interface
    procedure LoadBasicBuildingTypes;
    function AddNewBuildingType: IAbstractBuildingType;
    procedure AddBasicVehicleFactory(const aX, aY: integer);
  end;

implementation

end.

