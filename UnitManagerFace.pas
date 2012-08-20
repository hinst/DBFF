unit UnitManagerFace;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  BuildingUnitFaceA;

type
  IUnitManager = interface
    function AddNewBuildingType: IAbstractBuildingType;
  end;

implementation

end.

