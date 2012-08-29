unit UnitManagerExtendedFace;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  UnitManagerFace,
  MapUnitFace;

type
  IUnitManagerExtended = interface(IUnitManager) ['IUnitManagerExtended']
    procedure AddUnit(const aUnit: IMapUnit; const aX, aY: integer);
  end;

implementation

end.

