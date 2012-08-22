unit MapDataFace;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils,

  Generic2DArray;

type

  TTerrainType = integer;

  TCell = record
    typee: TTerrainType;
    busy: boolean;
  end;

  { TCellNumber }

  TCellNumber = object
  private
    fX, fY: integer;
  public
    property X: integer read fX write fX;
    property Y: integer read fY write fY;
    procedure Assign(const a: TCellNumber);
  end;

  TCellNumbers = array of TCellNumber;

  PCellNumber = ^TCellNumber;

  TCells = specialize T2Array<TCell>;

  IMapData = interface
    function GetCells: TCells;
    property Cells: TCells read GetCells;
  end;

implementation

{ TCellNumber }

procedure TCellNumber.Assign(const a: TCellNumber);
begin
  X := a.X;
  Y := a.Y;
end;

end.

