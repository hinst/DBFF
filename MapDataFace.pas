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

  PCell = ^TCell;

  { TCellNumber }

  TCellNumber = object
  private
    fX, fY: integer;
  public
    property X: integer read fX write fX;
    property Y: integer read fY write fY;
    procedure Assign(const a: TCellNumber);
    function IsNegative: boolean;
    function Equals(const aX, aY: integer): boolean;
    procedure SetXY(const aX, aY: integer);
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

function TCellNumber.IsNegative: boolean;
begin
  result := (X > 0) or (Y > 0);
end;

function TCellNumber.Equals(const aX, aY: integer): boolean;
begin
  result := (aX = X) and (aY = Y);
end;

procedure TCellNumber.SetXY(const aX, aY: integer);
begin
  X := aX;
  Y := aY;
end;

end.

