unit MapDataFace;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes,
  SysUtils,
  gvector,

  Generic2DArray;

type

  TTerrainType = integer;

  TCell = object
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
    function Equals(const aCell: TCellNumber): boolean;
    procedure SetXY(const aX, aY: integer);
    function Up: TCellNumber;
    function Down: TCellNumber;
    function Left: TCellNumber;
    function Right: TCellNumber;
  end;

  TCellNumbers = array of TCellNumber;

  TCellNumberList = specialize TVector<TCellNumber>;

  PCellNumber = ^TCellNumber;

  TCells = specialize T2Array<TCell>;

  IMapData = interface
    function GetCells: TCells;
    property Cells: TCells read GetCells;
  end;

  TCellNumbersOperation = (cnTop, cnBottom, cnLeft, cnRight);

  // 0 - top, 1 - bottom, 2 - left, 3 - right

operator = (const aCell, bCell: TCellNumber): boolean;

operator in (const aCell: TCellNumber; const aCells: TCellNumbers): boolean;

function FindBoundaryCells(const aCells: TCellNumbers): TCellNumberList;

implementation

operator = (const aCell, bCell: TCellNumber): boolean;
begin
  result := aCell.Equals(bCell);
end;

operator in(const aCell: TCellNumber; const aCells: TCellNumbers): boolean;
var
  cell: TCellNumber;
begin
  result := false;
  for cell in aCells do
    if cell = aCell then
      result := true;
end;

function FindBoundaryCells(const aCells: TCellNumbers): TCellNumberList;
var
  list: TCellNumberList;
  cell: TCellNumber;
begin
  list := TCellNumberList.Create;
  for cell in aCells do
  begin
    if not (cell.Up in aCells) then
      list.PushBack(cell.Up);
    if not (cell.Down in aCells) then
      list.PushBack(cell.Down);
    if not (cell.Left in aCells) then
      list.PushBack(cell.Left);
    if not (cell.Right in aCells) then
      list.PushBack(cell.Right);
  end;
  result := list;
end;

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

function TCellNumber.Equals(const aCell: TCellNumber): boolean;
begin
  result := (aCell.X = X) and (aCell.Y = Y);
end;

procedure TCellNumber.SetXY(const aX, aY: integer);
begin
  X := aX;
  Y := aY;
end;

function TCellNumber.Up: TCellNumber;
begin
  result.SetXY(X, Y - 1);
end;

function TCellNumber.Down: TCellNumber;
begin
  result.SetXY(X, Y + 1);
end;

function TCellNumber.Left: TCellNumber;
begin
  result.SetXY(X - 1, Y);
end;

function TCellNumber.Right: TCellNumber;
begin
  result.SetXY(X + 1, Y);
end;

end.

