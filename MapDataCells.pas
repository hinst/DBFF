unit MapDataCells;

{ $DEFINE TCellNumber_INLINE}

interface

uses
  SysUtils,
  Classes,

  heContnrs,

  Generic2DArray;

type
  TTerrainType = integer;

  TCell = object
    typee: TTerrainType;
    busy: boolean;
  end;

  PCell = ^TCell;

  TCellNumber = object
  private
    fX, fY: integer;
  public
    property X: integer read fX write fX;
    property Y: integer read fY write fY;
    procedure Assign(const a: TCellNumber);
      {$IFDEF TCellNumber_INLINE} inline; {$ENDIF}
    function IsNegative: boolean;
      {$IFDEF TCellNumber_INLINE} inline; {$ENDIF}
    class function Negative: TCellNumber;
      {$IFDEF TCellNumber_INLINE} inline; {$ENDIF}
    class function Construct(const aX, aY: integer): TCellNumber;
      {$IFDEF TCellNumber_INLINE} inline; {$ENDIF}
    function Equals(const aX, aY: integer): boolean;
      {$IFDEF TCellNumber_INLINE} inline; {$ENDIF}
    function Equals(const aCell: TCellNumber): boolean;
      {$IFDEF TCellNumber_INLINE} inline; {$ENDIF}
    function Compare(const aCell: TCellNumber): integer;
      {$IFDEF TCellNumber_INLINE} inline; {$ENDIF}
    procedure SetXY(const aX, aY: integer);
      {$IFDEF TCellNumber_INLINE} inline; {$ENDIF}
    {$REGION SIDE CELLS}
    function Up: TCellNumber;
      {$IFDEF TCellNumber_INLINE} inline; {$ENDIF}
    function Down: TCellNumber;
      {$IFDEF TCellNumber_INLINE} inline; {$ENDIF}
    function Left: TCellNumber;
      {$IFDEF TCellNumber_INLINE} inline; {$ENDIF}
    function Right: TCellNumber;
      {$IFDEF TCellNumber_INLINE} inline; {$ENDIF}
    function UpLeft: TCellNumber;
      {$IFDEF TCellNumber_INLINE} inline; {$ENDIF}
    function DownLeft: TCellNumber;
      {$IFDEF TCellNumber_INLINE} inline; {$ENDIF}
    function UpRight: TCellNumber;
      {$IFDEF TCellNumber_INLINE} inline; {$ENDIF}
    function DownRight: TCellNumber;
      {$IFDEF TCellNumber_INLINE} inline; {$ENDIF}
    {$ENDREGION}
    function ToText: string;
      {$IFDEF TCellNumber_INLINE} inline; {$ENDIF}
  end;

  TCellNumberArray = array of TCellNumber;

  { TCellNumberVector }

  TCellNumberVector = class(specialize TheCmpVector<TCellNumber>)
  public
    constructor Create(const aCells: TCellNumberArray); overload;
    constructor Create(const aCells: array of TCellNumber); overload;
    function Compare(const A, B: TCellNumber): Integer; override;
      // Adds an item only if there is no such item already contained
    function AddWise(const aCell: TCellNumber): Integer;
    function AddWise(const aCells: TCellNumberVector): integer;
    procedure Add(const aCells: TCellNumberArray); overload;
    procedure Add(const aCells: array of TCellNumber); overload;
    procedure Remove(const aCells: TCellNumberArray); overload;
    function ToArray: TCellNumberArray;
  end;

  PCellNumber = ^TCellNumber;

  TCells = specialize T2Array<TCell>;

operator = (const aCell, bCell: TCellNumber): boolean;

operator in (const aCell: TCellNumber; const aCells: TCellNumberArray): boolean;

operator in (const aCell: TCellNumber; const aCells: TCellNumberVector): boolean;

function CreateContiguousCells(const aCell: TCellNumber): TCellNumberVector;

function FindContiguousCells(const aCells: TCellNumberArray): TCellNumberVector;

function CreateSurroundingCells(const aCell: TCellNumber): TCellNumberVector;

function FindSurroundingCells(const aCells: TCellNumberArray): TCellNumberVector;

function Contains(const aCells: TCellNumberArray; const aCell: TCellNumber): boolean;

operator := (const aCell: TCellNumber): string;

operator + (const aText: string; const aCell: TCellNumber): string;

function ToText(const aCells: TCellNumberArray): string; overload;

function CreateArray(const aCell: TCellNumber): TCellNumberArray; overload;

implementation

procedure TCellNumber.Assign(const a: TCellNumber);
begin
  X := a.X;
  Y := a.Y;
end;

function TCellNumber.IsNegative: boolean;
begin
  result := (X < 0) or (Y < 0);
end;

class function TCellNumber.Negative: TCellNumber;
begin
  result.SetXY(-1, -1);
end;

class function TCellNumber.Construct(const aX, aY: integer): TCellNumber;
begin
  result.SetXY(aX, aY);
end;

function TCellNumber.Equals(const aX, aY: integer): boolean;
begin
  result := (aX = X) and (aY = Y);
end;

function TCellNumber.Equals(const aCell: TCellNumber): boolean;
begin
  result := (aCell.X = X) and (aCell.Y = Y);
end;

function TCellNumber.Compare(const aCell: TCellNumber): integer;
begin
  if self.Equals(aCell) then
    result := 0
  else
    if self.X > aCell.X then
      result := 1
    else
      result := -1;
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

function TCellNumber.UpLeft: TCellNumber;
begin
  result.SetXY(X - 1, Y - 1);
end;

function TCellNumber.DownLeft: TCellNumber;
begin
  result.SetXY(X - 1, Y + 1);
end;

function TCellNumber.UpRight: TCellNumber;
begin
  result.SetXY(X + 1, Y - 1);
end;

function TCellNumber.DownRight: TCellNumber;
begin
  result.SetXY(X + 1, Y + 1);
end;

function TCellNumber.ToText: string;
begin
  if IsNegative then
    result := '(N, E)'
  else
    result := '(' + IntToStr(X) + ', ' + IntToStr(Y) + ')';
end;

constructor TCellNumberVector.Create(const aCells: TCellNumberArray);
begin
  Create;
  Add(aCells);
end;

constructor TCellNumberVector.Create(const aCells: array of TCellNumber);
begin
  Create;
  Add(aCells);
end;

function TCellNumberVector.Compare(const A, B: TCellNumber): Integer;
begin
  result := A.Compare(B);
end;

function TCellNumberVector.AddWise(const aCell: TCellNumber): Integer;
begin
  result := -1;
  if not Has(aCell) then
    result := inherited Add(aCell);
end;

function TCellNumberVector.AddWise(const aCells: TCellNumberVector): integer;
var
  cell: TCellNumber;
begin
  result := 0;
  for cell in aCells do
    if AddWise(cell) <> -1 then
      inc(result);
end;

procedure TCellNumberVector.Add(const aCells: TCellNumberArray);
var
  cell: TCellNumber;
begin
  for cell in aCells do
    Add(cell);
end;

procedure TCellNumberVector.Add(const aCells: array of TCellNumber);
var
  cell: TCellNumber;
begin
  for cell in aCells do
    Add(cell);
end;

procedure TCellNumberVector.Remove(const aCells: TCellNumberArray);
var
  cell: TCellNumber;
begin
  for cell in aCells do
    self.Remove(cell);
end;

function TCellNumberVector.ToArray: TCellNumberArray;
var
  i: integer;
begin
  SetLength(result, self.Count);
  for i := 0 to self.Count - 1 do
    result[i].Assign(Items[i]);
end;

operator = (const aCell, bCell: TCellNumber): boolean;
begin
  result := aCell.Equals(bCell);
end;

operator in (const aCell: TCellNumber; const aCells: TCellNumberArray): boolean;
begin
  result := Contains(aCells, aCell);
end;

operator in (const aCell: TCellNumber; const aCells: TCellNumberVector): boolean;
begin
  result := aCells.Has(aCell);
end;

function CreateContiguousCells(const aCell: TCellNumber): TCellNumberVector;
begin
  result := TCellNumberVector.Create(
    [
      aCell.Up,
      aCell.Down,
      aCell.Left,
      aCell.Right
    ]
  );
end;

function FindContiguousCells(const aCells: TCellNumberArray): TCellNumberVector;
var
  cell: TCellNumber;
  cells: TCellNumberVector;
begin
  result := TCellNumberVector.Create(aCells);
  for cell in aCells do
  begin
    cells := CreateContiguousCells(cell);
    result.AddWise(cells);
    cells.Free;
  end;
  result.Remove(aCells);
end;

function CreateSurroundingCells(const aCell: TCellNumber): TCellNumberVector;
begin
  result := CreateContiguousCells(aCell);
  result.Add(
    [
      aCell.UpLeft,
      aCell.DownLeft,
      aCell.UpRight,
      aCell.DownRight
    ]
  );
end;

function FindSurroundingCells(const aCells: TCellNumberArray): TCellNumberVector;
var
  cell: TCellNumber;
  cells: TCellNumberVector;
begin
  result := FindContiguousCells(aCells);
  for cell in aCells do
  begin
    cells := CreateSurroundingCells(cell);
    result.AddWise(cells);
    cells.Free;
  end;
  result.Remove(aCells);
end;

function Contains(const aCells: TCellNumberArray; const aCell: TCellNumber
  ): boolean;
var
  cell: TCellNumber;
begin
  result := false;
  for cell in aCells do
    if cell.Equals(aCell) then
      result := true;
end;

operator := (const aCell: TCellNumber): string;
begin
  result := aCell.ToText;
end;

operator + (const aText: string; const aCell: TCellNumber): string;
begin
  result := aText + string(aCell);
end;

function ToText(const aCells: TCellNumberArray): string;
var
  cell: TCellNumber;
begin
  result := ' ';
  for cell in aCells do
    result += cell.ToText + ' ';
end;

function CreateArray(const aCell: TCellNumber): TCellNumberArray;
begin
  SetLength(result, 1);
  result[0] := TCellNumber.Construct(aCell.X, aCell.Y);
end;

end.

