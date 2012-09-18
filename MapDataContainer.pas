unit MapDataContainer;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  MapDataCells,
  MapDataFace;

type

  { TMapData }

  TMapData = class(TComponent, IMapData)
  public
    constructor Create(const aOwner: TComponent); reintroduce;
  private
    fCells: TCells;
    procedure Initialize;
    function GetCells: TCells;
    function GetContiguousCells(const aCells: TCellNumberArray): TCellNumberVector;
    function GetSurroundingCells(const aCell: TCellNumber): TCellNumberVector;
    function GetSurroundingCells(const aCells: TCellNumberArray): TCellNumberVector;
    procedure Finalize;
  public
    property Cells: TCells read fCells;
    procedure RemoveNonExistingCells(var aInput: TCellNumberVector);
    destructor Destroy; override;
  end;

implementation

constructor TMapData.Create(const aOwner: TComponent);
begin
  inherited Create(aOwner);
  Initialize;
end;

procedure TMapData.Initialize;
begin
  fCells := TCells.Create;
end;

function TMapData.GetCells: TCells;
begin
  result := Cells;
end;

function TMapData.GetContiguousCells(const aCells: TCellNumberArray): TCellNumberVector;
begin
  result := FindContiguousCells(aCells);
  RemoveNonExistingCells(result);
end;

function TMapData.GetSurroundingCells(const aCell: TCellNumber): TCellNumberVector;
begin
  result := CreateSurroundingCells(aCell);
  RemoveNonExistingCells(result);
end;

function TMapData.GetSurroundingCells(const aCells: TCellNumberArray): TCellNumberVector;
begin
  result := FindSurroundingCells(aCells);
  RemoveNonExistingCells(result);
end;

procedure TMapData.Finalize;
begin
  if Assigned(Cells) then
    FreeAndNil(fCells);
end;

procedure TMapData.RemoveNonExistingCells(var aInput: TCellNumberVector);
var
  result: TCellNumberVector;
  i: integer;
  cell: TCellNumber;
begin
  result := TCellNumberVector.Create;
  for i := 0 to aInput.Count - 1 do
  begin
    cell := aInput[i];
    if Cells.CellExists[cell.X, cell.Y] then
      result.Add(cell);
  end;
  {$REGION Replace variable argument with a new list}
  aInput.Free;
  aInput := result;
  {$ENDREGION}
end;

destructor TMapData.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

end.

