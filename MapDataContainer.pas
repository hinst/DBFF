unit MapDataContainer;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

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
    function GetNearbyCells(const aCells: TCellNumbers): TCellNumbers;
    procedure Finalize;
  public
    property Cells: TCells read fCells;
    property NearbyCells[const aCells: TCellNumbers]: TCellNumbers read GetNearbyCells;
    procedure RemoveNonExistingCells(var aInput: TCellNumberList);
    destructor Destroy; override;
  end;

implementation

{ TMapData }

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

function TMapData.GetNearbyCells(const aCells: TCellNumbers): TCellNumbers;
var
  list: TCellNumberList;
  i: integer;
begin
  list := FindBoundaryCells(aCells);
  RemoveNonExistingCells(list);
  SetLength(result, list.Size);
  for i := 0 to list.Size - 1 do
    result[i] := list[i];
  list.Free;
end;

procedure TMapData.Finalize;
begin
  if Assigned(Cells) then
    FreeAndNil(fCells);
end;

procedure TMapData.RemoveNonExistingCells(var aInput: TCellNumberList);
var
  result: TCellNumberList;
  i: integer;
  cell: TCellNumber;
begin
  result := TCellNumberList.Create;
  for i := 0 to aInput.Size - 1 do
  begin
    cell := aInput[i];
    if Cells.CellExists[cell.X, cell.Y] then
      result.PushBack(cell);
  end;
  aInput.Free;
  aInput := result;
end;

destructor TMapData.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

end.

