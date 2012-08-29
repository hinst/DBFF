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
begin
  list := FindBoundaryCells(aCells);
  list.Free;
end;

procedure TMapData.Finalize;
begin
  if Assigned(Cells) then
    FreeAndNil(fCells);
end;

destructor TMapData.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

end.

