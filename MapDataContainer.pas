unit MapDataContainer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

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
    procedure Finalize;
  public
    property Cells: TCells read fCells;
    destructor Destroy; override;
  end;

implementation

{ TMapData }

constructor TMapData.Create(const aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

procedure TMapData.Initialize;
begin
  fCells := TCells.Create;
end;

function TMapData.GetCells: TCells;
begin
  result := Cells;
end;

procedure TMapData.Finalize;
begin
  if Assigned(Cells) then
    FreeAndNil(fCells);
end;

destructor TMapData.Destroy;
begin
  inherited Destroy;
end;

end.
