unit PathFinder;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  NiceTypes,

  MapDataFace;

type

  { TPathFind }

  TPathFind = class
  public
    constructor Create;
  private
    fReady: boolean;
    fMap: IMapData;
    fPath: TCellNumbers;
    fMatrix: T2DIntegerArray;
    fWaveNumber: integer;
    fDestination: TCellNumber;
    procedure Initialize;
    procedure AllocateMatrix;
    procedure SetMap(const aMap: IMapData);
    procedure SetStart(const aCell: PCellNumber);
    procedure SetDestination(const aCell: PCellNumber);
    function GetDestination: PCellNumber;
    procedure Mark(const aX, aY: integer); inline;
    function GetBack(const aX, aY: integer): integer; inline;
    procedure Wave;
    procedure BackWave;
  public
    property Ready: boolean read fReady;
    property Map: IMapData read fMap write fMap;
    property Start: PCellNumber write SetStart;
    property Destination: PCellNumber read GetDestination write SetDestination;
    property Path: TCellNumbers read fPath;
    property WaveNumber: integer read fWaveNumber;
    function DestinationApproached: boolean;
    procedure Iterate;
    destructor Destroy; override;
  end;

implementation

{ TPathFind }

constructor TPathFind.Create;
begin
  inherited Create;
  Initialize;
end;

procedure TPathFind.Initialize;
begin
  fReady := false;
  SetLength(fPath, 0);
  fWaveNumber := 0;
end;

procedure TPathFind.AllocateMatrix;
begin
  SetLength(fMatrix, Map.Cells.Width, Map.Cells.Height);
  Fill2DIntegerArray(fMatrix, -1);
end;

procedure TPathFind.SetMap(const aMap: IMapData);
begin
  fMap := aMap;
  AllocateMatrix;
end;

procedure TPathFind.SetStart(const aCell: PCellNumber);
begin
  fMatrix[aCell^.X, aCell^.Y] := 0;
end;

procedure TPathFind.SetDestination(const aCell: PCellNumber);
begin
  fDestination.Assign(aCell^);
end;

function TPathFind.GetDestination: PCellNumber;
begin
  result := @ fDestination;
end;

procedure TPathFind.Mark(const aX, aY: integer);
begin
  if not Map.Cells.CellExists[aX, aY] then exit;
  if not (fMatrix[aX, aY] = -1) then exit;
  if Map.Cells.Matrix[aX, aY].busy then exit;
  fMatrix[aX, aY] := WaveNumber;
end;

function TPathFind.GetBack(const aX, aY: integer): integer;
begin
  result := -1;
  if not Map.Cells.CellExists[aX, aY] then exit;
  result := fMatrix[aX, aY];
  if not result = WaveNumber then exit;
  fPath[fWaveNumber - 1].SetXY(aX, aY);
end;

procedure TPathFind.Wave;
var
  x, y: integer;
begin
  inc(fWaveNumber);
  for x := 0 to Length(fMatrix) - 1 do
    for y := 0 to Length(fMatrix[x]) - 1 do
    begin
      if fMatrix[x, y] <> WaveNumber - 1 then continue;
      Mark(x - 1, y - 1);
      Mark(x + 0, y - 1);
      Mark(x + 1, y - 1);

      Mark(x, y + 1);
      Mark(x - 1, y);

      Mark(x - 1, y + 1);
      Mark(x + 0, y + 1);
      Mark(x + 1, y + 1);
    end;
end;

procedure TPathFind.BackWave;
var
  x, y: integer;
begin
  if Length(fPath) = 0 then
  begin
    SetLength(fPath, WaveNumber);
    fPath[WaveNumber - 1].Assign(fDestination);
  end;
  dec(fWaveNumber);
  if WaveNumber = 0 then
  begin
    fReady := true;
    exit;
  end;
  for x := 0 to Length(fMatrix) - 1 do
    for y := 0 to Length(fMatrix[x]) - 1 do
    begin
      if not fMatrix[x, y] = WaveNumber + 1 then exit;
      GetBack(x - 1, y - 1);
      GetBack(x + 0, y - 1);
      GetBack(x + 1, y - 1);

      GetBack(x, y + 1);
      GetBack(x - 1, y);

      GetBack(x - 1, y + 1);
      GetBack(x + 0, y + 1);
      GetBack(x + 1, y + 1);
    end;
end;

function TPathFind.DestinationApproached: boolean;
begin
  result := fMatrix[Destination^.X, Destination^.Y] <> -1;
end;

procedure TPathFind.Iterate;
begin
  if Ready then
    exit;
  if not DestinationApproached then
    Wave
  else
    BackWave;
end;

destructor TPathFind.Destroy;
begin
  inherited Destroy;
end;

end.

