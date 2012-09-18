unit PathFinder;

{$DEFINE DEBUG_LOG_PATHFINDER_WAVES}

interface

uses
  Classes,
  SysUtils,

  NiceTypes,
  NiceExceptions,
  StringFeatures,
  LogEntityFace,
  NoLogEntity,

  MapDataCells,
  MapDataFace;

type

  { TPathFind class
    How to use:
    0. Create
    1. Assign map property
    2. Assign Start property
    3. Assign Destination property
    4. Call Iterate methodd until Ready property gets true
    5. Use Path property
  }

  TPathFind = class
  public
    constructor Create;
  private
    fLog: ILog;
    fReady: boolean;
    fMap: IMapData;
    fPath: TCellNumberArray;
    fMatrix: T2DIntegerArray;
    fWaveNumber: integer;
    fDestination: TCellNumber;
    fWaved: boolean;
    procedure Initialize;
    procedure ReplaceLog(const aLog: ILog);
    procedure AllocateMatrix;
    procedure SetMap(const aMap: IMapData);
    procedure SetStart(const aCell: PCellNumber);
      // Creates internal copy
    procedure SetDestination(const aCell: PCellNumber);
      // Returns pointer to internal copy
    function AccessDestination: PCellNumber;
      // Get One Step Accessible Cells
      // caller is responsible for calling Free
    function GetOneSAC(const aFrom: TCellNumber): TCellNumberVector;
    procedure Mark(const aCells: TCellNumberVector);
    procedure Mark(const aX, aY: integer); inline;
    procedure Wave;
    procedure BackWave;
    procedure BackWave(const aCells: TCellNumberVector);
    function BackWave(const aX, aY: integer): boolean;
    function MatrixToText: string;
    procedure WriteMatrixToLog;
    procedure ReleaseResources;
  public
    property Log: ILog read fLog write ReplaceLog;
    property Ready: boolean read fReady;
    property Map: IMapData read fMap write SetMap;
    property Start: PCellNumber write SetStart;
    property Destination: PCellNumber read AccessDestination write SetDestination;
      // One Step Accessible Cells
      // don't forget to release
    property OneSAC[const aFrom: TCellNumber]: TCellNumberVector read GetOneSAC;
    property Path: TCellNumberArray read fPath;
    property WaveNumber: integer read fWaveNumber;
    property Waved: boolean read fWaved;
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
  fLog := TNoLog.Create;
  fReady := false;
  SetLength(fPath, 0);
  fWaveNumber := 0;
end;

procedure TPathFind.ReplaceLog(const aLog: ILog);
begin
  Log.Free;
  fLog := aLog;
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

function TPathFind.AccessDestination: PCellNumber;
begin
  result := @ fDestination;
end;

function TPathFind.GetOneSAC(const aFrom: TCellNumber): TCellNumberVector;
begin
  result := Map.SurroundingCells[ aFrom ];
end;

procedure TPathFind.Mark(const aCells: TCellNumberVector);
var
  i: integer;
begin
  for i := 0 to aCells.Count - 1 do
    Mark(aCells[i].X, aCells[i].Y);
end;

procedure TPathFind.Mark(const aX, aY: integer);
begin
  if not Map.Cells.CellExists[aX, aY] then
    exit; // no such cell
  if not (fMatrix[aX, aY] = -1) then
    exit; // already marked
  if Map.Cells.Matrix[aX, aY].busy then
    exit; // something is on the cell
  fMatrix[aX, aY] := WaveNumber;
end;

procedure TPathFind.Wave;
const
  ADDITIONAL_WAVES = 2; // WHO KNOWS
var
  x, y: integer;
  cells: TCellNumberVector;
begin
  inc(fWaveNumber);
  for x := 0 to Length(fMatrix) - 1 do
    for y := 0 to Length(fMatrix[x]) - 1 do
      if fMatrix[x, y] = WaveNumber - 1 then
      begin
        cells := OneSAC[ TCellNumber.Construct(x, y) ];
        Mark(cells);
        cells.Free;
      end;
  if WaveNumber >= Map.Cells.Width + Map.Cells.Height + ADDITIONAL_WAVES then
    fWaved := true;
end;

procedure TPathFind.BackWave;
var
  cells: TCellNumberVector;
begin
  if not DestinationApproached then
  begin
    Log.Write('ERROR', 'Can not BackWave: destination not approached');
    exit;
  end;
  fWaveNumber := fMatrix[Destination^.X, Destination^.Y];
  {$IFDEF DEBUG_LOG_PATHFINDER_WAVES}
  WriteMatrixToLog;
  {$ENDIF}
  SetLength(fPath, WaveNumber);
  dec(fWaveNumber);
  Path[WaveNumber].Assign(Destination^);
  while WaveNumber >= 1 do
  begin
    {$IFDEF DEBUG_LOG_PATHFINDER_WAVES}
    Log.Write('Searching for back step from ' + Path[WaveNumber].ToText);
    {$ENDIF}
    cells := OneSAC[ Path[WaveNumber] ];
    BackWave(cells);
    cells.Free;
    dec(fWaveNumber);
    {$IFDEF DEBUG_LOG_PATHFINDER_WAVES}
    Log.Write('  Found: ' + Path[WaveNumber].ToText);
    {$ENDIF}
  end;
  fReady := true;
end;

procedure TPathFind.BackWave(const aCells: TCellNumberVector);
var
  i: integer;
  r: boolean;
begin
  for i := 0 to aCells.Count - 1 do
  begin
    r := BackWave(aCells[i].X, aCells[i].Y);
    if r then
      break;
  end;
end;

function TPathFind.BackWave(const aX, aY: integer): boolean;
begin
  if not Map.Cells.CellExists[aX, aY] then
    exit(false);
  result :=
    fMatrix[aX, aY] = WaveNumber;
  if not result then
    exit(false);
  fPath[WaveNumber - 1].SetXY(aX, aY);
  result := true;
end;

function TPathFind.MatrixToText: string;
const
  MAX_ELEMENT_WIDTH = 4;
var
  x, y: integer;
begin
  AssertAssigned(Map, 'Map', TVariableType.Propertie);
  result := IntToStr(Map.Cells.Width) + ' x ' + IntToStr(Map.Cells.Height);
  for y := 0 to Map.Cells.Height - 1 do
  begin
    result += LineEnding;
    for x := 0 to Map.Cells.Width - 1 do
      result := result + LeftSpaceStr(IntToStr(fMatrix[x, y]), MAX_ELEMENT_WIDTH);
  end;
end;

procedure TPathFind.WriteMatrixToLog;
begin
  Log.Write(MatrixToText);
end;

procedure TPathFind.ReleaseResources;
begin
  FreeLog(fLog);
end;

function TPathFind.DestinationApproached: boolean;
begin
  result := fMatrix[Destination^.X, Destination^.Y] <> -1;
end;

procedure TPathFind.Iterate;
begin
  if Ready then
    exit;
  if not Waved then
    Wave
  else
    BackWave;
end;

destructor TPathFind.Destroy;
begin
  ReleaseResources;
  inherited Destroy;
end;

end.

