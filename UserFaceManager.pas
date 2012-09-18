unit UserFaceManager;

{ $DEFINE DEBUG_LOG_USER_UNIT_SELECTION}
{$DEFINE DEBUG_LOG_USER_NAVIGATE}

interface

uses
  Classes,
  SysUtils,

  zgl_math_2d,
  zgl_mouse,
  zgl_keyboard,
  zgl_primitives_2d,

  NiceTypes,
  NiceExceptions,
  LogEntity,
  LogEntityFace,

  Common,
  MapDataCells,
  UnitManagerFace,
  UnitManager,
  MapUnitFace,
  UnitFactoryFace,
  UnitProduction,
  MapScrollManager,
  ZenGLFCLGraphics,
  BasicTankUnit,
  LevelData;

type

  { TUserFace }

  TUserFace = class
  public
    constructor Create;
  private
    fLog: ILog;
    fLevel: TLevelData;
    fUnitMan: TUnitManager;
    fSelectedUnits: TIMapUnits;
    fSlUnitsGlow: LongWord;
    fSlUnitsGlowSpeed: integer;
    fUpdate100: double;
    function GetUnitMan: TUnitManager; inline;
    function GetScroll: TMapScrollManager; inline;
    procedure Initialize;
    procedure AssignDefaults;
    procedure DrawChosenUnitRect(const aRect: zglTRect);
    procedure DrawChosenUnitFrame(const aUnit: IMapUnit);
    procedure DrawChosenUnitFrames;
    procedure Update100;
    procedure Update100ChUnitsGlow;
    procedure ProcessLevelInput(const aTime: double);
    function ChooseClick: boolean;
    function NavigateClick: boolean;
    procedure UserSelect;
    procedure UserNavigate;
    procedure Finalize;
  public
    property Log: ILog read fLog;
    // This property should be assigned
    property Level: TLevelData read fLevel write fLevel;
    property UnitMan: TUnitManager read GetUnitMan;
    property Scroll: TMapScrollManager read GetScroll;
    property SelectedUnits: TIMapUnits read fSelectedUnits;
    property SlUnitsGlow: LongWord read fSlUnitsGlow;
    procedure Draw;
    procedure Update(const aTime: double);
    procedure ReceiveInput(const aTime: double);
    procedure UserConstructBasicTank;
    destructor Destroy; override;
  end;

implementation

{ TUserFace }

constructor TUserFace.Create;
begin
  inherited Create;
  Initialize;
end;

function TUserFace.GetUnitMan: TUnitManager;
begin
  result := nil;
  if Level = nil then exit;
  result := Level.UnitMan;
end;

function TUserFace.GetScroll: TMapScrollManager;
begin
  result := nil;
  if Level = nil then exit;
  if Level.MapView = nil then exit;
  result := Level.MapView.Scroll;
end;

procedure TUserFace.Initialize;
begin
  AssignDefaults;
  fLog := TLog.Create(GlobalLogManager, 'UserFace');
  fSelectedUnits := TIMapUnits.Create;
end;

procedure TUserFace.AssignDefaults;
begin
  fSlUnitsGlow := $000000;
  fSlUnitsGlowSpeed := $111111;
end;

procedure TUserFace.DrawChosenUnitRect(const aRect: zglTRect);
var
  x, y, w, h: single;
  color: LongWord;
begin
  UnpackPRect(@aRect, x, y, w, h);
  color := SlUnitsGlow and T6Colors.Red;
  pr2d_Rect3(x, y, w, h, color);
end;

procedure TUserFace.DrawChosenUnitFrame(const aUnit: IMapUnit);
var
  rect: zglPRect;
  screenRect: zglTRect;
begin
  rect := aUnit.GraphicalRect;
  AssertAssigned(Scroll, 'Scroll', TVariableType.Propertie);
  screenRect := Scroll.GlobalToScreen(rect);
  DrawChosenUnitRect(screenRect);
end;

procedure TUserFace.DrawChosenUnitFrames;
var
  u: IMapUnit;
begin
  for u in SelectedUnits do
    DrawChosenUnitFrame(u);
end;

procedure TUserFace.Update100;
begin
  Update100ChUnitsGlow;
end;

procedure TUserFace.Update100ChUnitsGlow;
begin
  fSlUnitsGlow += fSlUnitsGlowSpeed;
  if (SlUnitsGlow >= $FFFFFF) or (SlUnitsGlow <= $000000) then
    fSlUnitsGlowSpeed := - fSlUnitsGlowSpeed;
end;

procedure TUserFace.ProcessLevelInput(const aTime: double);
begin
  if ChooseClick then
    UserSelect;
  if NavigateClick then
    UserNavigate;
  if key_Press(K_F4) then
    UserConstructBasicTank;
end;

function TUserFace.ChooseClick: boolean;
begin
  result :=
    mouse_Click(M_BLEFT) // left click
    and not key_Down(K_CTRL); // without ctrl
end;

function TUserFace.NavigateClick: boolean;
begin
  result := mouse_Click(M_BRIGHT); // right click
end;

procedure TUserFace.UserSelect;
{$DEFINE DEBUG_THIS_PROCEDURE}

  procedure LogWrite(const aText: string); inline;
  begin
    {$IFDEF DEBUG_LOG_USER_UNIT_SELECTION}
      Log.Write(aText);
    {$ENDIF}
  end;

var
  u: IMapUnit;
begin
  SelectedUnits.Clear;
  LogWrite('It appears that user wants to select an unit');
  u := Level.UnitMan.UnitAtWindowPoint[mouse_X, mouse_Y];
  if u = nil then
  begin
    LogWrite('But there is no unit at this point');
    exit; // no unit to UserSelect
  end;
  LogWrite('Unit selected. Class: "' + u.Reverse.ClassName + '"');
  SelectedUnits.Add(u);
end;

procedure TUserFace.UserNavigate;

  procedure LogWrite(const aText: string); inline;
  begin
    {$IFDEF DEBUG_LOG_USER_NAVIGATE}
      Log.Write(aText);
    {$ENDIF}
  end;

var
  u: IMapUnit;
  moveable: IMoveableMapUnit;
  targetCell: TCellNumber;
  once: boolean;
begin
  once := false;
  targetCell := Scroll.CellNumberAtWindowPoint[mouse_X, mouse_Y];
  for u in SelectedUnits do
  begin
    if u.Reverse is IMoveableMapUnit then
    begin
      LogWrite('Navigating ' + u.Reverse.ClassName + ' to ' + targetCell.ToText);
      moveable := u.Reverse as IMoveableMapUnit;
      moveable.Navigate(targetCell);
      once := true;
    end;
  end;
  if not once then
    LogWrite('No moveable units selected.');
end;

procedure TUserFace.Finalize;
begin
  FreeAndNil(fSelectedUnits);
  FreeLog(fLog);
end;

procedure TUserFace.Draw;
begin
  DrawChosenUnitFrames;
end;

procedure TUserFace.Update(const aTime: double);
begin
  fUpdate100 += aTime;
  if fUpdate100 >= 100 then
  begin
    Update100;
    fUpdate100 -= 100;
  end;
end;

procedure TUserFace.ReceiveInput(const aTime: double);
begin
  if Level <> nil then
    ProcessLevelInput(aTime);
end;

procedure TUserFace.UserConstructBasicTank;
{$DEFINE DEBUG_THIS}
  procedure LogWrite(const aText: string);
  begin
    {$IFDEF DEBUG_THIS}
    Log.Write(aText);
    {$ENDIF}
  end;

var
  u: IMapUnit;
  factory: IUnitFactory;
  productionItem: TUnitProductionItem;
  productionsInitiated: integer;

begin
  LogWrite('UserConstructBasicTank called');
  productionsInitiated := 0;
  if SelectedUnits.Count = 0 then
  begin
    LogWrite('Can not initiate production: Selected is nothing');
    exit;
  end;
  for u in SelectedUnits do
  begin
    if not (u.Reverse is IUnitFactory) then continue;
    factory := u.Reverse as IUnitFactory;
    if not factory.IsVehicleFactory then continue;
    productionItem := TUnitProductionItem.Create;
    productionItem.Progress := 0;
    productionItem.TimeCost := 3000;
    productionItem.MapUnit := UnitMan.CreateVehicle(TBasicTank, TBasicTankType) as IMapUnit;
    factory.Production.Que.Add(productionItem);
    inc(productionsInitiated);
  end;
  LogWrite('  ' + IntToStr(productionsInitiated) + ' productions initiated');
end;
{$UNDEF DEBUG_THIS}

destructor TUserFace.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

end.

