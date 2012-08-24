unit UserFaceManager;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  zgl_math_2d,
  zgl_mouse,
  zgl_keyboard,
  zgl_primitives_2d,

  NiceExceptions,
  LogEntity,
  LogEntityFace,

  Common,
  UnitManager,
  MapUnitFace,
  MapScrollManager,
  ZenGLFCLGraphics,
  LevelData;

type

  { TUserFace }

  TUserFace = class
  public
    constructor Create;
  private
    fLog: ILog;
    fLevel: TLevelData;
    fSelectedUnits: TIMapUnits;
    fSlUnitsGlow: LongWord;
    fSlUnitsGlowSpeed: integer;
    fUpdate100: double;
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
    procedure Select;
    procedure Finalize;
  public
    property Log: ILog read fLog;
    // This property should be assigned
    property Level: TLevelData read fLevel write fLevel;
    // This property should be assigned
    property Scroll: TMapScrollManager read GetScroll;
    property SelectedUnits: TIMapUnits read fSelectedUnits;
    property SlUnitsGlow: LongWord read fSlUnitsGlow;
    procedure Draw;
    procedure Update(const aTime: double);
    procedure ReceiveInput(const aTime: double);
    destructor Destroy; override;
  end;

implementation

{ TUserFace }

constructor TUserFace.Create;
begin
  inherited Create;
  Initialize;
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
  AssertAssigned(Scroll, 'Scroll');
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
  //WriteLN(IntToHex(SlUnitsGlow, 6));
  fSlUnitsGlow += fSlUnitsGlowSpeed;
  if (SlUnitsGlow >= $FFFFFF) or (SlUnitsGlow <= $000000) then
    fSlUnitsGlowSpeed := - fSlUnitsGlowSpeed;
end;

procedure TUserFace.ProcessLevelInput(const aTime: double);
begin
  if ChooseClick then
    Select;
end;

function TUserFace.ChooseClick: boolean;
begin
  result :=
    mouse_Click(M_BLEFT) // left click
    and not key_Down(K_CTRL); // without ctrl
end;

procedure TUserFace.Select;
{$DEFINE DEBUG_THIS_PROCEDURE}
var
  u: IMapUnit;
begin
  SelectedUnits.Clear;
  {$IFDEF DEBUG_THIS_PROCEDURE}
  Log.Write('It appears that user wants to select an unit');
  {$ENDIF}
  u := Level.UnitMan.UnitAtWindowPoint[mouse_X, mouse_Y];
  if u = nil then
  begin
    {$IFDEF DEBUG_THIS_PROCEDURE}
    Log.Write('But there is no unit at this point');
    {$ENDIF}
    exit; // no unit to Select
  end;
  {$IFDEF DEBUG_THIS_PROCEDURE}
  Log.Write('Unit selected. Class: "' + u.Reverse.ClassName + '"');
  {$ENDIF}
  SelectedUnits.Add(u);
end;
{$UNDEF DEBUG_THIS_PROCEDURE}

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

destructor TUserFace.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

end.

