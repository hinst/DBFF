unit MapUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  zgl_math_2d,
  zgl_textures,

  LogEntityFace,
  LogEntity,

  Common,
  MapUnitFace,
  MapDataFace,
  MapScrollManager;

type


  { TMapUnit }

  TMapUnit = class
  public
    constructor Create;
  protected
    fLog: ILog;
    fLeftTopCell: TCellNumber;
    fGraphicalRect: zglTRect;
    function GetLeftTopCell: PCellNumber;
    function GetGraphicalRect: zglPRect;
    function Reverse: TObject;
    function GetUnitWidth: integer; virtual; abstract;
    function GetUnitHeight: integer; virtual; abstract;
    procedure Initialize;
    procedure Finalize;
  public
    property Log: ILog read fLog;
    property LeftTopCell: PCellNumber read GetLeftTopCell;
    property GraphicalRect: zglPRect read GetGraphicalRect;
    property UnitWidth: integer read GetUnitWidth;
    property UnitHeight: integer read GetUnitHeight;
    procedure UpdateGraphicalRect(const aScroll: TMapScrollManager);
    destructor Destroy; override;
  end;

implementation

{ TMapUnit }

constructor TMapUnit.Create;
begin
  inherited Create;
  Initialize;
end;

function TMapUnit.GetLeftTopCell: PCellNumber;
begin
  result := @fLeftTopCell;
end;

function TMapUnit.GetGraphicalRect: zglPRect;
begin
  result := @fGraphicalRect;
end;

function TMapUnit.Reverse: TObject;
begin
  result := self;
end;

procedure TMapUnit.Initialize;
begin
  fLog := TLog.Create(GlobalLogManager, self.ClassName);
end;

procedure TMapUnit.Finalize;
begin
  FreeLog(fLog);
end;

procedure TMapUnit.UpdateGraphicalRect(const aScroll: TMapScrollManager);
begin
  GraphicalRect^.X := LeftTopCell^.X * aScroll.TileWidth;
  GraphicalRect^.Y := LeftTopCell^.Y * aScroll.TileHeight;
  GraphicalRect^.W := UnitWidth * aScroll.TileWidth;
  GraphicalRect^.H := UnitHeight * aScroll.TileHeight;
end;

destructor TMapUnit.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

end.

