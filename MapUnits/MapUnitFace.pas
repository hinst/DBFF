unit MapUnitFace;

{$INTERFACES CORBA}

interface

uses
  Classes,
  SysUtils,

  zgl_math_2d,

  NiceInterfaces,

  ZenGLFCLGraphics,
  MapDataCells,
  MapDataFace,
  MapScrollManagerFace,
  TerrainManagerFaceE;

type

  IMapUnit = interface(IReversible) ['IMapUnit']
    function GetOccupatedCells: TCellNumberArray;
    property OccupatedCells: TCellNumberArray read GetOccupatedCells;

    function GetUnitWidth: integer;
    property UnitWidth: integer read GetUnitWidth;
    function GetUnitHeight: integer;
    property UnitHeight: integer read GetUnitHeight;

    function GetLeftTopCell: PCellNumber;
    property LeftTopCell: PCellNumber read GetLeftTopCell;
    function GetGraphicalRect: zglPRect;
    property GraphicalRect: zglPRect read GetGraphicalRect;
    function GetTerrainPossible(const aTerrain: PTerrain): boolean;
    property TerrainPossible[const aTerrain: PTerrain]: boolean read GetTerrainPossible;

    procedure Draw(const aScroll: IMapScrollManager);
    procedure DrawTopLayer(const aScroll: IMapScrollManager);
    procedure Update(const aTime: double);
    procedure UpdateGraphicalRect(const aScroll: IMapScrollManager);
  end;

  IMoveableMapUnit = interface ['IMoveableMapUnit']
    procedure Navigate(const aPoint: TCellNumber);
  end;

implementation

end.

