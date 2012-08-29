unit MapUnitFace;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes,
  SysUtils,

  zgl_math_2d,

  NiceInterfaces,

  MapDataFace,
  MapScrollManagerFace,
  TerrainManagerFaceE;

type

  { IMapUnit }

  IMapUnit = interface(IReversible) ['IMapUnit']
    function GetOccupatedCells: TCellNumbers;
    property OccupatedCells: TCellNumbers read GetOccupatedCells;

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

implementation

end.

