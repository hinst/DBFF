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
  MapScrollManager;

type

  { IMapUnit }

  IMapUnit = interface(IReversible)
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

    procedure Draw(const aScroll: TMapScrollManager);
    procedure DrawTopLayer(const aScroll: TMapScrollManager);
    procedure Update(const aTime: double);
    procedure UpdateGraphicalRect(const aScroll: TMapScrollManager);
  end;

implementation

end.

