unit MapDataFace;

{$INTERFACES CORBA}

interface

uses
  Classes,
  SysUtils,
  heContnrs,

  Generic2DArray,
  MapDataCells;

type
  IMapData = interface
    function GetCells: TCells;
    property Cells: TCells read GetCells;
    function GetContiguousCells(const aCells: TCellNumberArray): TCellNumberVector;
    property ContiguousCells[const aCells: TCellNumberArray]: TCellNumberVector
      read GetContiguousCells;
    function GetSurroundingCells(const aCell: TCellNumber): TCellNumberVector;
    function GetSurroundingCells(const aCells: TCellNumberArray): TCellNumberVector;
    property SurroundingCells[const aCells: TCellNumberArray]: TCellNumberVector
      read GetSurroundingCells;
  end;

implementation

end.

