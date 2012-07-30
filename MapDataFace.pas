unit MapDataFace;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils,

  Generic2DArray;

type

  TTerrainType = integer;

  TCell = record
    typee: TTerrainType;
    busy: boolean;
  end;

  TCells = specialize T2Array<TCell>;

  IMapData = interface
    function GetCells: TCells;
    property Cells: TCells read GetCells;
  end;

implementation

end.

