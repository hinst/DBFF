unit MapScrollManagerFace;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes,
  SysUtils,

  MapDataFace;

type
  IMapScrollManager = interface
    function GetViewLeft: single;
    function GetViewTop: single;
    function GetViewRight: single;
    function GetViewBottom: single;
    property ViewLeft: single read GetViewLeft;
    property ViewTop: single read GetViewTop;
    property ViewRight: single read GetViewRight;
    property ViewBottom: single read GetViewBottom;

    function GetTileWidth: single;
    function GetTileHeight: single;
    property TileWidth: single read GetTileWidth;
    property TileHeight: single read GetTileHeight;

    function GetCellVisible(const aX, aY: integer): boolean;
    property CellVisible[const x, y: integer]: boolean read GetCellVisible;

    function GetCellAtWindowPoint(const aX, aY: integer): PCell;
    property CellAtWindowPoint[const x, y: integer]: PCell read GetCellAtWindowPoint;

    function ScreenX(const aCell: TCellNumber): single;
    function ScreenY(const aCell: TCellNumber): single;
  end;

implementation

end.

