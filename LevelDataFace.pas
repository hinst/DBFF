unit LevelDataFace;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  MapDataFace;

type
  ILevelData = interface
    function GetMap: IMapData;
    property Map: IMapData read GetMap;
  end;

implementation

end.

