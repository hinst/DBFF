unit TerrainManagerFace;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils,
  MapDataFace;

type
  ITerrainManager = interface
    procedure LoadTerrains(const aFileName: string);
    procedure LoadMasks(const aFileName: string);
    // This function should be used for debugging
    function GetTerrainsInfoAsText: string;
    function GetTypeColor(const aType: TTerrainType): LongWord;
    function Reverse: TObject;
  end;

implementation

end.

