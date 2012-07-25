unit TerrainManagerFace;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  ITerrainManager = interface
    procedure LoadTerrains(const aFileName: string);
    // This function should be used for debugging
    function GetTerrainsInfoAsText: string;
  end;

implementation

end.

