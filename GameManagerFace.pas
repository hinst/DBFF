unit GameManagerFace;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  SysUtils,

  EngineManagerFace,
  LevelDataFace;

type
  IGameManager = interface
    function GetEngine: IEngineManager;
    property Engine: IEngineManager read GetEngine;
    function GetLevel: ILevelData;
    property Level: ILevelData read GetLevel;

    procedure Load;
    procedure Draw;
    procedure Update(const aTime: double);
end;

implementation

end.

