unit LevelLoaderFace;

{$mode objfpc}{$H+}

interface

uses
  LogEntityFace,
  LevelDataFace;

type
  ILevelLoader = interface
    procedure SetLog(const aLog: ILog);
    property Log: ILog write SetLog;
    procedure Load(const aLevel: ILevelData);
    procedure Free;
  end;

implementation

end.

