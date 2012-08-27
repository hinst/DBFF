unit TextureCache;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  zgl_textures,
  zgl_window,

  NiceExceptions,
  LogEntityFace,
  NoLogEntity,

  EngineManagerFace;

type

  { TMapTextureCacheItem }

  TMapTextureCacheItem = object
  public
    x, y: integer;
    texture: zglPTexture;
  end;

  PMapTextureCacheItem = ^TMapTextureCacheItem;

const
  MAX_MAP_TEXTURE_CACHE = 1000;

type

  { TMapTextureCache }

  TMapTextureCache = class
  public
    constructor Create;
  private type
    TCache = array of PMapTextureCacheItem;
  private
    fLog: ILog;
    fEngine: IEngineManager;
    fCache: TCache;
    fCleaningDistance: integer;
    property Cache: TCache read fCache;
    procedure Initialize;
    procedure SetLog(const aLog: ILog); inline;
    function GetCacheItem(const x, y: integer): PMapTextureCacheItem; inline;
    procedure Finalize;
  public
    property Engine: IEngineManager read fEngine write fEngine;
    property CleaningDistance: integer read fCleaningDistance write fCleaningDistance;
    property Log: ILog read fLog write SetLog;
    function GetDefaultDistance(const aTileWidth: integer): integer;
    property Access[const x, y: integer]: PMapTextureCacheItem read GetCacheItem;
    function AddNew: PMapTextureCacheItem;
    procedure Clean(const aX, aY: integer);
    procedure CleanAll(const aDisposeTextures: boolean);
    destructor Destroy; override;
  end;

implementation

uses
  Common;

{ TMapTextureCache }

constructor TMapTextureCache.Create;
begin
  inherited Create;
  Initialize;
end;

procedure TMapTextureCache.Initialize;
  procedure PreClean;
  var
    i: integer;
  begin
    for i := 0 to Length(Cache) - 1 do
      Cache[i] := nil;
  end;

begin
  fLog := TNoLog.Create;
  SetLength(fCache, MAX_MAP_TEXTURE_CACHE);
  PreClean;
end;

procedure TMapTextureCache.SetLog(const aLog: ILog);
begin
  FreeLog(fLog);
  fLog := aLog;
end;

function TMapTextureCache.GetDefaultDistance(const aTileWidth: integer): integer;
begin
  result := Round((wndWidth div aTileWidth) * 1.5);
end;

function TMapTextureCache.GetCacheItem(const x, y: integer
  ): PMapTextureCacheItem;
var
  i: integer;
  item: PMapTextureCacheItem;
begin
  result := nil;
  for i := 0 to Length(Cache) - 1 do
  begin
    item := Cache[i];
    if item = nil then continue;
    if item^.x <> x then continue;
    if item^.y <> y then continue;
    result := item;
    break;
  end;
end;

procedure TMapTextureCache.Finalize;
begin
  CleanAll(GlobalEngineRunning);
  SetLength(fCache, 0);
  FreeLog(fLog);
end;

function TMapTextureCache.AddNew: PMapTextureCacheItem;
var
  i: integer;
begin
  for i := 0 to Length(Cache) - 1 do
    if Cache[i] = nil then
    begin
      New(result);
      Cache[i] := result;
      break;
    end;
  if result = nil then
    Log.Write('Warning: insertion of a cache item impossible');
end;

procedure TMapTextureCache.Clean(const aX, aY: integer);
//{$DEFINE DEBUG_THIS}
var
  i: integer;
  item: PMapTextureCacheItem;
  distance: integer;
begin
  {$IFDEF DEBUG_THIS}
  AssertAssigned(Engine, 'Engine');
  {$ENDIF}
  for i := 0 to Length(Cache) - 1 do
  begin
    if Cache[i] = nil then
      continue;
    item := Cache[i];
    distance := Abs(item^.x - Ax) + Abs(item^.y - aY);
    if distance > CleaningDistance then
    begin
      {$IFDEF DEBUG_THIS}
      Log.Write('Now cleaning: ' + IntToStr(item^.x) + ':' + IntToStr(item^.y)
        + '(' + IntToStr(distance) + ' > ' + IntToStr(CleaningDistance) + ')');
      {$ENDIF}
      Engine.DisposeTexture(item^.texture);
      Dispose(item);
      Cache[i] := nil;
    end;
  end;
end;
{$UNDEF DEBUG_THIS}

procedure TMapTextureCache.CleanAll(const aDisposeTextures: boolean);
var
  item: PMapTextureCacheItem;
begin
  for item in Cache do
  if item <> nil then
  begin
    if aDisposeTextures then
      Engine.DisposeTexture(item^.texture);
    Dispose(item);
  end;
end;

destructor TMapTextureCache.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

end.

