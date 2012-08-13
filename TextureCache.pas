unit TextureCache;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  zgl_textures,
  zgl_window,

  EngineManager;

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
    fEngine: TEngineManager;
    fCache: TCache;
    fDistance: integer;
    property Cache: TCache read fCache;
    procedure Initialize;
    function GetCacheItem(const x, y: integer): PMapTextureCacheItem; inline;
    procedure Finalize;
  public
    property Engine: TEngineManager read fEngine write fEngine;
    property Distance: integer read fDistance write fDistance;
    function GetDefaultDistance(const aTileWidth: integer): integer;
    property Access[const x, y: integer]: PMapTextureCacheItem read GetCacheItem;
    function AddNew: PMapTextureCacheItem;
    procedure Clean(const aX, aY: integer);
    procedure CleanAll(const aDisposeTextures: boolean);
    destructor Destroy; override;
  end;

implementation

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
  SetLength(fCache, MAX_MAP_TEXTURE_CACHE);
  PreClean;
end;

function TMapTextureCache.GetDefaultDistance(const aTileWidth: integer): integer;
begin
  result := wndWidth div aTileWidth;
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
    if item <> nil then
    begin
      if (item^.x = x) and (item^.y = y) then
      begin
        result := item;
        break;
      end;
    end;
  end;
end;

procedure TMapTextureCache.Finalize;
begin
  SetLength(fCache, 0);
end;

function TMapTextureCache.AddNew: PMapTextureCacheItem;
var
  i: integer;
begin
  result := nil;
  for i := 0 to Length(Cache) - 1 do
    if Cache[i] = nil then
    begin
      New(result);
      Cache[i] := result;
      break;
    end;
end;

procedure TMapTextureCache.Clean(const aX, aY: integer);
var
  i: integer;
  item: PMapTextureCacheItem;
begin
  for i := 0 to Length(Cache) - 1 do
  begin
    if Cache[i] = nil then
      continue;
    item := Cache[i];
    if item^.x + item^.y > Distance then
    begin
      Engine.DisposeTexture(item^.texture);
      Dispose(item);
      Cache[i] := nil;
    end;
  end;
end;

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

