unit ZenGLTextureLoader;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  SyncObjs,
  Contnrs,

  NiceExceptions,
  LogEntityFace,
  LogEntity,
  LogManager,

  zgl_textures;

type

  { TTextureLoader }

  TTextureLoader = class(TComponent)
  public
    constructor Create(const aOwner: TComponent); reintroduce;
  private type
    TTextureLoadQuery = record
      debugNumber: integer;
      fileName: string;
      result : zglPTexture;
      event: TSimpleEvent;
    end;
    PTextureLoadQuery = ^TTextureLoadQuery;
  public type
    ETimeout = class(Exception);
  private
    fDebug: boolean;
    fLog: ILog;
    fQueue: TQueue;
    fQueueLock: TCriticalSection;
    property Que: TQueue read fQueue;
    property QueLock: TCriticalSection read fQueueLock;
    procedure Initialize;
    function AddQuery(const aFileName: string): PTextureLoadQuery;
    function PopQuery: PTextureLoadQuery;
    procedure DisposeQuery(var aQuery: PTextureLoadQuery);
    procedure Finalize;
  public const
    LoadTextureTimeout = 1000 * 10;
  public
    property DEBUG: boolean read fDebug;
    property Log: ILog read fLog;
    procedure EnableDebug(const aLogManager: TLogManager);
    function LoadTexture(const aFileName: string): zglPTexture;
    procedure ProcessLoadTexture;
    destructor Destroy; override;
  end;

implementation

{ TTextureLoader }

constructor TTextureLoader.Create(const aOwner: TComponent);
begin
  inherited Create(aOwner);
  Initialize;
end;

procedure TTextureLoader.Initialize;
begin
  fQueueLock := TCriticalSection.Create;
  fQueue := TQueue.Create;
end;

function TTextureLoader.AddQuery(const aFileName: string): PTextureLoadQuery;
begin
  QueLock.Enter;
  New(result);
  result^.fileName := aFileName;
  result^.result := nil;
  result^.event := TSimpleEvent.Create;
  Que.Push(result);
  QueLock.Leave;
end;

function TTextureLoader.PopQuery: PTextureLoadQuery;
begin
  QueLock.Enter;
  if Que.Count = 0 then
    result := nil
  else
    result := PTextureLoadQuery(Que.Pop);
  QueLock.Leave;
end;

procedure TTextureLoader.DisposeQuery(var aQuery: PTextureLoadQuery);
begin
  FreeAndNil(aQuery^.event);
  Dispose(aQuery);
  aQuery := nil;
end;

procedure TTextureLoader.Finalize;
begin
  FreeAndNil(fQueue);
  FreeAndNil(fQueueLock);
  FreeLog(fLog);
end;

procedure TTextureLoader.EnableDebug(const aLogManager: TLogManager);
begin
  AssertArgumentAssigned(aLogManager, 'aLogManager');
  fLog := TLog.Create(aLogManager, 'TextureLoader');
  fDebug := true;
end;

function TTextureLoader.LoadTexture(const aFileName: string): zglPTexture;
var
  query: PTextureLoadQuery;
  waitResult: TWaitResult;
  debugNumber: integer;
begin
  if DEBUG then
  begin
    debugNumber := random(1000);
    Log.Write('Creating query #' + IntToStr(debugNumber) + ': '
      + LineEnding + '"' + aFileName + '"');
  end;
  query := AddQuery(aFileName);
  query^.debugNumber := debugNumber;
  if DEBUG then
    Log.Write('Now waiting #' + IntToStr(debugNumber));
  waitResult := query^.event.WaitFor(LoadTextureTimeout);
  if waitResult = wrTimeout then
    raise ETimeout.Create(IntToStr(LoadTextureTimeout));
  result := query^.result;
  DisposeQuery(query);
  if DEBUG then
    Log.Write('Query closed #' + IntToStr(debugNumber));
end;

procedure TTextureLoader.ProcessLoadTexture;
var
  query: PTextureLoadQuery;
begin
  query := PopQuery;
  if query = nil then
    exit;
  if DEBUG then
    Log.Write('Processing query #' + IntToStr(query^.debugNumber));
  query^.result := tex_LoadFromFile(query^.fileName);
  query^.event.SetEvent;
end;

destructor TTextureLoader.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

end.

