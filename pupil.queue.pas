unit Pupil.Queue;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, Pupil.Types;

type

   { TGazeOnSurfaceQueue }

   TGazeOnSurfaceQueue = class (specialize TThreadList<TGazeOnSurface>)
     type
       TGazeOnSurfaceList = specialize TList<TGazeOnSurface>;
     public
       constructor Create;
       //destructor Destroy; override;
       procedure Enqueue(AValue : TGazeOnSurface);
       function Dequeue : TGazeOnSurface;
   end;

implementation

{ TGazeOnSurfaceQueue }

constructor TGazeOnSurfaceQueue.Create;
begin
  inherited Create;
  Duplicates := dupAccept;
end;

procedure TGazeOnSurfaceQueue.Enqueue(AValue : TGazeOnSurface);
begin
  Add(AValue);
end;

function TGazeOnSurfaceQueue.Dequeue: TGazeOnSurface;
var
  LList : TGazeOnSurfaceList;
begin
  LList := LockList;
  try
    with LList do begin
      Result := Extract(First);
    end;
  finally
    UnlockList;
  end;
end;

end.

