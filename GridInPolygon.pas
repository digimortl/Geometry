unit GridInPolygon;

interface

uses
  SysUtils, Geometry;

type
  P2dByteArray = ^T2dByteArray;
  T2dByteArray = array [0..MaxInt div SizeOf(PByteArray) - 1] of PByteArray;  

function GridInPolygon(Polygon: PPolygon; X, Y: PFloatArray; CountX,
  CountY: Integer; Grid: P2dByteArray; bBounded: Boolean): Boolean;

implementation

uses
  Math;

type
  PSegmentNode = ^TSegmentNode;
  TSegmentNode = record
    left: PVertex;
    right: PVertex;
    Next: PSegmentNode;
    Less: PSegmentNode;
    More: PSegmentNode;
  end;

  PPSegmentArray = ^PSegmentArray;
  PSegmentArray = array [0..MaxInt div SizeOf(PSegmentNode) - 1] of PSegmentNode;

  PCrossNode = ^TCrossNode;
  TCrossNode = record
    y: Float;
    Next: PCrossNode;
  end;

function GetLeft(v0, v1: PVertex): PVertex;
begin
  if IsLs(v0.x, v1.x, EpsFloat) then
    Result := v0
  else
    if IsLs(v1.x, v0.x, EpsFloat) then
      Result := v1
    else
      if IsLs(v0.y, v1.y, EpsFloat) then
        Result := v0
      else
        Result := v1;
end;

function GetRight(v0, v1: PVertex): PVertex;
begin
  if IsGt(v0.x, v1.x, EpsFloat) then
    Result := v0
  else
    if IsGt(v1.x, v0.x, EpsFloat) then
      Result := v1
    else
      if IsGt(v0.y, v1.y, EpsFloat) then
        Result := v0
      else
        Result := v1;
end;

function GetDy(v0, v1: PVertex): Float;
var
  left, right: PVertex;
begin
  left := GetLeft(v0, v1);
  right := GetRight(v0, v1);
  if IsEq(left.x, right.x, EpsFloat) then
    if IsLs(right.y - left.y, 0.0, EpsFloat) then
      Result := -MaxFloat
    else
      Result := MaxFloat
  else
    Result := (right.y - left.y) / (right.x - left.x);
end;

{ SegmentNode functions }

procedure InsertSegmentToTree(var Root: PSegmentNode; left, right: PVertex);
begin
  if Root = nil then
  begin
    Root := AllocMem(SizeOf(TSegmentNode));
    Root.left := left;
    Root.right := right;
  end
  else if IsLs(left.x, Root.left.x, EpsFloat) then
    InsertSegmentToTree(Root.Less, left, right)
  else if IsGt(left.x, Root.left.x, EpsFloat) then
    InsertSegmentToTree(Root.More, left, right)
  else if IsLs(left.y, Root.left.y, EpsFloat) then
    InsertSegmentToTree(Root.Less, left, right)
  else if IsGt(left.y, Root.left.y, EpsFloat) then
    InsertSegmentToTree(Root.More, left, right)
  else if IsLs(GetDy(left, right), GetDy(Root.left, Root.right), EpsFloat) then
    InsertSegmentToTree(Root.Less, left, right)
  else
    InsertSegmentToTree(Root.More, left, right);
end;

procedure InsertSegmentToList(var Root: PSegmentNode; Node: PSegmentNode);
begin
  if Node = nil then
    Exit;
  Node.Next := Root;
  Root := Node;
end;

procedure RemoveSegmentFromList(var Root: PSegmentNode; Prev, Node: PSegmentNode);
begin
  if (Root = nil) or (Node = nil) then
    Exit;

  if Prev = nil then
    Root := Node.Next
  else
    Prev.Next := Node.Next;
  Node.Next := nil;
end;

procedure GetSegmentCount(Root: PSegmentNode; var Count: Integer);
begin
  if Root.Less <> nil then GetSegmentCount(Root.Less, Count);
  Inc(Count);
  if Root.More <> nil then GetSegmentCount(Root.More, Count);
end;

procedure GetSegmentArray(Root: PSegmentNode; P: PPSegmentArray; var Count: Integer);
begin
  if Root.Less <> nil then GetSegmentArray(Root.Less, P, Count);
  P[Count] := Root;
  Inc(Count);
  if Root.More <> nil then GetSegmentArray(Root.More, P, Count);
end;

procedure ResetSegments(var Root: PSegmentNode);
begin
  if Root <> nil then
  begin
    ResetSegments(Root.Less);
    ResetSegments(Root.More);
    Geometry.Free(Pointer(Root));
  end;
end;

{ CrossNode functions }

procedure InsertCross(var Root: PCrossNode; y: Float);
var
  ExistingNode: PCrossNode;
begin
  if Root = nil then
  begin
    Root := AllocMem(SizeOf(TCrossNode));
    Root.y := y;
  end
  else if IsLs(y, Root.y, EpsFloat) then
  begin
    ExistingNode := Root;
    Root := AllocMem(SizeOf(TCrossNode));
    Root.y := y;
    Root.Next := ExistingNode;
  end
  else
    InsertCross(Root.Next, y);
end;

function GetCrossCount(Root: PCrossNode): Integer;
begin
  Result := 0;
  while Root <> nil do
  begin
    Inc(Result);
    Root := Root.Next;
  end;
end;

procedure ResetCross(var Root: PCrossNode);
var
  Next: PCrossNode;
begin
  while Root <> nil do
  begin
    Next := Root.Next;
    Geometry.Free(Pointer(Root));
    Root := Next;
  end;
end;

{ Implement functions }

function GetSegments(Polygon: PPolygon): PSegmentNode;
var
  I, J: Integer;
  v0, v1: PVertex;
begin
  Result := nil;
  if Polygon <> nil then
    for I := 0 to Polygon.Count - 1 do
      for J := 0 to Polygon.Contours[I].Count - 1 do
      begin
        v0 := @Polygon.Contours[I].Vertices[J];
        v1 := @Polygon.Contours[I].Vertices[GetNext(J, Polygon.Contours[I].Count)];
        if not PntsEqu(v0, v1) then
          InsertSegmentToTree(Result, GetLeft(v0, v1), GetRight(v0, v1));
      end;
end;

procedure ResetGrid(Grid: P2dByteArray; CountX, CountY: Integer);
var
  I, J: Integer;
begin
  for I := 0 to CountX - 1 do
    for J := 0 to CountY - 1 do
      Grid[I][J] := 0;
end;

function GetCrosses(var Segments: PSegmentNode; x: Float): PCrossNode;
var
  Prev, Curr, Next: PSegmentNode;
  t: Float;
begin
  Result := nil;
  Prev := nil;
  Curr := Segments;
  while Curr <> nil do
  begin
    Next := Curr.Next;
    if IsGe(x, Curr.left.x, EpsFloat) and
       IsLs(x, Curr.right.x, EpsFloat) then
    begin
      t := (x - Curr.left.x) / (Curr.right.x - Curr.left.x);
      InsertCross(Result, Curr.left.y + t * (Curr.right.y - Curr.left.y));
      Prev := Curr;
    end
    else
      RemoveSegmentFromList(Segments, Prev, Curr);
    Curr := Next;
  end;
end;

function GridInPolygon(Polygon: PPolygon; X, Y: PFloatArray; CountX,
  CountY: Integer; Grid: P2dByteArray; bBounded: Boolean): Boolean;
var
  I, J: Integer;
  Segments: PSegmentNode;
  SegmentCount: Integer;
  SegmentArray: PPSegmentArray;
  TmpSegments: PSegmentNode;
  Crosses: PCrossNode;
  CrossNode: PCrossNode;
  CrossIndex: Integer;
begin
  Result := False;
  if (Polygon = nil) or (X = nil) or (Y = nil) or (CountX = 0) or (CountY = 0) or (Grid = nil) then Exit;

  Segments := GetSegments(Polygon);
  if Segments <> nil then
    try
      SegmentCount := 0;
      GetSegmentCount(Segments, SegmentCount);
      if SegmentCount > 0 then
      begin
        SegmentArray := AllocMem(SegmentCount * SizeOf(PSegmentNode));
        SegmentCount := 0;
        GetSegmentArray(Segments, SegmentArray, SegmentCount);
        try
          ResetGrid(Grid, CountX, CountY);
          TmpSegments := nil;
          J := 0;
          for I := 0 to CountX - 1 do
          begin
            if IsLs(X[I], SegmentArray[0].left.x, EpsFloat) then
              Continue
            else if IsGt(X[I], SegmentArray[SegmentCount - 1].left.x, EpsFloat) and
               (TmpSegments = nil) then
              Break;

            while J < SegmentCount do
            begin
              if IsGe(X[I], SegmentArray[J].left.x, EpsFloat) and
                 IsLs(X[I], SegmentArray[J].right.x, EpsFloat) then
                InsertSegmentToList(TmpSegments, SegmentArray[J])
              else
                if IsLs(X[I], SegmentArray[J].left.x, EpsFloat) then
                  Break;
              Inc(J);
            end;

            Crosses := GetCrosses(TmpSegments, X[I]);
            if Crosses <> nil then
              try
                CrossNode := Crosses;
                CrossIndex := 0;
                while (CrossNode <> nil) and (CrossNode.Next <> nil) do
                begin
                  while CrossIndex < CountY do
                  begin
                    if (IsGt(Y[CrossIndex], CrossNode.Next.y, EpsFloat) and bBounded) or
                       (IsGe(Y[CrossIndex], CrossNode.Next.y, EpsFloat) and (not bBounded)) then
                      Break;

                    if (IsGe(Y[CrossIndex], CrossNode.y, EpsFloat) and bBounded) or
                       (IsGt(Y[CrossIndex], CrossNode.y, EpsFloat) and (not bBounded)) then
                      Grid[I][CrossIndex] := 1;

                    Inc(CrossIndex);
                  end;
                  CrossNode := CrossNode.Next.Next;
                end;
              finally
                ResetCross(Crosses);
              end;
          end;
          Result := True;
        finally
          Geometry.Free(Pointer(SegmentArray));
        end;
      end;
    finally
      ResetSegments(Segments);
    end;
end;

end.
