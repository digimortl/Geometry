unit Cycles;

interface

uses
  Geometry;

type
  PTypedContour = ^TTypedContour;
  TTypedContour = record
    Contour: TContour;
    Types: PIntegerArray;
    TypeCount: Integer;
  end;

  PTypedContourArray = ^TTypedContourArray;
  TTypedContourArray = array [0..MaxInt div SizeOf(TTypedContour) - 1] of TTypedContour;

function FindCycles(const Src: PPolygon): TPolygon;
function FindTypedCycles(const Src: PTypedContourArray; const SrcCount: Integer;
  var Dst: PTypedContourArray; var DstCount: Integer): Boolean;

implementation

uses
  Math;

{ ======================== Private constants ================================== }

const
  Head  = 0;
  Tail  = 1;

  Left  = 0;
  Right = 1;

{ ======================== Private types declaration ========================== }

type
  { Cross list node }
  PCrossNode = ^TCrossNode;
  TCrossNode = record
    t: Float;
    Next: PCrossNode;
  end;

  { Segment list node }
  PSegmentNode = ^TSegmentNode;
  TSegmentNode = record
    ID: Integer;
    Crosses: PCrossNode;
    EndPoints: array [Head..Tail] of TVertex;
    Parents: array [Left..Right] of PSegmentNode;
    Childs: array [Left..Right] of PSegmentNode;
    TypeCode: Integer;
    Next: PSegmentNode;
    Prev: PSegmentNode;
  end;

  { Sortred list node }
  PSortedNode = ^TSortedNode;
  TSortedNode = record
    x, y, dy: Float;
    Segment: PSegmentNode;
    Less: PSortedNode;
    More: PSortedNode;
  end;

  { Vertex list node }
  PVertexNode = ^TVertexNode;
  TVertexNode = record
    Coords: TVertex;
    Next: PVertexNode;
  end;

  { Contour type node }
  PTypedNode = ^TTypedNode;
  TTypedNode = record
    TypeCode: Integer;
    Less: PTypedNode;
    More: PTypedNode;
  end;

  { Contour list node }
  PContourNode = ^TContourNode;
  TContourNode = record
    Count: Integer;
    Vertices: PVertexNode;
    Types: PTypedNode;
    Next: PContourNode;
  end;

{ ======================== Private functions ================================== }

{ Cross list functions}

function GetCrossCount(Root: PCrossNode): Integer;
begin
  Result := 0;
  while Root <> nil do
  begin
    Inc(Result);
    Root := Root.Next;
  end;
end;

procedure InsertCross(var Root: PCrossNode; t: Float);
var
  ExistingNode: PCrossNode;
begin
  if Root = nil then
  begin
    Root := AllocMem(SizeOf(TCrossNode));
    Root.t := t;
  end
  else
    if IsLs(t, Root.t, EpsFloat) then
    begin
      ExistingNode := Root;
      Root := AllocMem(SizeOf(TCrossNode));
      Root.t := t;
      Root.Next := ExistingNode;
    end
    else
      if IsGt(t, Root.t, EpsFloat) then
        InsertCross(Root.Next, t);
end;

procedure ResetCrosses(var Root: PCrossNode);
var
  Next: PCrossNode;
begin
  while Root <> nil do
  begin
    Next := Root.Next;
    Dispose(Root);
    Root := Next;
  end;
end;

{ Segment list functions }

function GetSegmentCount(Root: PSegmentNode): Integer;
begin
  Result := 0;
  while Root <> nil do
  begin
    Inc(Result);
    Root := Root.Next;
  end;
end;

function InsertSegment(var Root: PSegmentNode): PSegmentNode;
begin
  Result := AllocMem(SizeOf(TSegmentNode));
  Result.Next := Root;
  if Root <> nil then Root.Prev := Result;
  Root := Result;
end;

procedure RemoveSegment(var Root: PSegmentNode; Node: PSegmentNode);
begin
  if (Root <> nil) and (Node <> nil) then
    if Root = Node then
    begin
      Root := Node.Next;
      if Root <> nil then Root.Prev := nil;
      ResetCrosses(Node.Crosses);
      Dispose(Node);
    end
    else
    begin
      if Node.Prev <> nil then Node.Prev.Next := Node.Next;
      if Node.Next <> nil then Node.Next.Prev := Node.Prev;
      ResetCrosses(Node.Crosses);
      Dispose(Node);
    end;
end;

procedure HideSegment(Node: PSegmentNode);
begin
  if Node <> nil then
  begin
    { Hide childs }
    if Node.Childs[Left] <> nil then
      if Node.Childs[Left].Parents[Left] = Node then
        Node.Childs[Left].Parents[Left] := nil
      else
        Node.Childs[Left].Parents[Right] := nil;

    if Node.Childs[Right] <> nil then
      if Node.Childs[Right].Parents[Left] = Node then
        Node.Childs[Right].Parents[Left] := nil
      else
        Node.Childs[Right].Parents[Right] := nil;

    { Hide parents }
    if Node.Parents[Left] <> nil then
      if Node.Parents[Left].Childs[Left] = Node then
        Node.Parents[Left].Childs[Left] := nil
      else
        Node.Parents[Left].Childs[Right] := nil;

    if Node.Parents[Right] <> nil then
      if Node.Parents[Right].Childs[Left] = Node then
        Node.Parents[Right].Childs[Left] := nil
      else
        Node.Parents[Right].Childs[Right] := nil;
  end;
end;

procedure ResetSegments(var Root: PSegmentNode);
var
  Next: PSegmentNode;
begin
  while Root <> nil do
  begin
    Next := Root.Next;
    ResetCrosses(Root.Crosses);
    Dispose(Root);
    Root := Next;
  end;
end;

{ Sorted list functions }

procedure InsertSorted(var Root: PSortedNode; var Count: Integer; x, y, dy: Float; Segment: PSegmentNode);
begin
  if Root = nil then
  begin
    Root := AllocMem(SizeOf(TSortedNode));
    Root.x := x;
    Root.y := y;
    Root.dy := dy;
    Root.Segment := Segment;
    Inc(Count);
  end
  else
    if IsLs(x, Root.x, EpsFloat) then
      InsertSorted(Root.Less, Count, x, y, dy, Segment)
    else
      if IsGt(x, Root.x, EpsFloat) then
        InsertSorted(Root.More, Count, x, y, dy, Segment)
      else
        if IsLs(y, Root.y, EpsFloat) then
          InsertSorted(Root.Less, Count, x, y, dy, Segment)
        else
          if IsGt(y, Root.y, EpsFloat) then
            InsertSorted(Root.More, Count, x, y, dy, Segment)
          else
            if IsLs(dy, Root.dy, EpsFloat) then
              InsertSorted(Root.Less, Count, x, y, dy, Segment)
            else
              InsertSorted(Root.More, Count, x, y, dy, Segment);
end;

procedure LoadSortedArray(Root: PSortedNode; var Index: Integer; P: PPointerArray);
begin
  if Root.Less <> nil then
    LoadSortedArray(Root.Less, Index, P);
  P[Index] := Root.Segment;
  Inc(Index);
  if Root.More <> nil then
    LoadSortedArray(Root.More, Index, P);
end;

procedure ResetSorteds(var Root: PSortedNode);
begin
  if Root <> nil then
  begin
    ResetSorteds(Root.Less);
    ResetSorteds(Root.More);
    Geometry.Free(Pointer(Root));
  end;
end;

{ Contour type node functions }

procedure InsertTypedNode(var Root: PTypedNode; TypeCode: Integer);
begin
  if Root = nil then
  begin
    Root := AllocMem(SizeOf(TTypedNode));
    Root.TypeCode := TypeCode;
  end
  else if TypeCode < Root.TypeCode then
    InsertTypedNode(Root.Less, TypeCode)
  else if TypeCode > Root.TypeCode then
    InsertTypedNode(Root.More, TypeCode);
end;

procedure GetTypedArrayCount(Root: PTypedNode; var Count: Integer);
begin
  if Root.Less <> nil then
    GetTypedArrayCount(Root.Less, Count);
  Inc(Count);
  if Root.More <> nil then
    GetTypedArrayCount(Root.More, Count);
end;


procedure LoadTypedArray(Root: PTypedNode; var Index: Integer; P: PIntegerArray);
begin
  if Root.Less <> nil then
    LoadTypedArray(Root.Less, Index, P);
  P[Index] := Root.TypeCode;
  Inc(Index);
  if Root.More <> nil then
    LoadTypedArray(Root.More, Index, P);
end;

procedure ResetTypes(var Root: PTypedNode);
begin
  if Root <> nil then
  begin
    ResetTypes(Root.Less);
    ResetTypes(Root.More);
    Geometry.Free(Pointer(Root));
  end;
end;

{ Vertex list functions }

function GetVertexCount(Root: PVertexNode): Integer;
begin
  Result := 0;
  while Root <> nil do
  begin
    Inc(Result);
    Root := Root.Next;
  end;
end;

function InsertVertex(var Root: PVertexNode): PVertexNode;
begin
  Result := AllocMem(SizeOf(TVertexNode));
  Result.Next := Root;
  Root := Result;
end;

procedure ResetVertices(var Root: PVertexNode);
var
  Next: PVertexNode;
begin
  while Root <> nil do
  begin
    Next := Root.Next;
    Dispose(Root);
    Root := Next;
  end;
end;

{ Contour list functions }

function GetContourCount(Root: PContourNode): Integer;
begin
  Result := 0;
  while Root <> nil do
  begin
    Inc(Result);
    Root := Root.Next;
  end;
end;

function InsertContour(var Root: PContourNode): PContourNode;
begin
  Result := AllocMem(SizeOf(TContourNode));
  Result.Next := Root;
  Root := Result;
end;

procedure ResetContours(var Root: PContourNode);
var
  Next: PContourNode;
begin
  while Root <> nil do
  begin
    Next := Root.Next;
    ResetVertices(Root.Vertices);
    ResetTypes(Root.Types);
    Dispose(Root);
    Root := Next;
  end;
end;

{ Implement functions }

function GetMax(v0, v1: Float; p0, p1: Pointer): Pointer;
begin
  if v0 > v1 then
    Result := p0
  else
    Result := p1;
end;

function GetMin(v0, v1: Float; p0, p1: Pointer): Pointer;
begin
  if v0 < v1 then
    Result := p0
  else
    Result := p1;
end;

function GetLeft(Node: PSegmentNode): PVertex;
begin
  if IsLs(Node.EndPoints[Head].x, Node.EndPoints[Tail].x, EpsFloat) then
    Result := @Node.EndPoints[Head]
  else
    if IsGt(Node.EndPoints[Head].x, Node.EndPoints[Tail].x, EpsFloat) then
      Result := @Node.EndPoints[Tail]
    else
      if IsLs(Node.EndPoints[Head].y, Node.EndPoints[Tail].y, EpsFloat) then
        Result := @Node.EndPoints[Head]
      else
        Result := @Node.EndPoints[Tail];
end;

function GetRight(Node: PSegmentNode): PVertex;
begin
  if IsGt(Node.EndPoints[Head].x, Node.EndPoints[Tail].x, EpsFloat) then
    Result := @Node.EndPoints[Head]
  else
    if IsLs(Node.EndPoints[Head].x, Node.EndPoints[Tail].x, EpsFloat) then
      Result := @Node.EndPoints[Tail]
    else
      if IsGt(Node.EndPoints[Head].y, Node.EndPoints[Tail].y, EpsFloat) then
        Result := @Node.EndPoints[Head]
      else
        Result := @Node.EndPoints[Tail];
end;

function GetIncrement(Node: PSegmentNode): Float;
var
  l, r: PVertex;
begin
  l := GetLeft(Node);
  r := GetRight(Node);
  if IsEq(l.x, r.x, EpsFloat) then
    if IsLs(r.y - l.y, 0.0, EpsFloat) then
      Result := -MaxFloat
    else
      Result := MaxFloat
  else
    Result := (r.y - l.y) / (r.x - l.x);
end;

procedure LoadSegments(var Root: PSegmentNode; Src: PPolygon);
var
  I, J: Integer;
  Node: PSegmentNode;
begin
  for I := 0 to Src.Count - 1 do
    for J := 0 to Src.Contours[I].Count - 2 do
      if not PntsEqu(@Src.Contours[I].Vertices[J], @Src.Contours[I].Vertices[J + 1]) then
      begin
        Node := InsertSegment(Root);
        Node.EndPoints[Head] := Src.Contours[I].Vertices[J];
        Node.EndPoints[Tail] := Src.Contours[I].Vertices[J + 1];
      end;
end;

procedure LoadTypedSegments(var Root: PSegmentNode; Src: PTypedContourArray; SrcCount: Integer);
var
  I, J: Integer;
  Node: PSegmentNode;
begin
  if (Src <> nil) and (SrcCount <> 0)then
    for I := 0 to SrcCount - 1 do
      for J := 0 to Src[I].Contour.Count - 2 do
        if not PntsEqu(@Src[I].Contour.Vertices[J], @Src[I].Contour.Vertices[J + 1]) then
        begin
          Node := InsertSegment(Root);
          Node.EndPoints[Head] := Src[I].Contour.Vertices[J];
          Node.EndPoints[Tail] := Src[I].Contour.Vertices[J + 1];
          if Src[I].TypeCount > 0 then Node.TypeCode := Src[I].Types[0];
        end;
end;

type
  TForEachFunc = procedure(var Root: PSegmentNode; Node: PSegmentNode);
  TSearchFunc = procedure (N0, N1: PSegmentNode);

procedure ForEachSegments(var Root: PSegmentNode; F: TForEachFunc);
var
  Node, Next: PSegmentNode;
begin
  Node := Root;
  while Node <> nil do
  begin
    Next := Node.Next;
    F(Root, Node);
    Node := Next;
  end;
end;

procedure GetSortedSegments(Root: PSegmentNode; var P: PPointerArray; var Count: Integer);
var
  left: PVertex;
  Sorts: PSortedNode;
begin
  P := nil;
  Count := 0;
  { Create sorted segment list }
  Sorts := nil;
  while Root <> nil do
  begin
    left := GetLeft(Root);
    InsertSorted(Sorts, Count, left.x, left.y, GetIncrement(Root), Root);
    Root := Root.Next;
  end;

  { Create sorted segment array }
  if Count > 0 then
  begin
    GetMem(P, SizeOf(PSortedNode) * Count);
    Count := 0;
    LoadSortedArray(Sorts, Count, P);
  end;

  { Clean up }
  ResetSorteds(Sorts);
end;

function GetRightIndex(P: PPointerArray; L, R: Integer; x: Float): Integer;
var
  M: Integer;
begin
  while L < R - 1 do
  begin
    M := (L + R) shr 1;
    if IsGe(x, GetLeft(P[M]).x, EpsFloat) then
      L := M
    else
      R := M;
  end;
  Result := R;
end;

procedure SearchSegments(var Root: PSegmentNode; F: TSearchFunc);
var
  I, L, R, Count: Integer;
  P: PPointerArray;
begin
  GetSortedSegments(Root, P, Count);
  if (P <> nil) and (Count > 0) then
  begin
    for L := 0 to Count - 2 do
    begin
      R := GetRightIndex(P, L + 1, Count - 1, GetRight(P[L]).x);
      for I := L + 1 to R do F(P[L], P[I]);
    end;
    FreeMem(P);
  end;
end;

procedure SegmentsIntersects(S0, S1: PSegmentNode);
var
  n: TVertex;
  t0, t1: Float;
  v0, v1: PVertex;
  v2, v3: PVertex;
begin
  v0 := @S0.EndPoints[Head];
  v1 := @S0.EndPoints[Tail];
  v2 := @S1.EndPoints[Head];
  v3 := @S1.EndPoints[Tail];
  if not (
    IsGt(Min(v0.x, v1.x), Max(v2.x, v3.x), EpsFloat) or
    IsLs(Max(v0.x, v1.x), Min(v2.x, v3.x), EpsFloat) or
    IsGt(Min(v0.y, v1.y), Max(v2.y, v3.y), EpsFloat) or
    IsLs(Max(v0.y, v1.y), Min(v2.y, v3.y), EpsFloat)
  ) then
    case LinesCross(v0, v1, v2, v3, t0, t1) of
      lnSkewCross:
      begin
        if IsGt(t0, 0.0, EpsFloat) and IsLs(t0, 1.0, EpsFloat) then
          InsertCross(S0.Crosses, t0);
        if IsGt(t1, 0.0, EpsFloat) and IsLs(t1, 1.0, EpsFloat) then
          InsertCross(S1.Crosses, t1);
      end;
      lnCollinear:
      begin
        { Test v0-v1 with Norm v2 }
        n := Flip(v2, v3);
        if LinesIntersect(v0, v1, v2, @n, t0) = lnSkew then
          if IsGt(t0, 0.0, EpsFloat) and IsLs(t0, 1.0, EpsFloat) then
            InsertCross(S0.Crosses, t0);
        { Test v0-v1 with Norm v3 }
        n := Flip(v3, v2);
        if LinesIntersect(v0, v1, v3, @n, t0) = lnSkew then
          if IsGt(t0, 0.0, EpsFloat) and IsLs(t0, 1.0, EpsFloat) then
            InsertCross(S0.Crosses, t0);
        { Test v2-v3 with Norm v0 }
        n := Flip(v0, v1);
        if LinesIntersect(v2, v3, v0, @n, t1) = lnSkew then
          if IsGt(t1, 0.0, EpsFloat) and IsLs(t1, 1.0, EpsFloat) then
            InsertCross(S1.Crosses, t1);
        { Test v2-v3 with Norm v1 }
        n := Flip(v1, v0);
        if LinesIntersect(v2, v3, v1, @n, t1) = lnSkew then
          if IsGt(t1, 0.0, EpsFloat) and IsLs(t1, 1.0, EpsFloat) then
            InsertCross(S1.Crosses, t1);
      end;
    end;
end;

procedure InsertIntersects(var Root: PSegmentNode; Node: PSegmentNode);
var
  V: TVertex;
  NewNode: PSegmentNode;
  Cross: PCrossNode;
begin
  if Node.Crosses <> nil then
  begin
    V := Node.EndPoints[Head];
    Cross := Node.Crosses;
    while Cross <> nil do
    begin
      NewNode := InsertSegment(Root);
      NewNode.EndPoints[Head] := V;
      NewNode.EndPoints[Tail] := Divide(@Node.EndPoints[Head], @Node.EndPoints[Tail], Cross.t);
      NewNode.TypeCode := Node.TypeCode;
      V := NewNode.EndPoints[Tail];
      Cross := Cross.Next;
    end;
    Node.EndPoints[Head] := V;
  end;
end;

procedure LabelingEquSegments(S0, S1: PSegmentNode);
begin
  S1.ID := Integer(
    PntsEqu(@S0.EndPoints[Head], @S1.EndPoints[Head]) and PntsEqu(@S0.EndPoints[Tail], @S1.EndPoints[Tail]) or
    PntsEqu(@S0.EndPoints[Head], @S1.EndPoints[Tail]) and PntsEqu(@S0.EndPoints[Tail], @S1.EndPoints[Head])
    );
end;

procedure RemoveEquSegments(var Root: PSegmentNode; Node: PSegmentNode);
begin
  if Node.ID <> 0 then
    RemoveSegment(Root, Node);
end;

function InsertParent(S0, S1: PSegmentNode): Boolean;
var
  d0, d1: Float;
  Tmp: PSegmentNode;
begin
  Result := False;
  if (S0.Parents[Left] <> nil) and (S0.Parents[Right] <> nil) then
    Exit  
  else
    if (S0.Parents[Left] = nil) and (S0.Parents[Right] = nil) then
    begin
      if IsGe(Cross(@S0.EndPoints[Tail], @S0.EndPoints[Head], @S0.EndPoints[Tail], @S1.EndPoints[Head]), 0.0, EpsFloat) then
        S0.Parents[Left] := S1
      else
        S0.Parents[Right] := S1;
      Result := True;
    end
    else
    begin
      if S0.Parents[Left] <> nil then
        Tmp := S0.Parents[Left]
      else
        Tmp := S0.Parents[Right];
      d0 := Cross(@S0.EndPoints[Tail], @S0.EndPoints[Head], @S0.EndPoints[Tail], @S1.EndPoints[Head]);
      d1 := Cross(@S0.EndPoints[Tail], @S0.EndPoints[Head], @S0.EndPoints[Tail], @Tmp.EndPoints[Head]);
      if IsNe(d0, d1, EpsFloat) then
      begin
        S0.Parents[Left] := GetMax(d0, d1, S1, Tmp);
        S0.Parents[Right] := GetMin(d0, d1, S1, Tmp);
        Result := True;
      end;
    end;
end;

procedure InsertChildren(S0, S1: PSegmentNode);
var
  d0, d1: Float;
  Tmp: PSegmentNode;
begin
  if (S0.Childs[Left] <> nil) and (S0.Childs[Right] <> nil) then
    Exit
  else
    if (S0.Childs[Left] = nil) and (S0.Childs[Right] = nil) then
    begin
      if InsertParent(S1, S0) then
        if IsGe(Cross(@S0.EndPoints[Head], @S0.EndPoints[Tail], @S0.EndPoints[Head], @S1.EndPoints[Tail]), 0.0, EpsFloat) then
          S0.Childs[Left] := S1
        else
          S0.Childs[Right] := S1;
    end
    else
    begin
      if S0.Childs[Left] <> nil then
        Tmp := S0.Childs[Left]
      else
        Tmp := S0.Childs[Right];
      d0 := Cross(@S0.EndPoints[Head], @S0.EndPoints[Tail], @S0.EndPoints[Head], @S1.EndPoints[Tail]);
      d1 := Cross(@S0.EndPoints[Head], @S0.EndPoints[Tail], @S0.EndPoints[Head], @Tmp.EndPoints[Tail]);
      if IsNe(d0, d1, EpsFloat) then
        if InsertParent(S1, S0) then
        begin
          S0.Childs[Left] := GetMax(d0, d1, S1, Tmp);
          S0.Childs[Right] := GetMin(d0, d1, S1, Tmp);
        end;
    end;
end;

procedure RelateSegments(S0, S1: PSegmentNode);
begin
  if PntsEqu(@S0.EndPoints[Head], @S1.EndPoints[Tail]) then
    InsertChildren(S1, S0)
  else
    if PntsEqu(@S0.EndPoints[Tail], @S1.EndPoints[Head]) then
      InsertChildren(S0, S1);
end;

function RemoveDeadChilds(var Root: PSegmentNode; Node: PSegmentNode): Boolean;
var
  ParentLeft, ParentRight: PSegmentNode;
begin
  Result := False;
  while (Node <> nil) and (Node.Childs[Left] = nil) and (Node.Childs[Right] = nil) do
  begin
    ParentLeft := Node.Parents[Left];
    ParentRight := Node.Parents[Right];

    HideSegment(Node);
    RemoveSegment(Root, Node);
    Node := nil;

    if ParentLeft <> nil then
      Node := ParentLeft
    else
      Node := ParentRight;
    Result := True;
  end;
end;

function RemoveDeadParents(var Root: PSegmentNode; Node: PSegmentNode): Boolean;
var
  ChildLeft, ChildRight: PSegmentNode;
begin
  Result := False;
  while (Node <> nil) and (Node.Parents[Left] = nil) and (Node.Parents[Right] = nil) do
  begin
    ChildLeft := Node.Childs[Left];
    ChildRight := Node.Childs[Right];

    HideSegment(Node);
    RemoveSegment(Root, Node);
    Node := nil;

    if ChildLeft <> nil then
      Node := ChildLeft
    else
      Node := ChildRight;
    Result := True;
  end;
end;

procedure RemoveDeadendSegments(var Root: PSegmentNode);
var
  Node: PSegmentNode;
begin
  { Remove deadend childs }
  Node := Root;
  while Node <> nil do
  begin
    if RemoveDeadChilds(Root, Node) then
      Node := Root
    else
      Node := Node.Next;
  end;

  { Remove deadend parents }
  Node := Root;
  while Node <> nil do
  begin
    if RemoveDeadParents(Root, Node) then
      Node := Root
    else
      Node := Node.Next;
  end;
end;

function TestClosed(Root: PSegmentNode; ID: Integer): PSegmentNode;
begin
  Result := Root;
  if Result <> nil then
    repeat
      if Result.Childs[Right] <> nil then
        Result := Result.Childs[Right]
      else
        Result := Result.Childs[Left];
      if (Result = Root) or ((Result <> nil) and (Result.ID = ID)) then
        Break;
      if Result <> nil then
        Result.ID := ID;
    until Result = nil;
end;

procedure MakeClosing(var Root: PSegmentNode; EntryNode: PSegmentNode; ContourNode: PContourNode);
var
  Node, Prev: PSegmentNode;
begin
  if EntryNode <> nil then
  begin
    ContourNode.Count := 0;
    Node := EntryNode;
    Prev := nil;
    repeat
      InsertVertex(ContourNode.Vertices).Coords := Node.EndPoints[Head];
      InsertTypedNode(ContourNode.Types, Node.TypeCode);
      Inc(ContourNode.Count);
      if Node.Childs[Right] <> nil then
        Node := Node.Childs[Right]
      else
        Node := Node.Childs[Left];
      HideSegment(Prev);
      RemoveSegment(Root, Prev);
      if Node = EntryNode then
      begin
        InsertVertex(ContourNode.Vertices).Coords := Node.EndPoints[Head];
        Inc(ContourNode.Count);
        Break;
      end;
      Prev := Node;
    until Node = nil;
    HideSegment(EntryNode);
    RemoveSegment(Root, EntryNode);
  end;
end;

procedure MakePolygon(var SegmentsRoot: PSegmentNode; var ContoursRoot: PContourNode);
var
  ID: Integer;
  EntryNode, SegmentNode: PSegmentNode;
begin
  ID := 1;
  SegmentNode := SegmentsRoot;
  while SegmentNode <> nil do
  begin
    EntryNode := TestClosed(SegmentNode, ID);
    if EntryNode <> nil then
    begin
      MakeClosing(SegmentsRoot, EntryNode, InsertContour(ContoursRoot));
      RemoveDeadendSegments(SegmentsRoot);
      SegmentNode := SegmentsRoot;
      Inc(ID);
    end
    else
      SegmentNode := SegmentNode.Next;
  end;
end;

function GetContours(Root: PContourNode): TPolygon;
var
  I, J: Integer;
  Node: PVertexNode;
begin
  Result.Count := GetContourCount(Root);
  if Result.Count > 0 then
  begin
    I := Result.Count - 1;
    Result.Contours := AllocMem(SizeOf(TContour) * Result.Count);
    while Root <> nil do
    begin
      if Root.Count > 0 then
      begin
        Result.Contours[I].Count := Root.Count;
        Result.Contours[I].Vertices := AllocMem(SizeOf(TVertex) * Result.Contours[I].Count);
        J := Result.Contours[I].Count - 1;
        Node := Root.Vertices;
        while Node <> nil do
        begin
          Result.Contours[I].Vertices[J] := Node.Coords;
          Dec(J);
          Node := Node.Next;
        end;
        Result.Contours[I].Hole := IsGt(GetSquare(@Result.Contours[I]), 0.0, EpsFloat);
      end;
      Dec(I);
      Root := Root.Next;
    end;
  end
  else
    FillChar(Result, SizeOf(TPolygon), 0);
end;

function GetTypedContours(Root: PContourNode; var Dst: PTypedContourArray; var DstCount: Integer): Boolean;
var
  I, J: Integer;
  Node: PVertexNode;
begin
  Result := False;
  Dst := nil;
  DstCount := GetContourCount(Root);
  if DstCount > 0 then
  begin
    I := DstCount - 1;
    Dst := AllocMem(SizeOf(TTypedContour) * DstCount);
    while Root <> nil do
    begin
      if Root.Count > 0 then
      begin
        Dst[I].Contour.Count := Root.Count;
        Dst[I].Contour.Vertices := AllocMem(SizeOf(TVertex) * Dst[I].Contour.Count);
        J := Dst[I].Contour.Count - 1;
        Node := Root.Vertices;
        while Node <> nil do
        begin
          Dst[I].Contour.Vertices[J] := Node.Coords;
          Dec(J);
          Node := Node.Next;
        end;
        Dst[I].Contour.Hole := IsGt(GetSquare(@Dst[I].Contour), 0.0, EpsFloat);
        Dst[I].TypeCount := 0;
        GetTypedArrayCount(Root.Types, Dst[I].TypeCount);
        Dst[I].Types := AllocMem(Dst[I].TypeCount * SizeOf(Integer));
        Dst[I].TypeCount := 0;
        LoadTypedArray(Root.Types, Dst[I].TypeCount, Dst[I].Types);
      end;
      Dec(I);
      Root := Root.Next;
    end;
  end;
end;

procedure SortTypedContoursBySquare(Src: PTypedContourArray; SrcCount: Integer);

  procedure _QuickSort(L, R: Integer);
  var
    I, J, M: Integer;
    S: Float;
    T: TTypedContour;
  begin
    I := L;
    J := R;
    M := (L + R) shr 1;
    S := Abs(GetSquare(@Src[M].Contour));
    repeat
      while IsGt(Abs(GetSquare(@Src[I].Contour)), S, EpsFloat) do Inc(I);
      while IsLs(Abs(GetSquare(@Src[J].Contour)), S, EpsFloat) do Dec(J);
      if I <= J then
      begin
        T := Src[I];
        Src[I] := Src[J];
        Src[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if J > L then _QuickSort(L, J);
    if I < R then _QuickSort(I, R);
  end;

  procedure _SelectionSort;
  var
    I, J: Integer;
    T: TTypedContour;
  begin
    for I := 0 to SrcCount - 2 do
      for J := SrcCount - 1 downto I + 1 do
        if IsLs(Abs(GetSquare(@Src[I].Contour)), Abs(GetSquare(@Src[J].Contour)), EpsFloat) then
        begin
          T := Src[I];
          Src[I] := Src[J];
          Src[J] := T;
        end;
  end;

begin
  if (Src <> nil) and (SrcCount > 0) then
    if SrcCount > 2 then
      _QuickSort(0, SrcCount - 1)
    else
      _SelectionSort;
end;

{ ======================== Public functions =================================== }

function FindCycles(const Src: PPolygon): TPolygon;
var
  Segments: PSegmentNode;
  Contours: PContourNode;
begin
  if (Src <> nil) and (Src.Contours <> nil) and (Src.Count > 0) then
  begin
    { Load segments from polygon arrays }
    Segments := nil;
    LoadSegments(Segments, Src);
    { Find intersections }
    SearchSegments(Segments, @SegmentsIntersects);
    { Insert intersections }
    ForEachSegments(Segments, @InsertIntersects);
    { Labeled equal segments }
    SearchSegments(Segments, @LabelingEquSegments);
    { Remove all equal segments }
    ForEachSegments(Segments, @RemoveEquSegments);
    { Relate segments }
    SearchSegments(Segments, @RelateSegments);
    { Remove deadend segments }
    RemoveDeadendSegments(Segments);
    { Make polygons }
    Contours := nil;
    MakePolygon(Segments, Contours);
    { Write contour list to array }
    Result := GetContours(Contours);
    { Sort closure contours by square }
    SortContoursBySquare(@Result);
    { Tidy up }
    ResetSegments(Segments);
    ResetContours(Contours);
  end
  else
    FillChar(Result, SizeOf(TPolygon), 0);
end;

{ ============================================================================= }

function FindTypedCycles(const Src: PTypedContourArray; const SrcCount: Integer;
  var Dst: PTypedContourArray; var DstCount: Integer): Boolean;
var
  Segments: PSegmentNode;
  Contours: PContourNode;
begin
  Dst := nil;
  DstCount := 0;
  Result := False;
  if (Src = nil) or (SrcCount = 0) then Exit;
  { Load segments from polygon arrays }
  Segments := nil;
  LoadTypedSegments(Segments, Src, SrcCount);
  { Find intersections }
  SearchSegments(Segments, @SegmentsIntersects);
  { Insert intersections }
  ForEachSegments(Segments, @InsertIntersects);
  { Labeled equal segments }
  SearchSegments(Segments, @LabelingEquSegments);
  { Remove all equal segments }
  ForEachSegments(Segments, @RemoveEquSegments);
  { Relate segments }
  SearchSegments(Segments, @RelateSegments);
  { Remove deadend segments }
  RemoveDeadendSegments(Segments);
  { Make polygons }
  Contours := nil;
  MakePolygon(Segments, Contours);
  { Write contour list to array }
  GetTypedContours(Contours, Dst, DstCount);
  { Sort closure contours by square }
  SortTypedContoursBySquare(Dst, DstCount);
  { Tidy up }
  ResetSegments(Segments);
  ResetContours(Contours);

  Result := True;
end;

{ ============================================================================= }

end.
