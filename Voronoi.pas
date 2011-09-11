unit Voronoi;

interface

uses
    Geometry;

type

  { Vertex in 3D }

  PVertex3d = ^TVertex3d;
  TVertex3d = record
    x, y, z: Float;
  end;

  PVertex3dArray = ^TVertex3dArray;
  TVertex3dArray = array [0..MaxInt div SizeOf(TVertex3d) - 1] of TVertex3d;

  { Voronoi cell }
  
  PCell = ^TCell;
  TCell = record
    Polygon: TPolygon;
    Vertex: TVertex;
    Value: Float;
  end;

  PCellArray = ^TCellArray;
  TCellArray = array [0..MaxInt div SizeOf(TCell) - 1] of TCell;

{ Create Voronoi diagrams }

function VoronoiDiagrams(const Points: PVertex3dArray; const PointCount: Integer;
  var Cells: PCellArray; var CellCount: Integer; const Rct: PRectFloat): Boolean;

{ Create merged and smoothed Voronoi diagrams }

function UnitedVoronoiDiagrams(const Points: PVertex3dArray; const PointCount: Integer;
  var Cells: PCellArray; var CellCount: Integer; const Rct: PRectFloat; SmoothMethod: Integer): Boolean;

implementation

uses
  Math;

{============================= Private constants ===============================}

const
  Head  = 0;
  Tail  = 1;

  Left  = 0;
  Right = 1;

{============================= Private types ===================================}

type
  { Vertex list node }
  PVertexNode = ^TVertexNode;
  TVertexNode = record
    Vertice: TVertex;
    z: Float;
    Index: Integer;
    Next: PVertexNode;
    Prev: PVertexNode;
  end;

  { Triangle list node }
  PTriangleNode = ^TTriangleNode;
  TTriangleNode = record
    Center: TVertex;
    Vertices: array [0..2] of PVertexNode;
    Neighbours: array [0..2] of PTriangleNode;
    Next: PTriangleNode;
    Prev: PTriangleNode;
  end;

  { Cell array node }
  PCellNode = ^TCellNode;
  TCellNode = record
    z: Float;
    Vertex: TVertex;
    Count: Integer;
    Ribs: PPointerArray;
    Less: PCellNode;
    Equal: PCellNode;
    More: PCellNode;
  end;

  PCellNodeArray = ^TCellNodeArray;
  TCellNodeArray = array [0..MaxInt div SizeOf(TCellNode) - 1] of TCellNode;

  { Ribs list node }
  PRibNode = ^TRibNode;
  TRibNode = record
    Vertices: array [Head..Tail] of PVertex;
    Cells: array [Left..Right] of PCellNode;
    Unions: array [Left..Right] of PCellNode;
    IsUnion: Boolean;
    Next: PRibNode;
    Prev: PRibNode;
  end;

{============================= Private functions ===============================}

{ Vertex list functions }

function GetVertexCount(Root: PVertexNode): Integer;
begin
  Result := 0;
  while Root <> nil do
  begin
    Root.Index := Result;
    Inc(Result);
    Root := Root.Next;
  end;
end;

function InsertVertex(var Root: PVertexNode):  PVertexNode;
begin
  Result := AllocMem(SizeOf(TVertexNode));
  Result.Next := Root;
  if Root <> nil then Root.Prev := Result;
  Root := Result;
end;

procedure RemoveVertex(var Root: PVertexNode; Node: PVertexNode);
begin
  if (Root <> nil) and (Node <> nil) then
    if Root = Node then
    begin
      Root := Node.Next;
      if Root <> nil then Root.Prev := nil;
      Dispose(Node);
    end
    else
    begin
      if Node.Next <> nil then
        Node.Next.Prev := Node.Prev;
      if Node.Prev <> nil then
        Node.Prev.Next := Node.Next;
      Dispose(Node);
    end;
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

{ Triangle list functions }

function InsertTriangle(var Root: PTriangleNode): PTriangleNode;
begin
  Result := AllocMem(SizeOf(TTriangleNode));
  Result.Next := Root;
  if Root <> nil then Root.Prev := Result;
  Root := Result;
end;

procedure ResetTriangles(var Root: PTriangleNode);
var
  Next: PTriangleNode;
begin
  while Root <> nil do
  begin
    Next := Root.Next;
    Dispose(Root);
    Root := Next;
  end;
end;

{ Ribs list node }

function InsertRib(var Root: PRibNode):  PRibNode;
begin
  Result := AllocMem(SizeOf(TRibNode));
  Result.Next := Root;
  if Root <> nil then Root.Prev := Result;
  Root := Result;
end;

procedure ResetRibs(var Root: PRibNode);
var
  Next: PRibNode;
begin
  while Root <> nil do
  begin
    Next := Root.Next;
    Dispose(Root);
    Root := Next;
  end;
end;

{ Cells list functions }

function GetNeCellCount(Root: PCellNode): Integer;

  procedure _Count(Node: PCellNode; var Index: Integer);
  begin
    if Node.Less <> nil then
      _Count(Node.Less, Index);
    Inc(Index);
    if Node.More <> nil then
      _Count(Node.More, Index);
  end;

begin
  Result := 0;
  if Root <> nil then
    _Count(Root, Result);
end;

function GetEqCellCount(Root: PCellNode): Integer;
begin
  Result := 0;
  while Root <> nil do
  begin
    Inc(Result);
    Root := Root.Equal;
  end;
end;

function InsertCell(var Root: PCellNode; z: Float): PCellNode;
begin
  if Root = nil then
  begin
    Result := AllocMem(SizeOf(TCellNode));
    Result.z := z;
    Root := Result;
  end
  else
    if IsLs(z, Root.z, EpsFloat) then
      Result := InsertCell(Root.Less, z)
    else
      if IsGt(z, Root.z, EpsFloat) then
        Result := InsertCell(Root.More, z)
      else
      begin
        Result := AllocMem(SizeOf(TCellNode));
        Result.z := z;
        Result.Equal := Root.Equal;
        Root.Equal := Result;
      end;
end;

procedure UnloadNeArray(Root: PCellNode; var P: PPointerArray; var Count: Integer);

  procedure _Unload(Node: PCellNode; var Index: Integer);
  begin
    if Node.Less <> nil then
      _Unload(Node.Less, Index);
    P[Index] := Node;
    Inc(Index);
    if Node.More <> nil then
      _Unload(Node.More, Index);
  end;

begin
  P := nil;
  Count := GetNeCellCount(Root);
  if Count > 0 then
  begin
    GetMem(P, SizeOf(Pointer) * Count);
    Count := 0;
    _Unload(Root, Count);
  end;
end;

procedure ResetCells(var Root: PCellNode);

  procedure _Remove(var Node: PCellNode);
  var
    Next: PCellNode;
  begin
    while Node <> nil do
    begin
      Next := Node.Equal;
      FreeMem(Node.Ribs);
      FreeMem(Node);
      Node := Next;
    end;
  end;

begin
  if Root <> nil then
  begin
    ResetCells(Root.Less);
    ResetCells(Root.More);
    _Remove(Root);
  end;
end;

{ Triangulation functions }

procedure AddVertices(var Vertices: PVertexNode; Points: PVertex3dArray; PointCount: Integer; Rct: PRectFloat);
var
  I: Integer;
  Clipped: Boolean;
  Node: PVertexNode;
  V: TVertex;
begin
  Clipped := (Rct <> nil) and (not IsRectEmpty(Rct));
  for I := 0 to PointCount - 1 do
  begin
    V := Vertex(Points[I].x, Points[I].y);
    if (not Clipped) or (Clipped and (PntInRect(Rct, @V) = pnInside)) then
    begin
      Node := InsertVertex(Vertices);
      Node.Vertice := V;
      Node.z := Points[I].z;
    end;
  end;
end;

function GetBounds(Root: PVertexNode; R: PRectFloat): Boolean;
begin
  Result := False;
  if Root <> nil then
  begin
    R.left := Root.Vertice.x;
    R.top := Root.Vertice.y;
    R.right := Root.Vertice.x;
    R.bottom := Root.Vertice.y;
    while Root <> nil do
    begin
      if IsLs(Root.Vertice.x, R.left, EpsFloat) then R.left := Root.Vertice.x;
      if IsGt(Root.Vertice.y, R.top, EpsFloat) then R.top := Root.Vertice.y;
      if IsGt(Root.Vertice.x, R.right, EpsFloat) then R.right := Root.Vertice.x;
      if IsLs(Root.Vertice.y, R.bottom, EpsFloat) then R.bottom := Root.Vertice.y;
      Root := Root.Next;
    end;
    Result := True;
  end;
end;

function GetIndex(T, N: PTriangleNode): Integer; overload;
begin
  Result := 0;
  while T.Neighbours[Result] <> N do Inc(Result);
end;

function GetIndex(T: PTriangleNode; V: PVertexNode): Integer; overload;
begin
  Result := 0;
  while T.Vertices[Result] <> V do Inc(Result);
end;

function GetAdjointTriangle(var T: PTriangleNode; V: PVertexNode): Boolean;
var
  I, K: Integer;
begin
  Result := True;
  K := -1;
  I := 0;
  while Result and (I < 3) do
    for I := 0 to 2 do
      if I <> K then
        if PntsEqu(@T.Vertices[(I + 1) mod 3].Vertice, @V.Vertice) or
           PntsEqu(@T.Vertices[(I + 2) mod 3].Vertice, @V.Vertice) then
        begin
          Result := False;
          Break;
        end
        else
          if IsGt(Cross(@V.Vertice, @T.Vertices[(I + 1) mod 3].Vertice,
                        @V.Vertice, @T.Vertices[(I + 2) mod 3].Vertice), 0.0, EpsFloat) then
          begin
            K := GetIndex(T.Neighbours[I], T);
            T := T.Neighbours[I];
            Break;
          end;
end;

function GetTriangleByPoint(var T: PTriangleNode; V: PVertexNode): Boolean;
var
  I, K: Integer;
begin
  Result := False;
  K := -1;
  I := 0;
  while (not Result) and (I < 3) do
    for I := 0 to 2 do
      if I <> K then
        if T.Vertices[(I + 1) mod 3] = V then
        begin
          Result := True;
          Break;
        end
        else
          if IsGt(Cross(@V.Vertice, @T.Vertices[(I + 1) mod 3].Vertice,
                        @V.Vertice, @T.Vertices[(I + 2) mod 3].Vertice), 0.0, EpsFloat) then
          begin
            K := GetIndex(T.Neighbours[I], T);
            T := T.Neighbours[I];
            Break;
          end;
end;

procedure MakeFlip(T: PTriangleNode; Index: Integer);
var
  nNeigbour: Integer;
  N: PTriangleNode;
begin
  N := T.Neighbours[Index];
  if N <> nil then
  begin
    nNeigbour := GetIndex(N, T);
    if not IsDelonay(
      @T.Vertices[Index].Vertice,
      @T.Vertices[(Index + 2) mod 3].Vertice,
      @N.Vertices[nNeigbour].Vertice,
      @T.Vertices[(Index + 1) mod 3].Vertice) then
    begin
      T.Vertices[(Index + 1) mod  3] := N.Vertices[nNeigbour];
      N.Vertices[(nNeigbour + 1) mod 3] := T.Vertices[Index];

      T.Neighbours[Index] := N.Neighbours[(nNeigbour + 2) mod 3];
      if T.Neighbours[Index] <> nil then
        T.Neighbours[Index].Neighbours[GetIndex(T.Neighbours[Index], N)] := T;

      N.Neighbours[nNeigbour] := T.Neighbours[(Index + 2) mod 3];
      if N.Neighbours[nNeigbour] <> nil then
        N.Neighbours[nNeigbour].Neighbours[GetIndex(N.Neighbours[nNeigbour], T)] := N;

      T.Neighbours[(Index + 2) mod 3] := N;
      N.Neighbours[(nNeigbour + 2) mod 3] := T;

      MakeFlip(T, Index);
      MakeFlip(N, (nNeigbour + 1) mod 3);
    end;
  end;
end;

procedure BreakTriangle(var Triangles: PTriangleNode; T: PTriangleNode; V: PVertexNode);
var
  T0, T1: PTriangleNode;
begin
  T0 := InsertTriangle(Triangles);
  T1 := InsertTriangle(Triangles);

  { Vertices }
  T0.Vertices[0] := V;
  T0.Vertices[1] := T.Vertices[2];
  T0.Vertices[2] := T.Vertices[0];
  { Adjoint triangles }
  T0.Neighbours[0] := T.Neighbours[1];
  if T0.Neighbours[0] <> nil then
    T0.Neighbours[0].Neighbours[GetIndex(T0.Neighbours[0], T)] := T0;
  T0.Neighbours[1] := T1;
  T0.Neighbours[2] := T;

  { Vertices }
  T1.Vertices[0] := V;
  T1.Vertices[1] := T.Vertices[0];
  T1.Vertices[2] := T.Vertices[1];
  { Adjoint triangles }
  T1.Neighbours[0] := T.Neighbours[2];
  if T1.Neighbours[0] <> nil then
    T1.Neighbours[0].Neighbours[GetIndex(T1.Neighbours[0], T)] := T1;
  T1.Neighbours[1] := T;
  T1.Neighbours[2] := T0;

  { Vertices }
  T.Vertices[0] := V;
  { Adjoint triangles }
  T.Neighbours[1] := T0;
  T.Neighbours[2] := T1;

  { Make flip }
  MakeFlip(T, 0);
  MakeFlip(T0, 0);
  MakeFlip(T1, 0);
end;

procedure Triangulate(var Triangles: PTriangleNode; var Vertices: PVertexNode;
  e0, e1, e2, e3: PVertexNode; Rct: PRectFloat);
var
  R: TRectFloat;
  T0, T1: PTriangleNode;
  V0, V1: PVertexNode;
begin
  if GetBounds(Vertices, @R) then
    if IsGe(R.right, R.left, EpsFloat) and IsGe(R.top, R.bottom, EpsFloat) then
    begin
      { Create superstructure }
      if IsEq(R.right, R.left, EpsFloat) then
      begin
        R.right := R.right + EpsFloat;
        R.left := R.left - EpsFloat;
      end;

      if IsEq(R.top, R.bottom, EpsFloat) then
      begin
        R.top := R.top + EpsFloat;
        R.bottom := R.bottom - EpsFloat;
      end;

      if (Rct <> nil) and
         IsLs(Rct.left, R.left, EpsFloat) and IsGt(Rct.right, R.right, EpsFloat) and
         IsLs(Rct.bottom, R.bottom, EpsFloat) and IsGt(Rct.top, R.top, EpsFloat) then
        R := Rct^;

      e0.Vertice := Vertex(2.0 * R.left - R.right, 2.0 * R.bottom - R.top);
      e0.z := MaxFloat;
      e0.Index := -1;

      e1.Vertice := Vertex(2.0 * R.left - R.right, 2.0 * R.top - R.bottom);
      e1.z := MaxFloat;
      e1.Index := -1;

      e2.Vertice := Vertex(2.0 * R.right - R.left, 2.0 * R.top - R.bottom);
      e2.z := MaxFloat;
      e2.Index := -1;

      e3.Vertice := Vertex(2.0 * R.right - R.left, 2.0 * R.bottom - R.top);
      e3.z := MaxFloat;
      e3.Index := -1;

      T0 := InsertTriangle(Triangles);
      T0.Vertices[0] := e0;
      T0.Vertices[1] := e1;
      T0.Vertices[2] := e2;

      T1 := InsertTriangle(Triangles);
      T1.Vertices[0] := e2;
      T1.Vertices[1] := e3;
      T1.Vertices[2] := e0;

      T0.Neighbours[1] := T1;
      T1.Neighbours[1] := T0;

      { Triangulation process }
      V0 := Vertices;
      T0 := Triangles;
      while V0 <> nil do
      begin
        V1 := V0.Next;
        if GetAdjointTriangle(T0, V0) then
          BreakTriangle(Triangles, T0, V0)
        else
          RemoveVertex(Vertices, V0);
        V0 := V1;
      end;
    end;
end;

{ Voronoi diagrams functions }

procedure TriangleCenters(Root: PTriangleNode);
begin
  while Root <> nil do
  begin
    Root.Center := Center(@Root.Vertices[0].Vertice, @Root.Vertices[1].Vertice, @Root.Vertices[2].Vertice);
    Root := Root.Next;
  end;
end;

procedure MakeCell(Voronois: PCellArray; Index: Integer; T: PTriangleNode; V: PVertexNode);
var
  I: Integer;
  Capacity: Integer;
  Curr: PTriangleNode;
  Contour: PContour;
begin
  Voronois[Index].Value := V.z;
  Voronois[Index].Vertex := V.Vertice;
  Voronois[Index].Polygon.Count := 1;
  Voronois[Index].Polygon.Contours := AllocMem(SizeOf(TContour) * Voronois[Index].Polygon.Count);
  Contour := @Voronois[Index].Polygon.Contours[0];
  Capacity := 0;
  Curr := T;
  repeat
    I := GetIndex(Curr, V);
    Geometry.Append(Pointer(Contour.Vertices), Capacity, Contour.Count, SizeOf(TVertex));
    Contour.Vertices[Contour.Count - 1] := Curr.Center;
    Curr := Curr.Neighbours[GetNext(I, 3)];
  until Curr = T;
  CloseContour(Contour);
end;

procedure MakeCellItem(Voronois: PCellNodeArray; Index: Integer; T: PTriangleNode;
  V: PVertexNode; var Ribs: PRibNode);
var
  I: Integer;
  Capacity: Integer;
  Curr, Next: PTriangleNode;
  R: PRibNode;
begin
  Voronois[Index].z := V.z;
  Voronois[Index].Vertex := V.Vertice;
  Capacity := 0;
  Curr := T;
  repeat
    I := GetIndex(Curr, V);
    Next := Curr.Neighbours[GetNext(I, 3)];
    if not PntsEqu(@Curr.Center, @Next.Center) then
    begin
      Geometry.Append(Pointer(Voronois[Index].Ribs), Capacity, Voronois[Index].Count, SizeOf(Pointer));
      R := InsertRib(Ribs);
      R.Vertices[Head] := @Curr.Center;
      R.Vertices[Tail] := @Next.Center;
      if Curr.Vertices[(I + 2) mod 3].Index <> -1 then
        R.Cells[Left] := @Voronois[Curr.Vertices[(I + 2) mod 3].Index]
      else
        R.Cells[Left] := nil;
      R.Cells[Right] := @Voronois[Index];
      R.Unions[Left] := nil;
      R.Unions[Right] := nil;

      R.IsUnion := Curr.Vertices[(I + 2) mod 3].z <> V.z;
      Voronois[Index].Ribs[Voronois[Index].Count - 1] := R;
    end;
    Curr := Next;
  until Curr = T;
  ReallocMem(Pointer(Voronois[Index].Ribs), SizeOf(Pointer) * Voronois[Index].Count);
end;

function GetRibIndex(Cell: PCellNode; Rib: PRibNode): Integer;
begin
  Result := 0;
  while (Result < Cell.Count) and (Cell.Ribs[Result] <> Rib) do
    Inc(Result);
end;

function GetCellIndex(C, N: PCellNode): Integer;
begin
  Result := 0;
  while (Result < C.Count) and (PRibNode(C.Ribs[Result]).Cells[Left] <> N) do
    Inc(Result);
end;

procedure UnionCellItems(Voronoi: PCellNode; Rib: PRibNode);
var
  I, J, K: Integer;
  Capacity: Integer;
  Cell: PCellNode;
begin
  Cell := Rib.Cells[Right];
  I := GetRibIndex(Cell, Rib);
  Capacity := 0;
  repeat
    if PRibNode(Cell.Ribs[I]).IsUnion then
    begin
      PRibNode(Cell.Ribs[I]).IsUnion := (Cell.Ribs[I] = Rib);
      Geometry.Append(Pointer(Voronoi.Ribs), Capacity, Voronoi.Count, SizeOf(Pointer));
      Voronoi.Ribs[Voronoi.Count - 1] := Cell.Ribs[I];
      if PRibNode(Cell.Ribs[I]).Cells[Left] <> nil then
      begin
        K := GetCellIndex(PRibNode(Cell.Ribs[I]).Cells[Left], Cell);
        PRibNode(PRibNode(Cell.Ribs[I]).Cells[Left].Ribs[K]).Unions[Left] := Voronoi;
      end
      else
        PRibNode(Cell.Ribs[I]).Unions[Left] := nil;
      PRibNode(Cell.Ribs[I]).Unions[Right] := Voronoi;
    end;
    I := GetNext(I, Cell.Count);
    if not PRibNode(Cell.Ribs[I]).IsUnion then
    begin
      J := GetCellIndex(PRibNode(Cell.Ribs[I]).Cells[Left], Cell);
      Cell := PRibNode(Cell.Ribs[I]).Cells[Left];
      I := J;
    end;
  until Cell.Ribs[I] = Rib;
  Rib.IsUnion := False;
  ReallocMem(Pointer(Voronoi.Ribs), SizeOf(TCellNode) * Voronoi.Count);
end;

function SplitCell(Cell: PCellNode): TPolygon;

  function _CanSplit(I0, I1: Integer): Boolean;
  begin
    Result := PRibNode(Cell.Ribs[I0]).Unions[Left] <> PRibNode(Cell.Ribs[I1]).Unions[Left]; 
  end;

  function _GetFirst: Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to Cell.Count - 1 do
      if _CanSplit(GetPrev(I, Cell.Count), I) then
      begin
        Result := I;
        Break;
      end;
  end;

var
  I, First: Integer;
  ContCapacity: Integer;
  VertCapacity: Integer;
  C: PContour;
begin
  FillChar(Result, SizeOf(TPolygon), 0);
  C := nil;
  ContCapacity := 0;
  VertCapacity := 0;
  First := _GetFirst;
  I := First;
  repeat
    if (C = nil) or _CanSplit(GetPrev(I, Cell.Count), I) then
    begin
      if C <> nil then
      begin
        Geometry.Append(Pointer(C.Vertices), VertCapacity, C.Count, SizeOf(TVertex));
        C.Vertices[C.Count - 1] := PRibNode(Cell.Ribs[I]).Vertices[Head]^;
        ReallocMem(Pointer(C.Vertices), SizeOf(TVertex) * C.Count);
      end;
      C := Geometry.Append(Pointer(Result.Contours), ContCapacity, Result.Count, SizeOf(TContour));
      VertCapacity := 0;
    end;
    Geometry.Append(Pointer(C.Vertices), VertCapacity, C.Count, SizeOf(TVertex));
    C.Vertices[C.Count - 1] := PRibNode(Cell.Ribs[I]).Vertices[Head]^;    
    I := GetNext(I, Cell.Count);
  until I = First;

  if (C <> nil) and (C.Count > 0) then
  begin
    Geometry.Append(Pointer(C.Vertices), VertCapacity, C.Count, SizeOf(TVertex));
    C.Vertices[C.Count - 1] := PRibNode(Cell.Ribs[I]).Vertices[Head]^;
    ReallocMem(Pointer(C.Vertices), SizeOf(TVertex) * C.Count);
  end;
  ReallocMem(Pointer(Result.Contours), SizeOf(TContour) * Result.Count);
end;

procedure SmoothSplitedPoly(P: PPolygon; SmoothMethod: Integer);
var
  I: Integer;
  Dst: TContour;
begin
  if P <> nil then
    for I := 0 to P.Count - 1 do
      if P.Contours[I].Count > 0 then
        if PntsEqu(@P.Contours[I].Vertices[0], @P.Contours[I].Vertices[P.Contours[I].Count - 1]) then
        begin
          Dec(P.Contours[I].Count);
          if SmoothContour(@P.Contours[I], @Dst, True, SmoothMethod) then
          begin
            Inc(Dst.Count);
            ReallocMem(Dst.Vertices, SizeOf(TVertex) * Dst.Count);
            Dst.Vertices[Dst.Count - 1] := Dst.Vertices[0];
            FreeMem(P.Contours[I].Vertices);
            P.Contours[I] := Dst;
          end;
        end
        else
        begin
          if SmoothContour(@P.Contours[I], @Dst, False, SmoothMethod) then
          begin
            Dst.Vertices[0] := P.Contours[I].Vertices[0];
            Dst.Vertices[Dst.Count - 1] := P.Contours[I].Vertices[P.Contours[I].Count - 1];
            FreeMem(P.Contours[I].Vertices);
            P.Contours[I] := Dst;
          end;
        end;
end;

function ConcatPoly(P: PPolygon): TContour;
var
  I, J, K: Integer;
begin
  FillChar(Result, SizeOf(TContour), 0);
  if P <> nil then
  begin
    { Get contour length }
    for I := 0 to P.Count - 1 do
      Inc(Result.Count, P.Contours[I].Count - 1);
    { Make new contour }
    if Result.Count > 0 then
    begin
      GetMem(Result.Vertices, SizeOf(TVertex) * Result.Count);
      K := 0;
      for I := 0 to P.Count - 1 do
        for J := 0 to P.Contours[I].Count - 2 do
        begin
          Result.Vertices[K] := P.Contours[I].Vertices[J];
          Inc(K);
        end;
      Result.Hole := P.Contours[0].Hole;
    end;
  end;
end;

{============================= Public functions ================================}

function VoronoiDiagrams(const Points: PVertex3dArray; const PointCount: Integer;
  var Cells: PCellArray; var CellCount: Integer; const Rct: PRectFloat): Boolean;
var
  Index: Integer;
  Edges: array [0..3] of TVertexNode;
  Vertices, V: PVertexNode;
  Triangles, T: PTriangleNode;
begin
  Result := False;
  Cells := nil;
  CellCount := 0;
  if (Points <> nil) and (PointCount > 0) then
  begin
    { Fill vertices }
    Vertices := nil;
    AddVertices(Vertices, Points, PointCount, Rct);
    { Triangulate }
    Triangles := nil;
    FillChar(Edges[0], SizeOf(Edges), 0);
    Triangulate(Triangles, Vertices, @Edges[0], @Edges[1], @Edges[2], @Edges[3], Rct);
    { Calc triangle centers coords }
    TriangleCenters(Triangles);
    { Voronoi diagrams }
    if (Vertices <> nil) and (Triangles <> nil) then
    begin
      CellCount := GetVertexCount(Vertices);
      if CellCount > 0 then
      begin
        Cells := AllocMem(SizeOf(TCell) * CellCount);
        Index := 0;
        V := Vertices;
        T := Triangles;
        while V <> nil do
        begin
          GetTriangleByPoint(T, V);
          MakeCell(Cells, Index, T, V);
          Inc(Index);
          V := V.Next;
        end;
        Result := True;
      end;
    end;
    { Tidy up }
    ResetTriangles(Triangles);
    ResetVertices(Vertices);
  end;
end;

function UnitedVoronoiDiagrams(const Points: PVertex3dArray; const PointCount: Integer;
  var Cells: PCellArray; var CellCount: Integer; const Rct: PRectFloat; SmoothMethod: Integer): Boolean;
var
  I, J, K: Integer;
  Edges: array [0..3] of TVertexNode;
  Vertices, V: PVertexNode;
  Triangles, T: PTriangleNode;
  Ribs, R: PRibNode;
  VoronoiCount: Integer;
  Voronois: PCellNodeArray;
  Unions, U: PCellNode;
  CellArrayCount: Integer;
  CellArray: PPointerArray;
  SplitPoly: TPolygon;
begin
  Result := False;
  Cells := nil;
  CellCount := 0;
  if (Points <> nil) and (PointCount > 0) then
  begin
    { Fill vertices }
    Vertices := nil;
    AddVertices(Vertices, Points, PointCount, Rct);
    { Triangulate }
    Triangles := nil;
    FillChar(Edges[0], SizeOf(Edges), 0);
    Triangulate(Triangles, Vertices, @Edges[0], @Edges[1], @Edges[2], @Edges[3], Rct);
    { Calc triangle centers coords }
    TriangleCenters(Triangles);
    { Voronoi diagrams }
    if (Vertices <> nil) and (Triangles <> nil) then
    begin
      VoronoiCount := GetVertexCount(Vertices);
      if VoronoiCount > 0 then
      begin
        { Create Voronoi cells }
        Voronois := AllocMem(SizeOf(TCellNode) * VoronoiCount);
        Ribs := nil;
        try
          I := 0;
          V := Vertices;
          T := Triangles;
          while V <> nil do
          begin
            GetTriangleByPoint(T, V);
            MakeCellItem(Voronois, I, T, V, Ribs);
            Inc(I);
            V := V.Next;
          end;
          { Union cells }
          Unions := nil;
          R := Ribs;
          while R <> nil do
          begin
            if R.IsUnion then
              UnionCellItems(InsertCell(Unions, R.Cells[Right].z), R);
            R := R.Next;
          end;

          { Unload unioned cells }
          try
            UnloadNeArray(Unions, CellArray, CellArrayCount);
            if (CellArray <> nil) and (CellArrayCount > 0) then
              try
                CellCount := CellArrayCount;
                Cells := AllocMem(SizeOf(TCell) * CellCount);
                for I := 0 to CellArrayCount - 1 do
                begin
                  Cells[I].Value := PCellNode(CellArray[I]).z;
                  Cells[I].Polygon.Count := GetEqCellCount(CellArray[I]);
                  Cells[I].Polygon.Contours := AllocMem(SizeOf(TContour) * Cells[I].Polygon.Count);
                  J := 0;
                  U := CellArray[I];
                  while U <> nil do
                  begin
                    if SmoothMethod = smNone then
                    begin
                      Cells[I].Polygon.Contours[J].Count := U.Count;
                      GetMem(Cells[I].Polygon.Contours[J].Vertices, SizeOf(TVertex) * Cells[I].Polygon.Contours[J].Count);
                      for K := 0 to U.Count - 1 do
                        Cells[I].Polygon.Contours[J].Vertices[K] := PRibNode(U.Ribs[K]).Vertices[Head]^;
                    end
                    else
                    begin
                      SplitPoly := SplitCell(U);
                      SmoothSplitedPoly(@SplitPoly, SmoothMethod);
                      Cells[I].Polygon.Contours[J] := ConcatPoly(@SplitPoly);
                      FreePolygon(@SplitPoly);
                    end;
                    CloseContour(@Cells[I].Polygon.Contours[J]);
                    Inc(J);
                    U := U.Equal;
                  end;
                  SortContoursBySquare(@Cells[I].Polygon);
                  Result := True;
                end;

              finally
                FreeMem(CellArray);
              end;

          finally
            ResetCells(Unions);
          end;

        finally
          for I := 0 to VoronoiCount - 1 do
            FreeMem(Voronois[I].Ribs);
          FreeMem(Voronois);
          ResetRibs(Ribs);
        end;
      end;
    end;
    { Tidy up }
    ResetTriangles(Triangles);
    ResetVertices(Vertices);
  end;
end;

{===============================================================================}

end.
