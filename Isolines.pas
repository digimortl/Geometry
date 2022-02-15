unit Isolines;

interface

uses
  Geometry;

procedure InitializeIsolines(const x, y: PFloatArray; const z: P2DFloatArray; nx, ny: Integer);
function CreateIsolines(z: Float; bClosure: Boolean): TPolygon;
procedure ReleaseIsolines;

implementation

uses
  Math;

type
  PRib = ^TRib;
  PSquare = ^TSquare;
  TSquare = record
    Ribs: array [0..3] of PRib;
  end;

  TRib = record
    Crossing: Float;
    Squares: array [0..1] of PSquare;
    Vertices: array [0..1] of record I, J: Integer; end;
    Indexes: array [0..1] of Integer;
    Parent: Pointer;
    Next, Prev: PRib;
  end;

  TRibs = class(TObject)
  private
    FCount: Integer;
    FHead, FTail: PRib;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Rib: PRib);
    procedure RemoveAll;
    procedure RemoveAt(Rib: PRib);
    property Count: Integer read FCount;
  end;

  TEdge = class(TRibs)
  private
    FIndex: Integer;
    I0, J0: Integer;
    I1, J1: Integer;
  public
    constructor Create(Index: Integer);
  end;

  TIsolines = class(TObject)
  private
    FNX, FNY: Integer;
    FX, FY: PFloatArray;
    FZ: P2DFloatArray;
    FDirect: Integer;
    FSquares: array of array of TSquare;
    FEdges: array [0..3] of TEdge;
    FRibs: TRibs;
    FCurrentCell: PSquare;
    procedure InitSquares;
    function GetX(Index: Integer): Float;
    function GetY(Index: Integer): Float;
    function GetZ(I, J: Integer): Float;
    function GetCrossing(Rib: PRib): TVertex;
    function GetCrossRib(I0, J0, I1, J1: Integer; z: PFloat): PRib;
    function SetSquareBottom(I, J: Integer; Rib: PRib): Boolean;
    function SetSquareLeft(I, J: Integer; Rib: PRib): Boolean;
    function SetSquareTop(I, J: Integer; Rib: PRib): Boolean;
    function SetSquareRight(I, J: Integer; Rib: PRib): Boolean;
    function FillCrossings(z: PFloat): Integer;
    procedure DeleteRib(Rib: PRib);
    function GetRibsCount: Integer;
    function GetNextInnerRib(Rib: PRib; z: PFloat): PRib;
    function GetFirstEdgeRib: PRib;
    function GetNextEdgeRib(Rib: PRib): PRib; overload;
    function GetNextEdgeRib(Index: Integer): PRib; overload;
    procedure MakeIsolines(z: PFloat; var  C: PContourArray; var Count: Integer; bClosure: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure InitIsolines(const x, y: PFloatArray; const z: P2DFloatArray; nx, ny: Integer);
    function CreateIsolines(z: Float): TPolygon;
    function CreateClosureIsolines(z: Float): TPolygon;
    function CreateIsocontours(z0, z1: Float): TPolygon;
    procedure ReleaseIsolines;
  end;

const
  Bottom   = 0;
  Left     = 1;
  Top      = 2;
  Right    = 3;

  Upper    = 0;
  Lower    = 1;

var
  FIsolines: TIsolines;

procedure InitializeIsolines(const x, y: PFloatArray; const z: P2DFloatArray; nx, ny: Integer);
begin
  FIsolines := TIsolines.Create;
  FIsolines.InitIsolines(x, y, z, nx, ny);
end;

function CreateIsolines(z: Float; bClosure: Boolean): TPolygon;
begin
  FillChar(Result, SizeOf(TPolygon), 0);
  if not Assigned(FIsolines) then Exit;

  if bClosure then
    Result := FIsolines.CreateClosureIsolines(z)
  else
    Result := FIsolines.CreateIsolines(z);
end;

procedure ReleaseIsolines;
begin
  if Assigned(FIsolines) then FreeAndNil(FIsolines);
end;

{ TIsolines }

constructor TIsolines.Create;

  procedure InitEdges;
  var
    I: Integer;
  begin
    for I := 0 to Length(FEdges) - 1 do
      FEdges[I] := TEdge.Create(I);
  end;

begin
  FRibs := TRibs.Create;
  InitEdges;
  ReleaseIsolines;
end;

procedure TIsolines.DeleteRib(Rib: PRib);
begin
  if Rib^.Squares[Upper] <> nil then
    Rib^.Squares[Upper]^.Ribs[Rib^.Indexes[Upper]] := nil;
  if Rib^.Squares[Lower] <> nil then
    Rib^.Squares[Lower]^.Ribs[Rib^.Indexes[Lower]] := nil;
  TRibs(Rib^.Parent).RemoveAt(Rib);
end;

destructor TIsolines.Destroy;

  procedure ClearEdges;
  var
    I: Integer;
  begin
    for I := 0 to Length(FEdges) - 1 do
      FEdges[I].Free;
  end;

begin
  ReleaseIsolines;
  ClearEdges;
  FRibs.Free;
  inherited Destroy;
end;

function TIsolines.FillCrossings(z: PFloat): Integer;
var
  I, J: Integer;
  Rib: PRib;
begin
  { Bottom }
  FEdges[Bottom].RemoveAll;
  I := FNX - 2;
  while I >= 0 do
  begin
    Rib := GetCrossRib(I, 0, I + 1, 0, z);
    if SetSquareBottom(I, 0, Rib) then
      FEdges[Bottom].Add(Rib);
    Dec(I);
  end;
  { Left }
  FEdges[Left].RemoveAll;
  J := 0;
  while J < FNY - 1 do
  begin
    Rib := GetCrossRib(0, J, 0, J + 1, z);
    if SetSquareLeft(0, J, Rib) then
      FEdges[Left].Add(Rib);
    Inc(J);
  end;
  { Top }
  FEdges[Top].RemoveAll;
  I := 0;
  while I < FNX - 1 do
  begin
    Rib := GetCrossRib(I, FNY - 1, I + 1, FNY - 1, z);
    if SetSquareTop(I, FNY - 2, Rib) then
      FEdges[Top].Add(Rib);
    Inc(I);
  end;
  { Right }
  FEdges[Right].RemoveAll;
  J := FNY - 2;
  while J >= 0 do
  begin
    Rib := GetCrossRib(FNX - 1, J, FNX - 1, J + 1, z);
    if SetSquareRight(FNX - 2, J, Rib) then
      FEdges[Right].Add(Rib);
    Dec(J);
  end;
  { Inner ribs }
  FRibs.RemoveAll;
  I := 0;
  while I < FNX - 1 do
  begin
    J := 0;
    while J < FNY - 1 do
    begin
      { Left }
      if I > 0 then
      begin
        Rib := GetCrossRib(I, J, I, J + 1, z);
        if SetSquareLeft(I, J, Rib) then
          FRibs.Add(Rib);
      end;
      { Bottom }
      if J > 0 then
      begin
        Rib := GetCrossRib(I, J, I + 1, J, z);
        if SetSquareBottom(I, J, Rib) then
          FRibs.Add(Rib);
      end;
      Inc(J);
    end;
    Inc(I);
  end;
  Result := GetRibsCount;
end;

function TIsolines.CreateClosureIsolines(z: Float): TPolygon;
begin
  FillChar(Result, SizeOf(TPolygon), 0);
  MakeIsolines(@z, Result.Contours, Result.Count, True);
end;

function TIsolines.CreateIsocontours(z0, z1: Float): TPolygon;
var
  I: Integer;
begin
  FillChar(Result, SizeOf(TPolygon), 0);
  MakeIsolines(@z1, Result.Contours, Result.Count, True);
  for I := 0 to Result.Count - 1 do
    Result.Contours[I].Hole := not Result.Contours[I].Hole;
  MakeIsolines(@z0, Result.Contours, Result.Count, True);
  SortContoursBySquare(@Result);
end;

function TIsolines.CreateIsolines(z: Float): TPolygon;
begin
  FillChar(Result, SizeOf(TPolygon), 0);
  MakeIsolines(@z, Result.Contours, Result.Count, False);
end;

function TIsolines.GetCrossing(Rib: PRib): TVertex;
begin
  Result.x := GetX(Rib^.Vertices[0].I) + Rib^.Crossing * (GetX(Rib^.Vertices[1].I) - GetX(Rib^.Vertices[0].I));
  Result.y := GetY(Rib^.Vertices[0].J) + Rib^.Crossing * (GetY(Rib^.Vertices[1].J) - GetY(Rib^.Vertices[0].J));
end;

function TIsolines.GetCrossRib(I0, J0, I1, J1: Integer; z: PFloat): PRib;
var
  z0, z1: Float;
begin
  Result := nil;
  z0 := GetZ(I0, J0);
  z1 := GetZ(I1, J1);

  if z0 = z^ then z0 := z0 + EpsFloat;
  if z1 = z^ then z1 := z1 + EpsFloat;

  if (z0 - z^) * (z1 - z^) < 0.0 then
  begin
    New(Result);
    Result^.Crossing := (z^ - GetZ(I0, J0)) / (GetZ(I1, J1) - GetZ(I0, J0));
  end;
end;

function TIsolines.GetFirstEdgeRib: PRib;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Length(FEdges) - 1 do
    if FEdges[I].FHead <> nil then
    begin
      Result := FEdges[I].FHead;
      Break;
    end;
end;

function TIsolines.GetNextEdgeRib(Rib: PRib): PRib;
var
  I, Index: Integer;
begin
  Result := nil;
  if Rib^.Next <> nil then
    Result := Rib^.Next
  else
  begin
    Index := TEdge(Rib^.Parent).FIndex;
    for I := 1 to 3 do
      if FEdges[(Index + I) mod 4].FHead <> nil then
      begin
        Result := FEdges[(Index + I) mod 4].FHead;
        Break;
      end;
  end;
end;

function TIsolines.GetNextEdgeRib(Index: Integer): PRib;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to 3 do
    if FEdges[(Index + I) mod 4].FHead <> nil then
    begin
      Result := FEdges[(Index + I) mod 4].FHead;
      Break;
    end;
end;

function TIsolines.GetNextInnerRib(Rib: PRib; z: PFloat): PRib;

  function _Triangulate(z0, z1, z2, z3, z: PFloat): Boolean;
  begin
    Result := (z0^ > z^) and (z2^ > z^);
  end;

  function _GetRib(Index: Integer): PRib;
  var
    LeftRib,
    RightRib,
    TopRib: PRib;
  begin
    Result := nil;
    FCurrentCell := Rib^.Squares[Index];
    if FCurrentCell <> nil then
    begin
      LeftRib := FCurrentCell^.Ribs[(Rib^.Indexes[Index] + Left) mod 4];
      RightRib := FCurrentCell^.Ribs[(Rib^.Indexes[Index] + Right) mod 4];
      TopRib := FCurrentCell^.Ribs[(Rib^.Indexes[Index] + Top) mod 4];
      if LeftRib <> nil then
      begin
        Result := LeftRib;
        if RightRib <> nil then
          if _Triangulate(
            @FZ[Rib^.Vertices[Index].I, Rib^.Vertices[Index].J],
            @FZ[TopRib^.Vertices[Index].I, TopRib^.Vertices[Index].J],
            @FZ[TopRib^.Vertices[Index xor 1].I, TopRib^.Vertices[Index xor 1].J],
            @FZ[Rib^.Vertices[Index xor 1].I, Rib^.Vertices[Index xor 1].J], z)
          then
            Result := RightRib;
      end
      else
        if RightRib <> nil then
          Result := RightRib
        else
          if TopRib <> nil then
            Result := TopRib;
    end;
  end;

begin
  Result := nil;
  if Rib^.Squares[FDirect] <> nil then
    if Rib^.Squares[FDirect] <> FCurrentCell then
      Result := _GetRib(FDirect)
    else
      Result := _GetRib(FDirect xor 1);
end;

function TIsolines.GetRibsCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Length(FEdges) - 1 do
    Result := Result + FEdges[I].Count;
  Result := Result + FRibs.Count;
end;

function TIsolines.GetX(Index: Integer): Float;
begin
  Result := FX[Index];
end;

function TIsolines.GetY(Index: Integer): Float;
begin
  Result := FY[Index];
end;

function TIsolines.GetZ(I, J: Integer): Float;
begin
  Result := FZ[I, J];
end;

procedure TIsolines.InitIsolines(const x, y: PFloatArray; const z: P2DFloatArray; nx, ny: Integer);
begin
  FNX := nx;
  FNY := ny;
  FX := x;
  FY := y;
  FZ := z;
  FDirect := Upper;
  InitSquares;
end;

procedure TIsolines.InitSquares;
begin
  SetLength(FSquares, FNX - 1, FNY - 1);
  { Bottom }
  FEdges[Bottom].I0 := FNX - 1;
  FEdges[Bottom].J0 := 0;
  FEdges[Bottom].I1 := 0;
  FEdges[Bottom].J1 := 0;
  { Left }
  FEdges[Left].I0 := 0;
  FEdges[Left].J0 := 0;
  FEdges[Left].I1 := 0;
  FEdges[Left].J1 := FNY - 1;
  { Top }
  FEdges[Top].I0 := 0;
  FEdges[Top].J0 := FNY - 1;
  FEdges[Top].I1 := FNX - 1;
  FEdges[Top].J1 := FNY - 1;
  { Right }
  FEdges[Right].I0 := FNX - 1;
  FEdges[Right].J0 := FNY - 1;
  FEdges[Right].I1 := FNX - 1;
  FEdges[Right].J1 := 0;
end;

procedure TIsolines.MakeIsolines(z: PFloat; var C: PContourArray;
  var Count: Integer; bClosure: Boolean);
var
  ContourCapacity: Integer;
  VertexCapacity: Integer;

  function _GetDirect(Rib: PRib): Boolean;
  begin
    Result := GetZ(Rib^.Vertices[1].I, Rib^.Vertices[1].J) >= z^;
  end;

  function _IsNeededEdges: Boolean;
  begin
    Result := (GetZ(0, 0) >= z^) and (GetZ(0, FNY - 1) >= z^) and
      (GetZ(FNX - 1, FNY - 1) >= z^) and (GetZ(FNX - 1, 0) >= z^);
  end;

  procedure _AddVertex(var V: PVertexArray; var Capacity, Count: Integer; P: TVertex);
  begin
    if (Count > 0) and  PntsEqu(@P, @V[Count - 1]) then
      Exit;

    PVertex(Append(Pointer(V), Capacity, Count, SizeOf(TVertex)))^ := P;
  end;

  procedure _AddEdges;
  var
    I: Integer;
    P: TVertex;
    Contour: PContour;
  begin
    Contour := Append(Pointer(C), ContourCapacity, Count, SizeOf(TContour));
    Contour.Count := Length(FEdges) + 1;
    GetMem(Contour.Vertices, SizeOf(TVertex) * Contour.Count);
    for I := 0 to Contour.Count - 1 do
    begin
      Contour.Vertices[I].x := GetX(FEdges[GetNext(I, Contour.Count - 1)].I0);
      Contour.Vertices[I].y := GetY(FEdges[GetNext(I, Contour.Count - 1)].J0);
    end;
  end;

  procedure _Edge;
  var
    I: Integer;
    Rib, Next: PRib;
    Contour: PContour;
  begin
    Rib := GetFirstEdgeRib;
    while Rib <> nil do
    begin
      Contour := Append(Pointer(C), ContourCapacity, Count, SizeOf(TContour));
      FillChar(Contour^, SizeOf(TContour), 0);
      VertexCapacity := Contour^.Count;
      I := TEdge(Rib^.Parent).FIndex;
      FDirect := IfThen((Rib^.Vertices[1].I = 0) or (Rib^.Vertices[1].J = FNY - 1), Lower, Upper);
      repeat
        _AddVertex(Contour^.Vertices, VertexCapacity, Contour^.Count, GetCrossing(Rib));
        Next := GetNextInnerRib(Rib, z);
        DeleteRib(Rib);
        Rib := Next;
      until Rib = nil;
      Rib := GetNextEdgeRib(I);
      ReallocMem(Contour^.Vertices, Contour^.Count * SizeOf(TVertex));
    end;
  end;

  procedure _EdgeClosure;
  var
    I, Index: Integer;
    P: TVertex;
    First, Rib, Next: PRib;
    Contour: PContour;
  begin
    Rib := GetFirstEdgeRib;
    if (Rib = nil) and _IsNeededEdges then
      _AddEdges
    else
    begin
      while Rib <> nil do
      begin
        Contour := Append(Pointer(C), ContourCapacity, Count, SizeOf(TContour));
        FillChar(Contour^, SizeOf(TContour), 0);
        VertexCapacity := Contour^.Count;
        First := Rib;
        repeat
          _AddVertex(Contour^.Vertices, VertexCapacity, Contour^.Count, GetCrossing(Rib));
          FDirect := IfThen(_GetDirect(Rib), Upper, Lower);

          Next := GetNextInnerRib(Rib, z);
          if Next = nil then
            if Rib^.Next <> nil then
              Next := Rib^.Next
            else
            begin
              Index := TEdge(Rib^.Parent).FIndex;
              for I := 1 to 4 do
              begin
                P.x := GetX(FEdges[(Index + I) mod 4].I0);
                P.y := GetY(FEdges[(Index + I) mod 4].J0);
                _AddVertex(Contour^.Vertices, VertexCapacity, Contour^.Count, P);
                Next := FEdges[(Index + I) mod 4].FHead;
                if Next <> nil then
                  Break;
              end;
            end;

          if Rib <> First then
            DeleteRib(Rib);
          Rib := Next;
        until Rib = First;
        CloseContour(Contour);
        Next := GetNextEdgeRib(Rib);
        DeleteRib(Rib);
        Rib := Next;
      end;
    end;
  end;

  procedure _Inner;
  var
    Rib, Next: PRib;
    Contour: PContour;
  begin
    while FRibs.FHead <> nil do
    begin
      Rib := FRibs.FHead;
      Contour := Append(Pointer(C), ContourCapacity, Count, SizeOf(TContour));
      FillChar(Contour^, SizeOf(TContour), 0);
      Contour^.Hole := not _GetDirect(Rib);
      FDirect := IfThen(_GetDirect(Rib), Upper, Lower);
      VertexCapacity := Contour^.Count;
      repeat
        _AddVertex(Contour^.Vertices, VertexCapacity, Contour^.Count, GetCrossing(Rib));
        Next := GetNextInnerRib(Rib, z);
        DeleteRib(Rib);
        Rib := Next;
      until Rib = nil;
      CloseContour(Contour);
    end;
  end;

begin
  ContourCapacity := Count;
  FCurrentCell := nil;
  FillCrossings(z);
  if bClosure then
    _EdgeClosure
  else
    _Edge;
  _Inner;
  ReallocMem(C, Count * SizeOf(TContour));
end;

procedure TIsolines.ReleaseIsolines;
begin
  FDirect := Upper;
  FNX := 0;
  FNY := 0;
  FX := nil;
  FY := nil;  
  FZ := nil;
  FSquares := nil;
  FCurrentCell := nil;
end;

function TIsolines.SetSquareBottom(I, J: Integer; Rib: PRib): Boolean;
begin
  Result := False;
  FSquares[I, J].Ribs[Bottom] := Rib;
  if Rib <> nil then
  begin
    Rib^.Indexes[Upper] := Bottom;
    Rib^.Indexes[Lower] := Top;

    Rib^.Vertices[0].I := I;
    Rib^.Vertices[0].J := J;
    Rib^.Vertices[1].I := I + 1;
    Rib^.Vertices[1].J := J;

    Rib^.Squares[Upper] := @FSquares[I, J];
    
    if J > 0 then
    begin
      Rib^.Squares[Lower] := @FSquares[I, J - 1];
      FSquares[I, J - 1].Ribs[Top] := Rib;
    end
    else
      Rib^.Squares[Lower] := nil;
    Result := True;
  end;
end;

function TIsolines.SetSquareLeft(I, J: Integer; Rib: PRib): Boolean;
begin
  Result := False;
  FSquares[I, J].Ribs[Left] := Rib;
  if Rib <> nil then
  begin
    Rib^.Indexes[Lower] := Left;
    Rib^.Indexes[Upper] := Right;

    Rib^.Vertices[0].I := I;
    Rib^.Vertices[0].J := J;
    Rib^.Vertices[1].I := I;
    Rib^.Vertices[1].J := J + 1;

    Rib^.Squares[Lower] := @FSquares[I, J];

    if I > 0 then
    begin
      Rib^.Squares[Upper] := @FSquares[I - 1, J];
      FSquares[I - 1, J].Ribs[Right] := Rib;
    end
    else
      Rib^.Squares[Upper] := nil;
    Result := True;
  end;
end;

function TIsolines.SetSquareRight(I, J: Integer; Rib: PRib): Boolean;
begin
  Result := False;
  FSquares[I, J].Ribs[Right] := Rib;
  if Rib <> nil then
  begin
    Rib^.Indexes[Upper] := Right;
    Rib^.Indexes[Lower] := Left;

    Rib^.Vertices[0].I := I + 1;
    Rib^.Vertices[0].J := J;
    Rib^.Vertices[1].I := I + 1;
    Rib^.Vertices[1].J := J + 1;

    Rib^.Squares[Upper] := @FSquares[I, J];

    if I < FNX - 2 then
    begin
      Rib^.Squares[Lower] := @FSquares[I + 1, J];
      FSquares[I + 1, J].Ribs[Left] := Rib;
    end
    else
      Rib^.Squares[Lower] := nil;
    Result := True;
  end;
end;

function TIsolines.SetSquareTop(I, J: Integer; Rib: PRib): Boolean;
begin
  Result := False;
  FSquares[I, J].Ribs[Top] := Rib;
  if Rib <> nil then
  begin
    Rib^.Indexes[Lower] := Top;
    Rib^.Indexes[Upper] := Bottom;

    Rib^.Vertices[0].I := I;
    Rib^.Vertices[0].J := J + 1;
    Rib^.Vertices[1].I := I + 1;
    Rib^.Vertices[1].J := J + 1;

    Rib^.Squares[Lower] := @FSquares[I, J];

    if J < FNY - 2 then
    begin
      Rib^.Squares[Upper] := @FSquares[I, J + 1];
      FSquares[I, J + 1].Ribs[Bottom] := Rib;
    end
    else
      Rib^.Squares[Upper] := nil;
    Result := True;
  end;
end;

{ TEdge }

constructor TEdge.Create(Index: Integer);
begin
  inherited Create;
  FIndex := Index;
  I0 := -1;
  J0 := -1;
  I1 := -1;
  J1 := -1;
end;

{ TRibs }

procedure TRibs.Add(Rib: PRib);
begin
  Rib^.Parent := Self;
  if FTail <> nil then
    FTail^.Next := Rib
  else
    FHead := Rib;
  Rib^.Prev := FTail;
  Rib^.Next := nil;
  FTail := Rib;
  Inc(FCount);
end;

constructor TRibs.Create;
begin
  FCount := 0;
  FHead := nil;
  FTail := nil;
end;

destructor TRibs.Destroy;
begin
  RemoveAll;
  inherited Destroy;
end;

procedure TRibs.RemoveAll;
var
  Rib, Next: PRib;
begin
  Rib := FHead;
  while Rib <> nil do
  begin
    Next := Rib^.Next;
    Dispose(Rib);
    Rib := Next;
  end;
  FCount := 0;
  FHead := nil;
  FTail := nil;
end;

procedure TRibs.RemoveAt(Rib: PRib);
begin
  if Rib = FHead then
    FHead := Rib^.Next
  else
    Rib^.Prev^.Next := Rib^.Next;

  if Rib = FTail then
    FTail := Rib^.Prev
  else
    Rib^.Next^.Prev := Rib^.Prev;

  Dispose(Rib);
  Dec(FCount);
end;

initialization

  FIsolines := nil;

finalization

  if Assigned(FIsolines) then FreeAndNil(FIsolines);

end.
