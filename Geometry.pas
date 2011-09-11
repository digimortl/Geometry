{*******************************************************************************}
{                                                                               }
{                   The Base computation geometry functions                     }
{                                                                               }
{*******************************************************************************}

unit Geometry;

interface

uses
  Math;

{ ============================= Public constants ============================== }

const
  EpsFloat  = 1.0e-05;
  MinFloat  = MinDouble;
  MaxFloat  = MaxDouble;

{ ============================= Public data types ============================= }

type
  PFloat = ^Float;
  Float = type Double;

  PFloatArray = ^FloatArray;
  FloatArray = array [0..MaxInt div SizeOf(Float) - 1] of Float;

  P2DFloatArray = ^T2DFloatArray;
  T2DFloatArray = array [0..MaxInt div SizeOf(PFloatArray) - 1] of PFloatArray;

  PByteArray = ^ByteArray;
  ByteArray = array [0..MaxInt - 1] of Byte;

  PVertex = ^TVertex;
  TVertex = record
    x, y: Float;
  end;

  PVertexArray = ^TVertexArray;
  TVertexArray = array [0..MaxInt div SizeOf(TVertex) - 1] of TVertex;

  PContour = ^TContour;
  TContour = record
    Count: Integer;
    Hole: Boolean;
    Vertices: PVertexArray;
  end;

  PContourArray = ^TContourArray;
  TContourArray = array [0..MaxInt div SizeOf(TContour) - 1] of TContour;

  PPolygon = ^TPolygon;
  TPolygon = record
    Count: Integer;
    Contours: PContourArray;
  end;

  PPolygonArray = ^TPolygonArray;
  TPolygonArray = array [0..MaxInt div SizeOf(TPolygon) - 1] of TPolygon;

  PRectFloat = ^TRectFloat;
  TRectFloat = record
    case Boolean of
      True: (left, top, right, bottom: Float);
      False:(TopLeft, BottomRight: TVertex);
  end;

  PRectFloatArray = ^TRectFloatArray;
  TRectFloatArray = array [0..MaxInt div SizeOf(TRectFloat) - 1] of TRectFloat;

{ ======================== Public function declarations ======================= }

{ Is a equal b? }
function IsEq(a, b, Eps: Float): Boolean;
{ Is a not equal b?}
function IsNe(a, b, Eps: Float): Boolean;
{ Is a great b? }
function IsGt(a, b, Eps: Float): Boolean;
{ Is a less b? }
function IsLs(a, b, Eps: Float): Boolean;
{ Is a great or equal b? }
function IsGe(a, b, Eps: Float): Boolean;
{ Is a less or equal b? }
function IsLe(a, b, Eps: Float): Boolean;
{ Next index }
function GetNext(Index, Count: Integer): Integer;
{ Previous index }
function GetPrev(Index, Count: Integer): Integer;
{ Translate 2D index to 1D index }
function TranslateIndex(I, J, Width: Integer): Integer;
{ Add element to array }
function Append(var P: Pointer; var Capacity, Count: Integer; Size: Integer): Pointer;
{ Cear element(s) }
procedure Free(var P: Pointer);
{ Copy contour function }
function CopyContour(const Src: PContour): TContour;
{ Copy polygons }
function CopyPolygon(const Src: PPolygon): TPolygon;
{ Add vertex to contour }
function AddVertex(C: PContour; V: TVertex): PVertex;
{ Add contour to polygon }
function AddContour(P: PPolygon; C: TContour): PContour;
{ Clear contour }
procedure FreeContour(C: PContour);
{ Clear polygon }
procedure FreePolygon(P: PPolygon);
{ Close contour }
procedure CloseContour(Contour: PContour);

{ Unclose contour }
procedure UncloseContour(Contour: PContour);
{ Change contour orientation }
procedure ChangeOrientation(Contour: PContour);
{ Get signed area of a polygon }
function GetSquare(const Contour: PContour): Float;
{ Get perimeter }
function GetPerimeter(const Contour: PContour): Float;
{ Get bounded box of contour }
function GetContourBounds(const Contour: PContour; var R: TRectFloat): Boolean;
{ Get bounded box of polygon }
function GetPolygonBounds(const Polygon: PPolygon; var R: TRectFloat): Boolean; overload;
{ Sort contours by area }
procedure SortContoursBySquare(Polygon: PPolygon);
{ Set rectangle contour }
function RectContour(Rect: TRectFloat): TContour;
{ Is points equal }

function PntsEqu(const v0, v1: PVertex): Boolean;
{ Set x and y coordinates to vertex }
function Vertex(x, y: Float): TVertex;
{ Cross product }
function Cross(const v0, v1, v2, v3: PVertex): Float;
{ Scalar product }
function Scalar(const v0, v1, v2, v3: PVertex): Float;
{ Module }
function Module(const v: PVertex): Float;
{ Distance of two vertices }
function Distance(const v0, v1: PVertex): Float;
{ Normalization }
function Norm(const v: PVertex): TVertex;
{ Divide line segment by param t = [0..1] }
function Divide(const v0, v1: PVertex; t: Float): TVertex;
{ Middle of line segment }
function Middle(const v0, v1: PVertex): TVertex;
{ Line segment by fixed length }
function FixLine(const v0, v1: PVertex; Len: Float): TVertex;
{ Sine v1v0v2 (rad) }
function Sine(const v0, v1, v2: PVertex): Float;
{ Cosine v1v0v2 (rad) }
function Cosine(const v0, v1, v2: PVertex): Float;
{ Rotate a v1 vertex by r rad }
function Rotate(const v0, v1: PVertex; r: Float): TVertex;
{ Flip v1 vertex relative v0 at 90 degrees. Where o = 1.0 - clockwise, -1.0 - counter-clockwise }
function Flip(const v0, v1: PVertex; o: Float = 1.0): TVertex;
{ Circle through three points coordinate }
function Center(const v0, v1, v2: PVertex): TVertex;
{ Quadrant number }
function Quadrant(const v0, v1, v2: PVertex): Integer;
{ Angle between v0v1 and v1v2 line segments (rad) }
function Angle(const v0, v1, v2: PVertex): Float;
{ Is two adjoint triangles Delonay? }
function IsDelonay(const v0, v1, v2, v3: PVertex): Boolean;

const
  ptLeft          = 0;
  ptRight         = 1;
  ptBetween       = 2;
  ptHead          = 3;
  ptTail          = 4;
  ptBehind        = 5;
  ptBeyond        = 6;
{ Point location relative line segment }
function PntClassify(const v0, v1, v: PVertex): Integer;

const
  lnParallel      = 0;
  lnCollinear     = 1;
  lnSkew          = 2;
  lnSkewNotCross  = 3;
  lnSkewCross     = 4;

{ Two line segments location }
function LinesIntersect(const v0, v1, v2, v3: PVertex; var t: Float): Integer;
{ Params of two line segments intersects }
function LinesCross(const v0, v1, v2, v3: PVertex; var t0, t1: Float): Integer;

const
  pnOutside       = 0;
  pnInside        = 1;
  pnBoundary      = 2;

{ Is a point lies within a rectangle? }
function PntInRect(const r: PRectFloat; const v: PVertex): Integer;
{ Is a point lies within a ellipse? }
function PntInEllipse(const c: PVertex; const r0, r1: PFloat; const v: PVertex): Integer;
{ Is a point lies within a polygon? }
function PntInTriangle(const v0, v1, v2, v: PVertex): Integer;
{ Is a point lies within a complex polygon? }
function PntInPolygon(const Polygon: PPolygon; const v: PVertex): Integer;
{ Is point on contour? }
function PntOnContour(const Contour: PContour; const v: PVertex; d: Float): Boolean;
{ Is rectangle empty? }
function IsRectEmpty(const r: PRectFloat): Boolean;
{ Is two rectangle intersects? }
function IsRectsIntersect(const R1, R2: PRectFloat): Boolean;
{ Generate intersected rectangle }
function IntersectRectFloat(const R1, R2: PRectFloat): TRectFloat;
{ Union rectangles }
function UnionRectFloat(const R1, R2: PRectFloat): TRectFloat;
{ Set float rectangle }
function RectFloat(left, top, right, bottom: Float): TRectFloat;
{ Get empty rect }
function EmptyRect(): TRectFloat;
{ Find value in array at NLogN time }
function BinarySerach(const P: PFloatArray; Count: Integer; Value: Float; var L, R: Integer): Integer; overload;
function BinarySerach(const P: PFloatArray; Count: Integer; Value: Float): Integer; overload;
{ Averange coordinates of points (approximation by N points) }
function Avg(const P: PVertexArray; Count: Integer): TVertex;
{ Middle lines coordinates (approximation by 3 points }
function MiddleLine(const P: PVertexArray; Count: Integer): TVertex;
{ Smoothing by Gaussian Kernel (approximation by N points. N - odd number of points) }
function GaussianKernel(const P: PVertexArray; Count: Integer): TVertex;
{ Contiguous parabolic splines (approximation by 3 points) }
function ParabolicSplines(const P: PVertexArray; Count: Integer; t: Float): TVertex;
{ Contiguous parabolic splines (approximation by 4 points) }
function ParabolicSplines4(const P: PVertexArray; Count: Integer; t: Float): TVertex;
{ B-Splines (approximation by 4 points) }
function BSplines(const P: PVertexArray; Count: Integer; t: Float): TVertex;
{ Bezier crev (approximation by N points) }
function Bezier(const P: PFloatArray; Count: Integer; t: Float): Float;
{ Bezier splines (interpolation by 4 points) }
function BezierSplines(const P: PVertexArray; Count: Integer; t: Float): TVertex;
{ Cubic splines - Catmull Rom (interpolation by 4 points) }
function CatmullRom(const P: PVertexArray; Count: Integer; t: Float): TVertex;

{ Smoothing methods }
const
  smNone           = -1;
  smAvg            = 0;
  smMiddle         = 1;
  smGaussian       = 2;
  smParabolic      = 3;
  smBSplines       = 4;
  smBezier         = 5;
  smCubic          = 6;
  smCatmullRom     = 7;

{ Main smoothing function }
function SmoothContour(const Src: PContour; Dst: PContour; Closure: Boolean; Method: Integer): Boolean;

implementation

function        IsEq(a, b, Eps: Float): Boolean;
begin
  Result := Abs(a - b) <= Eps;
end;

function        IsNe(a, b, Eps: Float): Boolean;
begin
  Result := Abs(a - b) > Eps;
end;

function        IsGt(a, b, Eps: Float): Boolean;
begin
  Result := (a - b) > Eps;
end;

function        IsLs(a, b, Eps: Float): Boolean;
begin
  Result := (b - a) > Eps;
end;

function        IsGe(a, b, Eps: Float): Boolean;
begin
  Result := not IsLs(a, b, Eps);
end;

function        IsLe(a, b, Eps: Float): Boolean;
begin
  Result := not IsGt(a, b, Eps);
end;

function        GetNext(Index, Count: Integer): Integer;
begin
  Result := (Index + 1) mod Count;
end;

function        GetPrev(Index, Count: Integer): Integer;
begin
  Result := (Index - 1 + Count) mod Count;
end;

function        TranslateIndex(I, J, Width: Integer): Integer;
begin
  Result := J * Width + I;
end;

procedure Grow(var A: Pointer; var Capacity: Integer; Size: Integer);

  procedure SetCapacity(NewCapacity: Integer);
  begin
    if NewCapacity <> Capacity then
    begin
      ReallocMem(A, NewCapacity * Size);
      Capacity := NewCapacity;
    end;
  end;

var
  Delta: Integer;
begin
  if Capacity > 64 then
    Delta := Capacity div 4
  else
    if Capacity > 8 then
      Delta := 16
    else
      Delta := 4;
  SetCapacity(Capacity + Delta);
end;

function Append(var P: Pointer; var Capacity, Count: Integer; Size: Integer): Pointer;

  procedure SetCapacity(NewCapacity: Integer);
  begin
    if NewCapacity <> Capacity then
    begin
      ReallocMem(P, NewCapacity * Size);
      Capacity := NewCapacity;
    end;
  end;

  procedure Grow;
  var
    Delta: Integer;
  begin
    if Capacity > 64 then
      Delta := Capacity div 4
    else
      if Capacity > 8 then
        Delta := 16
      else
        Delta := 4;
    SetCapacity(Capacity + Delta);
  end;

begin
  if Count = Capacity then Grow;
  Result := @PByteArray(P)^[Count * Size];
  FillChar(Result^, Size, 0);
  Inc(Count);
end;

procedure Free(var P: Pointer);
begin
  if P <> nil then
  begin
    FreeMem(P);
    P := nil;
  end;
end;

function CopyContour(const Src: PContour): TContour;
begin
  if (Src <> nil) and (Src.Count > 0) then
  begin
    Result.Hole := Src.Hole;
    Result.Count := Src.Count;
    GetMem(Result.Vertices, SizeOf(TVertex) * Result.Count);
    Move(Src.Vertices^, Result.Vertices^, SizeOf(TVertex) * Result.Count);
  end
  else
    FillChar(Result, SizeOf(TContour), 0);
end;

function CopyPolygon(const Src: PPolygon): TPolygon;
var
  I: Integer;
begin
  if (Src <> nil) and (Src.Count > 0) then
  begin
    Result.Count := Src.Count;
    GetMem(Result.Contours, SizeOf(TContour) * Result.Count);
    for I := 0 to Src.Count - 1 do
      Result.Contours[I] := CopyContour(@Src.Contours[I]);
  end
  else
    FillChar(Result, SizeOf(TPolygon), 0);
end;

function AddVertex(C: PContour; V: TVertex): PVertex;
begin
  Inc(C.Count);
  ReallocMem(C.Vertices, SizeOf(TVertex) * C.Count);
  C.Vertices[C.Count - 1] := V;
  Result := @C.Vertices[C.Count - 1];
end;
                                                              
function AddContour(P: PPolygon; C: TContour): PContour;
begin
  Inc(P.Count);
  ReallocMem(P.Contours, SizeOf(TContour) * P.Count);
  P.Contours[P.Count - 1] := C;
  Result := @P.Contours[P.Count - 1];
end;

procedure FreeContour(C: PContour);
begin
  FreeMem(C.Vertices);
  FillChar(C^, SizeOf(TContour), 0);
end;

procedure FreePolygon(P: PPolygon);
var
  I: Integer;
begin
  for I := 0 to P.Count - 1 do
    FreeContour(@P.Contours[I]);
  FreeMem(Pointer(P.Contours));
  FillChar(P^, SizeOf(TPolygon), 0);
end;

procedure CloseContour(Contour: PContour);
begin
  if (Contour.Count > 2) and
     (not PntsEqu(@Contour.Vertices[0], @Contour.Vertices[Contour.Count - 1])) then
  begin
    Inc(Contour.Count);
    ReallocMem(Contour.Vertices, SizeOf(TVertex) * Contour.Count);
    Contour.Vertices[Contour.Count - 1] := Contour.Vertices[0];
    Contour.Hole := IsGt(GetSquare(Contour), 0.0, EpsFloat);
  end;
end;

procedure UncloseContour(Contour: PContour);
begin
  if (Contour.Count > 1) and
     (PntsEqu(@Contour.Vertices[0], @Contour.Vertices[Contour.Count - 1])) then
  begin
    Dec(Contour.Count);
    ReallocMem(Contour.Vertices, SizeOf(TVertex) * Contour.Count);
    Contour.Hole := False;
  end;
end;

procedure ChangeOrientation(Contour: PContour);
var
  L, R: Integer;
  T: TVertex;
begin
  L := 0;
  R := Contour.Count - 1;
  while L < R do
  begin
    T := Contour.Vertices[L];
    Contour.Vertices[L] := Contour.Vertices[R];
    Contour.Vertices[R] := T;
    Inc(L);
    Dec(R);
  end;
end;

function GetSquare(const Contour: PContour): Float;
var
  I: Integer;
begin
  Result := 0.0;
  for I := 1 to Contour.Count - 2 do
    Result := Result + Cross(@Contour.Vertices[0], @Contour.Vertices[I], @Contour.Vertices[0], @Contour.Vertices[I + 1]);
  Result := Result / 2.0;
end;

function GetPerimeter(const Contour: PContour): Float;
var
  I: Integer;
begin
  Result := 0.0;
  for I := 0 to Contour.Count - 2 do
    Result := Result + Distance(@Contour.Vertices[I], @Contour.Vertices[I + 1]);
end;

function GetContourBounds(const Contour: PContour; var R: TRectFloat): Boolean;
var
  I: Integer;
begin
  Result := False;
  R := EmptyRect();
  if Contour.Count > 0 then
  begin
    R.left := Contour.Vertices[0].x;
    R.right := R.left;
    R.top := Contour.Vertices[0].y;
    R.bottom := R.top;
    for I := 0 to Contour.Count - 1 do
    begin
      R.left := Min(R.left, Contour.Vertices[I].x);
      R.right := Max(R.right, Contour.Vertices[I].x);
      R.top := Max(R.top, Contour.Vertices[I].y);
      R.bottom := Min(R.bottom, Contour.Vertices[I].y);
    end;
    Result := True;
  end;
end;

function GetPolygonBounds(const Polygon: PPolygon; var R: TRectFloat): Boolean;
var
  I: Integer;
  Box: TRectFloat;
begin
  Result := False;
  R := EmptyRect();
  if Polygon.Count > 0 then
  begin
    GetContourBounds(@Polygon.Contours[0], R);
    for I := 1 to Polygon.Count - 1 do
      if GetContourBounds(@Polygon.Contours[I], Box) then
      begin
        R.left := Min(R.left, Box.left);
        R.top := Max(R.top, Box.top);
        R.right := Max(R.right, Box.right);
        R.bottom := Min(R.bottom, Box.bottom);
      end;
    Result := True;
  end;
end;

procedure SortContoursBySquare(Polygon: PPolygon);

  procedure _QuickSort(L, R: Integer);
  var
    I, J, M: Integer;
    S: Float;
    T: TContour;
  begin
    I := L;
    J := R;
    M := (L + R) shr 1;
    S := Abs(GetSquare(@Polygon.Contours[M]));
    repeat
      while IsGt(Abs(GetSquare(@Polygon.Contours[I])), S, EpsFloat) do Inc(I);
      while IsLs(Abs(GetSquare(@Polygon.Contours[J])), S, EpsFloat) do Dec(J);
      if I <= J then
      begin
        T := Polygon.Contours[I];
        Polygon.Contours[I] := Polygon.Contours[J];
        Polygon.Contours[J] := T;
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
    T: TContour;
  begin
    for I := 0 to Polygon.Count - 2 do
      for J := Polygon.Count - 1 downto I + 1 do
        if IsLs(Abs(GetSquare(@Polygon.Contours[I])), Abs(GetSquare(@Polygon.Contours[J])), EpsFloat) then
        begin
          T := Polygon.Contours[I];
          Polygon.Contours[I] := Polygon.Contours[J];
          Polygon.Contours[J] := T;
        end;
  end;

begin
  if (Polygon.Contours <> nil) and (Polygon.Count > 0) then
    if Polygon.Count > 2 then
      _QuickSort(0, Polygon.Count - 1)
    else
      _SelectionSort;
end;

function RectContour(Rect: TRectFloat): TContour;
begin
  Result.Count := 4;
  Result.Hole := False;
  GetMem(Result.Vertices, SizeOf(TVertex) * Result.Count);
  Result.Vertices[0] := Vertex(Rect.left, Rect.bottom);
  Result.Vertices[1] := Vertex(Rect.left, Rect.top);
  Result.Vertices[2] := Vertex(Rect.right, Rect.top);
  Result.Vertices[3] := Vertex(Rect.right, Rect.bottom);
  CloseContour(@Result);
end;

function PntsEqu(const v0, v1: PVertex): Boolean;
begin
  Result := IsEq(v0^.x, v1^.x, EpsFloat) and IsEq(v0^.y, v1^.y, EpsFloat);
end;

function Vertex(x, y: Float): TVertex;
begin
  Result.x := x;
  Result.y := y;
end;

function Cross(const v0, v1, v2, v3: PVertex): Float;
begin
  Result := (v1^.x - v0^.x)*(v3^.y - v2^.y) - (v1^.y - v0^.y)*(v3^.x - v2^.x);
end;

function Scalar(const v0, v1, v2, v3: PVertex): Float;
begin
  Result := (v1^.x - v0^.x)*(v3^.x - v2^.x) + (v1^.y - v0^.y)*(v3^.y - v2^.y);
end;

function Module(const v: PVertex): Float;
begin
  Result := Hypot(v^.x, v^.y);
end;

function Distance(const v0, v1: PVertex): Float;
begin
  Result := Hypot(v1^.x - v0^.x, v1^.y - v0^.y);
end;

function Norm(const v: PVertex): TVertex;
var
  l: Float;
begin
  l := Module(v);
  if IsGt(l, 0.0, EpsFloat) then
    Result := Vertex(v^.x / l, v^.y / l)
  else
    FillChar(Result, SizeOf(TVertex), 0);
end;

function Divide(const v0, v1: PVertex; t: Float): TVertex;
begin
  Result.x := v0^.x + t * (v1^.x - v0^.x);
  Result.y := v0^.y + t * (v1^.y - v0^.y);
end;

function Middle(const v0, v1: PVertex): TVertex;
begin
  Result.x := 0.5 * (v0^.x + v1^.x);
  Result.y := 0.5 * (v0^.y + v1^.y);
end;

function FixLine(const v0, v1: PVertex; Len: Float): TVertex;
var
  l: Float;
begin
  l := Distance(v0, v1);
  if IsGt(l, 0.0, EpsFloat) then
    Result := Divide(v0, v1, Len / l)
  else
    Result := v0^;
end;

function Sine(const v0, v1, v2: PVertex): Float;
var
  d: Float;
begin
  d := Distance(v0, v1) * Distance(v1, v2);
  if IsGt(d, 0.0, EpsFloat) then
    Result := Cross(v1, v0, v1, v2) / d
  else
    Result := 0.0;
end;

function Cosine(const v0, v1, v2: PVertex): Float;
var
  d: Float;
begin
  d := Distance(v0, v1) * Distance(v1, v2);
  if IsGt(d, 0.0, EpsFloat) then
    Result := Scalar(v1, v0, v1, v2) / d
  else
    Result := 0.0;
end;

function Rotate(const v0, v1: PVertex; r: Float): TVertex;
var
  v: TVertex;
begin
  v := Vertex(v1^.x - v0^.x, v1^.y - v0^.y);
  Result.x := v0^.x + v.x * Cos(r) - v.y * Sin(r);
  Result.y := v0^.y + v.x * Sin(r) + v.y * Cos(r);
end;

function Flip(const v0, v1: PVertex; o: Float): TVertex;
begin
  Result.x := v0^.x + o * (v1^.y - v0^.y);
  Result.y := v0^.y - o * (v1^.x - v0^.x);
end;

function Center(const v0, v1, v2: PVertex): TVertex;
var
  a, b, c, d, e, f, g: Float;
begin
  a := v1^.x - v0^.x;
  b := v1^.y - v0^.y;
  c := v2^.x - v0^.x;
  d := v2^.y - v0^.y;
  e := a * (v0^.x + v1^.x) + b * (v0^.y + v1^.y);
  f := c * (v0^.x + v2^.x) + d * (v0^.y + v2^.y);
  g := 2.0 * (a * (V2^.y - V1^.y) - b * (V2^.x - V1^.x));
  if IsNe(g, 0.0, EpsFloat) then
    Result := Vertex((d * e - b * f) / g, (a * f - c * e) / g)
  else
    FillChar(Result, SizeOf(TVertex), 0);
end;

function Quadrant(const v0, v1, v2: PVertex): Integer;
var
  c, s: Float;
begin
  c := Cross(v1, v0, v1, v2);
  s := Scalar(v1, v0, v1, v2);
  if IsGe(c, 0.0, EpsFloat) and IsGt(s, 0.0, EpsFloat) then
    Result := 1
  else
    if IsGt(c, 0.0, EpsFloat) and IsLe(s, 0.0, EpsFloat) then
      Result := 2
    else
      if IsLe(c, 0.0, EpsFloat) and IsLs(s, 0.0, EpsFloat) then
        Result := 3
      else
        if IsLs(c, 0.0, EpsFloat) and IsGe(s, 0.0, EpsFloat) then
          Result := 4
        else
          Result := -1;
end;

function Angle(const v0, v1, v2: PVertex): Float;
var
  s: Float;
begin
  s := ArcSin(Sine(v0, v1, v2));
  Result := s;
  Exit;
  case Quadrant(v0, v1, v2) of
    1: Result := s;
    2: Result := Pi - s;
    3: Result := Pi - s;
    4: Result := 2.0 * Pi + s;
    else
      Result := 0.0;
  end;
end;

function PntClassify(const v0, v1, v: PVertex): Integer;
var
  s: Float;
begin
  if PntsEqu(v0, v) then
    Result := ptHead
  else
    if PntsEqu(v1, v) then
      Result := ptTail
    else
    begin
      s := Cross(v, v0, v, v1);
      if IsGt(s, 0.0, EpsFloat) then
        Result :=  ptLeft
      else
        if IsLs(s, 0.0, EpsFloat) then
          Result := ptRight
        else
          if IsLs(Scalar(v1, v0, v1, v), 0.0, EpsFloat) then
            Result := ptBeyond
          else
            if IsLs(Scalar(v0, v1, v0, v), 0.0, EpsFloat) then
              Result := ptBehind
            else
              Result := ptBetween;
    end;
end;

function LinesIntersect(const v0, v1, v2, v3: PVertex; var t: Float): Integer;
var
  n: TVertex;
  s: Float;
begin
  n.x := v3^.y - v2^.y;
  n.y := v2^.x - v3^.x;
  s := n.x * (v1^.x - v0^.x) + n.y * (v1^.y - v0^.y);
  if IsEq(s, 0.0, EpsFloat) then
  begin
    case PntClassify(v2, v3, v0) of
      ptLeft,
      ptRight: Result := lnParallel;
    else
      Result := lnCollinear;
    end;
  end
  else
  begin
    t := -(n.X * (v0^.x - v2^.x) + n.y * (v0^.y - v2^.y)) / s;
    Result := lnSkew;
  end;
end;

function LinesCross(const v0, v1, v2, v3: PVertex; var t0, t1: Float): Integer;
begin
  Result := LinesIntersect(v0, v1, v2, v3, t0);
  if Result = lnSkew then
  begin
    Result := lnSkewNotCross;
    if IsGe(t0, 0.0, EpsFloat) and IsLe(t0, 1.0, EpsFloat) then
    begin
      LinesIntersect(v2, v3, v0, v1, t1);
      if IsGe(t1, 0.0, EpsFloat) and IsLe(t1, 1.0, EpsFloat) then
        Result := lnSkewCross;
    end;
  end;
end;

function IsDelonay(const v0, v1, v2, v3: PVertex): Boolean;
var
  CosA, CosB, SinA, SinB: Float;
begin
  Result := True;
  CosA := Scalar(v0, v1, v0, v3);
  CosB := Scalar(v2, v1, v2, v3);
  if not (IsGe(CosA, 0.0, EpsFloat) and IsGe(CosB, 0.0, EpsFloat)) then
  begin
    SinA := Cross(v0, v1, v0, v3);
    SinB := -Cross(v2, v1, v2, v3);
    Result := IsGe((SinA * CosB + CosA * SinB), 0.0, EpsFloat);
  end;
end;

function PntInRect(const r: PRectFloat; const v: PVertex): Integer;
begin
  Result := pnOutside;
  if IsGe(v^.x, r^.left, EpsFloat) and IsLe(v^.x, r^.right, EpsFloat) and
     IsGe(v^.y, r^.bottom, EpsFloat) and IsLe(v^.y, r^.top, EpsFloat) then
  begin
    if IsEq(v^.x, r^.left, EpsFloat) or IsEq(v^.x, r^.right, EpsFloat) or
       IsEq(v^.y, r^.bottom, EpsFloat) or IsEq(v^.y, r^.top, EpsFloat) then
      Result := pnBoundary
    else
      Result := pnInside;
  end;
end;

function PntInEllipse(const c: PVertex; const r0, r1: PFloat; const v: PVertex): Integer;
var
  DivSum: Float;
begin
  DivSum := Sqr((v.x - c.x) / r0^) + Sqr((v.y - c.y) / r1^);
  if IsLs(DivSum, 1.0, EpsFloat) then
    Result := pnInside
  else
    if IsEq(DivSum, 1.0, EpsFloat) then
      Result := pnBoundary
    else
      Result := pnOutside;
end;

function PntInTriangle(const v0, v1, v2, v: PVertex): Integer;
var
  d0, d1, d2: Float;
begin
  Result := pnOutside;
  d0 := Cross(v0, v1, v0, v);
  d1 := Cross(v0, v2, v0, v);
  if (IsGe(d0, 0.0, EpsFloat) and IsLe(d1, 0.0, EpsFloat)) or
     (IsLe(d0, 0.0, EpsFloat) and IsGe(d1, 0.0, EpsFloat)) then
  begin
    d2 := Cross(v1, v2, v1, v);
    if (IsGe(d1, 0.0, EpsFloat) and IsLe(d2, 0.0, EpsFloat)) or
       (IsLe(d1, 0.0, EpsFloat) and IsGe(d2, 0.0, EpsFloat)) then
      if IsEq(d0, 0.0, EpsFloat) or IsEq(d1, 0.0, EpsFloat) or IsEq(d2, 0.0, EpsFloat) then
        Result := pnBoundary
      else
        Result := pnInside;
  end;
end;

const
  lCrossing     = 0;
  lInessential  = 1;
  lTouching     = 2;

function EdgeType(v0, v1, v: PVertex): Integer;
begin
  Result := lInessential;
  if not PntsEqu(v0, v1) then
  begin
    case PntClassify(v0, v1, v) of
      ptLeft: Result := IfThen(IsGt(v^.y, v0^.y, EpsFloat) and IsLe(v^.y, v1^.y, EpsFloat), lCrossing, lInessential);
      ptRight: Result := IfThen(IsGt(v^.y, v1^.y, EpsFloat) and IsLe(v^.y, v0^.y, EpsFloat), lCrossing, lInessential);
      ptBetween,
      ptHead,
      ptTail: Result := lTouching;
    end;
  end;
end;

function PntInPolygon(const Polygon: PPolygon; const v: PVertex): Integer;
var
  I, J: Integer;
  Parity: Boolean;
begin
  Parity := False;
  for I := 0 to Polygon.Count - 1 do
    for J := 0 to Polygon.Contours[I].Count - 1 do
      case EdgeType(@Polygon.Contours[I].Vertices[J],
        @Polygon.Contours[I].Vertices[GetNext(J, Polygon.Contours[I].Count)], v) of
        lTouching:
        begin
          Result := pnBoundary;
          Exit;
        end;
        lCrossing: Parity := not Parity;
      end;
  Result := IfThen(Parity, pnInside, pnOutside);
end;

function PntOnContour(const Contour: PContour; const v: PVertex; d: Float): Boolean;
var
  I: Integer;
  v0, v1: PVertex;
  s: Float;
begin
  Result := False;
  if Contour.Count > 1 then
  begin
    d := Abs(d);
    for I := 0 to Contour.Count - 2 do
    begin
      v0 := @Contour.Vertices[I];
      v1 := @Contour.Vertices[I + 1];
      if IsGe(Scalar(v0, v1, v0, v), 0.0, EpsFloat) and IsGt(Scalar(v1, v0, v1, v), 0.0, EpsFloat) then
      begin
        s := Hypot(v1.x - v0.x, v1.y - v0.y) * d;
        if IsLe(Abs(Cross(v, v0, v, v1)), s, EpsFloat) then
        begin
          Result := True;
          Break;
        end;
      end;
    end;
  end;
end;

function IsRectEmpty(const r: PRectFloat): Boolean;
begin
  Result := IsLe(r.right, r.left, EpsFloat) or IsLe(r.top, r.bottom, EpsFloat);
end;

function IsRectsIntersect(const R1, R2: PRectFloat): Boolean;
begin
  Result := (not (IsGt(R1.left, R2.right, EpsFloat) or IsLs(R1.right, R2.left, EpsFloat))) and
            (not (IsGt(R1.bottom, R2.top, EpsFloat) or IsLs(R1.top, R2.bottom, EpsFloat)));
end;

function IntersectRectFloat(const R1, R2: PRectFloat): TRectFloat;
begin
  Result := R1^;
  if IsGt(R2.left, R1.left, EpsFloat) then Result.left := R2.left;
  if IsLs(R2.top, R1.top, EpsFloat) then Result.top := R2.top;
  if IsLs(R2.right, R1.right, EpsFloat) then Result.right := R2.right;
  if IsGt(R2.bottom, R1.bottom, EpsFloat) then Result.bottom := R2.bottom;
end;

function UnionRectFloat(const R1, R2: PRectFloat): TRectFloat;
begin
  Result.left := Min(R1.left, R2.left);
  Result.top := Max(R1.top, R2.top);
  Result.right := Max(R1.right, R2.right);
  Result.bottom := Min(R1.bottom, R2.bottom);
end;

function RectFloat(left, top, right, bottom: Float): TRectFloat;
begin
  Result.left := left;
  Result.top := top;
  Result.right := right;
  Result.bottom := bottom;
end;

function EmptyRect(): TRectFloat;
begin
  Result.left := MaxFloat;
  Result.top := -MaxFloat;
  Result.right := -MaxFloat;
  Result.bottom := MaxFloat;
end;

function BinarySerach(const P: PFloatArray; Count: Integer; Value: Float; var L, R: Integer): Integer;
var
  M: Integer;
begin
  Result := -1;
  L := 0;
  R := Count - 1;
  while L < R - 1 do
  begin
    M := (L + R) shr 1;
    if Value < P[M] then
      R := M
    else if Value > P[M] then
      L := M
    else
    begin
      Result := M;
      Break;
    end;
  end;
end;

function BinarySerach(const P: PFloatArray; Count: Integer; Value: Float): Integer;
var
  L, R, M: Integer;
begin
  Result := -1;
  if (P = nil) or (Count <= 0) then Exit;
  L := 0;
  R := Count - 1;
  repeat
    M := (L + R) shr 1;
    if IsLs(Value, P[M], EpsFloat) then
      R := M - 1
    else if IsGt(Value, P[M], EpsFloat) then
      L := M + 1
    else
    begin
      Result := M;
      Break;
    end;
  until L > R;
end;

function Avg(const P: PVertexArray; Count: Integer): TVertex;
var
  I: Integer;
begin
  Result := Vertex(0.0, 0.0);
  if Count > 0 then
  begin
    for I := 0 to Count - 1 do
    begin
      Result.x := Result.x + P[I].x;
      Result.y := Result.y + P[I].y;      
    end;
    Result.x := Result.x / Count;
    Result.y := Result.y / Count;    
  end;
end;

function MiddleLine(const P: PVertexArray; Count: Integer): TVertex;
begin
  Result.x := 0.25 * P[0].x + 0.5 * P[1].x + 0.25 * P[2].x;
  Result.y := 0.25 * P[0].y + 0.5 * P[1].y + 0.25 * P[2].y;
end;

function GaussianKernel(const P: PVertexArray; Count: Integer): TVertex;

  function Gaussian(x: Float): Float;
  const
    Sigma = 0.85;
  begin
    Result := 1.0 / (Sqrt(2.0 * Pi) * Sigma) * Exp(-x * x / (2.0 * Sigma * Sigma))
  end;

  function GaussianSum: Float;
  var
    I, Num: Integer;
  begin
    Result := 0.0;
    Num := Count div 2;
    for I := -Num to Num do
      Result := Result + Gaussian(I);
  end;

var
  I: Integer;
  G, S: Float;
begin
  Result.x := 0.0;
  Result.y := 0.0;
  for I := 0 to Count - 1 do
  begin
    G := Gaussian(I - Count div 2);
    Result.x := Result.x + P[I].x * G;
    Result.y := Result.y + P[I].y * G;    
  end;
  S := GaussianSum;
  Result.x := Result.x / S;
  Result.y := Result.y / S;
end;

function Bezier(const P: PFloatArray; Count: Integer; t: Float): Float;

  function _Factorial(N: Int64): Int64;
  begin
    if N > 1 then
      Result := N * _Factorial(N - 1)
    else
      Result := 1;
  end;

  function _Bernstein(I, N: Integer; t: Float): Float;
  begin
    Result := _Factorial(N) / (_Factorial(I) * _Factorial(N - I)) *
      IntPower(t, I) * IntPower(1.0 - t, N - I);
  end;

var
  I: Integer;
begin
  Result := 0.0;
  for I := 0 to Count - 1 do
    Result := Result + P[I] * _Bernstein(I, Count - 1, t);
end;

function GetTangent(const P0, P1, P2: PVertex; t: Float): TVertex;
var
  a: Float;
begin
  a := t * Distance(P1, P2) / Distance(P0, P2);
  Result := Vertex((P2^.x - P0^.x) * a, (P2^.y - P0^.y) * a);
end;

function BezierSplines(const P: PVertexArray; Count: Integer; t: Float): TVertex;
var
  L, R: TVertex;
  Pnts: array [0..3] of Float;
begin
  L := GetTangent(@P[0], @P[1], @P[2], 1.0 / 4.0);
  R := GetTangent(@P[3], @P[2], @P[1], 1.0 / 4.0);
  Pnts[0] := P[1].x;
  Pnts[1] := P[1].x + L.x;
  Pnts[2] := P[2].x + R.x;
  Pnts[3] := P[2].x;
  Result.x := Bezier(@Pnts, 4, t);
  Pnts[0] := P[1].y;
  Pnts[1] := P[1].y + L.y;
  Pnts[2] := P[2].y + R.y;
  Pnts[3] := P[2].y;
  Result.y := Bezier(@Pnts, 4, t);
end;

function CubicSplines(const P: PVertexArray; Count: Integer; t: Float): TVertex;
var
  t2, t3: Float;
  h0, h1, h2, h3: Float;
  L, R: TVertex;
begin
  L := GetTangent(@P[0], @P[1], @P[2], 1.0);
  R := GetTangent(@P[3], @P[2], @P[1], 1.0);
  t2 := t * t;
  t3 := t2 * t;
  h0 := 2.0 * t3 - 3.0 * t2 + 1.0;
  h1 := -2.0 * t3 + 3.0 * t2;
  h2 := t3 - 2.0 * t2 + t;
  h3 := t3 - t2;
  Result.x := h0 * P[1].x + h1 * P[2].x + h2 * L.x - h3 * R.x;
  Result.y := h0 * P[1].y + h1 * P[2].y + h2 * L.y - h3 * R.y;
end;

function CatmullRom(const P: PVertexArray; Count: Integer; t: Float): TVertex;
var
  s, t2, t3: Float;
  h0, h1, h2, h3: Float;
begin
  s := 1.0 - t;
  t2 := t * t;
  t3 := t2 * t;
  h0 := -t * S * S;
  h1 := 2.0 - 5.0 * t2 + 3.0 * t3;
  h2 := t * (1.0 + 4.0 * t - 3.0 * t2);
  h3 := t2 * S;
  Result.x := 0.5 * (h0 * P[0].x + h1 * P[1].x + h2 * P[2].x - h3 * P[3].x);
  Result.y := 0.5 * (h0 * P[0].y + h1 * P[1].y + h2 * P[2].y - h3 * P[3].y);
end;

function ParabolicSplines(const P: PVertexArray; Count: Integer; t: Float): TVertex;
var
  s, s2, t2: Float;
begin
  s := 1.0 - t;
  s2 := s * s;
  t2 := t * t;
  Result.x := 0.5 * ((P[0].x - P[1].x) * s2 + (P[2].x - P[1].x) * t2 + 2.0 * P[1].x);
  Result.y := 0.5 * ((P[0].y - P[1].y) * s2 + (P[2].y - P[1].y) * t2 + 2.0 * P[1].y);
end;

function ParabolicSplines4(const P: PVertexArray; Count: Integer; t: Float): TVertex;
begin
  if IsLe(t, 0.5, EpsFloat) then
    Result := ParabolicSplines(@P[0], 3, t + 0.5)
  else
    Result := ParabolicSplines(@P[1], 3, t - 0.5);
end;

function BSplines(const P: PVertexArray; Count: Integer; t: Float): TVertex;
var
  s, t2, t3: Float;
  h0, h1, h2, h3: Float;
begin
  s := 1.0 - t;
  t2 := t * t;
  t3 := t2 * t;
  h0 := s * s * s / 6.0;
  h1 := (3.0 * t3 - 6.0 * t2 + 4.0) / 6.0;
  h2 := (-3.0 * t3 + 3.0 * t2 + 3.0 * t + 1.0) / 6.0;
  h3 := t3 / 6.0;
  Result.x := h0 * P[0].x + h1 * P[1].x + h2 * P[2].x + h3 * P[3].x;
  Result.y := h0 * P[0].y + h1 * P[1].y + h2 * P[2].y + h3 * P[3].y;
end;

procedure ExpandArray(const Src: PVertexArray; SrcCount: Integer;
  var Dst: PVertexArray; var DstCount: Integer; L, R: Integer; Closure: Boolean);
var
  I: Integer;  
begin
  DstCount :=  L + SrcCount + R;
  GetMem(Dst, DstCount * SizeOf(TVertex));
  Move(Src^, Dst[L],  SrcCount * SizeOf(TVertex));
  if Closure then
  begin
    Move(Src[SrcCount - L], Dst^, L * SizeOf(TVertex));
    Move(Src^, Dst[DstCount - R], R * SizeOf(TVertex));
  end
  else
  begin
    for I := L - 1 downto 0 do
    begin
      Dst[I].x := 2 * Dst[I + 1].x - Dst[I + 2].x;
      Dst[I].y := 2 * Dst[I + 1].y - Dst[I + 2].y;
    end;
    for I := DstCount - R to DstCount - 1 do
    begin
      Dst[I].x := 2 * Dst[I - 1].x - Dst[I - 2].x;
      Dst[I].y := 2 * Dst[I - 1].y - Dst[I - 2].y;
    end;
  end;
end;

type
  TFixSmooth = function (const P: PVertexArray; Count: Integer): TVertex;
  TExpSmooth = function (const P: PVertexArray; Count: Integer; t: Float): TVertex;

procedure SmoothFixPnts(const Src: PVertexArray; SrcCount: Integer; var Dst: PVertexArray;
  var DstCount: Integer; L, R: Integer; Closure: Boolean; F: TFixSmooth);
var
  I, TmpCount: Integer;
  Tmp: PVertexArray;
begin
  ExpandArray(Src, SrcCount, Tmp, TmpCount, L, R, Closure);
  DstCount := SrcCount;
  GetMem(Dst, DstCount * Sizeof(TVertex));
  for I := 0 to SrcCount - 1 do
    Dst[I] := F(@Tmp[I], L + 1 + R);
  Free(Pointer(Tmp));
end;

procedure SmoothExpPnts(const Src: PVertexArray; SrcCount: Integer; var Dst: PVertexArray;
  var DstCount: Integer; L, R, AuxPoints: Integer; Closure: Boolean; F: TExpSmooth);
var
  I, J, K: Integer;
  dt: Float;  
  TmpCount, AuxCount: Integer;
  Tmp: PVertexArray;
begin
  ExpandArray(Src, SrcCount, Tmp, TmpCount, L, R, Closure);
  DstCount := SrcCount + IfThen(Closure, SrcCount * AuxPoints, (SrcCount - 1) * AuxPoints);
  GetMem(Dst, DstCount * SizeOf(TVertex));
  dt := 1.0 / (AuxPoints + 1);
  K := 0;
  for I := 0 to SrcCount - 1 do
  begin
    AuxCount := IfThen((not Closure) and (I = SrcCount - 1), 0, AuxPoints);
    for J := 0 to AuxCount do
      Dst[K + J] := F(@Tmp[I], 4, dt * J);
    Inc(K, AuxPoints + 1);
  end;
  Free(Pointer(Tmp));
end;

function SmoothContour(const Src: PContour; Dst: PContour; Closure: Boolean; Method: Integer): Boolean;
const
  AuxPoints = 3;
begin
  Result := False;
  FillChar(Dst^, SizeOf(TContour), 0);
  Dst.Hole := Src.Hole;
  if Src.Count >= 3 then
  begin
    case Method of
      smAvg:        SmoothFixPnts(Src.Vertices, Src.Count, Dst.Vertices, Dst.Count, 1, 1, Closure, @Avg);
      smMiddle:     SmoothFixPnts(Src.Vertices, Src.Count, Dst.Vertices, Dst.Count, 1, 1, Closure, @MiddleLine);
      smGaussian:   SmoothFixPnts(Src.Vertices, Src.Count, Dst.Vertices, Dst.Count, 2, 2, Closure, @GaussianKernel);
      smParabolic:  SmoothExpPnts(Src.Vertices, Src.Count, Dst.Vertices, Dst.Count, 1, 2, AuxPoints, Closure, @ParabolicSplines4);
      smBSplines:   SmoothExpPnts(Src.Vertices, Src.Count, Dst.Vertices, Dst.Count, 1, 2, AuxPoints, Closure, @BSplines);
      smBezier:     SmoothExpPnts(Src.Vertices, Src.Count, Dst.Vertices, Dst.Count, 1, 2, AuxPoints, Closure, @BezierSplines);
      smCubic:      SmoothExpPnts(Src.Vertices, Src.Count, Dst.Vertices, Dst.Count, 1, 2, AuxPoints, Closure, @CubicSplines);
      smCatmullRom: SmoothExpPnts(Src.Vertices, Src.Count, Dst.Vertices, Dst.Count, 1, 2, AuxPoints, Closure, @CatmullRom);
      else
        Dst^ := CopyContour(Src);
    end;
    Result := True;
  end;
end;

end.
