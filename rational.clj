;
; Algebraic Geometry done according to Normal Wildberger's
;  Rational Trigonometry
;
; Rational Trigonometry works over large classes of fields,
; including prime finite fields, rationals, reals, etc.
;
; However, in order to retain exact definitions of perpendicularity
; and parallelism, avoiding using the reals unnecessarily is advised.
; If input is specified in terms of integers and rationals, then output
; will also be integers and rationals (when expressed in terms of
; quadrance and spread)
;
(use `clojure.pprint)

;
; Miscellaneous stuff used in this file
;

(defn printd [obj]
  (binding [*print-suppress-namespaces* true] (pprint obj)))

(defn at0 [a]
  (first a))

(defn at1 [a]
  (first (rest a)))

(defn at2 [a]
  (first (rest (rest a))))

(defn at3 [a]
  (first (rest (rest (rest a)))))


;
; These functions could be replaced with implementations
; that define different fields.  We have a field that
; will start with integers, promote to rationals, and
; then promote to floats.  If you don't allow floats to
; pollute the result, then equality checks are exact,
; which is important for parallel and perpendicular checks
; Also, if results include non-numerics, they answers can grow
; with the complexity of the calculations used to produce it.
; Currently, a few things like implementation of the distributive law
; are not written which would otherwise give a more reduced result.
;
; Todo: do fields need to be commutative?  If not, then there may
; be bugs in my code based on commutativity assumptions.
(declare add)
(declare mul)
(declare div)
(declare sub)
(declare one)
(declare zero)
(declare minusone)

(defn minusone [] -1)
(defn one      []  1)
(defn zero     []  0)

; Until we handle distributive property, answer isn't fully reduced
(defn add [a b]
  (cond
    (= a (zero))     b
    (= b (zero))     a
    (and (number? a) (number? b)) (+ a b)
    :else            (list `add a b)))

; Until we handle distributive property, answer isn't fully reduced
(defn mul [a b]
  (cond
    (or (= a (zero)) (= b (zero))) (zero)
    (and (number? a) (number? b)) (* a b)
    :else            (list `mul a b)))

;This is basically a macro to define subtraction as addition
(defn sub [a b]
  (add a (mul (minusone) b)))

;Maybe refactor into inverse and multiply
;Not quite sure how this API should handle div by zero
(defn div [a b]
  (cond
    ; Computations should prevent this possibility
    (= b (zero))     (list `undefined `div a b)
    (= a (zero))     a
    (= b (one))      a
    (and (number? a) (number? b)) (/ a b)
    :else            (list `div a b)))

(defn nchoosek [n k]
  (defn _nchoosekiter [m i]
    (/ (- (+ m 1 ) i) i))
  (defn _nchoosek [n k i]
    (cond
      (< i 1) 1
      :else (* 
        (_nchoosekiter n i)
        (_nchoosek n k (- i 1))))) 
  (_nchoosek n k k))

; Get the nth polynomial value for s
(defn spreadpoly [n s]
  ; (-4s)^p
  (defn _d [s p]
    (cond
      (= p 0) 1
      :else (* (* -4 s) (_d s (- p 1)))))
  (defn _iter [n s k]
    ; n/(n-k)
    (def a (/ n (- n k)))
    ; 2n - 1 - k
    (def b (- (- (* 2 n) 1) k))
    (def c (nchoosek b k))
    ; _d[n - 1 - k]
    (def d (_d s (- (- n 1) k)))
    (* a c d))
  (defn spreadpolysum [n s k]
    (cond
      (< k 0) 0
      :else (+ (_iter n s k) (spreadpolysum n s (- k 1)))))    
  (* s (spreadpolysum n s (- n 1))))
;
; Now we can start to define geometry
;
; For now, we have a problem with being forced to actualize points and lengths.
; It is perfectly fine to solve triangles based on ratios such that
; no lengths or absolute points are actually known. It is also ok
; to only know single points or lengths, and let all other items
; move around to fit constraints
;
; This is a type marker
(defn pointFromCoordinates [x y]
  (list `point (list `xy x y)))

; Lines are defined as
;   a*x + b*y + c = 0
;
; And notated as a ratio:
;
;  < a : b : c >
;
; Where the 2d slope is completely determined by a and b,
; which means that all lines with same a : b are parallel.
; But lines that are multiples of each other are also equal, because
; the line is defined by setting the equation equal to zero.
; This means that a normalization rule for lines would pick a convention
; to divide everything by b if it isn't zero, else to divide everything
; by a.  In that case, equivalent lines are numerically equal.
; And following that convention, the comparison can ignore c just
; to check that the lines are parallel.
;
; Be sure to check the condition for an undefined line, when
; the equation is impossible to satisfy for any (x,y), when
; a=b=0 and c != 0
;
;
(defn lineFromEquation [a b c]
  (def _invalidLine
    (and (= a (zero)) (= b (zero)) (not (= c (zero)))))
  (cond
    _invalidLine (list `line `lineFromEquation `undefined a b c)
    :else        (list `line (list `abc a b c))))

; Constructors for when we need to symbolically represent a
; typed quadrance
(defn quadrance [a]
  (list `quadrance a))

; Constructors for when we need to symbolically represent a
; typed quadrance
(defn quadranceFromLength [a]
  (list `quadrance (mul a a)))

; A type marker for a spread
(defn spread [a]
  (list `spread a))

; Functions taking 2 point arguments use this
(defn use2Points [pa pb f]
  (def va (at1 pa))
  (def vb (at1 pb))
  (def x1 (at1 va))
  (def x2 (at1 vb))
  (def y1 (at2 va))
  (def y2 (at2 vb))
  (f x1 y1 x2 y2))

; Functions taking 2 line arguments use this
(defn use2Lines [l1 l2 f]
  (def ln1 (at1 l1))
  (def ln2 (at1 l2))
  (def a1  (at1 ln1))
  (def b1  (at2 ln1))
  (def c1  (at3 ln1))
  (def a2  (at1 ln2))
  (def b2  (at2 ln2))
  (def c2  (at3 ln2))
  (f a1 b1 c1 a2 b2 c2))

; Functions using line and point arguments use this
(defn useLinePoint [l1 pa f]
  (def ln1 (at1 l1))
  (def a1  (at1 ln1))
  (def b1  (at2 ln1))
  (def c1  (at3 ln1))
  (def va  (at1 pa))
  (def x   (at1 va))
  (def y   (at2 va))
  (f a1 b1 c1 x y))
 
; Find the spread between 2 lines
; Or return undefined if they are parallel
(defn spreadFrom2Lines [l1 l2]
  (defn _use2LinesSpread [a1 b1 c1 a2 b2 c2]
    (def n  (sub (mul a1 b2) (mul a2 b1)))
    (def n2 (mul n n))
    (def d1 (add (mul a1 a1) (mul b1 b1)))  
    (def d2 (add (mul a2 a2) (mul b2 b2)))
    (def d  (mul d1 d2))
    (cond
      ;Undefined spread is parallel lines
      ;This isnt so much a failed computation as it
      ;is a test for parallel lines
      (= d (zero)) (list `spread `undefined `spreadFrom2Lines l1 l2)
      :else        (spread (div n2 d))))
  (use2Lines l1 l2 _use2LinesSpread))

; Find distance squared (quadrance) between two points   
(defn quadranceFrom2Points [pa pb]
  (defn _use2PointsForQuadrance [x1 y1 x2 y2]
    (def dx (sub x2 x1))
    (def dy (sub y2 y1))
    (add (mul dx dx) (mul dy dy)))
  (use2Points pa pb _use2PointsForQuadrance))

 
;Given 2 points, compute the line through them
;If points are the same, then the line is undefined
(defn lineFrom2Points [pa pb]
  (defn _use2PointsForLine [x1 y1 x2 y2]
    (def a (sub y1 y2))
    (def b (sub x2 y1))
    (def c (sub (mul x1 y2) (mul x2 y1)))
    (def _invalidLine (and (= a (zero)) (= b (zero))))
    (cond
      ;The line is undefined, but the undefined line would exist
      ;around pa if the distance to pb was infinitely small
      _invalidLine (list `line `undefined pa pb)
      :else                           (lineFromEquation a b c)))
  (use2Points pa pb _use2PointsForLine)) 


;Given 2 lines, compute the intersection
; Return undefined if they are parallel
(defn intersectionPointFrom2Lines [l1 l2]
  (defn _use2LinesIntersection [a1 b1 c1 a2 b2 c2]
    (def xn (sub (mul b1 c2) (mul b2 c1))) 
    (def yn (sub (mul c1 a2) (mul c2 a1)))
    (def d  (sub (mul a1 b2) (mul a2 b1)))
    (cond
      (= d (zero)) (list `point `undefined `intersectionPointFrom2Lines l1 l2)
      :else        (pointFromCoordinates (div xn d) (div yn d))))
  (use2Lines l1 l2 _use2LinesIntersection))

; Given a line and a point, find a new line that goes through the point,
; such that this line is parallel to the existing one.
(defn parallelFromLinePoint [l1 pa]
  (defn _parallelLinePoint [a1 b1 c1 x y]
    (def a a1)
    (def b b1)
    (def c (mul (minusone) (add (mul a1 x) (mul b1 y))))
    (lineFromEquation a b c))
  (useLinePoint l1 pa _parallelLinePoint))

; Given a line and a point, find a line that goes through both
(defn altitudeFromLinePoint [l1 pa]
  (defn _altitudeLinePoint [a1 b1 c1 x y]
    (def a (mul (minusone) b1))
    (def b a1)
    (def c (sub (mul b1 x) (mul a1 y)))
    (lineFromEquation a b c))
  (useLinePoint l1 pa _altitudeLinePoint))

; TODO: given a reference line with a point on it and a spread,
; compute possible points for a given quadrance along that spread
;
; Define example problems to test the APIs 
;
; For now, the definitions need to be nested so that
; inner objects are actualized.  Since lines can be
; determined from points, or points from lines, doing this
; sorting is something that the code could do eventually.
; But for now, the symbolic manipulation is too simplistic.
;

; Some points, and the associated lines with making a triangle of them
(def pA (pointFromCoordinates 0 0))
(def pB (pointFromCoordinates 4 0))
(def pC (pointFromCoordinates 4 3))
(def lC (lineFrom2Points pA pB))
(def lB (lineFrom2Points pC pA))
(def lA (lineFrom2Points pB pC))

(def lD (lineFromEquation 3 -1 6))

(def lF (lineFromEquation 5 2 9))
(def pD (intersectionPointFrom2Lines lD lF))
(def lE (altitudeFromLinePoint lC pD))
(def lP (parallelFromLinePoint lE pA))

;(def answer (intersectionPointFrom2Lines lA lP))
;(printd answer)

(printd lA)
(printd lP)
(def problem1 `(intersectionPointFrom2Lines lA lP))
(printd problem1)
(printd (eval problem1))

(printd (spreadpoly 3 (/ 2 5)))
