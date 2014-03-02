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
    :else            [`add a b]))

; Until we handle distributive property, answer isn't fully reduced
(defn mul [a b]
  (cond
    (or (= a (zero)) (= b (zero))) (zero)
    (and (number? a) (number? b)) (* a b)
    :else            [`mul a b]))

;This is basically a macro to define subtraction as addition
(defn sub [a b]
  (add a (mul (minusone) b)))

;Maybe refactor into inverse and multiply
;Not quite sure how this API should handle div by zero
(defn div [a b]
  (cond
    ; Computations should prevent this possibility
    (= b (zero))     [`undefined]
    (= b (one))      a
    (and (number? a) (number? b)) (/ a b)
    :else            [`div a b]))

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
(defn point [a b]
  [`point [`val a b]])

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
(defn line [a b c]
  (cond
    (and (= a (zero)) (= b (zero)) (not (= c (zero)))) [`line `undefined]
    :else                                         [`line [`val a b c]]))

; Constructors for when we need to symbolically represent a
; typed quadrance
(defn quadrance [a]
  [`quadrance a])

; Constructors for when we need to symbolically represent a
; typed quadrance
(defn quadranceFromLength [a]
  [`quadrance (mul a a)])

; A type marker for a spread
(defn spread [a]
  [`spread a])

; Functions taking 2 point arguments use this
(defn use2Points [pa pb f]
  (def va (get pa 1))
  (def vb (get pb 1))
  (def x1 (get va 1))
  (def x2 (get vb 1))
  (def y1 (get va 2))
  (def y2 (get vb 2))
  (f x1 y1 x2 y2))

; Functions taking 2 line arguments use this
(defn use2Lines [l1 l2 f]
  (def ln1 (get l1 1))
  (def ln2 (get l2 1))
  (def a1  (get ln1 1))
  (def b1  (get ln1 2))
  (def c1  (get ln1 3))
  (def a2  (get ln2 1))
  (def b2  (get ln2 2))
  (def c2  (get ln2 3))
  (f a1 b1 c1 a2 b2 c2))

; Functions using line and point arguments use this
(defn useLinePoint [l1 pa f]
  (def ln1 (get l1 1))
  (def a1  (get ln1 1))
  (def b1  (get ln1 2))
  (def c1  (get ln1 3))
  (def va  (get pa 1))
  (def x   (get va 1))
  (def y   (get va 2))
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
      (= d (zero)) [`spread `undefined]
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
    (cond
      (and (= a (zero)) (= b (zero))) [`line `undefined]
      :else                           (line a b c)))
  (use2Points pa pb _use2PointsForLine)) 


;Given 2 lines, compute the intersection
; Return undefined if they are parallel
(defn intersectionPointFrom2Lines [l1 l2]
  (defn _use2LinesIntersection [a1 b1 c1 a2 b2 c2]
    (def xn (sub (mul b1 c2) (mul b2 c1))) 
    (def yn (sub (mul c1 a2) (mul c2 a1)))
    (def d  (sub (mul a1 b2) (mul a2 b1)))
    (cond
      (= d (zero)) [`point `undefined]
      :else        (point (div xn d) (div yn d))))
  (use2Lines l1 l2 _use2LinesIntersection))

; Given a line and a point, find a new line that goes through the point,
; such that this line is parallel to the existing one.
(defn parallelFromLinePoint [l1 pa]
  (defn _parallelLinePoint [a1 b1 c1 x y]
    (def a a1)
    (def b b1)
    (def c (mul (minusone) (add (mul a1 x) (mul b1 y))))
    (line a b c))
  (useLinePoint l1 pa _parallelLinePoint))

; Given a line and a point, find a line that goes through both
(defn altitudeFromLinePoint [l1 pa]
  (defn _altitudeLinePoint [a1 b1 c1 x y]
    (def a (mul (minusone) b1))
    (def b a1)
    (def c (sub (mul b1 x) (mul a1 y)))
    (line a b c))
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
(def pA (point 0 0))
(def pB (point 4 0))
(def pC (point 4 3))
(def lC (lineFrom2Points pA pB))
(def lB (lineFrom2Points pC pA))
(def lA (lineFrom2Points pB pC))

; Some line not associated with any points
(def lD (line 3 -1 6))
(def lF (line 5 2 9))
; And a point generated at their intersection
(def pD (intersectionPointFrom2Lines lD lF))

; Generate a new point
(def lE (altitudeFromLinePoint lC pD))

(printd pD)
(printd (quadranceFrom2Points pA pC))
(printd (spreadFrom2Lines lD lC))
(printd (parallelFromLinePoint lE pA))



