#lang racket/base
(require racket/flonum
         racket/list
         racket/math
         racket/fixnum
         racket/contract
         racket/match
         mode-lambda
         mode-lambda/color
         apse/core)

(struct animation (frames))

(define APSE-PALETTE
  (for/list ([i (in-range PALETTE-DEPTH)])
    (argb 255 255 0 i)))

(match-define
  (list _TRANS _BLACK
        AShad1 ABase ATint1
        BShad1 BBase BTint1
        CShad1 CBase CTint1
        Hi1 Hi2 Hi3 Hi4
        _WHITE)
  APSE-PALETTE)

(define (apse-tone base)
  (list (fourth (color->shades base 7))
        base
        (fourth (color->tint base 7))))

(define (apse-palette A B C Hi1 Hi2 Hi3 Hi4)
  (append (list TRANSPARENT BLACK)
          (apse-tone A) (apse-tone B) (apse-tone C)
          (list Hi1 Hi2 Hi3 Hi4)
          (list WHITE)))

(define CHAR->COLOR
  (hasheq #\_ _TRANS
          #\$ _BLACK
          #\q ATint1 #\w BTint1 #\e CTint1
          #\a  ABase #\s  BBase #\d  CBase
          #\z AShad1 #\x BShad1 #\c CShad1
          #\f Hi1 #\g Hi2 #\h Hi3 #\j Hi4
          #\! _WHITE))

(define (qrow->row qr)
  (for/list ([c (in-string (symbol->string qr))])
    (hash-ref CHAR->COLOR c)))

(define (show-correct-size w h)
  (printf "~ax~a is:\n" w h)
  (define row (string->symbol (make-string w #\_)))
  (for ([_ (in-range (add1 h))])
    (printf "  ~a\n" row)))

(define (add-sprite!/rows sd n qrows
                          #:w w #:h h)
  (define rows (map qrow->row qrows))
  (unless (= h (length rows))
    (show-correct-size w h)
    (error 'add-sprite!/rows "height is wrong ~v" (length rows)))
  (for ([r (in-list rows)]
        [i (in-naturals)])
    (define l (length r))
    (unless (= w l)
      (show-correct-size w h)
      (error 'add-sprite!/rows "row ~v is wrong length ~v" i l)))
  (define pal 'pal:apse)
  (define bs (make-bytes (* 4 w h)))
  (for ([y (in-naturals)]
        [row (in-list rows)])
    (for ([x (in-naturals)]
          [c (in-list row)])
      (bytes-copy! bs (+ (* 4 w y) (* 4 x)) c)))
  (add-sprite!
   sd
   (λ ()
     (vector n pal w h bs))))

(define-syntax-rule (define-sprite sd n #:w w #:h h row ...)
  (begin
    (define n 'n)
    (add-sprite!/rows sd n '(row ...)
                      #:w w #:h h)))

(define (initialize-apse! sd)
  (define-sprite sd spr:point #:w 1 #:h 1 $)

  (define-sprite sd spr:point:TRANS #:w 1 #:h 1 _)
  (define-sprite sd spr:point:BLACK #:w 1 #:h 1 $)
  (define-sprite sd spr:point:WHITE #:w 1 #:h 1 !)

  (define-sprite sd spr:point:Hi1 #:w 1 #:h 1 f)
  (define-sprite sd spr:point:Hi2 #:w 1 #:h 1 g)
  (define-sprite sd spr:point:Hi3 #:w 1 #:h 1 h)
  (define-sprite sd spr:point:Hi4 #:w 1 #:h 1 j)

  (define-sprite sd spr:point:ATint1 #:w 1 #:h 1 q)
  (define-sprite sd spr:point:ABase #:w 1 #:h 1 a)
  (define-sprite sd spr:point:AShad1 #:w 1 #:h 1 z)

  (define-sprite sd spr:point:BTint1 #:w 1 #:h 1 w)
  (define-sprite sd spr:point:BBase #:w 1 #:h 1 s)
  (define-sprite sd spr:point:BShad1 #:w 1 #:h 1 x)

  (define-sprite sd spr:point:CTint1 #:w 1 #:h 1 e)
  (define-sprite sd spr:point:CBase #:w 1 #:h 1 d)
  (define-sprite sd spr:point:CShad1 #:w 1 #:h 1 c)

  (add-palette! sd 'pal:apse APSE-PALETTE))

(define (checkerboard csd W.0 H.0)
  (define S 8.0)
  (define sp (sprite-idx csd 'spr:point))
  (cons (sprite (fl/ W.0 2.0) (fl/ H.0 2.0) sp
                #:layer 0
                #:mx W.0 #:my H.0
                #:r 131 #:g 139 #:b 131)
        (for*/list ([c (in-range (sub1 (floor (/ W.0 S))))]
                    [r (in-range (floor (/ H.0 S)))]
                    #:when (or (and (even? c) (odd? r))
                               (and (odd? c) (even? r))))
          (define cx (fl+ (fl* S (fx->fl c)) (fl* 2.0 (fl/ S 2.0))))
          (define cy (fl+ (fl* S (fx->fl r)) (fl/ S 2.0)))
          (sprite cx cy sp #:mx S #:my S
                  #:layer 1
                  #:r 131 #:g 100 #:b 131))))

(define (apse-all-sprites scale pals)
  (λ (csd W H)
    (define W.0 (fx->fl W))
    (define H.0 (fx->fl H))
    (define buffer 2.0)

    (local-require mode-lambda/core)
    (define (loop-body st i idx start-y end-y start-x)
      (define pi
        (palette-idx csd (list-ref pals (modulo i (length pals)))))
      (define w (sprite-width csd idx))
      (define h (sprite-height csd idx))
      (define m scale)
      (define cx (+ start-x (/ (* m w) 2)))
      (define cy (+ start-y (/ (* m h) 2)))
      (define new-start-y start-y)
      (define new-end-y (max end-y (+ start-y (* m h))))
      (define new-start-x (+ start-x (* m w) buffer))
      (cond
        [(< W.0 new-start-x)
         (loop-body st i idx (+ buffer new-end-y) (+ buffer new-end-y) buffer)]
        [else
         (values (cons (sprite cx cy idx
                               #:layer 2
                               #:mx m #:my m
                               #:pal-idx pi)
                       st)
                 new-start-y
                 new-end-y
                 new-start-x)]))
    (define-values (st last-start-y last-end-y last-start-x)
      (for/fold ([st #f]
                 [start-y buffer] [end-y buffer]
                 [start-x buffer])
                ([(spr idx) (in-hash (compiled-sprite-db-spr->idx csd))]
                 [i (in-naturals)])
        (loop-body st i idx start-y end-y start-x)))

    (define cb (checkerboard csd W.0 H.0))
    (apse-inst 0.0
               (λ ()
                 (cons cb st)))))

(define (st:sprite-frame csd W H scaled-pals palette-pals spr)
  (define W.0 (fx->fl W))
  (define H.0 (fx->fl H))
  (define si (sprite-idx csd spr))
  (define w (fx->fl (sprite-width csd si)))
  (define h (sprite-height csd si))

  (define-values (left max-i)
    (let loop ([left W.0] [i 1])
      (define iw (fl+ 1.0 (fl* w (fx->fl i))))
      (cond
        [(fl< iw left)
         (loop (fl- left iw) (fx+ 1 i))]
        [else
         (values left i)])))
  (define initial-lx (fl/ left 2.0))

  (define cy (fl/ (fx->fl (* max-i h)) 2.0))

  (define-values (_0 scaled-seq)
    (for/fold ([lx initial-lx] [st #f])
              ([i (in-range 1 max-i)])
      (define pal (list-ref scaled-pals (modulo i (length scaled-pals))))
      (define pi (palette-idx csd pal))
      (define m (fx->fl i))
      (define mw (fl* m w))
      (define cx (fl+ lx (fl/ mw 2.0)))
      (values (fl+ 1.0 (fl+ cx (fl/ mw 2.0)))
              (cons (sprite cx cy si
                            #:mx m #:my m
                            #:layer 2
                            #:pal-idx pi)
                    st))))

  (define-values (_1 palette-seq)
    (for/fold ([lx 1.0] [st #f])
              ([pal (in-list palette-pals)]
               [i (in-naturals)])
      (define pi (palette-idx csd pal))
      (define cx (fl+ lx (fl/ w 2.0)))
      (values (fl+ 1.0 (fl+ cx (fl/ w 2.0)))
              (cond
                [(fl<= (fl+ cx (fl/ w 2.0)) W.0)
                 (cons (sprite cx
                               (fl- (fl- H.0 (fl/ (fx->fl h) 2.0)) 1.0)
                               si
                               #:layer 2
                               #:pal-idx pi)
                       st)]
                [else
                 st]))))

  (define cb (checkerboard csd W.0 H.0))
  (list cb scaled-seq palette-seq))

(define (apse-sprite spr
                     #:palettes pals)
  (λ (csd W H)
    (define scaled-pals (shuffle pals))
    (define palette-pals (shuffle pals))
    (define st (st:sprite-frame csd W H scaled-pals palette-pals spr))
    (apse-inst 0.0 (λ () st))))

(define (apse-animation anim
                        #:fps [fps 60.0]
                        #:palettes pals)
  (λ (csd W H)
    (define scaled-pals (shuffle pals))
    (define palette-pals (shuffle pals))
    (define i 0)
    (apse-inst fps
               (λ ()
                 (define spr (list-ref (animation-frames anim) i))
                 (set! i (modulo (+ 1 i) (length (animation-frames anim))))
                 (st:sprite-frame csd W H scaled-pals palette-pals spr)))))

(define (apse-palettes pals)
  (λ (csd W H)
    (define W.0 (fx->fl W))
    (define H.0 (fx->fl H))
    (define cb (checkerboard csd W.0 H.0))
    (define S 8.0)
    (define cols-per-pal 5)
    (define ds
      '([1 0 spr:point:TRANS]
        [2 0 spr:point:BLACK]

        [3 0 spr:point:ATint1]
        [3 1 spr:point:ABase]
        [3 2 spr:point:AShad1]

        [4 0 spr:point:BTint1]
        [4 1 spr:point:BBase]
        [4 2 spr:point:BShad1]

        [5 0 spr:point:CTint1]
        [5 1 spr:point:CBase]
        [5 2 spr:point:CShad1]

        [6 0 spr:point:Hi1]
        [6 1 spr:point:Hi2]
        [6 2 spr:point:Hi3]
        [6 3 spr:point:Hi4]

        [7 0 spr:point:WHITE]))

    (define r 1)
    (define base-c 0)

    (define (calc-cx col)
      (fl+ (fl* S (fx->fl col))
           (fl* 2.0 (fl/ S 2.0))))
    (define (calc-cy row)
      (fl+ (fl* S (fx->fl row))
           (fl/ S 2.0)))

    (define pst
      (for/list ([pal (in-list pals)]
                 [i (in-naturals)])
        (define pi (palette-idx csd pal))
        (define c (+ base-c (* i cols-per-pal)))

        (unless (< (fl+ (calc-cx (+ cols-per-pal c))
                        S)
                   W.0)
          (set! base-c (* -1 c))
          (set! c 0)
          (set! r (+ r 8)))

        (for/list ([d (in-list ds)])
          (match-define (list dr dc p) d)
          (define sp (sprite-idx csd p))
          (define cx (calc-cx (+ dc c)))
          (define cy (calc-cy (+ dr r)))
          (sprite cx cy sp #:mx S #:my S
                  #:layer 2
                  #:pal-idx pi))))
    (define st (list cb pst))
    (apse-inst 0.0 (λ () st))))

(define (apse-tile spr
                   #:scale [scale 1.0]
                   #:pal pal)
  (λ (csd W H)
    (define W.0 (fx->fl W))
    (define H.0 (fx->fl H))
    (define idx (sprite-idx csd spr))
    (define w.0 (fl* scale (fx->fl (sprite-width csd idx))))
    (define h.0 (fl* scale (fx->fl (sprite-height csd idx))))
    (define pi (palette-idx csd pal))
    (define gst
      (for*/list ([r (in-range (inexact->exact (floor (fl/ H.0 h.0))))]
                  [c (in-range (inexact->exact (floor (fl/ W.0 w.0))))])
        (define cx (fl* (fl+ (fx->fl c) 0.5) w.0))
        (define cy (fl* (fl+ (fx->fl r) 0.5) h.0))
        (sprite cx cy idx
                #:layer 2
                #:mx scale #:my scale
                #:pal-idx pi)))
    (define st (list gst))
    (apse-inst 0.0 (λ () st))))

(define-syntax-rule (with-apse-params [sd W H] . body)
  (begin
    (define apse (-apse sd W H (let () . body)))
    (provide apse)))

(provide define-sprite
         (struct-out animation)
         with-apse-params
         apse-sprite
         apse-all-sprites
         apse-animation
         apse-palettes
         apse-tile)
(provide
 (contract-out
  [apse-palette
   (-> color? color? color?
       color? color? color? color?
       (listof color?))]
  [initialize-apse!
   (-> sprite-db?
       void?)]))
