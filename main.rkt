#lang racket/base
(require racket/flonum
         racket/list
         racket/fixnum
         racket/contract
         gfx/color
         racket/match
         mode-lambda
         apse/core)

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

(define (add-apse-palette! sd)
  (add-palette! sd 'pal:apse APSE-PALETTE))

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

(define (add-sprite!/rows sd n qrows)
  (define rows (map qrow->row qrows))
  ;; xxx check rows all same width
  (define w (length (first rows)))
  (define h (length rows))
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

;; xxx specify size
;; xxx allow shading vs diffuse separation
(define-syntax-rule (define-sprite sd n row ...)
  (begin
    (define n 'n)
    (add-sprite!/rows sd n '(row ...))))

;; xxx show animations

(define current-sd (make-parameter #f))
(define current-W (make-parameter #f))
(define current-H (make-parameter #f))

;; xxx show the sprite with different sizes and palette options
(define (spr->make-st spr pal)
  (λ (csd W H)
    (define W.0 (fx->fl W))
    (define H.0 (fx->fl H))
    (define cy (fl/ H.0 2.0))
    (define si (sprite-idx csd spr))
    (define w (fx->fl (sprite-width csd si)))
    (define h (sprite-height csd si))
    (define pi (palette-idx csd pal))

    (define-values (_ st)
      (for/fold ([lx 0.0] [st #f])
                ;; xxx compute how many will fit
                ([i (in-range 1 6)])
        (define m (fx->fl i))
        (define mw (fl* m w))
        (define cx (fl+ lx (fl/ mw 2.0)))
        (values (fl+ cx (fl/ mw 2.0))
                (cons (sprite cx cy si
                              #:mx m #:my m
                              #:pal-idx pi)
                      st))))

    st))

(define (apse-sprite spr pal)
  (-apse (current-sd) (current-W) (current-H)
         (spr->make-st spr pal)))

(define-syntax-rule (with-apse-params [sd W H] . body)
  (begin
    (define apse
      (parameterize ([current-sd sd]
                     [current-W W]
                     [current-H H])
        . body))
    (provide apse)))

(provide define-sprite
         with-apse-params)
(provide
 (contract-out
  [apse-sprite
   (-> symbol? symbol?
       any/c)]
  [apse-palette
   (-> color? color? color?
       color? color? color? color?
       (listof color?))]
  [add-apse-palette!
   (-> sprite-db?
       void?)]))
