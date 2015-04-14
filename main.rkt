#lang racket/base
(require racket/flonum
         racket/list
         racket/fixnum
         racket/contract
         gfx/color
         racket/match
         mode-lambda)

(define APSE-PALETTE
  (for/list ([i (in-range PALETTE-DEPTH)])
    (argb 255 255 0 i)))

(match-define
  (list TRANS BLACK
        AShad1 ABase ATint1
        BShad1 BBase BTint1
        CShad1 CBase CTint1
        Hi1 Hi2 Hi3 Hi4
        WHITE)
  APSE-PALETTE)

(define CHAR->COLOR
  (hasheq #\_ TRANS
          #\$ BLACK
          #\q ATint1 #\w BTint1 #\e CTint1
          #\a  ABase #\s  BBase #\d  CBase
          #\z AShad1 #\x BShad1 #\c CShad1
          #\f Hi1 #\g Hi2 #\h Hi3 #\j Hi4
          #\! WHITE))

(define (add-apse-palette! sd)
  (add-palette! sd 'pal:apse APSE-PALETTE))

(struct -apse (sd W H make-st))

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
     (printf "l: ~v\n" (vector sd n pal w h bs))
     (vector n pal w h bs))))

;; xxx specify size
;; xxx allow shading vs diffuse separation
(define-syntax-rule (define-sprite sd n row ...)
  (add-sprite!/rows sd n '(row ...)))

;; xxx show animations

(define current-sd (make-parameter #f))
(define current-W (make-parameter #f))
(define current-H (make-parameter #f))

;; xxx show the sprite with different sizes and palette options
(define (spr->make-st spr pal)
  (λ (csd W H)
    (define W.0 (fx->fl W))
    (define H.0 (fx->fl H))
    (define cx (fl/ W.0 2.0))
    (define cy (fl/ H.0 2.0))
    (define si (sprite-idx csd spr))
    (define pi (palette-idx csd pal))
    (printf "~v\n" (vector spr pal cx cy si pi))
    (sprite cx cy si
            #:mx 10.0 #:my 10.0
            #:pal-idx pi)))

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
  [add-apse-palette!
   (-> sprite-db?
       any/c)]))

(module* tool #f
  (require racket/match
           racket/contract
           mode-lambda
           mode-lambda/backend/gl
           lux
           lux/chaos/gui
           lux/chaos/gui/key)

  (struct *apse (f fe output)
    #:methods gen:word
    ;; xxx add a word-evt for this, studio, puresuri, etc
    [(define (word-fps w)
       0.0)
     (define (word-label s ft)
       (lux-standard-label "APSE" ft))
     (define (word-evt w)
       (*apse-fe w))
     (define (word-output w)
       (*apse-output w))
     (define (word-event w e)
       (cond
         [(eq? e 'file-changed)
          (define f (*apse-f w))
          (define output
            (let ()
              (match-define (-apse sd W H make-st) (load-visuals f))
              (define csd (compile-sprite-db sd))
              (save-csd! csd "csd")
              (define st (make-st csd W H))
              (define render (stage-draw/dc csd W H))
              (render (vector (layer (fx->fl (/ W 2)) (fx->fl (/ H 2)))
                              #f #f #f #f #f #f #f)
                      st
                      '())))
          (define new-fe
            (wrap-fe (filesystem-change-evt f)))
          (struct-copy *apse w
                       [fe new-fe]
                       [output output])]
         [(or (eq? e 'close)
              (and (key-event? e)
                   (eq? 'escape (key-event-code e))))
          #f]
         [else
          w]))
     (define (word-tick w)
       w)])

  (define (wrap-fe e)
    (wrap-evt e (λ _ 'file-changed)))

  (define (load-visuals mp)
    (define ns (make-base-namespace))
    (namespace-attach-module (current-namespace) 'racket/base ns)
    (namespace-attach-module (current-namespace) 'mode-lambda ns)
    (namespace-attach-module (current-namespace) 'apse ns)
    (parameterize ([current-namespace ns])
      (namespace-require `(submod (file ,mp) apse))
      (namespace-variable-value 'apse)))

  (define (apse! f)
    (define obj (*apse f (wrap-fe always-evt) void))
    (call-with-chaos
     (make-gui #:mode gui-mode)
     (λ () (fiat-lux obj))))

  (provide
   (contract-out
    [apse!
     (-> path-string?
         void)])))

(module+ main
  (require (submod ".." tool)
           racket/cmdline)

  (command-line
   #:program "apse"
   #:args (file)
   (apse! file)))
