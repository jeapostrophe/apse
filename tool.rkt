#lang racket/base
(require racket/match
         racket/contract
         racket/fixnum
         mode-lambda
         mode-lambda/backend/gl
         apse/core
         lux
         lux/chaos/gui
         lux/chaos/gui/key)

(struct *apse (f fe output)
  #:methods gen:word
  [(define (word-fps w)
     (apse-inst-fps (*apse-output w)))
   (define (word-label s ft)
     (lux-standard-label "APSE" ft))
   (define (word-evt w)
     (*apse-fe w))
   (define (word-output w)
     ((apse-inst-draw (*apse-output w))))
   (define (word-event w e)
     (cond
       ;; xxx follow this pattern for studio and puresuri
       [(eq? e 'file-changed)
        (define f (*apse-f w))
        (define new-output
          (with-handlers ([exn:fail? (λ (x)
                                       ((error-display-handler) (exn-message x) x)
                                       #f)])
            (match-define (-apse sd W H make-apse-inst) (load-visuals f))
            (define csd (compile-sprite-db sd))
            (save-csd! csd "csd")
            (define render (stage-draw/dc csd W H))
            (define ai (make-apse-inst csd W H))
            (define the-layer (layer (fx->fl (/ W 2)) (fx->fl (/ H 2))))
            (define layer-config (make-vector LAYERS the-layer))
            (struct-copy apse-inst ai
                         [draw
                          (λ ()
                            (render layer-config
                                    ((apse-inst-draw ai))
                                    '()))])))
        (define new-fe
          (wrap-fe (filesystem-change-evt f)))
        (struct-copy *apse w
                     [fe new-fe]
                     [output
                      (or new-output
                          (*apse-output w))])]
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
  (namespace-attach-module (current-namespace) 'apse/core ns)
  (parameterize ([current-namespace ns])
    (namespace-require `(submod (file ,mp) apse))
    (namespace-variable-value 'apse)))

(define (apse! f)
  (define obj (*apse f (wrap-fe always-evt)
                     (apse-inst 0.0 (λ () void))))
  (call-with-chaos
   (make-gui #:mode gui-mode)
   (λ () (fiat-lux obj))))

(module+ main
  (require racket/cmdline)

  ;; (gl-screenshot-dir (build-path (current-directory) "shots"))
  (gl-filter-mode 'std)

  (command-line
   #:program "apse"
   #:args (file)
   (apse! file)))
