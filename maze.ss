;{{{ Maze Generator
;{{{ Reusable vector
(define ($make-rvector n)
  (let ([i 0] [vec '#()])
    (lambda (operation)
      (case operation
        [size (lambda (size)
          (let ([old-size (vector-length vec)])
            (if (and (<= size old-size) (< i (1- n)))
              (set! i (1+ i))
              (let ([new-size
                  (if (or (zero? old-size)
                          (<= size old-size))
                    size
                    (let loop ([s old-size])
                      (if (>= s size) s
                        (loop (ceiling (* s 3/2))))))])
                (set! i 0)
                (set! vec (make-vector new-size #f))))))]
        [ref (lambda (index)
          (let ([l (vector-ref vec index)])
            (and l (= i (car l)) (cdr l))))]
        [set! (lambda (index v)
          (vector-set! vec index (cons i v)))]))))
(define $rvector ($make-rvector 1024))
(define $rvector-size ($rvector 'size))
(define $rvector-ref ($rvector 'ref))
(define $rvector-set! ($rvector 'set!))
;}}}
(define ($strip n l)
  ($rvector-size n)
  (let loop ([l l])
    (unless (null? l)
      (let ([old ($rvector-ref (car l))])
        (if old
          (set-cdr! old (cdr l))
          ($rvector-set! (car l) l)))
      (loop (cdr l))))
  l)

(define gen-maze (case-lambda
    [() (gen-maze 49 30)] [(w h)
  (if (or (< w 2) (< h 2)) (values #f #f #f)
    (let* ([nw (* (1- w) h)] [vw (make-vector nw #t)]
           [nh (* w (1- h))] [vh (make-vector nh #t)]
           [size (* w h)] [l (make-vector (+ nw nh))]
           [uf (make-vector size)] [tr (make-vector size)])
      (let loop ([i (1- (vector-length l))])
        (unless (negative? i)
          (vector-set! l i i)
          (loop (1- i))))
      (let loop ([i (1- (vector-length uf))])
        (unless (negative? i)
          (vector-set! uf i (list i))
          (loop (1- i))))
      (let loop ([i (1- (vector-length l))] [j (1- (vector-length uf))])
        (let* ([ind (random (1+ i))] [num (vector-ref l ind)])
          (vector-set! l ind (vector-ref l i))
          (if (< num nw)
            (let-values ([(q r) (div-and-mod num (1- w))])
              (let* ([cl (+ (* w q) r)] [nl (last-pair (vector-ref uf cl))]
                     [cr (1+ cl)] [nr (last-pair (vector-ref uf cr))])
                (unless (eq? nl nr)
                  (if (< (car nl) (car nr))
                    (begin
                      (set-cdr! nr nl)
                      (vector-set! tr (car nr) (cons cr cl)))
                    (begin
                      (set-cdr! nl nr)
                      (vector-set! tr (car nl) (cons cl cr))))
                  (set! j (1- j))
                  (vector-set! vw num #f))))
            (let* ([cu (- num nw)] [nu (last-pair (vector-ref uf cu))]
                   [cd (+ cu w)] [nd (last-pair (vector-ref uf cd))])
              (unless (eq? nu nd)
                (if (< (car nu) (car nd))
                  (begin
                    (set-cdr! nd nu)
                    (vector-set! tr (car nd) (cons cd cu)))
                  (begin
                    (set-cdr! nu nd)
                    (vector-set! tr (car nu) (cons cu cd))))
                (set! j (1- j))
                (vector-set! vh cu #f))))
          (if (positive? j) (loop (1- i) j))))
      (values vw vh
        (let loop ([e 0] [s (1- size)])
          (if (= e s) (list e)
            (let* ([l (vector-ref uf s)] [p (vector-ref tr (car l))])
              ($strip size (append!
                (loop e (cadr l))
                (loop (cadr l) (cdr p))
                (reverse! (loop (car l) (car p)))))))))))]))
;;}}}
;{{{ Maze Wrapper
(define (maze-dim vw vh)
  (let* ([x (vector-length vw)] [y (vector-length vh)]
         [z (1+ (- x y))] [d (sqrt (+ (* z z) (* y 4)))])
    (if (or (not (integer? d)) (odd? (bitwise-xor z d)))
      (error 'maze-dim "invalid maze data"))
    (values (/ (+ d z) 2) (1+ (/ (- d z) 2)))))

(define (wrap-maze vw vh)
  (let-values ([(w h) (maze-dim vw vh)])
    (let ([evw (make-vector (* (1+ w) (+ h 2)) -1)] [nw (1+ w)]
          [evh (make-vector (* (+ w 2) (1+ h)) -1)] [nh (+ w 2)])
      (let loop ([i 1] [j 1] [ind 0])
        (if (>= j w)
          (when (< i h)
            (vector-set! evw (+ (* nw i) j) 1)
            (vector-set! evw (* nw (1+ i)) 1)
            (loop (1+ i) 1 ind))
          (begin
            (vector-set! evw (+ (* nw i) j)
              (if (vector-ref vw ind) 1 0))
            (loop i (1+ j) (1+ ind)))))
      (vector-set! evw nw 0)
      (vector-set! evw (+ (* nw h) w) 0)
      (let loop ([i 0] [j 1] [ind 0])
        (if (> j w)
          (when (< i h)
            (loop (1+ i) 1 ind))
          (if (or (= i 0) (= i h))
            (begin
              (vector-set! evh (+ (* nh i) j) 1)
              (loop i (1+ j) ind))
            (begin
              (vector-set! evh (+ (* nh i) j)
                (if (vector-ref vh ind) 1 0))
              (loop i (1+ j) (1+ ind))))))
      (values w h evw evh))))
;;}}}
;{{{ Maze Printer
(define (show-maze w h evw evh)
  (let ([hh #\x2500] [HH #\x2501] [vv #\x2502] [VV #\x2503] [dr #\x250c]
        [Dr #\x250e] [dl #\x2510] [DL #\x2513] [ur #\x2514] [UR #\x2517]
        [ul #\x2518] [Ul #\x251a] [vr #\x251c] [Vr #\x2520] [vl #\x2524]
        [Vl #\x2528] [dh #\x252c] [dH #\x252f] [uh #\x2534] [uH #\x2537]
        [vh #\x253c] [ll #\x2574] [uu #\x2575] [rr #\x2576] [dd #\x2577]
        [LL #\x2578] [UU #\x2579] [RR #\x257a] [DD #\x257b]
        [nw (1+ w)] [nh (+ w 2)] [esc #f])
    (let-syntax ([eprintf
        (syntax-rules ()
          [(_ arg ...)
           (if (not esc) (printf arg ...)
             (let ([s (with-output-to-string
                        (lambda () (printf arg ...)))])
               (display (string-append
                          (substring s 0 1) "\x1b;[m"
                          (substring s 1 (string-length s))))
               (set! esc #f)))])])
      (letrec ([draw-block (lambda (i j)
                 (case (+ (vector-ref evh (+ (* nh i) j)) (* 3
                       (+ (vector-ref evw (+ (* nw (1+ i)) j)) (* 3
                       (+ (vector-ref evh (+ (* nh i) (1+ j))) (* 3
                          (vector-ref evw (+ (* nw i) j))))))))
                   [(  0) (eprintf "  "         )]
                   [(  1) (eprintf "~c "  ll    )]
                   [(  3) (eprintf "~c "  dd    )]
                   [(  4) (eprintf "~c "  dl    )]
                   [(  9) (eprintf "~c~c" rr hh )]
                   [( 10) (eprintf "~c~c" hh hh )]
                   [( 12) (eprintf "~c~c" dr hh )]
                   [( 13) (eprintf "~c~c" dh hh )]
                   [( 27) (eprintf "~c "  uu    )]
                   [( 28) (eprintf "~c "  ul    )]
                   [( 30) (eprintf "~c "  vv    )]
                   [( 31) (eprintf "~c "  vl    )]
                   [( 36) (eprintf "~c~c" ur hh )]
                   [( 37) (eprintf "~c~c" uh hh )]
                   [( 39) (eprintf "~c~c" vr hh )]
                   [( 40) (eprintf "~c~c" vh hh )]
                   [(-17
                       7) (eprintf "~c~c" HH HH )]
                   [(-32) (eprintf "~c"   DL    )]
                   [(-19) (eprintf "~c~c" RR HH )]
                   [(-14) (eprintf "~c~c" dH HH )]
                   [(-11) (eprintf "~c"   LL    )]
                   [(  2) (eprintf "~c "  DD    )]
                   [( 11) (eprintf "~c~c" Dr hh )]
                   [( 18) (eprintf "~c"   UU    )]
                   [( 19) (eprintf "~c"   Ul    )]
                   [( 21) (eprintf "~c"   VV    )]
                   [( 22) (eprintf "~c"   Vl    )]
                   [( 29) (eprintf "~c "  VV    )]
                   [( 32) (eprintf "~c~c" UR HH )]
                   [( 34) (eprintf "~c~c" uH HH )]
                   [( 38) (eprintf "~c~c" Vr hh )]
                   [else  (eprintf "??")]))]
               [printer (case-lambda
                 [()
                  (let loop ([i 0] [j 0])
                    (if (> j w) (begin (newline) (loop (1+ i) 0))
                      (when (<= i h)
                        (draw-block i j)
                        (loop i (1+ j)))))]
                 [(p)
                  (let ([s (car p)] [t (cdr p)])
                    (let loop ([i 0] [j 0])
                      (if (> j w) (begin (newline) (loop (1+ i) 0))
                        (when (<= i h)
                          (if (or (= i s) (= i (1+ s)))
                            (cond [(= j t) (display "\x1b;[7m")]
                                  [(= j (1+ t)) (set! esc #t)]))
                          (draw-block i j)
                          (loop i (1+ j))))))]
                 [(p lp)
                  (if lp (let ([s (car lp)] [t (cdr lp)])
                    (printf "\x1b;[~d;~dH" (1+ s) (1+ (* t 2)))
                    (draw-block s t) (draw-block s (1+ t))
                    (printf "\x1b;[~d;~dH" (+ s 2) (1+ (* t 2)))
                    (draw-block (1+ s) t) (draw-block (1+ s) (1+ t))))
                  (if p (let ([s (car p)] [t (cdr p)])
                    (printf "\x1b;[~d;~dH\x1b;[7m" (1+ s) (1+ (* t 2)))
                    (draw-block s t) (set! esc #t) (draw-block s (1+ t))
                    (printf "\x1b;[~d;~dH\x1b;[7m" (+ s 2) (1+ (* t 2)))
                    (draw-block (1+ s) t) (set! esc #t) (draw-block (1+ s) (1+ t))))
                  (flush-output-port)])])
        printer))))
;;}}}

;{{{ Macro
(define-syntax with-demo
  (lambda (x)
    (syntax-case x ()
      [(_ winch body ...)
       (if (file-exists? "libdemo.so")
         #'(letrec
               ([demo-init (lambda () ((foreign-procedure "demo_init" () void)))]
                [demo-exit (lambda () ((foreign-procedure "demo_exit" () void)))]
                [iclean (lambda () (keyboard-interrupt-handler kbdi))]
                [isetup (lambda () (keyboard-interrupt-handler kbdi@))]
                [kbdi (keyboard-interrupt-handler)]
                [kbdi@ (lambda () (iclean) (kbdi) (demo-init) (isetup))]
                [callback (and winch (let ([x (foreign-callable winch () void)])
                                       (lock-object x) (foreign-callable-entry-point x)))])
             (load-shared-object "./libdemo.so")
             (if callback ((foreign-procedure "demo_winch_set" (void*) void) callback))
             (isetup) (demo-init) body ... (demo-exit) (iclean)
             (if callback ((foreign-procedure "demo_winch_unset" () void))))
         #'(begin (display "\x1b;[2J") body ...  (void)))])))
;}}}
(random-seed (time-nanosecond (current-time)))
(let-values ([(vw vh path) (gen-maze)])
  (if vw (let-values ([(w h evw evh) (wrap-maze vw vh)])
    (let* ([printer (show-maze w h evw evh)] [second 0.2] [local #t]
           [z (exact (floor second))] [f (exact (floor (* (- second z) 1e9)))]
           [duration (make-time 'time-duration f z)] [redraw #f])
      (if local
        (with-demo (lambda () (set! redraw #t))
          (display "\x1b;[2J\x1b;[H")
          (let loop ([l path] [li #f] [lj #f])
            (when redraw (set! redraw #f)
              (display "\x1b;[2J\x1b;[H")
              (printer) (display "\x1b;[H"))
            (if (null? l) (printer #f (cons li lj))
              (let-values ([(i j) (div-and-mod (car l) w)])
                (if li
                  (printer (cons i j) (cons li lj))
                  (printer (cons i j)))
                (sleep duration) (loop (cdr l) i j))))
          (newline))
        (with-demo #f
          (let loop ([l path])
            (display "\x1b;[2J\x1b;[H")
            (if (null? l) (printer)
              (let-values ([(i j) (div-and-mod (car l) w)])
                (printer (cons i j))
                (sleep duration) (loop (cdr l)))))))))))
