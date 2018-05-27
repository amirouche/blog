(library (zombie zread)
  (export
   zread-string
   parse-skribe)
  (import
    (rnrs)
    (only (scheme) reverse!)
    (srfi s14 char-sets)
    (matchable)
    (zombie combinatorix))

  (define (const value)
    (lambda args
      value))

  (define (compose . procs)
    (let ((procs (reverse! procs)))
      (lambda args
        (let loop ((procs (cdr procs))
                   (out (apply (car procs) args)))
          (if (null? procs)
              out
              (loop (cdr procs) ((car procs) out)))))))

  (define char-set:lisp-delimiters
    (char-set-union char-set:whitespace
                    (char-set #\( #\) #\[ #\] #\{ #\})))

  (define char-set:number-digits (char-set #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

  (define char-set:lisp-symbol
    (char-set-complement char-set:lisp-delimiters))

  (define %space (make-vector 1 'ws))

  (define (not-space? v)
    (not (eq? v %space)))

  (define parse-whitespace (lift (const %space)
                                 (one-or-more (parse-char-set char-set:whitespace))))

  (define (xchars->string xchars)
    (list->string (map xchar-char xchars)))

  (define parse-string (lift (match-lambda ((dq1 xchars dq2) (xchars->string xchars)))
                             (each (parse-xchar #\")
                                   (zero-or-more (either (lift (lambda (x) (cadr x))
                                                               (each (parse-xchar #\\) (parse-xchar #\")))
                                                         (otherwise (parse-xchar #\") any)))
                                   (parse-xchar #\"))))

  (define parse-boolean (either (lift (const #f) (each (parse-xchar #\#) (parse-xchar #\f)))
                                (lift (const #t) (each (parse-xchar #\#) (parse-xchar #\t)))))

  (define parse-symbol (lift
                        (compose string->symbol xchars->string)
                        (one-or-more (parse-char-set char-set:lisp-symbol))))


  (define parse-rational (lift (match-lambda ((a b c) (string->number
                                                       (string-append (xchars->string a)
                                                                      "/"
                                                                      (xchars->string c)))))
                               (each (one-or-more (parse-char-set char-set:number-digits))
                                     (parse-xchar #\/)
                                     (one-or-more (parse-char-set char-set:number-digits)))))

  (define parse-float
    (lift
     (lambda (x)
       (string->number
        (string-append (list->string (map xchar-char (list-ref x 0)))
                       "."
                       (list->string (map xchar-char (list-ref x 2))))))
     (each (one-or-more (parse-char-set char-set:number-digits))
           (parse-xchar #\.)
           (one-or-more (parse-char-set char-set:number-digits)))))

  (define parse-number (lift (compose string->number xchars->string)
                             (one-or-more (parse-char-set char-set:number-digits))))


  (define parse-open-paren (lift (const #f) (parse-xchar #\()))
  (define parse-close-paren (lift (const #f) (parse-xchar #\))))

  (define parse-open-square (lift (const #f) (parse-xchar #\[)))
  (define parse-close-square (lift (const #f) (parse-xchar #\])))

  (define parse-escape-skribe (each (parse-xchar #\,)
                                    (parse-xchar #\()))


  (define (skribe list)
    (error 'not-implemented))

  (define parse-skribe (lift (compose skribe cadr)
                             (each parse-open-square
                                   (zero-or-more
                                    (either
                                     ;; parse s-exp
                                     (lift cadr
                                           (each (parse-xchar #\,)
                                                 parse-exp))
                                     ;; something else
                                     (otherwise (parse-xchar #\])
                                                any)))
                                   parse-close-square)))

  (define parse-exp (lift cadr
                          (each parse-open-paren
                                (lift (lambda (x) (filter not-space? x))
                                      (zero-or-more (either parse-exp
                                                            parse-skribe
                                                            parse-boolean
                                                            parse-rational
                                                            parse-float
                                                            parse-number
                                                            parse-string
                                                            parse-symbol
                                                            parse-whitespace)))
                                parse-close-paren)))

  (define (zread-string string)
    (parse parse-exp string))
)
