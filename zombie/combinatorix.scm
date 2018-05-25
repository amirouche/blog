;;; Copyright © 2018 Amirouche Boubekki <amirouche@hypermove.net>
;;;
;;; This module is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation; either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This module is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this module.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Parser combinators.
;;
;; TODO:
;;
;;  - improve error handling
;;
;; Also see:
;;
;; - https://epsil.github.io/gll/
;; - https://docs.racket-lang.org/parsack/index.html
;; - https://docs.racket-lang.org/megaparsack/
;; - https://git.dthompson.us/guile-parser-combinators.git
;; - https://gitlab.com/tampe/stis-parser
;;
;;; Code:
(library (zombie combinatorix)
  (export lift
          parse
          parse-xchar
          either
          each
          zero-or-more
          one-or-more
          otherwise
          any
          parse-char-set
          xchar-char)
  (import
    (rnrs)
    (only (chezscheme) reverse!)
    (rename (srfi s9 records) (define-record-type define-record-type*))
    (srfi s14 char-sets)
    (matchable)
    (zombie streams))

  (define-record-type* <result>
    (make-result value stream)
    result?
    (value result-value)
    (stream result-stream))

  (define-record-type* <failure>
    (make-failure value parser args)
    failure?
    (value failure-value)
    (parser failure-parser)
    (args failure-args))

  (define continue make-result)
  (define (fail stream parser args)
    (make-failure (stream-car stream) parser args))

  (define-record-type* <xchar>
    (make-xchar char line column offset)
    xchar?
    (char xchar-char)
    (line xchar-line)
    (column xchar-column)
    (offset xchar-offset))

  (define (lift proc parser)
    "Apply PROC to the result of PARSER"
    (lambda (stream)
      (match (parser stream)
        (($ <result> value stream) (continue (proc value) stream))
        (else else))))

  ;; (define (xchar-format xchar port)
  ;;   (format port "<xchar ~s [~a,~a] @ ~a>"
  ;;           (xchar-char xchar)
  ;;           (xchar-line xchar)
  ;;           (xchar-column xchar)
  ;;           (xchar-offset xchar)))

  (define (string->xchar-stream string)
    ;; TODO: optimize
    (let loop ((chars (string->list string))
               (line 1)
               (column 1)
               (offset 0)
               (out '()))
      (if (null? chars)
          (list->stream (reverse! out))
          (if (eq? (car chars) #\newline)
              (loop (cdr chars)
                    (+ 1 line)
                    1
                    (+ 1 offset)
                    (cons (make-xchar #\newline line column offset) out))
              (loop (cdr chars)
                    line
                    (+ 1 column)
                    (+ 1 offset)
                    (cons (make-xchar (car chars) line column offset) out))))))

  (define (parse parser string)
    (match (parser (string->xchar-stream string))
      (($ <result> value (? stream-null? stream)) value)
      (else (error 'combinatorix else))))

  (define (parse-xchar char)
    (lambda (stream)
      (call-with-values stream
        (lambda (value next)
          (if next
              (if (char=? (xchar-char value) char)
                  (continue value next)
                  (fail stream parse-xchar char))
              (fail stream parse-xchar char))))))

  (define (%either . parsers)
    (lambda (stream)
      (let loop ((parsers parsers))
        (if (null? parsers)
            (fail stream %either (map (lambda (x) (x)) parsers))
            (let ((continue (((car parsers)) stream)))
              (if (result? continue)
                  continue
                  (loop (cdr parsers))))))))

  (define-syntax define-syntax-rule
    (syntax-rules ()
      [(define-syntax-rule (keyword args ...) body)
       (define-syntax keyword
         (syntax-rules ()
           [(keyword args ...) body]))]))

  (define-syntax-rule (either parser ...)
    (%either (lambda () parser) ...))

  (define (%each . parsers)
    (lambda (stream)
      (let loop ((parsers parsers)
                 (stream stream)
                 (out '()))
        (if (null? parsers)
            (continue (reverse! out) stream)
            (match (((car parsers)) stream)
              (($ <result> value stream) (loop (cdr parsers) stream (cons value out)))
              (else else))))))

  (define-syntax-rule (each parser ...)
    (%each (lambda () parser) ...))

  (define (zero-or-more parser)
    (lambda (stream)
      (let loop ((stream stream)
                 (out '()))
        (match (parser stream)
          (($ <result> value next)
           (loop next (cons value out)))
          (else (continue (reverse! out) stream))))))

  (define (one-or-more parser)
    (lift (lambda (x) (apply cons x)) (each parser (zero-or-more parser))))

  (define (otherwise predicate parser)
    (lambda (stream)
      (if (failure? (predicate stream))
          (parser stream)
          (fail stream predicate parser))))

  (define (parse-char-set char-set)
    (lambda (stream)
      (call-with-values stream
        (lambda (value next)
          (if next
              (if (char-set-contains? char-set (xchar-char value))
                  (continue value next)
                  (fail stream parse-char-set char-set))
              (fail stream parse-char-set char-set))))))

  (define any
    (lambda (stream)
      (call-with-values stream
        (lambda (value next)
          (if next
              (continue value next)
              (fail stream any '()))))))
  )
