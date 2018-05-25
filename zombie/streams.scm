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

;;; Comments:
;;
;; - 2018/02/21: imported from guile-wiredtiger grf3 library, changed
;; the name to 'streams'
;;
;; - 2018/02/25: replace the use of 'throw' and 'cons' with 'values'
;; because it is faster
;;
;; - 2018/05/19: imported in zombie as-is
;;
(library (zombie streams)
  (export list->stream
          stream->list
          stream-null
          stream-null?
          stream-car
          stream-cdr
          stream-map
          stream-for-each
          stream-filter
          stream-append
          stream-length
          stream-group
          stream-sort)
  (import
    (rnrs)
    (only (chezscheme) reverse!)
    (only (chezscheme) sort!))

  (define (list->stream lst)
    (let loop ((lst lst))
      (lambda ()
        (if (null? lst)
            (values #f #f)
            (values (car lst) (loop (cdr lst)))))))

  (define (stream->list stream)
    (let loop ((stream stream)
               (out '()))
      (call-with-values stream
        (lambda (value next)
          (if next
              (loop next (cons value out))
              (reverse! out))))))

  (define stream-null
    (lambda ()
      (values #f #f)))

  (define (stream-null? stream)
    (call-with-values stream
      (lambda (value next)
        (eq? next #f))))

  (define (stream-car stream)
    (call-with-values stream
      (lambda (value next)
        value)))

  (define (stream-cdr stream)
    (call-with-values stream
      (lambda (value next)
        next)))

  (define (stream-map proc stream)
    (let loop ((stream stream))
      (lambda ()
        (call-with-values stream
          (lambda (value next)
            (if next
                (values (proc value) (loop next))
                (values #f #f)))))))

  (define (stream-for-each proc stream)
    (let loop ((stream stream))
      (call-with-values stream
        (lambda (value next)
          (when next
            (proc value)
            (loop next))))))

  (define (stream-filter predicate? stream)
    (let loop1 ((stream stream))
      (lambda ()
        (let loop2 ((stream stream))
          (call-with-values stream
            (lambda (value next)
              (if next
                  (if (predicate? value)
                      (values value (loop1 next))
                      (loop2 next))
                  (values #f #f))))))))

  (define (stream-append . streams)
    (cond
     ((null? streams) (lambda () (values #f #f)))
     ;; wanna be fast path for the common case, if there is single
     ;; stream, return the first stream
     ((null? (cdr streams)) (car streams))
     ;; otherwise, unroll each stream...
     (else (let loop1 ((streams streams))
             (if (null? streams)
                 (lambda () (values #f #f))
                 (let loop2 ((stream (car streams)))
                   (call-with-values stream
                     (lambda (value next)
                       (if next
                           (lambda () (values value (loop2 next)))
                           (loop1 (cdr streams)))))))))))


  ;; (define (stream-take count stream)
  ;;   (let loop ((stream stream)
  ;;              (count count))
  ;;     (lambda ()
  ;;       (if (eq? count 0)
  ;;           '()
  ;;           (match (stream)
  ;;             ('() '())
  ;;             ((item . next) (cons item (loop next (1- count)))))))))

  ;; (define (stream-drop count stream)
  ;;   (let loop ((stream stream)
  ;;              (count count))
  ;;     (lambda ()
  ;;       (match (stream)
  ;;         ('() '())
  ;;         ((item . next) (if (eq? count 0)
  ;;                            (cons item (loop next 0))
  ;;                            ((loop next (1- count)))))))))


  ;; (define (stream-paginator count stream)
  ;;   (throw 'stream "not implemented error"))

  (define (stream-length stream)
    (let loop ((stream stream)
               (count 0))
      (call-with-values stream
        (lambda (value next)
          (if next
              (loop next (+ 1 count))
              count)))))

  ;; (define (stream-scatter stream)
  ;;   "Take a stream of lists and returns a stream made of all the
  ;;    elements of all the lists. parents are inherited."
  ;;   (let loop ((stream stream)
  ;;              (lst '())
  ;;              (parents '()))
  ;;     (lambda ()
  ;;       (if (null? lst)
  ;;           (match (stream)
  ;;             ('() '())
  ;;             ((item . next)
  ;;              (let ((lst (car item))
  ;;                    (parents (cdr item)))
  ;;                (if (null? lst)
  ;;                    ((loop next '() '()))
  ;;                    (cons (cons (car lst) parents)
  ;;                          (loop next (cdr lst) parents))))))
  ;;           (cons (cons (car lst) parents)
  ;;                 (loop stream (cdr lst) parents))))))

  ;; (define (stream-unique stream)
  ;;   (let ((seen '()))  ;; TODO: replace with a hash table
  ;;     (let loop1 ((stream stream))
  ;;       (lambda ()
  ;;         (let loop2 ((stream stream))
  ;;           (match (stream)
  ;;             ('() '())
  ;;             ((item . next) (if (list-index (cut equal? <> (car item)) seen)
  ;;                                (loop2 next)
  ;;                                (begin (set! seen (cons (car item) seen))
  ;;                                       (cons item (loop1 next)))))))))))

  (define (stream-group predicate? proc stream)
    "Return a new stream of stream values from STREAM. STREAM must be sorted.
Values from STREAM are grouped according to PROC. The value returned by
PROC must be comparable with PREDICATE?."
    (let* ((%stream-next
            (lambda (stream key)
              ;; TODO: maybe memoize that procedure, because if the underlying
              ;; stream is a cursor stream (see cursor->stream) it leads to
              ;; multiple cursor-key-set + cursor-search which can be expensive.

              ;; XXX: This only called in the case where the previous stream was
              ;; not fully consumed ie. next-group-callback is replaced in most
              ;; cases by a lambda returning a value without computation, see
              ;; %stream-group procedure.
              (let loop ((stream stream))
                (call-with-values stream
                  (lambda (value next)
                    (if next
                        (if (predicate? (proc value) key)
                            (loop next)
                            (lambda () (values value next))) ;; next-group
                        (lambda () (values #f #f)))))))) ;; end of stream
           (%stream-group
             (lambda (stream key)
               ;; worst case scenario, stream was not consumed, but the user
               ;; request the next group

               ;; TODO: use make-paramater instead of set!
               (let ((next-group-callback (lambda () (%stream-next stream key))))
                 (values (let loop ((stream stream))
                           (lambda ()
                             (call-with-values stream
                               (lambda (value next)
                                 (if next
                                     (if (predicate? (proc value) key)
                                         ;; save advance stream
                                         (begin (set! next-group-callback (lambda () (%stream-next next key)))
                                                (values value (loop next)))
                                         (and (set! next-group-callback (lambda () stream)) ;; next group
                                              (values #f #f))) ;; end of group stream
                                     (and (set! next-group-callback (lambda () (lambda () (values #f #f)))) ;; end of stream
                                          (values #f #f))))))) ;; end of group
                         (lambda () (next-group-callback)))))))

      (let loop ((stream (lambda () stream)))
        (lambda ()
          ;; the whole thing must appear pure, but depending on whether a
          ;; group is consumed, the next stream code path changes. That's
          ;; why, the loop's STREAM is wrapped in lambda as a thunk, to
          ;; allow 'next-group-stream' callback returned by %stream-group,
          ;; to return the correct/current next-group-callback depending
          ;; on whether the next group stream was computed or not and
          ;; compute it if it wasn't computed. TBH I am not sure this is
          ;; the right level of lambda nesting. It seems like there is too
          ;; much callback.
          (call-with-values (stream)
            (lambda (value next)
              (if next
                  (call-with-values (lambda () (%stream-group (stream) (proc value)))
                    (lambda (group next-group-stream)
                      (values group (loop next-group-stream))))
                  (values #f #f))))))))


  ;; (define (hash-increment ht key)
  ;;   (let ((value (hash-ref ht key)))
  ;;     (if (not value)
  ;;         (hash-set! ht key 1)
  ;;         (hash-set! ht key (1+ value)))))

  ;; (define (stream-group-count stream)
  ;;   (let ((groups (make-hash-table)))
  ;;     (let loop ((stream stream))
  ;;       (match (stream)
  ;;         ('() (sort (hash-map->list cons groups) (lambda (a b) (> (cdr a) (cdr b)))))
  ;;         ((item . next)
  ;;          (hash-increment groups (car item))
  ;;          (loop next))))))

  (define (stream-sort stream less?)
    (list->stream (sort! (stream->list stream) less?))) ;; TODO: improve preformance with a binary tree
  )
