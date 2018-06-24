#!chezscheme
(import (zombie html))
(import (zombie zread))

(define (pk . args)
  (display args)(newline)
  (car (reverse args)))

(define (template body)
  `((doctype html)
    (html
     (head
      (meta (@ (charset "utf8")))
      (meta (@ (name "viewport")
               (content "width=device-width, initial-scale=1, shrink-to-fit=no")))
      (link (@ (rel "stylesheet")
               (href "static/normalize.css")))
      (link (@ (rel "stylesheet")
               (href "static/main.css")))
      (title "Zombie Blog Generator"))
     (body ,body))))


(define (template/index index)
  `((div (@ (id "hero"))
         (h1 (@ (id "title")) "Zombie Blog Generator")
         (div (@ (id "halloffame"))
              (img (@ (src "static/zombie-male.png")))
              (img (@ (src "static/zombie-female.png")))
              (img (@ (src "static/zombie-boarder.png")))
              (img (@ (src "static/zombie-intern.png")))
              (img (@ (src "static/zombie-windy.png")))))
    (div (@ (id "container"))
         ,index)))


(define (read-string filepath)
  (list->string (call-with-input-file filepath
                  (lambda (port)
                    (let loop ((out '()))
                      (let ((in (read-char port)))
                        (if (eof-object? in)
                            (reverse out)
                            (loop (cons in out)))))))))

(sxml->html (template
             (template/index
              (pk 'out (zread-string
                        (read-string "index.scm")))))
            (current-output-port))
