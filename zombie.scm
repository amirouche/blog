#!chezscheme
(import (zombie html))


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


(define index
  '((div (@ (id "hero"))
         (h1 (@ (id "title")) "Zombie Blog Generator")
         (div (@ (id "halloffame"))
              (img (@ (src "static/zombie-male.png")))
              (img (@ (src "static/zombie-female.png")))
              (img (@ (src "static/zombie-boarder.png")))
              (img (@ (src "static/zombie-intern.png")))
              (img (@ (src "static/zombie-windy.png")))))
    (div (@ (id "container"))
         (h2 "Apocalypse future")
         (p "spam egg foo bar baz lorem ipsum hipster spam egg foo bar
baz lorem ipsum hipster spam egg foo bar baz lorem ipsum hipster spam
egg foo bar baz lorem ipsum hipster spam egg foo bar baz lorem ipsum
hipster spam egg foo bar baz lorem ipsum hipster spam egg foo bar baz
lorem ipsum hipster spam egg foo bar baz lorem ipsum hipster spam egg
foo bar baz lorem ipsum hipster spam egg foo bar baz lorem ipsum
hipster spam egg foo bar baz lorem ipsum hipster spam egg foo bar baz
lorem ipsum hipster spam egg foo bar baz lorem ipsum hipster spam egg
foo bar baz lorem ipsum hipster spam egg foo bar baz lorem ipsum
hipster spam egg foo bar baz lorem ipsum hipster spam egg foo bar baz
lorem ipsum hipster spam egg foo bar baz lorem ipsum hipster spam egg
foo bar baz lorem ipsum hipster spam egg foo bar baz lorem ipsum
hipster spam egg foo bar baz lorem ipsum hipster spam egg foo bar baz
lorem ipsum hipster spam egg foo bar baz lorem ipsum hipster spam egg
foo bar baz lorem ipsum hipster spam egg foo bar baz lorem ipsum
hipster spam egg foo bar baz lorem ipsum hipster spam egg foo bar baz
lorem ipsum hipster spam egg foo bar baz lorem ipsum hipster spam egg
foo bar baz lorem ipsum hipster spam egg foo bar baz lorem ipsum
hipster spam egg foo bar baz lorem ipsum hipster spam egg foo bar baz
lorem ipsum hipster spam egg foo bar baz lorem ipsum hipster spam egg
foo bar baz lorem ipsum hipster spam egg foo bar baz lorem ipsum
hipster spam egg foo bar baz lorem ipsum hipster spam egg foo bar baz")
         (h2 "Antiquote")
         (code (pre "lorem ipsum hipster spam egg foo bar baz lorem ipsum hipster spam egg
foo bar baz lorem ipsum hipster spam egg foo bar baz lorem ipsum
hipster spam egg foo bar baz lorem ipsum hipster spam egg foo bar baz
lorem ipsum hipster spam egg foo bar baz lorem ipsum hipster spam egg
foo bar baz lorem ipsum hipster spam egg foo bar baz lorem ipsum"))
         (h2 "Continuation")
         (p "hipster spam egg foo bar baz lorem ipsum hipster spam egg foo bar baz
lorem ipsum hipster spam egg foo bar baz lorem ipsum hipster spam egg
foo bar baz lorem ipsum hipster spam egg foo bar baz lorem ipsum
hipster spam egg foo bar baz lorem ipsum hipster spam egg foo bar baz
lorem ipsum hipster spam egg foo bar baz lorem ipsum hipster spam egg
foo bar baz lorem ipsum hipster spam egg foo bar baz lorem ipsum
hipster spam egg foo bar baz lorem ipsum hipster spam egg foo bar baz
lorem ipsum hipster spam egg foo bar baz lorem ipsum hipster spam egg
foo bar baz lorem ipsum hipster spam egg foo bar baz lorem ipsum
hipster spam egg foo bar baz lorem ipsum hipster spam egg foo bar baz
lorem ipsum hipster spam egg foo bar baz lorem ipsum hipster spam egg
foo bar baz lorem ipsum hipster spam egg foo bar baz lorem ipsum
hipster spam egg foo bar baz lorem ipsum hipster spam egg foo bar baz
lorem ipsum hipster spam egg foo bar baz lorem ipsum hipster spam egg
foo bar baz lorem ipsum hipster spam egg foo bar baz lorem ipsum
hipster spam egg foo bar baz lorem ipsum hipster spam egg foo bar baz
lorem ipsum hipster spam egg foo bar baz lorem ipsum hipster spam egg
foo bar baz lorem ipsum hipster spam egg foo bar baz lorem ipsum
hipster spam egg foo bar baz lorem ipsum hipster spam egg foo bar baz
lorem ipsum hipster spam egg foo bar baz lorem ipsum hipster spam egg
foo bar baz lorem ipsum hipster spam egg foo bar baz lorem ipsum
hipster ")
         (h2 "The end of the end")
         (p "All. Of. It."))))

(sxml->html (template index) (current-output-port))
