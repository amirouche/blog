#!chezscheme
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2016-2017 Amirouche Boubekki <amirouche@hypermove.net>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; ChangeLog:
;;
;; - 2017-XX-XX: add support for script tags
;;
(library (zombie html)
  (export sxml->html)
  (import
    (rnrs)
    (matchable))

  (define %void-elements ;; TODO: use fast hash table
    '(area
      base
      br
      col
      command
      embed
      hr
      img
      input
      keygen
      link
      meta
      param
      source
      track
      wbr))

  (define (ref alist key)
    (let ((out (assoc key alist)))
      (if out (cdr out) #f)))

  (define (void-element? tag)
    "Return #t if TAG is a void element."
    (pair? (memq tag %void-elements)))

  (define %escape-chars ;; TODO: use fast hash table
    '((#\" . "quot")
      (#\& . "amp")
      (#\' . "apos")
      (#\< . "lt")
      (#\> . "gt")))

  (define (string->escaped-html s port)
    "Write the HTML escaped form of S to PORT."
    (let ((escape (lambda (c)
                    (let ((escaped (ref %escape-chars c)))
                      (if escaped
                          (display (string-append "&" escaped) port)
                          (display c port))))))
      (string-for-each escape s)))

  (define (object->escaped-html scm port)
    "Write the HTML escaped form of SCM to PORT."
    (string->escaped-html
     (call-with-string-output-port
      (lambda (port)
        (write scm port)))
     port))

  (define (attribute-value->html value port)
    "Write the HTML escaped form of VALUE to PORT."
    (if (string? value)
        (string->escaped-html value port)
        (object->escaped-html value port)))

  (define (attribute->html attr value port)
    "Write ATTR and VALUE to PORT."
    (display (string-append (symbol->string attr) "=\"") port)
    (attribute-value->html value port)
    (display #\" port))

  (define (element->html tag attrs body port)
    "Write the HTML TAG to PORT, where TAG has the attributes in the
list ATTRS and the child nodes in BODY."
    (display (string-append "<" (symbol->string tag)) port)
    (for-each (match-lambda
                ((attr value)
                 (display #\space port)
                 (attribute->html attr value port)))
              attrs)
    (cond
     ((and (null? body) (void-element? tag)) (display " />" port))
     ((eqv? tag 'script) (display #\> port) (unless (null? body) (display (car body) port)) (display "</script>" port))
     (else (begin
             (display #\> port)
             (for-each (lambda (x) (sxml->html x port)) body)
             (display (string-append "</" (symbol->string tag) ">") port)))))

  (define (doctype->html doctype port)
    (display (string-append "<!DOCTYPE " (symbol->string doctype) ">") port))

  (define (sxml->html tree port)
    "Write the serialized HTML form of TREE to PORT."
    (match tree
      (() '())
      (('doctype type)
       (doctype->html type port))
      (((? symbol? tag) ('@ attrs ...) body ...)
       (element->html tag attrs body port))
      (((? symbol? tag) body ...)
       (element->html tag '() body port))
      ((nodes ...)
       (for-each (lambda (x) (sxml->html x port)) nodes))
      ((? string? text)
       (string->escaped-html text port))
      ;; Render arbitrary Scheme objects, too.
      (obj (object->escaped-html obj port))))
  )
