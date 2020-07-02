;;;============================================================================

;;; File: "statprof.scm"

;;; Copyright (c) 2005 by Guillaume Germain, All Rights Reserved.
;;; Copyright (c) 2020 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Statistical profiling library

;; See the README file for license and usage information.

(##include "~~lib/gambit/prim/prim#.scm") ;; map fx+ to ##fx+, etc
(##include "~~lib/_gambit#.scm")          ;; for macro-check-string,
                                          ;; macro-absent-obj, etc

(declare (extended-bindings)) ;; ##fx+ is bound to fixnum addition, etc
(declare (not safe))          ;; claim code has no type errors
(declare (block))             ;; claim no global is assigned

;;;============================================================================

;; Profiling & interrupt handling

(define *buckets* #f)
(define *total* 0)
(define *heartbeat-count* 0)

(define (profile-start!)
  (set! *buckets* (make-table))
  (set! *total* 0)
  (set! *heartbeat-count* 0)
  (##interrupt-vector-set! 2 profile-heartbeat!)) ;; ___INTR_HEARTBEAT

(define (profile-stop!)
  (##interrupt-vector-set! 2 ##thread-heartbeat!) ;; ___INTR_HEARTBEAT
  (profile-add! #f))

(define (identify-continuation cont)
  (let loop ((cont cont))
    (and cont
         (let ((locat (##continuation-locat cont)))
           (if locat
               (let* ((container (##locat-container locat))
                      (file (##container->path container)))
                 (if file
                     (let* ((filepos
                             (##position->filepos (##locat-position locat)))
                            (line
                             (fx+ (##filepos-line filepos) 1)))
                       (vector file line))
                     (loop (##continuation-next! cont))))
               (loop (##continuation-next! cont)))))))

(define (profile-heartbeat!)
  (set! *heartbeat-count* (fx+ 1 *heartbeat-count*))
  (profile-add! #t))

(define (profile-add! heartbeat?)
  (continuation-capture
   (lambda (cont)

     (let ((id (identify-continuation cont)))
       (if id
           (let* ((file
                   (vector-ref id 0))
                  (bucket
                   (or (table-ref *buckets* file #f)
                       (let ((b (make-vector 20000 0))) ;; fixme length limit
                         (table-set! *buckets* file b)
                         b)))
                  (line
                   (vector-ref id 1)))
             (if (fx< line (vector-length bucket))
                 (begin
                   (vector-set! bucket
                                line
                                (fx+ *heartbeat-count*
                                     (vector-ref bucket line)))
                   (set! *total* (fx+ *heartbeat-count* *total*))
                   (set! *heartbeat-count* 0))))))

     (if heartbeat?
         (##thread-heartbeat!))))) ;; to allow thread context switching


;; ----------------------------------------------------------------------------
;; Text formatting

(define (pad-left s l c)
  (let loop ((s (string->list s)))
    (if (< (length s) l)
        (loop (cons c s))
        (list->string s))))


;; ----------------------------------------------------------------------------
;; Palette generation & color formatting

(define (gradient from to step)
  (let ((inc (map (lambda (x) (/ x step))
                  (map - to from))))

    (let loop ((i 0)
               (acc '()))
      (if (= i step)
          (reverse acc)
          (loop (+ i 1)
                (cons (map
                       (lambda (x o)
                         (round (+ x (* i o))))
                       from
                       inc)
                      acc))))))

(define (as-rgb col)
  (apply string-append
         (map
          (lambda (x)
            (pad-left (number->string x 16) 2 #\0))
          col)))

(define palette
  (list->vector
   (cons '(255 255 255)
         (gradient '(127 127 255)
                   '(255 127 127)
                   16))))


;; ----------------------------------------------------------------------------
;; Functions to generate the report

(define (write-profile-report profile-dir)

  (define buckets (table->list *buckets*))

  (define output-dir (path-expand profile-dir))

  (with-exception-catcher
   (lambda (e)
     ;; ignore the exception, it probably means that the directory
     ;; already existed.  If there's another problem it will be
     ;; signaled later.
     #f)
   (lambda ()
     (create-directory (list path: output-dir
                             permissions: #o755))))

    (if (> *total* 0)
        (let ((max-intensity
               (fold max
                     0
                     (map
                      (lambda (data)
                        (fold max 0 (vector->list data)))
                      (map cdr buckets)))))
          (map
           (lambda (bucket)
             (let ((file (car bucket))
                   (data (cdr bucket)))

               (define (get-color n)
                 (let ((i (vector-ref data n)))
                   (if (= i 0)
                       (as-rgb (vector-ref palette 0))
                       (let ((x
                              (if (<= max-intensity 1)
                                  0
                                  (* (/ (log (+ 1. i))
                                        (ceiling (log max-intensity)))
                                     (- (vector-length palette) 1)))))
                         (as-rgb (vector-ref palette
                                             (inexact->exact (ceiling x))))))))

               (with-output-to-file (path-expand (string-append
                                                  (path-strip-directory file)
                                                  ".html")
                                                 output-dir)

                 (lambda ()
                   (let ((lines (call-with-input-file file
                                  (lambda (p) (read-all p read-line)))))
                     (print
                      (sexp->html
                       `(html
                         (body
                          (table
                           cellspacing: 0
                           cellpadding: 0
                           border: 0
                           style: "font-size: 12px;"
                           ,@(map
                              (lambda (line line#)
                                `(tr
                                  (td ,(string-append
                                        (number->string line#)
                                        ": "))
                                  ;; (td
                                  ;;  align: center
                                  ;;  ,(let ((n (vector-ref data line#)))
                                  ;;     (if (= n 0)
                                  ;;         ""
                                  ;;         (string-append "["
                                  ;;                        (number->string n)
                                  ;;                        "/"
                                  ;;                        (number->string *total*)
                                  ;;                        "]"))))

                                  (td
                                   align: center
                                   ,(let ((n (vector-ref data line#)))
                                      (if (= n 0)
                                          ""
                                          (string-append
                                           (number->string
                                            (round% (/ n *total*)))
                                           "% "))))

                                  (td (pre style: ,(string-append
                                                    "background-color:#"
                                                    (get-color line#))
                                           ,line))))
                              lines
                              (iota (length lines) 1))))))))))))

           buckets)))

  (with-output-to-file (path-expand "index.html" output-dir)
    (lambda ()
      (print
       (sexp->html
        `(html
          (body
           ,@(if (= 0 *total*)
                 '("profile is empty")
                 (map (lambda (bucket)
                        (let ((file-path
                               (path-expand
                                (string-append
                                 (path-strip-directory (car bucket))
                                 ".html")
                                output-dir)))
                          `(p (a href: ,file-path ,file-path)
                              " ["
                              ,(round%
                                (/ (fold + 0 (vector->list (cdr bucket)))
                                   *total*))
                              " %]")))
                      buckets)))))))))

(define (round% n)
  (/ (round
      (* 10000 n))
     100.))


;; ----------------------------------------------------------------------------
;; Included file "html.scm"
;; ----------------------------------------------------------------------------

;; html.scm -- A simple html generator for Gambit-C 4.0

;; Written by Guillaume Germain (germaing@iro.umontreal.ca)
;; This code is released in the public domain.


(define (stringify x)
  (with-output-to-string ""
    (lambda ()
      (print x))))

(define (to-escaped-string x)
  (stringify
   (map (lambda (c)
          (case c
            ((#\<) "&lt;")
            ((#\>) "&gt;")
            ((#\&) "&amp;")
            (else c)))
        (string->list
         (stringify x)))))

;; Quick and dirty conversion of s-expressions to html
(define (sexp->html exp)

  ;; write the opening tag
  (define (open-tag exp)
    (cond
     ;; null tag isn't valid
     ((null? exp)
      (error "null tag"))

     ;; a tag must be a list beginning with a symbol
     ((and (pair? exp)
           (symbol? (car exp)))
      (list "<"
            (car exp)
            " "
            (maybe-args (car exp) (cdr exp))))

     (else
      (error "invalid tag" exp))))

  ;; take care of the keywords / arguments
  (define (maybe-args tag exp)

    (cond
     ;; does the rest of the list begins with a keyword
     ((and (pair? exp)
           (keyword? (car exp)))

      ;; does the keyword has an associated value?
      (if (or (null? (cdr exp))
              (keyword? (cadr exp)))
          ;; no, we don't put an associated value
          (list (keyword->string (car exp))
                " "
                (maybe-args tag (cdr exp)))
          ;; yes, we take the next element in the list as the value
          (list (keyword->string (car exp))
                "=\""
                (cadr exp)
                "\" "
                (maybe-args tag (cddr exp)))))

     ;; must just be some content
     (else
      (content tag exp))))

  ;; handle the content of the tag and closing it
  (define (content tag exp)
    (cond
     ;; no content...
     ((null? exp)
      ;;(list "></" tag ">"))           ; close as "<br></br>"
      (list "/>"))                      ; close as "<br/>"

     ;; write the content, handle tags inside
     ((pair? exp)
      (list ">"
            (map (lambda (e)
                   (if (pair? e)
                       (open-tag e)
                       (to-escaped-string e)))
                 exp)
            "</"
            tag
            ">"))

     ;; non-null terminated list?
     (else
      (error "strange content..."))))

  ;; we rely on Gambit's flattening of list when printed with print
  (with-output-to-string (lambda ()
                           (print (open-tag exp)))))

;;;============================================================================
