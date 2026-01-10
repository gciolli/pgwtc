(define (usage)
  (display "Usage:

    racket -f ly-scm2csv.scm ID

        Reads cache/${ID}.scm and writes cache/${ID}.csv

"))

(define (msg stop? k)
  (lambda (x . ys)
    (let ((d (lambda (y) (display y (current-error-port)))))
      (d k)
      (d " ")
      (d x)
      (d ": ")
      (map d ys)
      (d "\n")
      (when stop? (exit)))))

(define deb  (msg #f "DEBUG"  ))
(define warn (msg #f "WARNING"))
(define err  (msg #t "ERROR"  ))

; uncomment to disable debug and warning messages
;(define deb (lambda xs #f))
;(define warn (lambda xs #f))

;;
;; Main code
;;

(define (csv . xs)
  (let loop ((l xs)
             (a #f))
    (if (null? l)
        a
        (loop
         (cdr l)
         (format "~a~a"
                 (if a (format "~a," a) "")
                 (or (car l) ""))))))

(define (music-add-missing t e)
  ;; This function sanitizes "e" by ensuring that it has exactly the
  ;; expected tags. First it checks that the existing tags are among
  ;; those that are allowed, and then it sets each allowed tag which
  ;; is not there already to an #f value.
  (let ((valid-tags
         (case t

           [(MultiMeasureRestMusic)
            '(articulations
              duration)]

           [(ContextSpeccedMusic)
            '(context-type
              element)]

           [(SimultaneousMusic)
            '(elements)]

           [(TimeScaledMusic)
            '(denominator
              numerator
              duration
              element)]

           [(LineBreakEvent)
            '(break-permission)]

           [(AdHocMarkEvent)
            '(text)]

           [(EventChord)
            '(line-break-permission
              page-break-permission
              page-marker
              duration
              elements)]

           [(GraceMusic)
            '(element)]

           [(NoteEvent)
            '(force-accidental
              articulations
              cautionary
              duration
              pitch)]

           [(RestEvent
             SkipEvent)
            '(duration)]

           [(BarEvent)
            '(bar-type)]

           [(BarCheck)
            '()]

           [else
            (err 602 "unsupported tag " t)])))
    (let loop ((l e))
      (unless (null? l)
        (if (member (caar l) valid-tags)
            (loop (cdr l))
            (err 606 "tag " t " does not support attribute " (caar l)))))
    (let loop ((l valid-tags)
               (a '()))
      (cond
       ((null? l)
        a)
       ((assoc (car l) e)
        =>
        (lambda (x)
          (loop (cdr l)
                (cons x a))))
       (else
        (loop (cdr l)
              (cons (cons (car l) #f)
                    a)))))))

(define (music-canonical e)
  ;; This function takes a list "e" of the following form:
  ;;
  ;;   (make-music T K1 V1 ... Kn Vn)
  ;;
  ;; where T is a valid tag, each Ki is a valid identifier for that
  ;; tag, and returns the same content in a canonical form.
  ;;
  ;; The canonical form is optimised for the "match" procedure,
  ;; meaning that (1) we return #f if the tag must be skipped, (2) we
  ;; add the missing K entries with V set to #f, and (3) we sort the
  ;; Ks.
  (unless (equal? (car e) 'make-music)
    (err 501 "event starts with " (car e) " instead of make-music"))
  (let ((tag (cadadr e)))
    (case tag

      ;;
      ;; We ignore some events
      ;;

      ((BarCheck
        BarEvent
        LineBreakEvent)
       #f)

      (else
       (let* ((e1
               (let loop ((l (cddr e))
                          (a '()))
                 (if (null? l)
                     a
                     (loop (cddr l)
                           (cons (cons (cadar l) (cadr l)) a)))))

              (e2
               (sort e1
                     (lambda (x y)
                       (string>? (symbol->string (car x))
                                 (symbol->string (car y))))))

              (e3
               (music-add-missing tag e2))

              (e4
               (let loop ((l e3)
                          (a '()))
                 (if (null? l)
                     a
                     (loop (cdr l)
                           (cons (list 'quote (caar l))
                                 (cons (cdar l) a))))))

              (e5
               (cons (car e)
                     (cons (cadr e) e4))))

         e5)))))

(define (parse-lilypond-event e0)
  (let ((e (music-canonical e0)))
    (if e
        (match e

               [`(make-music
                  (quote SkipEvent)
                  (quote duration)         (ly:make-duration ,d ...))
                (cons 'SkipEvent (vector d))]

               [`(make-music
                  (quote RestEvent)
                  (quote duration)         (ly:make-duration ,d ...))
                (cons 'RestEvent (vector d))]

               [`(make-music
                  (quote MultiMeasureRestMusic)
                  (quote articulations)    ,a
                  (quote duration)         (ly:make-duration ,d ...))
                (cons 'MultiMeasureRestMusic (vector d))]

               [`(make-music
                  (quote NoteEvent)
                  (quote force-accidental) ,_
                  (quote articulations)    ,a
                  (quote cautionary)       ,c
                  (quote duration)         (ly:make-duration ,d ...)
                  (quote pitch)            (ly:make-pitch    ,p ...))
                (cons 'NoteEvent (vector d p a c))]

               ;;
               ;; TODO forms
               ;;

               [`(make-music
                  (quote ContextSpeccedMusic)
                  (quote context-type) ,ct
                  (quote element)      ,_)
                (cons 'ContextSpeccedMusic (vector ct 'TODO))]

               [`(make-music
                  (quote SimultaneousMusic)
                  (quote elements) ,_)
                (cons 'SimultaneousMusic (vector 'TODO))]

               [`(make-music
                  (quote EventChord)
                  (quote line-break-permission) ,_
                  (quote page-break-permission) ,_
                  (quote page-marker) ,_
                  (quote duration) ,d
                  (quote elements) ,e)
                (cons 'EventChord (vector d e 'TODO))]

               [`(make-music
                  (quote TimeScaledMusic)
                  (quote denominator) ,n1
                  (quote numerator)   ,n2
                  (quote duration)    ,d
                  (quote element)     ,_)
                (cons 'TimeScaledMusic (vector d n1 n2 'TODO))]

               [`(make-music
                  (quote AdHocMarkEvent)
                  (quote text)      ,t)
                (cons 'AdHocMarkEvent (vector t 'TODO))]

               [`(make-music
                  (quote GraceMusic)
                  (quote element)      ,_)
                (cons 'GraceMusic (vector 'TODO))]

               ;;
               ;; unsupported forms
               ;;

               [_
                (err 140 "unsupported event form " e)
                (cons 'unsupported (vector e))]
               )
        #f)))

(define (vox->events es)
  (let-values (((metadata notes)
                (match es
                  [`(make-music
                     (quote RelativeOctaveMusic)
                     (quote element)
                     (make-music
                      (quote SequentialMusic)
                      (quote elements)
                      (list ,metadata . ,notes)))
                   (values metadata notes)]
                  )))
    (let loop ((l notes)
               (a '()))
      (if (null? l)
          (reverse a)
          (loop (cdr l)
                (let ((e (parse-lilypond-event (car l))))
                  (if e (cons e a) a)))))))

(define (event->csv carmen vox)
  (if vox
      (lambda (e)
        (let* ((v (cdr e))
	       (f (lambda (a b c d)
		    (csv carmen vox (car e) a b c d))))
          (case (car e)

            [(RestEvent SkipEvent)
	     (f (vector-ref v 0) #f #f #f)]

            [(MultiMeasureRestMusic)
             (f (vector-ref v 0) #f #f #f)]

            [(NoteEvent)
             (let ((articulation
		    (match (vector-ref v 2)
			   [`(list
			      (make-music
			       (quote TieEvent)))
			    'tie]
			   [`(list
			      (make-music
			       (quote ArticulationEvent)
			       (quote articulation-type)
			       (quote ,a)))
			    'articulation]
			   [else
			    (vector-ref v 2)])))
               (f (vector-ref v 0) (vector-ref v 1) articulation #f))]

            [(BarCheck)
             (f #f #f #f #f)]

            [(ContextSpeccedMusic SimultaneousMusic TimeScaledMusic
              AdHocMarkEvent GraceMusic ChordEvent)
             (f #f #f #f 'TODO)]

            [else
             (err 410 "unsupported event type " e)
             #f])))
      "carmen,vox,event_type,duration,pitch,articulation,notes"))

(define (parse-ly x)
  (let ((i-f (format "cache/~a.scm" x))
        (o-f (format "cache/~a.csv" x)))
    (with-output-to-file o-f
      (lambda ()
        (let loop
            ((voces (load i-f))
             (csv (list (event->csv #f #f))))
          (if (null? voces)
              (display-lines csv)
              (loop (cdr voces)
                    (append csv
                            (map (event->csv x (caar voces))
                                 (vox->events
                                  (cdar voces))))))))
      #:exists 'replace)))

(define (main argv)
  (cond
   [(= (vector-length argv) 1)
    (parse-ly (vector-ref argv 0))]
   [else
    (usage)]))

(main (current-command-line-arguments))
