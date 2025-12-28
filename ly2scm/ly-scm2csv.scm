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

; uncomment to disable debug
(define deb (lambda xs #f))

(define (parse-lilypond-event e)
  (deb 100 e)
  (match e
    [`(make-music
       (quote BarCheck))
     (deb 110)
     (cons 'BarCheck (vector))]

    ;;
    ;; Rests
    ;;

    [`(make-music
       (quote RestEvent)
       (quote duration)
       (ly:make-duration ,d))
     (deb 120)
     (cons 'RestEvent (vector d))]

    [`(make-music
       (quote MultiMeasureRestMusic)
       (quote duration)
       (ly:make-duration ,n ,d)
       (quote articulations)
       (quote ,a))
     (deb 125)
     (cons 'MultiMeasureRestMusic (vector n d))]

    ;;
    ;; various forms of NoteEvent
    ;;

    [`(make-music
       (quote NoteEvent)
       (quote duration)
       (ly:make-duration . ,d)
       (quote pitch)
       (ly:make-pitch . ,p))
     (deb 130)
     (cons 'NoteEvent (vector d p #f))]

    [`(make-music
       (quote NoteEvent)
       (quote pitch)
       (ly:make-pitch . ,p)
       (quote duration)
       (ly:make-duration . ,d))
     (deb 131)
     (cons 'NoteEvent (vector d p #f))]

    [`(make-music
       (quote NoteEvent)
       (quote force-accidental) ,_
       (quote duration)
       (ly:make-duration . ,d)
       (quote pitch)
       (ly:make-pitch . ,p))
     (deb 132)
     (cons 'NoteEvent (vector d p #f))]

    [`(make-music
       (quote NoteEvent)
       (quote articulations) ,a
       (quote duration)
       (ly:make-duration . ,d)
       (quote pitch)
       (ly:make-pitch . ,p))
     (deb 133)
     (cons 'NoteEvent (vector d p a))]

    [`(make-music
       (quote NoteEvent)
       (quote articulations) ,a
       (quote force-accidental) ,_
       (quote duration)
       (ly:make-duration . ,d)
       (quote pitch)
       (ly:make-pitch . ,p))
     (deb 134)
     (cons 'NoteEvent (vector d p a))]

    ;;
    ;; unsupported forms
    ;;

    [_
     (deb 140)
     (cons 'unsupported (vector e))]))

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
		  ;;[_ #f]
		  )))
    (let loop ((l notes)
	       (a '()))
      (deb 200 (length l))
      (if (null? l)
	  (reverse a)
	  (loop (cdr l)
		(cons (parse-lilypond-event (car l))
		      a))))))

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

(define (event->csv vox)
  (if vox
      (lambda (e)
	(let ((v (cdr e)))
	  (case (car e)
	    [(RestEvent)
	     (csv vox (car e) (vector-ref v 0) #f #f #f)]
	    [(MultiMeasureRestMusic)
	     (csv vox (car e) (vector-ref v 1) (vector-ref v 0) #f #f)]
	    [(NoteEvent)
	     (let ((articulation
		    (case (vector-ref v 2)
		      ['(list (make-music (quote TieEvent)))
		       'tie]
		      [else #f])))
	       (csv vox (car e) (vector-ref v 0) #f (vector-ref v 1) articulation))]
	    [(BarCheck)
	     (csv vox (car e) #f #f #f #f)]
	    [else
	     (warn 410 e)
	     #f])))
      "vox,event_type,duration,multi,pitch,articulation"))

(define (parse-ly x)
  (let ((i-f (format "cache/~a.scm" x))
	(o-f (format "cache/~a.csv" x)))
    (with-output-to-file o-f
      (lambda ()
	(let loop
	    ((voces (hash->list (load i-f)))
	     (csv (list (event->csv #f))))
	  (if (null? voces)
	      (display-lines csv)
	      (loop (cdr voces)
		    (append csv
			    (map (event->csv (caar voces))
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
