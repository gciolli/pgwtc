(define (usage)
  (display "Usage:

    racket -f ly-scm2csv.scm ID

        Reads cache/${ID}.scm and writes cache/${ID}.csv

"))

(define (ly->csv x)
  "TODO")

(define (parse-ly x)
  (let ((i-f (format "cache/~a.scm" x))
	(o-f (format "cache/~a.csv" x)))
    (with-input-from-file i-f
      (lambda ()
	(let ((y (read)))
	  (with-output-to-file o-f
	    (lambda ()
	      (display (ly->csv y))
	      (newline))
	    #:exists 'replace))))))

(define (main argv)
  (cond
   [(= (vector-length argv) 1)
    (parse-ly (vector-ref argv 0))]
   [else
    (usage)]))

(main (current-command-line-arguments))
