
;;; Teach Yourself Scheme in Fixnum Days, Dorai Sitaram
;;;   Chapter 14. Nondeterminism
;;;     https://ds26gte.github.io/tyscheme/

;;; R5RS version

;;; guile ........ ok
;;; mit-scheme ... ok
;;; scm .......... error
;;; csi .......... ??

(define amb-fail '*)

(define initialize-amb-fail
  (lambda ()
    (set! amb-fail
      (lambda ()
        (error "amb tree exhausted")))))

(initialize-amb-fail)

(define-syntax amb
  (syntax-rules ()
    ((amb alt ...)
     (let ((+prev-amb-fail amb-fail))
       (call/cc
	  (lambda (+sk)
	    (call/cc
	      (lambda (+fk)
		(set! amb-fail
		  (lambda ()
		    (set! amb-fail +prev-amb-fail)
		    (+fk 'fail)))
		(+sk alt)))
	    ...
	    (+prev-amb-fail)))))))

(define number-between
  (lambda (lo hi)
    (let loop ((i lo))
      (if (> i hi) (amb)
	(amb i (loop (+ i 1)))))))

(define assert
  (lambda (pred)
    (if (not pred) (amb))))

(define gen-prime
  (lambda (hi)
    (let ((i (number-between 2 hi)))
      (assert (prime? i))
      i)))

(define-syntax bag-of
  (syntax-rules ()
    ((bag-of e)
     (let ((+prev-amb-fail amb-fail)
	   (+results '()))
       (if (call/cc
	     (lambda (+k)
	       (set! amb-fail (lambda () (+k #f)))
	       (let ((+v e))
		 (set! +results (cons +v +results))
		 (+k #t))))
	 (amb-fail))
     (set! amb-fail +prev-amb-fail)
     (reverse! +results)))))

(define solve-kalotan-puzzle
  (lambda ()
    (let ((parent1 (amb 'm 'f))
	  (parent2 (amb 'm 'f))
	  (kibi (amb 'm 'f))
	  (kibi-self-desc (amb 'm 'f))
	  (kibi-lied? (amb #t #f)))
      (assert
	(distinct? (list parent1 parent2)))
      (assert
	(if (eqv? kibi 'm)
	  (not kibi-lied?)))
      (assert
	(if kibi-lied?
	  (xor
	    (and (eqv? kibi-self-desc 'm)
		 (eqv? kibi 'f))
	    (and (eqv? kibi-self-desc 'f)
		 (eqv? kibi 'm)))))
      (assert
	(if (not kibi-lied?)
	  (xor
	    (and (eqv? kibi-self-desc 'm)
		 (eqv? kibi 'm))
	    (and (eqv? kibi-self-desc 'f)
		 (eqv? kibi 'f)))))
      (assert
	(if (eqv? parent1 'm)
	  (and
	    (eqv? kibi-self-desc 'm)
	    (xor
	      (and (eqv? kibi 'f)
		   (eqv? kibi-lied? #f))
	      (and (eqv? kibi 'm)
		   (eqv? kibi-lied? #t))))))
      (assert
	(if (eqv? parent1 'f)
	  (and
	    (eqv? kibi 'f)
	    (eqv? kibi-lied? #t))))
      (list parent1 parent2 kibi))))

(define choose-color
  (lambda ()
    (amb 'red 'yellow 'blue 'white)))

(define color-europe
  (lambda ()

    ;choose colors for each country
    (let ((p (choose-color)) ;Portugal
	  (e (choose-color)) ;Spain
	  (f (choose-color)) ;France
	  (b (choose-color)) ;Belgium
	  (h (choose-color)) ;Holland
	  (g (choose-color)) ;Germany
	  (l (choose-color)) ;Luxemb
	  (i (choose-color)) ;Italy
	  (s (choose-color)) ;Switz
	  (a (choose-color)) ;Austria
	  )

      ;construct the adjacency list for
      ;each country: the 1st element is
      ;the name of the country; the 2nd
      ;element is its color; the 3rd
      ;element is the list of its
      ;neighbors' colors
      (let ((portugal
	      (list 'portugal p
		    (list e)))
	    (spain
	      (list 'spain e
		    (list f p)))
	    (france
	      (list 'france f
		    (list e i s b g l)))
	    (belgium
	      (list 'belgium b
		    (list f h l g)))
	    (holland
	      (list 'holland h
		    (list b g)))
	    (germany
	      (list 'germany g
		    (list f a s h b l)))
	    (luxembourg
	      (list 'luxembourg l
		    (list f b g)))
	    (italy
	      (list 'italy i
		    (list f a s)))
	    (switzerland
	      (list 'switzerland s
		    (list f i a g)))
	    (austria
	      (list 'austria a
		    (list i s g))))
	(let ((countries
		(list portugal spain
		      france belgium
		      holland germany
		      luxembourg
		      italy switzerland
		      austria)))

	  ;the color of a country
	  ;should not be the color of
	  ;any of its neighbors
	  (for-each
	    (lambda (c)
	      (assert
		(not (memq (cadr c)
			   (caddr c)))))
	    countries)

	  ;output the color
	  ;assignment
	  (for-each
	    (lambda (c)
	      (display (car c))
	      (display " ")
	      (display (cadr c))
	      (newline))
	    countries))))))

;;; missing procedures

(define prime? ; primes < 100 and anything >= 100
  (lambda (n)
    (case n
      ((2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)
       #t)
      (else (if (>= n 100) #t #f)))))

(define distinct?
  (lambda (args)
    (letrec ((distinct-from?
	       (lambda (obj lst)
	         (cond ((null? lst) #t)
		       ((eqv? obj (car lst)) #f)
		       (else (distinct-from? obj (cdr lst)))))))
      (cond ((null? args) #t)
	    ((not (distinct-from? (car args) (cdr args))) #f)
	    (else (distinct? (cdr args)))))))

(define distinct?
  (lambda (list-pair)
    (not (eqv? (car list-pair) (cadr list-pair)))))

(define xor
  (lambda (a b)
    (or (and a (not b))
	(and (not a) b))))

;;; examples

(define examples
  (lambda ()
    (display "(bag-of (gen-prime 20)) => ")
    (display (bag-of (gen-prime 20)))
    (newline)
    (display "(solve-kalotan-puzzle) => ")
    (display (solve-kalotan-puzzle))
    (newline)
    (display "(color-europe) => ")
    (newline)
    (display (color-europe))
    (newline)
    )) ; more examples

