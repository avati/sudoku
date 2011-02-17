#!/usr/bin/guile -s
!#

;; 
;; Author: Anand. V. Avati <avati@zresearch.com>
;;
;; Solves a `sudoku' puzzle
;;

(use-modules (ice-9 debugger))

(define initial (list
		 7 0 0  0 9 0  0 0 3
		 0 0 5  8 0 2  6 0 0
		 0 8 0  3 0 1  0 9 0

		 0 5 0  7 0 4  0 1 0
		 3 0 0  0 0 0  0 0 4
		 0 4 0  5 0 9  0 8 0

		 0 2 0  9 0 8  0 5 0
		 0 0 9  6 0 7  4 0 0
		 5 0 0  0 2 0  0 0 8))

(define (get-zero-positions space)
  (let ((zero-pos (list-index space 0)))
    (cond ((not zero-pos) '())
	  (else (append (list zero-pos)
			(map (lambda (x) 
			       (+ x zero-pos 1))
			     (get-zero-positions (list-tail 
						  space 
						  (+ zero-pos 1)))))))))

(define (get-vert-list pos)
  (map (lambda (x)
	 (+ (* x 9) (modulo pos 9)))
       '(0 1 2 3 4 5 6 7 8)))

(define (get-horiz-list pos)
  (map (lambda (x)
	 (+ x (- pos (modulo pos 9))))
       '(0 1 2 3 4 5 6 7 8)))

(define (get-block-list pos)
  (map (lambda (x)
	 (+ x
	    (+ (- (modulo pos 9)
		  (modulo pos 3))
	       (* (- (/ (- pos(modulo pos 9)) 9)
		   (modulo (/ (- pos (modulo pos 9)) 9) 3))
		  9))))
       '(0 1 2 9 10 11 18 19 20)))

(define (get-unique-number rangelist number)
  (cond ((= (length rangelist) 0) 
	 number)
	((and (not (eq? (car rangelist) 0))
	      (not (eq? number #f))) 
	 #f)
	((= (car rangelist) 0)
	 (get-unique-number (cdr rangelist) number))
	(else
	 (get-unique-number (cdr rangelist) (car rangelist)))))

(define (try-to-fix space pos)
  (let ((neighbours (map (lambda (x) 
		  (list-ref space x))
		  (append
		   (get-horiz-list pos)
		   (get-vert-list pos)
		   (get-block-list pos))) )
	(rangelist (list 0 1 2 3 4 5 6 7 8 9)))
    (for-each (lambda (num)
		(list-set! rangelist num 0)) neighbours)
    (get-unique-number rangelist #f)))
    

(define (get-a-fix space zero-pos-list)
  (if (= (length zero-pos-list) 0) 
      #f
      (let* ((pos (car zero-pos-list))
	    (value (try-to-fix space pos)))
	(if value
	    (cons pos value)
	    (get-a-fix space (cdr zero-pos-list))))))


(define (solve space)
  (let* ((zero-pos-list (get-zero-positions space))
	(fix (get-a-fix space zero-pos-list)))
    (if (not fix)
	space
	(solve (append (list-head space (car fix))
		       (list (cdr fix))
		       (list-tail space (+ 1 (car fix))))))))

(define (display-nine biglist)
  (cond ((= (length biglist) 0) #f)
	(else (begin
		(display (list-head biglist 9))
		(newline)
		(display-nine (list-tail biglist 9))))))

(display-nine (solve initial))