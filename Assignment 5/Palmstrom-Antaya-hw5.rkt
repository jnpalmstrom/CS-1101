;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Palmstrom-Antaya-hw5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Jack Palmstrom                ccc username: jnpalmstrom
;; Alexander Antaya              ccc username: aantaya

;;
;; Problem 1
;;

(define-struct river (name pH DO tributaries))
;; a River is a (make-river String Natural Number ListOfTributaries)
;; interp. represents a river where
;; name is the name of the river
;; pH is the pH of the water
;; DO is the dissolved oxygen in the water (in mg/L)
;; tributaries is a list of the tributaries that feed into the river

;; a ListOfTributaries is
;; empty
;; (cons tributaries ListOfTributaries)

;;
;; Problem 2
;;

;; Example of a river system

(define SUN (make-river "Sun River" 7 7 empty))
(define MADISON (make-river "Madison River" 7 2 empty))
(define GALLATIN (make-river "Gallatin River" 7 4 empty))
(define GARDNER (make-river "Gardner River" 7 7 empty))
(define SHIELDS (make-river "Shields River" 7 7 empty))
(define BOULDER (make-river "Boulder River" 7 7 empty))
(define BIG-HOLE (make-river "Big Hole River" 5 6 empty))
(define BEAVERHEAD (make-river "Beaverhead River" 5 7 empty))
(define JEFFERSON (make-river "Jefferson River" 6.6 8.2 (list BEAVERHEAD BIG-HOLE)))
(define YELLOWSTONE (make-river "Yellowstone River" 7 7 (list GARDNER SHIELDS BOULDER)))
(define MISSOURI (make-river "Missouri River" 6.5 8 (list JEFFERSON SUN YELLOWSTONE MADISON GALLATIN)))
                                     
;;
;; Problem 3
;;

;; Template for River

(define (fcn-for-river a-river)
   (... (river-name a-river)
        (river-pH a-river)
        (river-DO a-river)
        (fcn-for-lot (river-tributaries a-river))))

;; Template for ListOfTributaries
;; a ListOfTributaries is one of
;; empty
;; (cons tributaries ListOfTributaries)

(define (fcn-for-lot alot)
  (cond [(empty? alot) (...)]
        [(cons? alot)
         (...(fcn-for-river (first alot)) 
             (fcn-for-lot (rest alot)))]))

;;
;; Problem 4
;;

;; list-acidic-rivers: ListOfTributaries -> ListOfString                              SIGNATURE
;; returns a list of the names of rivers in the system                                PURPOSE
;; that have a pH level lower than 6.5

;; test case for helper-river
(check-expect (helper-river MISSOURI) (list "Beaverhead River" "Big Hole River"))

;; test cases for low-pH
(check-expect (low-pH MISSOURI) false)
(check-expect (low-pH YELLOWSTONE) false)
(check-expect (low-pH BIG-HOLE) true)

(define (low-pH alor)
  (< (river-pH alor) 6.5))

(define (helper-river alor)
  (if (low-pH alor)
      (cons (river-name alor)
            (list-acidic-rivers (river-tributaries alor)))
      (list-acidic-rivers (river-tributaries alor))))   

(define (list-acidic-rivers alor)
 (cond [(empty? alor) empty]
       [(cons? alor)
        (append (helper-river (first alor))
                (list-acidic-rivers (rest alor)))]))

;;
;; Problem 5
;;

;; lower-all-ph: River -> River                                                       SIGNATURE
;; consumes a River system and produces a River system with the pH                    PURPOSE
;; of all the Rivers in the system lowered by LOWER-RIVER-PH (0.1)

(define (lower-all-ph a-river)
  (make-river (river-name a-river)
              (- (river-pH a-river) 0.1)
              (river-DO a-river)
              (lower-all-ph-list (river-tributaries a-river))))

;; lower-all-ph-list: ListOfRiver -> ListOfRiver                                      SIGNATURE
;; consumes a ListOfRiver and lowers the pH of all the Rivers in the                  PURPOSE
;; list of river by 0.1

(define (lower-all-ph-list alor)
  (cond [(empty? alor) empty]
        [(cons? alor)
         (cons (lower-all-ph (first alor))
                           (lower-all-ph-list (rest alor)))]))

;; tests cases for lower-all-ph
(check-expect (lower-all-ph BEAVERHEAD)(make-river "Beaverhead River" 4.9 7 empty))

(check-expect (lower-all-ph JEFFERSON)(make-river "Jefferson River" 6.5 8.2
                                                  (list (make-river "Beaverhead River" 4.9 7 empty)
                                                        (make-river "Big Hole River" 4.9 6 empty))))

;;
;; Problem 6
;;

;; cold-water-fish-warning: River -> String                                            SIGNATURE
;; produces OK if all the rivers in the system have a DO level of                      PURPOSE
;; 5 mg/L or above, if any river in the system has a DO level below
;; 3 mg/L, deadly is produced, otherwise marginal is produced

;; test cases for cold-water-fish-warning
(check-expect (cold-water-fish-warning JEFFERSON)      "OK")
(check-expect (cold-water-fish-warning MISSOURI) "Marginal")
(check-expect (cold-water-fish-warning MADISON)    "Deadly")
(check-expect (cold-water-fish-warning (make-river "Blue River" 7 1
                                                   (list (make-river "Red River" 7 3 empty)
                                                         (make-river "Yellow River" 7 1 empty))))"Deadly")
(check-expect (cold-water-fish-warning (make-river "Long River" 7 4
                                                   (list (make-river "Short River" 7 4 empty)
                                                         (make-river "Deep River" 7 4 empty))))"Marginal")

(define (cold-water-fish-warning alor)
  (cond [(and (<= (river-DO alor) 3) (cold-water-fish-warning-helper2 (river-tributaries alor))) "Deadly"]
        [(and (>= (river-DO alor) 5) (cold-water-fish-warning-helper1 (river-tributaries alor))) "OK"]
        [else "Marginal"]))

(define (cold-water-fish-warning-helper1 alor)
  (cond [(empty? alor) true] 
        [(cons? alor)
         (if (>= (river-DO (first alor)) 5)
             (and (cold-water-fish-warning-helper1 (river-tributaries (first alor))) (cold-water-fish-warning-helper1 (rest alor)))
              false)]))

(define (cold-water-fish-warning-helper2 alor)
  (cond [(empty? alor) true]
        [(cons? alor)
         (if (<= (river-DO (first alor)) 3)
             (and (cold-water-fish-warning-helper2 (river-tributaries (first alor))) (cold-water-fish-warning-helper2 (rest alor)))
              false)]))

;;
;; Problem 7
;;

;; find-subsystem: String River -> River Or false                                     SIGNATURE
;; consumes the name of a River and a River system and produces a River               PURPOSE
;; system with the given name as the root of the River system if the named
;; river is found in the given River system

(define (find-subsystem a-name alor)
  (if (string-ci=? (river-name alor) a-name)
       alor
      (find-subsystem-list a-name (river-tributaries alor))))

; find-subsystem-list: String ListOfRiver -> River OR false                            SIGNATURE
; consumes the name of a River and a ListOfRiver and produces a River                  PURPOSE
; system with the given name as the root of the River system if the named
; river is found in the given ListOfRivers

(define (find-subsystem-list a-name alor)
  (cond [(empty? alor) false]
        [(cons? alor)
         (if (river? (find-subsystem a-name (first alor)))
                     (find-subsystem a-name (first alor))
                     (find-subsystem-list a-name (rest alor)))]))

;; test cases for find-subsystem
(check-expect (find-subsystem "Jefferson River" MISSOURI) JEFFERSON)
(check-expect (find-subsystem "Shields River" YELLOWSTONE) SHIELDS)
(check-expect (find-subsystem "Beaverhead River" BEAVERHEAD) BEAVERHEAD)
(check-expect (find-subsystem "Beaver" BEAVERHEAD) false)

;;
;; Problem 8
;; 

;; count-lower-ph: String River -> Natural                                             SIGNATURE
;; consumes a name of a river and River system and produces a count                    PURPOSE
;; of rivers in the River system with a pH lower than the given
;; pH value

(define (count-acidic-tributaries-of a-name alor)
  (if (and (is-pH-lower-than? a-name alor) (false? (find-subsystem a-name alor)))
      (+ 1
         (count-acidic-tributaries-list a-name (river-tributaries alor)))
         (count-acidic-tributaries-list a-name (river-tributaries alor))))

;; is-pH-lower-than?: String River -> Boolean                                          SIGNATURE
;; consumes a name of a river and a River system and produces true                     PURPOSE
;; if the river has a pH below the given pH value and false otherwise
(define (is-pH-lower-than? a-name alor)
  (< (river-pH alor) 6.5))

;; count-lower-ph-list: String ListOfRiver -> Natural                                  SIGNATURE
;; consumes a ListOfRiver and a name of river and produces a count of rivers           PURPOSE
;; in the ListOfRiver with a pH lower than the given pH value
(define (count-acidic-tributaries-list a-name alor)
  (cond [(empty? alor) 0]
        [(cons? alor)(+ (count-acidic-tributaries-of a-name (first alor))
                        (count-acidic-tributaries-list a-name (rest alor)))]))


;; test cases for is-pH-lower-than?
(check-expect (is-pH-lower-than? BIG-HOLE (make-river "Long River" 7 6 empty))    false)
(check-expect (is-pH-lower-than? BIG-HOLE (make-river "Short River" 5 6 empty))    true)
(check-expect (is-pH-lower-than? BIG-HOLE (make-river "Great River" 6.5 6 empty)) false)

;; test cases for count-acidic-tributaries-of
(check-expect (count-acidic-tributaries-of "Missouri River" MISSOURI)   2)
(check-expect (count-acidic-tributaries-of "Jefferson River" JEFFERSON) 2)



