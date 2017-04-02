;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Palmstrom-Antaya-hw1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;Name: Jack Palmstrom      ccc username: jnpalmstrom
;;Name: Alexander Antaya    ccc username: aaantaya

;;
;;Problem 1
;;

;;Data Definition
(define-struct Film (title genre rating running-time opening-date receipts))
;; a Film is a (make-film String String String Natural Date Natural)
;; interpretation: a Film where
;; title is the film's title
;; genre is the film's genre (drama, comedy, family, etc)
;; rating is the rating of the film (G, PG, PG-13, R, NC-17, NR)
;; running-time is the length of the film (in minutes)
;; opening-date is the day the film opens (year.month.day)
;; reciepts total box office receipts collected so far for the film (in millions of dollars)

(define-struct Date (year month day))
;; a Date is a (make-date Natural Natural Natural)
;; interpretation: a date where
;; year is the year of the film
;; month is the month of the film
;; day is the day of the film

;;Examples
(make-Date 1976 12 3)
(make-Date 1994 6 6)
(make-Date 2008 7 25)
(make-Date 2007 9 1)

;;
;;Problem 2
;;

(define ROCKY (make-Film "Rocky" "Action" "PG" 120 (make-Date 1976 12 3) 117))
;; String String String Number String Number -> String                                           SIGNATURE
;; Define ROCKY as a helper function to make the code easier to read.                            PURPOSE
;; it takes the date created in the make-film function and puts it into a single function.

(define LION-KING (make-Film "Lion King" "Family" "G" 88 (make-Date 1994 6 13) 968))
;; String String String Number String Number -> String                                           SIGNATURE
;; Define LION-KING as a helper function to make the code easier to read.                        PURPOSE
;; it takes the date created in the make-film function and puts it into a single function.

(define STEP-BROTHERS (make-Film "Step Brothers" "Comedy" "R" "98" (make-Date 2008 7 25) 128))
;; String String String Number String Number -> String                                           SIGNATURE
;; Define STEP-BROTHERS as a helper function to make the code easier to read.                    PURPOSE
;; it takes the date created in the make-film function and puts it into a single function.

(define JUNO (make-Film "Juno" "Romance" "PG-13" 96 (make-Date 2007 9 1) 143))
;; String String String Number String Number -> String                                           SIGNATURE
;; Define JUNO as a helper function to make the code easier to read.                             PURPOSE
;; it takes the date created in the make-film function and puts it into a single function.

;;
;;Problem 3
;;

;; suitable-for-children? Film -> Boolean                                                        SIGNATURE
;; Takes a film and produces true if the movie is rated either                                   PURPOSE
;; "G", "PG" or "PG-13" menaing it is suitable for childern
;; and produces false if the film is rated "R", "NC-17" or "NR".

;; Check Expects for suitable-for-children?
(check-expect (suitable-for-children? ROCKY) true)
(check-expect (suitable-for-children? STEP-BROTHERS) false)

(define (suitable-for-children? any-rating)
  (cond [(or (string=? (Film-rating any-rating) "G") (string=? (Film-rating any-rating) "PG") (string=? (Film-rating any-rating) "PG-13")) true]
        [else false]))

;;
;;Problem 4
;;

;; difference-in-receipts Film Film -> Number                                                   SIGNATURE
;; Takes the amount of tickets sold from one film and subtracts it from                         PURPOSE
;; the amount of tickets sold from another film to find the diffrence in
;; tickets sold.

;; Check Expects for difference-in-receipts
(check-expect (difference-in-receipts ROCKY JUNO) 26)
(check-expect (difference-in-receipts LION-KING JUNO) 825)
(check-expect (difference-in-receipts JUNO JUNO) 0)

(define (difference-in-receipts film-one film-two)
  (if (> (Film-receipts film-one) (Film-receipts film-two))
      (- (Film-receipts film-one) (Film-receipts film-two))
      (- (Film-receipts film-two) (Film-receipts film-one))))

;;
;;Problem 5
;;

;;modify-rating Film String -> Film                                                             SIGNATURE
;;Consumes a Film and a String, and produces a Film. The film that is                           PURPOSE
;;produced is the same as the original except that the film's rating
;;has been replaced by the given rating.

;; Check Expects for modify-rating
(check-expect (modify-rating ROCKY "R") (make-Film "Rocky" "Action" "R" 120 (make-Date 1976 12 3) 117))

(define (modify-rating film given-rating)
  (make-Film (Film-title film) (Film-genre film) given-rating (Film-running-time film) (Film-opening-date film) (Film-receipts film)))
  
;;
;;Problem 6
;;

;; year-of-release: Film Date -> Boolean
;; consumes the year of release of a film and a year
;; of a given date then outputs a boolean to tell if
;; the year the film came out before or during the given year

;; Check Expects for year-of-release
(check-expect (year-of-release ROCKY (make-Date 1920 1 1)) false)
(check-expect (year-of-release ROCKY (make-Date 2016 1 1)) true)
(check-expect (year-of-release ROCKY (make-Date 1976 1 1)) false)
              
(define (year-of-release movie a-date)
  (> (Date-year a-date) (Date-year (Film-opening-date movie))))

;; month-of-release: Film Date -> Boolean
;; consumes the month of release of a film and a month
;; of a given date then outputs a boolean to tell if
;; the month the film came out before or during the given month

;; Check Expects for month-of-release
(check-expect (month-of-release LION-KING (make-Date 1994 5 1)) false)
(check-expect (month-of-release LION-KING (make-Date 1994 7 1)) true)
(check-expect (month-of-release LION-KING (make-Date 1994 6 1)) false)

(define (month-of-release movie a-date)
  (> (Date-month a-date) (Date-month (Film-opening-date movie))))

;; day-of-release: Film Date -> Boolean
;; consumes the day of release of a film and a day
;; of a given date then outputs a boolean to tell if
;; the day the film came out before the given day

;; Check Expects for day-of-release
(check-expect (day-of-release ROCKY (make-Date 1976 12 2)) false)
(check-expect (day-of-release ROCKY (make-Date 1976 12 4)) true)
(check-expect (day-of-release ROCKY (make-Date 1976 12 3)) false)

(define (day-of-release movie a-date)
  (> (Date-day a-date) (Date-day (Film-opening-date movie))))

;; opens-before? Film Date -> Boolean                                              SIGNATURE
;; Consumes the year of release of a film as a number                              PURPOSE
;; and a given date then outputs a boolean to tell
;; if the given film comes out before the given date

;; Check Expects for opens-before?
(check-expect (opens-before? ROCKY (make-Date 1975 12 4)) false)
(check-expect (opens-before? ROCKY (make-Date 1977 5 15)) true)
(check-expect (opens-before? ROCKY (make-Date 1976 1 3)) false)
(check-expect (opens-before? ROCKY (make-Date 1976 12 4)) true)
(check-expect (opens-before? ROCKY (make-Date 1976 12 3)) false)
(check-expect (opens-before? ROCKY (make-Date 1976 12 2)) false)

(define (opens-before? movie a-date)
  (if (year-of-release movie a-date)
      true
      (if (and (= (Date-year a-date) (Date-year (Film-opening-date movie))) (month-of-release movie a-date))
          true
          (if (and (= (Date-year a-date) (Date-year (Film-opening-date movie))) (= (Date-month a-date) (Date-month (Film-opening-date movie))) (day-of-release movie a-date))
              true
              false))))
                            
                  






