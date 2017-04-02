;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Palmstrom-Antaya-hw3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;Name: Jack Palmstrom      ccc username: jnpalmstrom
;;Name: Alexander Antaya    ccc username: aantaya

;;
;;Problem 1
;;

(define-struct ad (political? name duration cost-to-produce national? time-of-day number-of-times))
;; an Ad is a (make-ad Boolean String Natural Natural Boolean String Natural)
;; interp. represents an ad where
;; political represents whether an ad is political
;; name is the name of the product or politician the ad is for
;; duration is the duration of the ad (in seconds)
;; cost-to-produce is the cost to produce the ad (in thousands of dollars)
;; national is whether or not the ad is to be aired nationally (as opposed to locally)
;; time-of-day is the time of day the ad is to be aired (either daytime, primetime, or off-hour)
;; number-of-times is the number of times the ad is to be aired

(define JEB (make-ad true "Jeb Bush" 120 50 false "primetime" 1))
(define TRUMP (make-ad true "Donald Trump" 30 500 true "daytime" 5))
(define KFC (make-ad false "Kentucky Fried Chicken" 60 100 true "daytime" 10))
(define NFL (make-ad false "National Football League" 90 500 true "primetime" 50))
(define NBA (make-ad false "National Basketball Association" 120 600 true "off-hour" 20))
(define NHL (make-ad false "National Hockey League" 90 500 false "daytime" 50))
(define MLB (make-ad false "Major League Baseball" 15 100 false "off-hour" 100))

;; a ListofAd
;; empty
;; (cons Ad ListOfAd)

(define POLITICAL (cons JEB (cons TRUMP empty)))

(define DAYTIME (cons KFC (cons TRUMP empty)))

(define NOT-POLITICAL (cons KFC (cons NFL empty)))

;;
;; Problem 2
;;

;; Template for Ad

(define (fcn-for-ad an-ad)
   (...(ad-political? an-ad)
       (ad-name an-ad)
       (ad-duration an-ad)
       (ad-cost-to-produce an-ad)
       (ad-national? an-ad)
       (ad-time-of-day an-ad)
       (ad-number-of-times an-ad)))

;; Template for ListOfAd
;; a ListOfAd is one of
;; empty
;; (cons ad ListOfAd)

(define (fcn-for-loa aloa)
  (cond [(empty? aloa) empty]
        [(cons? aloa)
         (... (fcn-for-ad (first aloa))
              (fcn-for-loa (rest aloa)))]))

;;
;; Problem 3
;;

;; count-political-ads: ListOfAds -> Natural                                           SIGNATURE
;; consumes a list of ads and produces the number of ads                           PURPOSE
;; in the list that are classified as political ads

(check-expect (count-political-ads POLITICAL) 2)
(check-expect (count-political-ads DAYTIME) 1)
(check-expect (count-political-ads empty) 0)

(define (count-political-ads aloa)
  (cond [(empty? aloa) 0]
        [(cons? aloa)
         (if  (ad-political? (first aloa))
              (+ 1 (count-political-ads (rest aloa)))
              (count-political-ads (rest aloa)))]))

;;
;; Problem 4
;;

;; any-ads-for?: ListOfAds String -> Boolean                                       SIGNATURE
;; consumes a list of ads and a String representing a product name                 PURPOSE
;; or politician's name, and produces a Boolean. The function returns
;; true if the list contains any ads for the given product or politician

(check-expect (any-ads-for? empty "Don") empty)
(check-expect (any-ads-for? POLITICAL "Donald Trump") true)

(define (any-ads-for? aloa given-string)
  (cond [(empty? aloa) empty]
        [(cons? aloa)
         (if (string=? given-string (ad-name (first aloa))) 
              true
              (any-ads-for? (rest aloa) given-string))]))

;;
;; Problem 5
;;

;; primetime-ads: ListOfAds -> ListOfAds                                           SIGNATURE
;; consumes a list of ads and produces a list of all the ads                       PURPOSE
;; airing in primetime

(check-expect (primetime-ads POLITICAL) (cons JEB empty))
(check-expect (primetime-ads empty) empty)

(define (primetime-ads loa)
  (cond [(empty? loa) empty]
        [(cons? loa)
         (if (string=? "primetime" (ad-time-of-day (first loa)))
              (cons (first loa) (primetime-ads (rest loa)))
              (primetime-ads (rest loa)))]))

;;
;; Problem 6
;;

;; politicians-sponsoring-ads: ListOfAds -> ListOfStrings                          SIGNATURE
;; consumes a list of ads and produces a list of strings                          PURPOSE
;; The list that is produced contains the names of the
;; politicians who have political ads

(check-expect (politicians-sponsoring-ad POLITICAL) (cons "Jeb Bush" (cons "Donald Trump" empty)))
(check-expect (politicians-sponsoring-ad empty) empty)

(define (politicians-sponsoring-ad loa)
  (cond [(empty? loa) empty]
        [(cons? loa)
         (if (ad-political? (first loa))
             (cons (ad-name (first loa)) (politicians-sponsoring-ad (rest loa)))
             (politicians-sponsoring-ad (rest loa)))]))

;;
;; Problem 7
;;

;; air-cost: Ad -> Number                                                         SIGNATURE
;; consumes an Ad and produces the complete cost of airing the Ad                 PURPOSE

(check-expect (air-cost KFC) 1600000)

(define (air-cost an-ad)
   (* (duration an-ad)
      (price-30-sec an-ad)
      (ad-number-of-times an-ad)))

;; Ad-duration: Ad -> Number                                                      SIGNATURE
;; consumes an Ad and produces the duration of the ad                             PURPOSE
;; in 30 second intervals

(check-expect (duration KFC) 2)
(check-expect (duration TRUMP) 1)

(define (duration an-ad)
  (/ (ad-duration an-ad) 30))

;; Price-30-sec: Ad -> Number                                                     SIGNATURE
;; consumes an Ad and produces the price of a 30 second Ad                        PURPOSE
;; depending on the time of the day that the Ad is aired

(check-expect (price-30-sec NFL) 100000)
(check-expect (price-30-sec KFC) 80000)
(check-expect (price-30-sec NBA) 50000)
(check-expect (price-30-sec JEB) 5000)
(check-expect (price-30-sec NHL) 4000)
(check-expect (price-30-sec MLB) 2500)

(define (price-30-sec an-ad)
   (cond [(and (boolean=? (ad-national? an-ad) true)
               (string=? (ad-time-of-day an-ad) "primetime"))
          100000]
         [(and (boolean=? (ad-national? an-ad) true)
               (string=? (ad-time-of-day an-ad) "daytime"))
          80000]
         [(and (boolean=? (ad-national? an-ad) true)
               (string=? (ad-time-of-day an-ad) "off-hour"))
          50000]
         [(and (boolean=? (ad-national? an-ad) false)
               (string=? (ad-time-of-day an-ad) "primetime"))
          5000]
         [(and (boolean=? (ad-national? an-ad) false)
               (string=? (ad-time-of-day an-ad) "daytime"))
          4000]
         [(and (boolean=? (ad-national? an-ad) false)
               (string=? (ad-time-of-day an-ad) "off-hour"))
          2500]))

;;
;; Problem 8
;;

;; campaign-air-cost: ListOfAds String -> Number                                SIGNATURE
;; consumes a list of ads and a name of a politician and produces               PURPOSE
;; the total cost of all the ads for the given politician

;; Example Lists:

(define LOP1 (cons KFC (cons JEB (cons TRUMP empty))))
(define LOP2 (cons JEB (cons TRUMP empty)))
(define LOP3 empty)

(check-expect (campaign-air-cost LOP1 "Jeb Bush") 20000)
(check-expect (campaign-air-cost LOP2 "Donald Trump") 400000)
(check-expect (campaign-air-cost LOP3 "Jeb Bush") 0)

(define (campaign-air-cost aloa politician)
  (cond [(empty? aloa) 0]
        [(cons? aloa)
         (if (and (string=? (ad-name (first aloa)) politician)
                  (ad-political? (first aloa)))
             (air-cost (first aloa))
             (campaign-air-cost (rest aloa) politician))]))

;;
;; Problem 9
;;

;; total-ad-cost: Ad -> Number                                                   SIGNATURE
;; consumes an Ad and produces the total cost of the ad                          PURPOSE
;; which is the sum of the cost of producing the
;; ad and the cost of airing the ad

(check-expect (total-ad-cost JEB) 70000)

(define (total-ad-cost an-ad)
   (+ (* 1000 (ad-cost-to-produce an-ad)) (air-cost an-ad)))

;;
;; Problem 10
;;

;; expensive-ads: ListOfAds Number -> ListOfAds                                  SIGNATURE
;; consumes a list of ads and a Number and produces                              PURPOSE
;; a list of those ads for which the total ad cost exceeds
;; the given number

(check-expect (expensive-ads LOP1 71000) (cons KFC (cons TRUMP empty)))
(check-expect (expensive-ads LOP2 71000) (cons TRUMP empty))
(check-expect (expensive-ads LOP2 2000000) empty)
(check-expect (expensive-ads LOP3 20000) empty)

(define (expensive-ads aloa number)
  (cond [(empty? aloa) empty]
        [(cons? aloa)
         (if (> (total-ad-cost (first aloa)) number)
              (cons (first aloa) (expensive-ads (rest aloa) number))
              (expensive-ads (rest aloa) number))]))



