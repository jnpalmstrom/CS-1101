;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Palmstrom-Antaya-hw6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #t)))
;; Jack Palmstrom         ccc username: jnpalmstrom
;; Alexander Antaya       ccc username: aantaya

;;
;;Part 1
;;

;; Definitions

(define-struct ad (political? name duration production-cost national? time-of-day repetitions))
;; an Ad is a (make-ad Boolean String Natural Natural Boolean String Natural)
;; interp:  a television ad, where
;;          political? is true if the ad is a political ad, false otherwise (for product ad)
;;          name is the name of the politician or the product the ad is for
;;          duration is the length of the ad (in seconds)
;;          production-cost is the cost to produce the ad (in thousands of dollars)
;;          national? is true if the ad is to be aired nationally (false if locally)
;;          time-of-day is one of "P" for primetime, "D" for daytime, "O" for off-hour
;;          repetitions is the number of times the ad is to be played

;; a ListOfAd is one of
;;   empty
;;   (cons Ad ListOfAd)

(define JEB (make-ad true "Jeb Bush" 120 50 false "primetime" 1))
(define TRUMP (make-ad true "Donald Trump" 30 500 true "daytime" 5))
(define KFC (make-ad false "Kentucky Fried Chicken" 60 100 true "daytime" 10))
(define NFL (make-ad false "National Football League" 90 500 true "primetime" 50))
(define NBA (make-ad false "National Basketball Association" 120 600 true "off-hour" 20))
(define NHL (make-ad false "National Hockey League" 90 500 false "daytime" 50))
(define MLB (make-ad false "Major League Baseball" 15 100 false "off-hour" 100))
(define POLITICAL (cons JEB (cons TRUMP empty)))
(define DAYTIME (cons KFC (cons TRUMP empty)))
(define NOT-POLITICAL (cons KFC (cons NFL empty)))
(define LOP1 (cons KFC (cons JEB (cons TRUMP empty))))
(define LOP2 (cons JEB (cons TRUMP empty)))
(define LOP3 empty)

;;
;; Problem 1
;;

;; Tests

(check-expect (count-political-ads POLITICAL) 2)
(check-expect (count-political-ads DAYTIME) 1)
(check-expect (count-political-ads empty) 0)

;; Functions

;; count-political: loa -> loa
;; consumes a list of ads and produces the number 
;; of ads in the list that are classified as political ads
(define (count-political-ads loa)
  (local [(define (political-list loa)
            (filter ad-political? loa))]
    (length (political-list loa))))

;;
;; Problem 2
;;

;; Tests
(check-expect (primetime-ads POLITICAL) (cons JEB empty))
(check-expect (primetime-ads empty) empty)

;; Functions

;; primetime-ads: ListofAds -> ListofAds
;; consumes a list of ads and produces a list of all the ads airing in primetime
(define (primetime-ads loa)
  (local [(define (national? an-ad)
            (string=? (ad-time-of-day an-ad) "primetime"))] (filter national? loa)))

;;
;; Problem 3
;;

;; Tests
(check-expect (politicians-sponsoring-ads POLITICAL) (list "Jeb Bush" "Donald Trump"))
(check-expect (politicians-sponsoring-ads DAYTIME) (list "Donald Trump"))
(check-expect (politicians-sponsoring-ads empty) empty)

;; Functions

;; politicians-sponsoring-ad: ListofAds -> ListofString
;; consumes a list of ads and produces a list of strings
;; the list that is produced contains the names of the politicians who have political ads
(define (politicians-sponsoring-ads loa)
  (local [(define (names? an-ad)
            (ad-name an-ad))]
    (map names? (filter ad-political? loa))))

;;
;; Problem 4
;;

;; Tests
(check-expect (cheap-to-produce LOP1 51000) (list JEB))
(check-expect (cheap-to-produce LOP1 2200000) (list KFC JEB TRUMP))
(check-expect (cheap-to-produce LOP2 20) empty)
(check-expect (cheap-to-produce LOP3 20000) empty)

;; Functions

;; cheap-to-produce: ListofPoliticians Number -> ListofPoliticians
;; consumes a list of ads and a number and produces a list of ads. The list that's produced
;; contains those ads for which the production costs are less than the given amount
(define (cheap-to-produce lop num)
  (local [(define (less-than? an-ad)
            (if (< (* 1000 (ad-production-cost an-ad)) num)
                true
                false))]
    (filter less-than? lop)))

;;
;; Part 2
;;

;;
;; Problem 5
;;

;; Definitions

(define-struct user (username inbox))
;; a User is a (make-user String ListOfMessages)
;; interp: a user, where
;;         username is a username of the user
;;         inbox is a list of all the messages in their mailbox

;; a ListOfMessages is one of
;;   empty
;;   (cons messages ListOfMessages)

(define-struct messages (user-who-sent text flag))
;; Messages are a (make-message User String Boolean)
;; interp: Messages where,
;;         username is the username of the user that sent the message
;;         text is the text contained inside of a message
;;         flag indicates whether or not the user has read the message

;; a ListOfMessages is one of
;;   empty
;;   (cons Messages ListOfMessages)

;; Mailsys: ListOfUsers
;; rembembers the username and messages in the mail system 
(define mailsys empty)

;;
;; Problem 6
;;

;; add-user: username -> void
;; add a new user with the given username to the mail system
;; EFFECT: modify the mail system

(define (add-user user)
  (set! mailsys (cons (make-user user empty) mailsys)))

;;
;; Problem 7
;;


;; send-email: User User String -> void
;; consumes the name of the sender of an email, the name of the recipient
;; of the email, and the text of an email message, and produces void
;; EFFECT: store a new unread message in the recipient's mailbox

(define (send-email sender recipient text)
  (local [(define (send-email sender recipient text alou)
            (cond [(empty? mailsys) (error "User not in system")]
                  [(cons? mailsys) (if (string=? recipient (user-username (first alou)))
                                        (set-user-inbox! (first alou) (cons (make-messages (look-for-user sender mailsys) text true) (user-inbox (first alou))))
                                        (send-email sender recipient text (rest alou)))]))]
    (send-email sender recipient text mailsys)))


(define (look-for-user username alou)
  (cond [(empty? alou) (error "User not in system")]
        [(cons? alou) (if (string=? username (user-username (first alou)))
                          (first alou)
                          (look-for-user username (rest alou)))]))

;;
;; Problem 8
;;

;; get-unread-messages: String -> ListOfMessages
;; produced list contains the unread messages in the mailbox
;; of the user with the given name

;; get-unread-messages2: String -> ListOfMessages
;; consumes a username and produces a list of unread messages
;; from the mailbox of the given user
(define (get-unread-messages a-username)
  (get-unread-messages2 a-username mailsys))

;; get-unread-messages2: String ListOfUsers -> ListOfMessages
;; consumes a username and produces a list of unread messages
;; from the mailbox of the given user
(define (get-unread-messages2 a-username alou)
  (cond [(empty? alou) (error "Cannot perform action")]
        [(cons? alou)
         (if (string-ci=? a-username (user-username (first alou)))
             (unread-messages (user-inbox (first alou)))
             (get-unread-messages2 a-username (rest alou)))]))

;unread-messages: ListOfMessages -> ListOfMessages
;consumes a ListofMessages and returns only the messages which are
;unread
(define (unread-messages alom)
  (filter unread-messages? alom))

;unread-messages?: Message -> Boolean
;consumes a messages and returns true if the message is unread
(define (unread-messages? a-message)
  (false? (messages-flag a-message)))

;;(define (marked-read a-username a-message)
;;  (set! mailsys (make-user (user-username a-username) (list (make-messages (messages-user-who-sent ...) (messages-text ...) true)))))

;;
;; Problem 9
;;

;; most-social: -> User
;; produces the person with the most friends in network

;; most-social-inlist: ListOfPerson Person -> Person
;; produces the person with the most friends in a given ListOfPerson
(define (most-messages)
  (if (empty? mailsys)
      (error "No users in system")
      (local [(define (most-messages given-user alou mostSoFar)
            (cond [(empty? alou) given-user]
                  [(cons? alou)
                   (begin
                     (if (> (length (user-inbox (first alou))) mostSoFar)  
                       (most-messages (first alou) (rest alou) (length (user-inbox (first alou))))
                       (most-messages given-user (rest alou) mostSoFar)) given-user)]))]
    (most-messages (first mailsys) mailsys 0))))


;;
;; Problem 10
;;

(set! mailsys empty)
;; sets the initial value of mailsys to empty

mailsys
;; calls mailsys to confirm it is empty

(add-user "Tom")
(add-user "Bob")
;; add two users to mailsys

mailsys
;; confirms that mailsys now has two new users (Tom, Bob)

(send-email "Tom" "Bob" "Hello")
;; sends an email from Tom to Bob containing the text "Hello"

mailsys
;; confirms that mailsys now has the message between Tom and Bob

(get-unread-messages "Bob")
;; displays the unread emails in Bobs inbox

mailsys
;; confirms that mailsys now has the message that Tom sent Bob and that it is
;; flagged as read

(most-messages)
;; displayes the user with the most UNREAD meassages in the inbox

(set! mailsys empty)
;; resets the mailsys (Mail-System) back to empty (containing no users)

(most-messages)
;; displayes the error message "No users in system". Meaning that there are no
;; users in the syetm to display the most messages of