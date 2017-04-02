;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Palmstrom-Antaya-hw4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Jack Palmstrom          ccc username: jnpalmstrom
;; Alexander Antaya        ccc username: aatantaya

;;
;; Problem 1
;;

(define-struct student (name-of-student email-address))
;; a Student is a (make-student String String)
;; interp. represents a student where
;; name-of-student is the student's name
;; email-address is the student's email address

(define JACK (make-student "Jack Palmstrom" "jnpalmstrom@wpi.edu"))
(define ALEX (make-student "Alexander Antaya" "aantaya@wpi.edu"))
(define FABBO (make-student "Ausin Fabbo" "atfabbo@wpi.edu"))
(define LUCAS (make-student "Lucas Mancinelli" "ljmancinelli@wpi.edu"))
(define BATCH (make-student "Nick Batchelder" "nebatchelder@wpi.edu"))

;; a ListOfStudent
;; empty
;; (cons Student ListOfStudent)

(define LOS1 (list LUCAS BATCH ALEX JACK))
(define LOS2 (list LUCAS FABBO ALEX))
(define LOS3 (list LUCAS ALEX))



;;
;; Problem 2
;;

;; Template for Student

(define (fcn-for-student a-student)
   (... (student-name-of-student a-student)
        (student-email-address a-student)))

;; Template for ListOfStudent
;; a ListOfStudent is one of
;; empty
;; (cons student ListOfStudent)

(define (fcn-for-los alos)
  (cond [(empty? alos) empty]
        [(cons? alos)
         (... (fcn-for-student (first alos))
              (fcn-for-los (rest alos)))]))

;;
;; Problem 3
;;

;; a BST is one of
;; false
;; CourseNode

;; a CourseNode is a (make-coursenode Number String String ListOfStudent BST BST)
(define-struct coursenode (course-id title instructor students left right))
;; interp. false means no BST, or empty BST
;;         course-id is the course id consisting of the department and course
;;         title is the title of the course
;;         instructor is the course's instructor
;;         students is a list of all the student's in the class
;;         left is constructed of courses with course numbers less than the course node
;;         right is constructed of courses with course numbers greater than the course node
;; INVARIANT: for a given node:
;;         course-id is > all course numbers in its l(eft) child
;;         course-id is < all course numbers in its r(ight) child
;;         the same course number never appears twice in the same tree

;;
;; Problem 4
;;

(define BST1 (make-coursenode 03.103 "Chinese I" "Batch" LOS1
                   (make-coursenode 02.102 "Stress Analysis" "Alex" LOS1
                         (make-coursenode 01.101 "Calculus I" "Jack" LOS1 false false) false)
                   (make-coursenode 04.104 "Static System Analysis" "Fabbo" LOS2 false
                         (make-coursenode 05.105 "Physics I" "Lucas" LOS1 false false))))

;;
;; Problem 5
;;

(define (fcn-for-coursenode acourse)
  (...        (coursenode-course-id acourse)
              (coursenode-title acourse)
              (coursenode-instructor acourse)
              (coursenode-students acourse)
              (fcn-for-coursenode (coursenode-left acourse))
              (fcn-for-coursenode (coursenode-right acourse))))


(define (fcn-for-bst aloc)
  (cond [(false? aloc) (...)]
        [(coursenode? aloc)
         (...
             (fcn-for-coursenode (first aloc))
             (fcn-for-bst (rest aloc)))]))

          
;;
;; Problem 6
;;

;; any-taught-by?: BST String -> Boolean                                             SIGNATURE
;; consumes a binary search tree and the name of an instructor, and                  PURPOSE
;; produces true if any of the courses in the course database are taught
;; by the given instructor

(check-expect (any-taught-by? BST1 "Haiau") false)
(check-expect (any-taught-by? BST1 "Batch") true)
(check-expect (any-taught-by? BST1 "Alex") true)
(check-expect (any-taught-by? BST1 "Jack") true)
(check-expect (any-taught-by? BST1 "Fabbo") true)
(check-expect (any-taught-by? BST1 "Lucas") true)

(define (any-taught-by? bst name)
  (cond [(false? bst) false]
        [(coursenode? bst)
         (if (string=? name (coursenode-instructor bst))
             true
             (or
              (any-taught-by? (coursenode-left bst) name)
              (any-taught-by? (coursenode-right bst) name)))]))

;;
;; Problem 7
;;

;; drop-student: BST Number String -> BST                                            SIGNATURE
;; drops the student with the given email address from                               PURPOSE
;; the list of students enrolled in the course

(check-expect (drop-student BST1 04.104 "atfabbo@wpi.edu") (make-coursenode 03.103 "Chinese I" "Batch" LOS1
                   (make-coursenode 02.102 "Stress Analysis" "Alex" LOS1
                         (make-coursenode 01.101 "Calculus I" "Jack" LOS1 false false) false)
                   (make-coursenode 04.104 "Static System Analysis" "Fabbo" LOS3 false
                         (make-coursenode 05.105 "Physics I" "Lucas" LOS1 false false))))
(check-expect (drop-student BST1 01.101 "Bobby@wpi.edu") BST1)
             
(define (drop-student abst coursenum mail)
  (cond [(boolean? abst) false]
        [(coursenode? abst)
         (if (= (coursenode-course-id abst) coursenum)
             (make-coursenode (coursenode-course-id abst)
                              (coursenode-title abst)
                              (coursenode-instructor abst)
                              (remove-students (coursenode-students abst) mail)
                              (coursenode-left abst)
                              (coursenode-right abst))

             (make-coursenode (coursenode-course-id abst)
                              (coursenode-title abst)
                              (coursenode-instructor abst)
                              (coursenode-students abst)
                              (drop-student (coursenode-left abst) coursenum mail)
                              (drop-student (coursenode-right abst) coursenum mail)))]))

(define (remove-students alos mail)
  (cond [(empty? alos) empty]
        [(cons? alos)
         (if (string=? (student-email-address (first alos)) mail)
             (rest alos) 
             (cons (first alos) (remove-students (rest alos) mail)))]))

;;
;; Problem 8
;;

;; list-titles-in-order-by-coursenum: BST -> ListOfCourses (sorted)                SIGNATURE
;; consumes a binary search tree and produces a list of the titles                 PURPOSE
;; of the courses, sorted in order by ascending course number

(check-expect (list-titles-in-order-by-coursenum BST1) (list "Calculus I" "Stress Analysis" "Chinese I" "Static System Analysis" "Physics I"))
(check-expect (list-titles-in-order-by-coursenum false) empty)

(define (list-titles-in-order-by-coursenum abst)
  (cond [(boolean? abst) empty]
        [(coursenode? abst)
         (append (list-titles-in-order-by-coursenum (coursenode-left abst))
                      (list (coursenode-title abst))
                            (list-titles-in-order-by-coursenum (coursenode-right abst)))]))

;;
;; Problem 9
;;

;; add-course: BST Number String String -> BST                                      SIGNATURE
;; consumes a binary search tree, a course number,                                  PURPOSE
;; a course title, and the name of the instructor,
;; and creates a binary search tree the same as the original
;;exceptthat a new course with the given information has been added to the tree

(check-expect (add-course BST1 06.106 "Music I" "Hart") (make-coursenode 03.103 "Chinese I" "Batch" LOS1
                                                               (make-coursenode 02.102 "Stress Analysis" "Alex" LOS1
                                                                     (make-coursenode 01.101 "Calculus I" "Jack" LOS1 false false) false)
                                                               (make-coursenode 04.104 "Static System Analysis" "Fabbo" LOS2 false
                                                                     (make-coursenode 05.105 "Physics I" "Lucas" LOS1 false
                                                                           (make-coursenode 06.106 "Music I" "Hart" empty false false))))) 

(define (add-course abst acoursenum atitle ainstructor)
  (cond [(boolean? abst)(make-coursenode acoursenum atitle ainstructor empty false false)]
        [(coursenode? abst)
         (if (< acoursenum (coursenode-course-id abst))
                    (make-coursenode (coursenode-course-id abst)
                                     (coursenode-title abst)
                                     (coursenode-instructor abst)
                                     (coursenode-students abst)
                                     (add-course (coursenode-left abst) acoursenum atitle ainstructor)
                                     (coursenode-right abst))

                    (make-coursenode (coursenode-course-id abst)
                                     (coursenode-title abst)
                                     (coursenode-instructor abst)
                                     (coursenode-students abst)
                                     (coursenode-left abst) 
                                     (add-course (coursenode-right abst) acoursenum atitle ainstructor)))]))
                                                      
                









