;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Hackathon) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define PROMPTS
  (list "Stay hydrated!!"
        "Check your posture!"
        "Stay focused!"
        "Have a snack!"
        "Take a few more notes!"
        "Consider participating?"
        "Stretch break!"
        "Instagram can wait!"
        "Pat yourself on the back!"))

(define ANSWERS (list
                 "thanks for the reminder"
                 "just did"
                 "got it"
                 "copy that"
                 "on it"
                 "right away"
                 "one step ahead"
                 "whatever you say"
                 "you're the boss"))

(define (randomize l) (list-ref l (random (length l))))

(define EMPTY (empty-scene 300 200))

(define MESSAGE (text (randomize PROMPTS) 24 "black"))

(define BUTTON (overlay/align "middle"
                              "middle"
                              (text (randomize ANSWERS) 16 "black")
                              (overlay/align "middle"
                                             "middle"
                                             (rectangle 180 35 "outline" "black")
                                             (rectangle 180 35 "solid" "grey"))))

(define-struct st [timer message cycle interval answer])

(define (TIMER s) (overlay/align "middle"
                                 "middle"
                                 (text (tme s) 40 "blue")
                                 (rectangle 110 60 "solid" "grey")))

(define (tme s)
  (string-append
   (number->string (quotient (st-timer (tock s)) 60))
   (if (< (modulo (st-timer (tock s)) 60) 10)
       ":0"
       ":")
   (number->string (modulo (st-timer (tock s)) 60))))

(define (tock s)
  (cond [(false? (st-timer s)) (make-st #f (st-message s) (st-cycle s) (st-interval s) (st-answer s))]
        [(> (st-timer s) 0) (make-st (- (st-timer s) 1) (st-message s) (st-cycle s) (st-interval s) (st-answer s))]
        [(= (st-timer s) 0) (make-st (st-interval s) (randomize PROMPTS) (+ 1 (st-cycle s)) (st-interval s) (randomize ANSWERS))]))

(define (render-timer s)
  (cond [(false? (st-timer s)) EMPTY]
        [(and (number? (st-timer s))
              (false? (st-message s)))
         (place-image (above (TIMER s)
                             (above (text "" 24 "black")
                                    (rectangle 180 35 "solid" "white")))
                      150 100 EMPTY)]
        [(and (number? (st-timer s))
              (string? (st-message s)))
         (place-image (above (TIMER s)
                             (above (text (st-message s) 24 "black")
                                    (overlay/align "middle"
                                                   "middle"
                                                   (text (st-answer s) 16 "black")
                                                   (overlay/align "middle"
                                                                  "middle"
                                                                  (rectangle 180 35 "outline" "black")
                                                                  (rectangle 180 35 "solid" "grey")))))
                      150 100
                      EMPTY)]))

(define (click s n1 n2 m)
  (if (mouse=? m "button-down")
      (make-st (st-timer s) #f (st-cycle s) (st-interval s) (st-answer s))
      s))

(define (done s)
  (>= (st-cycle s) 10))

(define (finished s) (place-image (above (text "Congrats!" 24 "red")
                                         (text "You made it!" 24 "red"))
                              150 100
                                EMPTY))

(define (checkins s)
  (big-bang s
    [on-tick tock .01]
    [to-draw render-timer]
    [on-mouse click]
    [stop-when done finished]))

(define SHORT "50 minutes")
(define MEDIUM "1 hour 15 minutes")
(define LONG "2 hours")
(define Q-SCREEN (above (text "Welcome!" 24 "black")
                        (text "How long is your class?" 24 "black")))
(define BUTTON1 (overlay/align "middle" "middle"
                               (text SHORT 10 "black")
                               (overlay/align "middle" "middle"
                                              (rectangle 100 30 "outline" "black")
                                              (rectangle 100 30 "solid" "gray"))))
(define BUTTON2 (overlay/align "middle" "middle"
                               (text MEDIUM 10 "black")
                               (overlay/align "middle" "middle"
                                              (rectangle 100 30 "outline" "black")
                                              (rectangle 100 30 "solid" "gray"))))
(define BUTTON3 (overlay/align "middle" "middle"
                               (text LONG 10 "black")
                               (overlay/align "middle" "middle"
                                              (rectangle 100 30 "outline" "black")
                                              (rectangle 100 30 "solid" "gray"))))
(define ERROR (above (text "Please press" 18 "red")
                     (text "1, 2, or 3 on your keyboard." 18 "red")))

(define (render-intro w)
  (cond [(= 0 w) (place-image (above Q-SCREEN
                                     (beside BUTTON1 BUTTON2 BUTTON3))
                              150
                              100
                              EMPTY)]
        [(= 1 w) (place-image (above ERROR
                                     (beside BUTTON1 BUTTON2 BUTTON3))
                              150
                              100
                              EMPTY)]
        [else EMPTY]))

(define (choice w key)
  (cond [(key=? key "1") 300]
        [(key=? key "2") (* 7.5 60)]
        [(key=? key "3") (* 12 60)]
        [else 1]))

(define (end w)
  (> w 1))

(define (picker w) (big-bang w
                     (to-draw render-intro)
                     (on-key choice)
                     (stop-when end)
                     (close-on-stop (> w 1))))

(define (main w)
  (local [(define INTERVAL (picker w))]
    (checkins (make-st INTERVAL #f 0 INTERVAL #f))))