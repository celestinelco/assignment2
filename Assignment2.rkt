;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Assignment2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rsound)
(require 2htdp/image)
(require 2htdp/universe)


;; a ws is
;; - (make-ws slider-frac-x slider-frac-y end-frame)
;; where slider-frac-x & slider-frac-y is a number between
;; zero and one, and end-frame is a frame
;; number
(define-struct ws [slider-frac-x slider-frac-y end-frame bgFr])


(define ps (make-pstream))
(define img (bitmap "DrRatchet.png"))
(define bgGif
  (list
   (bitmap "sn1.png") 
   (bitmap "sn2.png")
   (bitmap "sn3.png")
   (bitmap "sn4.png")
   (bitmap "sn5.png")
   (bitmap "sn6.png")
   (bitmap "sn7.png")
   (bitmap "sn8.png")
   (bitmap "sn9.png")
   (bitmap "sn10.png")
   (bitmap "sn11.png") ))


;; examples:
(make-ws 0.5 0.5 2000 0)
(make-ws 0.7 0.7 80000 0)


(define INITIAL-STATE
  (make-ws 0.0 0.0 0 0))


(define song
  (resample-to-rate
   FRAME-RATE
   (rs-read/clip "cutit.wav"
                 0
                 (* 60 FRAME-RATE))))


(define BOX-WIDTH 600)
(define BOX-HEIGHT 600)
(define SLIDER-Y (/ BOX-HEIGHT 2))
(define SLIDER-WIDTH 25)
(define SLIDER-HEIGHT 25)
(define SLIDER (rectangle SLIDER-WIDTH
                          SLIDER-HEIGHT
                          "solid"
                          "magenta"))
(define SLIDER-BG (rectangle BOX-WIDTH
                             BOX-HEIGHT
                             "solid"
                             "turquoise"))
;; draw the slider
;; ws -> image
(define (draw-slider ws)
  (place-image img
               (* (ws-slider-frac-x ws)
                  BOX-WIDTH)
               (* (ws-slider-frac-y ws)
                  BOX-HEIGHT)
               (draw-bg ws)))
(define (draw-bg ws)
  (;;;WRITE CONDITIONAL HERE FOR ws-bgFr ws
   list-ref bgGif (ws-bgFr ws)))
  


(check-expect
 (draw-slider (make-ws 0.5 0.5 1234 0))
 (place-image img
              300 SLIDER-Y
              (list-ref bgGif 0)))


;; handle a mouse action; on a drag or
;; click, update the slider position
;; ws number number event -> ws
(define (handle-mouse ws x y event)
  (cond [(string=? event "button-down")
         (make-ws (/ x BOX-WIDTH)
                  (/ y BOX-HEIGHT)
                  (ws-end-frame ws)
                  0)]
        [(string=? event "drag")
         (make-ws (/ x BOX-WIDTH)
                  (/ y BOX-HEIGHT)
                  (ws-end-frame ws)
                  0)]
        [else ws]))


(check-expect
 (handle-mouse (make-ws 0.6 0.6 1234 0)
               150 44 "move")
 (make-ws 0.6 0.6 1234 0))
(check-expect
 (handle-mouse (make-ws 0.6 0.6 4243 0)
               150 44 "drag")
 (make-ws (/ 150 BOX-WIDTH) (/ 44 BOX-HEIGHT) 4243 0))
(check-expect
 (handle-mouse (make-ws 0.6 0.6 1123 0)
               150 44 "button-down")
 (make-ws (/ 150 BOX-WIDTH) (/ 44 BOX-HEIGHT) 1123 0))




;; if it's time, queue up the next section
;; of the song
(define (tick-fun ws)
  (cond [(time-to-play? (ws-end-frame ws)
                        (pstream-current-frame ps))
         (both
          (queue-next-fragment
           (round (* (ws-slider-frac-x ws)
                     (rs-frames song)))
           (ws-slider-frac-y ws)
           (ws-end-frame ws))
          (make-ws (+ (ws-slider-frac-x ws) PLAY-POSNFRAC)
                   (ws-slider-frac-y ws)
                   (+ (ws-end-frame ws) PLAY-FRAMES)
                   (incrementBgFr ws))
          )]
        [else ws]))


(define (incrementBgFr ws)
  (cond [(= (ws-bgFr ws) 0) 1]
        [(= (ws-bgFr ws) 1) 2]
        [(= (ws-bgFr ws) 2) 3]
        [(= (ws-bgFr ws) 3) 4]
        [(= (ws-bgFr ws) 4) 5]
        [(= (ws-bgFr ws) 5) 6]
        [(= (ws-bgFr ws) 6) 7]
        [(= (ws-bgFr ws) 7) 8]
        [(= (ws-bgFr ws) 8) 9]
        [(= (ws-bgFr ws) 9) 10]
        [(= (ws-bgFr ws) 10) 0]))
        


;; queue up the next fragment
(define (queue-next-fragment song-frame yComp frame-to-play)
  (pstream-queue ps
                 (rs-scale yComp (clip song song-frame (+ song-frame PLAY-FRAMES)))
                 frame-to-play))


;; how long should each queued segment be, in seconds?
(define PLAY-SECONDS 1/20)
;; .. in frames?
(define PLAY-FRAMES (* PLAY-SECONDS FRAME-RATE))
;; .. as a fraction of the slider?
(define PLAY-POSNFRAC (/ PLAY-SECONDS (/ (rs-frames song) FRAME-RATE)))
;; how long should the big-bang ticks be?
(define TICK-LEN 1/40)
;; the longest lead time for which we'll queue the next sound
(define MAX-QUEUE-INTERVAL (* 3/80 FRAME-RATE))






;; is it time to play the next chunk, yet?
(define (time-to-play? end-frame cur-frame)
  (< (- end-frame cur-frame) MAX-QUEUE-INTERVAL))


(define (both a b) b)


(define (all a b c) c)


(big-bang INITIAL-STATE
          [to-draw draw-slider]
          [on-mouse handle-mouse]
          [on-tick tick-fun TICK-LEN])
