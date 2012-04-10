(ns captcha.core
  (:import [java.awt.image BufferedImage]
           [java.awt Color Font Graphics]
           [java.util Random]
           [javax.imageio ImageIO]))

;use to hold captchas.
(def captchas (ref {}))

(defstruct captcha :text :create-time)

(def r (Random.))

;captchas will timeout after 10 mins
(def max-hold-time 600000)

(defn- random-captcha-text
  []
  (last
    (let [captcha (atom "")
          indexs (range 4)]
      (for [i indexs]
        (reset! captcha (str (. r nextInt 10) @captcha))))))

(defn- random-captcha
  []
  (let [text (random-captcha-text)
        create-time (System/currentTimeMillis)]
    (struct captcha text create-time)))

(defn add-captcha
  "add a captcha for id, if id is reduplicate, will throw a RuntimeException."
  [id]
  (dosync
    (if (contains? @captchas (keyword id)) (throw (RuntimeException. "id reduplicate.")))
    (alter captchas assoc (keyword id) (random-captcha))))

(defn- get-captcha
  [id]
  (if (contains? @captchas (keyword id)) nil (throw (RuntimeException. "id not exist.")))
  ((keyword id) @captchas))

(defn check?
  "check captcha"
  [id captcha]
  (= captcha (:text (get-captcha (keyword id)))))

(defn refresh-captcha
  "refresh captcha. the ramdom captchas string will change."
  [id]
  (dosync
    (if (contains? @captchas (keyword id)) nil (throw (RuntimeException. "id not exist.")))
    (alter captchas assoc (keyword id) (random-captcha))))

(defn delete-captcha
  "delete captcha from captchas ref."
  [id]
  (dosync
    (if (contains? @captchas (keyword id)) nil (throw (RuntimeException. "id not exist.")))
    (alter captchas dissoc (keyword id))))

(defn get-img
  "return a BufferedImage objcet."
  [id]
  (let 
    [img (BufferedImage. 68 22 BufferedImage/TYPE_INT_RGB)
     g (. img getGraphics)
     c (Color. 200 150 255)
     index (atom 0)
     captcha (:text (get-captcha id))]
    (. g setColor c)
    (. g fillRect 0 0 68 22)
    (for [char captcha]
      (let
        [font (Font. "Arial" Font/BOLD 22)
         red (. r nextInt 88)
         green (. r nextInt 188)
         blue (. r nextInt 255)
         char-color (Color. red green blue)]
        (. g setColor char-color)
        (. g setFont font)
        (. g drawString (str char) (+ 3 (* 15 @index)) 18)
        (reset! index (inc @index))
        img))))

(defn timeout?
  [captcha max-time]
  (>= (- (System/currentTimeMillis) (captcha :create-time)) max-time))

(defn timeout-captcha-keys
           [captchas max-time]
           (let [ids (keys captchas)
                 timeout-item-fn? (fn [id] (timeout? (id captchas) max-time))]
             (filter timeout-item-fn? ids)))

(defn dissoc-all
  [captchas keys]
  (reduce dissoc captchas keys))

;remove timeout captchas
(defn remove-timeout-itmes
  [max-time]
  (dosync
    (alter captchas dissoc-all (timeout-captcha-keys @captchas max-time))))

(defn- loop-check-timeout
  []
  (while true
    (remove-timeout-itmes max-hold-time)
    (Thread/sleep 1000)))

;start a thread to check timeout items
(.start (Thread. (fn [] (loop-check-timeout))))