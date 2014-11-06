(ns captcha.core
  (:import [java.awt.image BufferedImage]
           [java.awt Color Font Graphics]
           [java.util Random]
           [javax.imageio ImageIO]
           [java.io ByteArrayOutputStream]))

(def r (Random.))

(defn gen-random-code
  "gen a random code.
   dictionary: char dictionary
   length: code length"
  [dictionary length]
  (let [dictionary-len (count dictionary)]
    (->>
     (repeatedly length
                 (fn [] (get dictionary
                            (.nextInt r dictionary-len) "0")))
     (apply str))))

(defn gen-buffered-image
  "return a BufferedImage objcet."
  [code]
  (let [img (BufferedImage. 68 22 BufferedImage/TYPE_INT_RGB)
        g (. img getGraphics)
        c (Color. 200 150 255)
        _ (. g setColor c)
        _ (. g fillRect 0 0 68 22)]
    (doseq [[index char] (map-indexed (fn [i v] [i v]) code)]
      (let [font (Font. "Arial" Font/BOLD 22)
            red (. r nextInt 88)
            green (. r nextInt 188)
            blue (. r nextInt 255)
            char-color (Color. red green blue)]
        (. g setColor char-color)
        (. g setFont font)
        (. g drawString (str char) (+ 3 (* 15 index)) 18)))
    img))

(defn image->byte-array
  [img]
  (let [out (ByteArrayOutputStream.)]
    (do
      (ImageIO/write img "png" out)
      (.toByteArray out))))

(defprotocol captcha-checker
  (get-img [this id] "gen a image, and return byte arrat of it.")
  (check [this id code] "check code."))

(defrecord CaptchaManager [captchas dictionary length]
  captcha-checker
  (get-img [this id]
    (let [code (gen-random-code dictionary length)]
      (do
        (swap! captchas assoc id code)
        (image->byte-array (gen-buffered-image code)))))
  (check [this id code]
    (= (get id @captchas)
       code)))

;;TODO Add timeout function
(defn make-captcha-manager
  "reuturn a captcha magager.
   valid options are:
   :dictionary
   :length
   :timeout"
  [& options]
  (let [captchas (atom {})
        {:keys [dictionary length]
         :or {dictionary (mapv str (range 10))
              length 4}} options
        manager (CaptchaManager. captchas dictionary length)]
    manager))
