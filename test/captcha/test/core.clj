(ns captcha.test.core
  (:use [captcha.core]
        [midje.sweet :only [fact =>]])
  (:use [clojure.test]))

(fact
  (count (add-captcha "zzwu")) => 1)

(fact
  (add-captcha "guan")
  (add-captcha "cao")
  (nil? (get-img "guan")) => false)


(fact
  (let
    [c (:text (:ming (add-captcha "ming")))]
    (check? "ming" c) => true))