(ns senden.ssr-test
  "SSR parity: senden.views render to stable HTML via shitsuke.hiccup/->html."
  (:require [clojure.test :refer [deftest is]]
            [shitsuke.hiccup :as hic]
            [senden.ssr :as ssr]
            [senden.views :as views]))

(deftest root-html-stable-test
  (let [html (ssr/root-html)]
    (is (clojure.string/starts-with? html "<!doctype html>"))
    (is (clojure.string/includes? html "Marketing"))
    (is (clojure.string/includes? html "Spring parka launch"))
    (is (clojure.string/includes? html "--shitsuke-colors-"))))

(deftest ssr-parity-test
  (is (= (hic/->html (views/root (ssr/sample-db)))
         (hic/->html (views/root (ssr/sample-db))))))
