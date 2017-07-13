(ns morcl.core
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.set :as clset]
            [clojure.string :as str]))

; odict слишком большой, соберём множество слов из датасета и профильтруем исходный словарь
(def word-set
  (set (map #(-> %
                 (str/lower-case)
                 (str/replace #"ё" "е"))
            (filter not-empty
                    (-> (slurp "data/dataset.txt")
                        (str/split #"[\n !?.,]"))))))

(defn clean-dict []
  (with-open [reader (io/reader "data/odict.csv")
              writer (io/writer "data/odict.min.csv")]
    (->> (csv/read-csv reader)
         (filter #(not-empty (clset/intersection word-set (set %))))
         (map (fn[x](map #(str/replace % #"ё" "е") x)))
         (csv/write-csv writer))))

(def dict
  (do
    (if-not (.exists (io/as-file "data/odict.min.csv"))
      (clean-dict))
    (with-open [reader (io/reader "data/odict.min.csv")]
      (doall
        (csv/read-csv reader)))))


(defn -read []
  (-> (slurp "data/dataset.txt")
      (str/split #"\n")
      ))

(defn -write [coll]
  (print
    ; (spit "data/output.txt"
    (str/join "\n" coll)))

(defn -words-only [line]
  (into [(first line)]
        (subvec line 2)))

(defn -find-word [word]
  ; Hidden Markov Model on opencorpora
  (first
    (filter (fn[x](some
                    #(= (-> word
                            (str/lower-case)
                            (str/replace #"ё" "е"))
                        (str/lower-case %))
                    (-words-only x)))
            dict)))

(defn -init-word [word-line]
  (first word-line))

(defn -pos-convert [code]
  (case code
    "с" "S"
    "со" "S"
    "м" "S"
    "ж" "S"
    "мо" "S"
    "жо" "S"
    "п" "A"
    "союз" "CONJ"
    "предл." "PR"
    "мс-п" "PR"
    "межд." "ADV"
    "част." "ADV"
    "вводн." "ADV"
    "предик." "ADV"
    "н" "ADV"
    "сравн." "ADV"
    "св-нсв" "V"
    "св" "V"
    "нсв" "V"
    "NI"
    ))

(defn -pos [word-line]
  (-pos-convert (second word-line)))

(defn -process-word [word]
  (let [word-line (-find-word word)]
    (str word
         "{"
         (or (-init-word word-line) word)
         "="
         (-pos word-line)
         "}")))

(defn -process-line [string]
  (apply str
         (interpose " "
                    (map -process-word
                         (filter not-empty
                                 (str/split string
                                            #"[!?., ]"))))))

(defn -process [coll]
  (map -process-line coll))

(defn -main [& args]
  (-> (-read)
      (-process)
      (-write)))
