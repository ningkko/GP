;; gorilla-repl.fileformat = 1

;; @@
(comment
 1. bitmutation keeps doubling plushy-size
  3. lexicase selection cases
  4. lexicase based on category?)

(ns gp.ast
  (:gen-class)
  (:require [gorilla-plot.core :as plot]
   			[clojure-csv.core :refer :all]))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@

(def data-addr "src/training_set_metadata.csv")

;;;;;;;;;
;; input and target

(defn read-column 
  [filename column-index]
  (with-open [reader (io/reader filename)]
    (let [data (csv/read-csv reader)]
      (doall
        (map #(nth % column-index) data)))))


(defn read-row 
  [filename row-index]
  (with-open [reader (io/reader filename)]
      (nth (csv/read-csv reader) row-index)))

(defn to-float 
  [input]
  (let [evaluated-input (read-string input)]
    (if (= clojure.lang.Symbol (type evaluated-input))
      (float 0.0)
      (float evaluated-input))))



(defn get-input
  "gets a small sample of input"
  [filename]
  (let [raw-input (fn[row-num] (drop-last (read-row filename row-num)))
        first-floats (fn[row-num] (take 4 (raw-input row-num)))
       	boolean-input (fn[row-num] (take-last 7 (raw-input row-num)))
        second-floats (fn[row-num] (rest (take-last 7 (raw-input row-num))))
        one-raw (fn[row-num] (flatten (concat (map #(to-float %) (first-floats row-num))
                              (list (boolean (boolean-input row-num)))
                              (map #(to-float %) (second-floats row-num)))))]
    (rest (map one-raw (range (count (read-column filename 0)))))))
  

  
(defn get-target
  "gets all target from the file"
  [file-name]
  (doall
    (map #(float (read-string %))
         (rest (read-column file-name 11)))))


(defn get-sample-input
  "gets a small sample of input"
  [filename size]
  (let [raw-input (fn[row-num] (drop-last (read-row filename row-num)))
        first-floats (fn[row-num] (take 4 (raw-input row-num)))
       	boolean-input (fn[row-num] (take-last 7 (raw-input row-num)))
        second-floats (fn[row-num] (rest (take-last 7 (raw-input row-num))))
        one-raw (fn[row-num] (flatten (concat (map #(to-float %) (first-floats row-num))
                              (list (boolean (boolean-input row-num)))
                              (map #(to-float %) (second-floats row-num)))))]
    (rest(map one-raw (range size)))))


(defn get-sample-target
  "gets a small sample of target"
  [file-name size]
  (doall
    (map to-float
         (rest (take (inc size) (read-column file-name 11))))))

(defn write-data
  "writes data into a given directory"
  [out-file data]
  (if (.exists (io/as-file out-file))
     (spit out-file(vec (concat (read-string (slurp out-file)) data)))
    (spit out-file data)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.ast/write-data</span>","value":"#'gp.ast/write-data"}
;; <=

;; @@
(def input (get-sample-input data-addr 100))

(def target (get-sample-target data-addr 100))

(def target-type (apply vector (distinct target)))

;;target-type

;
;Input has 11 columns, with the first column as obk=ject id, and the last column as target.
;So we have 10 actual inputs
;
;;(count (first input))
;;input

(def target-types (rest(distinct (read-column data-addr 11))))
(def target-type-nums (count target-types))
target-types
target-type-nums


;; target types after taking modulo
(def packed-target (map #(float %)
                              (map #(cond
                                      (= % (float 92)) 1
                                      (= % (float 88)) 2
                                      (= % (float 42)) 3
                                      (= % (float 90)) 4
                                      (= % (float 65)) 5
                                      (= % (float 16)) 6
                                      (= % (float 67)) 7
                                      (= % (float 95)) 8
                                      (= % (float 62)) 9
                                      (= % (float 15)) 10
                                      (= % (float 52)) 11
                                      (= % (float 6)) 12
                                      (= % (float 64)) 13
                                      (= % (float 53)) 14) target)))

target
packed-target

input
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>((615.0 349.04605 -61.943836 320.79654 true 1.0 0.0 0.0 0.0 0.0 0.017) (713.0 53.085938 -27.784405 223.52551 true 1.0 1.8181 1.6267 0.2552 45.4063 0.007) (730.0 33.57422 -6.579593 170.45558 true 1.0 0.232 0.2262 0.0157 40.2561 0.021) (745.0 0.189873 -45.586655 328.25446 true 1.0 0.3037 0.2813 1.1523 40.7951 0.007) (1124.0 352.71127 -63.823658 316.9223 true 1.0 0.1934 0.2415 0.0176 40.4166 0.024) (1227.0 35.683594 -5.379379 171.99295 true 1.0 0.0 0.0 0.0 0.0 0.02) (1598.0 347.8467 -64.76086 318.92984 true 1.0 0.1352 0.182 0.0304 39.7279 0.019) (1632.0 348.5959 -63.07262 320.0233 true 1.0 0.6857 0.7014 0.01 43.1524 0.021) (1920.0 149.41406 3.433834 234.91913 true 1.0 0.3088 0.3229 0.336 41.1401 0.027) (1926.0 149.41406 1.940072 236.56537 true 1.0 0.0 0.0 0.0 0.0 0.018) (2072.0 0.965665 -46.37508 325.84592 true 1.0 0.1516 0.19 0.0104 39.8317 0.007) (2103.0 346.5 -62.3204 321.95114 true 1.0 0.1695 0.5409 0.2283 42.4667 0.02) (2300.0 359.44672 -44.20153 331.73 true 1.0 0.236 2.7474 0.5335 46.7959 0.01) (2330.0 359.8052 -46.76848 327.136 true 1.0 0.4541 0.5736 0.2827 42.6207 0.011) (2624.0 346.65518 -63.260487 320.9522 true 1.0 0.0 0.0 0.0 0.0 0.019) (2677.0 53.964844 -28.63099 225.14294 true 1.0 0.0 0.0 0.0 0.0 0.009) (2922.0 352.39865 -62.69666 318.01743 true 1.0 0.1539 0.1469 0.0094 39.2171 0.02) (3041.0 346.13013 -63.07262 321.4231 true 1.0 0.1069 0.1274 0.0198 38.88 0.02) (3285.0 150.82031 1.64151 237.9945 true 1.0 0.161 0.1818 0.0079 39.7258 0.02) (3423.0 349.6154 -63.636005 318.92725 true 1.0 1.9876 1.1213 0.1591 44.4078 0.018) (3489.0 150.11719 2.836105 236.12473 true 1.0 1.133 1.4377 0.2168 45.0753 0.016) (3910.0 0.58952 -47.161343 325.3859 true 1.0 0.1969 2.6766 0.5926 46.7274 0.009) (4088.0 0.965665 -46.37508 325.84592 true 1.0 0.4833 0.4644 0.0321 42.0691 0.007) (4132.0 359.8117 -45.191612 329.4857 true 1.0 0.0561 0.0556 0.0301 36.975 0.01) (4171.0 2.097458 -45.783966 324.73785 true 1.0 0.0 0.0 0.0 0.0 0.011) (4173.0 152.05078 3.284369 237.15738 true 1.0 0.5149 0.5512 0.0221 42.5158 0.019) (4220.0 358.64807 -46.37508 329.46265 true 1.0 0.1197 0.1322 0.3351 38.9679 0.009) (4389.0 151.69922 3.583322 236.53322 true 1.0 0.2333 0.2205 0.9667 40.1939 0.016) (4595.0 349.6154 -63.636005 318.92725 true 1.0 0.5919 0.5995 0.0127 42.737 0.018) (4819.0 35.33203 -5.979157 172.28673 true 1.0 0.3053 0.287 0.0076 40.8445 0.022) (5527.0 347.86185 -61.943836 321.5191 true 1.0 0.1315 0.2487 0.8604 40.4896 0.017) (6180.0 33.222656 -4.780192 167.51566 true 1.0 0.3201 0.2685 0.5211 40.6793 0.018) (6266.0 0.929752 -44.597992 328.53143 true 1.0 0.0 0.0 0.0 0.0 0.011) (6762.0 348.5959 -63.07262 320.0233 true 1.0 0.3863 0.3983 0.0132 41.6735 0.021) (6947.0 34.277344 -5.67919 170.31493 true 1.0 0.568 0.5667 0.0181 42.5888 0.02) (7033.0 52.20703 -28.29155 224.20853 true 1.0 0.0826 0.085 0.0073 37.9414 0.007) (7164.0 347.86185 -61.943836 321.5191 true 1.0 0.4299 0.4245 0.0288 41.8371 0.017) (7315.0 2.07113 -45.191612 325.60623 true 1.0 0.133 0.1337 0.0171 38.9942 0.011) (7409.0 352.39865 -62.69666 318.01743 true 1.0 3.4451 0.5176 1.2609 42.3516 0.02) (7566.0 359.44672 -44.20153 331.73 true 1.0 0.0 0.0 0.0 0.0 0.01) (7698.0 347.01343 -62.508568 321.47205 true 1.0 0.2628 0.1876 0.0216 39.8011 0.018) (7703.0 53.085938 -28.122234 224.1009 true 1.0 0.083 0.082 0.2257 37.8568 0.007) (7756.0 149.41406 2.238686 236.23976 true 1.0 0.0 0.0 0.0 0.0 0.017) (8328.0 1.694561 -45.191612 326.27856 true 1.0 0.3779 0.4808 0.297 42.1592 0.011) (8688.0 32.695312 -4.929937 166.86847 true 1.0 0.0 0.0 0.0 0.0 0.018) (8745.0 349.96622 -62.69666 319.543 true 1.0 0.6276 0.6136 0.0129 42.7983 0.021) (8784.0 34.101562 -5.829153 170.24776 true 1.0 0.0 0.0 0.0 0.0 0.019) (9006.0 34.277344 -5.079716 169.52684 true 1.0 0.0 0.0 0.0 0.0 0.019) (9172.0 346.65518 -63.260487 320.9522 true 1.0 0.0 0.0 0.0 0.0 0.019) (9184.0 0.949367 -45.586655 326.99155 true 1.0 1.4031 1.2719 0.4971 44.7463 0.013) (9203.0 51.85547 -27.953188 223.54361 true 1.0 0.2138 0.1111 0.0626 38.5591 0.008) (9543.0 352.13287 -63.636005 317.42416 true 1.0 0.0 0.0 0.0 0.0 0.021) (9936.0 32.871094 -4.780192 166.95949 true 1.0 0.1633 0.0719 0.0389 37.558 0.017) (9985.0 150.82031 3.732834 235.66632 true 1.0 0.0 0.0 0.0 0.0 0.016) (10321.0 358.3125 -44.99388 332.1858 true 1.0 1.0833 1.1162 0.102 44.3954 0.009) (10337.0 54.66797 -27.615883 223.61078 true 1.0 0.683 0.6725 0.0089 43.0404 0.009) (10349.0 34.98047 -6.279288 172.18007 true 1.0 0.0 0.0 0.0 0.0 0.023) (10478.0 52.910156 -27.953188 223.77408 true 1.0 0.5552 0.2233 0.2002 40.2248 0.007) (10586.0 358.63635 -46.76848 328.89014 true 1.0 0.6052 0.6017 0.0153 42.7467 0.008) (10757.0 52.910156 -26.276812 220.92615 true 1.0 0.1699 0.1711 0.0185 39.5801 0.008) (10796.0 52.910156 -25.94448 220.36635 true 1.0 0.0 0.0 0.0 0.0 0.01) (10798.0 351.3 -62.3204 319.0386 true 1.0 0.1778 0.1872 0.0121 39.7959 0.018) (11165.0 150.9961 2.985506 236.64796 true 1.0 0.0 0.0 0.0 0.0 0.02) (11359.0 349.96622 -62.69666 319.543 true 1.0 0.1529 0.1415 0.0072 39.1281 0.021) (11507.0 53.085938 -28.122234 224.1009 true 1.0 0.3312 0.5095 0.0718 42.3102 0.007) (11770.0 346.13013 -63.07262 321.4231 true 1.0 0.1415 0.2171 0.435 40.156 0.02) (11773.0 150.64453 3.583322 235.69824 true 1.0 0.2207 0.5279 0.1679 42.4027 0.018) (11931.0 149.58984 3.583322 234.88538 true 1.0 0.0 0.0 0.0 0.0 0.024) (11978.0 358.64807 -46.37508 329.46265 true 1.0 0.492 0.4605 0.0179 42.0472 0.009) (12695.0 51.85547 -28.63099 224.73326 true 1.0 0.0 0.0 0.0 0.0 0.009) (12872.0 347.86185 -61.943836 321.5191 true 1.0 0.0 0.0 0.0 0.0 0.017) (13079.0 151.69922 3.583322 236.53322 true 1.0 0.2019 2.447 1.0434 46.4913 0.016) (13138.0 346.65518 -63.260487 320.9522 true 1.0 0.0756 0.5192 0.2158 42.3596 0.019) (13194.0 53.789062 -27.784405 223.6857 true 1.0 0.5195 0.5624 0.2843 42.5685 0.009) (13459.0 150.11719 2.836105 236.12473 true 1.0 0.3495 0.3449 0.6556 41.3068 0.016) (13482.0 33.75 -4.630479 168.14624 true 1.0 0.2929 0.3115 0.0205 41.0501 0.019) (13504.0 1.363636 -46.76848 324.66934 true 1.0 0.4469 0.3816 0.0766 41.5643 0.008) (14080.0 150.9961 4.181528 235.29198 true 1.0 0.0 0.0 0.0 0.0 0.015) (14156.0 53.085938 -27.11186 222.3843 true 1.0 0.0 0.0 0.0 0.0 0.007) (14279.0 54.66797 -27.615883 223.61078 true 1.0 0.3434 0.5728 0.4518 42.6167 0.009) (14398.0 2.07113 -45.191612 325.60623 true 1.0 0.2812 0.2634 1.0581 40.631 0.011) (14539.0 150.64453 3.583322 235.69824 true 1.0 0.2882 0.2359 0.0434 40.359 0.018) (14553.0 359.8052 -46.76848 327.136 true 1.0 1.1897 1.1667 0.1717 44.5143 0.011) (14601.0 32.695312 -4.929937 166.86847 true 1.0 0.3837 0.3653 0.2005 41.4527 0.018) (14674.0 33.75 -4.630479 168.14624 true 1.0 0.2012 0.0567 0.4176 37.0171 0.019) (14983.0 349.6154 -63.636005 318.92725 true 1.0 0.3391 0.3238 0.0255 41.1476 0.018) (15002.0 349.04605 -61.943836 320.79654 true 1.0 0.3409 0.3512 0.0531 41.353 0.017) (15251.0 32.871094 -4.780192 166.95949 true 1.0 0.4653 2.327 0.6097 46.3585 0.017) (15475.0 351.38297 -64.01124 317.57407 true 1.0 0.0 0.0 0.0 0.0 0.023) (15626.0 346.13013 -63.07262 321.4231 true 1.0 0.0 0.0 0.0 0.0 0.02) (15674.0 0.965665 -46.37508 325.84592 true 1.0 0.2927 0.2727 0.3286 40.7172 0.007) (15700.0 359.4156 -46.76848 327.7299 true 1.0 0.0 0.0 0.0 0.0 0.009) (15718.0 51.85547 -27.953188 223.54361 true 1.0 0.1193 2.3179 0.7672 46.3482 0.008) (15845.0 53.789062 -27.784405 223.6857 true 1.0 0.3174 0.3471 0.8216 41.3232 0.009) (15968.0 149.41406 2.238686 236.23976 true 1.0 0.3509 0.4729 0.4544 42.1164 0.017) (16339.0 51.328125 -27.447618 222.53505 true 1.0 0.0 0.0 0.0 0.0 0.013) (16349.0 150.82031 3.134927 236.34135 true 1.0 0.0 0.0 0.0 0.0 0.016) (16463.0 151.69922 3.583322 236.53322 true 1.0 0.2023 0.1805 0.0254 39.7082 0.016) (16496.0 359.4156 -46.76848 327.7299 true 1.0 0.3391 0.3895 0.2635 41.6162 0.009))</span>","value":"((615.0 349.04605 -61.943836 320.79654 true 1.0 0.0 0.0 0.0 0.0 0.017) (713.0 53.085938 -27.784405 223.52551 true 1.0 1.8181 1.6267 0.2552 45.4063 0.007) (730.0 33.57422 -6.579593 170.45558 true 1.0 0.232 0.2262 0.0157 40.2561 0.021) (745.0 0.189873 -45.586655 328.25446 true 1.0 0.3037 0.2813 1.1523 40.7951 0.007) (1124.0 352.71127 -63.823658 316.9223 true 1.0 0.1934 0.2415 0.0176 40.4166 0.024) (1227.0 35.683594 -5.379379 171.99295 true 1.0 0.0 0.0 0.0 0.0 0.02) (1598.0 347.8467 -64.76086 318.92984 true 1.0 0.1352 0.182 0.0304 39.7279 0.019) (1632.0 348.5959 -63.07262 320.0233 true 1.0 0.6857 0.7014 0.01 43.1524 0.021) (1920.0 149.41406 3.433834 234.91913 true 1.0 0.3088 0.3229 0.336 41.1401 0.027) (1926.0 149.41406 1.940072 236.56537 true 1.0 0.0 0.0 0.0 0.0 0.018) (2072.0 0.965665 -46.37508 325.84592 true 1.0 0.1516 0.19 0.0104 39.8317 0.007) (2103.0 346.5 -62.3204 321.95114 true 1.0 0.1695 0.5409 0.2283 42.4667 0.02) (2300.0 359.44672 -44.20153 331.73 true 1.0 0.236 2.7474 0.5335 46.7959 0.01) (2330.0 359.8052 -46.76848 327.136 true 1.0 0.4541 0.5736 0.2827 42.6207 0.011) (2624.0 346.65518 -63.260487 320.9522 true 1.0 0.0 0.0 0.0 0.0 0.019) (2677.0 53.964844 -28.63099 225.14294 true 1.0 0.0 0.0 0.0 0.0 0.009) (2922.0 352.39865 -62.69666 318.01743 true 1.0 0.1539 0.1469 0.0094 39.2171 0.02) (3041.0 346.13013 -63.07262 321.4231 true 1.0 0.1069 0.1274 0.0198 38.88 0.02) (3285.0 150.82031 1.64151 237.9945 true 1.0 0.161 0.1818 0.0079 39.7258 0.02) (3423.0 349.6154 -63.636005 318.92725 true 1.0 1.9876 1.1213 0.1591 44.4078 0.018) (3489.0 150.11719 2.836105 236.12473 true 1.0 1.133 1.4377 0.2168 45.0753 0.016) (3910.0 0.58952 -47.161343 325.3859 true 1.0 0.1969 2.6766 0.5926 46.7274 0.009) (4088.0 0.965665 -46.37508 325.84592 true 1.0 0.4833 0.4644 0.0321 42.0691 0.007) (4132.0 359.8117 -45.191612 329.4857 true 1.0 0.0561 0.0556 0.0301 36.975 0.01) (4171.0 2.097458 -45.783966 324.73785 true 1.0 0.0 0.0 0.0 0.0 0.011) (4173.0 152.05078 3.284369 237.15738 true 1.0 0.5149 0.5512 0.0221 42.5158 0.019) (4220.0 358.64807 -46.37508 329.46265 true 1.0 0.1197 0.1322 0.3351 38.9679 0.009) (4389.0 151.69922 3.583322 236.53322 true 1.0 0.2333 0.2205 0.9667 40.1939 0.016) (4595.0 349.6154 -63.636005 318.92725 true 1.0 0.5919 0.5995 0.0127 42.737 0.018) (4819.0 35.33203 -5.979157 172.28673 true 1.0 0.3053 0.287 0.0076 40.8445 0.022) (5527.0 347.86185 -61.943836 321.5191 true 1.0 0.1315 0.2487 0.8604 40.4896 0.017) (6180.0 33.222656 -4.780192 167.51566 true 1.0 0.3201 0.2685 0.5211 40.6793 0.018) (6266.0 0.929752 -44.597992 328.53143 true 1.0 0.0 0.0 0.0 0.0 0.011) (6762.0 348.5959 -63.07262 320.0233 true 1.0 0.3863 0.3983 0.0132 41.6735 0.021) (6947.0 34.277344 -5.67919 170.31493 true 1.0 0.568 0.5667 0.0181 42.5888 0.02) (7033.0 52.20703 -28.29155 224.20853 true 1.0 0.0826 0.085 0.0073 37.9414 0.007) (7164.0 347.86185 -61.943836 321.5191 true 1.0 0.4299 0.4245 0.0288 41.8371 0.017) (7315.0 2.07113 -45.191612 325.60623 true 1.0 0.133 0.1337 0.0171 38.9942 0.011) (7409.0 352.39865 -62.69666 318.01743 true 1.0 3.4451 0.5176 1.2609 42.3516 0.02) (7566.0 359.44672 -44.20153 331.73 true 1.0 0.0 0.0 0.0 0.0 0.01) (7698.0 347.01343 -62.508568 321.47205 true 1.0 0.2628 0.1876 0.0216 39.8011 0.018) (7703.0 53.085938 -28.122234 224.1009 true 1.0 0.083 0.082 0.2257 37.8568 0.007) (7756.0 149.41406 2.238686 236.23976 true 1.0 0.0 0.0 0.0 0.0 0.017) (8328.0 1.694561 -45.191612 326.27856 true 1.0 0.3779 0.4808 0.297 42.1592 0.011) (8688.0 32.695312 -4.929937 166.86847 true 1.0 0.0 0.0 0.0 0.0 0.018) (8745.0 349.96622 -62.69666 319.543 true 1.0 0.6276 0.6136 0.0129 42.7983 0.021) (8784.0 34.101562 -5.829153 170.24776 true 1.0 0.0 0.0 0.0 0.0 0.019) (9006.0 34.277344 -5.079716 169.52684 true 1.0 0.0 0.0 0.0 0.0 0.019) (9172.0 346.65518 -63.260487 320.9522 true 1.0 0.0 0.0 0.0 0.0 0.019) (9184.0 0.949367 -45.586655 326.99155 true 1.0 1.4031 1.2719 0.4971 44.7463 0.013) (9203.0 51.85547 -27.953188 223.54361 true 1.0 0.2138 0.1111 0.0626 38.5591 0.008) (9543.0 352.13287 -63.636005 317.42416 true 1.0 0.0 0.0 0.0 0.0 0.021) (9936.0 32.871094 -4.780192 166.95949 true 1.0 0.1633 0.0719 0.0389 37.558 0.017) (9985.0 150.82031 3.732834 235.66632 true 1.0 0.0 0.0 0.0 0.0 0.016) (10321.0 358.3125 -44.99388 332.1858 true 1.0 1.0833 1.1162 0.102 44.3954 0.009) (10337.0 54.66797 -27.615883 223.61078 true 1.0 0.683 0.6725 0.0089 43.0404 0.009) (10349.0 34.98047 -6.279288 172.18007 true 1.0 0.0 0.0 0.0 0.0 0.023) (10478.0 52.910156 -27.953188 223.77408 true 1.0 0.5552 0.2233 0.2002 40.2248 0.007) (10586.0 358.63635 -46.76848 328.89014 true 1.0 0.6052 0.6017 0.0153 42.7467 0.008) (10757.0 52.910156 -26.276812 220.92615 true 1.0 0.1699 0.1711 0.0185 39.5801 0.008) (10796.0 52.910156 -25.94448 220.36635 true 1.0 0.0 0.0 0.0 0.0 0.01) (10798.0 351.3 -62.3204 319.0386 true 1.0 0.1778 0.1872 0.0121 39.7959 0.018) (11165.0 150.9961 2.985506 236.64796 true 1.0 0.0 0.0 0.0 0.0 0.02) (11359.0 349.96622 -62.69666 319.543 true 1.0 0.1529 0.1415 0.0072 39.1281 0.021) (11507.0 53.085938 -28.122234 224.1009 true 1.0 0.3312 0.5095 0.0718 42.3102 0.007) (11770.0 346.13013 -63.07262 321.4231 true 1.0 0.1415 0.2171 0.435 40.156 0.02) (11773.0 150.64453 3.583322 235.69824 true 1.0 0.2207 0.5279 0.1679 42.4027 0.018) (11931.0 149.58984 3.583322 234.88538 true 1.0 0.0 0.0 0.0 0.0 0.024) (11978.0 358.64807 -46.37508 329.46265 true 1.0 0.492 0.4605 0.0179 42.0472 0.009) (12695.0 51.85547 -28.63099 224.73326 true 1.0 0.0 0.0 0.0 0.0 0.009) (12872.0 347.86185 -61.943836 321.5191 true 1.0 0.0 0.0 0.0 0.0 0.017) (13079.0 151.69922 3.583322 236.53322 true 1.0 0.2019 2.447 1.0434 46.4913 0.016) (13138.0 346.65518 -63.260487 320.9522 true 1.0 0.0756 0.5192 0.2158 42.3596 0.019) (13194.0 53.789062 -27.784405 223.6857 true 1.0 0.5195 0.5624 0.2843 42.5685 0.009) (13459.0 150.11719 2.836105 236.12473 true 1.0 0.3495 0.3449 0.6556 41.3068 0.016) (13482.0 33.75 -4.630479 168.14624 true 1.0 0.2929 0.3115 0.0205 41.0501 0.019) (13504.0 1.363636 -46.76848 324.66934 true 1.0 0.4469 0.3816 0.0766 41.5643 0.008) (14080.0 150.9961 4.181528 235.29198 true 1.0 0.0 0.0 0.0 0.0 0.015) (14156.0 53.085938 -27.11186 222.3843 true 1.0 0.0 0.0 0.0 0.0 0.007) (14279.0 54.66797 -27.615883 223.61078 true 1.0 0.3434 0.5728 0.4518 42.6167 0.009) (14398.0 2.07113 -45.191612 325.60623 true 1.0 0.2812 0.2634 1.0581 40.631 0.011) (14539.0 150.64453 3.583322 235.69824 true 1.0 0.2882 0.2359 0.0434 40.359 0.018) (14553.0 359.8052 -46.76848 327.136 true 1.0 1.1897 1.1667 0.1717 44.5143 0.011) (14601.0 32.695312 -4.929937 166.86847 true 1.0 0.3837 0.3653 0.2005 41.4527 0.018) (14674.0 33.75 -4.630479 168.14624 true 1.0 0.2012 0.0567 0.4176 37.0171 0.019) (14983.0 349.6154 -63.636005 318.92725 true 1.0 0.3391 0.3238 0.0255 41.1476 0.018) (15002.0 349.04605 -61.943836 320.79654 true 1.0 0.3409 0.3512 0.0531 41.353 0.017) (15251.0 32.871094 -4.780192 166.95949 true 1.0 0.4653 2.327 0.6097 46.3585 0.017) (15475.0 351.38297 -64.01124 317.57407 true 1.0 0.0 0.0 0.0 0.0 0.023) (15626.0 346.13013 -63.07262 321.4231 true 1.0 0.0 0.0 0.0 0.0 0.02) (15674.0 0.965665 -46.37508 325.84592 true 1.0 0.2927 0.2727 0.3286 40.7172 0.007) (15700.0 359.4156 -46.76848 327.7299 true 1.0 0.0 0.0 0.0 0.0 0.009) (15718.0 51.85547 -27.953188 223.54361 true 1.0 0.1193 2.3179 0.7672 46.3482 0.008) (15845.0 53.789062 -27.784405 223.6857 true 1.0 0.3174 0.3471 0.8216 41.3232 0.009) (15968.0 149.41406 2.238686 236.23976 true 1.0 0.3509 0.4729 0.4544 42.1164 0.017) (16339.0 51.328125 -27.447618 222.53505 true 1.0 0.0 0.0 0.0 0.0 0.013) (16349.0 150.82031 3.134927 236.34135 true 1.0 0.0 0.0 0.0 0.0 0.016) (16463.0 151.69922 3.583322 236.53322 true 1.0 0.2023 0.1805 0.0254 39.7082 0.016) (16496.0 359.4156 -46.76848 327.7299 true 1.0 0.3391 0.3895 0.2635 41.6162 0.009))"}
;; <=

;; @@
; Instructions must all be either functions that take one Push state and return another
; or constant literals.
; TMH: ERCs?
(def default-instructions
  (list
	'in1
    'exec_dup
    'exec_if
    'boolean_and
    'boolean_or
    'boolean_not
    'boolean_=
    'close
    true
    false
  ;;============== new functions ================
   
    'pi
    'e
    'float_negative
    'float_positive
    'float_absolute
    'float_sqrt
    'float_cbrt
    'float_+
    'float_-
    'float_*
    'float_%
    'float_=
   	'float_log
   	'float_sin
   	'float_cos
   	'float_tan
   	'float_sinh
   	'float_cosh
   	'float_tanh
   	'float_ceil
   	'float_floor
   	'float_round
   	'random-coefficient
   	
   ))


(def example-push-state
  {:exec '()
   :float '(1.0 2.0 3.0 4.0 5.0 6.0 7.0)
   :input {:in1 4}})


(def opens ; number of blocks opened by instructions (default = 0)
  {'exec_dup 1 
   'exec_if 2})


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.ast/opens</span>","value":"#'gp.ast/opens"}
;; <=

;; @@
;;;;;;;;;
;; Utilities


(def empty-push-state
  {:exec '()
   :float '()
   :boolean '()
   :input {}})


(defn abs
  "Absolute value."
  [x]
  (if (neg? x)
    (- x)
    x))

(defn not-lazy
  "Returns lst if it is not a list, or a non-lazy version of lst if it is."
  [lst]
  (if (seq? lst)
    (apply list lst)
    lst))

(defn lazy-contains? [col key]
  "does a lazy-seq contain something?"
  (some #{key} col))

(defn square 
  "square fucntion"
  [n]
  (* n n))

(defn exp 
  "x^(10^n)"
  [x n]
  (* x (reduce * (repeat n 10))))


(defn push-to-stack
  "Pushes item onto stack in state"
  [state stack item]
  (update state stack conj item))


(defn pop-stack
  "Removes top item of stack."
  [state stack]
  (update state stack rest))


(defn peek-stack
  "Returns top item on a stack."
  [state stack]
  (if (empty? (get state stack))
    :no-stack-item
    (first (get state stack))))


(defn empty-stack?
  "Returns true if the stack is empty."
  [state stack]
  (empty? (get state stack)))


(defn get-args-from-stacks
  "Takes a state and a list of stacks to take args from. If there are enough args
  on each of the desired stacks, returns a map of the form {:state :args}, where
  :state is the new state and :args is a list of args from the stacks. If there
  aren't enough args on the stacks, returns :not-enough-args."
  [state stacks]
  (loop [state state
         stacks (reverse stacks)
         args '()]
    (if (empty? stacks)
      {:state state :args args}
      (let [stack (first stacks)]
        (if (empty-stack? state stack)
          :not-enough-args
          (recur (pop-stack state stack)
                 (rest stacks)
                 (conj args (peek-stack state stack))))))))


(defn make-push-instruction
  "A utility function for making Push instructions. Takes a state, the function
  to apply to the args, the stacks to take the args from, and the stack to return
  the result to. Applies the function to the args (taken from the stacks) and pushes
  the return value onto return-stack."
  [state function arg-stacks return-stack]
  (let [args-pop-result (get-args-from-stacks state arg-stacks)]
    (if (= args-pop-result :not-enough-args)
      state
      (let [args (:args args-pop-result)
            result (apply function args)
            new-state (:state args-pop-result)]
        (push-to-stack new-state return-stack result)))))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.ast/make-push-instruction</span>","value":"#'gp.ast/make-push-instruction"}
;; <=

;; @@


;;;;;;;;;
;; Instructions

(defn in1
  "Pushes the input labeled :in1 on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:in1 (:input state))))


(def random-coefficient (exp (rand) (rand-int 2)))

(defn float_+
  [state]
  (make-push-instruction state +' [:float :float] :float))

(defn float_-
  [state]
  (make-push-instruction state -' [:float :float] :float))

(defn float_*
  [state]
  (make-push-instruction state *' [:float :float] :float))


(defn float_%
  [state]
  (make-push-instruction state
                         (fn [f1 f2]
                           (if (or (nil? f1)) (nil? f2))
                           (float 0.0)
                           (if (zero? f2)
                             f1
                             (quot f1 f2)))
                         [:float :float]
                         :float))

(defn float_=
  [state]
  (make-push-instruction state = [:float :float] :boolean))


(defn exec_dup
  [state]
  (if (empty-stack? state :exec)
    state
    (push-to-stack state :exec (first (:exec state)))))

(defn exec_if
  [state]
  (make-push-instruction state
                         #(if %1 %3 %2)
                         [:boolean :exec :exec]
                         :exec))


(defn boolean_and
  [state]
  (make-push-instruction state #(and %1 %2) [:boolean :boolean] :boolean))

(defn boolean_or
  [state]
  (make-push-instruction state #(or %1 %2) [:boolean :boolean] :boolean))

(defn boolean_not
  [state]
  (make-push-instruction state not [:boolean] :boolean))

(defn boolean_=
  [state]
  (make-push-instruction state = [:boolean :boolean] :boolean))



(def pi Math/PI)

(def e Math/E)

(defn float_=
  [state]
  (make-push-instruction state = [:float :float] :boolean))



(defn float_absolute
  [state]
  (make-push-instruction state
                         #(float(Math/abs %))
                         [:float]
                         :float))

(defn float_negative
  [state]
  (make-push-instruction state
                         #(neg? %)
                         [:float]
                         :boolean))


(defn float_positive
  [state]
  (make-push-instruction state
                         #(pos? %)
                         [:float]
                         :boolean))

(defn float_sqrt
  [state]
  (make-push-instruction state
                         #(do
                            (float (Math/sqrt (float %))))
                         [:float]
                         :float))

(defn float_cbrt
  [state]
  (make-push-instruction state
                         #(float (Math/cbrt (float %)))
                         [:float]
                         [:float]))


(defn float_log
  [state]
  (make-push-instruction state
                         #(float (Math/log %))
                         [:float]
                         [:float]))


(defn float_cos
  [state]
  (make-push-instruction state
                         #(float (Math/cos %))
                         [:float]
                         [:float]))


(defn float_sin
  [state]
  (make-push-instruction state
                         #(float (Math/sin %))
                         [:float]
                         [:float]))



(defn float_tan
  [state]
  (make-push-instruction state
                         #(float (Math/tan %))
                         [:float]
                         [:float]))



(defn float_sinh
  [state]
  (make-push-instruction state
                         #(float (Math/sinh %))
                         [:float]
                         [:float]))



(defn float_cosh
  [state]
  (make-push-instruction state
                         #(float (Math/cosh %))
                         [:float]
                         [:float]))



(defn float_tanh
  [state]
  (make-push-instruction state
                         #(float (Math/tanh %))
                         [:float]
                         [:float]))




(defn float_ceil
  [state]
  (make-push-instruction state
                         #(float (Math/ceil %))
                         [:float]
                         [:float]))


(defn float_floor
  [state]
  (make-push-instruction state
                         #(float (Math/floor %))
                         [:float]
                         [:float]))



(defn float_round
  [state]
  (make-push-instruction state
                         #(float (Math/round %))
                         [:float]
                         [:float]))




(defn float_mod
  "see what category the predicted output falls in"
  [state]
  (make-push-instruction state
                         #(float (mod %1 15))
                         [:float]
                         [:float]))









;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.ast/float_mod</span>","value":"#'gp.ast/float_mod"}
;; <=

;; @@

;;;;;;;;;
;; Interpreter

(defn interpret-one-step
  "Takes a Push state and executes the next instruction on the exec stack."
  [state]
  (let [popped-state (pop-stack state :exec)
        first-raw (first (:exec state))
        first-instruction (if (symbol? first-raw) 
                            (eval first-raw)
                            first-raw)]
    (cond
      (fn? first-instruction) 
      (first-instruction popped-state)
      ;
      (float? first-instruction) 
      (push-to-stack popped-state :float first-instruction)
      ;
      (seq? first-instruction)
      (update popped-state :exec #(concat %2 %1) first-instruction)
      ;
      (or (= first-instruction true) (= first-instruction false))
      (push-to-stack popped-state :boolean first-instruction)
      ;
      :else 
      (throw (Exception. (str "Unrecognized Push instruction in program: " 
                              first-instruction))))))

(defn interpret-program
  "Runs the given problem starting with the stacks in start-state."
  [program start-state step-limit]
  (loop [state (assoc start-state :exec program :step 1)]
    (if (or (empty? (:exec state))
            (> (:step state) step-limit))
      state
      (recur (update (interpret-one-step state) :step inc)))))

(defn push-from-plushy
  "Returns the Push program expressed by the given plushy representation."
  [plushy]
  (let [opener? #(and (vector? %) (= (first %) 'open))] ;; [open <n>] marks opens
    (loop [push () ;; iteratively build the Push program from the plushy
           plushy (mapcat #(if-let [n (get opens %)] [% ['open n]] [%]) plushy)]
      (if (empty? plushy)       ;; maybe we're done?
        (if (some opener? push) ;; done with plushy, but unclosed open
          (recur push '(close)) ;; recur with one more close
          push)                 ;; otherwise, really done, return push
        (let [i (first plushy)]
          (if (= i 'close) 
            (if (some opener? push) ;; process a close when there's an open
              (recur (let [post-open (reverse (take-while (comp not opener?)
                                                          (reverse push)))
                           open-index (- (count push) (count post-open) 1)
                           num-open (second (nth push open-index))
                           pre-open (take open-index push)]
                       (if (= 1 num-open)
                         (concat pre-open [post-open])
                         (concat pre-open [post-open ['open (dec num-open)]])))
                     (rest plushy))
              (recur push (rest plushy))) ;; unmatched close, ignore
            (recur (concat push [i]) (rest plushy)))))))) ;; anything else

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.ast/push-from-plushy</span>","value":"#'gp.ast/push-from-plushy"}
;; <=

;; @@
;;;;;;;;;
;; GP

(defn make-random-plushy
  "Creates and returns a new plushy."
  [instructions max-initial-plushy-size]
  (repeatedly (rand-int max-initial-plushy-size)
              #(rand-nth instructions)))


(defn tournament-selection
  "Selects an individual from the population using a tournament."
  [pop argmap]
  (let [tournament-size (:tournament-size argmap)
        tournament-set (take tournament-size (shuffle pop))]
    (apply min-key :total-error tournament-set)))


(defn lexicase-selection
  "Selects an individual from the population using lexicase selection."
  [pop argmap]
  (loop [survivors pop
         cases (shuffle (range (count (:errors (first pop)))))]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (rand-nth survivors)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map :errors survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases))))))



(defn select-parent
  "Selects a parent from the population using the specified method."
  [pop argmap]
  (case (:parent-selection argmap)
    :tournament (tournament-selection pop argmap)
    :lexicase (lexicase-selection pop argmap)))


(defn uniform-crossover
  "Crosses over two individuals using uniform crossover. Pads shorter one."
  [plushy-a plushy-b]
  (let [shorter (min-key count plushy-a plushy-b)
        longer (if (= shorter plushy-a)
                 plushy-b
                 plushy-a)
        length-diff (- (count longer) (count shorter))
        shorter-padded (concat shorter (repeat length-diff :crossover-padding))]
    (remove #(= % :crossover-padding)
            (map #(if (< (rand) 0.5) %1 %2)
                 shorter-padded
                 longer))))



(defn multipoint-crossover
  "Multi point crossover is a generalization of the one-point crossover wherein alternating segments are swapped to get new off-springs...
  take odd genomes, uniform sized
  a-1+b-2+a-3+b-4+...+a_left+b_left"

  [plushy-a plushy-b]
  (let [shorter (min-key count plushy-a plushy-b)
        longer (if (= shorter plushy-a)
                 plushy-b
                 plushy-a)
        length (count longer) ;;length of genes
        ;;at least 2 chunks'
        chunk-number (+ 2 (rand-int (dec length)))
        chunk-size (int (/ length chunk-number))
        length-diff (- (count longer) (count shorter))
        shorter-padded (concat shorter (repeat length-diff :crossover-padding))
        segmented-a (map vec (partition-all chunk-size plushy-a))
        segmented-b (map vec (partition-all chunk-size plushy-b))]  

    (loop [use-a (rand-nth [true false])
           a segmented-a
           b segmented-b
           result []]

      (if (empty? a)
        (remove #(= % :crossover-padding) result)
        (recur (not use-a)
               (rest a)
               (rest b)
               (concat result (if use-a
                                (first a) 
                                (first b)))))))) 



(defn multipoint-parallel
  "a1-b1-a3-b3-... or a-2-b2-a4-b4-..."
  [plushy-a plushy-b]
  (let [shorter (min-key count plushy-a plushy-b)
        longer (if (= shorter plushy-a)
                 plushy-b
                 plushy-a)
        length (count longer) ;;length of genes
        chunk-number (+ 1 (rand-int length))
        chunk-size (int (/ length chunk-number))
        length-diff (- (count longer) (count shorter))
        shorter-padded (concat shorter (repeat length-diff :crossover-padding))
        segmented-a (map vec (partition-all chunk-size plushy-a))
        segmented-b (map vec (partition-all chunk-size plushy-b))]

    (loop [start-at-0th (rand-nth [true false])
           a (if start-at-0th
               segmented-a
               (rest segmented-a))
           b (if start-at-0th
               segmented-b
               (rest segmented-b))
           result []]

      (if (empty? a)
        (remove #(= % :crossover-padding) result)
        (recur start-at-0th
               (rest (rest a))
               (rest (rest b))
               (concat result (first a) (first b))))))) 


(defn crossover
  [plushy-a plushy-b argmap]
  (case (:crossover argmap)
    :uniform-crossover (uniform-crossover plushy-a plushy-b)
    :multipoint-crossover (multipoint-crossover plushy-a plushy-b)
    :multipoint-parallel (multipoint-parallel plushy-a plushy-b)))

(defn bit-mutation
  "see definition above. Mutation rate [0 1)"
  [plushy mutation-rate instructions]
  (loop [p plushy
         result []]

    (if (empty? p)
      result
      (recur (rest p)
             (conj result 
                     (if (<= (rand) mutation-rate)
                       (rand-nth instructions)
                       (first p)))))))

(defn uniform-addition
  "Randomly adds new instructions before every instruction (and at the end of
  the plushy) with some probability."
  [plushy instructions mutation-rate]
  (let [rand-code (repeatedly (inc (count plushy))
                              (fn []
                                (if (< (rand) mutation-rate)
                                  (rand-nth instructions)
                                  :mutation-padding)))]
    (remove #(= % :mutation-padding)
            (interleave (conj plushy :mutation-padding)
                        rand-code))))

(defn uniform-deletion
  "Randomly deletes instructions from plushy at some rate."
  [plushy mutation-rate]
  (remove (fn [x] (< (rand) mutation-rate))
          plushy))

(defn new-individual
  "Returns a new individual produced by selection and variation of
  individuals in the population."
  [pop argmap]
  {:plushy
   (let [prob (rand)]
     (cond
       (< prob 0.5) (let[crossed-plushy (crossover 
                                         (:plushy (select-parent pop argmap)) 
                                         (:plushy (select-parent pop argmap))
                                         argmap)]
                      (if (:bit-mutation argmap)
                        (bit-mutation crossed-plushy (:mutation-rate argmap) (:instructions argmap))
                        crossed-plushy))
       
       (< prob 0.75) (uniform-addition (:plushy (select-parent pop argmap))
                                       (:instructions argmap)
                                       (:mutation-rate argmap))
       :else (uniform-deletion (:plushy (select-parent pop argmap))
                               (:mutation-rate argmap))))})



;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.ast/new-individual</span>","value":"#'gp.ast/new-individual"}
;; <=

;; @@

(defn absolute-badness
  "calculates the absolute difference between a predicted value and a target value"
  [correct-output output]
  (abs (- correct-output output))
)

(defn modulo-badness
  "Take modulo of the predicted outputs"
  [correct-output output]
  (if (= (float (mod (Math/round output) 15)) correct-output)
    0
    1))

(defn regression-error-function
  "Finds the behaviors and errors of the individual."
  [argmap past-total-errors individual]
  (let [program (push-from-plushy (:plushy individual))
        badness-function (:badness-function argmap)
        inputs input
        correct-outputs (cond
                          (= badness-function absolute-badness) target
                          (= badness-function modulo-badness) packed-target)
        outputs (map (fn [input]
                       (peek-stack
                        (interpret-program 
                         program
                         (assoc empty-push-state :input {:in1 input})
                         (:step-limit argmap))
                        :float))
                     inputs)

        errors (map (fn [correct-output output]
                      (cond
                        (= output :no-stack-item) (float 2000000)
                        
                        ;; if the answer does not contain  in1
                        (not (= nil (some #{:in1} (:plushy individual)))) 
                        (float 2000000)

                        :else
                        (badness-function correct-output output)))
                    correct-outputs
                    outputs)]
    
    (assoc individual
           :behaviors outputs
           :errors errors
           :total-error (let [total-err (apply +' errors)]
                          (cond
                            (Double/isNaN total-err) (float 1000000)
                            ; (= (rand-nth errors) (/ total-err (count errors))) (float 2000000)
                            (not (= nil 
                                        (some #{total-err} past-total-errors))) (float 3000000)
                            (= (rand-nth outputs) (rand-nth outputs)) (float 2000000)
                            :else total-err)))))


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.ast/regression-error-function</span>","value":"#'gp.ast/regression-error-function"}
;; <=

;; @@
;;------------------------------------------------------------------------------------------
(defn report
  "Reports information each generation."
  [pop generation filename]
  (let [best (first pop)]
    (println "-------------------------------------------------------")
    (println "               Report for Generation" generation)
    (println "-------------------------------------------------------")
    (print "Best plushy: ") (prn (:plushy best))
    (print "Best program: ") (prn (push-from-plushy (:plushy best)))
    (println "Best total error:" (:total-error best))
    ;(write-data (vector (:total-error best)) filename)
    (println "Best errors:" (:errors best))
    (println "Best behaviors:" (:behaviors best))
    (println)))

(defn propel-gp
  "Main GP loop."
  [{:keys [population-size max-generations error-function instructions 
           max-initial-plushy-size]
    :as argmap}]
  (println "Starting GP with args:" argmap)
  (loop [generation 0
         population (repeatedly
                     population-size
                     #(hash-map :plushy
                                (make-random-plushy instructions
                                                    max-initial-plushy-size)))
         past-total-errors []];; also check to prevent same total-error all the time

    (let [evaluated-pop (sort-by :total-error 

                                 (map (partial error-function argmap past-total-errors)
                                      population))]

      (report evaluated-pop generation (:out-file argmap))
      (cond


        (zero? (:total-error (first evaluated-pop))) (println "SUCCESS")
        (>= generation max-generations) nil
        :else (recur (inc generation)
                     (repeatedly population-size #(new-individual evaluated-pop argmap))
                     (conj  past-total-errors (:total-error (first evaluated-pop))))))))

(defn -main
  "Runs propel-gp, giving it a map of arguments."
  [& args]
  (binding [*ns* (the-ns 'gp.ast)]
    (propel-gp (update-in (merge {:instructions default-instructions
                                  :badness-function modulo-badness
                                  :error-function regression-error-function
                                  :max-generations 200
                                  :population-size 200
                                  :max-initial-plushy-size 40
                                  :step-limit 10
                                  :parent-selection :lexicase
                                  :tournament-size 8
                                  :mutation-rate 0.06
                                  :crossover :uniform-crossover
                                  :bit-mutation false
                                  :out-file "error/lexicase.csv"}
                                 (apply hash-map
                                        (map read-string args)))
                          [:error-function]
                          #(if (fn? %) % (eval %))))))


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gp.ast/-main</span>","value":"#'gp.ast/-main"}
;; <=

;; @@
;;(-main)
;; @@

;; @@

;; @@
