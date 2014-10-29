(ns h4ssi.map-relational-mapper-test
  (:require [clojure.test :refer :all]
            [clojure.set :as sets]
            [h4ssi.map-relational-mapper :refer :all]))

(defcollect idrec idrecs id [id])

(defcollect kwrec kwrecs id [id :kw])

(defcollect lower lowers id [id attr])
(defcollect upper uppers id [id attr [lowers]])

(deftest test-collect
  (testing "collecting a single record"
    (is (=
         (collect-idrec [{:idrec_id 1}])
         (map->Idrec {:id 1}))))
  (testing "collecting multiple records"
    (is (=
         (collect-idrecs [{:idrec_id 1} {:idrec_id 2}])
         (map map->Idrec [{:id 1}       {:id 2}]))))
  (testing "extracting keyword"
    (is (=
         (collect-kwrec [{:kwrec_id 1 :kwrec_kw "value"}])
         (map->Kwrec {:id 1 :kw :value}))))
  (testing "collect a single record with subrecs"
    (is (=
         (collect-upper (seq (sets/join [{:upper_id 1 :upper_attr "upper"}] [{:lower_id 1 :lower_attr "lower1"} {:lower_id 2 :lower_attr "lower2"}])))
         (map->Upper
          {:id 1
           :attr "upper"
           :lowers (map map->Lower
                        [{:id 1 :attr "lower1"}
                         {:id 2 :attr "lower2"}])}))))
  (testing "collect multiple records with subrecs"
    (is (=
         (collect-uppers [{:upper_id 1 :upper_attr "upper1" :lower_id 1 :lower_attr "lower1"}
                          {:upper_id 1 :upper_attr "upper1" :lower_id 2 :lower_attr "lower2"}
                          {:upper_id 2 :upper_attr "upper2" :lower_id 3 :lower_attr "lower3"}
                          {:upper_id 2 :upper_attr "upper2" :lower_id 4 :lower_attr "lower4"}])
         (map map->Upper
              [ {:id 1
                 :attr "upper1"
                 :lowers (map map->Lower
                              [{:id 1 :attr "lower1"}
                               {:id 2 :attr "lower2"}])}
                {:id 2
                 :attr "upper2"
                 :lowers (map map->Lower
                              [{:id 3 :attr "lower3"}
                               {:id 4 :attr "lower4"}])}])))))



#_(remove-ns 'h4ssi.map-relational-mapper-test)
#_(run-tests)
