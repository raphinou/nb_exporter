; remarks or questions that popped up during development:
; first, second, and then nth 2
; zip/children returns a map, that is not zips
; defmulti well used?
; read userinput without echo?
; (sql(read)) has problems if valuen read is a path with slashes like /tmp/blup




(ns com.raphinou.nb_exporter (:require [clojure.zip :as zip] [clojure.xml :as xml]  ))
(use 'clojure.contrib.str-utils)
(import '(del.icio.us Delicious))


(defn- startparse-tagsoup [s ch]
  (let [p (org.ccil.cowan.tagsoup.Parser.)]
      (.setFeature p "http://www.ccil.org/~cowan/tagsoup/features/default-attributes" false)
      (.setFeature p "http://www.ccil.org/~cowan/tagsoup/features/cdata-elements" true)
      (.setFeature p "http://xml.org/sax/features/namespace-prefixes" true)  
      (.setContentHandler p ch)
      (.parse p s))) 
 

 (defn- container [z]
 "get to div containing list of links"
 (-> z zip/down zip/right zip/down zip/right zip/right zip/right )
 )

(defn- has-title? [note]
(= (:class (:attrs (first (:content (first (:content note)))))) "AttributeTitle")
)

(defn- has-labels? [n]
(>=(count (:content n )) 2))

(defmulti title has-title?)
(defmethod title true [n]
  (first (:content (first (:content (first (:content n)))))))

(defmethod title false [n] (str "Google notebook import without title"))

(defn- extract-labels-from-string [s]
 (if (nil? s) '() (re-split #", *" (re-gsub #"\n" "" s))))

(defmulti url has-title?) 

(defmethod url true [n]
(:href (:attrs (nth (:content (first (:content n))) 2))))

(defmethod url false [n]
  (:href (:attrs (second (:content (first (:content n)))))))


(defmulti labels has-title?) 

(defmethod labels true [n]
    (if (has-labels? n) (extract-labels-from-string 
                 (try (second (:content   (nth (:content n) 2)))
                 (catch java.lang.IndexOutOfBoundsException e (first (:content (first  (:content (second (:content   (nth (:content n ) 1))))))))
                 
                 )
                 ) '() ))

(defmethod labels false [n]
    (if (has-labels? n) (extract-labels-from-string (second (:content (second (:content n))))) '() ))
(defn- note-with-title-title [n]
  (first (:content (first (:content (first (:content n)))))))





(defn- notes [z]
    (filter #(and (= (:tag %)  :div) (= (:class (:attrs % )) "PubNote")) (zip/children (container z))))
  

(defn- user-input [s]
    (println s)
    (str (read))
    )


(defn run []
(let [dz (zip/xml-zip (xml/parse  (user-input "enter html file result of google notebook export between double quotes:")  startparse-tagsoup) )
      delicious (Delicious. (user-input "login?") (user-input "password?") "https://api.del.icio.us/" )
      notes-list (notes dz)
      ]

(loop [ notes  notes-list]
    (let [n (first notes)]
      (println "Adding  " (title n) " with url " (url n) " and label " (labels n))
      (.addPost delicious (url n) (title n) "Google notebook export" (str-join " " (conj  (labels n) "google-notebook-import")) (java.util.Date.) false false)
    )
    (. Thread (sleep 1100))
    (if (= 1 (count notes)) nil  (recur (rest notes)))

      )
))

(run)


