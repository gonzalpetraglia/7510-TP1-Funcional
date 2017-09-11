(ns logical-interpreter
  (:require [clojure.string :as str]))

(import '(java.io BufferedReader StringReader))

(defn remove-whitespaces
   "Removes the whitespaces of a string, no matter where they are."
   [line]
   (str/replace line #" +" ""))

(defn fact-tokenizer
     "Returns a list of the words in a fact."
     [string]
     (filter seq (str/split string #"[\.,)( :-]")))


(defn rule-identifier
     "Returns the name of the rule and the number of parameters."
     [string]
     (re-find #"\w+" string))


(defmulti facts-dispatcher fact-tokenizer)

(defmethod facts-dispatcher :default [query] false)


(defmulti rule-dispatcher rule-identifier)

(defmethod rule-dispatcher :default [query] false)


(def title-regex
  #"\w+\((?:\w+,)*\w+\)")


(def rule-construction-regex
  #"^\w+\((\w+,)*\w+\):-(\w+\((\w+,)*\w+\),)*\w+\((\w+,)*\w+\).$")


(def fact-regex
  #"^\w+\((\w+,)*\w+\)\.$")


(def query-regex
  #"^\w+\((?:\w+,)*\w+\)$")

(defn is-valid-fact?
  "Decides if a given line is a valid fact."
  [line]
  (re-matches fact-regex line))



(defn is-valid-query?
  "Decides if the given line has a valid format."
  [line]
  (re-matches query-regex (remove-whitespaces line)))


(defn is-valid-rule?
  "Decides if a given line could be a rule construction."
        [line]
  (re-matches rule-construction-regex line))


(defn is-valid-line? 
  "Decides if a given line is valid to be in a database."
  [line]  
  (or (is-valid-fact? line) (is-valid-rule? line)))

(defn strip-delimiters
  "Takes away possible delimmiters in a string."
  [s]
  (str/replace s #",|\)" ""))

(defn get-parameters
  "Get parameters of a query-type line."
  [line]  
  (map strip-delimiters (re-seq #"\w+[,\)]" line)))
  

(defn get-parameters-rule-construction
  "Get the parameters in a rule construction."
  [rule]  
  (get-parameters  (re-find title-regex rule)))

(defn replacer
  "Returns the value of the parameter for this specific instance."
  [rule-construction instance [full-match parameter delimiter]]
  (str ((into {} (map vector 
                  (get-parameters-rule-construction rule-construction) 
                  (get-parameters instance))) parameter) delimiter))



(defn replace-parameters
  "Replace the parameters in a rule construction by the ones in the instance."
  [rule-construction instance]
  (str/replace rule-construction #"(\w+)(,|\))"  (partial replacer rule-construction instance)))


(defn separate-facts
  "Returns the facts that must be true for the rule to be true." 
  [rule]
  (rest (re-seq title-regex rule)))



(defn facts-extractor
  "Return the facts that must be true for a rule to be true."
  [rule-construction instance]
  (separate-facts (replace-parameters rule-construction instance))) 
  


(defn rule-checker
  "Returns a function which checks if the query is implied by the rule."
  [rule query]
  (every? facts-dispatcher (facts-extractor rule query)))
  

(defn interpret-line
  "Reads a single line of the data base."
  [line]
  (if (is-valid-line? line)
    (if (is-valid-fact? line)
      (defmethod facts-dispatcher (fact-tokenizer line) [query] true) 
      (defmethod rule-dispatcher (rule-identifier line) [query] (rule-checker line query))) 
    :invalid))
  

(defn read-db-recursively
  "Read the data-base (as an array of lines) using recursion."
  [lines]
  (if (not (empty? lines))
    (if (= (interpret-line (first lines)) :invalid)
      :invalid      
      (read-db-recursively (rest lines)))))
        
      
    

(defn separate-lines
   "Takes a string, gives back the same string as an array of separated lines."
   [database]
   (filter seq (line-seq (BufferedReader. (StringReader. database)))))



(defn read-db
   "Read the data-base,generating the dispatch function"
   [database]
   (read-db-recursively (separate-lines (remove-whitespaces database))))


(defn query-is-true? 
   "Returns true if the rules and facts in database imply query, false if not.Precondition, database must have been read before, the query is valid."
   [query]
   (or (facts-dispatcher query) (rule-dispatcher query)))


(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"
  [database query]
  (if (or (= (read-db database) :invalid) (not (is-valid-query? query))) 
    nil 
    (query-is-true? query)))  







                                                                                                                                        
