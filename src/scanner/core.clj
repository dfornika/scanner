(ns scanner.core
  (:gen-class))


(defonce db (atom {:had-error false
                   :current-char 0
                   :current-line 0}))


(def token-types #{;; single chars
                   :left-paren
                   :right-paren
                   :left-brace
                   :right-brace
                   :comma
                   :dot
                   :minus
                   :plus
                   :semicolon
                   :slash
                   :star
                   ;; one or two chars
                   :bang
                   :bang-equal
                   :assign
                   :equal
                   :greater
                   :greater-equal
                   :less
                   :less-equal
                   ;; literals
                   :identifier
                   :string
                   :number
                   ;; keywords
                   :and
                   :class
                   :else
                   :false
                   :fun
                   :for
                   :if
                   :nil
                   :or
                   :print
                   :return
                   :super
                   :this
                   :true
                   :var
                   :while
                   :eof
                   })

(defn token-type->lexeme
  ""
  []
  {;; single chars
   :left-paren  "("
   :right-paren ")"
   :left-brace  "{"
   :right-brace "}"
   :comma       ","
   :dot         "."
   :minus       "-"
   :plus        "+"
   :semicolon   ";"
   :slash       "/"
   :star        "*"
   ;; one or two chars
   :bang          "!"
   :bang-equal    "!="
   :assign        "="
   :equal         "=="
   :greater       ">"
   :greater-equal ">="
   :less          "<"
   :less-equal    "<="
   ;; literals
   ;; :identifier    
   ;; :string
   ;; :number
   ;; keywords
   :and           "and"
   :class         "class"
   :else          "else"
   :false         "false"
   :fun           "fun"
   :for           "for"
   :if            "if"
   :nil           "nil"
   :or            "or"
   :print         "print"
   :return        "return"
   :super         "super"
   :this          "this"
   :true          "true"
   :var           "var"
   :while         "while"
   ;; :eof          
   })
                     

(defn report!
  [line where message db]
  (binding [*out* *err*]
    (println (str "[line " line "] Error" where ": " message )))
  (swap! db assoc :had-error true))

(defn error!
  [line message db]
  (report! line "" message db))

(defn scan-token
  [remaining-chars]
  (case (first remaining-chars)
    \( [{:token-type :left-paren  :lexeme "("} (rest remaining-chars)]
    \) [{:token-type :right-paren :lexeme ")"} (rest remaining-chars)]
    \{ [{:token-type :left-brace  :lexeme "{"} (rest remaining-chars)]
    \} [{:token-type :right-brace :lexeme "}"} (rest remaining-chars)]
    \, [{:token-type :comma       :lexeme ","} (rest remaining-chars)]
    \. [{:token-type :dot         :lexeme "."} (rest remaining-chars)]
    \- [{:token-type :minus       :lexeme "-"} (rest remaining-chars)]
    \+ [{:token-type :plus        :lexeme "+"} (rest remaining-chars)]
    \; [{:token-type :semicolon   :lexeme ";"} (rest remaining-chars)]
    \* [{:token-type :star        :lexeme "*"} (rest remaining-chars)]
    \! (if (= (second remaining-chars) \=)
         [{:token-type :bang-equal :lexeme "!="} (rest (rest remaining-chars))]
         [{:token-type :bang :lexeme "!"} (rest remaining-chars)])
    \/ (if (= (second remaining-chars) \/)
         [nil (rest (drop-while #(not= \n) remaining-chars))]
         [{:token-type :slash :lexeme "/"} (rest remaining-chars)])
    [nil (rest remaining-chars)]
  ))

(defn scan-tokens
  [source-input db]
  (loop [partially-tokenized {:tokens []
                              :source source-input}]
    (if (empty? (:source partially-tokenized))
      (conj (:tokens partially-tokenized) {:token-type :eof :lexeme ""})
      (recur (let [[new-token remaining-source] (scan-token (:source partially-tokenized))
                   existing-tokens (:tokens partially-tokenized)]
               (if-some [t new-token]
                 {:tokens (conj existing-tokens t)
                  :source remaining-source}
                 {:tokens existing-tokens
                  :source remaining-source}
                 ))))))

(defn run
  [source db]
  (let [tokens (scan-tokens source db)]
    (doseq [t tokens]
      (println t))))


(defn run-file
  [path db]
  (run (slurp path) db)
  (if (:had-error db)
    (System/exit 65)))

(defn run-prompt
  [db]
  (loop [line ""]
    (print "> ")
    (flush)
    (let [line (read-line)]
      (run line db))
    (recur "")))

(defn -main
  ""
  [& args]
  (cond
    (> (count args) 1) (println "Usage: java -jar scanner.jar [script]")
    (= (count args) 1) (run-file (first args) db)
    :else (run-prompt db)))


(comment
  (swap! db assoc :had-error false)

                     
  )
