(ns scanner.core
  (:gen-class))


(defonce db (atom {:had-error false}))


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
  [char]
  (case char
    \( {:token-type :left-paren :lexeme "("}
    
  ))

(defn scan-tokens
  [source]
  (loop [tokens []
         line 0
         remaining-source source]
    (if (empty? remaining-source)
      tokens
      (recur (conj tokens (first remaining-source))
             line
             (apply str (rest remaining-source))))))

(defn run
  [source]
  (println source))


(defn run-file
  [path db]
  (run (slurp path))
  (if (:had-error db)
    (System/exit 65)))

(defn run-prompt
  [db]
  )

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