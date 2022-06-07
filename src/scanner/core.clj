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
  [current-char remaining-chars]
  (case current-char
    \( {:token-type :left-paren  :lexeme "("}
    \) {:token-type :right-paren :lexeme ")"}
    \{ {:token-type :left-brace  :lexeme "{"}
    \} {:token-type :right-brace :lexeme "}"}
    \, {:token-type :comma       :lexeme ","}
    \. {:token-type :dot         :lexeme "."}
    \- {:token-type :minus       :lexeme "-"}
    \+ {:token-type :plus        :lexeme "+"}
    \; {:token-type :semicolon   :lexeme ";"}
    \* {:token-type :star        :lexeme "*"}
    \! (if (= (first remaining-chars) \=) {:token-type :bang-equal :lexeme "!="} {:token-type :bang :lexeme "!"})
    \/ (if (= (first remaining-chars) \/) nil {:token-type :slash :lexeme "/"})
    nil
  ))

(defn scan-tokens
  [source db]
  (loop [tokens []
         remaining-source source]
    (if (empty? remaining-source)
      (conj tokens {:token-type :eof :lexeme ""})
      (recur (let [current-char (first remaining-source)]
                 (if-some [t (scan-token current-char (rest remaining-source))]
                   (conj tokens t)
                   tokens))
                 (rest remaining-source)))))

(defn run
  [source db]
  (let [tokens (scan-tokens source db)]
    (println tokens)))


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
