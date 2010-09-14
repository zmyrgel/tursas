(defn supported-options
  "Prints the supported options for Tursas."
  []
  (map #(str "option name " % "\n")
         '("Hash type spin default 1 min 1 max 32"
           "UCI_EngineAbout type string default Tursas by Timo Myyrä, see www.wickedbsd.net/projects/tursas")))

