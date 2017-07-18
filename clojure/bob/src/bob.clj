(ns bob)

(defn every [fn msg]
	( and 
		(every? fn msg) 
		(not (empty? msg))
	)
)

(defn response-for [msg] 
	(cond 
		(clojure.string/blank? msg) "Fine. Be that way!"
		(every 
			#(Character/isUpperCase %) 
			(filter #(Character/isLetter %) msg)
		) "Whoa, chill out!"
		(= \? (last (clojure.string/trim msg))) "Sure."
		:else "Whatever."
	)
)
