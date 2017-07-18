(ns rna-transcription)

(defn rna-map [nucleotide] 
	(case nucleotide
		\G \C
		\C \G
		\T \A
		\A \U
		false))

(defn to-rna [string] 
	(let [rna (map rna-map string)] 
		(if (= 0 (count (filter #(= false % ) rna)))
		(clojure.string/join "" rna)
		(throw (AssertionError. "Not DNA")))))