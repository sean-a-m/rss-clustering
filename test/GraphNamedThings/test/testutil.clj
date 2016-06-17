;Utilities for testing.  Should probably be moved into test directory.


(ns GraphNamedThings.test.testutil)


(defn write-doc [doc-rec]
  (spit "test/doc" (prn doc-rec)))

(defn read-doc [file]
  (read-string (slurp file)))
