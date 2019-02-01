(ert-deftest convert-dot-dash-namespace-tests ()
  "convert a clojure namespace to a file system acceptable
version, ie. . -> / and - -> _.  that is dots to forward slashed
and dashes to underscores."
  (should
   (equal
    (convert-dot-dash-namespace ".")
    "/"))
  (should
   (equal
    (convert-dot-dash-namespace ".-")
    "/_"))
  (should
   (equal
    (convert-dot-dash-namespace "abc.one-two")
    "abc/one_two")))

(ert-deftest get-relative-root-tests ()
  "determine the relative path of a file between its namespace
  and the project root"
  (should
   (equal
    (get-relative-root "/home/fenton/projects/rum-sample"
                       "abc/one_two"
                       "/home/fenton/projects/rum-sample/src/clj/abc/one_two.clj"
                       "clj")
    "src/clj")))

(ert-deftest get-src-or-test ()
  "if we are in a src path return the test or vice versa"
  (should (equal (get-src-or-test "src/cljc" "cljc") "test/clj"))
  (should (equal (get-src-or-test "src" "cljc") "test"))
  (should (equal (get-src-or-test "src/cljs") "test/cljs"))
  (should (equal (get-src-or-test "test/cljs") "src/cljs"))
  (should (equal (get-src-or-test "test/clj") "src/clj"))
  )
(ert-deftest get-test-or-impl-file-tests ()
  "get the test file given the source file and vice versa"
  (should
   (equal
    (get-test-or-impl-file
     "my-proj.core"
     "/home/fenton/projects/rum-sample/src/clj/my_proj/core.clj"
     "/home/fenton/projects/rum-sample")
    "/home/fenton/projects/rum-sample/test/clj/my_proj/core_test.clj"
    ))
  (should
   (equal
    (get-test-or-impl-file
     "my-proj.core"
     "/home/fenton/projects/rum-sample/test/clj/my_proj/core_test.clj"
     "/home/fenton/projects/rum-sample")
    "/home/fenton/projects/rum-sample/src/clj/my_proj/core.clj"
    ))

  )
