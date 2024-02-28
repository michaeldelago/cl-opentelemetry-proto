(defpackage opentelemetry-common-lisp/tests/main
  (:use :cl
        :opentelemetry-common-lisp
        :rove))
(in-package :opentelemetry-common-lisp/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :opentelemetry-common-lisp)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
