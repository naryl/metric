
(defpackage #:metric-impl
  (:use :cl
        :alexandria
        :usocket)
  (:export #:configure
           #:*error-handler*
           #:make-name
           #:*defined-metrics*
           #:measure%
           #:count%))

(defpackage #:metric
  (:use #:metric-impl)
  (:import-from :cl
                #:in-package
                #:defmacro
                #:let #:progn
                #:unwind-protect
                #:*package*
                #:&body
                #:get-universal-time
                #:- #:setf
                #:gethash
                #:list #:lambda)
  (:export #:configure ; Reexported from metric-impl
           #:*error-handler* ; Reexported from metric-impl
           #:measure
           #:count
           #:time
           #:defmetric))
