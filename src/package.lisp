
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
                #:get-internal-run-time
                #:float
                #:/ #:- #:setf
                #:internal-time-units-per-second
                #:gethash
                #:list #:lambda)
  (:export #:configure ; Reexported from metric-impl
           #:*error-handler* ; Reexported from metric-impl
           #:measure
           #:count
           #:time
           #:defmetric))
