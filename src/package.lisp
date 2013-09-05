
(defpackage #:metric-impl
  (:use :cl
        :alexandria
        :usocket)
  (:export #:configure
           #:*error-handler*
           #:measure%
           #:count%))

(defpackage #:metric
  (:use #:metric-impl)
  (:export #:configure ; Reexported from metric-impl
           #:*error-handler* ; Reexported from metric-impl
           #:measure
           #:count
           #:time
           #:defmetric))
