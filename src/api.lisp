
(in-package metric)

(defmacro measure (name value)
  "Stores a cumulative metric named NAME. Results are sent per configured intervals."
  `(metric-impl:measure% ,name ,value ,*package*))

(defmacro count (name)
  "Increments a counter named NAME. Counters are sent and reset per configured intervals"
  `(metric-impl:count% ,name ,*package*))

(defmacro time ((name) &body body)
  "Executes BODY and records its execution time in a metric called NAME"
  (alexandria:with-gensyms (time)
    `(let ((,time (get-internal-run-time)))
       (unwind-protect
            (progn
              ,@body)
         (metric-impl:measure% ,name (float (/ (- (get-internal-run-time) ,time)
                                               internal-time-units-per-second))
                               ,*package*)))))

(defmacro defmetric (name (metric-name) &body body)
  "Defines a metric to be calculated and sent along with each heap of data once per
  configured interval"
  `(setf (gethash ',name metric-impl:*defined-metrics*)
            (list (metric-impl:make-name ,metric-name ,*package*)
                  (lambda () ,@body))))
