
(cl:in-package metric)

(cl:defmacro measure (name value)
  "Stores a cumulative metric named NAME. Results are sent per configured intervals."
  `(metric-impl::measure% ,name ,value ,cl:*package*))

(cl:defmacro count (name)
  "Increments a counter named NAME. Counters are sent and reset per configured intervals"
  `(metric-impl::count% ,name ,cl:*package*))

(cl:defmacro time ((name) cl:&body body)
  "Executes BODY and records its execution time in a metric called NAME"
  (alexandria:with-gensyms (time)
    `(cl:let ((,time (cl:get-universal-time)))
       (cl:unwind-protect
            (cl:progn
              ,@body)
         (metric-impl::report-metric ,name (cl:- (cl:get-universal-time) ,time))))))

(cl:defmacro defmetric (name (metric-name) cl:&body body)
  "Defines a metric to be calculated and sent along with each heap of data once per
  configured interval"
  `(cl:setf (cl:gethash ',name metric-impl::*defined-metrics*)
         (cl:list ,metric-name
               (cl:lambda () ,@body))))
