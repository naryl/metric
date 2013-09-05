
(in-package metric-impl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-socket ((stream-var host port) &body body)
    (with-gensyms (socket)
      `(let* ((,socket (socket-connect ,host ,port))
              (,stream-var (socket-stream ,socket)))
         (unwind-protect
              (progn ,@body)
           (when ,socket (socket-close ,socket)))))))

(defun new-ht ()
  (make-hash-table :test 'equal :synchronized t))

(defvar *metrics* (new-ht))

(defvar *timer* nil)

(defvar *error-handler* #'invoke-debugger
  "Called when an error happens while submitting data to graphite")

;;;; Reporting to Graphite

(defvar *host* nil)
(defvar *port* nil)

(defun configure (&key (host "localhost") (port 2003) (interval 60))
  "HOST:PORT is Graphite's socket address
  INTERVAL is how often to send metrics
  Returns t if Graphite is available"
  (when (and *timer*
			 (sb-ext:timer-scheduled-p *timer*))
	(sb-ext:unschedule-timer *timer*))
  (setf  *host* host
         *port* port)
  ;; Check that the server is available
  (ignore-errors
    (let ((socket (socket-connect host port :timeout 1)))
      (socket-close socket)
	  (setf *timer* (sb-ext:make-timer #'report :name "Graphite reporter" :thread t))
	  (sb-ext:schedule-timer *timer* interval :repeat-interval interval)
	  t)))

(defgeneric report-metric (name metric stream time))

(defvar *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun universal-to-unix-time (universal-time)
  (- universal-time *unix-epoch-difference*))

(defun get-unix-time ()
  (universal-to-unix-time (get-universal-time)))

(defun report ()
  (unless (and *host* *port*)
    (error "Use METRIC:CONFIGURE to specify server address"))
  (handler-bind ((error *error-handler*))
    (let ((time (get-unix-time))
          (metrics *metrics*))
      (setf *metrics* (new-ht))
      (with-socket (socket *host* *port*)
        (maphash (lambda (k v)
                   (report-metric k v socket time))
                 metrics)
        (maphash (lambda (k v)
                   (declare (ignore k))
                   (report-metric (first v) (second v) socket time))
                 *defined-metrics*)))))

;;;; Collecting metrics

(declaim (inline make-name))
(defun make-name (name package)
  (concatenate 'string (string-downcase (string (package-name package))) "." name))

;;; Metric

(defstruct metric
  (min 0 :type number)
  (max 0 :type number)
  (count 0 :type integer)
  (sum 0 :type number))

(defun measure% (name value package)
  (let ((name (make-name name package)))
    (let ((metric (gethash name *metrics* (make-metric))))
      (setf (metric-min metric)
            (min (metric-min metric) value))
      (setf (metric-max metric)
            (max (metric-max metric) value))
      (incf (metric-count metric))
      (incf (metric-sum metric)
            value)
      (setf (gethash name *metrics*)
            metric))))

(defmethod report-metric (name (metric metric) stream time)
  (format stream "~A.min ~A ~A~%"
          name (metric-min metric) time)
  (format stream "~A.max ~A ~A~%"
          name (metric-max metric) time)
  (format stream "~A.avg ~A ~A~%"
          name (/ (metric-sum metric)
                  (metric-count metric))
          time)
  (format stream "~A.sum ~A ~A~%"
          name (metric-sum metric) time)
  (format stream "~A.count ~A ~A~%"
          name (metric-count metric) time))

;;; Counter

(defstruct counter
  (count 0 :type integer))

(defun count% (name package)
  (let ((name (make-name name package)))
    (let ((counter (gethash name *metrics* (make-counter))))
      (incf (counter-count counter))
      (setf (gethash name *metrics*)
            counter))))

(defmethod report-metric (name (counter counter) stream time)
  (format stream "~A.count ~A ~A~%"
          name (counter-count counter) time))

;;; defmetric

(defvar *defined-metrics* (make-hash-table))

(defmethod report-metric (name (func function) stream time)
  (format stream "~A ~A ~A~%"
          name (funcall func) time))
