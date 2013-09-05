
(defsystem metric
  :pathname "src/"
  :components ((:file "package")
               (:file "impl")
               (:file "api"))
  :depends-on (alexandria
               usocket))
