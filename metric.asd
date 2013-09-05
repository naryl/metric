
(defsystem metric
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "impl")
               (:file "api"))
  :depends-on (alexandria
               usocket))
