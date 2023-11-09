(asdf:defsystem cl-hyprland-ipc
  :version "0.1.0"
  :author "eriedaberrie <eriedaberrie@gmail.com>"
  :maintainer "eriedaberrie <eriedaberrie@gmail.com>"
  :license "gpl3"
  :description "Common Lisp bindings for Hyprland IPC"
  :homepage "https://github.com/eriedaberrie/cl-hyprland-ipc"
  :pathname #P"src/"
  :components ((:file "package")
               (:file "util" :depends-on ("package"))
               (:file "hyprctl" :depends-on ("util"))
               (:file "events" :depends-on ("util")))
  :depends-on (#:babel #:com.inuoe.jzon #:sb-bsd-sockets))
