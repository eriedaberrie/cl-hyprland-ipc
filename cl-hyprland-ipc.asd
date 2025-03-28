(asdf:defsystem "cl-hyprland-ipc"
  :version "0.1.0"
  :author "eriedaberrie <eriedaberrie@gmail.com>"
  :maintainer "eriedaberrie <eriedaberrie@gmail.com>"
  :license "GPLv3-or-later"
  :description "Common Lisp bindings for Hyprland IPC"
  :homepage "https://github.com/eriedaberrie/cl-hyprland-ipc"
  :pathname #P"src/"
  :components ((:file "package")
               (:file "sockets" :depends-on ("package"))
               (:file "hyprctl" :depends-on ("package" "sockets"))
               (:file "events" :depends-on ("package" "sockets")))
  :depends-on (#:alexandria
               #:babel
               #:com.inuoe.jzon
               #:sb-bsd-sockets
               #:split-sequence
               #:uiop))
