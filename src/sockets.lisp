(in-package #:hyprland-ipc)

(defvar *hyprctl-socket* nil)
(defvar *events-socket* nil)

(defun init-socket-locations ()
  (a:when-let ((signature (uiop:getenv "HYPRLAND_INSTANCE_SIGNATURE")))
    (macrolet ((hypr-socket-for (var file)
                 `(setf ,var
                        (namestring (uiop:xdg-runtime-dir "hypr" signature ,file)))))
      (hypr-socket-for *hyprctl-socket* ".socket.sock")
      (hypr-socket-for *events-socket* ".socket2.sock"))))

(uiop:register-image-restore-hook #'init-socket-locations)

(defmacro with-local-stream-socket ((socket address) &body body)
  `(let ((,socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
     (sb-bsd-sockets:socket-connect ,socket ,address)
     (unwind-protect
          (progn ,@body)
       (sb-bsd-sockets:socket-close ,socket))))
