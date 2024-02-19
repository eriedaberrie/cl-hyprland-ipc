(in-package #:hyprland-ipc)

(defvar *hyprctl-socket* nil)
(defvar *events-socket* nil)

(defun init-socket-locations ()
  (when-let (signature (uiop:getenv "HYPRLAND_INSTANCE_SIGNATURE"))
    (let ((hypr-directory (make-pathname :directory (list :absolute "tmp" "hypr" signature))))
      (setf *hyprctl-socket* (namestring (merge-pathnames ".socket.sock" hypr-directory)))
      (setf *events-socket* (namestring (merge-pathnames ".socket2.sock" hypr-directory))))))

(uiop:register-image-restore-hook #'init-socket-locations
                                  (not (and *hyprctl-socket* *events-socket*)))

(defmacro with-local-stream-socket ((socket address) &body body)
  `(let ((,socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
     (sb-bsd-sockets:socket-connect ,socket ,address)
     (unwind-protect
          (progn ,@body)
       (sb-bsd-sockets:socket-close ,socket))))
