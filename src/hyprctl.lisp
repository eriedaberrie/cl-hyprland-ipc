(in-package #:hyprland-ipc)

(defun %hyprctl (request)
  (with-local-stream-socket (hyprctl-socket *hyprctl-socket*)
    (sb-bsd-sockets:socket-send hyprctl-socket request NIL)
    (babel:octets-to-string
     (let ((full-buffer (make-array 0
                                    :element-type '(unsigned-byte 8)
                                    :fill-pointer 0
                                    :adjustable T))
           (response-buffer (make-array 8192 :element-type '(unsigned-byte 8))))
       (loop :for response-length := (nth-value 1
                                                (sb-bsd-sockets:socket-receive
                                                 hyprctl-socket
                                                 response-buffer
                                                 NIL))
             :do (dotimes (i response-length)
                   (vector-push-extend (aref response-buffer i)
                                       full-buffer
                                       response-length))
             :while (= response-length (length response-buffer)))
       full-buffer))))

(defun hyprctl (request &optional jsonp)
  "Send REQUEST to hyprctl and return the response, or the parsed object if JSONP is non-NIL."
  (funcall (if jsonp #'com.inuoe.jzon:parse #'identity)
           (%hyprctl (concatenate 'string (if jsonp "j/" "/") request))))

(defun hyprctl-batch (requests)
  "Pass every request in REQUESTS to hyrpctl in a batch, which is more efficient than if done individually.

Return the response, which is probably not very useful."
  (%hyprctl (format NIL "[[BATCH]]~{~A~^;~}" (alexandria:ensure-list requests))))
