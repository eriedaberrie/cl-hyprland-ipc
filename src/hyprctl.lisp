(in-package #:hyprland-ipc)

(defun %hyprctl (request response)
  (with-local-socket (hyprctl-socket *hyprctl-socket* :stream)
    (sb-bsd-sockets:socket-send hyprctl-socket request NIL)
    (when response
      (babel:octets-to-string
       (let ((full-buffer (make-array 0
                                      :element-type '(unsigned-byte 8)
                                      :fill-pointer 0
                                      :adjustable T)))
         (loop :while (let* ((response-buffer
                               (make-array 8192
                                           :element-type '(unsigned-byte 8)))
                             (response-length
                               (nth-value 1
                                          (sb-bsd-sockets:socket-receive hyprctl-socket
                                                                         response-buffer
                                                                         NIL))))
                        (loop :for i :from 0 :below response-length
                              :do (vector-push-extend (aref response-buffer i)
                                                      full-buffer
                                                      response-length))
                        (= response-length (length response-buffer))))
         full-buffer)))))

(defun hyprctl (request &optional response &aux (json (eq response :json)))
  "Send REQUEST to hyprctl.

Return the response string if RESPONSE is non-NIL. If it is :JSON, instead return the parsed JSON."
  (funcall (if json #'com.inuoe.jzon:parse #'identity)
           (%hyprctl (concatenate 'string (if json "j/" "/") request)
                     (and response T))))

(defun hyprctl-batch (requests &optional response)
  "Pass every request in REQUESTS to hyrpctl in a batch, which is more efficient than if done individually.

Return the response string when RESPONSE is non-NIL, which is probably garbage."
  (%hyprctl (format NIL "[[BATCH]]~{~A~^;~}" (alexandria:ensure-list requests))
            response))
