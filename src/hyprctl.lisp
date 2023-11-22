(in-package #:hyprland-ipc)

(defvar *hyprctl-retry-send-count* 3
  "The number of times hyprctl commands should retry sending to the socket.")

(defun %hyprctl (request)
  (prog ((tries-left *hyprctl-retry-send-count*))
   :start
     (with-local-stream-socket (hyprctl-socket *hyprctl-socket*)
       (handler-case
           (sb-bsd-sockets:socket-send hyprctl-socket request NIL)
         (sb-bsd-sockets:socket-error (e)
           (if (>= (decf tries-left) 0)
               (go :start)
               (error e))))
       (return
         (loop :with full-buffer := (make-array 0
                                                :element-type '(unsigned-byte 8)
                                                :fill-pointer 0
                                                :adjustable T)
               :with response-buffer := (make-array 8192
                                                    :element-type '(unsigned-byte 8))
               :for response-length := (nth-value 1
                                                  (sb-bsd-sockets:socket-receive
                                                   hyprctl-socket
                                                   response-buffer
                                                   NIL))
               :do (dotimes (i response-length)
                     (vector-push-extend (aref response-buffer i)
                                         full-buffer
                                         response-length))
               :when (< response-length (length response-buffer))
                 :return (babel:octets-to-string full-buffer))))))

(defun hyprctl (request &optional jsonp)
  "Send REQUEST to hyprctl and return the response, or the parsed object if JSONP is non-NIL."
  (funcall (if jsonp #'com.inuoe.jzon:parse #'identity)
           (%hyprctl (concatenate 'string (if jsonp "j/" "/") request))))

(defun hyprctl-batch (requests)
  "Pass every request in REQUESTS to hyrpctl in a batch, which is more efficient than if done individually.

Return the response, which is probably not very useful."
  (%hyprctl (format NIL "[[BATCH]]~{~A~^;~}" (ensure-list requests))))

(defun find-data-by-id (id default-getter existing-data
                        &optional
                          (predicate (lambda (data)
                                       (= id (gethash "id" data)))))
  (find-if predicate (or existing-data (hyprctl default-getter T))))

(defun ensure-trimmed-hex-address (address)
  "Remove the prefixed \"0x\" from ADDRESS if it exists."
  (or (nth-value 1 (starts-with-subseq "0x" address :return-suffix T))
      address))

(defun find-client-data (address &optional clients-data)
  "Return the data of the client at ADDRESS.

CLIENTS-DATA should be the parsed JSON from a call to \"hyprctl clients\" if provided. If not, it will be fetched automatically."
  (find-data-by-id address
                   "clients"
                   clients-data
                   (lambda (client)
                     (string= (ensure-trimmed-hex-address address)
                              (ensure-trimmed-hex-address (gethash "address" client))))))

(defun find-workspace-data (id &optional workspaces-data)
  "Return the data of the workspace with id ID.

WORKSPACES-DATA should be the parsed JSON from a call to \"hyprctl workspaces\" if provided. If not, it will be fetched automatically"
  (find-data-by-id id "workspaces" workspaces-data))

(defun find-monitor-data (id &optional monitors-data)
  "Return the data of the monitor with id ID.

MONITORS-DATA should be the parsed JSON from a call to \"hyprctl monitors\" if provided. If not, it will be fetched automatically"
  (find-data-by-id id "monitors" monitors-data))
