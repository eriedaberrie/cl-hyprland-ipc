(in-package #:hyprland-ipc)

(defun ensure-trimmed-hex-address (address)
  "Remove the prefixed \"0x\" from ADDRESS if it exists."
  (or (nth-value 1 (starts-with-subseq "0x" address :return-suffix T))
      address))

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
  (%hyprctl (format NIL "[[BATCH]]~{~A~^;~}" (ensure-list requests))))

(defun find-data-by-id (id default-getter existing-data
                        &optional
                          (predicate (lambda (data)
                                       (= id (gethash "id" data)))))
  (find-if predicate (or existing-data (hyprctl default-getter T))))

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
