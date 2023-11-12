(in-package #:hyprland-ipc)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *events-spec* '((:workspace "workspace" 1)
                          (:focused-monitor "focusedmon" 2)
                          (:active-window NIL 3)
                          (:fullscreen "fullscreen" 1)
                          (:monitor-removed "monitorremoved" 1)
                          (:monitor-added "monitoradded" 1)
                          (:create-workspace "createworkspace" 1)
                          (:destroy-workspace "destroyworkspace" 1)
                          (:move-workspace "moveworkspace" 2)
                          (:rename-workspace "renameworkspace" 2)
                          (:active-special "activespecial" 2)
                          (:active-layout "activelayout" 2)
                          (:open-window "openwindow" 4)
                          (:close-window "closewindow" 1)
                          (:move-window "movewindow" 2)
                          (:open-layer "openlayer" 1)
                          (:close-layer "closelayer" 1)
                          (:submap "submap" 1)
                          (:change-floating-mode "changefloatingmode" 2)
                          (:urgent "urgent" 1)
                          (:minimize "minimize" 2)
                          (:screencast "screencast" 2)
                          (:window-title "windowtitle" 1)
                          (:ignore-group-lock "ignoregrouplock" 1)
                          (:lock-groups "lockgroups" 1))
    "A list containing the spec for the events received from the socket.

In order: identifier for use in HANDLE-EVENTS, string prefix received from the socket, and argument count."))

(defun handle-events-raw (handler)
  "Listen to Hyprland events, calling HANDLER every time a new event happens.

HANDLER should have a single argument, which is the line that was received from the socket."
  (with-local-stream-socket (events-socket *events-socket*)
    (let ((events-stream (sb-bsd-sockets:socket-make-stream events-socket
                                                            :input T)))
      (loop :for line := (read-line events-stream)
            :while line
            :do (funcall handler line)))))

(defun handle-line-if-matching-event (line prefix argc handler)
  "If LINE starts with PREFIX, split the remainder of the line into ARGC strings with commas and apply them to HANDLER.

Return whether or not LINE started with PREFIX, and also the result of HANDLER if it was called."
  (multiple-value-bind (matchp data-string)
      (alexandria:starts-with-subseq prefix
                                     line
                                     :return-suffix T)
    (when matchp
      (multiple-value-bind (sequences index)
          (split-sequence:split-sequence #\,
                                         data-string
                                         :count (- argc 1))
        (values T
                (apply handler
                       (nconc sequences (list (subseq data-string index)))))))))

(defun handle-events (&rest handlers)
  "Listen to for events from Hyprland, and apply the arguments of the events to the HANDLER with the matching KEY in HANDLERS."
  (let* (active-window-data
         (handler-table (loop :for (key handler) :on handlers :by #'cddr
                              :when (eq key :active-window)
                                :nconc `(("activewindow>>"
                                          2
                                          ,(lambda (&rest args)
                                             (setf active-window-data args)))
                                         ("activewindowv2>>"
                                          1
                                          ,(lambda (&rest args)
                                             (apply handler
                                                    (append active-window-data
                                                            args)))))
                              :else
                                :collect (destructuring-bind (prefix argc)
                                             (cdr (assoc key *events-spec*))
                                           (list (concatenate 'string prefix ">>")
                                                 argc
                                                 handler)))))
    (handle-events-raw (lambda (line)
                         (dolist (handler-spec handler-table)
                           (when (apply #'handle-line-if-matching-event line handler-spec)
                             (return)))))))
