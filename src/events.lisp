(in-package #:hyprland-ipc)

(defvar *events-spec* '((:workspace "workspace" 1)
                        (:focused-monitor "focusedmon" 2)
                        (:active-window nil 3)
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

In order: identifier for use in HANDLE-EVENTS, string prefix received from the socket, and argument count.")

(defun handle-events-raw (handler &key return-on-non-nil-p)
  "Listen to Hyprland events, calling HANDLER every time a new event happens.

HANDLER should have a single argument, which is the line that was received from the socket. If RETURN-ON-NON-NIL-P is non-NIL, stop at and return the first non-NIL result."
  (with-local-stream-socket (events-socket *events-socket*)
    (loop :with events-stream := (sb-bsd-sockets:socket-make-stream events-socket
                                                                    :input t)
          :for line := (read-line events-stream)
          :while line
          :for value := (funcall handler line)
          :when (and return-on-non-nil-p value)
            :return value)))

(defun handle-line-if-matching-event (line prefix argc handler)
  "If LINE starts with PREFIX, split the remainder of the line into ARGC strings with commas and apply them to HANDLER.

Return whether or not LINE started with PREFIX, and also the result of HANDLER if it was called."
  (when-let (data-string (nth-value 1
                                    (starts-with-subseq prefix
                                                        line
                                                        :return-suffix t)))
    (multiple-value-bind (sequences index)
        (split-sequence #\,
                        data-string
                        :count (1- argc))
      (values t
              (apply handler
                     (nconc sequences (list (subseq data-string index))))))))

(defun handle-events (&rest handlers &key return-on-non-nil-p &allow-other-keys)
  "Listen for events from Hyprland, and apply the arguments of the events to the HANDLER with the matching KEY in HANDLERS.

Pass RETURN-ON-NON-NIL-P to HANDLE-EVENTS-RAW."
  (let ((handler-table (loop :with active-window-data
                             :with added-active-window-listener-p
                             :with event-spec
                             :for (key handler) :on handlers :by #'cddr
                             :when (eq key :active-window)
                               :collect (list "activewindowv2>>"
                                              1
                                              (let ((handler handler))
                                                (lambda (&rest args)
                                                  (apply handler
                                                         (append active-window-data
                                                                 args)))))
                               :and :unless added-active-window-listener-p
                                      :collect (list "activewindow>>"
                                                     2
                                                     (lambda (&rest args)
                                                       (setf active-window-data args)
                                                       nil))
                                      :and :do (setf added-active-window-listener-p t)
                             :end
                             :else :when (setf event-spec (assoc key *events-spec*))
                                     :collect (destructuring-bind (prefix argc)
                                                  (cdr event-spec)
                                                (list (concatenate 'string prefix ">>")
                                                      argc
                                                      handler)))))
    (handle-events-raw (lambda (line)
                         (dolist (handler-spec handler-table)
                           (multiple-value-bind (matchp value)
                               (apply #'handle-line-if-matching-event line handler-spec)
                             (when matchp
                               (return value)))))
                       :return-on-non-nil-p return-on-non-nil-p)))
