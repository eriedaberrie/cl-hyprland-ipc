(uiop:define-package #:hyprland-ipc
  (:use #:cl)
  (:export

   #:*hyprctl-socket*
   #:*events-socket*

   ;; Hyprctl
   #:hyprctl
   #:hyprctl-batch

   ;; Events
   #:*events-spec*
   #:handle-events-raw
   #:handle-events
   ))