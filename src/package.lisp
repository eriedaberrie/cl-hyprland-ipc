(uiop:define-package #:hyprland-ipc
  (:use #:cl #:alexandria)
  (:export

   #:*hyprctl-socket*
   #:*events-socket*

   ;; Hyprctl
   #:hyprctl
   #:hyprctl-batch
   #:get-client-data

   ;; Events
   #:*events-spec*
   #:handle-events-raw
   #:handle-events
   ))
