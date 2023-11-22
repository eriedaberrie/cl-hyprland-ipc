(uiop:define-package #:hyprland-ipc
  (:use #:cl #:alexandria)
  (:export

   #:*hyprctl-socket*
   #:*events-socket*

   ;; Hyprctl
   #:*hyprctl-retry-send-count*
   #:hyprctl
   #:hyprctl-batch
   #:find-client-data
   #:find-workspace-data
   #:find-monitor-data

   ;; Events
   #:*events-spec*
   #:handle-events-raw
   #:handle-events
   ))
