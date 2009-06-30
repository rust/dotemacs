;;; iiimp.el --- IIIM Protocol Library
;;;

;; Copyright (C) 2000-2002 MIYASHITA Hisashi
;; Copyright (C) 2004-2005 Hiroshi Miura, NTT DATA Intellilink Co. 

;; Author: MIYASHITA Hisashi <himi@m17n.org>
;; Maintainer: Hiroshi Miura <miura@da-cha.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; This module supplies low level features for communications
;; via IIIMP.
;; This library depends on Mule-UCS-Unicode modules.
;; This library depends on udclient helper program to connect unix domain socket.

;;; Code:

(defvar iiimp-emacs-unicode-p
  (eq (make-char 'latin-iso8859-1 160) 160)) ; #xa0

(defvar iiimp-use-mule-ucs nil)

(if (or (<= emacs-major-version 21)
        iiimp-use-mule-ucs)
    (progn 
      (require 'un-define)
      (setq iiimp-use-mule-ucs t))
  (fset 'ucs-to-char 'identity)
  (fset 'char-to-ucs 'identity))

(defconst iiimp-protocol-version 2
  "IIIMP version number.")

(defvar iiimp-debug-flag nil)
(defvar iiimp-debug-buffer nil)

;; We cannot use such a fundamental function without
;; dirty aliasing...;_;
;; Emacs-XEmacs imcompatibility absorption layer...
(defalias 'iiimp-coding-system-or-name-p
  (cond ((coding-system-p 'no-conversion)
	 (function coding-system-p))
	((find-coding-system 'no-conversion)
	 (function find-coding-system))
	(t
	 (error
	  "This system have no function to detect coding-system(or its name)."))))

(defvar iiimp-current-com-id nil)
(make-variable-buffer-local 'iiimp-current-com-id)
(put 'iiimp-current-com-id 'permanent-local t)

(defvar iiimp-async-invocation-handler nil)
(make-variable-buffer-local 'iiimp-async-invocation-handler)
(put 'iiimp-async-invocation-handler 'permanent-local t)

(defvar iiimp-process-name "IIIMP")

(defvar iiimp-string-conversion-or-coding-system
  (if (iiimp-coding-system-or-name-p 'utf-16-be-no-signature-unix)
      'utf-16-be-no-signature-unix
    (if (iiimp-coding-system-or-name-p 'utf-16be) 'utf-16be
      (if (iiimp-coding-system-or-name-p 'utf-16-be-unix) 'utf-16-be-unix))))
;        (error "You need unicode-capable Emacs!")))))

(defvar iiimp-text-warning-char ?!)

(defvar iiimp-helper-path
  (unless (functionp 'make-network-process) "udclient")
  "Path to the utility program that bridges between pipe and local IPC.
If nil is specified, then `make-network-process' function will be
used for local IPC communication.")

(defconst iiimp-basic-opcode-spec-list
  '((iiimp-im-no-protocol
     0 (nil . nil)
     "NOP(not written in spec.).")
    (iiimp-im-connect
     1 (iiimp-send-im-connect . nil)
     "Connection request.")
    (iiimp-im-connect-reply
     2 (nil . iiimp-parse-im-connect-reply)
     "Connection accepted.")
    (iiimp-im-disconnect
     3 (iiimp-send-im-disconnect . nil)
     "Disconnection request.")
    (iiimp-im-disconnect-reply
     4 (nil . iiimp-parse-im-disconnect-reply)
     "Disconnection accepted.")
    (iiimp-im-register-trigger-keys
     5 (nil . iiimp-parse-im-register-trigger-keys)
     "Triggered Key Request.")
    (iiimp-im-trigger-notify
     6 (iiimp-send-im-trigger-notify . iiimp-parse-im-trigger-notify)
     "Notify input event.")
    (iiimp-im-trigger-notify-reply
     7 (iiimp-send-im-trigger-notify-reply
	. iiimp-parse-im-trigger-notify-reply)
     "Input events are accepted.")
    (iiimp-im-setimvalues
     8 (iiimp-send-im-setimvalues . iiimp-parse-im-setimvalues)
     "Set IM Attributes.")
    (iiimp-im-setimvalues-reply
     9 (iiimp-send-im-setimvalues-reply
	. iiimp-parse-im-setimvalues-reply)
     "IM Attributes accepted.")
    (iiimp-im-getimvalues
     10 (iiimp-send-im-getimvalues . nil)
     "Get IM Attributes.")
    (iiimp-im-getimvalues-reply
     11 (nil . iiimp-parse-im-getimvalues-reply)
     "Received IM Attributes.")
    (iiimp-im-forward-event
     12 (iiimp-send-im-forward-event . iiimp-parse-im-forward-event)
     "Forward events.")
    (iiimp-im-forward-event-reply
     13 (iiimp-send-im-forward-event-reply
	 . iiimp-parse-im-forward-event-reply)
     "Events are forwarded.")
    (iiimp-im-commit-string
     14 (nil . iiimp-parse-im-commit-string)
     "String is committed.")
    (iiimp-im-forward-event-with-operations
     15 (iiimp-send-im-forward-event-with-operations
	 . iiimp-parse-im-forward-event-with-operations)
     "Forward events with operations.")
    (iiimp-im-forward-event-with-operations-reply
     16 (iiimp-send-im-forward-event-with-operations-reply
	 . iiimp-parse-im-forward-event-with-operations-reply)
     "Events forwarded with operations.")
    (iiimp-im-register-hotkeys
     17 (nil . iiimp-parse-im-register-hotkeys)
     "Register hotkeys.")
    (iiimp-im-hotkey-notify
     18 (iiimp-send-im-hotkey-notify . iiimp-parse-im-hotkey-notify)
     "Notify hotkey.")
    (iiimp-im-hotkey-notify-reply
     19 (iiimp-send-im-hotkey-notify-reply
	 . iiimp-parse-im-hotkey-notify-reply)
     "Hotkey is notified.")
    (iiimp-im-createic
     20 (iiimp-send-im-createic . nil)
     "Create IC.")
    (iiimp-im-createic-reply
     21 (nil . iiimp-parse-im-createic-reply)
     "IC is created.")
    (iiimp-im-destroyic
     22 (iiimp-send-im-destroyic . nil)
     "Destroy IC.")
    (iiimp-im-destroyic-reply
     23 (nil . iiimp-parse-im-destroyic-reply)
     "IC is destroyed.")
    (iiimp-im-seticvalues
     24 (iiimp-send-im-seticvalues . nil)
     "Set IC values.")
    (iiimp-im-seticvalues-reply
     25 (nil . iiimp-parse-im-seticvalues-reply)
     "IC values are set.")
    (iiimp-im-geticvalues
     26 (iiimp-send-im-geticvalues . nil)
     "Get IC values.")
    (iiimp-im-geticvalues-reply
     27 (nil . iiimp-parse-im-geticvalues-reply)
     "Received IC values.")
    (iiimp-im-seticfocus
     28 (iiimp-send-im-seticfocus . nil)
     "Set IC focus.")
    (iiimp-im-seticfocus-reply
     29 (nil . iiimp-parse-im-seticfocus-reply)
     "IC focus is set.")
    (iiimp-im-unseticfocus
     30 (iiimp-send-im-unseticfocus . nil)
     "Unset IC focus.")
    (iiimp-im-unseticfocus-reply
     31 (nil . iiimp-parse-im-unseticfocus-reply)
     "IC focus is unset.")
    (iiimp-im-resetic
     32 (iiimp-send-im-resetic . nil)
     "Reset IC.")
    (iiimp-im-resetic-reply
     33 (nil . iiimp-parse-im-resetic-reply)
     "IC is reset.")
    (iiimp-im-hotkey-state-notify
     34 (nil . iiimp-parse-im-hotkey-state-notify)
     "Notify hotkey state.")
    (iiimp-im-hotkey-state-notify-reply
     35 (iiimp-send-im-hotkey-state-notify-reply . nil)
     "Hotkey state is notified.")
    (iiimp-im-select-hotkey-profile
     36 (nil . iiimp-parse-im-select-hotkey-profile)
     "Select hotkey profile.")

    (iiimp-im-preedit-start
     40 (nil . iiimp-parse-im-preedit-start)
     "Setup of preedit indicator is requested.")
    (iiimp-im-preedit-start-reply
     41 (iiimp-send-im-preedit-start-reply . nil)
     "Preedit indicator is set up.")
    (iiimp-im-preedit-draw
     42 (nil . iiimp-parse-im-preedit-draw)
     "Drawing of preedit indicator is requested.")
    (iiimp-im-preedit-draw-reply
     43 (iiimp-send-im-preedit-draw-reply . nil)
     "Preedit indicator is drawn.")
    (iiimp-im-preedit-done
     46 (nil . iiimp-parse-im-preedit-done)
     "Cleanup of preedit indicator is requested.")
    (iiimp-im-preedit-done-reply
     47 (iiimp-send-im-preedit-done-reply . nil)
     "Preedit indicator is cleaned.")

    (iiimp-im-status-start
     50 (nil . iiimp-parse-im-status-start)
     "Update of IM status is requested.")
    (iiimp-im-status-start-reply
     51 (iiimp-send-im-status-start-reply . nil)
     "IM Status update process is set up.")
    (iiimp-im-status-draw
     52 (nil . iiimp-parse-im-status-draw)
     "Drawing of IM status is requested.")
    (iiimp-im-status-draw-reply
     53 (iiimp-send-im-status-draw-reply . nil)
     "IM status is drawn.")
    (iiimp-im-status-done
     54 (nil . iiimp-parse-im-status-done)
     "Cleanup of IM status indicator is requested.")
    (iiimp-im-status-done-reply
     55 (iiimp-send-im-status-done-reply . nil)
     "IM status indicator is cleaned.")

    (iiimp-im-lookup-choice-start
     70 (nil . iiimp-parse-im-lookup-choice-start)
     "Lookup choice will be sent.")
    (iiimp-im-lookup-choice-start-reply
     71 (iiimp-send-im-lookup-choice-start-reply . nil)
     "Ready for lookup choice acceptance.")
    (iiimp-im-lookup-choice-draw
     72 (nil . iiimp-parse-im-lookup-choice-draw)
     "Lookup choice data is sent.")
    (iiimp-im-lookup-choice-draw-reply
     73 (iiimp-send-im-lookup-choice-draw-reply . nil)
     "Lookup choice data is received.")
    (iiimp-im-lookup-choice-done
     74 (nil . iiimp-parse-im-lookup-choice-done)
     "Lookup choice is done.")
    (iiimp-im-lookup-choice-done-reply
     75 (iiimp-send-im-lookup-choice-done-reply . nil)
     "Closed the session of lookup choice.")
    (iiimp-im-lookup-choice-process
     76 (nil . iiimp-parse-im-lookup-choice-process)
     "Change the page of lookup choice.")
    (iiimp-im-lookup-choice-process-reply
     77 (iiimp-send-im-lookup-choice-process-reply . nil)
     "The page of lookup choice is changed.")

    (iiimp-im-aux-start
     90 (nil . iiimp-parse-im-aux-start)
     "Update of auxiliary data is requested.")
    (iiimp-im-aux-start-reply
     91 (iiimp-send-im-aux-start-reply . nil)
     "Auxiliary data update process is set up.")
    (iiimp-im-aux-draw
     92 (nil . iiimp-parse-im-aux-draw)
     "Drawing of auxiliary data is requested.")
    (iiimp-im-aux-draw-reply
     93 (iiimp-send-im-aux-draw-reply . nil)
     "Auxiliary data is drawn.")
    (iiimp-im-aux-done
     94 (nil . iiimp-parse-im-aux-done)
     "Intersession for auxiliary data is done.")
    (iiimp-im-aux-done-reply
     95 (iiimp-send-im-aux-done-reply . nil)
     "Intersession for auxiliary data is closed.")
    (iiimp-im-aux-setvalues
     96 (iiimp-send-im-aux-setvalues . nil)
     "Set auxiliary data.")
    (iiimp-im-aux-setvalues-reply
     97 (nil . iiimp-parse-im-aux-setvalues-reply)
     "Auxiliary data is set.")
    (iiimp-im-protocol-version
     100 (nil . iiimp-parse-im-protocol-version)
     "Negotiate Protocol version.")
    (iiimp-im-file-operation
     101 (nil . nil)
     "IM_FILE_OPERATION")
    (iiimp-im-file-operation-reply
     102 (nil . nil)
     "IM_FILE_OPERATION_REPLY")
    ;; Opcode list ends here
    ))

(defconst iiimp-basic-opcode-vector
  (let ((ls iiimp-basic-opcode-spec-list)
	(vec (make-vector 256 nil))
	elem)
    (while (setq elem (car ls))
      (aset vec (nth 1 elem) (car elem))
      (put (car elem) 'iiimp-opcode elem)
      (setq ls (cdr ls)))
    vec))

(defsubst iiimp-opcode-spec (opcode)
  (if (integerp opcode) nil
    (get opcode 'iiimp-opcode)))

(defsubst iiimp-opcode-send-function (opcode)
  (or (car (nth 2 (iiimp-opcode-spec opcode)))
      (error "Opcode:%S has no sending function." opcode)))

(defsubst iiimp-opcode-number (opcode)
  (or (nth 1 (iiimp-opcode-spec opcode))
      (error "Unknown opcode:%S" opcode)))

(defun iiimp-dump-binary (str)
  (let ((i 0)
	(m (length str)))
    (while (> m i)
      (insert (format "%02X "
		      (aref str i)))
      (setq i (1+ i)))))

;;;
;;; Network stream manager
;;;

(defun iiimp-connection-error (&rest args)
  (signal 'iiimp-connection-error
	  args))
(put 'iiimp-connection-error 'error-conditions
     '(iiimp-fatal iiimp-connection error))
(put 'iiimp-connection-error 'error-message
     "IIIMP: connection reset by peer.")

;; COM-ID := [<PROCESS> <BUFFER> <PROTO> <HOST> <PORT> <END-POINT>]

(defun iiimp-construct-com-id (proc buf proto host port)
  (let ((obuf (current-buffer)))
    (if (eq obuf buf)
	(setq iiimp-current-com-id
	      (vector proc buf proto host port (point-min)))
      (prog2
	  (set-buffer buf)
	  (setq iiimp-current-com-id
		(vector proc buf proto host port (point-min)))
	(set-buffer obuf)))))

(defsubst iiimp-com-id-process (com-id)
  (aref com-id 0))

(defsubst iiimp-com-id-buffer (com-id)
  (aref com-id 1))

(defsubst iiimp-com-id-proto (com-id)
  (aref com-id 2))

(defsubst iiimp-com-id-host (com-id)
  (aref com-id 3))

(defsubst iiimp-com-id-port (com-id)
  (aref com-id 4))

(defsubst iiimp-com-id-end-point (com-id)
  (aref com-id 5))

(defsubst iiimp-record-end-point (com-id pt)
  (aset com-id 5 pt)
  com-id)

;; IM-ID := (id._NUMBER_of_IM COM-ID)

(defsubst iiimp-im-id-to-com-id (im-id)
  (nth 1 im-id))

(defsubst iiimp-im-id-to-handle (im-id)
  (car im-id))

(defsubst iiimp-construct-im-id (com-id im-num)
  (list im-num com-id))

;; IC-ID := (id._NUMBER_of_IC id._NUMBER_of_IM COM-ID)

(defsubst iiimp-ic-id-to-com-id (ic-id)
  (nth 2 ic-id))

(defsubst iiimp-ic-id-to-im-id (ic-id)
  (cdr ic-id))

(defsubst iiimp-ic-id-to-handle (ic-id)
  (car ic-id))

(defsubst iiimp-construct-ic-id (com-id im-num ic-num)
  (list ic-num im-num com-id))


;;; network I/O

(defsubst iiimp-check-channel-connection (com-id)
  (or (eq (process-status (iiimp-com-id-process com-id)) 'open)
      (eq (process-status (iiimp-com-id-process com-id)) 'run)))

(defun iiimp-proc-to-com-id (proc)
  (let ((oldbuf (current-buffer))
	(nbuf (process-buffer proc)))
    (if (not (eq oldbuf nbuf))
	(prog2
	    (set-buffer nbuf)
	    iiimp-current-com-id
	  (set-buffer oldbuf))
      iiimp-current-com-id)))

(defun iiimp-network-sentinel (proc stat)
  (let ((com-id (iiimp-proc-to-com-id proc)))
    (if (and com-id
	     (not (iiimp-check-channel-connection com-id)))
	(if (or (eq stat 'hangup) (equal stat "deleted\n")) nil
	  (iiimp-disable-async-invocation com-id)
	  (iiimp-connection-error "sentinel" com-id)))))

(defun iiimp-open-network-channel (proto host port buf)
  (let ((coding-system-for-read nil)
	(coding-system-for-write nil)
	proc)
    (save-excursion
      (set-buffer buf)
      (if (fboundp (function set-buffer-multibyte))
	  (set-buffer-multibyte nil))
      (buffer-disable-undo buf)
      (setq proc
	    (cond ((string= proto "unix")
		   (if iiimp-helper-path
		       (let ((process-connection-type nil))
			 (start-process 
			  iiimp-process-name buf
			  iiimp-helper-path host))
		     (make-network-process
		      :name iiimp-process-name :buffer buf :family 'local
		      :service host)))
		  (t (open-network-stream iiimp-process-name buf host port))))
      (process-kill-without-query proc) ;; obsolete since 21.1.
      ;; (set-process-query-on-exit-flag proc nil)
      (set-process-coding-system proc 'binary 'binary)
      (set-process-sentinel
       proc (function iiimp-network-sentinel))
      (set-marker-insertion-type (process-mark proc) t))
    proc))

(defun iiimp-create-network-channel (proto host port)
  (let* ((buf (generate-new-buffer " *IIIMP*"))
	 (proc (iiimp-open-network-channel proto host port buf)))
    (iiimp-construct-com-id proc buf proto host port)))

(defun iiimp-destroy-network-channel (com-id)
  (let* ((proc (iiimp-com-id-process com-id))
	 (stat (process-status proc)))
    (while (not (or (or (eq stat 'closed)
			(eq stat 'exit))
		    (eq stat nil)))
      (set-process-sentinel proc nil)
      (delete-process proc)
      (setq stat (process-status proc))
      (sleep-for 1))
    (kill-buffer (iiimp-com-id-buffer com-id))
    t))

(defun iiimp-async-invocation-handler-1 (beg end len)
  (funcall (cdr iiimp-async-invocation-handler)
	   (car iiimp-async-invocation-handler)))

(defun iiimp-enable-async-invocation (com-id func)
  (let ((buf (iiimp-com-id-buffer com-id)))
    (save-excursion
      (set-buffer buf)
      (setq iiimp-async-invocation-handler
	    (cons com-id func))
      (make-local-hook 'after-change-functions)
      (add-hook 'after-change-functions
		(function
		 iiimp-async-invocation-handler-1)
		nil t))))

(defun iiimp-disable-async-invocation (com-id)
  (let ((buf (iiimp-com-id-buffer com-id)))
    (if (buffer-live-p buf)
	(save-excursion
	  (set-buffer buf)
	  (remove-hook 'after-change-functions
		       (function
			iiimp-async-invocation-handler-1)
		       t)))))

;;
;;
;;

(defmacro iiimp-communication (com-id &rest form)
  `(save-excursion
     (set-buffer (iiimp-com-id-buffer ,com-id))
     (let ((after-change-functions nil))
       ,@form)))

(defun iiimp-check-channel-bytes (com-id start bytes wait)
  (cond ((eq wait t)
	 (catch 'tag
	   (while (< (point-max) (+ start bytes))
	     (if (iiimp-check-channel-connection com-id)
		 (accept-process-output
		  (iiimp-com-id-process com-id)
		  1)
	       (throw 'tag nil)))
	   t))
	((numberp wait)
	 (if (< (point-max) (+ start bytes))
	     (accept-process-output
	      (iiimp-com-id-process com-id)
	      wait))
	 (>= (point-max) (+ start bytes)))
	((null wait)
	 (>= (point-max) (+ start bytes)))
	(t
	 (error "Invalid argument(wait):%S" wait))))

(defun iiimp-wait-channel (com-id until)
  (while (< (point-max) until)
    (accept-process-output (iiimp-com-id-process com-id))))

(defun iiimp-read-channel (com-id start min-bytes)
  (iiimp-wait-channel com-id (+ start min-bytes))
  (if iiimp-debug-flag
      (save-excursion
	(let ((str (buffer-substring start (+ start min-bytes))))
	  (set-buffer iiimp-debug-buffer)
	  (insert (format "<= [%d bytes]:[" (length str)))
	  (iiimp-dump-binary str)
	  (insert "]\n")))))

(defun iiimp-write-channel (com-id str &optional bytes)
  (if (eq iiimp-debug-flag 'verbose)
      (save-excursion
	(set-buffer iiimp-debug-buffer)
	(insert (format "=> [%d bytes]:[" (length str)))
	(iiimp-dump-binary str)
	(insert "]\n")))
  (if iiimp-emacs-unicode-p (setq str (string-make-unibyte str)))
  (if bytes
      (process-send-string (iiimp-com-id-process com-id)
			   (substring str 0 bytes))
    (process-send-string (iiimp-com-id-process com-id) str)))

(defun iiimp-packet-substring (packet start end)
  (if (stringp packet)
      (substring packet start end)
    (iiimp-wait-channel packet end)
    (buffer-substring start end)))

(defun iiimp-packet-skip-bytes (packet start bytes)
  (iiimp-read-channel packet start bytes)
  (+ start bytes))

(defun iiimp-packet-erase (packet start)
  (cond ((vectorp packet)
	 (delete-region (point-min) start)
	 (iiimp-record-end-point packet (point-min))
	 (point-min))
	((stringp packet)
	 start)
	(t
	 (error "Invalid packet:%S" packet))))

(defun iiimp-packet-aref (packet idx)
  (if (stringp packet)
      (aref packet idx)
    (iiimp-wait-channel packet idx)
    (goto-char idx)
    (following-char)))

:;;
;;; Composite data type manipulaters.
;;;

(defconst iiimp-attribid-alist
  '(;; IM attribid
    (input-method-list . 4097) ; #x1001
    (object-descriptor-list . 4112) ; #x1010
    (client-descriptor . 4113) ; #x1011 
    (ccdef . 4144) ; #x1030
    (jar-gui-object . 4145) ; #x1031
    (jar-lwe-object . 4146) ; #x1032
    (binary-gui-object . 4147) ; #x1033
    (binary-lwe-object . 4148) ; #x1034
    (script-gui-object . 4149) ; #x1035
    (script-lwe-object . 4150) ; #x1036
    ;; IC attribid
    (input-language . 1)
    (character-subsets . 2)
    (input-method . 3)))

(defconst iiimp-id0-feedback-alist
  '((reverse . 1)
    (underline . 2)
    (highlight . 4)
    (primary . 8)
    (secondary . 16)
    (tertiary . 32)))

(defconst iiimp-annotation-properties
  '((iiim-input-string 1 iiimp-text-pack iiimp-text-unpack)
    (iiim-reading 2 iiimp-text-pack iiimp-text-unpack)
    (iiim-part-of-speech 3 iiimp-string-pack iiimp-string-unpack)
    (iiim-clause 4 iiimp-string-pack iiimp-string-unpack)))

(defmacro iiimp-pack-number (bitsspec)
  (let ((sbits 0) (totbits 0) (num 0)
	prg bls plspec spec val bits ibits
	mask tmp)
    (while bitsspec
      (setq plspec nil)
      (while (and (setq spec (car bitsspec))
		  (< sbits 8))
	(setq sbits (+ sbits (cdr spec))
	      plspec (cons spec plspec)
	      bitsspec (cdr bitsspec)))
      ;; Check bitsspec restriction.
      (if (> sbits 32)
	  (error "Cannot serialize this %S bitsspec because we need larger integer area." bitsspec)
	(if (and (> sbits 28)
		 (> (length plspec) 1))
	    (error "Cannot serialize more than 28 bits number that is not aligned to byte boundary.%S" bitsspec)))
      ;; Expand bitsspec chunk.
      (setq ibits sbits
	    plspec (nreverse plspec)
	    num 0)
      (while (setq spec (car plspec))
	(setq val (car spec)
	      bits (cdr spec)
	      ibits (- ibits bits)
	      mask (if (>= bits 28)
		       nil
		     (1- (lsh 1 bits))))
	(if (numberp val)
	    (setq num
		  (if mask
		      (logior (lsh (logand val mask) ibits) num)
		    (logior (lsh val ibits) num)))
	  (setq tmp (if mask
			(list 'logand val mask)
		      val))
	  (setq prg (cons
		     (if (> ibits 0)
			 (list 'lsh tmp ibits)
		       tmp)
		     prg)))
	(setq plspec (cdr plspec)))
      (setq prg
	    (cond ((> (length prg) 1)
		   (if (> num 0)
		       (list 'cons sbits (append (list 'logior num) prg))
		     (list 'cons sbits (cons 'logior prg))))
		  ((= (length prg) 1)
		   (if (> num 0)
		       (list 'cons sbits (list 'logior num (car prg)))
		     (list 'cons sbits (car prg))))
		  (t `(quote (,sbits . ,num))))
	    bls (cons prg bls)
	    totbits (+ totbits sbits)
	    sbits 0
	    prg nil))
    (setq bls (cons 'list (nreverse bls)))
    ;; embedded program
    `(let ((x-ls ,bls)
	   (x-str (make-string ,(/ (+ totbits 7) 8) 0))
	   (x-num 0) (x-i 0) (x-sb 0)
	   x-b x-elem)
       (while (setq x-elem (car x-ls))
	 (setq x-b (car x-elem))
	 (if (> x-b 28)
	     (setq x-num (cdr x-elem))
	   (setq x-num (logior (lsh x-num x-b) (cdr x-elem))))
	  (setq x-sb (+ x-b x-sb)
		x-ls (cdr x-ls))
	 (while (or (>= x-sb 8)
		    (and (null x-ls)
			 (> x-sb 0)))
	   (setq x-sb (- x-sb 8))
	   (aset x-str x-i (logand (lsh x-num (- x-sb)) ?\xFF))
	   (setq x-i (1+ x-i))))
       x-str)))

(defmacro iiimp-unpack-number (bitsspec packet start)
  (let ((result
	 (copy-sequence
	  `(let ((x-i ,start)
		 (x-num 0)
		 (x-ib 0)))))
	spec var bits)
    (while (setq spec (car bitsspec))
      (setq var (car spec)
	    bits (cdr spec)
	    result
	    (nconc
	     result
	     `((setq x-ib (+ x-ib ,bits))
	       (while (> x-ib 0)
		 (setq x-num
		       (logior (lsh x-num 8)
			       (iiimp-packet-aref ,packet x-i))
		       x-i (1+ x-i)
		       x-ib (- x-ib 8)))
	       (setq ,var (lsh x-num x-ib)
		     x-num (logand x-num
				   (1- (lsh 1 (- x-ib)))))))
	    bitsspec (cdr bitsspec)))
    (nconc result '(x-i))))

(defsubst iiimp-pack-list (packer val)
  (mapconcat packer val ""))

(defsubst iiimp-unpack-list (packet start len unpacker)
  (let ((end (+ len start))
	slot result)
    (while (> end start)
      (setq slot (funcall unpacker packet start)
	    start (cdr slot)
	    result (cons (car slot) result)))
    (cons (nreverse result) start)))

(defsubst iiimp-padding-bytes (n)
  (% (- 4 (% n 4)) 4))

(defsubst iiimp-make-padding (n)
  (make-string (iiimp-padding-bytes n) 0))

;; im,ic network representation.
(defsubst iiimp-pack-im-ic (im-id &optional ic-id)
  (concat
   (iiimp-pack-number
    (((iiimp-im-id-to-handle im-id) . 16)))
   (if ic-id
       (iiimp-pack-number
	(((iiimp-im-id-to-handle ic-id) . 16))))))

;; annotation id.
(defsubst iiimp-aid-to-aid-spec (aid)
  (let ((ll iiimp-annotation-properties))
    (while (and ll (/= aid (nth 1 (car ll))))
      (setq ll (cdr ll)))
    (or (car ll)
	;;(error "Invalid aid:%d" aid)
	)))

;; attributes.
(defsubst iiimp-request-attrs-pack (syms)
  (let ((result "") val)
    (while syms
      (setq val (cdr (assq (car syms) iiimp-attribid-alist))
	    result (concat
		    result
		    (iiimp-pack-number ((val . 16)))))
      (setq syms (cdr syms)))
    result))

;; <STRING> := [<bytelen of L.CHAR> <L.CHAR> Pad(2+2n)]
(defsubst iiimp-string-pack (str)
  (let* ((bytes (encode-coding-string
		 str iiimp-string-conversion-or-coding-system))
	 len)
    (if (and (not (eq iiimp-string-conversion-or-coding-system
		      'utf-16-be-no-signature-unix))
	     (= (aref bytes 0) ?\xFE)
	     (= (aref bytes 1) ?\xFF))
	(setq bytes (substring bytes 2)))
    (setq len (length bytes))
    (concat (iiimp-pack-number ((len . 16)))
	    bytes
	    (iiimp-make-padding (+ len 2)))))

(defsubst iiimp-string-unpack (packet start)
  (let (bytes len end)
    (setq start
	  (iiimp-unpack-number ((len . 16)) packet start))
    (setq end (+ start len))
    (cons (decode-coding-string
	   (iiimp-packet-substring packet start end)
	   iiimp-string-conversion-or-coding-system)
	  (+ end (iiimp-padding-bytes (+ len 2))))))

;; <FEEDBACK> := [<feedback:C32>]
(defsubst iiimp-feedback-pack (fb)
  (iiimp-pack-number (((car fb) . 16)
		      ((cdr fb) . 16))))

(defsubst iiimp-feedback-unpack (packet start)
  (let (fb1 fb2)
    (setq start
	  (iiimp-unpack-number
	   ((fb1 . 16) (fb2 . 16))
	   packet start))
    (cons (cons fb1 fb2) start)))

;; <FEEDBACK-ATTR> := [<feedback id:C32> <FEEDBACK>]
(defsubst iiimp-feedback-attr-pack (feedback-attr)
  (let ((fbid (car feedback-attr))
	(fb (nth 1 feedback-attr)))
    (concat
     (iiimp-pack-number (((car fbid) . 16)
			 ((cdr fbid) . 16)))
     (iiimp-feedback-pack fb))))

(defsubst iiimp-feedback-attr-unpack (packet start)
  (let (fbid1 fbid2 res)
    (setq start
	  (iiimp-unpack-number
	   ((fbid1 . 16) (fbid2 . 16))
	   packet start)
	  res (iiimp-feedback-unpack packet start))
    (cons (list (cons fbid1 fbid2) (car res))
	  (cdr res))))

;; <CHAR-WITH-FEEDBACK> :=
;;     [<CHAR> <bytelen of L.FEEDBACK> <L.FEEDBACK> ]
(defsubst iiimp-char-with-feedback-pack (char-feedback)
  (let* ((char (char-to-ucs (car char-feedback)))
	 (lfb (nth 1 char-feedback))
	 (lfbpack
	  (iiimp-pack-list
	   (function iiimp-feedback-pack)
	   lfb))
	 (len (length lfbpack)))
    (concat (iiimp-pack-number ((char . 16) (len . 16)))
	    lfbpack)))

(defsubst iiimp-char-with-feedback-unpack (packet start)
  (let (len char end slot lfb)
    (setq start
	  (iiimp-unpack-number ((char . 16) (len . 16))
			       packet start))
    (setq end (+ start len))
    (while (> end start)
      (setq slot (iiimp-feedback-unpack packet start)
	    start (cdr slot)
	    lfb (cons (car slot) lfb)))
    (cons (list (ucs-to-char char) lfb)
	  start)))

;; <OPERATION> := [<STRING> <bytelen of val.> <val.>xn Pad(n) ]
;;       -> (<STRING>) if val is empty
;;       -> (<STRING> . (<VAL>:binary-string)) otherwise.
(defsubst iiimp-operation-pack (operation)
  (let* ((val (nth 1 operation))
	 (len (length val)))
    (if val
	(concat
	 (iiimp-string-pack (car operation))
	 (iiimp-pack-number ((len . 16)))
	 val
	 (iiimp-make-padding (+ len 2)))
      (concat
       (iiimp-string-pack (car operation))
       (iiimp-pack-number ((0 . 16)))
       (iiimp-make-padding 2)))))

(defsubst iiimp-operation-unpack (packet start)
  (let* ((elem (iiimp-string-unpack packet start))
	 (str (car elem))
	 len)
    (setq start 
	  (iiimp-unpack-number ((len . 16))
			       packet (cdr elem)))
    (if (> len 0)
	(cons
	 (list str (iiimp-packet-substring
		    packet start (+ start len)))
	 (+ start (iiimp-padding-bytes (+ len 2))))
      (cons
       (list str)
       (+ start 2)))))

;; <ANNOTATION-VALUE> :=
;;     [<start idx.> <end idx.> <bytelen of val.> <val.>xn Pad(n) ]
(defsubst iiimp-annotation-value-pack (annotation-value)
  (let* ((sidx (car annotation-value))
	 (eidx (nth 1 annotation-value))
	 (val (nth 2 annotation-value))
	 (len (length val)))
    (concat 
     (iiimp-pack-number
      (((car sidx) . 16)
       ((cdr sidx) . 16)
       ((car eidx) . 16)
       ((cdr eidx) . 16)
       (len . 32)))
     val
     (iiimp-make-padding len))))

(defsubst iiimp-annotation-value-unpack (packet start)
  (let (sidx1 sidx2 eidx1 eidx2 len)
    (setq start
	  (iiimp-unpack-number
	   ((sidx1 . 16)
	    (sidx2 . 16)
	    (eidx1 . 16)
	    (eidx2 . 16)
	    (len . 32))
	   packet start))
    (cons
     (list (cons sidx1 sidx2)
	   (cons eidx1 eidx2)
	   (iiimp-packet-substring
	    packet start (+ start len)))
     (+ start len (iiimp-padding-bytes len)))))

;; <ANNOTATION> := [<attr. ID> <bytelen of L.AV> <L.AV>]
(defsubst iiimp-annotation-pack (annotation)
  (let* ((aid (car annotation))
	 (aid1 (car aid))
	 (aid2 (cdr aid))
	 (lav (nth 1 annotation))
	 (lavpack
	  (iiimp-pack-list
	   (function iiimp-annotation-value-pack)
	   lav))
	 (len (length lavpack)))
    (concat (iiimp-pack-number
	     ((aid1 . 16)
	      (aid2 . 16)
	      (len . 32)))
	    lavpack)))

(defsubst iiimp-annotation-unpack (packet start)
  (let (aid1 aid2 len end slot lav)
    (setq start
	  (iiimp-unpack-number
	   ((aid1 . 16)
	    (aid2 . 16)
	    (len . 32))
	   packet start))
    (setq end (+ start len))
    (while (> end start)
      (setq slot (iiimp-annotation-value-unpack packet start)
	    start (cdr slot)
	    lav (cons (car slot) lav)))
    (cons (list (cons aid1 aid2) lav)
	  start)))

;; <TEXT> := [<bytelen of L.CWF> <L.CWF>
;;            <bytelen of L.ANNOTATION> <L.ANNOTATION>]
;;         ---> string with text-properties.

(defun iiimp-pack-feedback-list (prop)
  (let ((val-id0 0)
	elem fbl slot
	pack tem1 tem2)
    (while (setq elem (car prop))
      (cond ((symbolp elem)
	     (setq slot (cdr (assq elem iiimp-id0-feedback-alist)))
	     (if slot (setq val-id0 (logior val-id0 slot))))
	    ((consp elem)
	     (setq fbl (cons elem fbl)))
	    (t
	     (error "Unknown property:%S" elem)))
      (setq prop (cdr prop)))
    (concat
     (if (> val-id0 0)
	 (iiimp-pack-number ((0 . 32) (val-id0 . 32))))
     (iiimp-pack-list
      (lambda (fbelem)
	(setq tem1 (car fbelem)
	      tem2 (cdr fbelem))
	(iiimp-pack-number
	 (((car tem1) . 16)
	  ((cdr tem1) . 16)
	  ((car tem2) . 16)
	  ((car tem2) . 16))))
      fbl))))

(defun iiimp-unpack-feedback-list (packet start end)
  (let (aid1 aid2 fb1 fb2 ll lcand slot)
    (while (> end start)
      (setq start
	    (iiimp-unpack-number
	     ((aid1 . 16)
	      (aid2 . 16)
	      (fb1 . 16)
	      (fb2 . 16))
	     packet start))
      (if (and (= aid1 0)
	       (= aid2 0)
	       (= fb1 0))
	  (progn
	    (setq lcand iiimp-id0-feedback-alist)
	    (while (setq slot (car lcand))
	      (if (/= 0 (logand fb2 (cdr slot)))
		  (setq ll (cons (car slot) ll)))
	      (setq lcand (cdr lcand))))
	(setq ll (cons (cons (cons aid1 aid2)
			     (cons fb1 fb2))
		       ll))))
    (nreverse ll)))

(defun iiimp-extract-annotation (props)
  (let ((input-string (plist-get props 'iiim-input-string))
	(reading (plist-get props 'iiim-reading))
	(speech (plist-get props 'iiim-part-of-speech))
	(clause (plist-get props 'iiim-clause)))
    (list input-string reading speech clause)))

(defun iiimp-judge-annotation-interval (prev next i stl)
  (let (result cur1 cur2 cur-s)
    (while (and prev next)
      (setq cur1 (car prev)
	    cur2 (car next)
	    cur-s (car stl))
      (setq prev (cdr prev)
	    next (cdr next)
	    stl (cdr stl))
      (if (eq cur1 cur2)
	  (setq result (cons cur-s result))
	(setq result (cons i result))))
    (nreverse result)))

(defun iiimp-construct-annotation-interval (props st end)
  (let ((propspecs iiimp-annotation-properties)
	result cur cur-s cur-e)
    (while (and props st end)
      (setq cur-s (car st)
	    cur-e (car end)
	    cur (car props))
      (setq st (cdr st)
	    end (cdr end)
	    props (cdr props)
	    propspecs (cdr propspecs))
      (if (and (not (eq cur-s cur-e))
	       cur)
	  (setq result
		(cons (list (car (car propspecs))
			    cur cur-s cur-e)
		      result))))
    result))

(defun iiimp-pack-annotation-interval (il)
  (let (nspec nslot propspec
	packed len result elem tmp)
    (while (setq elem (car il))
      (setq il (cdr il))
      (setq nslot (assq (car elem) nspec))
      (if nslot
	  (setcdr nslot (nconc (cdr nslot)
			       (list (cdr elem))))
	(setq nspec (cons (cons (car elem)
				(list (cdr elem)))
			  nspec))))
    (while (setq elem (car nspec))
      (setq propspec (assq (car elem) iiimp-annotation-properties))
      (setq packed (iiimp-pack-list
		    (lambda (x)
		      (setq tmp
			    (funcall (nth 2 propspec) (car x))
			    len (length tmp))
		      (concat
		       (iiimp-pack-number (((nth 1 x) . 32)
					   ((nth 2 x) . 32)
					   (len . 32)))
		       tmp
		       (iiimp-make-padding len)))
		    (cdr elem)))
      (setq result (concat
		    result
		    (iiimp-pack-number (((nth 1 propspec) . 32)
					((length packed) . 32)))
		    packed))
      (setq nspec (cdr nspec)))
    result))

(defun iiimp-text-pack (str)
  (let ((i 0)
	(len (length str))
	char props propf
	fbpack ilpack result
	prev-a next-a il ile1 ile2)
    (while (< i len)
      (setq char (char-to-ucs (aref str i))
	    props (text-properties-at i str)
	    propf (plist-get props 'iiim-feedback)
	    fbpack (if propf (iiimp-pack-feedback-list propf))
	    next-a (iiimp-extract-annotation props)
	    ile2 (iiimp-judge-annotation-interval
		  prev-a next-a i ile1)
	    il (nconc il (iiimp-construct-annotation-interval
			  prev-a ile1 ile2))
	    prev-a next-a
	    ile1 ile2)
      (setq result (concat
		    result
		    (iiimp-pack-number
		     ((char . 16) ((length fbpack) . 16)))
		    fbpack)
	    i (1+ i)))

    (setq ilpack
	  (iiimp-pack-annotation-interval il))
    (concat
     (iiimp-pack-number (((length result) . 32)))
     result
     (iiimp-pack-number (((length ilpack) . 32)))
     ilpack)))

(defun iiimp-unichar-to-string (char)
  (if (not (or iiimp-use-mule-ucs
	       iiimp-emacs-unicode-p))
      (progn
	(let ((c char)
	      (str "")
	      (lst ())
	      len first)
	  (if (< c #x80)
	      (progn
		(setq first 0)
		(setq len 1))
	    (if (< c #x800)
		(progn
		  (setq first #xc0)
		  (setq len 2))
	      (if (< c #x10000)
		  (progn
		    (setq first #xe0)
		    (setq len 3))
		(if (< c #x200000)
		    (progn
		      (setq first #xf0)
		      (setq len 4))
		  (if (< c #x4000000)
		      (progn
			(setq first #xf8)
			(setq len 5))
		    (setq first #xfc)
		    (setq len 6))))))
	  (setq len (1- len))
	  (while (> len 0)
	    (setq lst (append lst (list (logior (logand c #x3f) #x80))))
	    (setq c (lsh c -6))
	    (setq len (1- len)))
	  (setq lst (reverse lst))
	  (setq str (concat str
			    (append (list (logior c first)) lst)))
	  (decode-coding-string str 'utf-8 t)))
    (string char)))

(defun iiimp-text-unpack (packet start)
  (let ((str "") tmpstr
	len end ucs char
        len-l end-l
	sidx eidx len-v
	props props-prev
	(sp 0) (ep 0)
	aid aid-spec
        surrogate)
    (setq start
	  (iiimp-unpack-number ((len . 32))
			       packet start))
    (setq end (+ start len))
    ;; deal with CHAR-WITH-FEEDBACK
    (while (> end start)
      (setq start
	    (iiimp-unpack-number
	     ((ucs . 16)
	      (len-l . 16))
	     packet start))
      (setq end-l (+ start len-l)
	    props (iiimp-unpack-feedback-list
		   packet start end-l)
	    start end-l)
      ;; surrogate support
      (if surrogate
	  (progn
	    (setq ucs (+ (logand ucs ?\x3ff) (lsh surrogate 10) ?\x10000))
	    (setq surrogate nil))
	(if (= (logand ucs ?\xfc00) ?\xd800)
	    (progn
	      (setq surrogate (logand ucs ?\x3ff))
	      (setq ucs 0))))
      (setq char (ucs-to-char ucs))
      (if (null char)
	  (setq tmpstr (iiimp-unichar-to-string iiimp-text-warning-char)
		props (nconc
		       (list 'unknown-ucs
			     (cons 'ucs ucs))
		       props))
	(setq tmpstr (iiimp-unichar-to-string char)))

      (setq str (concat str tmpstr))
      (if (equal props props-prev)
	  (setq ep (length str))
	(if props-prev
	    (put-text-property
	     sp ep
	     'iiim-feedback props-prev str))
	(setq props-prev props
	      sp ep
	      ep (length str))))
    (if props
	(put-text-property
	 sp (length str)
	 'iiim-feedback props str))

    (setq start
	  (iiimp-unpack-number ((len . 32))
			       packet start))
    (setq end (+ start len))
    (while (> end start)
      ;; deal with ANNOTATION.
      (setq start
	    (iiimp-unpack-number
	     ((aid . 32)
	      (len-l . 32))
	     packet start))
      (setq aid-spec (iiimp-aid-to-aid-spec aid))
      (setq end-l (+ start len-l))
      (if aid-spec
	  ;; deal with LIST of ANNOTATION-VALUE.
	  (while (> end-l start)
	    (setq start
		  (iiimp-unpack-number
		   ((sidx . 32)
		    (eidx . 32)
		    (len-v . 32))
		   packet start))
	    (put-text-property
	     sidx eidx (car aid-spec)
	     (car (funcall (nth 3 aid-spec)
			   packet start))
	     str)
	    (setq start (+ start len-v)))
	;; ignore unknown ANNOTATION-ID.
	(setq start end-l)))
    ;; remove all `\0'
    (while (string-match "\0" str)
      (setq str (concat (substring str 0 (match-beginning 0))
                        (substring str (match-end 0)))))
    (cons str start)))


;; <ATTRIBID> := [<predefined indicator:C1>
;;                <Enumerated ID val.:C15>]
(defsubst iiimp-attribid-pack (attribid)
  (if (listp attribid)
      (iiimp-pack-number (((car attribid) . 1)
			  ((nth 1 attribid) . 15)))
    (setq attribid
	  (cdr (assq attribid iiimp-attribid-alist)))
    (iiimp-pack-number ((0 . 1)
			(attribid . 15)))))

(defsubst iiimp-attribid-unpack (packet start)
  (let (pid eid sym)
    (setq start
	  (iiimp-unpack-number ((pid . 1) (eid . 15))
			       packet start))
    (if (and (= pid 0)
	     (setq sym (car (rassq eid iiimp-attribid-alist))))
	(cons sym start)
      (cons (list pid eid) start))))

;; <CLIENT-DESCRIPTOR> := [<STRING> <STRING> <STRING> <STRING>]
(defun iiimp-client-descriptor-pack (client-descriptor)
  (concat
   (iiimp-string-pack (car client-descriptor))
   (iiimp-string-pack (nth 1 client-descriptor))
   (iiimp-string-pack (nth 2 client-descriptor))
   (iiimp-string-pack (nth 3 client-descriptor))))

(defun iiimp-client-descriptor-unpack (packet start)
  (let (slot st1 st2 st3 st4)
    (setq slot (iiimp-string-unpack packet start)
	  st1 (car slot)
	  start (cdr slot)
	  slot (iiimp-string-unpack packet start)
	  st2 (car slot)
	  start (cdr slot)
	  slot (iiimp-string-unpack packet start)
	  st3 (car slot)
	  start (cdr slot)
	  slot (iiimp-string-unpack packet start)
	  st4 (car slot)
	  start (cdr slot))
    (cons (list st1 st2 st3 st4) start)))

;; <OBJECT-DESCRIPTOR> :=
;;     [<object category:C16> Pad(2)
;;      <object size:C32>
;;      <ATTRIBID> <ATTRIBID>
;;      <STRING> <STRING> <STRING> <STRING>]
(defun iiimp-object-descriptor-pack (object-descriptor)
  (let ((dum 0))
    (concat
     (iiimp-pack-number
      (((car object-descriptor) . 16)
       (dum . 16)
       ((nth 1 object-descriptor) . 32)))
     (iiimp-attribid-pack (nth 2 object-descriptor))
     (iiimp-attribid-pack (nth 3 object-descriptor))
     (iiimp-string-pack (nth 4 object-descriptor))
     (iiimp-string-pack (nth 5 object-descriptor))
     (iiimp-string-pack (nth 6 object-descriptor))
     (iiimp-string-pack (nth 7 object-descriptor)))))

(defun iiimp-object-descriptor-unpack (packet start)
  (let (slot ocat dum size at1 at2 st1 st2 st3 st4)
    (setq start (iiimp-unpack-number
		 ((ocat . 16)
		  (dum . 16)
		  (size . 32))
		 packet start)
	  slot (iiimp-attribid-unpack packet start)
	  at1 (car slot)
	  start (cdr slot)
	  slot (iiimp-attribid-unpack packet start)
	  at2 (car slot)
	  start (cdr slot)
	  slot (iiimp-string-unpack packet start)
	  st1 (car slot)
	  start (cdr slot)
	  slot (iiimp-string-unpack packet start)
	  st2 (car slot)
	  start (cdr slot)
	  slot (iiimp-string-unpack packet start)
	  st3 (car slot)
	  start (cdr slot)
	  slot (iiimp-string-unpack packet start)
	  st4 (car slot)
	  start (cdr slot))
    (cons (list ocat size at1 at2 st1 st2 st3 st4) start)))

;; <CCDEF> := [<STRING>]
(defsubst iiimp-ccdef-pack (ccdef)
  (iiimp-string-pack (car ccdef)))

(defsubst iiimp-ccdef-unpack (packet start)
  (let ((slot (iiimp-string-unpack packet start)))
    (cons (list (car slot))
	  (cdr slot))))

;; <LANGUAGE> := [<STRING> <STRING>]
(defsubst iiimp-language-pack (language)
  (concat
   (iiimp-string-pack (car language))
   (iiimp-string-pack (nth 1 language))))

(defsubst iiimp-language-unpack (packet start)
  (let (slot st1 st2)
    (setq slot (iiimp-string-unpack packet start)
	  st1 (car slot)
	  start (cdr slot)
	  slot (iiimp-string-unpack packet start)
	  st2 (car slot)
	  start (cdr slot))
    (cons (list st1 st2) start)))

;; <JARFILE-OBJECT> := [<bytelen of L.STRING> <L.STRING(classname)>
;;                      <bytelen of jar file> <jar file byte stream>
;;                      Pad(n)]
(defun iiimp-jarfile-object-pack (jarfile-object)
  (let* ((lstr (car jarfile-object))
	 (ljar (nth 1 jarfile-object))
	 (lstrpack
	  (iiimp-pack-list
	   (function iiimp-string-pack)
	   lstr))
	 (lstr-len (length lstrpack))
	 (ljar-len (length ljar)))
    (concat (iiimp-pack-number ((lstr-len . 32)))
	    lstrpack
	    (iiimp-pack-number ((ljar-len . 32)))
	    ljar
	    (iiimp-make-padding ljar-len))))

(defun iiimp-jarfile-object-unpack (packet start)
  (let (len end slot lstr lan)
    (setq start
	  (iiimp-unpack-number ((len . 32))
			       packet start))
    (setq end (+ start len))
    (while (> end start)
      (setq slot (iiimp-string-unpack packet start)
	    start (cdr slot)
	    lstr (cons (car slot) lstr)))

    (setq start
	  (iiimp-unpack-number ((len . 32))
			       packet start)
	  end (+ start len))
    (cons (list lstr (iiimp-packet-substring
		      packet start end))
	  (+ end (iiimp-padding-bytes len)))))

;; <IM-DESCRIPTOR> := [<ATTRIBID> Pad(2)
;;                     <STRING> <STRING>
;;                     <bytelen of L.LANGUAGE> <L.LANGUAGE>]
(defun iiimp-im-descriptor-pack (im-descriptor)
  (let* ((llanpack
	  (iiimp-pack-list
	   (function iiimp-language-pack)
	   (nth 3 im-descriptor)))
	 (len (length llanpack)))
    (concat
     (iiimp-attribid-pack (car im-descriptor))
     (iiimp-make-padding 2)
     (iiimp-string-pack (nth 1 im-descriptor))
     (iiimp-string-pack (nth 2 im-descriptor))
     (iiimp-pack-number ((len . 32)))
     llanpack)))

(defun iiimp-im-descriptor-unpack (packet start)
  (let (at str1 str2 slot llan len end)
    (setq slot (iiimp-attribid-unpack packet start)
	  at (car slot)
	  start (iiimp-packet-skip-bytes
		 packet (cdr slot) 2)
	  slot (iiimp-string-unpack packet start)
	  str1 (car slot)
	  start (cdr slot)
	  slot (iiimp-string-unpack packet start)
	  str2 (car slot)
	  start (cdr slot))

    (setq start
	  (iiimp-unpack-number ((len . 32))
			       packet start))
    (setq end (+ start len))
    (while (> end start)
      (setq slot (iiimp-language-unpack packet start)
	    start (cdr slot)
	    llan (cons (car slot) llan)))
    (cons (list at str1 str2 llan)
	  start)))

;; <IMATTRIBUTE> := [<ATTRIBID> Pad(2)
;;                   <bytelen of val.> <val.>xn Pad(n)]
;;  -> (input-method-list (INPUTMETHOD_DESCRIPTOR ...))
;;     (object-descriptor-list (OBJECTDESCRIPTOR ...))
;;     (client-descriptor CLIENTDESCRIPTOR)
;;     (ccdef CCDEF)
;;     (jar-gui-object JARFILE_OBJECT)
;;     (jar-lwe-object JARFILE_OBJECT)
;;     (binary-gui-object <BINARY-STRING>)
;;     (binary-lwe-object <BINARY-STRING>)
;;     (script-gui-object <BINARY-STRING>)
;;     (script-lwe-object <BINARY-STRING>)
;;     (ATTRIBID <BINARY-STRING>)
;; PENDING...

(defun iiimp-im-attribute-pack (im-attribute)
  (let ((type (car im-attribute))
	(val (nth 1 im-attribute))
	len)
    (cond ((eq type 'input-method-list)
	   (setq val
		 (iiimp-pack-list
		  (function iiimp-im-descriptor-pack)
		  val)))
	  ((eq type 'object-descriptor-list)
	   (setq val
		 (iiimp-pack-list
		  (function iiimp-object-descriptor-pack)
		  val)))
	  ((eq type 'client-descriptor)
	   (setq val (iiimp-client-descriptor-pack val)))
	  ((eq type 'ccdef)
	   (setq val (iiimp-ccdef-pack val)))
	  ((eq type 'jar-gui-object)
	   (setq val (iiimp-jarfile-object-pack val)))
	  ((eq type 'jar-lwe-object)
	   (setq val (iiimp-jarfile-object-pack val)))
	  ((eq type 'script-gui-object)
	   nil)
	  ((eq type 'script-lwe-object)
	   nil)
	  ((consp type)
	   nil)
	  (t
	   (error "Unknown IMATTRIBUTE:%S" im-attribute)))
    (setq len (length val))
    (concat
     (iiimp-attribid-pack type)
     (iiimp-make-padding 2)
     (iiimp-pack-number ((len . 32)))
     val
     (iiimp-make-padding len))))

(defun iiimp-im-attribute-unpack (packet start)
  (let (at val end len slot)
    (setq slot (iiimp-attribid-unpack packet start)
	  at (car slot)
	  start (iiimp-packet-skip-bytes
		 packet (cdr slot) 2))
    (setq start
	  (iiimp-unpack-number ((len . 32)) packet start)
	  end (+ start len))
    (cond ((consp at)
	   (setq val
		 (iiimp-packet-substring
		  packet start end)))
	  ((eq at 'input-method-list)
	   (setq slot
		 (iiimp-unpack-list
		  packet start len
		  (function iiimp-im-descriptor-unpack))
		 val (car slot)))
	  ((eq at 'object-descriptor-list)
	   (setq slot
		 (iiimp-unpack-list
		  packet start len
		  (function iiimp-object-descriptor-unpack))
		 val (car slot)))
	  ((eq at 'client-descriptor)
	   (setq slot
		 (iiimp-client-descriptor-unpack packet start)
		 val (car slot)))
	  ((eq at 'ccdef)
	   (setq slot
		 (iiimp-ccdef-unpack packet start)
		 val (car slot)))
	  ((eq at 'jar-gui-object)
	   (setq slot
		 (iiimp-jarfile-object-unpack packet start)
		 val (car slot)))
	  ((eq at 'jar-lwe-object)
	   (setq slot
		 (iiimp-jarfile-object-unpack packet start)
		 val (car slot)))
	  ((eq at 'script-gui-object)
	   (setq val
		 (iiimp-packet-substring
		  packet start end)))
	  ((eq at 'script-lwe-object)
	   (setq val
		 (iiimp-packet-substring
		  packet start end)))
	  (t
	   (setq val
		 (iiimp-packet-substring
		  packet start end))))
    (cons
     (list at val)
     (+ end (iiimp-padding-bytes len)))))

;; <ICATTRIBUTE> := [<ATTRIBID> Pad(2)
;;                   <bytelen of val.> <val.>xn Pad(n)]
;;               -> (input-language . <STRING>)
;;                  (character-subsets . <not supported yet>)
;;                  (input-method . <not supported yet>)

(defun iiimp-ic-attribute-pack (ic-attribute)
  (let ((type (car ic-attribute))
	(val (cdr ic-attribute))
	packed len)
    (cond ((eq 'input-language type)
	   (setq packed (iiimp-string-pack val)
		 len (length packed))
	   (concat (iiimp-attribid-pack '(0 1))
		   (iiimp-pack-number ((len . 16)))
		   packed))
	  ((eq 'character-subsets type)
	   (error "NOT SUPPORTED YET.")
	   ;;(setq packed (iiimp-string-pack val))
	   (concat (iiimp-attribid-pack '(0 2))
		   (iiimp-pack-number ((len . 16)))
		   packed
		   (iiimp-make-padding len)))
	  ((eq 'input-method type)
	   (setq packed (iiimp-string-pack val)
		 len (length packed))
	   (concat (iiimp-attribid-pack '(0 3))
		   (iiimp-pack-number ((len . 16)))
		   packed))
	  (t
	   (error "Unknown IC ATTRIBID:%S" type)))))

(defun iiimp-ic-attribute-unpack (packet start)
  (let (at end len slot val)
    (setq slot (iiimp-attribid-unpack packet start)
	  at (car slot)
	  start (cdr slot))
    (setq start
	  (iiimp-unpack-number ((len . 16)) packet start)
	  end (+ start len))
    (setq at (nth 1 at))
    (cond ((= at 1)
	   (setq at 'input-language
		 val (car (iiimp-string-unpack packet start))))
	  ((= at 2)
	   (setq at 'character-subsets
		 val (iiimp-packet-substring
		      packet start end))
	   ;; (error "NOT SUPPORTED YET.")
	   )
	  ((= at 3)
	   (setq at 'input-method
		 val (iiimp-packet-substring
		      packet start end))
	   ;; (error "NOT SUPPORTED YET.")
	   )
	  (t
	   (error "Unknown IC ATTRIBID:%S" at)))
    (cons (cons at val)
	  (+ end (iiimp-padding-bytes len)))))

;; <KEYEVENT> := [<keycode:I32> <keychar:I32>
;;                <keymod:I32> <time stamp:I32>]
(defun iiimp-keyevent-pack (keyevent)
  (let ((kcode (car keyevent))
	(kchar (nth 1 keyevent))
	(kmod (nth 2 keyevent))
	(ts (nth 3 keyevent)))
    (iiimp-pack-number
     (((car kcode) . 16)
      ((cdr kcode) . 16)
      ((car kchar) . 16)
      ((cdr kchar) . 16)
      ((car kmod) . 16)
      ((cdr kmod) . 16)
      ((car ts) . 16)
      ((cdr ts) . 16)))))

(defun iiimp-keyevent-unpack (packet start)
  (let (kcode1 kcode2 kchar1 kchar2
        kmod1 kmod2 ts1 ts2)
    (setq start
	  (iiimp-unpack-number
	   ((kcode1 . 16)
	    (kcode2 . 16)
	    (kchar1 . 16)
	    (kchar2 . 16)
	    (kmod1 . 16)
	    (kmod2 . 16)
	    (ts1 . 16)
	    (ts2 . 16))
	   packet start))
    (cons (list (cons kcode1 kcode2)
		(cons kchar1 kchar2)
		(cons kmod1 kmod2)
		(cons ts1 ts2))
	  start)))

;; <CONTENTS> := [<TYPE:C32>
;;                if type is #0 {<STRING>}
;;                if type is #1 {<TEXT>}
;;                if type is #2 {<bytelen of L.KEYEVENT>
;;                               <L.KEYEVENT>}]
(defun iiimp-contents-pack (contents)
  (let ((type (car contents))
	(con (nth 1 contents))
	tmp)
    (cond ((eq type 'string)
	   (concat (iiimp-pack-number ((0 . 32)))
		   (iiimp-string-pack con)))
	  ((eq type 'text)
	   (concat (iiimp-pack-number ((1 . 32)))
		   (iiimp-text-pack con)))
	  ((eq type 'keyevent)
	   (setq tmp
		 (iiimp-pack-list
		  (function iiimp-keyevent-pack)
		  con))
	   (concat
	    (iiimp-pack-number
	     ((2 . 32) ((length tmp) . 32)))
	    tmp))
	  (t
	   (error "Unknown type in CONTENTS:%S"
		  contents)))))

(defun iiimp-contents-unpack (packet start)
  (let (type slot con end len)
    (setq start
	  (iiimp-unpack-number ((type . 32)) packet start))
    (cond ((= type 0)
	   (setq slot (iiimp-string-unpack packet start)
		 con (car slot)
		 start (cdr slot)
		 type 'string))
	  ((= type 1)
	   (setq slot (iiimp-text-unpack packet start)
		 con (car slot)
		 start (cdr slot)
		 type 'text))
	  ((= type 2)
	   (setq type 'keyevent)
	   (setq start
		 (iiimp-unpack-number ((len . 32))
				      packet start))
	   (setq end (+ start len))
	   (while (> end start)
	     (setq slot (iiimp-keyevent-unpack packet start)
		   start (cdr slot)
		   con (cons (car slot) con))))
	  (t
	   (error "IIIMP error(Unknown type in CONTENTS):%d"
		  type)))
    (cons (list type con)
	  start)))

;; <HOTKEY> := (hotkey-id state-flag action-flag
;;              (keyevents) "<text_label>")
(defun iiimp-hotkey-unpack (packet start)
  (let (slot hotkey-id state-flag action-flag keyevents label len)
    (setq start
	  (iiimp-unpack-number
	   ((hotkey-id . 16)
	    (state-flag . 8)
	    (action-flag . 8)
	    (len . 32))
	   packet start))
    (setq slot
	  (iiimp-unpack-list
	   packet start len
	   (function iiimp-keyevent-unpack))
	  keyevents (car slot)
	  start (cdr slot))
    (setq slot
	  (iiimp-string-unpack packet start)
	  label (car slot)
	  start (cdr slot))
    (cons (list hotkey-id state-flag action-flag keyevents label)
	  start)))

;;;
;;; IIIMP message.
;;;

;; (<opcode> <com-id> <im-id> <ic-id or nil> [<message-specific-data>])
;; |---------message common header(CPART)---|-message speicific part-|

(defsubst iiimp-message-opcode (mes)
  (car mes))

(defsubst iiimp-message-com-id (mes)
  (nth 1 mes))

(defsubst iiimp-message-im-id (mes)
  (nth 2 mes))

(defsubst iiimp-message-ic-id (mes)
  (nth 3 mes))

(defsubst iiimp-message-specific-data (mes)
  (nthcdr 4 mes))

(defun iiimp-show-message (com-id opcode start len)
  (if iiimp-debug-buffer
      (let* ((end (+ start len))
	     (str (iiimp-packet-substring com-id start end)))
	(save-excursion
	  (set-buffer iiimp-debug-buffer)
	  (goto-char (point-max))
	  (insert (format "OpCode:%S(%d bytes) [" opcode len))
	  (iiimp-dump-binary str)
	  (insert "]\n")))))

;;;
;;; Protocol Parser
;;;

(defsubst iiimp-parse-header (com-id start)
  (let (type opcode len)
    (setq start
	  (iiimp-unpack-number ((type . 1) (opcode . 7))
			       com-id start))
    (setq opcode (or (aref iiimp-basic-opcode-vector opcode) opcode))
    (setq start
	  (if (= type 0)
	      (iiimp-unpack-number ((len . 24))
				   com-id start)
	    (iiimp-unpack-number ((len . 56))
				 com-id start)))
    (setq len (* 4 len))
    (cons (list opcode len) start)))

(defsubst iiimp-parse-im-ic-reply-message-internal (com-id opcode start len)
  (let (im-num ic-num)
    (setq start
	  (iiimp-unpack-number
	   ((im-num . 16)
	    (ic-num . 16))
	   com-id start))
    (list opcode com-id
	  (iiimp-construct-im-id com-id im-num)
	  (iiimp-construct-ic-id com-id im-num ic-num))))

(defun iiimp-parse-default (com-id opcode start len)
  (if iiimp-debug-flag
      (iiimp-show-message com-id opcode start len))
  (list opcode com-id nil nil))

(defun iiimp-parse-unexpected (com-id opcode start len)
  (if iiimp-debug-flag
      (iiimp-show-message com-id opcode start len))
  nil)

;; IM_CONNECT_REPLY => (<CPART> <language-list>)

(defsubst iiimp-message-im-language-list (mes)
  (nth 4 mes))

(defun iiimp-parse-im-connect-reply (com-id opcode start len)
  (let (slot im-num lln)
    (setq start
	  (iiimp-unpack-number
	   ((im-num . 16)
	    (len . 16))
	   com-id start))
    (setq slot
	  (iiimp-unpack-list
	   com-id start len
	   (function iiimp-string-unpack))
	  lln (car slot)
	  start (cdr slot))
    (list 'iiimp-im-connect-reply
	  com-id
	  (iiimp-construct-im-id com-id im-num)
	  nil ; IC is not specified.
	  lln)))

;; IM_DISCONNECT_REPLY => (<CPART>)

(defun iiimp-parse-im-disconnect-reply (com-id opcode start len)
  (let (im-num dum)
    (setq start
	  (iiimp-unpack-number
	   ((im-num . 16)
	    (dum . 16))
	   com-id start))
    (list 'iiimp-im-disconnect-reply
	  com-id
	  (iiimp-construct-im-id com-id im-num)
	  nil)))

;; IM_REGISTER_TRIGGER_KEYS => (<CPART> <keyevent-list(on)>
;;                                      <keyevent-list(off)>)

(defsubst iiimp-message-trigger-on-key-list (mes)
  (nth 4 mes))
(defsubst iiimp-message-trigger-off-key-list (mes)
  (nth 5 mes))

(defun iiimp-parse-im-register-trigger-keys (com-id opcode start len)
  (let (slot im-num dum len kls-on kls-off)
    (setq start
	  (iiimp-unpack-number
	   ((im-num . 16)
	    (dum . 16)
	    (len . 32))
	   com-id start))
    (setq slot
	  (iiimp-unpack-list
	   com-id start len
	   (function iiimp-keyevent-unpack))
	  kls-on (car slot)
	  start (cdr slot))
    (setq start
	  (iiimp-unpack-number
	   ((len . 32))
	   com-id start))
    (setq slot
	  (iiimp-unpack-list
	   com-id start len
	   (function iiimp-keyevent-unpack))
	  kls-off (car slot)
	  start (cdr slot))
    (list 'iiimp-im-register-trigger-keys
	  com-id
	  (iiimp-construct-im-id com-id im-num)
	  nil ; IC is not specified.
	  kls-on kls-off)))

;; IM_TRIGGER_NOTIFY => (<CPART> <flag>)

(defsubst iiimp-message-trigger-notify-flag (mes)
  (nth 4 mes))

(defun iiimp-parse-im-trigger-notify (com-id opcode start len)
  (let (im-num ic-num flag dum)
    (setq start
	  (iiimp-unpack-number
	   ((im-num . 16)
	    (ic-num . 16)
	    (flag . 16)
	    (dum . 16))
	   com-id start))
    (list 'iiimp-im-trigger-notify
	  com-id
	  (iiimp-construct-im-id com-id im-num)
	  (iiimp-construct-ic-id com-id im-num ic-num)
	  (cond ((= flag 0) t)
		((= flag 1) nil)
		(t (error "IIIM Protocol error:IM_TRIGGER_NOTIFY(flag):%S"
			  flag))))))

;; IM_TRIGGER_NOTIFY_REPLY => (<CPART>)

(defun iiimp-parse-im-trigger-notify-reply (com-id opcode start len)
  (iiimp-parse-im-ic-reply-message-internal
   com-id opcode start len))

;; IM_PROTOCOL_VERSION => (<CPART> <version>)
(defun iiimp-parse-im-protocol-version (com-id opcode start len)
  (let (im-num ver dum)
    (setq start
	  (iiimp-unpack-number
	   ((im-num . 16)
	    (ver . 8)
	    (dum . 8))
	   com-id start))
    (list 'iiimp-im-version
	  com-id
	  (iiimp-construct-im-id com-id im-num)
	  ver)))


;;; get/set IM values

;; IM_GET/SET_IMVALUES => (<CPART> <im-attribtute-list>)

(defsubst iiimp-message-im-attribute-list (mes)
  (nth 4 mes))

(defun iiimp-parse-imvalues-internal (com-id opcode start len)
  (let (slot im-num dum lia)
    (setq start
	  (iiimp-unpack-number
	   ((im-num . 16)
	    (dum . 16)
	    (len . 32))
	   com-id start))
    (setq slot
	  (iiimp-unpack-list
	   com-id start len
	   (function iiimp-im-attribute-unpack))
	  lia (car slot)
	  start (cdr slot))
    (list opcode com-id
	  (iiimp-construct-im-id com-id im-num)
	  nil ; IC is not specified.
	  lia)))

(defun iiimp-parse-im-getimvalues-reply (com-id opcode start len)
  (iiimp-parse-imvalues-internal com-id opcode start len))

(defun iiimp-parse-im-setimvalues (com-id opcode start len)
  (iiimp-parse-imvalues-internal com-id opcode start len))

(defun iiimp-parse-im-setimvalues-reply (com-id opcode start len)
  (let (im-num dum)
    (setq start
	  (iiimp-unpack-number
	   ((im-num . 16)
	    (dum . 16))
	   com-id start))
    (list 'iiimp-im-setimvalues-reply
	  com-id
	  (iiimp-construct-im-id com-id im-num)
	  nil ; IC is not specified.
	  )))

;;; IC messages

(defun iiimp-parse-im-createic-reply (com-id opcode start len)
  (iiimp-parse-im-ic-reply-message-internal
   com-id opcode start len))

(defun iiimp-parse-im-destroyic-reply (com-id opcode start len)
  (iiimp-parse-im-ic-reply-message-internal
   com-id opcode start len))

(defun iiimp-parse-im-seticvalues-reply (com-id opcode start len)
  (iiimp-parse-im-ic-reply-message-internal
   com-id opcode start len))

;; IM_GETICVALUES_REPLY => (<CPART> <ic-attribute-list>)

(defsubst iiimp-message-ic-attribute-list (mes)
  (nth 4 mes))

(defun iiimp-parse-im-geticvalues-reply (com-id opcode start len)
  (let (slot im-num ic-num flag lia)
    (setq start
	  (iiimp-unpack-number
	   ((im-num . 16)
	    (ic-num . 16)
	    (len . 16))
	   com-id start))
    (setq slot
	  (iiimp-unpack-list
	   com-id start len
	   (function iiimp-icattribute-unpack))
	  lia (car slot)
	  start (cdr slot))
    (list opcode
	  com-id
	  (iiimp-construct-im-id com-id im-num)
	  (iiimp-construct-ic-id com-id im-num ic-num)
	  lia)))

(defun iiimp-parse-im-seticfocus-reply (com-id opcode start len)
  (iiimp-parse-im-ic-reply-message-internal
   com-id opcode start len))

(defun iiimp-parse-im-unseticfocus-reply (com-id opcode start len)
  (iiimp-parse-im-ic-reply-message-internal
   com-id opcode start len))

(defun iiimp-parse-im-resetic-reply (com-id opcode start len)
  (iiimp-parse-im-ic-reply-message-internal
   com-id opcode start len))

;;; forward event

;; IM_FORWARD_EVENT => (<CPART> <contents>)

(defsubst iiimp-message-forward-event-contents (mes)
  (nth 4 mes))

(defun iiimp-parse-im-forward-event (com-id opcode start len)
  (let (im-num ic-num contents)
    (setq start
	  (iiimp-unpack-number
	   ((im-num . 16)
	    (ic-num . 16))
	   com-id start))
    (setq contents (car (iiimp-contents-unpack com-id start)))
    (list 'iiimp-im-forward-event
	  com-id
	  (iiimp-construct-im-id com-id im-num)
	  (iiimp-construct-ic-id com-id im-num ic-num)
	  contents)))

(defun iiimp-parse-im-forward-event-reply (com-id opcode start len)
  (iiimp-parse-im-ic-reply-message-internal
   com-id opcode start len))

;; IM_FORWARD_EVENT_WITH_OPERATIONS => (<CPART> <contents> <operations>)

(defsubst iiimp-message-forward-event-operations (mes)
  (nth 5 mes))

(defun iiimp-parse-im-forward-event-with-operations (com-id opcode start len)
  (let (im-num ic-num contents len ops)
    (setq start
	  (iiimp-unpack-number
	   ((im-num . 16)
	    (ic-num . 16))
	   com-id start))
    (setq contents (iiimp-contents-unpack com-id start)
	  start (cdr contents)
	  contents (car contents))
    (setq start
	  (iiimp-unpack-number
	   ((len . 32))
	   com-id start))
    (setq ops
	  (car (iiimp-unpack-list
		com-id start len
		(function iiimp-operation-unpack))))
    (list 'iiimp-im-forward-event-with-operations
	  com-id
	  (iiimp-construct-im-id com-id im-num)
	  (iiimp-construct-ic-id com-id im-num ic-num)
	  contents
	  ops)))

;; IM_FORWARD_EVENT_WITH_OPERATIONS_REPLY
;;            => (<CPART> nil <operations>)

(defun iiimp-parse-im-forward-event-with-operations-reply
  (com-id opcode start len)
  (let (im-num ic-num len ops)
    (setq start
	  (iiimp-unpack-number
	   ((im-num . 16)
	    (ic-num . 16)
	    (len . 32))
	   com-id start))
    (setq ops
	  (car (iiimp-unpack-list
		com-id start len
		(function iiimp-operation-unpack))))
    (list 'iiimp-im-forward-event-with-operations-reply
	  com-id
	  (iiimp-construct-im-id com-id im-num)
	  (iiimp-construct-ic-id com-id im-num ic-num)
	  nil
	  ops)))

;; IM_COMMIT_STRING => (<CPART> <contents>)

(defsubst iiimp-message-committed-string (mes)
  (nth 4 mes))

(defun iiimp-parse-im-commit-string (com-id opcode start len)
  (let (im-num ic-num contents)
    (setq start
	  (iiimp-unpack-number
	   ((im-num . 16)
	    (ic-num . 16))
	   com-id start))
    (setq contents (car (iiimp-contents-unpack com-id start)))
    (list 'iiimp-im-commit-string
	  com-id
	  (iiimp-construct-im-id com-id im-num)
	  (iiimp-construct-ic-id com-id im-num ic-num)
	  contents)))

;; IM_PREEDIT_START => (<CPART>)

(defun iiimp-parse-im-preedit-start (com-id opcode start len)
  (iiimp-parse-im-ic-reply-message-internal
   com-id opcode start len))

;; IM_PREEDIT_DRAW => (<CPART>
;;                     [<caretpos> <chg-st> <chg-len> <contents>])

;; ensure the result is [<caretpos> <chg-st> <chg-len> <contents>].
(defsubst iiimp-message-preedit-draw-data (mes)
  (nth 4 mes))

(defun iiimp-parse-im-preedit-draw (com-id opcode start len)
  (let (im-num ic-num caret chg-first chg-len contents)
    (setq start
	  (iiimp-unpack-number
	   ((im-num . 16)
	    (ic-num . 16)
	    (caret . 32)
	    (chg-first . 32)
	    (chg-len . 32))
	   com-id start))
    (setq contents (car (iiimp-contents-unpack com-id start)))
    (list 'iiimp-im-preedit-draw
	  com-id
	  (iiimp-construct-im-id com-id im-num)
	  (iiimp-construct-ic-id com-id im-num ic-num)
	  (vector caret chg-first chg-len contents)
	  ;; message ends here.
	  )))

(defun iiimp-parse-im-preedit-done (com-id opcode start len)
  (iiimp-parse-im-ic-reply-message-internal
   com-id opcode start len))

(defun iiimp-parse-im-status-start (com-id opcode start len)
  (iiimp-parse-im-ic-reply-message-internal
   com-id opcode start len))

;; IM_STATUS_DRAW => (<CPART> <contents>)

(defsubst iiimp-message-status-draw-contents (mes)
  (nth 4 mes))

(defun iiimp-parse-im-status-draw (com-id opcode start len)
  (let (im-num ic-num contents)
    (setq start
	  (iiimp-unpack-number
	   ((im-num . 16)
	    (ic-num . 16))
	   com-id start))
    (setq contents (car (iiimp-contents-unpack com-id start)))
    (list 'iiimp-im-status-draw
	  com-id
	  (iiimp-construct-im-id com-id im-num)
	  (iiimp-construct-ic-id com-id im-num ic-num)
	  contents)))

(defun iiimp-parse-im-status-done (com-id opcode start len)
  (iiimp-parse-im-ic-reply-message-internal
   com-id opcode start len))

;;; LOOKUP messages.

;; IM_LOOKUP_CHOICE_START =>
;;      (<CPART>
;;       [<masterp> <choices-num> <rows> <cols> <direction> <labelownp>])

;; ensure the result is (<masterp> <choices-num> <rows> <cols>
;;                       <direction> <labelownp>)
(defsubst iiimp-message-lookup-choice-start-data (mes)
  (nth 4 mes))

(defun iiimp-parse-im-lookup-choice-start (com-id opcode start len)
  (let (im-num ic-num masterp choices-num
	rows cols dir labelownp)
    (setq start
	  (iiimp-unpack-number
	   ((im-num . 16)
	    (ic-num . 16)
	    (masterp . 16)
	    (choices-num . 16)
	    (rows . 16)
	    (cols . 16)
	    (dir . 16)
	    (labelownp . 16))
	   com-id start))
    (setq masterp (= masterp 2))
    (setq dir (if (= dir 0) 'horizontal 'vertical))
    (setq labelownp (if (= labelownp 0) nil t))
    (list 'iiimp-im-lookup-choice-start
	  com-id
	  (iiimp-construct-im-id com-id im-num)
	  (iiimp-construct-ic-id com-id im-num ic-num)
	  (vector masterp choices-num rows cols dir labelownp)
	  ;; message ends here
	  )))

;; IM_LOOKUP_CHOICE_DRAW =>
;;      (<CPART>
;;       [<idxfirst> <idxlast> <idxcurrent>
;;        <choice-list> <label-list> <title>])

;; ensure the result is [<idxfirst> <idxlast> <idxcurrent>
;;                       <choice-list> <label-list> <title>]
(defsubst iiimp-message-lookup-choice-draw-data (mes)
  (nth 4 mes))

(defun iiimp-parse-im-lookup-choice-draw (com-id opcode start len)
  (let (slot im-num ic-num
	idxfirst idxlast idxcur
	chlist chlistlen lblist lblistlen
	title)
    (setq start
	  (iiimp-unpack-number
	   ((im-num . 16)
	    (ic-num . 16)
	    (idxfirst . 32)
	    (idxlast . 32)
	    (idxcur . 32)
	    (chlistlen . 32))
	   com-id start))
    (setq slot (iiimp-unpack-list
		com-id start chlistlen
		(function iiimp-text-unpack))
	  chlist (car slot)
	  start (cdr slot))
    (setq start
	  (iiimp-unpack-number
	   ((lblistlen . 32))
	   com-id start))
    (setq slot (iiimp-unpack-list
		com-id start lblistlen
		(function iiimp-text-unpack))
	  lblist (car slot)
	  start (cdr slot))
    (setq title (car (iiimp-text-unpack com-id start)))
    (list 'iiimp-im-lookup-choice-draw
	  com-id
	  (iiimp-construct-im-id com-id im-num)
	  (iiimp-construct-ic-id com-id im-num ic-num)
	  (vector idxfirst idxlast idxcur
		  chlist lblist title)
	  ;; message ends here.
	  )))

;; IM_LOOKUP_CHOICE_PROCESS => (<CPART> <type> <value>)

(defsubst iiimp-message-lookup-choice-process-type (mes)
  (nth 4 mes))

(defsubst iiimp-message-lookup-choice-process-value (mes)
  (nth 5 mes))

(defun iiimp-parse-im-lookup-choice-process (com-id opcode start len)
  (let (im-num ic-num type val)
    (setq start
	  (iiimp-unpack-number
	   ((im-num . 16)
	    (ic-num . 16)
	    (type . 16)
	    (val . 16))
	   com-id start))
    (if (= type 0)
	(setq type 'index)
      (setq type 'page)
      (cond ((= val 1)
	     (setq val 'next))
	    ((= val 2)
	     (setq val 'prev))
	    ((= val 3)
	     (setq val 'fist))
	    (t
	     (setq val 'last))))
    (list 'iiimp-im-lookup-choice-process
	  com-id
	  (iiimp-construct-im-id com-id im-num)
	  (iiimp-construct-ic-id com-id im-num ic-num)
	  type val)))

(defun iiimp-parse-im-lookup-choice-done (com-id opcode start len)
  (iiimp-parse-im-ic-reply-message-internal
   com-id opcode start len))

;;; AUX messages.

;; IM_AUX_* => (<CPART> <window-id> <im-name>
;;                      [<specific data of aux-draw>])

(defsubst iiimp-message-aux-window-id (mes)
  (nth 4 mes))

(defsubst iiimp-message-aux-im-name (mes)
  (nth 5 mes))

;; (iiimp-im-aux-start
;;   <COM-ID> <IM-ID> <IC-ID>
;;   (<WindowClassID1> . <WindowClassID2>)
;;   <STR:InputMethodName>)

(defun iiimp-parse-im-aux-start (com-id opcode start len)
  (let (im-num ic-num wid1 wid2 im-name)
    (setq start
	  (iiimp-unpack-number
	   ((im-num . 16)
	    (ic-num . 16)
	    (wid1 . 16)
	    (wid2 . 16))
	   com-id start))
    (setq im-name (car (iiimp-string-unpack com-id start)))
    (list 'iiimp-im-aux-start
	  com-id
	  (iiimp-construct-im-id com-id im-num)
	  (iiimp-construct-ic-id com-id im-num ic-num)
	  (cons wid1 wid2)
	  im-name)))

;; (iiimp-im-aux-draw
;;   <COM-ID> <IM-ID> <IC-ID>
;;   (<WindowClassID1> . <WindowClassID2>)
;;   <STR:InputMethodName>
;;   ((NUM-MSW . NUM-LSW) ...)
;;   (STR STR ...))

(defsubst iiimp-message-aux-int-list (mes)
  (nth 6 mes))

(defsubst iiimp-message-aux-string-list (mes)
  (nth 7 mes))

(defun iiimp-parse-im-aux-draw (com-id opcode start len)
  (let (im-num ic-num wid1 wid2 slot im-name
	num1 num2 len ivals svals)
    (setq start
	  (iiimp-unpack-number
	   ((im-num . 16)
	    (ic-num . 16)
	    (wid1 . 16)
	    (wid2 . 16))
	   com-id start))
    (setq slot (iiimp-string-unpack com-id start)
	  im-name (car slot)
	  start (cdr slot))
    (setq start
	  (iiimp-unpack-number
	   ((len . 32))
	   com-id start))
    (setq slot (iiimp-unpack-list
		com-id start len
		(lambda (p s)
		  (setq s
			(iiimp-unpack-number
			 ((num1 . 16)
			  (num2 . 16))
			 p s))
		  (cons (cons num1 num2)
			s)))
	  ivals (car slot)
	  start (cdr slot))
    (setq start
	  (iiimp-unpack-number
	   ((len . 32))
	   com-id start))
    (setq svals (car (iiimp-unpack-list
		      com-id start len
		      (function iiimp-string-unpack))))
    (list 'iiimp-im-aux-draw
	  com-id
	  (iiimp-construct-im-id com-id im-num)
	  (iiimp-construct-ic-id com-id im-num ic-num)
	  (cons wid1 wid2)
	  im-name ivals svals)))
  
;; (iiimp-im-aux-done
;;   <COM-ID> <IM-ID> <IC-ID>
;;   (<WindowClassID1> . <WindowClassID2>)
;;   <STR:InputMethodName>)
(defun iiimp-parse-im-aux-done (com-id opcode start len)
  (let (im-num ic-num wid1 wid2 im-name)
    (setq start
	  (iiimp-unpack-number
	   ((im-num . 16)
	    (ic-num . 16)
	    (wid1 . 16)
	    (wid2 . 16))
	   com-id start))
    (setq im-name (car (iiimp-string-unpack com-id start)))
    (list 'iiimp-im-aux-done
	  com-id
	  (iiimp-construct-im-id com-id im-num)
	  (iiimp-construct-ic-id com-id im-num ic-num)
	  (cons wid1 wid2)
	  im-name)))

;; (iiimp-im-aux-setvalues-reply
;;   <COM-ID> <IM-ID> <IC-ID>
;;   (<WindowClassID1> . <WindowClassID2>)
;;   <STR:InputMethodName>)
(defun iiimp-parse-im-aux-setvalues-reply (com-id opcode start len)
  (let (im-num ic-num wid1 wid2 im-name)
    (setq start
	  (iiimp-unpack-number
	   ((im-num . 16)
	    (ic-num . 16)
	    (wid1 . 16)
	    (wid2 . 16))
	   com-id start))
    (setq im-name (car (iiimp-string-unpack com-id start)))
    (list 'iiimp-im-aux-setvalues-reply
	  com-id
	  (iiimp-construct-im-id com-id im-num)
	  (iiimp-construct-ic-id com-id im-num ic-num)
	  (cons wid1 wid2)
	  im-name)))

;;; Hotkey Handlers

;; IM_REGISTER_HOTKEYS
(defun iiimp-parse-im-register-hotkeys (com-id opcode start len)
  (let (slot im-num dum scope profile-id len hotkeys)
    (setq start
	  (iiimp-unpack-number
	   ((im-num . 16)
	    (dum . 16)
	    (profile-id . 16)
	    (len . 16))
	   com-id start))
    (setq scope (/ profile-id 128)) ;; MSB is scope.
    (setq profile-id (logand profile-id 127))
    (setq slot
	  (iiimp-unpack-list
	   com-id start len
	   (function iiimp-hotkey-unpack))
	  hotkeys (car slot))
    (list 'iiimp-im-register-hotkeys
	  com-id
	  (iiimp-construct-im-id com-id im-num)
	  nil ; IC is not specified
	  scope profile-id hotkeys)))

(defun iiimp-parse-im-hotkey-notify (com-id opcode start len)
  (let (im-num ic-num hotkey-id hotkey-idx)
    (setq start
	  (iiimp-unpack-number
	   ((im-num . 16)
	    (ic-num . 16)
	    (hotkey-id . 16)
	    (hotkey-idx . 16))
	   com-id start))
    (list 'iiimp-im-hotkey-notify
	  com-id
	  (iiimp-construct-im-id com-id im-num)
	  (iiimp-construct-ic-id com-id im-num ic-num)
	  hotkey-id hotkey-idx)))

(defun iiimp-parse-im-hotkey-notify-reply (com-id opcode start len)
  (iiimp-parse-im-ic-reply-message-internal
   com-id opcode start len))

(defun iiimp-parse-im-select-hotkey-profile (com-id opcode start len)
  (let (im-num dum scope-and-profile-id)
    (setq start
	  (iiimp-unpack-number
	   ((im-num . 16)
	    (dum . 16)
	    (scope-and-profile-id . 16))
	   com-id start))
    (list 'iiimp-im-select-hotkey-profile
	  com-id
	  (iiimp-construct-im-id com-id im-num)
	  nil ; IC is not specified
	  scope-and-profile-id)))

(defun iiimp-parse-im-hotkey-state-notify (com-id opcode start len)
  (let (im-num ic-num dum state)
    (setq start
	  (iiimp-unpack-number
	   ((im-num . 16)
	    (ic-num . 16)
	    (dum . 16)
	    (state . 16))
	   com-id start))
    (list 'iiimp-im-hotkey-state-notify
	  com-id
	  (iiimp-construct-im-id com-id im-num)
	  (iiimp-construct-ic-id com-id im-num ic-num)
	  state)))

(defsubst iiimp-message-register-hotkeys-scope (mes)
  (nth 4 mes))
(defsubst iiimp-message-register-hotkeys-profie-id (mes)
  (nth 5 mes))
(defsubst iiimp-message-register-hotkeys-hotkeys (mes)
  (nth 6 mes))

(defsubst iiimp-message-select-hotkey-profile-id (mes)
  (nth 4 mes))

;;;
;;; Protocol Handler/Event Dispatcher
;;;

(defun iiimp-select-message-parser (opcode)
  (let* ((slot (iiimp-opcode-spec opcode))
	 (hh (nth 2 slot)))
    (or (cdr hh)
	(if (integerp slot)
	    (function iiimp-parse-default) 
	  (function iiimp-parse-unexpected)))))

(defun iiimp-wait-message (com-id
			   &optional
			   wait intercept-message not-discard)
  (let ((start (iiimp-com-id-end-point com-id))
	st2 type opcode len parser val)
    (iiimp-communication
     com-id
     (if (not (iiimp-check-channel-connection com-id))
	 (iiimp-connection-error "wait-message" com-id)
       ;; Has a header already arrived?
       (if (iiimp-check-channel-bytes com-id start 4 wait)
	   (progn
	     (setq val (iiimp-parse-header com-id start)
		   st2 (cdr val)
		   opcode (car (car val))
		   len (nth 1 (car val)))
	     (setq val (funcall
			(iiimp-select-message-parser opcode)
			com-id opcode st2 len))
	     (if (not not-discard)
		 (if (not iiimp-debug-flag)
		     (iiimp-packet-erase com-id (+ st2 len))
		   (iiimp-record-end-point com-id (+ st2 len))))
	     val)
	 nil)))))

;;;
;;; Send message
;;;

(defun iiimp-make-header (opcode packed)
  (let ((len (/ (length packed) 4)))
    (concat
     (iiimp-pack-number ((0 . 1) ((iiimp-opcode-number opcode) . 7)
			 (len . 24)))
     packed)))

(defsubst iiimp-send-im-ic-request-internal (com-id opcode im-id ic-id)
  (iiimp-write-channel
   com-id
   (iiimp-make-header
    opcode
    (iiimp-pack-im-ic im-id ic-id))))

(defun iiimp-send-im-connect (com-id username &optional authlist)
  (let ((authlistpack (iiimp-pack-list
		       (function iiimp-string-pack)
		       authlist)))
  (iiimp-write-channel
   com-id
   (iiimp-make-header
    'iiimp-im-connect
    (concat
     (iiimp-pack-number ((?B . 8) (iiimp-protocol-version . 8)))
     (iiimp-string-pack username)
     (iiimp-pack-number (((length authlistpack) . 16)))
     authlistpack)))))

(defun iiimp-send-im-disconnect (com-id im-id)
  (iiimp-write-channel
   com-id
   (iiimp-make-header
    'iiimp-im-disconnect
    (concat
     (iiimp-pack-im-ic im-id)
     (iiimp-pack-number ((0 . 16)))))))

(defun iiimp-send-im-trigger-notify (com-id im-id ic-id flag)
  (iiimp-write-channel
   com-id
   (iiimp-make-header
    'iiimp-im-trigger-notify
    (concat
     (iiimp-pack-im-ic im-id ic-id)
     (if flag
	(iiimp-pack-number
	 ((0 . 16) (0 . 16)))
      (iiimp-pack-number
       ((1 . 16) (0 . 16))))))))

(defun iiimp-send-im-trigger-notify-reply (com-id im-id ic-id)
  (iiimp-send-im-ic-request-internal
   com-id 'iiimp-im-trigger-notify-reply im-id ic-id))

(defun iiimp-send-im-setimvalues (com-id im-id im-attrs)
  (let* ((packed (iiimp-pack-list
		  (function iiimp-im-attribute-pack)
		  im-attrs))
	 (len (length packed)))
    (iiimp-write-channel
     com-id
     (iiimp-make-header
      'iiimp-im-setimvalues
      (concat
       (iiimp-pack-im-ic im-id)
       (iiimp-pack-number
	((0 . 16) ;; for padding
	 (len . 32)))
       packed)))))

(defun iiimp-send-im-setimvalues-reply (com-id im-id)
  (iiimp-write-channel
   com-id
   (iiimp-make-header
    'iiimp-im-setimvalues-reply
    (concat
     (iiimp-pack-im-ic im-id)
     (iiimp-pack-number ((0 . 16)) ;; for padding
     )))))

(defun iiimp-send-im-getimvalues (com-id im-id request-attrs)
  (let* ((packed (iiimp-request-attrs-pack
		  request-attrs))
	 (len (length packed)))
    (iiimp-write-channel
     com-id
     (iiimp-make-header
      'iiimp-im-getimvalues
      (concat
       (iiimp-pack-im-ic im-id)
       (iiimp-pack-number
	((0 . 16) ;; for padding
	 (len . 32)))
       packed
       (iiimp-make-padding len))))))

(defun iiimp-send-im-createic (com-id im-id ic-attrs)
  (let* ((packed (iiimp-pack-list
		  (function iiimp-ic-attribute-pack)
		  ic-attrs))
	 (len (length packed)))
    (iiimp-write-channel
     com-id
     (iiimp-make-header
      'iiimp-im-createic
      (concat
       (iiimp-pack-im-ic im-id)
       (iiimp-pack-number
	((len . 16)))
       packed)))))

(defun iiimp-send-im-destroyic (com-id im-id ic-id)
  (iiimp-send-im-ic-request-internal
   com-id 'iiimp-im-destroyic im-id ic-id))

(defun iiimp-send-im-seticvalues (com-id im-id ic-id ic-attrs)
  (let* ((packed (iiimp-pack-list
		  (function iiimp-ic-attribute-pack)
		  ic-attrs))
	 (len (length packed)))
    (iiimp-write-channel
     com-id
     (iiimp-make-header
      'iiimp-im-seticvalues
      (concat
       (iiimp-pack-im-ic im-id ic-id)
       (iiimp-pack-number ((len . 16)))
       packed
       (iiimp-make-padding 2))))))

(defun iiimp-send-im-geticvalues (com-id im-id ic-id request-attrs)
  (let* ((packed (iiimp-request-attrs-pack
		  request-attrs))
	 (len (length packed)))
    (iiimp-write-channel
     com-id
     (iiimp-make-header
      'iiimp-im-geticvalues
      (concat
       (iiimp-pack-im-ic im-id ic-id)
       (iiimp-pack-number
	((len . 16)))
       packed
       (iiimp-make-padding (+ 2 len)))))))

(defun iiimp-send-im-seticfocus (com-id im-id ic-id)
  (iiimp-send-im-ic-request-internal
   com-id 'iiimp-im-seticfocus im-id ic-id))

(defun iiimp-send-im-unseticfocus (com-id im-id ic-id)
  (iiimp-send-im-ic-request-internal
   com-id 'iiimp-im-unseticfocus im-id ic-id))

(defun iiimp-send-im-resetic (com-id im-id ic-id)
  (iiimp-send-im-ic-request-internal
   com-id 'iiimp-im-resetic im-id ic-id))

(defun iiimp-send-im-forward-event (com-id im-id ic-id contents)
  (iiimp-write-channel
   com-id
   (iiimp-make-header
    'iiimp-im-forward-event
    (concat
     (iiimp-pack-im-ic im-id ic-id)
     (iiimp-contents-pack contents)))))

(defun iiimp-send-im-forward-event-reply (com-id im-id ic-id)
  (iiimp-send-im-ic-request-internal
   com-id 'iiimp-im-forward-event-reply im-id ic-id))

(defun iiimp-send-im-forward-event-with-operations
  (com-id im-id ic-id contents operations)
  (let* ((packed (iiimp-pack-list
		  (function iiimp-operation-pack)
		  operations))
	 (len (length packed)))
  (iiimp-write-channel
   com-id
   (iiimp-make-header
    'iiimp-im-forward-event-with-operations
    (concat
     (iiimp-pack-im-ic im-id ic-id)
     (iiimp-contents-pack contents)
     (iiimp-pack-number ((len . 32)))
     packed)))))

(defun iiimp-send-im-forward-event-with-operations-reply
  (com-id im-id ic-id operations)
  (let* ((packed (iiimp-pack-list
		  (function iiimp-operation-pack)
		  operations))
	 (len (length packed)))
  (iiimp-write-channel
   com-id
   (iiimp-make-header
    'iiimp-im-forward-event-with-operations
    (concat
     (iiimp-pack-im-ic im-id ic-id)
     (iiimp-pack-number ((len . 32)))
     packed)))))

(defun iiimp-send-im-preedit-start-reply
  (com-id im-id ic-id &optional maxlen)
  (iiimp-write-channel
   com-id
   (iiimp-make-header
    'iiimp-im-preedit-start-reply
    (concat
     (iiimp-pack-im-ic im-id ic-id)
     (if (null maxlen)
	 (iiimp-pack-number ((65535 . 16) (65535 . 16)))
       (iiimp-pack-number ((maxlen . 32))))))))

(defun iiimp-send-im-preedit-draw-reply (com-id im-id ic-id)
  (iiimp-send-im-ic-request-internal
   com-id 'iiimp-im-preedit-draw-reply im-id ic-id))

(defun iiimp-send-im-preedit-done-reply (com-id im-id ic-id)
  (iiimp-send-im-ic-request-internal
   com-id 'iiimp-im-preedit-done-reply im-id ic-id))

(defun iiimp-send-im-status-start-reply (com-id im-id ic-id)
  (iiimp-send-im-ic-request-internal
   com-id 'iiimp-im-status-start-reply im-id ic-id))

(defun iiimp-send-im-status-draw-reply (com-id im-id ic-id)
  (iiimp-send-im-ic-request-internal
   com-id 'iiimp-im-status-draw-reply im-id ic-id))

(defun iiimp-send-im-status-done-reply (com-id im-id ic-id)
  (iiimp-send-im-ic-request-internal
   com-id 'iiimp-im-status-done-reply im-id ic-id))

;;; LOOKUP messages.
(defun iiimp-send-im-lookup-choice-start-reply (com-id im-id ic-id)
  (iiimp-send-im-ic-request-internal
   com-id 'iiimp-im-lookup-choice-start-reply im-id ic-id))

(defun iiimp-send-im-lookup-choice-draw-reply (com-id im-id ic-id)
  (iiimp-send-im-ic-request-internal
   com-id 'iiimp-im-lookup-choice-draw-reply im-id ic-id))

(defun iiimp-send-im-lookup-choice-process-reply (com-id im-id ic-id)
  (iiimp-send-im-ic-request-internal
   com-id 'iiimp-im-lookup-choice-process-reply im-id ic-id))

(defun iiimp-send-im-lookup-choice-done-reply (com-id im-id ic-id)
  (iiimp-send-im-ic-request-internal
   com-id 'iiimp-im-lookup-choice-done-reply im-id ic-id))


;;; AUX messages.

(defun iiimp-send-im-aux-start-reply
  (com-id im-id ic-id wid im-name)
  (iiimp-write-channel
   com-id
   (iiimp-make-header
    'iiimp-im-aux-start-reply
    (concat
     (iiimp-pack-im-ic im-id ic-id)
     (iiimp-pack-number
      (((car wid) . 16) ((cdr wid) . 16)))
     (iiimp-string-pack im-name)))))

(defun iiimp-send-im-aux-draw-reply
  (com-id im-id ic-id wid im-name)
  (iiimp-write-channel
   com-id
   (iiimp-make-header
    'iiimp-im-aux-draw-reply
    (concat
     (iiimp-pack-im-ic im-id ic-id)
     (iiimp-pack-number
      (((car wid) . 16) ((cdr wid) . 16)))
     (iiimp-string-pack im-name)))))

(defun iiimp-send-im-aux-done-reply
  (com-id im-id ic-id wid im-name)
  (iiimp-write-channel
   com-id
   (iiimp-make-header
    'iiimp-im-aux-done-reply
    (concat
     (iiimp-pack-im-ic im-id ic-id)
     (iiimp-pack-number
      (((car wid) . 16) ((cdr wid) . 16)))
     (iiimp-string-pack im-name)))))

(defun iiimp-send-im-aux-setvalues
  (com-id im-id ic-id wid im-name dword-bin strlist)
  (let* ((dwlen (length dword-bin))
	 (packed (iiimp-pack-list
		  (function iiimp-string-pack)
		  strlist))
	 (sl-len (length packed)))
  (if (/= 0 (% dwlen 4))
      (error "DWORD-BIN must be aligned to 4-octets boundary."))
  (iiimp-write-channel
   com-id
   (iiimp-make-header
    'iiimp-im-aux-setvalues
    (concat
     (iiimp-pack-im-ic im-id ic-id)
     (iiimp-pack-number
      (((car wid) . 16) ((cdr wid) . 16)))
     (iiimp-string-pack im-name)
     (iiimp-pack-number
      ((dwlen . 32)))
     dword-bin
     (iiimp-pack-number
      ((sl-len . 32)))
     packed)))))

;;; HOTKEY messages

(defun iiimp-send-im-hotkey-notify (com-id im-id ic-id hotkey-id hotkey-idx)
  (iiimp-write-channel
   com-id
   (iiimp-make-header
    'iiimp-im-hotkey-notify
    (concat
     (iiimp-pack-im-ic im-id ic-id)
     (iiimp-pack-number
       ((hotkey-id . 16) (hotkey-idx . 16)))))))

(defun iiimp-send-im-hotkey-notify-reply (com-id im-id ic-id)
  (iiimp-send-im-ic-request-internal
   com-id 'iiimp-im-hotkey-notify-reply im-id ic-id))

(defun iiimp-send-im-hotkey-state-notify-reply (com-id im-id ic-id)
  (iiimp-send-im-ic-request-internal
   com-id 'iiimp-im-hotkey-state-notify-reply im-id ic-id))

;;; IIIMP message sending API.

(defun iiimp-send-message (msg com-id &rest val)
  (iiimp-add-debug-log (format "=> [%S]:%S\n" msg val))
  (if (iiimp-check-channel-connection com-id)
      (apply (iiimp-opcode-send-function msg)
	     com-id val)
    (iiimp-connection-error "iiimp-send-message" com-id msg)))

;;;
;;; For debugging.
;;;

(defun iiimp-debug ()
  (if iiimp-debug-flag 
      (setq iiimp-debug-flag nil)
    (message "IIIMP debug is on.")
    (setq iiimp-debug-flag t))
  (setq iiimp-debug-buffer (get-buffer-create "*IIIMP debug*")))

(defun iiimp-add-debug-log (obj)
  (if (and iiimp-debug-flag
	   iiimp-debug-buffer)
      (let ((oldbuf (current-buffer))
	    (win (get-buffer-window iiimp-debug-buffer t)))
	(set-buffer iiimp-debug-buffer)
	(goto-char (point-max))
	(if win (set-window-point win (point-max)))
	(insert obj)
	(set-buffer oldbuf))))

(provide 'iiimp)

;; iiimp.el ends here.
