;;; iiimcf-comp.el --- for byte-compile and other housekeeping jobs.

;; Copyright (C) 2000 Miyashita Hisashi

;; This file is part of IIIMECF.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Comment:

(require 'bytecomp)

(let ((load-path (append '("." "./lisp") load-path))
      (compile-file-list-1st '("./lisp/iiimp.el"
			       "./lisp/iiimcf.el"
			       "./lisp/iiimcf-UI.el"
			       "./lisp/EIMIL.el"
			       "./lisp/PCE.el"
			       "./lisp/iiimcf-sc.el"))
      path file)

;; Check Mule-UCS

(while (and (null (locate-library "un-define"))
            (string< emacs-version "22")) ;(not (eq (make-char 'latin-iso8859-1 160) 160)))
  (progn
    (message "I cannot find Mule-UCS.")
    (message "Please type the path where Mule-UCS is installed.")
    (setq path (read-from-minibuffer ""))
    (setq load-path (cons (expand-file-name path) load-path))))

  (message "Remove old byte-compiled files-----")
  (mapcar 
   (lambda (x)
     (setq file (concat
		 (file-name-sans-extension x)
		 ".elc"))
     (if (file-exists-p file)
	 (progn
	   (message "Remove %s" file)
	   (delete-file file)))
     nil)
   compile-file-list-1st)

  (message "Compiling 1st stage-----")
  (mapcar
   (lambda (file)
     (save-excursion
       (byte-compile-file file)))
   compile-file-list-1st))

;; iiimcf-comp.el ends here.
