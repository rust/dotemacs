;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; kw-cheating-section.

;;hatena-kw-cheating ��Ȥ���硣`getdiary.rb' �Τ���ǥ��쥯�ȥꡣ
(defvar hatena-plugin-directory "c:/home/mylisp/hatena-mode")
(defconst hatena-kw-if nil
  "�ǥե���Ȥ� kw-cheating(¾�ͤ�������ή���ɤ�) �򤹤뤫�ɤ���")
(defconst hatena-kw-repeat 20
  "kw-cheating ������֡����ä˰�� ������ɤ�򹹿����뤫")

;; kw-cheating-section
(defvar hatena-edit-wdw nil)
(defvar hatena-kw-cheating-wdw nil)
(defvar hatena-kw-url
  "http://d.hatena.ne.jp/images/keyword/keywordlist")
(defvar hatena-kw-file
  (expand-file-name 
   (concat hatena-directory "keywords.txt"))
  "keyword file for searching keywords")
(defvar hatena-kw-result-file
  (expand-file-name 
   (concat hatena-directory "result.txt"))
  "result for ruby processing of keyword cheating")
(defvar hatena-kw-window-split-ratio 0.50
  "edit window ���礭����Ψ��")


(defvar hatena-kw-list-buf nil)
(defvar hatena-kw-cheating-buf nil)
(defvar hatena-edit-buf nil)

(defvar hatena-kw-post-timer nil
  "���ߤΥƥ����Ȥ�ϤƤʤ��ꤲ�륿���ޡ�")
(defvar hatena-kw-ruby-timer nil
  "ruby ��ư����")
(defvar hatena-kw-get-timer nil
  "ruby �ν��Ϥ����ե������ԥå����åפ��륿���ޡ�")


(defconst hatena-kw-temp-diary "10101010"
  "�ϤƤʤ˥�����ɤ��䤤��碌�뤿���Ŭ��������")
(defvar hatena-kw-url-file 
  (expand-file-name (concat hatena-directory "urls.txt"))
  "kw-cheating ����� url ����¸")

(defun hatena-kw-init()
  (interactive)
  ;;�ޤ�����Ĥ˥ꥻ�åȤ��Ƥ��顢��Ĥ˳�롣
  ;;��������ȡ����� `M-x hatena' ���Ƥ��뤬��Ĥ�������ˤʤ롣
  (delete-other-windows)
  (setq hatena-edit-buf (current-buffer))
  ;;hatena-edit-wdw ������񤯥Хåե�
  ;;hatena-keyword-cheating-wdw ¾�������򸫤륦����ɥ�
  (setq hatena-edit-wdw (selected-window))
  (setq hatena-kw-cheating-wdw 
	(split-window hatena-edit-wdw 
		      (floor (* (window-height)
				hatena-kw-window-split-ratio))))
  (save-selected-window 
    (select-window hatena-kw-cheating-wdw)
    (setq hatena-kw-cheating-buf
	  (switch-to-buffer "*hatena-keyword-cheating*"))
    (setq mode-name "Hatena kw-cheating"))
  ;;�����ޡ���������
  (if hatena-kw-post-timer
      (cancel-timer hatena-kw-post-timer))
  (if hatena-kw-ruby-timer
      (cancel-timer hatena-kw-ruby-timer))
  (if hatena-kw-get-timer
      (cancel-timer hatena-kw-get-timer))
  (setq hatena-kw-post-timer
	(run-at-time 0 
		     hatena-kw-repeat
		     'hatena-kw-post-func))
  (setq hatena-kw-ruby-timer
	(run-at-time (/ hatena-kw-repeat 4)
		     hatena-kw-repeat
		     'hatena-kw-ruby-func))
  (setq hatena-kw-get-timer
	(run-at-time (/ hatena-kw-repeat 2);; get �� post ��꾯���٤餻��
		     hatena-kw-repeat
		     'hatena-kw-get-func))
  ;; �ϤƤʤ˥�����ɤ��䤤��碌�뤿��Υڡ�������
  (hatena-kw-submit hatena-kw-temp-diary t)
  (message (concat "creating diary on " hatena-url "for kw-cheating"))
  )

(defun hatena-kw-final()
  (interactive)
  (if hatena-kw-post-timer
      (cancel-timer hatena-kw-post-timer))
  (if hatena-kw-ruby-timer
      (cancel-timer hatena-kw-ruby-timer))
  (if hatena-kw-get-timer
      (cancel-timer hatena-kw-get-timer)))

(defun hatena-kw-post-func()
  "`hatena-kw-temp-diary' ����Ʊ���ݥ���. "
;  (if (string-match "Hatena" mode-name)
      (progn
	(hatena-kw-submit hatena-kw-temp-diary))
;    (cancel-timer hatena-kw-post-timer));; hatena-mode �Ǥʤ���С�ľ������ߤ��롣
  )

(defun hatena-kw-get-func()
  "ruby �� output �� hatena-kw-cheating-buf ��ɽ�����롣"
;  (if (string-match "Hatena" mode-name)
;      (progn
	  (with-current-buffer (current-buffer)
	    (set-buffer hatena-kw-cheating-buf)
	    (delete-region (point-min) (point-max)) ;;�����ä���
	    ;;from insert-file-contents-as-coding-system
	    (let ((coding-system-for-read 'euc-jp)
		  format-alist)
	      (insert-file-contents hatena-kw-result-file))
	    ;;�ʰץ������
	    (goto-char (point-min))
	    (while (re-search-forward "<[^>]+>" nil t)
			       (replace-match "" nil nil))
	    (goto-char (point-min))
	    (while (re-search-forward "\n\t+" nil t)
			       (replace-match "\n" nil nil))
	    (goto-char (point-min))
	    (while (re-search-forward "\n+" nil t)
			       (replace-match "\n" nil nil))
	    
	    )
;	  )
;    (cancel-timer hatena-kw-get-timer) ;; hatena-mode �Ǥʤ���С�ľ������ߤ��롣
 ;   (message "Hatena get-timer cancelled"))
  )

(defvar hatena-kw-ruby-process nil)
(defun hatena-kw-ruby-func()
  "  getdiary.rb �˽������Ϥ���getdiary �ϡ��Хå����饦��ɤǰʲ��ν����򤹤롣
1. hatena-kw-post-diary ���������äƤ��ơ�������ɤ�ȴ���Ф���
2. http://d.hatena.ne.jp/keyword/ ���顢ȴ���Ф���������ɤ�ޤ� 
�����ϤƤʥ������꡼��URL��������롣
3. �ꥹ�Ȥ��顢Ŭ����URL���������������ɼ��դΥƥ����Ȥ������󥰤���
   hatena-kw-result-file ����¸���롣"
;  (if (string-match "Hatena" mode-name)
      (start-process "ruby-process" 
		 "*hatena keyword*"
		 "ruby"
		 (concat hatena-plugin-directory "/getdiary.rb")
		 (concat hatena-url hatena-usrid "/" hatena-kw-temp-diary)
		 hatena-kw-file
		 hatena-kw-url-file
		 hatena-kw-result-file
		 )
;   (cancel-timer hatena-kw-ruby-timer) ;; hatena-mode �Ǥʤ���С�ľ������ߤ��롣
;   (message "Hatena ruby timer cancelled"))
)

(defun hatena-kw-submit (kwdate &optional newpage)
 "��Ʊ���ǡ�post ���롣�桼��������ϸƤФ줺��hatena-mode �λ��Τ�ư���"
 ;;;����äȾ�Ĺ�����ɻ����ʤ���
 (if (string-match hatena-fname-regexp kwdate)
     (let* 
	 ((file buffer-file-name)
	  (userid hatena-usrid)
	  (year (match-string 1 kwdate))
	  (month (match-string 2 kwdate))
	  (day (match-string 3 kwdate))
	  (date (concat year month day))
	  (timestamp 
	   (format-time-string "%Y%m%d%H%m%S" (current-time)))
	  (baseurl (concat hatena-url userid "/"))
	  (referer (concat baseurl "edit?date=" date))
	  (nexturl (concat baseurl (concat year month day)))
	  (url (concat baseurl "edit"))
	  (title "")
	  (imagetitle "")
	  (send-file file)
	  (prebody (save-excursion
		     (set-buffer hatena-edit-buf)
		     (buffer-string)))
	  (full-body 
	   (with-temp-buffer
	     (insert prebody)
	     (cond ( (string-match "\\`title[ ��]*\\(.*\\)?\n" prebody)
		     (progn 
		       (setq title (match-string 1 prebody))
		       (substring (buffer-string)
				  (length (match-string 0 prebody)))
		       ))
		   ( (string-match hatena-header-regexp prebody)
		     (progn
		       (setq title (match-string 1 prebody))
		       (substring prebody
				  (1+ (length (match-string 0 prebody))))) )
		   (t prebody))))
	  (body (hatena-url-encode-string full-body hatena-default-coding-system))
	  (trivial "0")
	  (post-data 
	   (concat "mode=enter"
		   "&body=" body 
		   "&trivial=" trivial 
		   "&imagetitle=" imagetitle
		   "&title=" title 
		   "&day=" day 
		   "&month=" month 
		   "&year=" year 
		   (if newpage nil
		     (concat "&date=" date))
		   "&timestamp=" timestamp )))
       (with-temp-file hatena-tmpfile 
	 (insert post-data))
       (start-process "curl-process" 
		      "*hatena keyword*"
		      hatena-curl-command
		      "-b" hatena-cookie
		      "-x" hatena-proxy
		      "--data" (concat "@" hatena-tmpfile)
		      url))
   ))

(defun hatena-current-second(number)
  "���ߤޤǤ��ÿ����֤���emacs �Ǥ�������������줹��Τǡ���ư��������"
  (let* ((ct (current-time))
	 (high (float (car ct)))
	 (low (float (car (cdr ct))))
	 str)
    (setq str (format "%f"(+ 
			   (+ (* high (lsh 2 15)) low)
			   number)))
    (substring str 0 10) ;;
    ))

(provide 'hatena-kw)