;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; kw-cheating-section.

;;hatena-kw-cheating を使う場合。`getdiary.rb' のあるディレクトリ。
(defvar hatena-plugin-directory "c:/home/mylisp/hatena-mode")
(defconst hatena-kw-if nil
  "デフォルトで kw-cheating(他人の日記の流し読み) をするかどうか")
(defconst hatena-kw-repeat 20
  "kw-cheating する期間。何秒に一回 キーワードをを更新するか")

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
  "edit window の大きさ比率。")


(defvar hatena-kw-list-buf nil)
(defvar hatena-kw-cheating-buf nil)
(defvar hatena-edit-buf nil)

(defvar hatena-kw-post-timer nil
  "現在のテキストをはてなに投げるタイマー")
(defvar hatena-kw-ruby-timer nil
  "ruby を動かす")
(defvar hatena-kw-get-timer nil
  "ruby の出力したファイルをピックアップするタイマー")


(defconst hatena-kw-temp-diary "10101010"
  "はてなにキーワードを問い合わせるための適当な日付")
(defvar hatena-kw-url-file 
  (expand-file-name (concat hatena-directory "urls.txt"))
  "kw-cheating 候補の url を保存")

(defun hatena-kw-init()
  (interactive)
  ;;まず窓を一つにリセットしてから、二つに割る。
  ;;こうすると、いつ `M-x hatena' しても窓が二つある状況になる。
  (delete-other-windows)
  (setq hatena-edit-buf (current-buffer))
  ;;hatena-edit-wdw 日記を書くバッファ
  ;;hatena-keyword-cheating-wdw 他の日記を見るウィンドウ
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
  ;;タイマースタート
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
	(run-at-time (/ hatena-kw-repeat 2);; get は post より少し遅らせる
		     hatena-kw-repeat
		     'hatena-kw-get-func))
  ;; はてなにキーワードを問い合わせるためのページを作る
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
  "`hatena-kw-temp-diary' に非同期ポスト. "
;  (if (string-match "Hatena" mode-name)
      (progn
	(hatena-kw-submit hatena-kw-temp-diary))
;    (cancel-timer hatena-kw-post-timer));; hatena-mode でなければ、直ちに停止する。
  )

(defun hatena-kw-get-func()
  "ruby の output を hatena-kw-cheating-buf に表示する。"
;  (if (string-match "Hatena" mode-name)
;      (progn
	  (with-current-buffer (current-buffer)
	    (set-buffer hatena-kw-cheating-buf)
	    (delete-region (point-min) (point-max)) ;;全部消して
	    ;;from insert-file-contents-as-coding-system
	    (let ((coding-system-for-read 'euc-jp)
		  format-alist)
	      (insert-file-contents hatena-kw-result-file))
	    ;;簡易レンダリング
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
;    (cancel-timer hatena-kw-get-timer) ;; hatena-mode でなければ、直ちに停止する。
 ;   (message "Hatena get-timer cancelled"))
  )

(defvar hatena-kw-ruby-process nil)
(defun hatena-kw-ruby-func()
  "  getdiary.rb に処理を渡す。getdiary は、バックグラウンドで以下の処理をする。
1. hatena-kw-post-diary の日記を取ってきて、キーワードを抜き出す。
2. http://d.hatena.ne.jp/keyword/ から、抜き出したキーワードを含む 
　　はてなダイアリーのURLを取得する。
3. リストから、適当なURLを取得し、キーワード周辺のテキストをレンダリングし、
   hatena-kw-result-file に保存する。"
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
;   (cancel-timer hatena-kw-ruby-timer) ;; hatena-mode でなければ、直ちに停止する。
;   (message "Hatena ruby timer cancelled"))
)

(defun hatena-kw-submit (kwdate &optional newpage)
 "非同期で、post する。ユーザーからは呼ばれず、hatena-mode の時のみ動作する"
 ;;;ちょっと冗長だけど仕方ない。
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
	     (cond ( (string-match "\\`title[ 　]*\\(.*\\)?\n" prebody)
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
  "現在までの秒数を返す。emacs では整数がケタ溢れするので、浮動小数点で"
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