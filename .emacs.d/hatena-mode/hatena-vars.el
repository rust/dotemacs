(provide 'hatena-vars)

(defgroup hatena nil
  "major mode for Hatena::Diary"
  :prefix "hatena-"
  :group 'hypermedia)

(defgroup hatena-face nil
  "Hatena, Faces."
  :prefix "hatena-"
  :group 'hatena)

(defcustom hatena-usrid nil
  "hatena-mode �Υ桼����̾"
  :type 'string
  :group 'hatena)

(defcustom hatena-directory 
  (expand-file-name "~/.hatena/")
  "��������¸����ǥ��쥯�ȥ�."
  :type 'directory
  :group 'hatena)

(defcustom hatena-init-file (concat
			     (file-name-as-directory hatena-directory)
			     "init")
  "*hatena-mode �ν�����ե����롣"
  :type 'file
  :group 'hatena)

(defcustom hatena-password-file 
  (expand-file-name (concat hatena-directory ".password"))
  "�ѥ�����¸����ե�����"
  :type 'file
  :group 'hatena)

(defcustom hatena-entry-type 1
  "����ȥ�Υޡ������å� * ��ɤΤ褦�˽������뤫��
0�ʤ� * �� *pn* �ˡ�1 �ʤ� * �� *<time>* ���֤�����������"
  :type 'integer
  :group 'hatena)

(defcustom hatena-change-day-offset 6
  "�ϤƤʤ�, ���դ��Ѥ������ .+6 �Ǹ��� 6 �������դ��ѹ�����."
  :type 'integer
  :group 'hatena)

(defcustom hatena-trivial nil
  "����äȤ��������򤹤뤫�ɤ���. non-nil ��\"����äȤ�������\"�ˤʤ�"
  :type 'boolean
  :group 'hatena)

(defcustom hatena-use-file t
  "�ѥ���ɤ�(�Ź沽����)��¸���뤫�ɤ��� non-nil �ʤ�ѥ���ɤ� base 64 �ǥ��󥳡��ɤ�����¸����"
  :type 'boolean
  :group 'hatena)

(defcustom hatena-cookie 
  (expand-file-name 
   (concat hatena-directory "Cookie@hatena"))
  "���å�����̾����"
  :type 'file
  :group 'hatena)

(defcustom hatena-browser-function nil  ;; ���̤ϡ�'browse-url
  "Function to call browser.
If non-nil, `hatena-submit' calls this function.  The function
is expected to accept only one argument(URL)."
  :type 'symbol
  :group 'hatena)

(defcustom hatena-proxy ""
  "curl ��ɬ�פʻ������������ꤹ��"
  :type 'string
  :group 'hatena)

(defcustom hatena-default-coding-system 'euc-jp
  "�ǥե���ȤΥ����ǥ��󥰥����ƥ�"
  :type 'symbol
  :group 'hatena)


(defcustom hatena-url "http://d.hatena.ne.jp/"
  "�ϤƤʤΥ��ɥ쥹"
  :type 'string
  :group 'hatena)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;�����ե����������ɽ��
(defvar hatena-fname-regexp
  "\\([0-9][0-9][0-9][0-9]\\)\\([01][0-9]\\)\\([0-3][0-9]\\)$" )
(defvar hatena-mode-map nil)

;;�Ť�����
(defvar hatena-header-regexp 
  (concat "\\`      Title: \\(.*\\)\n"
          "Last Update: \\(.*\\)\n"
          "____________________________________________________" ))

(defvar hatena-tmpfile 
  (expand-file-name (concat hatena-directory "hatena-temp.dat")))
(defvar hatena-tmpfile2
  (expand-file-name (concat hatena-directory "hatena-temp2.dat")))
(defvar hatena-curl-command "curl" "curl ���ޥ��")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;face

(defvar hatena-font-lock-keywords nil)
(defvar hatena-html-face 'hatena-html-face)
(defvar hatena-title-face 'hatena-title-face)
(defvar hatena-header-face 'hatena-header-face)
(defvar hatena-subtitle-face 'hatena-subtitle-face)
(defvar hatena-markup-face 'hatena-markup-face)
(defvar hatena-link-face 'hatena-link-face)

(defface hatena-title-face
  '((((class color) (background light)) (:foreground "Navy" :bold t))
    (((class color) (background dark)) (:foreground "wheat" :bold t)))
  "title�� face"
  :group 'hatena-face)

(defface hatena-header-face
  '((((class color) (background light)) (:foreground "Gray70" :bold t))
    (((class color) (background dark)) (:foreground "SkyBlue4" :bold t)))
  "last update�� face"
  :group 'hatena-face)

(defface hatena-subtitle-face 
  '((((class color) (background light)) (:foreground "DarkOliveGreen"))
    (((class color) (background dark)) (:foreground "wheat")))
  "���֥����ȥ��face"
  :group 'hatena-face)

(defface hatena-markup-face 
  '((((class color) (background light)) (:foreground "firebrick" :bold t))
    (((class color) (background dark)) (:foreground "IndianRed3" :bold t)))
  "�ϤƤʤΥޡ������åפ�face"
  :group 'hatena-face)

(defface hatena-html-face 
  '((((class color) (background light)) (:foreground "DarkSeaGreen4"))
    (((class color) (background dark)) (:foreground "Gray50")))
  "html��face"
  :group 'hatena-face)

(defface hatena-link-face 
  '((((class color) (background light)) (:foreground "DarkSeaGreen4"))
    (((class color) (background dark)) (:foreground "wheat")))
  "html�����Ƕ��ޤ줿��ʬ��face"
  :group 'hatena-face)



