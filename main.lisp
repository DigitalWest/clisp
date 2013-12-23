(setq lexemes-map
      '(
	("<=" "operator" "op1")
	(">=" "operator" "op2")
	("<>" "operator" "op3")
	(":=" "operator" "op4")
	(":"  "operator" "op8")
	("+"  "operator" "op10")
	("-"  "operator" "op11")
	("*"  "operator" "op12")
	("/"  "operator" "op13")
	("="  "operator" "op14")

	(")"  "splitter" "sp1")
	("("  "splitter" "sp2")
	(","  "splitter" "sp3")
	(";"  "splitter" "sp4")
	("."  "splitter" "sp5")
	
	("PROGRAM" "service-word" "sw1")
	("VAR"     "service-word" "sw2")
	("INTEGER" "service-word" "sw3")
	("REAL"    "service-word" "sw4")
	("BEGIN"   "service-word" "sw5")
	("END"     "service-word" "sw6")
	("IF"      "service-word" "sw7")
	("THEN"    "service-word" "sw8")
	("ELSE"    "service-word" "sw9")
	("GOTO"    "service-word" "sw10")
	("FOR"     "service-word" "sw11")
	("TO"      "service-word" "sw12")
	("DO"      "service-word" "sw13")
	("READ"    "service-word" "sw14")
	("WRITE"   "service-word" "sw15") 
	))

(setq operators
      '("<=" ">=" "<>" ":=" ")" "(" "," ":" ";" "+" "-" "*" "/" "="))

(setq splitters
      '(#\space #\newline #\linefeed #\tab))

(defun starts-with (start-str str)
  (let* ((start-str-l (length start-str))
 	 (str-l (length str)))
    (and (>= str-l start-str-l)
 	 (string= start-str str :end1 start-str-l :end2 start-str-l))))

(defun carstr (str)
  (subseq str 0 1))

(defun cdrstr (str)
  (subseq str 1))

(defun remove-comments (str)
  (remove-comments-internal str "" nil))

(defun str-empty? (str)
  (string= "" str))

(defun  remove-comments-internal (str accum in-comment?)
  (if (str-empty? str)
      accum
      (let* ((c (carstr str))
	     (comment-end? (string= "}" c))
	     (comment-start? (string= "{" c)))
	(if in-comment?
	    (remove-comments-internal (cdrstr str) accum (not comment-end?))
	    (if comment-start?
		(remove-comments-internal (cdrstr str) accum t)
		(remove-comments-internal (cdrstr str) (concatenate 'string accum c) nil))))))

(defun append-if-not-empty (list str)
  (if (str-empty? str)
      list
      (cons str list)))

(defun find-match (tokens str)
  (if (endp tokens)
      nil
      (let ((token (car tokens)))
	(if (starts-with token str)
	    token
	    (find-match (cdr tokens) str)))))

(defun split-text (str)
  (reverse (split-text-internal str '() "")))

(defun split-text-internal (str accum current-token)
  (if (str-empty? str)
      (append-if-not-empty accum current-token)
      (if (find (char str 0) splitters)
	  (split-text-internal (cdrstr str)
			       (append-if-not-empty accum current-token)
			       "")
	  (if (and (string= current-token "END") ;hack for "end." in the end of program
		   (eq #\. (char str 0)))
	      (split-text-internal (cdrstr str)
				   (cons "." (cons "END" accum))
				   "")
	      (let ((operator-match (find-match operators str)))
		(if operator-match
		    (split-text-internal (subseq str (length operator-match))
					 (cons operator-match (append-if-not-empty accum current-token))
					 "")
		    (split-text-internal (cdrstr str)
					 accum
					 (concatenate 'string current-token (carstr str)))))))))

(setq num-c 0)
(setq float-c 0)
(setq id-c 0)

(defun next-num ()
  (setq num-c (+ 1 num-c))
  (concatenate 'string "num" (write-to-string num-c)))

(defun curr-num ()
	(concatenate 'string "num" (write-to-string num-c)))

(defun next-float ()
  (setq float-c (+ 1 float-c))
  (concatenate 'string "float" (write-to-string float-c)))

(defun curr-float ()
  (concatenate 'string "float" (write-to-string float-c)))

(defun next-id ()
  (setq id-c (+ 1 id-c))
  (concatenate 'string "id" (write-to-string id-c)))

(defun curr-id ()
	(concatenate 'string "id" (write-to-string id-c)))

(defparameter groups (make-hash-table :test 'equal))
(defparameter ids '())
(defparameter opers '())
(defparameter swords '())
(defparameter spls '())

(defun add-id-token (token)
	(push token ids))

(defun token-in-ids (token)
	(cond
		((find token ids :test 'equal) T)
		(t nil)))

(defun add-oper-token (token)
	(push token opers))

(defun token-in-oper (token)
	(cond
		((find token opers :test 'equal) T)
		(t nil)))

(defun add-swords-token (token)
	(push token swords))

(defun token-in-swords (token)
	(cond
		((find token swords :test 'equal) T)
		(t nil)))

(defun add-spls-token (token)
	(push token spls))

(defun token-in-spls (token)
	(cond
		((find token spls :test 'equal) T)
		(t nil)))

(defun get-lexem (token key)
	(let ((group (gethash key groups)))
		(when group
			(let ((lexem (find-if (lambda (item) (string= token (car item))) (gethash key groups))))
	(if lexem
		lexem
		nil)))))

(defun recognize (token)
  (let* ((lexem (find-if (lambda (item) (string= token (car item))) lexemes-map)))
    (if lexem
    	(cond 
    		((token-in-swords token) (list token (second lexem) (third lexem)))
    		((token-in-oper token) (list token (second lexem) (third lexem)))
    		((token-in-spls token) (list token (second lexem) (third lexem)))
    		((string= "service-word" (second lexem)) (add-swords-token token) (add-to-groups (second lexem) (list token (third lexem))) 
    			(list token (second lexem) (third lexem)))
    		((string= "splitter" (second lexem)) (add-spls-token token) (add-to-groups (second lexem) (list token (third lexem))) 
    			(list token (second lexem) (third lexem)))
    		((string= "operator" (second lexem)) (add-oper-token token) (add-to-groups (second lexem) (list token (third lexem))) 
    			(list token (second lexem) (third lexem)))
    		;(t (add-to-groups (second lexem) (list token (third lexem))) (list token (second lexem) (third lexem)))
    	)
	(cond
	  ((every 'digit-char-p token) (list token "number" (next-num)))
	  ((every (lambda (ch) (or (digit-char-p ch) (eq #\. ch))) token) (add-to-groups "float" (list token (next-float)))
	   (list token "float" (curr-float)))
	  ((token-in-ids token) (let* ((exist (get-lexem token "id"))
	  	(code (second exist)))
	   (list token "id" code)))
	  ((every (lambda (ch) (or (alphanumericp ch) (eq #\_ ch))) token) (add-id-token token)
	  (add-to-groups "id" (list token (next-id))) (list token "id" (curr-id)))
	  (t (list token "error" "error"))))))


(defun add-to-groups (key value)
  (let ((group (gethash key groups)))
    (if group
	(setf (gethash key groups) (cons value group))
	(setf (gethash key groups) (list value)))))

(defun pre-parse (input-file encoding-file lexems-file)
  (with-open-file (input-stream input-file)
    (with-open-file (encoding-stream encoding-file :direction :output :if-does-not-exist :create)
      (with-open-file (lexems-stream lexems-file :direction :output :if-does-not-exist :create)
	(loop
	   for line = (read-line input-stream nil 'eof)
	   until (eq line 'eof)
	   do
	     (loop
		for token in (split-text (remove-comments line))
		do
		  (let* ((lexem (recognize token))
			 (code (third lexem))
			 (type (second lexem))
			 (value (car lexem)))
		  (when lexem
		    (format t "~a " code)
		    (format encoding-stream "~a " code)
		    ;(add-to-groups type (list value code))
		    )))
	     (format t "~%")
	     (format encoding-stream "~%"))
	(format t "~%~%~%")
	(with-hash-table-iterator (my-iterator groups)
	  (loop
	     (multiple-value-bind (entry-p key value)
		 (my-iterator)
	       (if entry-p
		   (progn
		     (format t "---------~S---------~%" key)
		     (format lexems-stream "---------~S---------~%" key)
		     (loop for item in value
			do (progn
			     (format t "~a~%" item)
			     (format lexems-stream "~a~%" item)))
		     (format t "~%")
		     (format lexems-stream "~%"))
		   (return)))))
	))))

(defun ok-glass ()
  (pre-parse "D:/Projects/clisp/program.pas" "D:/Projects/clisp/encoding.txt" 
  	"D:/Projects/clisp/lexems.txt"))