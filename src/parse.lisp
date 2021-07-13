;;;; Perform syntactical analysis on Pseudocode text streams.

(in-package :davis.parse)

;;; Primary Interface

(defun parse-file (filespec)
  "Returns a syntax tree of the Pseudocode in the file named by FILESPEC."
  ;; TODO: Handle parsing errors.
  (handler-case (some->> filespec
                         (mmap-file-to-string) ; Avoid potentially loading a large file into working memory. 
                         ;; TODO: Handle multiple top-level forms in a file.
                         (parse 'top-level-form-list))))

;;; Macros

(defmacro define-delimited-list-rule (symbol element-type delimiter)
  "Generates a DEFRULE named SYMBOL that parses a list of ELEMENT-TYPE delimited by DELIMITER.

   (define-delimited-list-rule identifier-list identifier #\,) creates a rule that parses
\"foo, bar , baz\" into (foo bar baz)."
  ;; 
  (let ((delimiter (if (eq delimiter 'whitespace)
                       'whitespace
                       `(and (? whitespace) ,delimiter (? whitespace)))))
    `(defrule ,symbol (and (? ,element-type) (* (and ,delimiter ,element-type)))
       (:lambda (production)
        (cons (first production) (mapcar #'second (second production)))))))

(defmacro define-binary-operation-rule (symbol lower-rule operators &rest options)
  "Generates a left-recursive DEFRULE named SYMBOL that parses a binary operation with OPERATORS
into the appropriate Lisp form.

The operator is automatically looked-up with FIND-SYMBOL if it exists."
  `(defrule ,symbol (and ,lower-rule (? (and (? whitespace) ,operators (? whitespace) ,symbol)))
     (:destructure (lhs (&optional _ operator _ rhs))
      (if operator
          `(,(or (find-symbol operator :davis.parse) operator) ,lhs ,rhs)
          lhs))
     ,@options))

(defmacro define-unary-operation-rule (symbol lower-rule operators &rest options)
  "Generates a left-recursive DEFRULE named SYMBOL that parses a unary operation with OPERATORS
into the appropriate Lisp form.

The operator is automatically looked-up with FIND-SYMBOL if it exists."
  `(defrule ,symbol (and (? ,operators) (? whitespace) ,lower-rule)
     (:destructure (operator _ operand)
      (if operator
          `(,(or (find-symbol operator :davis.parse) operator) ,operand)
          operand))
     ,@options))

;;; Grammars

;; TODO: Rename this.
(define-delimited-list-rule top-level-form-list (or procedure record) (+ newline))

(defrule procedure (and (and "BEGIN" whitespace) identifier (? (and (? whitespace) parameter-list)) newline
                        ;; This is the most elegant solution I could find, because ESRAP does not
                        ;; perform backtracking. Every use of INDENTED-BLOCK must mark its 'ender'.
                        ; (? (and (! "END") indented-block))
                        (* indented-statement)
                        (and "END" whitespace) identifier)
  (:destructure (_ forename (&optional _ parameters) _
                   ; (&optional _ statements)
                   statements
                 _ surname)
   ;; A procedure definition not beginning and ending with matching names does not impair parsing,
   ;; but users should be warned of it.
   (when (not (string= forename surname))
     (warn 'inconsistent-procedure-names-warning :forename forename :surname surname))
   (list :type :procedure :name forename :parameters parameters :statements statements)))

(defrule parameter-list (and (and #\( (? whitespace)) identifier-list (and (? whitespace) #\)))
  (:function second))

;; TODO: FIXME and add warnings for bad indentation.
;(defrule indented-block #'parse-indented-block
;  (:lambda (production)
;   (remove nil production))) ; Ignore newlines.
;
;(defvar *current-indentation-level* 0
;  "The number of spaces or tabs preceding statements in the block being parsed.")
;
;(defun parse-indented-block (text position end)
;  ;; Get the indentation level of the first statement of the block, expecting following statements
;  ;; to maintain that level.
;  (let* ((indentation (parse 'whitespace text :start position :end end :junk-allowed t))
;         (indentation-level (length indentation)))
;    ;; Warn the user to remember to indent new blocks. This assumes no mixed indentation.
;    ;; TODO: Check for mixed indentation.
;    (when (<= indentation-level *current-indentation-level*)
;      ;; TODO: Improve indentation warnings.
;      (warn 'indentation-warning :position position))
;    ;; Bind the updated indentation level for parsing nested blocks.
;    (let ((*current-indentation-level* indentation-level))
;      ;; Actually parse the inner statements.
;      (parse '(* (and indented-statement)) text :start position :end end))))

(defrule indented-block (* indented-statement)
  (:lambda (production)
   (remove nil production)))

(defrule indented-statement (and (? whitespace) (? statement) newline)
  (:destructure (indentation statement _)
   ;; TODO: Check for mixed indentation.
   ;(when (< (length indentation) *current-indentation-level*)
   ;  ;; TODO: Get the character position.
   ;  (warn 'indentation-warning :position :stub))
   statement))

(defvar *statement-position* nil
  "A hack to obtain the position of the current statement being parsed by the STATEMENT rule.")

(defrule statement (or comment
                       display-statement
                       return-statement
                       let-statement
                       get-statement
                       open-statement
                       read-statement
                       close-statement
                       dim-statement
                       if-statement
                       casewhere-statement
                       while-statement
                       for-statement
                       repeat-statement
                       array-access-or-procedure-call)
  (:around (&bounds start end)
   (setf *statement-position* (list :start start :end end))
   (call-transform))
  (:lambda (statement)
   (append statement *statement-position*)))

(defrule display-statement (and (and "Display" whitespace) display-statement-arguments)
  (:destructure (_ arguments)
   ;; There are two styles of display statement arguments. The course specifications implicitly
   ;; concatenate arguments, but some exam papers place ampersands between them.
   ;; TODO: Support explicit concatenated display statements.
   (list :type :display-statement :arguments arguments)))

(define-delimited-list-rule display-statement-arguments expression whitespace)

(defrule return-statement (and (and "RETURN" whitespace) (? expression))
  (:destructure (_ value)
   (list :type :return-statement :arguments (list value))))

(defrule let-statement (and (? (and (or "Let" "Set" "LET") whitespace))
                            assignment-operation)
  (:destructure (_ assignment)
   (list :type :let-statement :arguments assignment)))

(defrule get-statement (and (and (or "Get" "Read") whitespace) identifier-list)
  (:destructure (_ arguments)
   (list :type :get-statement :arguments arguments)))

(defrule open-statement (and (and "Open" whitespace)
                             identifier
                             (and whitespace "for" whitespace)
                             open-statement-mode)
  (:destructure (_ filespec _ mode)
   (cons :open-statement (list mode))))

(defrule open-statement-mode (or "output" "input" "append" "relative access"))

;; There are two standard syntaxes for reading from files.
(defrule read-statement (and (and "Read" whitespace)
                             (or read-statement-inner-1
                                 read-statement-inner-2))
  (:destructure (_ arguments)
   (list :type :read-statement :arguments arguments)))

(defrule read-statement-inner-1 (and identifier-list " from " identifier)
  (:destructure (identifiers _ filespec)
   (list :identifiers identifiers :filespec filespec)))

(defrule read-statement-inner-2 (and identifier " into " identifier-list)
  (:destructure (filespec _ identifiers)
   (list :identifiers identifiers :filespec filespec)))

(defrule write-statement (and "Write " identifier " from " identifier-list)
  (:destructure (filespec _ identifiers)
   (list :type :write-statement :arguments (list filespec identifiers))))

(defrule close-statement (and "Close " identifier)
  (:destructure (_ filespec)
   (list :type :close-statement :arguments (list filespec))))

(defrule dim-statement (and "DIM "
                            identifier
                            (? whitespace)
                            dimension-list
                            " as "
                            identifier)
  (:destructure (_ name _ dimensions _ type)
   (list :type :dim-statement :arguments (list name dimensions type))))

(defrule if-statement (and "IF " expression (and " THEN" newline)
                           (* indented-statement)
                           (? (and (and (? whitespace) "ELSE" newline)
                                   (* indented-statement)))
                           (and (? whitespace) "ENDIF")) ; Cater for indents
  (:destructure (_ condition _ then-block (&optional _ else-block) _)
   (list :type :if-statement :arguments (list condition then-block else-block))))

(defrule casewhere-statement (and "CASEWHERE " expression (and " evaluates to" newline)
                                  casewhere-statement-choice-list
                                  (? (and (and (? whitespace) ; Indent
                                               "OTHERWISE"
                                               (? whitespace)
                                               ":")
                                          casewhere-statement-process))
                                  (and (? whitespace) "ENDCASE"))
  (:destructure (_ expression _ choices (&optional _ otherwise-process) _)
   (list :type :casewhere-statement :arguments (list choices :otherwise otherwise-process))))

(defrule casewhere-statement-choice-list (* casewhere-statement-choice))

(defrule casewhere-statement-choice (and (? whitespace)
                                         literal
                                         (and (? whitespace) #\: (? whitespace))
                                         casewhere-statement-process)
  (:destructure (_ value _ process)
   (cons value (list process))))

(defrule casewhere-statement-process (or (and statement newline)
                                         (and newline (*  indented-statement)))
  (:destructure (a b)
   ;; NEWLINE constantly produces :newline.
   (if (eq a :newline) b a)))

(defrule while-statement (and "WHILE " expression newline
                              indented-block
                              (and (? whitespace) "ENDWHILE"))
  (:destructure (_ condition _ statements _)
   (list :type :while-statement :arguments (list condition statements))))

(defrule for-statement (and "FOR "
                            assignment-operation
                            " TO "
                            expression
                            (? (and " STEP " expression))
                            newline
                            (* indented-statement)
                            (and (? whitespace) "NEXT ") identifier)
  (:destructure (_ assignment _ finish-value (&optional _ step-value) _ statements _ variable)
   (list :type :for-statement
         :arguments (list assignment finish-value step-value statements variable))))

(defrule assignment-operation (and (or array-access-or-procedure-call identifier)
                                   (and (? whitespace) #\= (? whitespace))
                                   expression)
  (:destructure (lhs _ rhs)
   (list :lhs lhs :rhs rhs)))

(defrule repeat-statement (and (and "REPEAT" newline)
                               (* indented-statement)
                               (and (? whitespace) "UNTIL ") expression)
  (:destructure (_ statements _ condition)
   (list :type :repeat-statement :arguments (list condition statements))))

(defrule expression binary-logical-operation)

(define-binary-operation-rule binary-logical-operation not-operation (or "AND" "OR"))

(define-unary-operation-rule not-operation equality-operation "NOT")

(define-binary-operation-rule equality-operation comparison-operation "="
  (:lambda (form)
   (if (and (consp form) (string= (first form) "="))
       (rplaca form 'equalp)
       form)))

(define-binary-operation-rule comparison-operation term-operation (or "<=" ">=" "<>" "<" ">"))

(define-binary-operation-rule term-operation factor-operation (or "+" "-"))

(define-binary-operation-rule factor-operation sign-operation (or "*" "/"))

(define-unary-operation-rule sign-operation length-of-operation (or "+" "-"))

(define-unary-operation-rule length-of-operation value "Length of"
  (:lambda (form)
   (if (and (consp form) (string= (first form) "Length of"))
       (rplaca form 'length)
       form)))

(defrule value (or literal
                   nested-expression
                   array-access-or-procedure-call
                   identifier))

(defrule literal (or numeric-literal string-literal boolean-literal))

;; To future me: you have permission to modify the productions of the literal rules.

(defrule numeric-literal (and (+ (digit-char-p character))
                              (? (and #\. (+ (digit-char-p character)))))
  (:text t)
  (:function parse-number))

(defrule string-literal (and opening-quotation-mark
                             (* (not closing-quotation-mark))
                             closing-quotation-mark)
  (:text t)
  (:function read-from-string)) ; A little hacky

(defrule opening-quotation-mark (or #\" #\LEFT_DOUBLE_QUOTATION_MARK)
  (:error-report :detail))

(defrule closing-quotation-mark (or #\" #\RIGHT_DOUBLE_QUOTATION_MARK)
  (:error-report :detail))

(defrule boolean-literal (or "true" "false")
  (:lambda (production)
   (switch (production :test #'string=)
     ("true" t)
     ("false" nil))))

(defrule nested-expression (and (and #\( (? whitespace))
                                expression
                                (and #\) (? whitespace)))
  (:function second))

(defrule array-access-or-procedure-call (and identifier
                                             (and (? whitespace) #\()
                                             expression-list
                                             (and (? whitespace) #\)))
  (:constant '(:type :array-access-or-procedure-call :stub)))

(define-delimited-list-rule expression-list expression #\,)

(define-delimited-list-rule identifier-list identifier #\,)

(defrule identifier (and (+ (or (alphanumericp character) #\- #\_)))
  (:lambda (production)
   (intern (text production) :davis.user)))

(defrule comment (and (or #\' #\RIGHT_SINGLE_QUOTATION_MARK) (* (not #\Newline)))
  (:constant '(:type :comment))
  (:error-report nil))

(defrule record (and "DIM RECORD " identifier newline
                     record-variable-list
                     "END RECORD")
  (:destructure (_ name _ variables _)
   (list :type :record :name name :variables variables)))

;; TODO: Add proper indentation support.
(defrule record-variable-list (* (and (? whitespace) record-variable newline))
  (:lambda (production)
   (mapcar #'second production)))

(defrule record-variable (and identifier " as " (and identifier (? (and (? whitespace) dimension-list))))
  (:destructure ( name _ (type &optional _ dimensions))
   (list :name name :type type :dimensions dimensions)))

(defrule dimension-list (and (and #\( (? whitespace)) integer-literal-list (and (? whitespace) #\)))
  (:function second))

(define-delimited-list-rule integer-literal-list integer-literal #\,)

(defrule integer-literal (+ (digit-char-p character))
  (:lambda (production)
   (parse-integer (text production))))

;; A sequence of strictly spaces and tabs. Newlines are described separately to also include comments.
(defrule whitespace (+ (or #\Space #\Tab))
  (:text t))

(defrule newline (and (? whitespace) (? comment) #\Newline)
  (:constant :newline))

;;; Utilities

(defun mmap-file-to-string (filespec)
  (with-mmap (address file-descriptor size filespec)
    (declare (ignore file-descriptor))
    (foreign-string-to-lisp address :count (1- size)))) ; The 1- strips the trailing newline. 

(defun <> (a b) (not (equalp a b)))

;;; Warnings

(define-condition inconsistent-procedure-names-warning (warning)
  ((forename :initarg :forename)
   (surname :initarg :surname)))

(define-condition indentation-warning (warning)
  ((position :initarg :position)))
