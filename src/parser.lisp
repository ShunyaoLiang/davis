;;;; Perform syntactical analysis on Pseudocode text streams.

(in-package :davis.parser)

;;; Globals

(defvar *statement-position* nil
  "A hack to obtain the position of the current statement being parsed by the STATEMENT rule.")

;; Modified by: PROCEDURE, IDENTIFIER, DIM-STATEMENT, ARRAY-ACCESS-OR-PROCEDURE-CALL.
(defvar *procedure-local-bindings* nil
  "The set of local variable bindings of the current procedure being parsed.")

;; Modified by: DIM-STATEMENT.
(defvar *procedure-arrays* nil
  "The set of all arrays declared by the current procedure being parsed.")

;; This is very similar to *PROCEDURE-ARRAYS*, but that is cleared for every procedure. This keeps
;; all arrays across parsing an entire program. Exported.
;; Modified by: DIM-STATEMENT, PROCEDURE
(defvar *program-arrays* nil
  "The set of all arrays declared by the current program being parsed.") 

;;; Primary Interface

(defun parse-pseudocode (pseudocode)
  "Returns a syntax tree of PSEUDOCODE."
  (setf *program-arrays* nil)
  (parse 'top-level-form-list pseudocode))

(defun parse-file (filespec)
  "Returns a syntax tree of the Pseudocode in the file named by FILESPEC."
  (some-> filespec
          (mmap-file-to-string) ; Avoid potentially loading a large file into working memory.
          (parse-pseudocode)))

;;; Macros

(defmacro define-delimited-list-rule (symbol element-type delimiter)
  "Generates a DEFRULE named SYMBOL that parses a list of ELEMENT-TYPE delimited by DELIMITER.

   (define-delimited-list-rule identifier-list identifier #\,) creates a rule that parses
\"foo, bar , baz\" into (foo bar baz)."
  ;; 
  (let ((delimiter (if (eq delimiter 'whitespace)
                       'whitespace
                       `(and (? whitespace) ,delimiter (? whitespace)))))
    `(defrule ,symbol (and ,element-type (* (and ,delimiter ,element-type)))
       (:lambda (production)
        (cons (first production) (mapcar #'second (second production)))))))

(defmacro define-binary-operation-rule (symbol lower-rule operators &rest options)
  "Generates a left-recursive DEFRULE named SYMBOL that parses a binary operation with OPERATORS
into the appropriate Lisp form.

The operator is automatically looked-up with FIND-SYMBOL if it exists."
  `(defrule ,symbol (and ,lower-rule (? (and (? whitespace) ,operators (? whitespace) ,symbol)))
     (:destructure (lhs (&optional _ operator _ rhs))
      (if operator
          `(:type :binary-operation
            :fields (:operator ,(or (find-symbol operator :davis.parser)
                                    operator)
                          :lhs ,lhs
                          :rhs ,rhs))
          lhs))
     ,@options))

(defmacro define-unary-operation-rule (symbol lower-rule operators &rest options)
  "Generates a left-recursive DEFRULE named SYMBOL that parses a unary operation with OPERATORS
into the appropriate Lisp form.

The operator is automatically looked-up with FIND-SYMBOL if it exists."
  `(defrule ,symbol (and (? ,operators) (? whitespace) ,lower-rule)
     (:destructure (operator _ operand)
      (if operator
          `(:type :unary-operation
            :fields (:operator ,(or (find-symbol operator :davis.parser) operator)
                     :operand ,operand))
          operand))
     ,@options))

(defmacro set-insert (item place)
  "Inserts ITEM into PLACE if ITEM is not already present in PLACE."
  `(let ((item ,item)) ; Prevent duplicate evaluation.
     (unless (member item ,place :test #'equalp)
       (push item ,place))))

;;; Grammars

;; TODO: Rename this.
(define-delimited-list-rule top-level-form-list (or procedure record comment) (+ newline))

(defrule procedure (and (and "BEGIN" whitespace)
                        identifier
                        (? (and (? whitespace) parameter-list))
                        newline
                        (* indented-statement)
                        (and "END" whitespace) identifier)
  (:around (&bounds start end)
   (setf *statement-position* (list :start start :end end))
   (call-transform))
  (:destructure (_ forename (&optional _ parameters) _
                   ; (&optional _ statements)
                   statements
                 _ surname)
   ;; A procedure definition not beginning and ending with matching names does not impair parsing,
   ;; but users should be warned of it.
   (when (not (string= forename surname))
     (warn 'inconsistent-procedure-names-warning :forename forename :surname surname))
   (list :type :procedure
         :fields (list :name forename
                       :parameters parameters
                       :statements statements
                       :local-bindings (->> (movef *procedure-local-bindings* nil)
                                            (remove forename)
                                            (remove surname)                          ; The procedure names are not bindings.
                                            (remove-if (rcurry #'member parameters))  ; Neither are the parameters 
                                            (remove-if (rcurry #'member *program-arrays*))) ; Neither are arrays
                       :arrays (movef *procedure-arrays* nil))
         :meta *statement-position*)))

(defrule parameter-list (and (and #\( (? whitespace)) (? identifier-list) (and (? whitespace) #\)))
  (:function second)
  (:lambda (parameters)
   (if (null (first parameters))
       nil
       parameters)))

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
   (when (getf statement :type) ; Don't annotate comments.
     (append statement '(:meta) (list *statement-position*)))))

(defrule display-statement (and (and "Display" whitespace) display-statement-arguments)
  (:destructure (_ fields)
   ;; There are two styles of display statement arguments. The course specifications implicitly
   ;; concatenate arguments, but some exam papers place ampersands between them.
   ;; TODO: Support explicit concatenated display statements.
   (list :type :display-statement :fields fields)))

(define-delimited-list-rule display-statement-arguments expression whitespace)

(defrule return-statement (and (and "RETURN" whitespace) (? expression))
  (:destructure (_ value)
   (list :type :return-statement :fields (list value))))

(defrule let-statement (and (? (and (or "Let" "Set" "LET") whitespace))
                            assignment-operation)
  (:destructure (_ assignment)
   (list :type :let-statement :fields assignment)))

(defrule get-statement (and (and (or "Get" "Read" "Input" "input") whitespace) identifier-list)
  (:destructure (_ arguments)
   (list :type :get-statement :fields arguments)))

(defrule open-statement (and (and "Open" whitespace)
                             identifier
                             (and whitespace "for" whitespace)
                             open-statement-mode)
  (:destructure (_ filespec _ mode)
   (list :type :open-statement (list :filespec filespec :mode mode))))

(defrule open-statement-mode (or "output" "input" "append" "relative access"))

;; There are two standard syntaxes for reading from files.
(defrule read-statement (and (and "Read" whitespace)
                             (or read-statement-inner-1
                                 read-statement-inner-2))
  (:destructure (_ fields)
   (list :type :read-statement :fields fields)))

(defrule read-statement-inner-1 (and identifier-list " from " identifier)
  (:destructure (identifiers _ filespec)
   (list :identifiers identifiers :filespec filespec)))

(defrule read-statement-inner-2 (and identifier " into " identifier-list)
  (:destructure (filespec _ identifiers)
   (list :identifiers identifiers :filespec filespec)))

(defrule write-statement (and "Write " identifier " from " identifier-list)
  (:destructure (filespec _ identifiers)
   (list :type :write-statement :fields (list filespec identifiers))))

(defrule close-statement (and "Close " identifier)
  (:destructure (_ filespec)
   (list :type :close-statement :fields (list filespec))))

(defrule dim-statement (and "DIM "
                            identifier
                            (? whitespace)
                            dimension-list
                            " as "
                            identifier)
  (:destructure (_ name _ dimensions _ type)
   ;; Arrays should not appear in the list of local bindings as they are global.
   (setf *procedure-local-bindings* (remove name *procedure-local-bindings*))
   ;; However, record it in *PROCEDURE-ARRAYS* and *PROGRAM-ARRAYS*.
   (set-insert name *procedure-arrays*) ; SET-INSERT protects us from duplicate definitions.
   (set-insert name *program-arrays*)
   ;; The type name should not appear either as they are not local bindings.
   (setf *procedure-local-bindings* (remove type *procedure-local-bindings*))
   (list :type :dim-statement :fields (list name dimensions type))))

(defrule if-statement (and "IF " expression (and " THEN" newline)
                           (* indented-statement)
                           (? (and (and (? whitespace) "ELSE" newline)
                                   (* indented-statement)))
                           (and (? whitespace) "ENDIF")) ; Cater for indents
  (:destructure (_ condition _ then-block (&optional _ else-block) _)
   (list :type :if-statement :fields (list condition then-block else-block))))

(defrule casewhere-statement (and "CASEWHERE " expression (and " evaluates to" newline)
                                  casewhere-statement-choice-list
                                  (? (and (and (? whitespace) ; Indent
                                               "OTHERWISE"
                                               (? whitespace)
                                               ":" (? whitespace))
                                          casewhere-statement-process))
                                  (and (? whitespace) "ENDCASE"))
  (:destructure (_ test _ choices (&optional _ otherwise-process) _)
   (list :type :casewhere-statement :fields (list test choices otherwise-process))))

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
   (if (eq a :newline)
       b
       ;; Turn one-liners into a list regardless so it is uniform.
       (list a))))

(defrule while-statement (and "WHILE " expression newline
                              indented-block
                              (and (? whitespace) "ENDWHILE"))
  (:destructure (_ condition _ statements _)
   (list :type :while-statement :fields (list condition statements))))

(defrule for-statement (and "FOR "
                            identifier
                            " = "
                            expression
                            (or " TO " " to ")
                            expression
                            (? (and " STEP " expression))
                            newline
                            (* indented-statement)
                            (and (? whitespace) "NEXT ") identifier)
  (:destructure (_ counter _ init-value _ finish-value (&optional _ step-value) _ statements _ counter*)
   (declare (ignore _ counter*))
   ;; TODO: Compare counter identifiers.
   (list :type :for-statement
         :fields (list counter init-value finish-value step-value statements))))

(defrule assignment-operation (and (or array-access-or-procedure-call identifier)
                                   (and (? whitespace) #\= (? whitespace))
                                   expression)
  (:destructure (lhs _ rhs)
   (list :lhs lhs :rhs rhs)))

(defrule repeat-statement (and (and "REPEAT" newline)
                               (* indented-statement)
                               (and (? whitespace) "UNTIL ") expression)
  (:destructure (_ statements _ condition)
   (list :type :repeat-statement :fields (list condition statements))))

(defrule expression binary-logical-operation)

(define-binary-operation-rule binary-logical-operation not-operation (or "AND" "OR"))

(define-unary-operation-rule not-operation equality-operation "NOT")

(define-binary-operation-rule equality-operation comparison-operation "="
  (:destructure (&whole form &key type fields)
   (declare (ignore type))
   (handler-case (when (eq (getf fields :operator) '=)
                   (setf (getf fields :operator) 'equalp))
     (simple-type-error () form))
   form))

(define-binary-operation-rule comparison-operation term-operation (or "<=" ">=" "<>" "<" ">"))

(define-binary-operation-rule term-operation factor-operation (or "+" "-"))

(define-binary-operation-rule factor-operation sign-operation (or "*" "/"))

(define-unary-operation-rule sign-operation length-of-operation (or "+" "-"))

(define-unary-operation-rule length-of-operation value "Length of"
  (:destructure (&whole form &key type fields)
   (declare (ignore type))
   (handler-case
     (when (string= (getf fields :operator) "Length of")
       (setf (getf fields :operator) 'length))
     (simple-type-error () form))
   form))

(defrule value (or literal
                   nested-expression
                   array-access-or-procedure-call
                   value-identifier))

(defrule value-identifier identifier
  (:lambda (production)
   (list :type :binding :fields (list production))))

(defrule literal (or numeric-literal string-literal boolean-literal)
  (:lambda (production)
   (list :type :literal :fields (list production))))

;; To future me: you have permission to modify the productions of the literal rules.
;; Thanks past me.

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
                                             (? expression-list)
                                             (and (? whitespace) #\)))
  (:destructure (name _ arguments _)
   (declare (ignore _))
   ;; Do not record the array or procedure identifier as a binding.
   (setf *procedure-local-bindings* (remove name *procedure-local-bindings*))
   `(:type :array-access-or-procedure-call
     :fields (:identifier ,name :arguments ,arguments))))

(define-delimited-list-rule expression-list expression #\,)

(define-delimited-list-rule identifier-list identifier #\,)

(defrule identifier (and (+ (or (alphanumericp character) #\- #\_)))
  (:lambda (production)
   ;; Record all identifiers in the current procedure.
   (let ((interned (intern (text production) :davis.user)))
     (set-insert interned *procedure-local-bindings*)
     interned)))

(defrule comment (and (or #\' #\RIGHT_SINGLE_QUOTATION_MARK) (* (not #\Newline)))
  (:constant nil)
  (:error-report nil))

(defrule record (and "DIM RECORD " identifier newline
                     record-slot-list
                     "END RECORD")
  (:destructure (_ name _ slots _)
   (list :type :record :fields (list :name name :slots slots))))

;; TODO: Add proper indentation support.
(defrule record-slot-list (* (and (? whitespace) record-slot newline))
  (:lambda (production)
   (mapcar #'second production)))

(defrule record-slot (and identifier " as " (and identifier (? (and (? whitespace) dimension-list))))
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

(defgeneric <= (a b)
  (:method ((a number) (b number))
   (cl:<= a b))
  (:method ((a string) (b string))
   (string<= a b)))

(defgeneric < (a b)
  (:method ((a number) (b number))
   (cl:< a b))
  (:method ((a string) (b string))
   (string< a b)))

(defgeneric >= (a b)
  (:method ((a number) (b number))
   (cl:>= a b))
  (:method ((a string) (b string))
   (string>= a b)))

(defgeneric > (a b)
  (:method ((a number) (b number))
   (cl:> a b))
  (:method ((a string) (b string))
   (string> a b)))

;;; Warnings

(define-condition inconsistent-procedure-names-warning (warning)
  ((forename :initarg :forename)
   (surname :initarg :surname))
  (:report (lambda (condition stream)
             (with-slots (forename surname) condition
               (format stream
                       "Procedure ~a is inconsistently named ~a"
                       forename
                       surname)))))

(define-condition indentation-warning (warning)
  ((position :initarg :position)))
