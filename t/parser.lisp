;;; Parser module tests.

(in-package :davis-test.parser)

(def-suite parser)

(in-suite parser)

(test snippet-tests
  (finishes (parse 'd::top-level-form-list
"BEGIN LoadArray
     Let i = 1
     Read DataValue
     WHILE DataValue <> “xxx”
         Let Element (i) = DataValue
         i = i + 1
         Read DataValue
     ENDWHILE
     Let NumElements = i
     Display “ There are” NumElements “ items loaded into the array”
END LoadArray")))

(test casewhere-statement-test
  (finishes (parse 'd::casewhere-statement
"CASEWHERE x evaluates to
     \"Red\": Display \"Stop\"
     \"Amber\": Display \"Slow\"
     \"Green\": Display \"Go\"
     OTHERWISE: Display \"ERROR\"
ENDCASE")))

(test if-statement-test
  (finishes (parse 'd::top-level-form-list
"BEGIN MAINPROGRAM
	Display Pascal (3, 5)
END MAINPROGRAM

BEGIN Pascal (Col, Row)
	IF Col = 0 OR Col = Row THEN
		RETURN 1
	ELSE
		RETURN Pascal (Col, Row - 1) + Pascal (Col - 1, Row - 1)
	ENDIF
END Pascal")))

(test simple-statement-tests
  (is-true (equal (parse 'd::display-statement "Display \"Hi\" name")
                  '(:type :display-statement :fields ("Hi" davis.user::|name|))))
  (signals esrap-parse-error
    (parse 'd::display-statement "DISPLAY \"Only a fool would write this\""))

  (is-true (equal (parse 'd::return-statement "RETURN 0")
                  '(:type :return-statement :fields (0))))
  (signals esrap-parse-error (parse 'd::return-statement "Return"))

  (is-true (equal (parse 'd::let-statement "Let CamelCaseName=false")
                  '(:type :let-statement :fields (:lhs davis.user::|CamelCaseName| :rhs nil)))))

(test <>-test
  (is-true (equal (parse 'd::expression "A <> B")
                  '(d::<> davis.user::a davis.user::b))))
