DATABASE hrmdb
DEFINE payment_rec RECORD LIKE payment.*
DEFINE query_ok SMALLINT
DEFINE match STRING
DEFINE i INT 
DEFINE hndl om.SaxDocumentHandler


FUNCTION payment_fun()
   
        OPEN WINDOW PAYMENT WITH FORM "paymentper"
        CALL fun_payment()
        
CLOSE WINDOW PAYMENT
END FUNCTION

FUNCTION fun_payment()
DEFINE query_ok smallint

    MENU 
        COMMAND "find"
        CLEAR FORM 
            CALL payment_query() RETURNING query_ok
        COMMAND "next"
            IF (query_ok) THEN
                CALL payment_fetch_rel(1)
            ELSE
                MESSAGE "YOU MUST QUERY FIRST." 

            END IF
        COMMAND "previous"
            IF (query_ok) THEN
                CALL payment_fetch_rel(-1)
            ELSE
                MESSAGE "YOU MUST QUERY FIRST."
            END IF

        ON ACTION ADD 
       IF(payment_input_clg("A")) then
            CALL payment_insert()
            END IF

        COMMAND "Modify"
            CALL payment_update()

        COMMAND "Delete"
        IF (payment_delete_check()) THEN 
            CALL payment_delete()
            END IF 
            COMMAND "clear"
            CLEAR FORM 
          COMMAND "quit"
            EXIT MENU
            COMMAND "print"
            CALL payment_report()
    END MENU

END FUNCTION


FUNCTION payment_query()
    DEFINE
        cont_ok SMALLINT,
        cust_cnt SMALLINT,
        where_clause STRING

    MESSAGE "Enter search criteria"
    LET cont_ok = FALSE                 
    LET int_flag = FALSE                          

    CONSTRUCT BY NAME where_clause ON payment.*
                        
    
    IF (int_flag) = TRUE THEN
        LET int_flag = FALSE                       
        CLEAR FORM
        LET cont_ok = FALSE
        MESSAGE "CANCELED BY USER"
    ELSE
        CALL payment_get_cust_cnt(where_clause) RETURNING cust_cnt
        IF (cust_cnt > 0) THEN
            MESSAGE cust_cnt USING "<<<<", " ROWS FOUND"
            CALL payment_cust_select(where_clause) RETURNING cont_ok   
        ELSE
            MESSAGE "NO ROWS FOUND"
            LET cont_ok = FALSE
        END IF
    END IF

    IF (cont_ok) THEN
        CALL payment_display()
    END IF

    RETURN cont_ok

END FUNCTION

FUNCTION payment_get_cust_cnt(p_where_clause)     
    DEFINE
        p_where_clause STRING,
        sql_text STRING,
        cust_cnt SMALLINT

    LET sql_text = "SELECT COUNT(*)
 FROM payment  WHERE " || p_where_clause CLIPPED
  
    WHENEVER ERROR CONTINUE
    PREPARE cust_cnt_stmt FROM sql_text  
    EXECUTE cust_cnt_stmt INTO cust_cnt
    FREE cust_cnt_stmt
    WHENEVER ERROR STOP

    IF SQLCA.SQLCODE <> 0 THEN         
        LET cust_cnt = 0
        ERROR SQLERRMESSAGE
    END IF

    RETURN cust_cnt

END FUNCTION

FUNCTION payment_cust_select(p_where_clause) 
    DEFINE
        p_where_clause STRING,
        sql_text STRING,
        fetch_ok SMALLINT

    LET sql_text = "SELECT * FROM payment WHERE " || p_where_clause CLIPPED 

    DECLARE cust_curs SCROLL CURSOR WITH HOLD FROM sql_text

    OPEN cust_curs
    CALL payment_fetch_cust(1)
        RETURNING fetch_ok 
    IF NOT (fetch_ok) THEN
        MESSAGE "NO ROWS IN THE TABLE" 
    END IF

    RETURN fetch_ok   


END FUNCTION

FUNCTION payment_fetch_cust(p_fetch_flag)
    DEFINE
        p_fetch_flag SMALLINT,
        fetch_ok SMALLINT

    LET fetch_ok = TRUE
    WHENEVER ERROR CONTINUE
    IF p_fetch_flag = 1 THEN
        FETCH NEXT cust_curs INTO payment_rec.*
    ELSE
        FETCH PREVIOUS cust_curs INTO payment_rec.*
    END IF

     CASE 
        WHEN (SQLCA.SQLCODE = 0)
            LET fetch_ok = TRUE
        WHEN (SQLCA.SQLCODE = NOTFOUND)
            LET fetch_ok = FALSE
        WHEN (SQLCA.SQLCODE < 0)
            LET fetch_ok = FALSE
            MESSAGE " Error ", SQLCA.SQLCODE USING "<<<<" 
    END CASE

    RETURN fetch_ok

END FUNCTION

FUNCTION payment_fetch_rel(p_fetch_flag)
    DEFINE
        p_fetch_flag SMALLINT,
        fetch_ok SMALLINT

    MESSAGE " "
    CALL payment_fetch_cust(p_fetch_flag) RETURNING fetch_ok

    IF (fetch_ok) THEN
        CALL payment_display()
    ELSE
        IF (p_fetch_flag = 1) THEN
            MESSAGE "END OF LIST"
        ELSE
            MESSAGE "BEGINING OF LIST"
        END IF
    END IF

END FUNCTION

FUNCTION payment_display()
    DISPLAY BY NAME payment_rec.*              
END FUNCTION

FUNCTION payment_input_clg(au_flag)
      DEFINE au_flag CHAR(20),
      count_ok INTEGER 

      LET count_ok = TRUE
      IF(au_flag = "A") THEN
              MESSAGE "ENTER PAYMENT DETAILS"

              INITIALIZE payment_rec.* TO NULL
      END IF 

     LET int_flag = FALSE 

     INPUT BY NAME payment_rec.* WITHOUT DEFAULTS ATTRIBUTES (UNBUFFERED)
     ON CHANGE Payment_type
    IF au_flag="A" THEN
    SELECT Payment_type,Unit_number,Paid_date,Paid_for,Amount,Paid_by,Collected_by,Remarks
    INTO payment_rec.*
    FROM payment
    WHERE Payment_type = payment_rec.payment_type
    IF(sqlca.sqlcode=0)THEN
    ERROR " ALREADY IN DATABASE"
    LET count_ok =FALSE
    EXIT INPUT
    END IF 
    END IF

    AFTER FIELD Payment_type
    IF payment_rec.payment_type IS NULL THEN
    ERROR "ENTER PAYMENT TYPE"
    NEXT FIELD Payment_type
    END IF

     AFTER FIELD unit_number
    IF payment_rec.unit_number IS NULL THEN
    MESSAGE "ENTER UNIT NUMBER "
    NEXT FIELD unit_number
    END IF


    IF  payment_rec.unit_number <= 0 then
    ERROR "UNIT NUMBER SHOULD BE GREATER THAN ZERO"
    NEXT FIELD unit_number
    END IF
    
    AFTER FIELD Paid_date
    IF payment_rec.paid_date IS NULL THEN
    MESSAGE "ENTER PAYMENT DATE"
    NEXT FIELD Paid_date
    END IF


    AFTER FIELD Paid_for
    IF payment_rec.paid_for IS NULL THEN
    MESSAGE "ENTER PAID FOR FIELD"
    NEXT FIELD Paid_for
    END IF

     AFTER FIELD Amount
    IF payment_rec.amount IS NULL THEN
    MESSAGE "ENTER AMOUNT FIELD"
    NEXT FIELD Amount
    END IF

     AFTER FIELD Paid_by
    IF payment_rec.paid_by IS NULL THEN
    MESSAGE "ENTER PAID BY FIELD"
    NEXT FIELD Paid_by
    END IF

      AFTER FIELD Collected_by
    IF payment_rec.collected_by IS NULL THEN
    MESSAGE "ENTER COLLECTED BY FILED"
    NEXT FIELD Collected_by
    END IF



    AFTER FIELD Remarks
    IF payment_rec.remarks IS NULL THEN
    MESSAGE "ENTER REMARKS FIELD "
    NEXT FIELD Remarks
    END IF


NEXT FIELD Payment_type
            END INPUT 

            IF (int_flag) THEN 
                LET int_flag = FALSE
                LET count_ok = FALSE 
                MESSAGE "OPERATION CANCELLED BY USER"
            END IF 

            RETURN count_ok

 END FUNCTION 

FUNCTION payment_insert()
    INPUT BY NAME payment_rec.* WITHOUT DEFAULTS ATTRIBUTE(UNBUFFERED)

    WHENEVER ERROR CONTINUE
    INSERT INTO payment VALUES(payment_rec.*)
    WHENEVER ERROR STOP

    IF SQLCA.SQLCODE = 0 THEN
        MESSAGE "ROW ADDED"
    ELSE
        ERROR SQLERRMESSAGE
    END IF

END FUNCTION

FUNCTION payment_update()
   

    WHENEVER ERROR CONTINUE
    SELECT  INTO payment_rec FROM payment WHERE Payment_type = payment_rec.Payment_type 
    INPUT BY NAME payment_rec.* WITHOUT DEFAULTS ATTRIBUTE(UNBUFFERED)
    WHENEVER ERROR STOP

    WHENEVER ERROR CONTINUE
    UPDATE payment
        SET  
              Unit_number = payment_rec.unit_number,
              Paid_date = payment_rec.paid_date,
              Paid_for =payment_rec.paid_for,
              Amount =payment_rec.amount,
              issue_date =payment_rec.paid_by,
              Collected_by = payment_rec.collected_by,
              Remarks =payment_rec.remarks

                WHERE Payment_type = payment_rec.payment_type
    WHENEVER ERROR STOP

    IF SQLCA.SQLCODE = 0 THEN
        MESSAGE "ROW UPDATED"
    ELSE
        ERROR SQLERRMESSAGE
    END IF

END FUNCTION
 
FUNCTION payment_delete()
  DEFINE del_ok SMALLINT
  
    WHENEVER ERROR CONTINUE
    DELETE FROM payment WHERE Payment_type = payment_rec.payment_type
    WHENEVER ERROR STOP
    IF SQLCA.SQLCODE = 0 THEN
       MESSAGE "ROW DELETED"
       INITIALIZE payment_rec.* TO NULL
       DISPLAY BY NAME payment_rec.* 
    ELSE
      ERROR SQLERRMESSAGE
    END IF 
 
 END FUNCTION   

FUNCTION payment_delete_check()
  DEFINE del_ok SMALLINT,
         del_count SMALLINT
  
  LET del_ok = FALSE
    
  WHENEVER ERROR CONTINUE
  SELECT COUNT(*)
 INTO del_count FROM payment 
    WHERE payment.Payment_type = payment_rec.payment_type
  WHENEVER ERROR STOP
  
  IF del_count = 0 THEN
    MESSAGE "NO RECORD FOUND SO CANNOT BE DELETED"
    LET del_ok = FALSE
  ELSE
    MENU  "DELETE" ATTRIBUTE ( STYLE="dialog", COMMENT="DELETE THE ROW?" )
    COMMAND "YES"
      LET del_ok = TRUE
      EXIT MENU
    COMMAND "NO"
      MESSAGE "OPERATION CANCLLED"
      EXIT MENU
    END MENU
  END IF
    
  RETURN del_ok
  
END FUNCTION

FUNCTION payment_clean_up()

    WHENEVER ERROR CONTINUE
    CLOSE cust_curs
    FREE cust_curs
    WHENEVER ERROR STOP

END FUNCTION

 FUNCTION payment_report()

    DEFINE i2genrec RECORD
                      LIKE payment.*
       

      IF fgl_report_LoadCurrentSettings("payreport.4rp") THEN
        CALL fgl_report_SelectDevice("PDF")
        CALL fgl_report_SelectPreview(TRUE)
        LET hndl = fgl_report_commitCurrentSettings()

    ELSE
        EXIT PROGRAM
    END IF

    START REPORT payreport TO XML HANDLER hndl

    DECLARE i2curr1 CURSOR FOR
        SELECT *
            FROM payment
        

    FOREACH i2curr1 INTO i2genrec.*
        OUTPUT TO REPORT payreport(i2genrec.*)

    END FOREACH

    FINISH REPORT payreport

END FUNCTION

REPORT payreport(printi2genrec)

    DEFINE printi2genrec RECORD
         LIKE payment.*
       

    FORMAT

--FIRST PAGE HEADER
--PAGE HEADER
--PRINTX createdate
        ON EVERY ROW
--DISPLAY "printi2genrec", printi2genrec.id

            PRINTX printi2genrec.payment_type
            PRINTX printi2genrec.unit_number
            PRINTX printi2genrec.paid_date
            PRINTX printi2genrec.paid_for
            PRINTX printi2genrec.amount
            PRINTX printi2genrec.paid_by
            PRINTX printi2genrec.collected_by
            PRINTX printi2genrec.remarks
            --LET createdate = DATE(CURRENT)
            --LET lin_x = "----------------------------------------------------------------------------------------------------------------------------------"
            --PRINTX lin_x
            --PRINTX createdate

--AFTER GROUP OF printi2genrec.id
-- PRINTX printi2genrec.id
            --SKIP 2 LINES

END REPORT
 
   
 
  