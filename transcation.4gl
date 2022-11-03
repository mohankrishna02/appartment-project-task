DATABASE hrmdb
GLOBALS 

DEFINE trans_rec RECORD LIKE owner.*
DEFINE indx INT 
DEFINE query_ok INT
DEFINE hndl om.SaxDocumentHandler

END GLOBALS

FUNCTION trans_fun() 
OPEN WINDOW TRANSCATION WITH FORM "transcationper"
CALL fun_trans()
CLOSE WINDOW TRANSCATION
END FUNCTION


FUNCTION fun_trans()
DEFINE query_ok SMALLINT

MENU
        COMMAND "find"
        CLEAR FORM 
            CALL trans_query() RETURNING query_ok
       CALL trans_displayarr()
        ON ACTION EXIT
        EXIT MENU
        COMMAND "print"
        CALL trans_report()
END MENU
END FUNCTION


FUNCTION trans_query()
    DEFINE
        cont_ok SMALLINT,
        cust_cnt SMALLINT,
        where_clause STRING

    MESSAGE "Enter search criteria"
    LET cont_ok = FALSE                         
    LET int_flag = FALSE                          

    CONSTRUCT BY NAME where_clause ON owner.*
                        
    
    IF (int_flag) = TRUE THEN
        LET int_flag = FALSE                       
        CLEAR FORM
        LET cont_ok = FALSE
        MESSAGE "Canceled by user."
    ELSE
        CALL trans_get_cust_cnt(where_clause) RETURNING cust_cnt
        IF (cust_cnt > 0) THEN
            MESSAGE cust_cnt USING "<<<<", " rows found."
            CALL trans_cust_select(where_clause) RETURNING cont_ok   #
        ELSE
            MESSAGE "No rows found."
            LET cont_ok = FALSE
        END IF
    END IF

    IF (cont_ok) THEN
        CALL trans_display()
    END IF

    RETURN cont_ok

END FUNCTION
FUNCTION trans_get_cust_cnt(p_where_clause)     
    DEFINE
        p_where_clause STRING,
        sql_text STRING,
        cust_cnt SMALLINT

    LET sql_text = "SELECT COUNT(*)
 FROM owner , tanent WHERE owner.unit_number = tanent.unit_number1 AND " || p_where_clause CLIPPED
  
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

FUNCTION trans_cust_select(p_where_clause)  
    DEFINE
        p_where_clause STRING,
        sql_text STRING,
        fetch_ok SMALLINT

    LET sql_text = "SELECT * FROM owner,tanent WHERE owner.unit_number = tanent.unit_number1 AND " || p_where_clause CLIPPED 

    DECLARE cust_curs SCROLL CURSOR WITH HOLD FROM sql_text

    OPEN cust_curs
    CALL trans_fetch_cust(1) 
        RETURNING fetch_ok 
    IF NOT (fetch_ok) THEN
        MESSAGE "no rows in table." 
    END IF

    RETURN fetch_ok    


END FUNCTION

FUNCTION trans_fetch_cust(p_fetch_flag)
    DEFINE
        p_fetch_flag SMALLINT,
        fetch_ok SMALLINT

    LET fetch_ok = TRUE
    WHENEVER ERROR CONTINUE
    IF p_fetch_flag = 1 THEN
        FETCH NEXT cust_curs INTO trans_rec.*
    ELSE
        FETCH PREVIOUS cust_curs INTO trans_rec.*
    END IF

     CASE 
        WHEN (SQLCA.SQLCODE = 0)
            LET fetch_ok = TRUE
        WHEN (SQLCA.SQLCODE = NOTFOUND)
            LET fetch_ok = FALSE
        WHEN (SQLCA.SQLCODE < 0)
            LET fetch_ok = FALSE
            MESSAGE " Error ", SQLCA.SQLCODE USING "<<<<" #
    END CASE

    RETURN fetch_ok

END FUNCTION

FUNCTION trans_display()
DISPLAY BY NAME trans_rec.*
END FUNCTION

FUNCTION trans_displayarr()
DEFINE arr_curr DYNAMIC ARRAY OF RECORD LIKE tanent.*
DEFINE indx SMALLINT
DECLARE empList CURSOR FOR 
SELECT * FROM tanent
WHERE unit_number1=trans_rec.unit_number
LET indx = 1
WHENEVER ERROR CONTINUE 
FOREACH empList
INTO arr_curr[indx].*
LET indx = indx + 1
END FOREACH
WHENEVER ERROR STOP
DISPLAY ARRAY arr_curr TO mohan.*
END FUNCTION

FUNCTION trans_report()

    DEFINE i2genrec RECORD
        owner_name LIKE owner.owner_name,
        unit_number LIKE owner.unit_number,
        tanen_name LIKE tanent.tanen_name,
        join_on LIKE tanent.join_on

    END RECORD

    IF fgl_report_loadcurrentsettings("transreport.4rp") THEN
        CALL fgl_report_selectdevice("PDF")
        CALL fgl_report_selectpreview(TRUE)
        LET hndl = fgl_report_commitcurrentsettings()

    ELSE
        EXIT PROGRAM
    END IF

    START REPORT trreport TO XML HANDLER hndl

    DECLARE i2curr1 CURSOR FOR
        SELECT owner_name,unit_number,tanen_name,join_on
        FROM owner,tanent
        WHERE owner.unit_number = tanent.unit_number1
        --ORDER BY id

    FOREACH i2curr1 INTO i2genrec.*
        OUTPUT TO REPORT trreport(i2genrec.*)

    END FOREACH

    FINISH REPORT trreport

END FUNCTION

REPORT trreport(printi2genrec)

    DEFINE printi2genrec RECORD
        owner_name LIKE owner.owner_name,
        unit_number LIKE owner.unit_number,
        tanen_name LIKE tanent.tanen_name,
        join_on LIKE tanent.join_on
    
    END RECORD
   -- ORDER EXTERNAL BY printi2genrec.id

    FORMAT

        --FIRST PAGE HEADER
        --PAGE HEADER
        --    PRINTX createdate
        ON EVERY ROW
           -- DISPLAY "printi2genrec", printi2genrec.id

            PRINTX printi2genrec.owner_name, printi2genrec.unit_number,printi2genrec.tanen_name,printi2genrec.join_on
--            LET createdate = DATE(CURRENT)
--            PRINTX createdate
--
--        AFTER GROUP OF printi2genrec.id
--            PRINTX printi2genrec.id, "form record ";
--            SKIP 2 LINES

END REPORT
 
  





