DATABASE hrmdb
DEFINE helpdesk_rec RECORD LIKE helpdesk.*
DEFINE query_ok SMALLINT
DEFINE match STRING
DEFINE i INT 
DEFINE hndl om.SaxDocumentHandler


FUNCTION helpdesk_fun()
   
        OPEN WINDOW HELPDESK WITH FORM "helpdeskper"
        CALL fun_helpdesk()
        
CLOSE WINDOW HELPDESK
END FUNCTION

FUNCTION fun_helpdesk()
DEFINE query_ok smallint

    MENU 
        COMMAND "find"
        CLEAR FORM 
            CALL helpdesk_query() RETURNING query_ok
        COMMAND "next"
            IF (query_ok) THEN
                CALL helpdesk_fetch_rel(1)
            ELSE
                MESSAGE "YOU MUST QUERY FIRST." 

            END IF
        COMMAND "previous"
            IF (query_ok) THEN
                CALL helpdesk_fetch_rel(-1)
            ELSE
                MESSAGE "YOU MUST QUERY FIRST."
            END IF

        ON ACTION ADD 
       IF(helpdesk_input_clg("A")) then
            CALL helpdesk_insert()
            END IF

        COMMAND "Modify"
            CALL helpdesk_update()

        COMMAND "Delete"
        IF (helpdesk_delete_check()) THEN 
            CALL helpdesk_delete()
            END IF 
            COMMAND "clear"
            CLEAR FORM 
          COMMAND "quit"
            EXIT MENU
            COMMAND "print"
            CALL helpdesk_report()
    END MENU

END FUNCTION


FUNCTION helpdesk_query()
    DEFINE
        cont_ok SMALLINT,
        cust_cnt SMALLINT,
        where_clause STRING

    MESSAGE "ENTER SEARCH CRITERIA"
    LET cont_ok = FALSE                 
    LET int_flag = FALSE                          

    CONSTRUCT BY NAME where_clause ON helpdesk.*
                        
    
    IF (int_flag) = TRUE THEN
        LET int_flag = FALSE                       
        CLEAR FORM
        LET cont_ok = FALSE
        MESSAGE "CANCELED BY USER"
    ELSE
        CALL helpdesk_get_cust_cnt(where_clause) RETURNING cust_cnt
        IF (cust_cnt > 0) THEN
            MESSAGE cust_cnt USING "<<<<", " ROWS FOUND"
            CALL helpdesk_cust_select(where_clause) RETURNING cont_ok   
        ELSE
            MESSAGE "NO ROWS FOUND"
            LET cont_ok = FALSE
        END IF
    END IF

    IF (cont_ok) THEN
        CALL helpdesk_display()
    END IF

    RETURN cont_ok

END FUNCTION

FUNCTION helpdesk_get_cust_cnt(p_where_clause)     
    DEFINE
        p_where_clause STRING,
        sql_text STRING,
        cust_cnt SMALLINT

    LET sql_text = "SELECT COUNT(*)
 FROM helpdesk  WHERE " || p_where_clause CLIPPED
  
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

FUNCTION helpdesk_cust_select(p_where_clause) 
    DEFINE
        p_where_clause STRING,
        sql_text STRING,
        fetch_ok SMALLINT

    LET sql_text = "SELECT * FROM helpdesk WHERE " || p_where_clause CLIPPED 

    DECLARE cust_curs SCROLL CURSOR WITH HOLD FROM sql_text

    OPEN cust_curs
    CALL helpdesk_fetch_cust(1)
        RETURNING fetch_ok 
    IF NOT (fetch_ok) THEN
        MESSAGE "NO ROWS IN THE TABLE" 
    END IF

    RETURN fetch_ok   


END FUNCTION

FUNCTION helpdesk_fetch_cust(p_fetch_flag)
    DEFINE
        p_fetch_flag SMALLINT,
        fetch_ok SMALLINT

    LET fetch_ok = TRUE
    WHENEVER ERROR CONTINUE
    IF p_fetch_flag = 1 THEN
        FETCH NEXT cust_curs INTO helpdesk_rec.*
    ELSE
        FETCH PREVIOUS cust_curs INTO helpdesk_rec.*
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

FUNCTION helpdesk_fetch_rel(p_fetch_flag)
    DEFINE
        p_fetch_flag SMALLINT,
        fetch_ok SMALLINT

    MESSAGE " "
    CALL helpdesk_fetch_cust(p_fetch_flag) RETURNING fetch_ok

    IF (fetch_ok) THEN
        CALL helpdesk_display()
    ELSE
        IF (p_fetch_flag = 1) THEN
            MESSAGE "END OF LIST"
        ELSE
            MESSAGE "BEGINING OF LIST"
        END IF
    END IF

END FUNCTION

FUNCTION helpdesk_display()
    DISPLAY BY NAME helpdesk_rec.*              
END FUNCTION

FUNCTION helpdesk_input_clg(au_flag)
      DEFINE au_flag CHAR(20),
      count_ok INTEGER 

      LET count_ok = TRUE
      IF(au_flag = "A") THEN
              MESSAGE "ENTER HELP DESK DETAILS"

              INITIALIZE helpdesk_rec.* TO NULL
      END IF 

     LET int_flag = FALSE 

     INPUT BY NAME helpdesk_rec.* WITHOUT DEFAULTS ATTRIBUTES (UNBUFFERED)
     ON CHANGE Attended_by
    IF au_flag="A" THEN
    SELECT Attended_by,Complaint_by,Completed_by,Issue,Issue_date,Status,Unit_number
    INTO helpdesk_rec.*
    FROM helpdesk
    WHERE Attended_by = helpdesk_rec.attended_by
    IF(sqlca.sqlcode=0)THEN
    ERROR " ALREADY IN DATABASE"
    LET count_ok =FALSE
    EXIT INPUT
    END IF 
    END IF

    AFTER FIELD Attended_by
    IF helpdesk_rec.attended_by IS NULL THEN
    ERROR "ENTER ATTENDED BY FIELD"
    NEXT FIELD Attended_by
    END IF
    IF NOT helpdesk_rec.attended_by MATCHES "[a-z A-Z]*"  THEN
    MESSAGE "NAME SHOULD  BE CHARACTER"
    NEXT FIELD Attended_by
    END IF
    AFTER FIELD Complaint_by
    IF helpdesk_rec.complaint_by IS NULL THEN
    MESSAGE "ENTER COMPLAINT BY FIELD"
    NEXT FIELD Complaint_by
    END IF
    IF NOT helpdesk_rec.complaint_by MATCHES "[a-z A-Z]*"  THEN
    MESSAGE "NAME SHOULD  BE CHARACTER"
    NEXT FIELD Complaint_by
    END IF
    LET match =helpdesk_rec.complaint_by
    FOR i=1 TO LENGTH(match)
    IF match.getCharAt(i) MATCHES "[0-9]" THEN
    ERROR "NAME SHOULD BE CHARACTER"
    NEXT FIELD Complaint_by
    END IF
    END FOR

    AFTER FIELD Completed_by
    IF helpdesk_rec.completed_by IS NULL THEN
    MESSAGE "ENTER COMPLETED BY FIELD"
    NEXT FIELD Completed_by
    END IF

     AFTER FIELD Issue
    IF helpdesk_rec.issue IS NULL THEN
    MESSAGE "ENTER ISSUE FIELD"
    NEXT FIELD Issue
    END IF

     AFTER FIELD Issue_date
    IF helpdesk_rec.issue_date IS NULL THEN
    MESSAGE "ENTER DATE FIELD"
    NEXT FIELD Issue_date
    END IF

      AFTER FIELD Status
    IF helpdesk_rec.status IS NULL THEN
    MESSAGE "ENTER STATUS FIELD"
    NEXT FIELD Status
    END IF



    AFTER FIELD unit_number
    IF helpdesk_rec.unit_number IS NULL THEN
    MESSAGE "ENTER UNIT NUMBER "
    NEXT FIELD unit_number
    END IF


    IF  helpdesk_rec.unit_number <= 0 then
    ERROR "UNIT NUMBER SHOULD BE GREATER THAN ZERO"
    NEXT FIELD unit_number
    END IF


NEXT FIELD Attended_by
            END INPUT 

            IF (int_flag) THEN 
                LET int_flag = FALSE
                LET count_ok = FALSE 
                MESSAGE "OPERATION CANCELLED BY USER"
            END IF 

            RETURN count_ok

 END FUNCTION 

FUNCTION helpdesk_insert()
    INPUT BY NAME helpdesk_rec.* WITHOUT DEFAULTS ATTRIBUTE(UNBUFFERED)

    WHENEVER ERROR CONTINUE
    INSERT INTO helpdesk VALUES(helpdesk_rec.*)
    WHENEVER ERROR STOP

    IF SQLCA.SQLCODE = 0 THEN
        MESSAGE "ROW ADDED"
    ELSE
        ERROR SQLERRMESSAGE
    END IF

END FUNCTION

FUNCTION helpdesk_update()
   

    WHENEVER ERROR CONTINUE
    SELECT  INTO helpdesk_rec FROM helpdesk WHERE Attended_by = helpdesk_rec.attended_by 
    INPUT BY NAME helpdesk_rec.* WITHOUT DEFAULTS ATTRIBUTE(UNBUFFERED)
    WHENEVER ERROR STOP

    WHENEVER ERROR CONTINUE
    UPDATE helpdesk
        SET  
              Complaint_by = helpdesk_rec.complaint_by,
              Completed_by = helpdesk_rec.completed_by,
              completed_by =helpdesk_rec.completed_by,
              Issue =helpdesk_rec.issue,
              issue_date =helpdesk_rec.issue_date,
              status =helpdesk_rec.status,
              unit_number =helpdesk_rec.unit_number

                WHERE Attended_by = helpdesk_rec.attended_by
    WHENEVER ERROR STOP

    IF SQLCA.SQLCODE = 0 THEN
        MESSAGE "ROW UPDATED"
    ELSE
        ERROR SQLERRMESSAGE
    END IF

END FUNCTION
 
FUNCTION helpdesk_delete()
  DEFINE del_ok SMALLINT
  
    WHENEVER ERROR CONTINUE
    DELETE FROM helpdesk WHERE Attended_by = helpdesk_rec.attended_by
    WHENEVER ERROR STOP
    IF SQLCA.SQLCODE = 0 THEN
       MESSAGE "ROW DELETED"
       INITIALIZE helpdesk_rec.* TO NULL
       DISPLAY BY NAME helpdesk_rec.* 
    ELSE
      ERROR SQLERRMESSAGE
    END IF 
 
 END FUNCTION   

FUNCTION helpdesk_delete_check()
  DEFINE del_ok SMALLINT,
         del_count SMALLINT
  
  LET del_ok = FALSE
    
  WHENEVER ERROR CONTINUE
  SELECT COUNT(*)
 INTO del_count FROM helpdesk
    WHERE helpdesk.Attended_by = helpdesk_rec.attended_by
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

FUNCTION helpdesk_clean_up()

    WHENEVER ERROR CONTINUE
    CLOSE cust_curs
    FREE cust_curs
    WHENEVER ERROR STOP

END FUNCTION
FUNCTION helpdesk_report()

    DEFINE i2genrec RECORD
                      LIKE helpdesk.*
       

      IF fgl_report_LoadCurrentSettings("helpreport.4rp") THEN
        CALL fgl_report_SelectDevice("PDF")
        CALL fgl_report_SelectPreview(TRUE)
        LET hndl = fgl_report_commitCurrentSettings()

    ELSE
        EXIT PROGRAM
    END IF

    START REPORT hpreport TO XML HANDLER hndl

    DECLARE i2curr1 CURSOR FOR
        SELECT *
            FROM helpdesk
        

    FOREACH i2curr1 INTO i2genrec.*
        OUTPUT TO REPORT hpreport(i2genrec.*)

    END FOREACH

    FINISH REPORT hpreport

END FUNCTION

REPORT hpreport(printi2genrec)

    DEFINE printi2genrec RECORD
         LIKE helpdesk.*
       

    FORMAT

--FIRST PAGE HEADER
--PAGE HEADER
--PRINTX createdate
        ON EVERY ROW
--DISPLAY "printi2genrec", printi2genrec.id

            PRINTX printi2genrec.attended_by
            PRINTX printi2genrec.complaint_by
            PRINTX printi2genrec.completed_by
            PRINTX printi2genrec.issue
            PRINTX printi2genrec.issue_date
            PRINTX printi2genrec.status
            PRINTX printi2genrec.unit_number
            --printi2genrec.phn_number
            --LET createdate = DATE(CURRENT)
            --LET lin_x = "----------------------------------------------------------------------------------------------------------------------------------"
            --PRINTX lin_x
            --PRINTX createdate

--AFTER GROUP OF printi2genrec.id
-- PRINTX printi2genrec.id
            --SKIP 2 LINES

END REPORT
 
 
   
 
  