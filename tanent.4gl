DATABASE hrmdb
DEFINE tanent_rec RECORD LIKE tanent.*
DEFINE query_ok SMALLINT
DEFINE match STRING
DEFINE i INT 
DEFINE hndl om.SaxDocumentHandler


FUNCTION tanent_fun()
   
        OPEN WINDOW TANENT WITH FORM "tanentper"
        CALL fun_tanent()
        
CLOSE WINDOW TANENT
END FUNCTION

FUNCTION fun_tanent()
DEFINE query_ok smallint

    MENU 
        COMMAND "find"
        CLEAR FORM 
            CALL tanent_query() RETURNING query_ok
        COMMAND "next"
            IF (query_ok) THEN
                CALL tanent_fetch_rel(1)
            ELSE
                MESSAGE "YOU MUST QUERY FIRST." 

            END IF
        COMMAND "previous"
            IF (query_ok) THEN
                CALL tanent_fetch_rel(-1)
            ELSE
                MESSAGE "YOU MUST QUERY FIRST."
            END IF

        ON ACTION ADD 
       IF(tanent_input_clg("A")) then
            CALL tanent_insert()
            END IF

        COMMAND "Modify"
            CALL tanent_update()

        COMMAND "Delete"
        IF (tanent_delete_check()) THEN 
            CALL tanent_delete()
            END IF 
            COMMAND "clear"
            CLEAR FORM 
          COMMAND "quit"
            EXIT MENU
            COMMAND "print"
            CALL tanent_report()
    END MENU

END FUNCTION


FUNCTION tanent_query()
    DEFINE
        cont_ok SMALLINT,
        cust_cnt SMALLINT,
        where_clause STRING

    MESSAGE "Enter search criteria"
    LET cont_ok = FALSE                 
    LET int_flag = FALSE                          

    CONSTRUCT BY NAME where_clause ON tanent.*
                        
    
    IF (int_flag) = TRUE THEN
        LET int_flag = FALSE                       
        CLEAR FORM
        LET cont_ok = FALSE
        MESSAGE "CANCELED BY USER"
    ELSE
        CALL tanent_get_cust_cnt(where_clause) RETURNING cust_cnt
        IF (cust_cnt > 0) THEN
            MESSAGE cust_cnt USING "<<<<", " ROWS FOUND"
            CALL tanent_cust_select(where_clause) RETURNING cont_ok   
        ELSE
            MESSAGE "NO ROWS FOUND"
            LET cont_ok = FALSE
        END IF
    END IF

    IF (cont_ok) THEN
        CALL tanent_display()
    END IF

    RETURN cont_ok

END FUNCTION

FUNCTION tanent_get_cust_cnt(p_where_clause)     
    DEFINE
        p_where_clause STRING,
        sql_text STRING,
        cust_cnt SMALLINT

    LET sql_text = "SELECT COUNT(*)
 FROM tanent  WHERE " || p_where_clause CLIPPED
  
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

FUNCTION tanent_cust_select(p_where_clause) 
    DEFINE
        p_where_clause STRING,
        sql_text STRING,
        fetch_ok SMALLINT

    LET sql_text = "SELECT * FROM tanent WHERE " || p_where_clause CLIPPED 

    DECLARE cust_curs SCROLL CURSOR WITH HOLD FROM sql_text

    OPEN cust_curs
    CALL tanent_fetch_cust(1)
        RETURNING fetch_ok 
    IF NOT (fetch_ok) THEN
        MESSAGE "NO ROWS IN THE TABLE" 
    END IF

    RETURN fetch_ok   


END FUNCTION

FUNCTION tanent_fetch_cust(p_fetch_flag)
    DEFINE
        p_fetch_flag SMALLINT,
        fetch_ok SMALLINT

    LET fetch_ok = TRUE
    WHENEVER ERROR CONTINUE
    IF p_fetch_flag = 1 THEN
        FETCH NEXT cust_curs INTO tanent_rec.*
    ELSE
        FETCH PREVIOUS cust_curs INTO tanent_rec.*
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

FUNCTION tanent_fetch_rel(p_fetch_flag)
    DEFINE
        p_fetch_flag SMALLINT,
        fetch_ok SMALLINT

    MESSAGE " "
    CALL tanent_fetch_cust(p_fetch_flag) RETURNING fetch_ok

    IF (fetch_ok) THEN
        CALL tanent_display()
    ELSE
        IF (p_fetch_flag = 1) THEN
            MESSAGE "END OF LIST"
        ELSE
            MESSAGE "BEGINING OF LIST"
        END IF
    END IF

END FUNCTION

FUNCTION tanent_display()
    DISPLAY BY NAME tanent_rec.*              
END FUNCTION

FUNCTION tanent_input_clg(au_flag)
      DEFINE au_flag CHAR(20),
      count_ok INTEGER 

      LET count_ok = TRUE
      IF(au_flag = "A") THEN
              MESSAGE "ENTER TANENT DETAILS"

              INITIALIZE tanent_rec.* TO NULL
      END IF 

     LET int_flag = FALSE 

     INPUT BY NAME tanent_rec.* WITHOUT DEFAULTS ATTRIBUTES (UNBUFFERED)
     ON CHANGE tanen_name
    IF au_flag="A" THEN
    SELECT tanen_name,unit_number,address,join_on,size_sqft
    INTO tanent_rec.*
    FROM tanent
    WHERE tanen_name=tanent_rec.tanen_name
    IF(sqlca.sqlcode=0)THEN
    ERROR " ALREADY IN DATABASE"
    LET count_ok =FALSE
    EXIT INPUT
    END IF 
    END IF

    AFTER FIELD tanen_name
    IF tanent_rec.tanen_name IS NULL THEN
    ERROR "ENTER TANENT NAME"
    NEXT FIELD tanen_name
    END IF
    IF NOT tanent_rec.tanen_name MATCHES "[a-z A-Z]*"  THEN
    MESSAGE "NAME SHOULD  BE CHARACTEr"
    NEXT FIELD tanen_name
    END IF

     AFTER FIELD unit_number
    IF tanent_rec.unit_number IS NULL THEN
    MESSAGE "ENTER UNIT NUMBER "
    NEXT FIELD unit_number
    END IF


    IF  tanent_rec.unit_number <= 0 then
    ERROR "UNIT NUMBER SHOULD BE GREATER THAN ZERO"
    NEXT FIELD unit_number
    END IF
    
    AFTER FIELD address
    IF tanent_rec.address IS NULL THEN
    MESSAGE "ENTER  ADDRESS"
    NEXT FIELD address
    END IF
   

    AFTER FIELD join_on
    IF tanent_rec.join_on IS NULL THEN
    MESSAGE "ENTER JOIN DATE"
    NEXT FIELD join_on
    END IF

     AFTER FIELD size_sqft
    IF tanent_rec.size_sqft IS NULL THEN
    MESSAGE "ENTER SIZE IN SQFT"
    NEXT FIELD size_sqft
    END IF


NEXT FIELD tanen_name
            END INPUT 

            IF (int_flag) THEN 
                LET int_flag = FALSE
                LET count_ok = FALSE 
                MESSAGE "OPERATION CANCELLED BY USER"
            END IF 

            RETURN count_ok

 END FUNCTION 

FUNCTION tanent_insert()
    INPUT BY NAME tanent_rec.* WITHOUT DEFAULTS ATTRIBUTE(UNBUFFERED)

    WHENEVER ERROR CONTINUE
    INSERT INTO tanent VALUES(tanent_rec.*)
    WHENEVER ERROR STOP

    IF SQLCA.SQLCODE = 0 THEN
        MESSAGE "ROW ADDED"
    ELSE
        ERROR SQLERRMESSAGE
    END IF

END FUNCTION

FUNCTION tanent_update()
   

    WHENEVER ERROR CONTINUE
    SELECT  INTO tanent_rec FROM tanent WHERE tanen_name = tanent_rec.tanen_name 
    INPUT BY NAME tanent_rec.* WITHOUT DEFAULTS ATTRIBUTE(UNBUFFERED)
    WHENEVER ERROR STOP

    WHENEVER ERROR CONTINUE
    UPDATE owner
        SET  
              unit_number = tanent_rec.unit_number,
              address = tanent_rec.address,
              join_on =tanent_rec.join_on,
              size_sqft =tanent_rec.size_sqft

                WHERE tanen_name = tanent_rec.tanen_name
    WHENEVER ERROR STOP

    IF SQLCA.SQLCODE = 0 THEN
        MESSAGE "ROW UPDATED"
    ELSE
        ERROR SQLERRMESSAGE
    END IF

END FUNCTION
 
FUNCTION tanent_delete()
  DEFINE del_ok SMALLINT
  
    WHENEVER ERROR CONTINUE
    DELETE FROM tanent WHERE tanen_name = tanent_rec.tanen_name
    WHENEVER ERROR STOP
    IF SQLCA.SQLCODE = 0 THEN
       MESSAGE "ROW DELETED"
       INITIALIZE tanent_rec.* TO NULL
       DISPLAY BY NAME tanent_rec.* 
    ELSE
      ERROR SQLERRMESSAGE
    END IF 
 
 END FUNCTION   

FUNCTION tanent_delete_check()
  DEFINE del_ok SMALLINT,
         del_count SMALLINT
  
  LET del_ok = FALSE
    
  WHENEVER ERROR CONTINUE
  SELECT COUNT(*)
 INTO del_count FROM tanent
    WHERE owner.tanen_name = tanent_rec.tanen_name
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

FUNCTION tanent_clean_up()

    WHENEVER ERROR CONTINUE
    CLOSE cust_curs
    FREE cust_curs
    WHENEVER ERROR STOP

END FUNCTION
FUNCTION tanent_report()

    DEFINE i2genrec RECORD
                      LIKE tanent.*
       

      IF fgl_report_LoadCurrentSettings("tanenreport.4rp") THEN
        CALL fgl_report_SelectDevice("PDF")
        CALL fgl_report_SelectPreview(TRUE)
        LET hndl = fgl_report_commitCurrentSettings()

    ELSE
        EXIT PROGRAM
    END IF

    START REPORT treport TO XML HANDLER hndl

    DECLARE i2curr1 CURSOR FOR
        SELECT *
            FROM tanent
        

    FOREACH i2curr1 INTO i2genrec.*
        OUTPUT TO REPORT treport(i2genrec.*)

    END FOREACH

    FINISH REPORT treport

END FUNCTION

REPORT treport(printi2genrec)

    DEFINE printi2genrec RECORD
         LIKE tanent.*
       

    FORMAT

--FIRST PAGE HEADER
--PAGE HEADER
--PRINTX createdate
        ON EVERY ROW
--DISPLAY "printi2genrec", printi2genrec.id

            PRINTX printi2genrec.tanen_name
            PRINTX printi2genrec.unit_number
            PRINTX printi2genrec.address
            PRINTX printi2genrec.join_on
            PRINTX printi2genrec.size_sqft
            --PRINTX printi2genrec.timing
            --PRINTX printi2genrec.unit_number
            --printi2genrec.phn_number
            --LET createdate = DATE(CURRENT)
            --LET lin_x = "----------------------------------------------------------------------------------------------------------------------------------"
            --PRINTX lin_x
            --PRINTX createdate

--AFTER GROUP OF printi2genrec.id
-- PRINTX printi2genrec.id
            --SKIP 2 LINES

END REPORT

 
 
   
 
  