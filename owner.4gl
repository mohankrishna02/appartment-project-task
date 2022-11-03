DATABASE hrmdb
DEFINE owner_rec RECORD LIKE owner.*
DEFINE query_ok SMALLINT
DEFINE match STRING
DEFINE i INT 
DEFINE hndl om.SaxDocumentHandler


FUNCTION owner_fun()
   
        OPEN WINDOW OWNER WITH FORM "ownerper"
        CALL fun_owner()
        
CLOSE WINDOW OWNER
END FUNCTION

FUNCTION fun_owner()
DEFINE query_ok smallint

    MENU 
        COMMAND "find"
        CLEAR FORM 
            CALL owner_query() RETURNING query_ok
        COMMAND "next"
            IF (query_ok) THEN
                CALL owner_fetch_rel(1)
            ELSE
                MESSAGE "YOU MUST QUERY FIRST." 

            END IF
        COMMAND "previous"
            IF (query_ok) THEN
                CALL owner_fetch_rel(-1)
            ELSE
                MESSAGE "YOU MUST QUERY FIRST."
            END IF

        ON ACTION ADD 
       IF(owner_input_clg("A")) then
            CALL owner_insert()
            END IF

        COMMAND "Modify"
            CALL owner_update()

        COMMAND "Delete"
        IF (owner_delete_check()) THEN 
            CALL owner_delete()
            END IF 
            COMMAND "clear"
            CLEAR FORM 
          COMMAND "quit"
            EXIT MENU
            COMMAND "print"
            CALL owner_report()
    END MENU

END FUNCTION


FUNCTION owner_query()
    DEFINE
        cont_ok SMALLINT,
        cust_cnt SMALLINT,
        where_clause STRING

    MESSAGE "ENTER SEARCH CRITERIA"
    LET cont_ok = FALSE                 
    LET int_flag = FALSE                          

    CONSTRUCT BY NAME where_clause ON owner.*
                        
    
    IF (int_flag) = TRUE THEN
        LET int_flag = FALSE                       
        CLEAR FORM
        LET cont_ok = FALSE
        MESSAGE "CANCELED BY USER"
    ELSE
        CALL owner_get_cust_cnt(where_clause) RETURNING cust_cnt
        IF (cust_cnt > 0) THEN
            MESSAGE cust_cnt USING "<<<<", " ROWS FOUND"
            CALL owner_cust_select(where_clause) RETURNING cont_ok   
        ELSE
            MESSAGE "NO ROWS FOUND"
            LET cont_ok = FALSE
        END IF
    END IF

    IF (cont_ok) THEN
        CALL owner_display()
    END IF

    RETURN cont_ok

END FUNCTION

FUNCTION owner_get_cust_cnt(p_where_clause)     
    DEFINE
        p_where_clause STRING,
        sql_text STRING,
        cust_cnt SMALLINT

    LET sql_text = "SELECT COUNT(*)
 FROM owner  WHERE " || p_where_clause CLIPPED
  
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

FUNCTION owner_cust_select(p_where_clause) 
    DEFINE
        p_where_clause STRING,
        sql_text STRING,
        fetch_ok SMALLINT

    LET sql_text = "SELECT * FROM owner WHERE " || p_where_clause CLIPPED 

    DECLARE cust_curs SCROLL CURSOR WITH HOLD FROM sql_text

    OPEN cust_curs
    CALL owner_fetch_cust(1)
        RETURNING fetch_ok 
    IF NOT (fetch_ok) THEN
        MESSAGE "NO ROWS IN THE TABLE" 
    END IF

    RETURN fetch_ok   


END FUNCTION

FUNCTION owner_fetch_cust(p_fetch_flag)
    DEFINE
        p_fetch_flag SMALLINT,
        fetch_ok SMALLINT

    LET fetch_ok = TRUE
    WHENEVER ERROR CONTINUE
    IF p_fetch_flag = 1 THEN
        FETCH NEXT cust_curs INTO owner_rec.*
    ELSE
        FETCH PREVIOUS cust_curs INTO owner_rec.*
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

FUNCTION owner_fetch_rel(p_fetch_flag)
    DEFINE
        p_fetch_flag SMALLINT,
        fetch_ok SMALLINT

    MESSAGE " "
    CALL owner_fetch_cust(p_fetch_flag) RETURNING fetch_ok

    IF (fetch_ok) THEN
        CALL owner_display()
    ELSE
        IF (p_fetch_flag = 1) THEN
            MESSAGE "END OF LIST"
        ELSE
            MESSAGE "BEGINING OF LIST"
        END IF
    END IF

END FUNCTION

FUNCTION owner_display()
    DISPLAY BY NAME owner_rec.*              
END FUNCTION

FUNCTION owner_input_clg(au_flag)
      DEFINE au_flag CHAR(20),
      count_ok INTEGER 

      LET count_ok = TRUE
      IF(au_flag = "A") THEN
              MESSAGE "ENTER OWNER DETAILS"

              INITIALIZE owner_rec.* TO NULL
      END IF 

     LET int_flag = FALSE 

     INPUT BY NAME owner_rec.* WITHOUT DEFAULTS ATTRIBUTES (UNBUFFERED)
     ON CHANGE owner_name
    IF au_flag="A" THEN
    SELECT owner_name,unit_number,address,purchased_on,size_sqft
    INTO owner_rec.*
    FROM owner
    WHERE owner_name=owner_rec.owner_name
    IF(sqlca.sqlcode=0)THEN
    ERROR " ALREADY IN DATABASE"
    LET count_ok =FALSE
    EXIT INPUT
    END IF 
    END IF

    AFTER FIELD owner_name
    IF owner_rec.owner_name IS NULL THEN
    ERROR "ENTER OWNER NAME FIELD FIELD"
    NEXT FIELD owner_name
    END IF
    IF NOT owner_rec.owner_name MATCHES "[a-z A-Z]*"  THEN
    MESSAGE "NAME SHOULD  BE CHARACTEr"
    NEXT FIELD owner_name
    END IF

     AFTER FIELD unit_number
    IF owner_rec.unit_number IS NULL THEN
    MESSAGE "ENTER UNIT NUMBER "
    NEXT FIELD unit_number
    END IF


    IF  owner_rec.unit_number <= 0 then
    ERROR "UNIT NUMBER SHOULD BE GREATER THAN ZERO"
    NEXT FIELD unit_number
    END IF
    
    AFTER FIELD address
    IF owner_rec.address IS NULL THEN
    MESSAGE "ENTER  ADDRESS"
    NEXT FIELD address
    END IF
   

    AFTER FIELD purchased_on
    IF owner_rec.purchased_on IS NULL THEN
    MESSAGE "ENTER PURCHASED DATE"
    NEXT FIELD purchased_on
    END IF

     AFTER FIELD size_sqft
    IF owner_rec.size_sqft IS NULL THEN
    MESSAGE "ENTER SIZE IN SQFT"
    NEXT FIELD size_sqft
    END IF


NEXT FIELD owner_name
            END INPUT 

            IF (int_flag) THEN 
                LET int_flag = FALSE
                LET count_ok = FALSE 
                MESSAGE "OPERATION CANCELLED BY USER"
            END IF 

            RETURN count_ok

 END FUNCTION 

FUNCTION owner_insert()
    INPUT BY NAME owner_rec.* WITHOUT DEFAULTS ATTRIBUTE(UNBUFFERED)

    WHENEVER ERROR CONTINUE
    INSERT INTO owner VALUES(owner_rec.*)
    WHENEVER ERROR STOP

    IF SQLCA.SQLCODE = 0 THEN
        MESSAGE "ROW ADDED"
    ELSE
        ERROR SQLERRMESSAGE
    END IF

END FUNCTION

FUNCTION owner_update()
   

    WHENEVER ERROR CONTINUE
    SELECT  INTO owner_rec FROM owner WHERE owner_name = owner_rec.owner_name 
    INPUT BY NAME owner_rec.* WITHOUT DEFAULTS ATTRIBUTE(UNBUFFERED)
    WHENEVER ERROR STOP

    WHENEVER ERROR CONTINUE
    UPDATE owner
        SET  
              unit_number = owner_rec.unit_number,
              address = owner_rec.address,
              purchased_on =owner_rec.purchased_on,
              size_sqft =owner_rec.size_sqft

                WHERE owner_name = owner_rec.owner_name
    WHENEVER ERROR STOP

    IF SQLCA.SQLCODE = 0 THEN
        MESSAGE "ROW UPDATED"
    ELSE
        ERROR SQLERRMESSAGE
    END IF

END FUNCTION
 
FUNCTION owner_delete()
  DEFINE del_ok SMALLINT
  
    WHENEVER ERROR CONTINUE
    DELETE FROM owner WHERE owner_name = owner_rec.owner_name
    WHENEVER ERROR STOP
    IF SQLCA.SQLCODE = 0 THEN
       MESSAGE "ROW DELETED"
       INITIALIZE owner_rec.* TO NULL
       DISPLAY BY NAME owner_rec.* 
    ELSE
      ERROR SQLERRMESSAGE
    END IF 
 
 END FUNCTION   

FUNCTION owner_delete_check()
  DEFINE del_ok SMALLINT,
         del_count SMALLINT
  
  LET del_ok = FALSE
    
  WHENEVER ERROR CONTINUE
  SELECT COUNT(*)
 INTO del_count FROM owner
    WHERE owner.owner_name = owner_rec.owner_name
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

FUNCTION owner_clean_up()

    WHENEVER ERROR CONTINUE
    CLOSE cust_curs
    FREE cust_curs
    WHENEVER ERROR STOP

END FUNCTION
FUNCTION owner_report()

    DEFINE i2genrec RECORD
                      LIKE owner.*
       

      IF fgl_report_LoadCurrentSettings("ownerreport.4rp") THEN
        CALL fgl_report_SelectDevice("PDF")
        CALL fgl_report_SelectPreview(TRUE)
        LET hndl = fgl_report_commitCurrentSettings()

    ELSE
        EXIT PROGRAM
    END IF

    START REPORT owreport TO XML HANDLER hndl

    DECLARE i2curr1 CURSOR FOR
        SELECT *
            FROM owner
        

    FOREACH i2curr1 INTO i2genrec.*
        OUTPUT TO REPORT owreport(i2genrec.*)

    END FOREACH

    FINISH REPORT owreport

END FUNCTION

REPORT owreport(printi2genrec)

    DEFINE printi2genrec RECORD
         LIKE owner.*
       

    FORMAT

--FIRST PAGE HEADER
--PAGE HEADER
--PRINTX createdate
        ON EVERY ROW
--DISPLAY "printi2genrec", printi2genrec.id

            PRINTX printi2genrec.owner_name
            PRINTX printi2genrec.unit_number
            PRINTX printi2genrec.address
            PRINTX printi2genrec.purchased_on
            PRINTX printi2genrec.size_sqft
            --PRINTX printi2genrec.unit_number
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
 
 
   
 
  