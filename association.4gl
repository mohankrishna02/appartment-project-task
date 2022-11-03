DATABASE hrmdb
DEFINE association_rec RECORD LIKE Association.*
DEFINE query_ok SMALLINT
DEFINE match STRING
DEFINE i INT 
DEFINE hndl om.SaxDocumentHandler


FUNCTION association_fun()
   
        OPEN WINDOW ASSOCIATION WITH FORM "associationper"
        CALL fun_association()
        
CLOSE WINDOW ASSOCIATION
END FUNCTION

FUNCTION fun_association()
DEFINE query_ok smallint

    MENU 
        COMMAND "find"
        CLEAR FORM 
            CALL association_query() RETURNING query_ok
        COMMAND "next"
            IF (query_ok) THEN
                CALL association_fetch_rel(1)
            ELSE
                MESSAGE "YOU MUST QUERY FIRST." 

            END IF
        COMMAND "previous"
            IF (query_ok) THEN
                CALL association_fetch_rel(-1)
            ELSE
                MESSAGE "YOU MUST QUERY FIRST."
            END IF

        ON ACTION ADD 
       IF(association_input_clg("A")) then
            CALL association_insert()
            END IF

        COMMAND "Modify"
            CALL association_update()

        COMMAND "Delete"
        IF (association_delete_check()) THEN 
            CALL association_delete()
            END IF 
            COMMAND "clear"
            CLEAR FORM 
          COMMAND "quit"
            EXIT MENU
            COMMAND "print"
            CALL association_report()
    END MENU

END FUNCTION


FUNCTION association_query()
    DEFINE
        cont_ok SMALLINT,
        cust_cnt SMALLINT,
        where_clause STRING

    MESSAGE "ENTER SEARCH CRITERIA"
    LET cont_ok = FALSE                 
    LET int_flag = FALSE                          

    CONSTRUCT BY NAME where_clause ON Association.*
                        
    
    IF (int_flag) = TRUE THEN
        LET int_flag = FALSE                       
        CLEAR FORM
        LET cont_ok = FALSE
        MESSAGE "CANCELED BY USER"
    ELSE
        CALL association_get_cust_cnt(where_clause) RETURNING cust_cnt
        IF (cust_cnt > 0) THEN
            MESSAGE cust_cnt USING "<<<<", " ROWS FOUND"
            CALL association_cut_select(where_clause) RETURNING cont_ok   
        ELSE
            MESSAGE "NO ROWS FOUND"
            LET cont_ok = FALSE
        END IF
    END IF

    IF (cont_ok) THEN
        CALL association_display()
    END IF

    RETURN cont_ok

END FUNCTION

FUNCTION association_get_cust_cnt(p_where_clause)     
    DEFINE
        p_where_clause STRING,
        sql_text STRING,
        cust_cnt SMALLINT

    LET sql_text = "SELECT COUNT(*)
 FROM Association  WHERE " || p_where_clause CLIPPED
  
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

FUNCTION association_cut_select(p_where_clause) 
    DEFINE
        p_where_clause STRING,
        sql_text STRING,
        fetch_ok SMALLINT

    LET sql_text = "SELECT * FROM Association WHERE " || p_where_clause CLIPPED 

    DECLARE cust_curs SCROLL CURSOR WITH HOLD FROM sql_text

    OPEN cust_curs
    CALL association_fetch_cust(1)
        RETURNING fetch_ok 
    IF NOT (fetch_ok) THEN
        MESSAGE "NO ROWS IN THE TABLE" 
    END IF

    RETURN fetch_ok   


END FUNCTION

FUNCTION association_fetch_cust(p_fetch_flag)
    DEFINE
        p_fetch_flag SMALLINT,
        fetch_ok SMALLINT

    LET fetch_ok = TRUE
    WHENEVER ERROR CONTINUE
    IF p_fetch_flag = 1 THEN
        FETCH NEXT cust_curs INTO association_rec.*
    ELSE
        FETCH PREVIOUS cust_curs INTO association_rec.*
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

FUNCTION association_fetch_rel(p_fetch_flag)
    DEFINE
        p_fetch_flag SMALLINT,
        fetch_ok SMALLINT

    MESSAGE " "
    CALL association_fetch_cust(p_fetch_flag) RETURNING fetch_ok

    IF (fetch_ok) THEN
        CALL association_display()
    ELSE
        IF (p_fetch_flag = 1) THEN
            MESSAGE "END OF LIST"
        ELSE
            MESSAGE "BEGINING OF LIST"
        END IF
    END IF

END FUNCTION

FUNCTION association_display()
    DISPLAY BY NAME association_rec.*              
END FUNCTION

FUNCTION association_input_clg(au_flag)
      DEFINE au_flag CHAR(20),
      count_ok INTEGER 

      LET count_ok = TRUE
      IF(au_flag = "A") THEN
              MESSAGE "ENTER ASSOCIATION DETAILS"

              INITIALIZE association_rec.* TO NULL
      END IF 

     LET int_flag = FALSE 

     INPUT BY NAME association_rec.* WITHOUT DEFAULTS ATTRIBUTES (UNBUFFERED)
     ON CHANGE Association_name
    IF au_flag="A" THEN
    SELECT Association_name,Position,Responsbility,Unit_number
    INTO association_rec.*
    FROM Association
    WHERE Association_name=association_rec.Association_name
    IF(sqlca.sqlcode=0)THEN
    ERROR "ASSOCIATION NAME ALREADY IN DATABASE"
    LET count_ok =FALSE
    EXIT INPUT
    END IF 
    END IF

    AFTER FIELD Association_name
    IF association_rec.Association_name IS NULL THEN
    ERROR "ENTER ASSOCIATION NAME"
    NEXT FIELD Association_name
    END IF
    IF NOT association_rec.Association_name MATCHES "[a-z A-Z]*"  THEN
    MESSAGE "NAME SHOULD  BE CHARACTER"
    NEXT FIELD Association_name
    END IF
    AFTER FIELD Position
    IF association_rec.position IS NULL THEN
    MESSAGE "ENTER POSITION FIELD"
    NEXT FIELD Position
    END IF
    IF NOT association_rec.position MATCHES "[a-z A-Z]*"  THEN
    MESSAGE "POSITION SHOULD  BE CHARACTER"
    NEXT FIELD Position
    END IF
    LET match =association_rec.position
    FOR i=1 TO LENGTH(match)
    IF match.getCharAt(i) MATCHES "[0-9]" THEN
    ERROR "POSITION SHOULD BE CHARACTER"
    NEXT FIELD Position
    END IF
    END FOR

    AFTER FIELD Responsbility
    IF association_rec.responsbility IS NULL THEN
    MESSAGE "ENTER RESPONSBILITY"
    NEXT FIELD Responsbility
    END IF


    AFTER FIELD unit_number
    IF association_rec.unit_number IS NULL THEN
    MESSAGE "ENTER UNIT NUMBER "
    NEXT FIELD unit_number
    END IF


    IF  association_rec.unit_number <= 0 then
    ERROR "UNIT NUMBER SHOULD BE GREATER THAN ZERO"
    NEXT FIELD unit_number
    END IF


NEXT FIELD Association_name

            END INPUT 

            IF (int_flag) THEN 
                LET int_flag = FALSE
                LET count_ok = FALSE 
                MESSAGE "OPERATION CANCELLED BY USER"
            END IF 

            RETURN count_ok

 END FUNCTION 

FUNCTION association_insert()
    INPUT BY NAME association_rec.* WITHOUT DEFAULTS ATTRIBUTE(UNBUFFERED)

    WHENEVER ERROR CONTINUE
    INSERT INTO Association VALUES(association_rec.*)
    WHENEVER ERROR STOP

    IF SQLCA.SQLCODE = 0 THEN
        MESSAGE "ROW ADDED"
    ELSE
        ERROR SQLERRMESSAGE
    END IF

END FUNCTION

FUNCTION association_update()
   

    WHENEVER ERROR CONTINUE
    SELECT  INTO association_rec FROM Association WHERE Association_name = association_rec.Association_name 
    INPUT BY NAME association_rec.* WITHOUT DEFAULTS ATTRIBUTE(UNBUFFERED)
    WHENEVER ERROR STOP

    WHENEVER ERROR CONTINUE
    UPDATE Association
        SET  
              Position = association_rec.position,
              Responsbility = association_rec.responsbility,
              unit_number =association_rec.unit_number

                WHERE Association_name = association_rec.Association_name
    WHENEVER ERROR STOP

    IF SQLCA.SQLCODE = 0 THEN
        MESSAGE "ROW UPDATED"
    ELSE
        ERROR SQLERRMESSAGE
    END IF

END FUNCTION
 
FUNCTION association_delete()
  DEFINE del_ok SMALLINT
  
    WHENEVER ERROR CONTINUE
    DELETE FROM Association WHERE Association_name = association_rec.Association_name
    WHENEVER ERROR STOP
    IF SQLCA.SQLCODE = 0 THEN
       MESSAGE "ROW DELETED"
       INITIALIZE association_rec.* TO NULL
       DISPLAY BY NAME association_rec.* 
    ELSE
      ERROR SQLERRMESSAGE
    END IF 
 
 END FUNCTION   

FUNCTION association_delete_check()
  DEFINE del_ok SMALLINT,
         del_count SMALLINT
  
  LET del_ok = FALSE
    
  WHENEVER ERROR CONTINUE
  SELECT COUNT(*)
 INTO del_count FROM Association
    WHERE Association.Association_name = association_rec.Association_name
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

FUNCTION association_cleanup_fun()

    WHENEVER ERROR CONTINUE
    CLOSE cust_curs
    FREE cust_curs
    WHENEVER ERROR STOP

END FUNCTION

FUNCTION association_report()

    DEFINE i2genrec RECORD
                      LIKE association.*
       

      IF fgl_report_LoadCurrentSettings("assreport.4rp") THEN
        CALL fgl_report_SelectDevice("PDF")
        CALL fgl_report_SelectPreview(TRUE)
        LET hndl = fgl_report_commitCurrentSettings()

    ELSE
        EXIT PROGRAM
    END IF

    START REPORT asreport TO XML HANDLER hndl

    DECLARE i2curr1 CURSOR FOR
        SELECT *
            FROM Association
        

    FOREACH i2curr1 INTO i2genrec.*
        OUTPUT TO REPORT asreport(i2genrec.*)

    END FOREACH

    FINISH REPORT asreport

END FUNCTION

REPORT asreport(printi2genrec)

    DEFINE printi2genrec RECORD
         LIKE Association.*
       

    FORMAT

--FIRST PAGE HEADER
--PAGE HEADER
--PRINTX createdate
        ON EVERY ROW
--DISPLAY "printi2genrec", printi2genrec.id

            PRINTX printi2genrec.association_name
            PRINTX printi2genrec.position
            PRINTX printi2genrec.responsbility
            PRINTX printi2genrec.unit_number
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


 
 
   
 
  