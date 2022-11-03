DATABASE hrmdb
DEFINE amenity_rec RECORD LIKE amenity.*
DEFINE query_ok SMALLINT
DEFINE match STRING
DEFINE i INT 
DEFINE hndl om.SaxDocumentHandler


FUNCTION amenity_fun()
   
        OPEN WINDOW amenity WITH FORM "amenityper"
        CALL fun_amenity()
        
CLOSE WINDOW amenity
END FUNCTION

FUNCTION fun_amenity()
DEFINE query_ok smallint

    MENU 
        COMMAND "find"
        CLEAR FORM 
            CALL query_cust() RETURNING query_ok
        COMMAND "next"
            IF (query_ok) THEN
                CALL fetch_rel_cust(1)
            ELSE
                MESSAGE "YOU MUST QUERY FIRST." 

            END IF
        COMMAND "previous"
            IF (query_ok) THEN
                CALL fetch_rel_cust(-1)
            ELSE
                MESSAGE "YOU MUST QUERY FIRST."
            END IF

        ON ACTION ADD 
       IF(fun_input_clg1("A")) then
            CALL insert_cust()
            END IF

        COMMAND "Modify"
            CALL update_cust()

        COMMAND "Delete"
        IF (delete_check()) THEN 
            CALL delete_value()
            END IF 
            COMMAND "clear"
            CLEAR FORM 
          COMMAND "quit"
            EXIT MENU
            COMMAND "print"
            CALL amenity_report()
    END MENU

END FUNCTION


FUNCTION query_cust()
    DEFINE
        cont_ok SMALLINT,
        cust_cnt SMALLINT,
        where_clause STRING

    MESSAGE "Enter search criteria"
    LET cont_ok = FALSE                 
    LET int_flag = FALSE                          

    CONSTRUCT BY NAME where_clause ON amenity.*
                        
    
    IF (int_flag) = TRUE THEN
        LET int_flag = FALSE                       
        CLEAR FORM
        LET cont_ok = FALSE
        MESSAGE "Canceled by user."
    ELSE
        CALL get_cust_cntm(where_clause) RETURNING cust_cnt
        IF (cust_cnt > 0) THEN
            MESSAGE cust_cnt USING "<<<<", " rows found."
            CALL cust1_select(where_clause) RETURNING cont_ok   
        ELSE
            MESSAGE "No rows found."
            LET cont_ok = FALSE
        END IF
    END IF

    IF (cont_ok) THEN
        CALL display_custm()
    END IF

    RETURN cont_ok

END FUNCTION

FUNCTION get_cust_cntm(p_where_clause)     
    DEFINE
        p_where_clause STRING,
        sql_text STRING,
        cust_cnt SMALLINT

    LET sql_text = "SELECT COUNT(*)
 FROM amenity  WHERE " || p_where_clause CLIPPED
  
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

FUNCTION cust1_select(p_where_clause) 
    DEFINE
        p_where_clause STRING,
        sql_text STRING,
        fetch_ok SMALLINT

    LET sql_text = "SELECT * FROM amenity WHERE " || p_where_clause CLIPPED 

    DECLARE cust_curs SCROLL CURSOR WITH HOLD FROM sql_text

    OPEN cust_curs
    CALL fetch_custm(1)
        RETURNING fetch_ok 
    IF NOT (fetch_ok) THEN
        MESSAGE "no rows in table." 
    END IF

    RETURN fetch_ok   


END FUNCTION

FUNCTION fetch_custm(p_fetch_flag)
    DEFINE
        p_fetch_flag SMALLINT,
        fetch_ok SMALLINT

    LET fetch_ok = TRUE
    WHENEVER ERROR CONTINUE
    IF p_fetch_flag = 1 THEN
        FETCH NEXT cust_curs INTO amenity_rec.*
    ELSE
        FETCH PREVIOUS cust_curs INTO amenity_rec.*
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

FUNCTION fetch_rel_cust(p_fetch_flag)
    DEFINE
        p_fetch_flag SMALLINT,
        fetch_ok SMALLINT

    MESSAGE " "
    CALL fetch_custm(p_fetch_flag) RETURNING fetch_ok

    IF (fetch_ok) THEN
        CALL display_custm()
    ELSE
        IF (p_fetch_flag = 1) THEN
            MESSAGE "End of list"
        ELSE
            MESSAGE "Beginning of list"
        END IF
    END IF

END FUNCTION

FUNCTION display_custm()
    DISPLAY BY NAME amenity_rec.*              
END FUNCTION

FUNCTION fun_input_clg1(au_flag)
      DEFINE au_flag CHAR(20),
      count_ok INTEGER 

      LET count_ok = TRUE
      IF(au_flag = "A") THEN
              MESSAGE "ENTER AMENITY DETAILS"

              INITIALIZE amenity_rec.* TO NULL
      END IF 

     LET int_flag = FALSE 

     INPUT BY NAME amenity_rec.* WITHOUT DEFAULTS ATTRIBUTES (UNBUFFERED)
     ON CHANGE amenity_name
    IF au_flag="A" THEN
    SELECT amenity_name,booked_by,charges,on_date,owner_type,timing,unit_number
    INTO amenity_rec.*
    FROM amenity
    WHERE amenity_name=amenity_rec.amenity_name
    IF(sqlca.sqlcode=0)THEN
    ERROR "AMENITY IS ALREADY IN DATABASE"
    LET count_ok =FALSE
    EXIT INPUT
    END IF 
    END IF

    AFTER FIELD amenity_name
    IF amenity_rec.amenity_name IS NULL THEN
    ERROR "ENTER AMENITY NAME"
    NEXT FIELD amenity_name
    END IF
    IF NOT amenity_rec.booked_by MATCHES "[a-z A-Z]*"  THEN
    MESSAGE "NAME SHOULD BE CHARACTER"
    NEXT FIELD amenity_name
    END IF
    LET match =amenity_rec.amenity_name
    FOR i=1 TO LENGTH(match)
    IF match.getCharAt(i) MATCHES "[0-9]" THEN
    ERROR "NAME SHOULD BE CHARACTER ONLY"
    NEXT FIELD amenity_name
    END IF
    END FOR

    AFTER FIELD booked_by
    IF amenity_rec.booked_by IS NULL THEN
    MESSAGE "ENTER BOOKED BY FIELD"
    NEXT FIELD booked_by
    END IF
    IF NOT amenity_rec.booked_by MATCHES "[a-z A-Z]*"  THEN
    MESSAGE "NAME SHOULD BE CHARACTER"
    NEXT FIELD booked_by
    END IF
    LET match =amenity_rec.booked_by
    FOR i=1 TO LENGTH(match)
    IF match.getCharAt(i) MATCHES "[0-9]" THEN
    ERROR "NAME SHOULD BE CHARACTER ONLY"
    NEXT FIELD booked_by
    END IF
    END FOR

    AFTER FIELD charges
    IF amenity_rec.charges IS NULL THEN
    MESSAGE "ENTER CHARGES FIELD"
    NEXT FIELD charges
    END IF

    IF  amenity_rec.charges <= 0 then
    ERROR "VALUE SHOULD BE GRATER THAN ZERO"
    NEXT FIELD charges
    END IF

    AFTER FIELD on_date
    IF amenity_rec.on_date IS NULL THEN
    MESSAGE "ENTER ON DATE FIELD"
    NEXT FIELD on_date
    END IF

    AFTER FIELD owner_type
    IF amenity_rec.owner_type IS NULL THEN
    MESSAGE "ENTER OWNER_TYPE"
    NEXT FIELD owner_type
    END IF
    IF NOT amenity_rec.owner_type MATCHES "[a-z A-Z]*"  THEN
    MESSAGE "OWNER_TYPE SHOULD BE CHARACTER"
    NEXT FIELD owner_type
    END IF
    LET match =amenity_rec.owner_type
    FOR i=1 TO LENGTH(match)
    IF match.getCharAt(i) MATCHES "[0-9]" THEN
    ERROR "OWNER_TYPE SHOULD BE CHARACTER ONLY"
    NEXT FIELD owner_type
    END IF
    END FOR

    AFTER FIELD timing
    IF amenity_rec.timing IS NULL THEN
    MESSAGE "ENTER TIMING"
    NEXT FIELD timing
    END IF

    AFTER FIELD unit_number
    IF amenity_rec.unit_number IS NULL THEN
    MESSAGE "ENTER UNIT NUMBER "
    NEXT FIELD unit_number
    END IF


    IF  amenity_rec.unit_number <= 0 then
    ERROR "UNIT NUMBER SHOULD BE GREATER THAN ZERO"
    NEXT FIELD unit_number
    END IF


NEXT FIELD amenity_name

            END INPUT 

            IF (int_flag) THEN 
                LET int_flag = FALSE
                LET count_ok = FALSE 
                MESSAGE "OPERATION CANCELLED BY USER"
            END IF 

            RETURN count_ok

 END FUNCTION 

FUNCTION insert_cust()
    INPUT BY NAME amenity_rec.* WITHOUT DEFAULTS ATTRIBUTE(UNBUFFERED)

    WHENEVER ERROR CONTINUE
    INSERT INTO amenity VALUES(amenity_rec.*)
    WHENEVER ERROR STOP

    IF SQLCA.SQLCODE = 0 THEN
        MESSAGE "ROW ADDED"
    ELSE
        ERROR SQLERRMESSAGE
    END IF

END FUNCTION

FUNCTION update_cust()
   

    WHENEVER ERROR CONTINUE
    SELECT  INTO amenity_rec FROM amenity WHERE amenity_name = amenity_rec.amenity_name 
    INPUT BY NAME amenity_rec.* WITHOUT DEFAULTS ATTRIBUTE(UNBUFFERED)
    WHENEVER ERROR STOP

    WHENEVER ERROR CONTINUE
    UPDATE amenity
        SET  
              booked_by = amenity_rec.booked_by,
              charges = amenity_rec.charges,
            on_date = amenity_rec.on_date,
              owner_type = amenity_rec.owner_type,
              unit_number =amenity_rec.unit_number

                WHERE amenity_name = amenity_rec.amenity_name
    WHENEVER ERROR STOP

    IF SQLCA.SQLCODE = 0 THEN
        MESSAGE "ROW UPDATED"
    ELSE
        ERROR SQLERRMESSAGE
    END IF

END FUNCTION
 
FUNCTION delete_value()
  DEFINE del_ok SMALLINT
  
    WHENEVER ERROR CONTINUE
    DELETE FROM amenity WHERE amenity_name = amenity_rec.amenity_name
    WHENEVER ERROR STOP
    IF SQLCA.SQLCODE = 0 THEN
       MESSAGE "ROW DELETED"
       INITIALIZE amenity_rec.* TO NULL
       DISPLAY BY NAME amenity_rec.* 
    ELSE
      ERROR SQLERRMESSAGE
    END IF 
 
 END FUNCTION   

FUNCTION delete_check()
  DEFINE del_ok SMALLINT,
         del_count SMALLINT
  
  LET del_ok = FALSE
    
  WHENEVER ERROR CONTINUE
  SELECT COUNT(*)
 INTO del_count FROM amenity
    WHERE amenity.amenity_name = amenity_rec.amenity_name
  WHENEVER ERROR STOP
  
  IF del_count = 0 THEN
    MESSAGE "NO RECORD FOUND SO CANNOT BE DELETED"
    LET del_ok = FALSE
  ELSE
    MENU  "DELETE" ATTRIBUTE ( STYLE="dialog", COMMENT="DELETE THE ROW?" )
    COMMAND "Yes"
      LET del_ok = TRUE
      EXIT MENU
    COMMAND "No"
      MESSAGE "OPERATION CANCLLED"
      EXIT MENU
    END MENU
  END IF
    
  RETURN del_ok
  
END FUNCTION

FUNCTION clean_up_fun()

    WHENEVER ERROR CONTINUE
    CLOSE cust_curs
    FREE cust_curs
    WHENEVER ERROR STOP

END FUNCTION

 FUNCTION amenity_report()

    DEFINE i2genrec RECORD
                      LIKE Amenity.*
       

      IF fgl_report_LoadCurrentSettings("amenityreport.4rp") THEN
        CALL fgl_report_SelectDevice("PDF")
        CALL fgl_report_SelectPreview(TRUE)
        LET hndl = fgl_report_commitCurrentSettings()

    ELSE
        EXIT PROGRAM
    END IF

    START REPORT amreport TO XML HANDLER hndl

    DECLARE i2curr1 CURSOR FOR
        SELECT *
            FROM Amenity
        

    FOREACH i2curr1 INTO i2genrec.*
        OUTPUT TO REPORT amreport(i2genrec.*)

    END FOREACH

    FINISH REPORT amreport

END FUNCTION

REPORT amreport(printi2genrec)

    DEFINE printi2genrec RECORD
         LIKE Amenity.*
       

    FORMAT

--FIRST PAGE HEADER
--PAGE HEADER
--PRINTX createdate
        ON EVERY ROW
--DISPLAY "printi2genrec", printi2genrec.id

            PRINTX printi2genrec.amenity_name
            PRINTX printi2genrec.booked_by
            PRINTX printi2genrec.charges
            PRINTX printi2genrec.on_date
            PRINTX printi2genrec.owner_type
            PRINTX printi2genrec.timing
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
 
 
 
   
 
  