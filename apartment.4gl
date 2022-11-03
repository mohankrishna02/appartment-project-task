DATABASE hrmdb
DEFINE apartment_rec RECORD LIKE Apartment.*
DEFINE query_ok SMALLINT
DEFINE match STRING
DEFINE i INT 
DEFINE hndl om.SaxDocumentHandler


FUNCTION apartment_fun()
   
        OPEN WINDOW APARTMENT WITH FORM "apartmentper"
        CALL fun_apartment()
        
CLOSE WINDOW APARTMENT
END FUNCTION

FUNCTION fun_apartment()
DEFINE query_ok smallint

    MENU 
        COMMAND "find"
        CLEAR FORM 
            CALL apartment_query() RETURNING query_ok
        COMMAND "next"
            IF (query_ok) THEN
                CALL apartment_fetch_rel(1)
            ELSE
                MESSAGE "YOU MUST QUERY FIRST." 

            END IF
        COMMAND "previous"
            IF (query_ok) THEN
                CALL apartment_fetch_rel(-1)
            ELSE
                MESSAGE "YOU MUST QUERY FIRST."
            END IF

        ON ACTION ADD 
       IF(apartment_input_clg("A")) then
            CALL apartment_insert()
            END IF

        COMMAND "Modify"
            CALL apartment_update()

        COMMAND "Delete"
        IF (apartment_delete_check()) THEN 
            CALL apartment_delete()
            END IF 
            COMMAND "clear"
            CLEAR FORM 
          COMMAND "quit"
            EXIT MENU
            COMMAND "print"
            CALL apartment_report()
    END MENU

END FUNCTION


FUNCTION apartment_query()
    DEFINE
        cont_ok SMALLINT,
        cust_cnt SMALLINT,
        where_clause STRING

    MESSAGE "Enter search criteria"
    LET cont_ok = FALSE                 
    LET int_flag = FALSE                          

    CONSTRUCT BY NAME where_clause ON Apartment.*
                        
    
    IF (int_flag) = TRUE THEN
        LET int_flag = FALSE                       
        CLEAR FORM
        LET cont_ok = FALSE
        MESSAGE "CANCELED BY USER"
    ELSE
        CALL apartment_get_cust_cnt(where_clause) RETURNING cust_cnt
        IF (cust_cnt > 0) THEN
            MESSAGE cust_cnt USING "<<<<", " ROWS FOUND"
            CALL apartment_cust_select(where_clause) RETURNING cont_ok   
        ELSE
            MESSAGE "NO ROWS FOUND"
            LET cont_ok = FALSE
        END IF
    END IF

    IF (cont_ok) THEN
        CALL apartment_display()
    END IF

    RETURN cont_ok

END FUNCTION

FUNCTION apartment_get_cust_cnt(p_where_clause)     
    DEFINE
        p_where_clause STRING,
        sql_text STRING,
        cust_cnt SMALLINT

    LET sql_text = "SELECT COUNT(*)
 FROM Apartment  WHERE " || p_where_clause CLIPPED
  
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

FUNCTION apartment_cust_select(p_where_clause) 
    DEFINE
        p_where_clause STRING,
        sql_text STRING,
        fetch_ok SMALLINT

    LET sql_text = "SELECT * FROM Apartment WHERE " || p_where_clause CLIPPED 

    DECLARE cust_curs SCROLL CURSOR WITH HOLD FROM sql_text

    OPEN cust_curs
    CALL apartment_fetch_cust(1)
        RETURNING fetch_ok 
    IF NOT (fetch_ok) THEN
        MESSAGE "NO ROWS IN THE TABLE" 
    END IF

    RETURN fetch_ok   


END FUNCTION

FUNCTION apartment_fetch_cust(p_fetch_flag)
    DEFINE
        p_fetch_flag SMALLINT,
        fetch_ok SMALLINT

    LET fetch_ok = TRUE
    WHENEVER ERROR CONTINUE
    IF p_fetch_flag = 1 THEN
        FETCH NEXT cust_curs INTO apartment_rec.*
    ELSE
        FETCH PREVIOUS cust_curs INTO apartment_rec.*
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

FUNCTION apartment_fetch_rel(p_fetch_flag)
    DEFINE
        p_fetch_flag SMALLINT,
        fetch_ok SMALLINT

    MESSAGE " "
    CALL apartment_fetch_cust(p_fetch_flag) RETURNING fetch_ok

    IF (fetch_ok) THEN
        CALL apartment_display()
    ELSE
        IF (p_fetch_flag = 1) THEN
            MESSAGE "END OF LIST"
        ELSE
            MESSAGE "BEGINING OF LIST"
        END IF
    END IF

END FUNCTION

FUNCTION apartment_display()
    DISPLAY BY NAME apartment_rec.*              
END FUNCTION

FUNCTION apartment_input_clg(au_flag)
      DEFINE au_flag CHAR(20),
      count_ok INTEGER 

      LET count_ok = TRUE
      IF(au_flag = "A") THEN
              MESSAGE "ENTER APARTMENT DETAILS"

              INITIALIZE apartment_rec.* TO NULL
      END IF 

     LET int_flag = FALSE 

     INPUT BY NAME apartment_rec.* WITHOUT DEFAULTS ATTRIBUTES (UNBUFFERED)
    ON CHANGE Apartment_name
    IF au_flag="A" THEN
    SELECT Apartment_name,Block_name,floor_no,Size_sqft,Unit_number
    INTO apartment_rec.*
    FROM Apartment
    WHERE Apartment_name = apartment_rec.Apartment_name
    IF(sqlca.sqlcode=0)THEN
    ERROR "APARTMENT NAME ALREADY IN DATABASE"
    LET count_ok =FALSE
    EXIT INPUT
    END IF 
    END IF


    AFTER FIELD Apartment_name
    IF apartment_rec.Apartment_name IS NULL THEN
    ERROR "ENTER APARTMENT NAME"
    NEXT FIELD Apartment_name
    END IF
    IF NOT apartment_rec.apartment_name MATCHES "[a-z A-Z]*"  THEN
    MESSAGE "NAME SHOULD  BE CHARACTER"
    NEXT FIELD Apartment_name
    END IF
    AFTER FIELD Block_name
    IF apartment_rec.block_name IS NULL THEN
    MESSAGE "ENTER BLOCK NAME"
    NEXT FIELD Block_name
    END IF
    IF NOT apartment_rec.block_name MATCHES "[a-z A-Z]*"  THEN
    MESSAGE "NAME SHOULD  BE CHARACTER"
    NEXT FIELD Block_name
    END IF
    LET match =apartment_rec.block_name
    FOR i=1 TO LENGTH(match)
    IF match.getCharAt(i) MATCHES "[0-9]" THEN
    ERROR "NAME SHOULD BE CHARACTER"
    NEXT FIELD Block_name
    END IF
    END FOR

    AFTER FIELD floor_no
    IF apartment_rec.floor_no IS NULL THEN
    MESSAGE "ENTER FLOOR NUMBER"
    NEXT FIELD floor_no
    END IF

    LET match =apartment_rec.floor_no
    FOR i=1 TO LENGTH(match)
    IF NOT match.getCharAt(i) MATCHES "[0-9]" THEN
    ERROR "FLOOR NUMBER SHOULD BE IN DIGITS"
    NEXT FIELD floor_no
    END IF
    END FOR

    AFTER FIELD Size_sqft
    IF apartment_rec.size_sqft IS NULL THEN
    MESSAGE "ENTER SIZE IN SQFT"
    NEXT FIELD Size_sqft
    END IF

    AFTER FIELD unit_number
    IF apartment_rec.unit_number IS NULL THEN
    MESSAGE "ENTER UNIT NUMBER "
    NEXT FIELD unit_number
    END IF


    IF  apartment_rec.unit_number <= 0 then
    ERROR "UNIT NUMBER SHOULD BE GREATER THAN ZERO"
    NEXT FIELD unit_number
    END IF


NEXT FIELD Apartment_name

            END INPUT 

            IF (int_flag) THEN 
                LET int_flag = FALSE
                LET count_ok = FALSE 
                MESSAGE "OPERATION CANCELLED BY USER"
            END IF 

            RETURN count_ok

 END FUNCTION 

FUNCTION apartment_insert()
    INPUT BY NAME apartment_rec.* WITHOUT DEFAULTS ATTRIBUTE(UNBUFFERED)

    WHENEVER ERROR CONTINUE
    INSERT INTO Apartment VALUES(apartment_rec.*)
    WHENEVER ERROR STOP

    IF SQLCA.SQLCODE = 0 THEN
        MESSAGE "ROW ADDED"
    ELSE
        ERROR SQLERRMESSAGE
    END IF

END FUNCTION

FUNCTION apartment_update()
   

    WHENEVER ERROR CONTINUE
    SELECT  INTO apartment_rec FROM Apartment WHERE Apartment_name = apartment_rec.Apartment_name 
    INPUT BY NAME apartment_rec.* WITHOUT DEFAULTS ATTRIBUTE(UNBUFFERED)
    WHENEVER ERROR STOP

    WHENEVER ERROR CONTINUE
    UPDATE Apartment
        SET  
              Block_name = apartment_rec.block_name,
              floor_no = apartment_rec.floor_no,
              size_sqft = apartment_rec.size_sqft,
              unit_number =apartment_rec.unit_number

                WHERE Apartment_name = apartment_rec.Apartment_name
    WHENEVER ERROR STOP

    IF SQLCA.SQLCODE = 0 THEN
        MESSAGE "ROW UPDATED"
    ELSE
        ERROR SQLERRMESSAGE
    END IF

END FUNCTION
 
FUNCTION apartment_delete()
  DEFINE del_ok SMALLINT
  
    WHENEVER ERROR CONTINUE
    DELETE FROM Apartment WHERE Apartment_name = apartment_rec.Apartment_name
    WHENEVER ERROR STOP
    IF SQLCA.SQLCODE = 0 THEN
       MESSAGE "ROW DELETED"
       INITIALIZE apartment_rec.* TO NULL
       DISPLAY BY NAME apartment_rec.* 
    ELSE
      ERROR SQLERRMESSAGE
    END IF 
 
 END FUNCTION   

FUNCTION apartment_delete_check()
  DEFINE del_ok SMALLINT,
         del_count SMALLINT
  
  LET del_ok = FALSE
    
  WHENEVER ERROR CONTINUE
  SELECT COUNT(*)
 INTO del_count FROM Apartment
    WHERE Apartment.Apartment_name = apartment_rec.Apartment_name
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

FUNCTION apartment_cleanup_fun()

    WHENEVER ERROR CONTINUE
    CLOSE cust_curs
    FREE cust_curs
    WHENEVER ERROR STOP

END FUNCTION

FUNCTION apartment_report()

    DEFINE i2genrec RECORD
                      LIKE apartment.*
       

      IF fgl_report_LoadCurrentSettings("aparreport.4rp") THEN
        CALL fgl_report_SelectDevice("PDF")
        CALL fgl_report_SelectPreview(TRUE)
        LET hndl = fgl_report_commitCurrentSettings()

    ELSE
        EXIT PROGRAM
    END IF

    START REPORT apreport TO XML HANDLER hndl

    DECLARE i2curr1 CURSOR FOR
        SELECT *
            FROM Apartment
        

    FOREACH i2curr1 INTO i2genrec.*
        OUTPUT TO REPORT apreport(i2genrec.*)

    END FOREACH

    FINISH REPORT apreport

END FUNCTION

REPORT apreport(printi2genrec)

    DEFINE printi2genrec RECORD
         LIKE Apartment.*
       

    FORMAT

--FIRST PAGE HEADER
--PAGE HEADER
--PRINTX createdate
        ON EVERY ROW
--DISPLAY "printi2genrec", printi2genrec.id

            PRINTX printi2genrec.apartment_name
            PRINTX printi2genrec.block_name
            PRINTX printi2genrec.floor_no
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

 
 
   
 
  