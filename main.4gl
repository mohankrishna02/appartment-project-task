IMPORT FGL amenity
IMPORT FGL apartment
IMPORT FGL association
IMPORT FGL helpdesk
IMPORT FGL owner
IMPORT FGL payment
IMPORT FGL tanent
IMPORT FGL transcation
DATABASE hrmdb
MAIN 
CLOSE WINDOW SCREEN 
OPEN WINDOW HOME WITH FORM "main"
MENU 
ON ACTION amenity
CALL amenity_fun()
ON ACTION Apartment 
CALL Apartment_fun()
ON ACTION Association
CALL association_fun()
ON ACTION Helpdesk
CALL helpdesk_fun()
ON ACTION owner
CALL owner_fun()
ON ACTION Payment
CALL payment_fun()
ON ACTION tanent
CALL tanent_fun()
ON ACTION Transaction
CALL trans_fun()
ON ACTION EXIT 
EXIT MENU 
END MENU 
CLOSE WINDOW HOME
END MAIN 