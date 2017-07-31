
#include "oordb.ch"

CLASS TBMemTable FROM TTable

  DEFINE DATABASE WITH MyDataBase CLASS

EXPORTED:

    PROPERTY isMemTable INIT .T.

    DEFINE FIELDS
    DEFINE PRIMARY INDEX
    DEFINE SECONDARY INDEX

    METHOD onAfterOpen()

ENDCLASS

/*
    FIELDS
*/
BEGIN FIELDS CLASS TBMemTable

    /* Id */
    ADD AUTOINC FIELD "Id"

    /* First */
    ADD STRING FIELD "First" SIZE 20

    /* Last */
    ADD STRING FIELD "Last" SIZE 20

    /* Street */
    ADD STRING FIELD "Street" SIZE 30

    /* City */
    ADD STRING FIELD "City" SIZE 30

    /* State */
    ADD STRING FIELD "State" SIZE 2

END FIELDS CLASS

/*
    PRIMARY INDEX
*/
BEGIN PRIMARY INDEX CLASS TBMemTable

    DEFINE INDEX "Primary" KEYFIELD "Id"

END PRIMARY INDEX CLASS

/*
    SECONDARY INDEX
*/
BEGIN SECONDARY INDEX CLASS TBMemTable

    DEFINE INDEX "First" KEYFIELD "First"
    DEFINE INDEX "Last" KEYFIELD "Last"
    DEFINE INDEX "State" KEYFIELD "State" /* order by 'State' field */

END SECONDARY INDEX CLASS

/*
    onAfterOpen
*/
METHOD PROCEDURE onAfterOpen() CLASS TBMemTable
    LOCAL itm
    LOCAL field
    LOCAL aliasTest
    LOCAL value

    IF ::count() = 0

        aliasTest := TAlias():new( "../../testDbf/test" )

        ?
        ? "Opening table and adding 10 records..."

        FOR itm := 1 TO 10
            aliasTest:dbGoTo( hb_randomInt( aliasTest:lastRec() ) )
            IF ::insert()
                FOR EACH field IN ::fieldList
                    IF field:fieldMethodType = "C"
                        value := aliasTest:fieldValueGet( field:name )
                        IF value != nil
                            field:value := value
                        ENDIF
                    ENDIF
                NEXT
                ::post()
            ENDIF
        NEXT
        
    ENDIF

    ::super:onAfterOpen()

RETURN
