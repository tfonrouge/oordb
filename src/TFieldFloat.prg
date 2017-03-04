/*
 *
 */

#include "oordb.ch"
#include "xerror.ch"

/*
    TFieldFloat
*/
CLASS TFieldFloat FROM TFieldNumeric

   PRIVATE:

   PROTECTED:
   DATA FDBS_LEN INIT 12
   DATA FDBS_DEC INIT 2
   DATA FDBS_TYPE INIT "B"
   DATA FFieldType INIT ftFloat
   DATA FType INIT "Float"
   DATA FtypeNameList INIT HB_HSetCaseMatch( {"es"=>"Numerico Decimal"} )
   METHOD StrFormat( value ) INLINE hb_StrFormat( "%10." + Chr( 48 + ::FDBS_DEC ) + "f", value )
   PUBLIC:
   PUBLISHED:

ENDCLASS

/*
    ENDCLASS TFieldFloat
*/
