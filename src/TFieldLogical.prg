/*
 *
 */

#include "oordb.ch"
#include "xerror.ch"

/*
    TFieldLogical
*/
CLASS TFieldLogical FROM TField

   PRIVATE:

   PROTECTED:
   DATA FDBS_LEN INIT 1
   DATA FDBS_DEC INIT 0
   DATA FDBS_TYPE INIT "L"
   DATA FFieldType INIT ftLogical
   DATA FSize INIT 1
   DATA FType INIT "Logical"
   DATA FtypeNameList INIT HB_HSetCaseMatch( {"es"=>"Logico"} )
   DATA FValType INIT "L"
   METHOD GetEmptyValue BLOCK {|| .F. }
   PUBLIC:

   METHOD GetAsString(value)
   METHOD GetKeyVal( keyVal )
   METHOD IndexExpression( fieldName )

   PROPERTY AsBoolean READ GetAsVariant WRITE SetAsVariant

   PUBLISHED:

   PROPERTY KeySize INIT 1

ENDCLASS

/*
    GetAsString
*/
METHOD FUNCTION GetAsString(value) CLASS TFieldLogical
    IF pCount() = 0
        value := ::getAsVariant()
    ENDIF
RETURN iif( value, ".T.", ".F." )

/*
    GetKeyVal
*/
METHOD FUNCTION GetKeyVal( keyVal ) CLASS TFieldLogical

   SWITCH ValType( keyVal )
   CASE 'C'
      RETURN keyVal
   CASE 'L'
      RETURN iif( keyVal, "T", "F" )
   CASE 'U'
      RETURN iif( ::GetAsVariant(), "T", "F" )
   ENDSWITCH

   RAISE TFIELD ::Name ERROR "Don't know how to convert to key value..."

   RETURN NIL

/*
    IndexExpression
*/
METHOD FUNCTION IndexExpression( fieldName ) CLASS TFieldLogical

   IF ::FIndexExpression != NIL
      RETURN ::FIndexExpression
   ENDIF
   IF fieldName = NIL
      fieldName := ::FFieldExpression
   ENDIF

   RETURN "iif(" + fieldName + ",'T','F')"

/*
    ENDCLASS TFieldLogical
*/
