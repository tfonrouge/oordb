/*
 *
 */

#include "oordb.ch"
#include "xerror.ch"

/*
    TFieldDate
*/
CLASS TFieldDate FROM TField

   PRIVATE:

   PROTECTED:
   DATA FDBS_LEN INIT 4
   DATA FDBS_DEC INIT 0
   DATA FDBS_TYPE INIT "D"
   DATA FFieldType INIT ftDate
   DATA FSize INIT 8     // Size on index is 8 = len of DToS()
   DATA FType INIT "Date"
   DATA FtypeNameList INIT HB_HSetCaseMatch( {"es"=>"Fecha"} )
   DATA FValType INIT "D"
   METHOD GetEmptyValue BLOCK {|| CToD( "" ) }
   PUBLIC:

   METHOD GetAsString(value)
   METHOD GetKeyVal( keyVal )
   METHOD IndexExpression( fieldName )
   METHOD SetAsVariant( variant )

   PUBLISHED:

   PROPERTY KeySize INIT 8

ENDCLASS

/*
    getAsString
*/
METHOD FUNCTION getAsString(value) CLASS TFieldDate
    IF pCount() = 0
        value := ::getAsVariant()
    ENDIF
RETURN fDateS( value )

/*
    GetKeyVal
*/
METHOD FUNCTION GetKeyVal( keyVal ) CLASS TFieldDate

   SWITCH ValType( keyVal )
   CASE 'C'
      RETURN keyVal
   CASE 'D'
      RETURN DToS( keyVal )
   CASE 'T'
      RETURN dToS( hb_tToD( keyVal ) )
   CASE 'U'
      RETURN DToS( ::GetAsVariant() )
   ENDSWITCH

   RAISE TFIELD ::Name ERROR "Don't know how to convert to key value..."

   RETURN NIL

/*
    IndexExpression
*/
METHOD FUNCTION IndexExpression( fieldName ) CLASS TFieldDate

   IF ::FIndexExpression != NIL
      RETURN ::FIndexExpression
   ENDIF
   IF fieldName = NIL
      fieldName := ::FFieldExpression
   ENDIF

   RETURN "DToS(" + fieldName + ")"

/*
    SetAsVariant
*/
METHOD PROCEDURE SetAsVariant( variant ) CLASS TFieldDate

   SWITCH ValType( variant )
   CASE 'C'
      ::Super:SetAsVariant( AsDate( variant ) )
      EXIT
   CASE 'D'
      ::Super:SetAsVariant( variant )
      EXIT
   CASE 'T'
      ::super:setAsVariant( hb_tToD( variant ) )
      EXIT
   ENDSWITCH

   RETURN

/*
    ENDCLASS TFieldDate
*/
