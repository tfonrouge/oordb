/*
 *
 */

#include "oordb.ch"
#include "xerror.ch"

/*
    TFieldInteger
*/
CLASS TFieldInteger FROM TFieldNumeric

   PRIVATE:

   PROTECTED:
   DATA FDBS_LEN INIT 4
   DATA FDBS_DEC INIT 0
   DATA FDBS_TYPE INIT "I"
   DATA FFieldType INIT ftInteger
   DATA FSize INIT 4
   DATA FType INIT "Integer"
   DATA FtypeNameList INIT HB_HSetCaseMatch( {"es"=>"Numero Entero"} )
   METHOD StrFormat( value ) INLINE hb_StrFormat( "%d", value )
   PUBLIC:

   METHOD GetKeyVal( keyVal )
   METHOD IndexExpression( fieldName )
   METHOD SetAsVariant( variant )

   PROPERTY AsInteger READ GetAsVariant WRITE SetAsVariant

   PUBLISHED:

   PROPERTY KeySize INIT 8

ENDCLASS

/*
    GetKeyVal
*/
METHOD FUNCTION GetKeyVal( keyVal ) CLASS TFieldInteger

   SWITCH ValType( keyVal )
   CASE 'C'
      RETURN keyVal
   CASE 'N'
      RETURN hb_NumToHex( keyVal, 8 )
   CASE 'U'
      RETURN hb_NumToHex( ::GetAsVariant(), 8 )
   ENDSWITCH

   RAISE TFIELD ::Name ERROR "Don't know how to convert to key value ('" + ValType( keyVal ) + "')..."

   RETURN NIL

/*
    IndexExpression
*/
METHOD FUNCTION IndexExpression( fieldName ) CLASS TFieldInteger

   IF ::FIndexExpression != NIL
      RETURN ::FIndexExpression
   ENDIF
   IF fieldName = NIL
      fieldName := ::FFieldExpression
   ENDIF

   RETURN "HB_NumToHex(" + fieldName + ",8)"

/*
    SetAsVariant
*/
METHOD PROCEDURE SetAsVariant( variant ) CLASS TFieldInteger

   SWITCH ValType( variant )
   CASE 'C'
      ::NormalizeValue( @variant )
      ::Super:SetAsVariant( Int( Val( variant ) ) )
      EXIT
   CASE 'N'
      ::Super:SetAsVariant( Int( variant ) )
      EXIT
   ENDSWITCH

   RETURN

/*
    ENDCLASS TFieldInteger
*/
