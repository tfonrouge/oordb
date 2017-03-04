/*
 *
 */

#include "oordb.ch"
#include "xerror.ch"

/*
    TFieldNumeric
*/
CLASS TFieldNumeric FROM TField

PROTECTED:

   DATA FDBS_LEN INIT 15   // 000000000000.00
   DATA FDBS_DEC INIT 2
   DATA FDBS_TYPE INIT "N"
   DATA FFieldType INIT ftNumeric
   DATA FSize
   DATA FType INIT "Numeric"
   DATA FtypeNameList INIT HB_HSetCaseMatch( {"es"=>"Numero"} )
   DATA FValType INIT "N"
   METHOD GetEmptyValue BLOCK {|| 0 }
   METHOD StrFormat( value ) INLINE Str( value )

PUBLIC:

   METHOD GetAsString( value )
   METHOD GetKeyVal( keyVal )
   METHOD IndexExpression( fieldName )
   METHOD NormalizeValue( cValue )
   METHOD SetAsVariant( variant )

   PROPERTY AsNumeric READ GetAsVariant WRITE SetAsVariant
   PROPERTY KeySize INIT 15
   PROPERTY superFieldType INIT ftNumeric

ENDCLASS

/*
    GetAsString
*/
METHOD FUNCTION GetAsString( value ) CLASS TFieldNumeric

   LOCAL Result

   IF value == NIL
      value := ::GetAsVariant()
   ENDIF

   IF ::OnGetText != NIL
      Result := Value
      ::OnGetText:Eval( Self, @Result )
   ELSE
      IF ::Picture = NIL
         Result := ::StrFormat( value )
      ELSE
         Result := Transform( value, ::Picture )
      ENDIF
   ENDIF

   RETURN Result

/*
    GetKeyVal
*/
METHOD FUNCTION GetKeyVal( keyVal ) CLASS TFieldNumeric

   SWITCH ValType( keyVal )
   CASE 'C'
      RETURN keyVal
   CASE 'N'
      RETURN StrZero( keyVal, ::FDBS_LEN, ::FDBS_DEC )
   CASE 'U'
      RETURN StrZero( ::GetAsVariant(), ::FDBS_LEN, ::FDBS_DEC )
   ENDSWITCH

   RAISE TFIELD ::Name ERROR "Don't know how to convert to key value..."

   RETURN NIL

/*
    IndexExpression
*/
METHOD FUNCTION IndexExpression( fieldName ) CLASS TFieldNumeric

   IF ::FIndexExpression != NIL
      RETURN ::FIndexExpression
   ENDIF
   IF fieldName = NIL
      fieldName := ::FFieldExpression
   ENDIF

   RETURN "StrZero(" + fieldName + "," +  HB_NToS( ::FDBS_LEN ) + "," + HB_NToS( ::FDBS_DEC ) + ")"

/*
    NormalizeValue
*/
METHOD PROCEDURE NormalizeValue( cValue ) CLASS TFieldNumeric
    LOCAL itm
    FOR EACH itm IN ",'"
        IF itm $ cValue
            cValue := StrTran( cValue, itm, "" )
        ENDIF
    NEXT
RETURN

/*
    SetAsVariant
*/
METHOD PROCEDURE SetAsVariant( variant ) CLASS TFieldNumeric

   SWITCH ValType( variant )
   CASE 'C'
      ::NormalizeValue( @variant )
      ::Super:SetAsVariant( Val( variant ) )
      EXIT
   CASE 'N'
      ::Super:SetAsVariant( variant )
      EXIT
   ENDSWITCH

   RETURN

/*
    ENDCLASS TFieldNumeric
*/
