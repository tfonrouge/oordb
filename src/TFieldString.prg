/*
 *
 */

#include "oordb.ch"
#include "xerror.ch"

/*
    TFieldString
*/
CLASS TFieldString FROM TField

PROTECTED:

   DATA FDBS_LEN INIT 0
   DATA FDBS_DEC INIT 0
   DATA FDBS_TYPE INIT "C"
   DATA FFieldType INIT ftString
   DATA FSize
   DATA FType INIT "String"
   DATA FtypeNameList INIT HB_HSetCaseMatch( {"es"=>"Texto"} )
   DATA FValType INIT "C"
   METHOD GetEmptyValue INLINE Space( ::Size )
   METHOD GetAsNumeric INLINE Val( ::GetAsVariant() )
   METHOD GetSize()
   METHOD SetAsNumeric( n ) INLINE ::SetAsVariant( LTrim( Str( n ) ) )
   METHOD SetAsVariant( value )
   METHOD SetBuffer( buffer, lNoCheckValidValue )
   METHOD SetDBS_LEN( dbs_Len )
   METHOD SetDefaultValue( DefaultValue )
   METHOD SetSize( size )

PUBLIC:

   METHOD GetAsString()
   METHOD IndexExpression( fieldName, isMasterFieldComponent, keyFlags )

   PROPERTY AsNumeric READ GetAsNumeric WRITE SetAsNumeric
   PROPERTY KeySize READ Size
   PROPERTY Size READ GetSize WRITE SetSize
   PROPERTY superFieldType INIT ftString

ENDCLASS

/*
    GetAsString
*/
METHOD FUNCTION GetAsString() CLASS TFieldString

   LOCAL Result := ""
   LOCAL i

   SWITCH ::FFieldMethodType
   CASE 'A'
      FOR EACH i IN ::FFieldArrayIndex
         Result += ::FTable:FieldList[ i ]:AsString()
      NEXT
      EXIT
   OTHERWISE
      Result := ::GetAsVariant()
   ENDSWITCH

   RETURN Result

/*
    GetSize
*/
METHOD FUNCTION GetSize() CLASS TFieldString

   LOCAL i

   IF ::FSize = NIL
      IF ::FFieldMethodType = "A"
         ::FSize := 0
         FOR EACH i IN ::FFieldArrayIndex
            ::FSize += ::FTable:FieldList[ i ]:Size
         NEXT
      ENDIF
   ENDIF

   RETURN ::FSize

/*
    IndexExpression
*/
METHOD FUNCTION IndexExpression( fieldName, isMasterFieldComponent, keyFlags ) CLASS TFieldString

   LOCAL exp
   LOCAL i
   LOCAL itmName

   IF ::FIndexExpression != NIL
      RETURN ::FIndexExpression
   ENDIF

   IF fieldName = NIL
      fieldName := ::FFieldExpression
   ENDIF

   IF ::FFieldMethodType = "A"
      exp := ""
      FOR EACH i IN ::FFieldArrayIndex
         IF ValType( fieldName ) = "A" .AND. i:__enumIndex <= Len( fieldName )
            itmName := fieldName[ i:__enumIndex ]
         ELSE
            itmName := NIL
         ENDIF
         exp += iif( Len( exp ) = 0, "", "+" ) + ::FTable:FieldList[ i ]:IndexExpression( itmName, isMasterFieldComponent == .T. .OR. ( ::IsKeyIndex .AND. !::KeyIndex:CaseSensitive ), keyFlags )
      NEXT
   ELSE
      IF isMasterFieldComponent == .T. .OR. ::IsMasterFieldComponent .OR. ( ::IsKeyIndex .AND. ::KeyIndex:CaseSensitive )
         IF keyFlags != NIL .OR. ::KeyIndex != NIL
            IF keyFlags = NIL
               keyFlags := ::KeyIndex:KeyFlags
            ENDIF
            IF keyFlags != NIL .AND. hb_HHasKey( keyFlags, ::Name )
               SWITCH keyFlags[ ::Name ]
               CASE "U"
                  exp := "Upper(" + fieldName + ")"
                  EXIT
               ENDSWITCH
            ENDIF
         ENDIF
         IF exp = NIL
            exp := fieldName
         ENDIF
      ELSE
         IF ::FFieldExpression = NIL
            exp := "<error: IndexExpression on '" + ::Name + "'>"
         ELSE
            exp := "Upper(" + fieldName + ")"
         ENDIF
      ENDIF
   ENDIF

   RETURN exp

/*
    SetAsVariant
*/
METHOD FUNCTION SetAsVariant( value ) CLASS TFieldString
    IF ::Calculated .AND. len( value ) != ::size
        value := PadR( value, ::size )
    ENDIF
RETURN ::super:SetAsVariant( value )

/*
    SetBuffer
*/
METHOD FUNCTION SetBuffer( buffer, lNoCheckValidValue ) CLASS TFieldString

   LOCAL size := ::Size

   IF ::FFieldType = ftMemo .OR. Len( buffer ) = size
      RETURN ::Super:SetBuffer( buffer, lNoCheckValidValue )
   ELSE
      RETURN ::Super:SetBuffer( PadR( buffer, size ), lNoCheckValidValue )
   ENDIF

   RETURN .F.

/*
    SetDBS_LEN
*/
METHOD PROCEDURE SetDBS_LEN( dbs_Len ) CLASS TFieldString

   ::FDBS_LEN := dbs_Len
   IF ::FSize = NIL
      ::FSize := dbs_Len
   ENDIF

   RETURN

/*
    SetDefaultValue
*/
METHOD PROCEDURE SetDefaultValue( DefaultValue ) CLASS TFieldString

   ::FDefaultValue := DefaultValue

   ::FBuffer := NIL /* to force ::Reset on next read */

   RETURN

/*
    SetSize
*/
METHOD PROCEDURE SetSize( size ) CLASS TFieldString

   ::FSize := size
   ::FDBS_LEN := size

   RETURN

/*
    ENDCLASS TFieldString
*/
