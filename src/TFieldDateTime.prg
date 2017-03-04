/*
 *
 */

#include "oordb.ch"
#include "xerror.ch"

/*
    TFieldDateTime
*/
CLASS TFieldDateTime FROM TField

PROTECTED:

   DATA FDBS_LEN INIT 8
   DATA FDBS_DEC INIT 0
   DATA FDBS_TYPE INIT "@"
   DATA FFieldType INIT ftDateTime
   DATA FFormatDate
   DATA FFormatTime
   DATA FSize INIT 23
   DATA FTime
   DATA FType INIT "DateTime"
   DATA FtypeNameList INIT HB_HSetCaseMatch( {"es"=>"Fecha Hora"} )
   DATA FValType INIT "C"
   METHOD GetAsDatePart() INLINE hb_TToD( ::Value )
   METHOD GetAsTimePart()
   METHOD GetEmptyValue BLOCK {|| hb_CToT( "" ) }
   METHOD GetFormatDate INLINE iif( ::FFormatDate = NIL, ::ClsFmtDate, ::FFormatDate )
   METHOD GetFormatTime INLINE iif( ::FFormatTime = NIL, ::ClsFmtTime, ::FFormatTime )
   METHOD GetTime INLINE iif( ::FTime = NIL, ::FTime := TTime():New(), ::FTime )
   METHOD SetAsDatePart( date ) INLINE ::Value := hb_DToT( date, ::GetAsTimePart )
   METHOD SetAsTimePart( cTimePart )
   METHOD SetFormatDate( formatDate ) INLINE ::FFormatDate := formatDate
   METHOD SetFormatTime( formatTime ) INLINE ::FFormatTime := formatTime

   PROPERTY Time READ GetTime

PUBLIC:

   CLASSDATA ClsFmtDate INIT "YYYY-MM-DD"
   CLASSDATA ClsFmtTime INIT "HH:MM"

   METHOD DiffSeconds( dateTimePrev )
   METHOD DiffTime( dateTimePrev, formatTime )
   METHOD GetAsDisplay INLINE ::GetAsString
   METHOD GetAsDisplayEmptyValue INLINE ::GetAsString( ::GetEmptyValue )
   METHOD GetAsString( value )
   METHOD GetKeyVal( keyVal )
   METHOD IndexExpression( fieldName )
   METHOD SetAsVariant( variant )

   PROPERTY AsDatePart READ GetAsDatePart WRITE SetAsDatePart
   PROPERTY AsTimePart READ GetAsTimePart WRITE SetAsTimePart
   PROPERTY FormatDate READ GetFormatDate WRITE SetFormatDate
   PROPERTY FormatTime READ GetFormatTime WRITE SetFormatTime

   PROPERTY KeySize INIT 23

ENDCLASS

/*
    DiffSeconds
*/
METHOD FUNCTION DiffSeconds( dateTimePrev ) CLASS TFieldDateTime

   LOCAL t1, t2

   IF dateTimePrev = NIL
      dateTimePrev := hb_DateTime()
   ENDIF

   IF dateTimePrev = ::Value
      RETURN 0.0
   ENDIF

   IF dateTimePrev < ::Value
      t1 := ::Value
      t2 := dateTimePrev
   ELSE
      t2 := ::Value
      t1 := dateTimePrev
   ENDIF

   RETURN ( t1 - t2 ) * 86400

/*
    DiffTime
*/
METHOD FUNCTION DiffTime( dateTimePrev, formatTime ) CLASS TFieldDateTime
    LOCAL t := TTime():New()

    IF formatTime != NIL
        t:Format := formatTime
    ENDIF

    t:SetAsSeconds( ::DiffSeconds( dateTimePrev ) )

RETURN t

/*
    GetAsString
*/
METHOD FUNCTION GetAsString( value ) CLASS TFieldDateTime

   IF value = NIL
      value := ::Value
   ENDIF
   IF ::FormatDate = NIL .AND. ::FormatTime = NIL
      RETURN hb_TSToStr( value )
   ENDIF

   RETURN hb_TToC( value, ::FormatDate, ::FormatTime )

/*
    GetAsTimePart
*/
METHOD GetAsTimePart() CLASS TFieldDateTime

   LOCAL cTime := "00:00:00"
   LOCAL time

   time := ::GetAsVariant()

   IF !Empty( time )
      HB_TToD( time, @cTime, ::FormatTime )
   ENDIF

   ::Time:Format := ::FormatTime
   ::Time:AsString := cTime

   RETURN ::Time

/*
    GetKeyVal
*/
METHOD FUNCTION GetKeyVal( keyVal ) CLASS TFieldDateTime

   SWITCH ValType( keyVal )
   CASE 'C'
      RETURN keyVal
   CASE 'D'
      RETURN dToS( keyVal )
   CASE 'T'
      RETURN hb_TToS( keyVal )
   CASE 'U'
      RETURN hb_TToS( ::GetAsVariant() )
   ENDSWITCH

   RAISE TFIELD ::Name ERROR "Don't know how to convert to key value..."

   RETURN hb_TToS( keyVal )

/*
    IndexExpression
*/
METHOD FUNCTION IndexExpression( fieldName ) CLASS TFieldDateTime

   IF ::FIndexExpression != NIL
      RETURN ::FIndexExpression
   ENDIF
   IF fieldName = NIL
      fieldName := ::FFieldExpression
   ENDIF

   RETURN "HB_TToS(" + fieldName + ")"

/*
    SetAsTimePart
*/
METHOD FUNCTION SetAsTimePart( cTimePart ) CLASS TFieldDateTime

   ::Time:Format := ::FormatTime
   ::Time:AsString := cTimePart

   ::SetAsVariant( hb_DToT( ::Value, ::Time:AsSeconds ) )

   RETURN ::Time

/*
    SetAsVariant
*/
METHOD PROCEDURE SetAsVariant( variant ) CLASS TFieldDateTime

   SWITCH ValType( variant )
   CASE 'T'
      ::Super:SetAsVariant( variant )
      EXIT
   CASE 'C'
      variant := RTrim( variant )
      IF NumToken( variant ) > 1
         variant := hb_CToT( variant, ::FormatDate, ::FormatTime )
      ELSE
         variant := hb_SToT( variant )
      ENDIF
      ::Super:SetAsVariant( variant )
      EXIT
   CASE 'D'
      ::Super:SetAsVariant( hb_DToT( variant, ::GetAsTimePart() ) )
      EXIT
   CASE 'N'
      ::Super:SetAsVariant( hb_NToT( variant ) )
      EXIT
   ENDSWITCH

   RETURN

/*
    EndClass TFieldDateTime
*/
