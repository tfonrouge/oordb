/*
 *
 */

#include "hbclass.ch"
#include "property.ch"

CLASS TTime

   PROTECTED:
   DATA FDays INIT 0
   DATA FDecimals INIT 0.0
   DATA FHours INIT 0
   DATA FMinutes INIT 0
   DATA FSeconds INIT 0
   DATA FFormat INIT "HH:MM:SS"
   METHOD GetAsDays
   METHOD GetAsHours
   METHOD GetAsMinutes
   METHOD GetAsSeconds
   METHOD GetAsString( format )
   PUBLIC:

   CONSTRUCTOR New( time, format )

   METHOD SetAsDays( days )
   METHOD SetAsHours( hours )
   METHOD SetAsMinutes( minutes )
   METHOD SetAsSeconds( seconds )
   METHOD SetAsString ( time )
   METHOD SetFormat( format ) INLINE ::FFormat := format

   PROPERTY AsDays READ GetAsDays WRITE SetAsDays
   PROPERTY AsHours READ GetAsHours WRITE SetAsHours
   PROPERTY AsMinutes READ GetAsMinutes WRITE SetAsMinutes
   PROPERTY AsSeconds READ GetAsSeconds WRITE SetAsSeconds
   PROPERTY AsString READ GetAsString( ... ) WRITE SetAsString
   PROPERTY Days READ FDays
   PROPERTY Decimals READ FDecimals
   PROPERTY Hours READ FHours
   PROPERTY Minutes READ FMinutes
   PROPERTY Seconds READ FSeconds
   PROPERTY Format READ FFormat WRITE SetFormat

   METHOD Op_Add( timeToAdd )      OPERATOR "+"
   METHOD Op_Div( timeToDiv )      OPERATOR "/"
   METHOD Op_Minus( timeToMinus )  OPERATOR "-"
   METHOD Op_Mult( timeToMult )    OPERATOR "*"
   OPERATOR "==" FUNCTION TTime_Op_Equal()
   OPERATOR "=" FUNCTION TTime_Op_Equal()
   OPERATOR ">" FUNCTION TTIME_Op_Gt()
   OPERATOR ">=" FUNCTION TTIME_Op_GtEq()
   OPERATOR "<" FUNCTION TTIME_Op_Mi()
   OPERATOR "<=" FUNCTION TTIME_Op_MiEq()

ENDCLASS

STATIC FUNCTION  TTime_Op_Equal( equalTo )

   LOCAL Self := QSelf()

   SWITCH ValType( equalTo )
   CASE "O"
      RETURN ::AsSeconds = equalTo:AsSeconds
   CASE "N"
      RETURN ::AsSeconds = Int( equalTo )
   ENDSWITCH

   RETURN .F.

STATIC FUNCTION  TTIME_Op_Gt( gtThan )

   LOCAL Self := QSelf()

   SWITCH ValType( gtThan )
   CASE "O"
      RETURN ::AsSeconds > gtThan:AsSeconds
   CASE "N"
      RETURN ::AsSeconds > Int( gtThan )
   ENDSWITCH

   RETURN .F.

STATIC FUNCTION  TTIME_Op_GtEq( gtEqThan )

   LOCAL Self := QSelf()

   SWITCH ValType( gtEqThan )
   CASE "O"
      RETURN ::AsSeconds >= gtEqThan:AsSeconds
   CASE "N"
      RETURN ::AsSeconds >= Int( gtEqThan )
   ENDSWITCH

   RETURN .F.

STATIC FUNCTION  TTIME_Op_Mi( miThan )

   LOCAL Self := QSelf()

   SWITCH ValType( miThan )
   CASE "O"
      RETURN ::AsSeconds < miThan:AsSeconds
   CASE "N"
      RETURN ::AsSeconds < Int( miThan )
   ENDSWITCH

   RETURN .F.

STATIC FUNCTION  TTIME_Op_MiEq( miEqThan )

   LOCAL Self := QSelf()

   SWITCH ValType( miEqThan )
   CASE "O"
      RETURN ::AsSeconds <= miEqThan:AsSeconds
   CASE "N"
      RETURN ::AsSeconds <= Int( miEqThan )
   ENDSWITCH

   RETURN .F.

METHOD New( time, format ) CLASS TTime

   IF format != NIL
      ::FFormat := format
   ENDIF

   ::AsString := time

   RETURN Self

METHOD FUNCTION GetAsDays CLASS TTime
   RETURN ( ::FHours + ( ::FMinutes / 60 ) + ( ::FSeconds / 60 / 60 ) ) / 24

METHOD FUNCTION GetAsHours CLASS TTime
   RETURN ::FHours + ( ::FMinutes / 60 ) + ( ::FSeconds / 60 / 60 )

METHOD FUNCTION GetAsMinutes CLASS TTime
   RETURN ( ::FHours * 60 ) + ::FMinutes + ( ::FSeconds / 60 )

METHOD FUNCTION GetAsSeconds CLASS TTime
   RETURN ( ::FHours * 3600 ) + ( ::FMinutes * 60 ) + ::FSeconds

METHOD FUNCTION GetAsString( format ) CLASS TTime
    LOCAL asString := ""
    LOCAL numToken
    LOCAL i
    LOCAL s
    LOCAL tk
    LOCAL tkSep
    LOCAL itm
    LOCAL len
    LOCAL asSecs := ::GetAsSeconds()
    LOCAL newSeconds
    LOCAL FHours, FMinutes, FSeconds, FDecimals
    LOCAL maskLen := 0

    IF Empty( format )
        format := ::FFormat
    ENDIF

    numToken := NumToken( format, ":." )

    FOR i := 1 TO numToken
        tk := Token( format, ":.", i )
        tkSep := ":"
        itm := Upper( tk )
        len := Len( itm )
        SWITCH Left( itm, 1 )
        CASE "H"
            IF asSecs < 0
                s := Replicate( "*", len )
            ELSE
                s := StrZero( ::FHours, len )
            ENDIF
            EXIT
        CASE "M"
            IF asSecs < 0
                s := Replicate( "*", len )
            ELSE
                s := StrZero( ::FMinutes, len )
            ENDIF
            EXIT
        CASE "S"
            IF asSecs < 0
                s := Replicate( "*", len )
            ELSE
                s := StrZero( Int( ::FSeconds ), len )
            ENDIF
            EXIT
        CASE "N"
            maskLen := len
            IF asSecs < 0
                s := Replicate( "*", len )
            ELSE
                IF ( s := SubStr( Trans( ::FDecimals, "." + Replicate( "9", len ) ), 2 ) ) = "*"
                    newSeconds := Round( asSecs, maskLen )
                    EXIT
                ENDIF
            ENDIF
            tkSep := "."
            EXIT
        OTHERWISE
            s := NIL
        ENDSWITCH
        IF s != NIL
            asString += iif( i = 1, "", tkSep ) + s
        ENDIF
    NEXT

    IF newSeconds != NIL .OR. maskLen = 0 .AND. ::FDecimals > 0 .AND. Int( asSecs ) != Int( newSeconds := Round( asSecs, maskLen ) )
        FHours := ::FHours
        FMinutes := ::FMinutes
        FSeconds := ::FSeconds
        FDecimals := ::FDecimals
        ::SetAsSeconds( newSeconds )
        asString := ::GetAsString()
        ::FHours := FHours
        ::FMinutes := FMinutes
        ::FSeconds := FSeconds
        ::FDecimals := FDecimals
    ENDIF

    RETURN asString

/*
    Op_Add : ~ Format after "99:59:59" will return wrong string
*/
METHOD Op_Add( timeToAdd ) CLASS TTime

   LOCAL time := TTime():New()

   IF ValType( timeToAdd ) = "O"
      time:SetAsSeconds( ::AsSeconds + timeToAdd:AsSeconds )
   ELSE
      time:SetAsSeconds( ::AsSeconds + timeToAdd )
   ENDIF

   RETURN time

/*
    Op_Div
*/
METHOD Op_Div( timeToDiv ) CLASS TTime

   LOCAL time := TTime():New()

   time:SetAsSeconds( ::AsSeconds / timeToDiv:AsSeconds )

   RETURN time

/*
    Op_Minus : ~ if time to substract is > Self, then time will be set to 0
*/
METHOD Op_Minus( timeToMinus ) CLASS TTime

   LOCAL time := TTime():New()

   IF ValType( timeToMinus ) = "O"
      time:SetAsSeconds( Max( ::AsSeconds - timeToMinus:AsSeconds, 0 ) )
   ELSE
      time:SetAsSeconds( Max( ::AsSeconds - timeToMinus, 0 ) )
   ENDIF

   RETURN time

/*
    Op_Mult
*/
METHOD Op_Mult( timeToMult ) CLASS TTime

   LOCAL time := TTime():New()

   time:SetAsSeconds( ::AsSeconds * timeToMult:AsSeconds )

   RETURN time

METHOD PROCEDURE SetAsDays( days ) CLASS TTime

   ::SetAsSeconds( days * 86400 )

   RETURN

METHOD PROCEDURE SetAsHours( hours ) CLASS TTime

   ::SetAsSeconds( hours * 3600 )

   RETURN

METHOD PROCEDURE SetAsMinutes( minutes ) CLASS TTime

    ::SetAsSeconds( minutes * 60 )

   RETURN

METHOD PROCEDURE SetAsSeconds( seconds ) CLASS TTime

    ::FHours := Int( seconds / 3600 )
    ::FMinutes := Int( ( seconds % 3600 ) / 60 )
    ::FSeconds := seconds % 60
    IF seconds < 0
        ::FDecimals := - ( seconds - Int( seconds ) )
    ELSE
        ::FDecimals := seconds - Int( seconds )
    ENDIF

   RETURN

METHOD PROCEDURE SetAsString( time ) CLASS TTime

   LOCAL i
   LOCAL numToken
   LOCAL tk
   LOCAL h, m, s
   LOCAL changed
   LOCAL fail
   LOCAL bh, bm, bs, bd

   IF !Empty( time )
      numToken := NumToken( time, ":." )
      IF numToken > 0
         bh := ::FHours
         bm := ::FMinutes
         bs := ::FSeconds
         bd := ::FDecimals
         ::FHours := 0
         ::FMinutes := 0
         ::FSeconds := 0
         ::FDecimals := 0.0
         changed := .F.
         fail := .F.
         FOR i := 1 TO numToken
            tk := Token( ::FFormat, ":.", i )
            SWITCH Upper( Left( tk, 1 ) )
            CASE "H"
               h := Val( Token( time, ":.", i ) )
               IF h >= 0 .AND. h < 24
                  ::FHours := h
                  changed := .T.
               ELSE
                  fail := .T.
               ENDIF
               EXIT
            CASE "M"
               m := Val( Token( time, ":.", i ) )
               IF m >= 0 .AND. m < 60
                  ::FMinutes := m
                  changed := .T.
               ELSE
                  fail := .T.
               ENDIF
               EXIT
            CASE "S"
               s := Val( Token( time, ":.", i ) )
               IF s >= 0 .AND. s < 60
                  ::FSeconds += s
                  changed := .T.
               ELSE
                  fail := .T.
               ENDIF
               EXIT
            CASE "N"
               s := Val( "0." + Token( time, ":.", i ) )
               IF s != ::FDecimals
                  ::FSeconds += s
                  ::FDecimals := s
                  changed := .T.
               ELSE
                  fail := .T.
               ENDIF
               EXIT
            ENDSWITCH
         NEXT
         IF !changed .OR. fail
            ::FHours := bh
            ::FMinutes := bm
            ::FSeconds := bs
            ::FDecimals := bd
         ENDIF
      ENDIF
   ENDIF

   RETURN
