/*
 *
 */

THREAD STATIC base36Array := "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"

THREAD STATIC base64Array := "0123456789<>ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

THREAD STATIC __S_UI_ALERT_DELAY

/*
    __ClsInstName
*/
FUNCTION __ClsInstName( className )

   LOCAL Result

   Result := &( className + "()" )

   RETURN Result

/*
    AsCodeBlock : Convierte la cadena a un code block
*/
FUNCTION AsCodeBlock( cStr, xDefaultVal )

   IF Empty( cStr )
      RETURN xDefaultVal
   ENDIF

   RETURN &( "{||" + cStr + "}" )

/*
    AsDate
*/
FUNCTION AsDate( xVal, sTemplate )

   LOCAL dVal := CToD( "" )
   LOCAL cType := ValType( xVal )

   DO CASE
   CASE cType = "C"
      dVal := Str2Date( xVal, sTemplate )
   CASE cType = "D"
      dVal := xVal
   ENDCASE

   RETURN dVal

/*
    AsLogical
*/
FUNCTION AsLogical( xVal )

   SWITCH ValType( xVal )
   CASE 'C'
      RETURN AScan( { ".T.", "TRUE", "YES", "SI", "1", "ON" }, {| e| Upper( xVal ) == e } ) > 0
   CASE 'N'
      RETURN xVal > 0
   END

   RETURN .F.

/*
    AsNumeric
*/
FUNCTION AsNumeric( xVal )

   SWITCH ValType( xVal )
   CASE 'C'
      RETURN Val( xVal )
   CASE 'N'
      RETURN xVal
   END

   RETURN 0

/*
    AsString
*/
FUNCTION AsString( xVal )

   LOCAL result

   SWITCH ValType( xVal )
   CASE 'C'
      RETURN xVal
   CASE 'D'
      // RETURN FDateS( xVal )
      IF Empty( xVal )
         RETURN "  /   /    "
      ENDIF
      RETURN Str( Day( xVal ), 2 ) + "/" + Left( CMonth( xVal ), 3 ) + "/" + Str( Year( xVal ), 4 )
   CASE 'B'
      RETURN "{|| ... }"
   CASE 'L'
      RETURN iif( xVal, ".T.", ".F." )
   CASE 'N'
      RETURN LTrim( Str( xVal ) )
   CASE 'O'
      BEGIN SEQUENCE WITH {| oErr| Break( oErr ) }
         IF xVal:IsDerivedFrom( "TTime" )
            result := xVal:AsString
         ELSE
            result := "<Object>: " + xVal:ClassName
         ENDIF
      RECOVER
         result := "<Object Not Valid>"
      END SEQUENCE
      RETURN result
   CASE 'A'
      RETURN "<Array>: " + LTrim( Str( Len( xVal ) ) )
   CASE 'T'
      RETURN hb_tToC( xVal )
   ENDSWITCH

   RETURN ""

/*
    Base2N() : Convierte string de base numerica a numero en base 10
    Teo. USA 1995
*/
FUNCTION Base2N( sBase, nBase, l, cFill )

   THREAD STATIC dec
   THREAD STATIC i, c, n, s, lb

   dec := 0
   n := 1
   lb := Len( sBase := Upper( AllTrim( sBase ) ) )
   s := Left( base36Array, nBase )
   FOR i := lb TO 1 STEP -1
      IF ( c := At( SubStr( sBase, i, 1 ), s ) ) == 0
         RETURN iif( l == NIL, 0, N2Base( 0, 10, l, cFill ) )  // Regresa un cero...
      ENDIF
      dec += ( c - 1 ) * n
      n *= nBase
   NEXT

   RETURN iif( l == NIL, dec, N2Base( dec, 10, l, cFill ) )

/*
    base64ToN() : Convierte string de base 64 a numero en base 10
    Teo. México 2015
*/
FUNCTION base64ToN( sBase )

   THREAD STATIC dec
   THREAD STATIC i, c, n, lb

   dec := 0
   n := 1
   lb := Len( sBase := AllTrim( sBase ) )

   FOR i := lb TO 1 STEP -1
      IF ( c := At( SubStr( sBase, i, 1 ), base64Array ) ) == 0
         RETURN 0 /* character not valid */
      ENDIF
      dec += ( c - 1 ) * n
      n *= 64
   NEXT

   RETURN dec

/*
    Dec
*/
FUNCTION Dec( p, base, fill, n )

   LOCAL s, i, b, c

   IF n == NIL
      n := 1
   ENDIF

   SWITCH ValType( p )
   CASE 'C'
      IF Len( p ) == 1 .AND. Empty( base )
         RETURN Chr( Asc( p ) -n )
      ENDIF
      iif( Empty( base ), base := 36, base )
      IF base < 2 .OR. base > 36
         RETURN "*"
      ENDIF
      s := CharMirr( Upper( p ) )
      // s := Upper( p )
      b := Left( base36Array, base ) + " "
      FOR i := 1 TO Len( s )
         iif( ( c := SubStr( s, i, 1 ) ) == " ", b := " ", NIL )
         IF !( c $ b )
            EXIT
         ENDIF
      NEXT
      RETURN Left( p, Len( p ) -i + 1 ) + N2Base( Base2N( Right( p, i - 1 ), base ) -n, base, i - 1, fill )
   CASE 'N'
      p -= n
      EXIT
   CASE 'D'
      IF Empty( p )
         p := Date()
      ENDIF
      p -= n
      EXIT
   CASE 'A'
      IF !Empty( p )
         ASize( p, Len( p ) -n )
      ENDIF
   OTHERWISE
      // mess_open("ERROR","* Dec() *",-1,C_ERROR)
   ENDSWITCH

   RETURN p

/*
    Exec : Evalua la cadena y regresa el resultado
*/
FUNCTION Exec( cStr, xDefaultVal )

   LOCAL xRet

   BEGIN SEQUENCE // WITH {|e| ArelErrorHandle(e) }

      IF ValType( cStr ) = "B"

         xRet := Eval( cStr )

      ELSE

         xRet := AsCodeBlock( cStr, xDefaultVal ):Eval()

      ENDIF

   RECOVER

      xRet := xDefaultVal

   END SEQUENCE

   RETURN xRet

/*
    FDate2F() : Regresa un valor DATE como cadena en formato:
                            MM-DD-YYYY, D$-M$-Y$ ... D$ DD de M$ de Y$
*/
FUNCTION FDate2F( d, p )

   LOCAL i, n, s

   IF ValType( d ) == "C"
      IF Len( d ) == 6
         d += "01"
      ENDIF
      d := stoj( d )
   ENDIF
   IF Empty( iif( d == NIL, d := Date(), d ) ) .OR. DToS( d ) = "000000" .OR. p == NIL .OR. ( n := numtoken( p ) ) == 0
      RETURN ""
   ENDIF
   FOR i := 1 TO n
      s := Upper( token( p,, i ) )
      DO CASE
      CASE s == "DD" .OR. s == "D$"
         // p:=stuff(p,attoken(p,,i),2,iif(s=="D$",fdia(d),str(day(d),2)))
         p := SwapToken( p,, i, iif( s == "D$", NToCDOW( DoW( d ) ), Str( Day( d ), 2 ) ) )
      CASE s == "DDD"
         // p:=stuff(p,attoken(p,,i),3,left(fdia(d),3))
         p := SwapToken( p,, i, Left( NToCDOW( DoW( d ) ), 3 ) )
      CASE s == "MM"
         // p:=stuff(p,attoken(p,,i),2,str(month(d),2))
         p := SwapToken( p,, i, Str( Month( d ), 2 ) )
      CASE s == "M$"
         // p:=stuff(p,attoken(p,,i),2,fmes(d))
         p := SwapToken( p,, i, CMonth( d ) )
      CASE s == "MMM"
         // p:=stuff(p,attoken(p,,i),3,left(fmes(d),3))
         p := SwapToken( p,, i, Left( CMonth( d ), 3 ) )
      CASE s == "YY"
         // p:=stuff(p,attoken(p,,i),2,substr(str(year(d),4),3,2))
         p := SwapToken( p,, i, SubStr( Str( Year( d ), 4 ), 3, 2 ) )
      CASE s == "Y$" .OR. s == "YYYY"
         // p:=stuff(p,attoken(p,,i),len(s),hb_nToS(year(d)))
         p := SwapToken( p,, i, LTrim( Str( Year( d ) ) ) )
      ENDCASE
   NEXT

   RETURN p

/*
    FDateS
*/
FUNCTION FDateS( d )

   IF Empty( iif( d == NIL, d := Date(), d ) ) .OR. Empty( d ) .OR. DToS( d ) = "000000"
      RETURN "  /   /    "
   ENDIF

   RETURN Str( Day( d ), 2 ) + "/" + Left( CMonth( d ), 3 ) + "/" + Str( Year( d ), 4 )

/*
    Inc
*/
FUNCTION Inc( p, base, fill, n )

   LOCAL s, i, b, c

   IF n == NIL
      n := 1
   ENDIF

   SWITCH ValType( p )
   CASE 'C'
      IF Len( p ) == 1 .AND. Empty( base )
         RETURN Chr( Asc( p ) + n )
      ENDIF
      iif( Empty( base ), base := 36, base )
      IF base < 2 .OR. base > 36
         RETURN "*"
      ENDIF
      s := CharMirr( Upper( p ) )
      // s := Upper( p )
      b := Left( base36Array, base ) + " "
      FOR i := 1 TO Len( s )
         iif( ( c := SubStr( s, i, 1 ) ) == " ", b := " ", NIL )
         IF !( c $ b )
            EXIT
         ENDIF
      NEXT
      RETURN Left( p, Len( p ) -i + 1 ) + N2Base( Base2N( Right( p, i - 1 ), base ) + n, base, i - 1, fill )
   CASE 'N'
      p += n
      EXIT
   CASE 'D'
      IF Empty( p )
         p := Date()
      ENDIF
      p += n
      EXIT
   CASE 'A'
      ASize( p, Len( p ) + n )
      EXIT
   OTHERWISE
      // mess_open("ERROR","* Inc() *",-1,C_ERROR)
   ENDSWITCH

   RETURN p

/*
    MyErrorNew
*/
FUNCTION MyErrorNew( SubSystem, Operation, Description, Args, ProcFile, ProcName, ProcLine )

   LOCAL oErr

   oErr := ErrorNew()
   oErr:SubSystem := iif( SubSystem = NIL, "", SubSystem )
   oErr:Operation := iif( Operation = NIL, "", Operation )
   oErr:Description := Description
   oErr:Args := Args
   oErr:Cargo := HB_HSetAutoAdd( { "ProcFile" => ProcFile, "ProcName" => ProcName, "ProcLine" => ProcLine }, .T. )

   RETURN oErr

/*
    nToBase64() : Convierte numero a string de base numerica 64
    Teo. México 2015
*/
FUNCTION nToBase64( nVal, l, cFill )

   THREAD STATIC sBase
   THREAD STATIC n

   sBase := ""
   n := 1
   iif( cFill == NIL, cFill := "0", NIL )
   WHILE .T.
      n *= 64
      IF n > nVal
         EXIT
      ENDIF
   ENDDO
   WHILE n != 1
      n /= 64
      sBase += SubStr( base64Array, nVal / n + 1, 1 )
      nVal %= n
   ENDDO
   iif( l == NIL, l := Len( sBase ), NIL )

   RETURN iif( Len( sBase ) > l, Replicate( "*", l ), PadL( sBase, l, cFill ) )


/*
    N2Base() : Convierte numero a string de base numerica 'n'
    Teo. USA 1995
*/
FUNCTION N2Base( nVal, nBase, l, cFill )

   THREAD STATIC sBase
   THREAD STATIC n

   sBase := ""
   n := 1
   iif( cFill == NIL, cFill := "0", NIL )
   WHILE .T.
      n *= nBase
      IF n > nVal
         EXIT
      ENDIF
   ENDDO
   WHILE n != 1
      n /= nBase
      sBase += SubStr( base36Array, nVal / n + 1, 1 )
      nVal %= n
   ENDDO
   iif( l == NIL, l := Len( sBase ), NIL )

   RETURN iif( Len( sBase ) > l, Replicate( "*", l ), PadL( sBase, l, cFill ) )


FUNCTION SET_UI_ALERT_DELAY( delay )
    LOCAL ret := __S_UI_ALERT_DELAY

    IF PCount() > 0
        __S_UI_ALERT_DELAY := delay
    ENDIF

RETURN ret

/*
    SToJ
*/
FUNCTION SToJ( cDate )
   RETURN Str2Date( cDate, "YYYYMMDD" )

/*
    Str2Date : convierte una fecha en string a tipo date
    Teo. USA 1995
*/
FUNCTION Str2Date( sDate, sTemplate, sReturn )

   LOCAL sDay
   LOCAL tMonth, sMonth, nMonth
   LOCAL sYear
   LOCAL s, i, j
   LOCAL d

   IF Empty( sDate )
      RETURN CToD( "" )
   ENDIF

   IF Empty( sTemplate )
      sTemplate := "DD MMM YYYY"
   ENDIF

   sDay := sMonth := sYear := ""

   sDate := Upper( sDate )
   sTemplate := Upper( sTemplate )

   // Hay separadores...
   IF ( j := NumToken( sTemplate ) ) > 1
      FOR i := 1 TO j
         s := Token( sTemplate,, i )
         DO CASE
         CASE s = "D"
            sDay := Token( sDate,, i )
         CASE s = "M"
            tMonth := s
            sMonth := Token( sDate,, i )
         CASE s = "Y"
            sYear := Token( sDate,, i )
         ENDCASE
      NEXT
   ELSE

      IF ( i := At( "YYYY", sTemplate ) ) > 0
         sYear := SubStr( sDate, i, 4 )
      ELSEIF ( i := At( "YY", sTemplate ) ) > 0
         sYear := SubStr( sDate, i, 2 )
      ENDIF

      IF ( i := At( "MMM", sTemplate ) ) > 0
         tMonth := "MMM"
         sMonth := SubStr( sDate, i, 3 )
      ELSEIF ( i := At( "MM", sTemplate ) ) > 0
         tMonth := "MM"
         sMonth := SubStr( sDate, i, 2 )
      ENDIF

      IF ( i := At( "DD", sTemplate ) ) > 0
         sDay := SubStr( sDate, i, 2 )
      ENDIF

   ENDIF


   IF ( tMonth == "M$" .OR. tMonth == "MMM" ) .AND. Len( sMonth ) >= 3
      FOR i := 1 TO 12
         IF Upper( NToCMonth( i ) ) = sMonth
            nMonth := i
            EXIT
         ENDIF
         IF Upper( NToCMonth( i ) ) = sMonth
            nMonth := i
            EXIT
         ENDIF
      NEXT
   ELSE
      nMonth := Val( sMonth )
   ENDIF

   IF Empty( nMonth )
      d := CToD( "" )
   ELSE
      d := CToD( iif( Empty( sDay ), "1", sDay ) + "/" + LTrim( Str( nMonth ) ) + "/" + sYear )
   ENDIF

   IF !Empty( sReturn )
      DO CASE
      CASE Empty( d )
         RETURN Space( Len( sReturn ) )
      CASE sReturn == "MMDD"
         RETURN Base2N( LTrim( Str( Month( d ) ) ), 10, 2 ) + Base2N( LTrim( Str( Day( d ) ) ), 10, 2 )
      CASE sReturn == "DDMMM"
         RETURN Str( Day( d ), 2 ) + Left( CMonth( d ), 3 )
      CASE sReturn == "YYYYMM"
         RETURN Base2N( LTrim( Str( Year( d ) ) ), 10, 4 ) + Base2N( LTrim( Str( Month( d ) ) ), 10, 2 )
      CASE sReturn == "MMMYYYY"
         RETURN Left( CMonth( d ), 3 ) + Str( Year( d ), 4 )
      OTHERWISE
         RETURN fdate2f( d, sReturn )
      ENDCASE
   ENDIF

   RETURN d

/*
    SwapToken :
*/
FUNCTION SwapToken( cString, cDelims, nOccur, cSwapStr )

   LOCAL n := 0

   TokenInit( cString, cDelims )

   WHILE ( !TokenEnd() )
      n++
      IF n = nOccur
         cString := Stuff( cString, TokenAt(), TokenAt( .T. ) - TokenAt(), cSwapStr )
         EXIT
      ENDIF
      TokenNext( cString )
   ENDDO

   TokenExit()

   RETURN cString


/*
    ui_ShowError
*/
FUNCTION ui_ShowError( errObj )

   LOCAL result

   IF .T. // Empty(__DynSN2Sym("wxhShowError"))
      result := hb_Alert( errObj:Description, NIL, NIL, SET_UI_ALERT_DELAY() )
   ELSE
      result := __dynsN2Sym( "wxhShowError" ):Exec( NIL,, errObj )
   ENDIF

   RETURN result

/*
    ui_Alert
*/
FUNCTION ui_Alert( ... )

   LOCAL result
   LOCAL a

   IF .T. // Empty(__DynSN2Sym("wxhAlert"))
      a := ASize( hb_aParams(), 3 )
      result := hb_Alert( a[ 1 ], a[ 2 ], a[ 3 ], SET_UI_ALERT_DELAY() )
   ELSE
      result := __dynsN2Sym( "wxhAlert" ):Exec( ... )
   ENDIF

   RETURN result

/*
    ui_AlertYesNo
*/
FUNCTION ui_AlertYesNo( msgWarn )

   LOCAL result

   IF .T. // Empty(__DynSN2Sym("wxhAlertYesNo"))
      result := hb_Alert( msgWarn, { "Yes", "No" }, NIL, SET_UI_ALERT_DELAY() )
   ELSE
      result := __dynsN2Sym( "wxhAlertYesNo" ):Exec( msgWarn )
   ENDIF

   RETURN result
