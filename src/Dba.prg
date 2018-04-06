/*
 *
 */

/*
    dba : database access
*/

#include "inkey.ch"
#include "dbinfo.ch"

THREAD STATIC aws := {}                  // stack de PushWS/PopWS

/*
    AddRec
*/
FUNCTION AddRec( nrec, index )

   LOCAL rn := RecNo()

   IF GetNextEmpty( index )
      nrec := RecNo()
      RETURN .T.
   ENDIF

   dbAppend( .F. )

   IF ! NetErr()
      nrec := RecNo()
      RETURN .T.
   ENDIF

   WHILE .T.

      IF GetNextEmpty()
         nrec := RecNo()
         RETURN .T.
      ENDIF

      dbAppend( .F. )

      IF ! NetErr()
         IF Deleted()
            dbRecall()
         ENDIF
         nrec := RecNo()
         RETURN .T.
      ENDIF

      dbSkip( 0 )
      dbGoto( rn )
      Inkey( .5 ) // ** Espera 1/2 segundo

      IF ui_Alert( "Si cancela, el Sistema puede no completar el proceso iniciado;" + ;
            "*;" + ;
            "Puede verificar que otro usuario, en otra estacion de trabajo;" + ;
            "no tenga detenido algun proceso inadvertidamente (o no).;" + ;
            "Tambien puede checar su red de computadoras...;" + ;
            ";;" + ;
            "De cualquier modo desea cancelar  ?", ;
            { " Si ", " No " } ) = 1
         nrec := 0
         RETURN .F. // ** ni modo...
      ENDIF

   ENDDO

   nrec := 0

   RETURN .F.         // Not locked

/*
    RecClear
*/
FUNCTION RecCLEAR()

   LOCAL i, a := dbStruct(), j

   j := Len( a )
   FOR i := 1 TO j
      DO CASE
      CASE a[ i, 2 ] $ "CM"
         // fieldput(i,iif(a[i,3]==8,replicate(chr(0),8),""))
         FieldPut( i, "" )
      CASE a[ i, 2 ] $ "NBI"
         FieldPut( i, 0 )
      CASE a[ i, 2 ] $ "D@"
         FieldPut( i, CToD( "" ) )
      CASE a[ i, 2 ] == "L"
         FieldPut( i, .F. )
      ENDCASE
   NEXT
   dbRecall()

   RETURN .T.

/*
    DbSkipX() : menos
    Teo. USA 1995
*/
FUNCTION DbSkipX( n, ord )

   THREAD STATIC bord, orden, ret

   ret := .T.
   bord := ordSetFocus()
   IF ord == NIL
      orden := ordSetFocus()
   ELSE
      orden := ord
   ENDIF
   ordSetFocus( orden )
   dbSkip( n )
   IF ( ( n == NIL .OR. n > 0 ) .AND. Eof() ) .OR. Bof()
      ret := .F.
   ENDIF
   ordSetFocus( bord )

   RETURN ret

/*
    existsKey() : Checa si existe key en indice dado
*/
FUNCTION existsKey( cKey, xOrder, nRec, bFor, bEval )

   LOCAL nr := RecNo()
   LOCAL recNo

   IF !( Seek( cKey, xOrder ) )
      dbGoto( nr )
      RETURN .F.
   ENDIF
   IF bFor == NIL .OR. bFor:eval
      IF Empty( nRec )
         IF !bEval == NIL
            bEval:Eval()
         ENDIF
         dbGoto( nr )
         RETURN .T.
      ENDIF
      IF HB_ISBLOCK( nRec )
         recNo := nRec:Eval()
      ELSE
         recNo := nRec
      ENDIF
      IF !RecNo() == recNo
         IF !bEval == NIL
            bEval:Eval()
         ENDIF
         dbGoto( nr )
         RETURN .T.
      ENDIF
   ENDIF
   dbGoto( nr )

   RETURN .F.

/*
    Get4Seek(cFieldname,xSeekvalue,nOrder,lSoftseek)
    Regresa 'xVar' (nombre de la variable) del registro en xSeek...
    Teo. USA 1995
*/
FUNCTION Get4Seek( xVar, xs, no, ss, cAlias )

   LOCAL ret
   LOCAL nrec

   nrec := RecNo()

   IF cAlias = NIL
      ( Seek( xs, no, ss ) )
   ELSE
      ( cAlias )->( Seek( xs, no, ss ) )
   ENDIF

   IF ValType( xVar ) == "B"
      ret := Eval( xVar )
      dbGoto( nrec )
      RETURN ret
   ELSE
      IF cAlias = NIL
         ret := Exec( xVar )
      ELSE
         ret := ( cAlias )->( Exec( xVar ) )
      ENDIF
   ENDIF

   dbGoto( nrec )

   RETURN iif( ValType( ret ) == "C" .AND. Len( ret ) == 8, ret, ret )

/*
    Get4SeekLast(cFieldname,xSeekvalue,nOrder,lSoftseek)
    Regresa 'var' (nombre de la variable) del registro en xSeek...
    Teo. USA 1995
*/
FUNCTION Get4SeekLast( var, xs, no, ss )

   LOCAL ret
   LOCAL nrec := RecNo()

   ( seeklast( xs, no, ss ) )
   IF ValType( var ) == "B"
      ret := var:Eval()
      dbGoto( nrec )
      RETURN ret
   ELSE
      ret := Eval( FieldBlock( var ) )
   ENDIF
   dbGoto( nrec )
   // RETURN iif(valtype(ret)=="C" .AND. len(ret)==8,ret,ret)

   RETURN ret

/*
    GetNextEmpty
*/
STATIC FUNCTION GetNextEmpty( index )

   LOCAL rec := RecNo(), key

   IF index == NIL
      IF ordNumber( "Primary" ) > 0
         index := "Primary"
      ELSEIF ordNumber( "X01" ) > 0
         index := "X01"
      ELSE
         index := ordName()
      ENDIF
   ENDIF

   DbGoTopX( index )

   IF ValType( key := KeyVal( index ) ) != "C"
      dbGoto( rec )
      RETURN .F.
   ENDIF

   ( Seek( Space( Len( key ) ), index ) )

   WHILE !Eof() .AND. Empty( KeyVal( index ) )
      IF IsLocked()
         ( DbSkipX( 1, index ) )
         LOOP
      ENDIF
      dbRLock( RecNo() )
      // rlock()
      ( DbSkipX( 0, index ) )
      IF dbRLock( RecNo() ) .AND. Empty( KeyVal( index ) )
         // IF rlock() .AND. empty(KeyVal(index))
         RecClear()
         RETURN .T.
      ELSE
         dbRUnlock( RecNo() )
      ENDIF
      ( DbSkipX( 1, index ) )
   ENDDO

   dbGoto( rec )

   RETURN .F.

/*
    DbGoBottom
*/
FUNCTION DbGoBottomX( ord )

   LOCAL bord

   IF ord == NIL
      dbGoBottom()
      RETURN NIL
   ENDIF
   bord := ordSetFocus()
   ordSetFocus( ord )
   dbGoBottom()
   ordSetFocus( bord )

   RETURN NIL

/*
    DbGoTopX
*/
FUNCTION DbGoTopX( ord )

   LOCAL bord

   IF ord == NIL
      dbGoTop()
      RETURN NIL
   ENDIF
   bord := ordSetFocus()
   ordSetFocus( ord )
   dbGoTop()
   ordSetFocus( bord )

   RETURN NIL

/*
    IsLocked
*/
FUNCTION IsLocked( recNo )
   RETURN dbRecordInfo( DBRI_LOCKED, recNo )

/*
    KeyVal() : regresa la expresion del indice (necesario para indicar index)
    Teo. USA 1995
*/
FUNCTION KeyVal( ord )

   LOCAL ret
   LOCAL oo

   IF ord == NIL
      RETURN ordKeyVal()
   ENDIF
   oo := ordSetFocus()
   ordSetFocus( ord )
   ret := ordKeyVal()
   ordSetFocus( oo )

   RETURN ret

/*
    PopWS
*/
FUNCTION PopWS( n )

   LOCAL ws, a, wa

   n := iif( n == NIL, Len( aws ), n )
   IF n <= 0 .OR. n > Len( aws )
      // mess_open("!","ERROR: PopWS Fuera de rango !!!",-1,C_ERROR,,0)
      RETURN 0
   ENDIF
   ws := Alias()
   IF !Empty( wa := aws[ n ] )
      a := wa[ 1 ]
      dbSelectArea( a )
      IF !Empty( Alias() )
         ordSetFocus( wa[ 2 ] )
         dbGoto( wa[ 3 ] )
         IF !Empty( wa[ 2 ] )
            ordDescend(,, wa[ 4 ] )
         ENDIF
      ENDIF
   ELSE
      a := ""
   ENDIF
   IF !Empty( ws )
      dbSelectArea( ws )
   ENDIF
   ADel( aws, n )
   ASize( aws, Len( aws ) -1 )

   RETURN a

/*
    PushWS
*/
FUNCTION PushWS( p )

   LOCAL n

   IF p != NIL
      RETURN Len( aws )
   ENDIF
   IF !Alias() == ""
      n := { Alias(), ordSetFocus(), RecNo(), ordDescend() }
   ENDIF
   AAdd( aws, n )

   RETURN n

/*
    RecLock
*/
FUNCTION RecLock( nr, n )
   LOCAL intentos

   nr := iif( nr == NIL, RecNo(), nr )
   n := nr

   IF nr > LastRec()
      // mess_open("!","Locking error: End Of File...",-1,C_ERROR,,0)
      ui_Alert( "Locking error: End of file..." )
      RETURN .F.
   ENDIF
   IF IsLocked( nr )
      nr := n := -1
      RETURN .T.
   ENDIF
   IF dbRLock( n )
      RETURN .T.        // Locked
   ENDIF

   intentos := 4

   WHILE !dbRLock( n )
      dbSkip( 0 )
      dbGoto( n )
      Inkey( .5 ) // ** Espera 1/2 segundo
      IF intentos-- > 0
         LOOP
      ENDIF
      IF ui_Alert( "Si cancela, el Sistema puede no completar el proceso iniciado;" + ;
            "*;" + ;
            "Puede verificar que otro usuario, en otra estacion de trabajo;" + ;
            "no tenga detenido algun proceso inadvertidamente (o no).;" + ;
            "Tambien puede checar su red de computadoras...;" + ;
            ";;" + ;
            "De cualquier modo desea cancelar  ?", ;
            { " Si ", " No " } ) = 1
         RETURN .F. // ** ni modo...
      ENDIF
   ENDDO

   RETURN .T. // ** Agueso debe salir con registro bloqueado

/*
    RecUnLock
*/
FUNCTION RecUnLock( n )

   IF ValType( n ) == "A"
      AEval( n, {| x, i| iif( ValType( x ) == "A", RecUnLock( x ), iif( ValType( n[ i ] ) == "N", RecUnLock( n[ i ] ), NIL ) ) } )
      RETURN NIL
   ENDIF
   dbRUnlock( iif( n == NIL, n := RecNo(), n ) )
   dbSkip( 0 )

   RETURN .T.

/*
    Seek() : no requiere presentacion
    Teo. USA 1995
*/
FUNCTION SEEK( expr, ord, ss )

   THREAD STATIC ret
   THREAD STATIC bord

   IF ord == NIL
      RETURN dbSeek( expr, ss )
   ENDIF
   bord := ordSetFocus()
   ordSetFocus( ord )
   ret := dbSeek( expr, ss )
   ordSetFocus( bord )

   RETURN ret

/*
    SeekLast() : tampoco
    Teo. USA 1995
*/
FUNCTION SeekLast( expr, ord, ss )

   THREAD STATIC ret
   THREAD STATIC bord

   IF ord == NIL
      RETURN dbSeek( expr, ss, .T. )
   ENDIF

   bord := ordSetFocus()
   ordSetFocus( ord )
   ret := dbSeek( expr, ss, .T. )
   ordSetFocus( bord )

   RETURN ret
