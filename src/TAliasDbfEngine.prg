/*
 *
 */

/*
    TAlias
*/

#include "oordb.ch"
#include "xerror.ch"

REQUEST HB_FNAMENAME

THREAD STATIC __S_Instances

CLASS TAlias FROM OORDBBASE

PROTECTED:

   DATA FRecNo
   DATA FStack    INIT {}
   DATA FStackLen INIT 0
   DATA FthreadId
   DATA FfullFileName
   METHOD GetRecNo INLINE ::SyncFromRecNo(), ::FRecNo
   METHOD SetRecNo( RecNo ) INLINE ::dbGoto( RecNo )

   DATA FstackLock INIT {}
   METHOD GetAliasName() INLINE __S_Instances[ ::FfullFileName, "aliasName" ]
   METHOD setWorkArea( fullFileName, keepOpen )

PUBLIC:

   DATA RddDriver
   DATA lReadOnly INIT .F.
   DATA lShared INIT .T.

   CLASSDATA keepOpen INIT .F.

   CONSTRUCTOR New( table, aliasName )
   DESTRUCTOR onDestructor()

   METHOD __dbZap()
   METHOD AddRec( index )
   METHOD dbCloseArea()
   METHOD dbDelete()
   METHOD dbGoBottom( indexName )
   METHOD dbGoto( RecNo )
   METHOD dbGoTop( indexName )
   METHOD dbInfo( ... )
   METHOD DbOpen( table, aliasName )
   METHOD dbOrderInfo( ... )
   METHOD dbRecall()
   METHOD dbRLock( recNo ) INLINE ::recLock( recNo, noRetry )
   METHOD dbSkip( nRecords, indexName )
   METHOD dbStruct()
   METHOD dbUnlock() INLINE ::FstackLock := {}, ( ::workArea )->( dbUnlock() )
   METHOD Deleted()
   METHOD Eval( codeBlock, ... )
   METHOD existsKey( KeyValue, IndexName, RecNo )
   METHOD FCount INLINE ( ::workArea )->( FCount() )
   METHOD fieldGet(nPos) INLINE (::workarea)->(fieldGet(nPos))
   METHOD FieldPos( FieldName ) INLINE ( ::workArea )->( FieldPos( FieldName ) )
   METHOD fieldValue( fieldName, value ) BLOCK ;
      {|self,fieldName,value|
         IF pCount() > 2
            ::fieldValueSet( fieldName, value )
         ELSE
            value := ::fieldValueGet( fieldName )
         ENDIF
         RETURN value
      }
   METHOD fieldValueGet( fieldName )
   METHOD fieldValueSet( fieldName, value )
   METHOD FLock() INLINE ( ::workArea )->( FLock() )
   METHOD Get4Seek( xVal, keyVal, indexName, softSeek )
   METHOD Get4SeekLast( xVal, keyVal, indexName, softSeek )
   METHOD IsLocked( RecNo )
   METHOD KeyVal( indexName )
   METHOD LastRec INLINE ( ::workArea )->( LastRec() )
   METHOD ordCondSet( ... )
   METHOD ordCreate( ... )
   METHOD ordCustom( Name, cBag, KeyVal )
   METHOD ordDescend( Name, cBag, lDescend )
   METHOD ordDestroy( tagName, bagName )
   METHOD ordKeyAdd( Name, cBag, KeyVal )
   METHOD ordKeyDel( Name, cBag, KeyVal )
   METHOD ordKeyNo( ... )
   METHOD ordKeyVal()
   METHOD ordNumber( ordName, ordBagName )
   METHOD ordSetFocus( Name, cBag )
   METHOD Pop()
   METHOD Push()
   METHOD RawGet4Seek( direction, xVal, keyVal, indexName, softSeek )
   METHOD recClear() INLINE ( ::workArea )->( recClear() )
   METHOD RecCount INLINE ( ::workArea )->( RecCount() )
   METHOD RecLock( recNo, lNoRetry )
   METHOD RecUnLock( RecNo )
   METHOD Seek( cKey, indexName, softSeek )
   METHOD SeekLast( cKey, indexName, softSeek )
   METHOD SyncFromDataEngine
   METHOD SyncFromRecNo

   METHOD validateDbStruct(table)

   MESSAGE DbSeek METHOD SEEK

    /*!
     * needed for tdbrowse.prg (oDBE:Alias)
     */
   PROPERTY ALIAS READ GetAliasName
   METHOD Instances INLINE __S_Instances

   PROPERTY workArea

PUBLISHED:

   PROPERTY Bof   INIT .T.
   PROPERTY Eof   INIT .T.
   PROPERTY Found INIT .F.
   PROPERTY Name READ GetAliasName
   PROPERTY RecNo READ GetRecNo WRITE SetRecNo

ENDCLASS

/*
    New
*/
METHOD New( table, aliasName ) CLASS TAlias
   LOCAL fullFileName

   IF __S_Instances = nil
      __S_Instances := HB_HSetCaseMatch( { => }, .F. )
   ENDIF

   IF Empty( table )
      RAISE ERROR "TAlias: Empty Table parameter."
   ENDIF

   IF HB_ISOBJECT( table )

      fullFileName := table:fullFileName

      IF Empty( fullFileName )
         RAISE ERROR "TAlias: Empty Table Name..."
      ENDIF

   ELSE

      fullFileName := table

   ENDIF

   IF !::DbOpen( table, aliasName )
      // RAISE ERROR "TAlias: Cannot Open Table '" + table:TableFileName + "'"
      Break( "TAlias: Cannot Open Table '" + fullFileName + "'" )
   ENDIF

   ::SyncFromDataEngine()

   RETURN Self

/*
    onDestructor
*/
METHOD PROCEDURE onDestructor() CLASS TAlias

    IF __S_Instances != nil .AND. ::FthreadId == hb_threadId()
        ::dbCloseArea()
    ENDIF

RETURN

/*
    __DbZap
*/
METHOD FUNCTION __dbZap() CLASS TAlias
   RETURN ( ::workArea )->( __dbZap() )

/*
    AddRec
*/
METHOD FUNCTION AddRec( index ) CLASS TAlias

   LOCAL Result

   Result := ( ::workArea )->( AddRec(, index ) )
   ::SyncFromDataEngine()

   RETURN Result

/*
    DbCloseArea
*/
METHOD PROCEDURE dbCloseArea() CLASS TAlias

    IF ::FfullFileName != nil
        IF hb_HHasKey( __S_Instances, ::FfullFileName )
            __S_Instances[ ::FfullFileName ]["counter"] -= 1
            IF __S_Instances[ ::FfullFileName ]["counter"] = 0
                IF ( ::workarea )->( select() ) > 0 .AND. ! __S_Instances[ ::FfullFileName ]["keepOpen"]
                    ( ::workArea )->( dbCloseArea() )
                ENDIF
                hb_hDel( __S_Instances, ::FfullFileName )
            ENDIF
        ENDIF
    ENDIF

RETURN

/*
    DbDelete
*/
METHOD PROCEDURE dbDelete() CLASS TAlias

   ::SyncFromRecNo()
   ( ::workArea )->( dbDelete() )

   RETURN

/*
    DbGoBottom
*/
METHOD FUNCTION dbGoBottom( indexName ) CLASS TAlias

   LOCAL Result

   IF Empty( indexName )
      Result := ( ::workArea )->( dbGoBottom() )
   ELSE
      Result := ( ::workArea )->( DbGoBottomX( indexName ) )
   ENDIF
   ::SyncFromDataEngine()

   RETURN Result

/*
    DbGoTo
*/
METHOD FUNCTION dbGoto( RecNo ) CLASS TAlias

   LOCAL Result

   Result := ( ::workArea )->( dbGoto( RecNo ) )
   ::SyncFromDataEngine()

   RETURN Result

/*
    DbGoTop
*/
METHOD FUNCTION dbGoTop( indexName ) CLASS TAlias

   LOCAL Result

   IF Empty( indexName )
      Result := ( ::workArea )->( dbGoTop() )
   ELSE
      Result := ( ::workArea )->( DbGoTopX( indexName ) )
   ENDIF
   ::SyncFromDataEngine()

   RETURN Result

/*
    DbInfo
*/
METHOD FUNCTION dbInfo( ... ) CLASS TAlias

RETURN ( ::workArea )->( dbInfo( ... ) )

/*
    DbOpen
*/
METHOD FUNCTION DbOpen( table, aliasName ) CLASS TAlias
   LOCAL fullFileName
   LOCAL wa
   LOCAL result := .F.

   wa := Alias()

   IF hb_isObject( table )
      /* Check for a previously open workarea */
      IF ! hb_HHasKey( __S_Instances, table:fullFileName )
         IF table:IsTempTable
            IF table:CreateTable()
               fullFileName := table:fullFileName
               ::lShared := .F.
            ENDIF
         ELSE
            fullFileName := table:fullFileName
         ENDIF
      ELSE
         fullFileName := table:fullFileName
      ENDIF
   ELSE
      fullFileName := table
   ENDIF

   IF ! empty( fullFileName )

      IF hb_hHasKey( __S_Instances, fullFileName )

         ::setWorkArea( fullFileName )

         result := ( ::workarea )->( select() ) > 0

      ELSE

         IF ! hb_dbExists( fullFileName )
            IF ! hb_isObject( table ) .OR. ! table:AutoCreate .OR. ! table:CreateTable( fullFileName )
               Break( "TAlias: Cannot Create Table '" + fullFileName + "'" )
            ENDIF
         ENDIF

         IF aliasName = nil
            aliasName := hb_fNameName( fullFileName )
            SWITCH token( upper( aliasName ), ":", 1 )
            CASE "MEM"
               aliasName := subStr( aliasName, 5 )
               EXIT
            ENDSWITCH
         ENDIF

         /* checks if alias hasn't been opened yet */
         IF ( aliasName )->( select() ) = 0
            dbUseArea( .T., ::RddDriver, fullFileName, aliasName, ::lShared, ::lReadOnly )
            result := !NetErr()
            ::setWorkArea( fullFileName )
         ELSE
            /* alias has been opened already, mark it as keep open when this obj is destroyed */
            dbSelectArea( aliasName )
            ::setWorkArea( fullFileName, .T. )
            result := .T.
         ENDIF

      ENDIF

   ENDIF

   IF !Empty( wa )
      dbSelectArea( wa )
   ENDIF

   RETURN result

/*
    DbOrderInfo
*/
METHOD FUNCTION dbOrderInfo( ... ) CLASS TAlias

RETURN ( ::workArea )->( dbOrderInfo( ... ) )

/*
    DbRecall
*/
METHOD PROCEDURE dbRecall() CLASS TAlias

   ::SyncFromRecNo()
   ( ::workArea )->( dbRecall() )

   RETURN

/*
    DbSkip
*/
METHOD FUNCTION dbSkip( nRecords, indexName ) CLASS TAlias

   LOCAL Result

   ::SyncFromRecNo()

   IF Empty( indexName )
      Result := ( ::workArea )->( dbSkip( nRecords ) )
   ELSE
      Result := ( ::workArea )->( DbSkipX( nRecords, indexName ) )
   ENDIF

   ::SyncFromDataEngine()

   RETURN Result

/*
    DbStruct
*/
METHOD FUNCTION dbStruct() CLASS TAlias
   RETURN ( ::workArea )->( dbStruct() )

/*
    Deleted
*/
METHOD FUNCTION Deleted() CLASS TAlias

   ::SyncFromRecNo()

   RETURN ( ::workArea )->( Deleted() )

/*
    Eval
*/
METHOD FUNCTION Eval( codeBlock, ... ) CLASS TAlias

   ::SyncFromRecNo()

   RETURN ( ::workArea )->( codeBlock:Eval( ... ) )

/*
    existsKey
*/
METHOD FUNCTION existsKey( KeyValue, IndexName, RecNo ) CLASS TAlias
   RETURN ( ::workArea )->( existsKey( KeyValue, IndexName, RecNo ) )

/*
    fieldValueGet
*/
METHOD FUNCTION fieldValueGet( fieldName ) CLASS TAlias

   ::SyncFromRecNo()

   RETURN ( ::workArea )->( FieldGet( FieldPos( fieldName ) ) )

/*
    fieldValueSet
*/
METHOD FUNCTION fieldValueSet( fieldName, value ) CLASS TAlias

   ::SyncFromRecNo()

   RETURN ( ::workArea )->( FieldPut( FieldPos( fieldName ), value ) )

/*
    Get4Seek
*/
METHOD FUNCTION Get4Seek( xVal, keyVal, indexName, softSeek ) CLASS TAlias
   RETURN ::RawGet4Seek( 1, xVal, keyVal, indexName, softSeek )

/*
    Get4SeekLast
*/
METHOD FUNCTION Get4SeekLast( xVal, keyVal, indexName, softSeek ) CLASS TAlias
   RETURN ::RawGet4Seek( 0, xVal, keyVal, indexName, softSeek )

/*
    IsLocked
*/
METHOD FUNCTION IsLocked( RecNo ) CLASS TAlias
   RETURN ( ::workArea )->( IsLocked( iif( RecNo == NIL, ::FRecNo, RecNo ) ) )

/*
    KeyVal
*/
METHOD FUNCTION KeyVal( indexName ) CLASS TAlias

   ::SyncFromRecNo()

   RETURN ( ::workArea )->( KeyVal( indexName ) )

/*
    OrdCondSet
*/
METHOD FUNCTION ordCondSet( ... ) CLASS TAlias

   ::SyncFromRecNo()

   RETURN ( ::workArea )->( ordCondSet( ... ) )

/*
    OrdCreate
*/
METHOD FUNCTION ordCreate( ... ) CLASS TAlias

   ::SyncFromRecNo()

   RETURN ( ::workArea )->( ordCreate( ... ) )

/*
    OrdCustom
*/
METHOD FUNCTION ordCustom( Name, cBag, KeyVal ) CLASS TAlias

   ::SyncFromRecNo()

   RETURN ( ::workArea )->( ordCustom( Name, cBag, KeyVal ) )

/*
    ordDescend
*/
METHOD FUNCTION ordDescend( Name, cBag, lDescend ) CLASS TAlias

   ::SyncFromRecNo()

   RETURN ( ::workArea )->( ordDescend( Name, cBag, lDescend ) )

/*
    ordDestroy
*/
METHOD FUNCTION ordDestroy( tagName, bagName ) CLASS TAlias

   ::SyncFromRecNo()

   RETURN ( ::workArea )->( ordDestroy( tagName, bagName ) )

/*
    OrdKeyAdd
*/
METHOD FUNCTION ordKeyAdd( Name, cBag, KeyVal ) CLASS TAlias

   ::SyncFromRecNo()

   RETURN ( ::workArea )->( ordKeyAdd( Name, cBag, KeyVal ) )

/*
    OrdKeyDel
*/
METHOD FUNCTION ordKeyDel( Name, cBag, KeyVal ) CLASS TAlias

   ::SyncFromRecNo()

   RETURN ( ::workArea )->( ordKeyDel( Name, cBag, KeyVal ) )

/*
    OrdKeyNo
*/
METHOD FUNCTION ordKeyNo( ... ) CLASS TAlias

   ::SyncFromRecNo()

   RETURN ( ::workArea )->( ordKeyNo( ... ) )

/*
    OrdKeyVal
*/
METHOD FUNCTION ordKeyVal() CLASS TAlias

   ::SyncFromRecNo()

   RETURN ( ::workArea )->( ordKeyVal() )

/*
    OrdNumber
*/
METHOD FUNCTION ordNumber( ordName, ordBagName ) CLASS TAlias

   ::SyncFromRecNo()

   RETURN ( ::workArea )->( ordNumber( ordName, ordBagName ) )

/*
    OrdSetFocus
*/
METHOD FUNCTION ordSetFocus( Name, cBag ) CLASS TAlias

   ::SyncFromRecNo()

   RETURN ( ::workArea )->( ordSetFocus( Name, cBag ) )

/*
    Pop
*/
METHOD PROCEDURE Pop() CLASS TAlias

   IF ::FStackLen > 0
      ::FBof  := ::FStack[ ::FStackLen, 1 ]
      ::FEof  := ::FStack[ ::FStackLen, 2 ]
      ::FFound := ::FStack[ ::FStackLen, 3 ]
      ::FRecNo := ::FStack[ ::FStackLen, 4 ]
      ::ordSetFocus( ::FStack[ ::FStackLen, 5 ] )
      --::FStackLen
   ENDIF

   RETURN

/*
    Push
*/
METHOD PROCEDURE Push() CLASS TAlias

   IF Len( ::FStack ) < ++::FStackLen
      AAdd( ::FStack, { NIL, NIL, NIL, NIL, NIL } )
   ENDIF
   ::FStack[ ::FStackLen, 1 ] := ::FBof
   ::FStack[ ::FStackLen, 2 ] := ::FEof
   ::FStack[ ::FStackLen, 3 ] := ::FFound
   ::FStack[ ::FStackLen, 4 ] := ::FRecNo
   ::FStack[ ::FStackLen, 5 ] := ::ordSetFocus()

   RETURN

/*
    RawGet4Seek
*/
METHOD FUNCTION RawGet4Seek( direction, xVal, keyVal, indexName, softSeek ) CLASS TAlias

   IF ValType( xVal ) = "O"
      xVal := xVal:FieldReadBlock
   END

   IF keyVal = NIL
      keyVal := ""
   ENDIF

   IF direction = 1
      RETURN ( ::workArea )->( Get4Seek( xVal, keyVal, indexName, softSeek ) )
   ENDIF

   RETURN ( ::workArea )->( Get4SeekLast( xVal, keyVal, indexName, softSeek ) )

/*
    RecLock
*/
METHOD FUNCTION RecLock( recNo, lNoRetry ) CLASS TAlias

   LOCAL n

   ::SyncFromRecNo()
   IF recNo = NIL
      recNo := ::FrecNo
   ENDIF
   IF ::IsLocked()
      n := AScan( ::FstackLock, {| e| e[ 1 ] = recNo } )
      IF n > 0
         ::FstackLock[ n, 2 ]++
      ELSE
         AAdd( ::FstackLock, { recNo, 1 } )
      ENDIF
      RETURN .T.
   ENDIF

   IF lNoRetry = noRetry
      RETURN ( ::workArea )->( dbRLock( recNo ) )
   ENDIF

   RETURN ( ::workArea )->( RecLock( recNo ) )

/*
    RecUnLock
*/
METHOD FUNCTION RecUnLock( RecNo ) CLASS TAlias

   LOCAL n

   ::SyncFromRecNo()
   IF RecNo = NIL
      RecNo := ::FRecNo
   ENDIF
   n := AScan( ::FstackLock, {| e| e[ 1 ] = RecNo } )
   IF n > 0 .AND. ::FstackLock[ n, 2 ] > 0
      ::FstackLock[ n, 2 ]--
      RETURN .T.
   ENDIF
   hb_ADel( ::FstackLock, n, .T. )

   RETURN ( ::workArea )->( RecUnLock( RecNo ) )

/*
    Seek
*/
METHOD FUNCTION SEEK( cKey, indexName, softSeek ) CLASS TAlias

   LOCAL Result

   Result := ( ::workArea )->( Seek( cKey, indexName, softSeek ) )
   ::SyncFromDataEngine()

   RETURN Result

/*
    SeekLast
*/
METHOD FUNCTION SeekLast( cKey, indexName, softSeek ) CLASS TAlias

   LOCAL Result

   Result := ( ::workArea )->( SeekLast( cKey, indexName, softSeek ) )
   ::SyncFromDataEngine()

   RETURN Result

/*
    setWorkArea
*/
METHOD PROCEDURE setWorkArea( fullFileName, keepOpen ) CLASS TAlias

   ::FfullFileName := fullFileName
   ::FthreadId := hb_threadId()

   IF keepOpen = nil
    keepOpen := ::keepOpen
   ENDIF

   IF hb_hHasKey( __S_Instances, fullFileName )
      __S_Instances[ ::FfullFileName, "counter" ] += 1
   ELSE
      __S_Instances[ ::FfullFileName ] := { => }
      __S_Instances[ ::FfullFileName, "nWorkArea" ]   := ( alias() )->( select() )
      __S_Instances[ ::FfullFileName, "aliasName" ]   := alias()
      __S_Instances[ ::FfullFileName, "counter" ]     := iif( ( alias() )->(select() ) > 0, 1, 0 )
      __S_Instances[ ::FfullFileName, "keepOpen" ]    := keepOpen
   ENDIF

   ::FworkArea := __S_Instances[ ::FfullFileName, "aliasName" ]

   RETURN

/*
    SyncFromDataEngine
*/
METHOD PROCEDURE SyncFromDataEngine CLASS TAlias

   ::FBof  := ( ::workArea )->( Bof() )
   ::FEof  := ( ::workArea )->( Eof() )
   ::FFound := ( ::workArea )->( Found() )
   ::FRecNo := ( ::workArea )->( RecNo() )

   RETURN

/*
    SyncFromRecNo
*/
METHOD PROCEDURE SyncFromRecNo CLASS TAlias

   IF ( ::workArea )->( RecNo() ) != ::FRecNo
      ::dbGoto( ::FRecNo )
   ENDIF

RETURN

/*
    validateDbStruct
*/
METHOD PROCEDURE validateDbStruct(table) CLASS TAlias
    LOCAL itm
    LOCAL n

    /* Check for a valid db structure (based on definitions on DEFINE FIELDS) */
    IF !Empty( table:TableFileName ) .AND. table:validateDbStruct .AND. !hb_HHasKey( table:instances[ table:TableClass ], "DbStructValidated" )
        table:CheckDbStruct()
    ENDIF

    /* sets the DBS field info for each table field */
    FOR EACH itm IN table:FieldList
        IF itm:IsTableField()
            n := Upper( itm:DBS_NAME )
            n := AScan( table:DbStruct, {| e| e[ 1 ] == n } )
            IF n > 0
                itm:SetDbStruct( table:DbStruct[ n ] )
            ENDIF
            itm:Clear()
        ENDIF
    NEXT
RETURN

/*
    ENDCLASS TAlias
*/
