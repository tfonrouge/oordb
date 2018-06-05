#include "oordb.ch"
#include "xerror.ch"

CLASS EngineMongoDb FROM EngineBase
PROTECTED:

    DATA bsonDoc
    DATA Fcollection

PUBLIC:

    CONSTRUCTOR New( table )

    METHOD __dbZap()
    METHOD AddRec( index )
    METHOD dbCloseArea()
    METHOD dbDelete()
    METHOD dbGoBottom( indexName )
    METHOD dbGoto( RecNo )
    METHOD dbGoTop( indexName )
    METHOD dbInfo( ... )
    METHOD DbOpen( table )
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
    METHOD ordNumber( ordName )
    METHOD ordSetFocus(name)
    METHOD Pop()
    METHOD Push()
    METHOD RawGet4Seek( direction, xVal, keyVal, indexName, softSeek )
    METHOD recClear() INLINE ( ::workArea )->( recClear() )
    METHOD RecCount INLINE ( ::workArea )->( RecCount() )
    METHOD RecLock( recNo, lNoRetry )
    METHOD RecUnLock( RecNo )
    METHOD seek(doc, indexName, softSeek)
    METHOD SeekLast( cKey, indexName, softSeek )
    METHOD SyncFromDataEngine(bof, eof, found, recNo)
    METHOD SyncFromRecNo

    METHOD validateDbStruct VIRTUAL

    MESSAGE DbSeek METHOD SEEK

    PROPERTY Bof   INIT .T.
    PROPERTY Eof   INIT .T.
    PROPERTY Found INIT .F.
    PROPERTY indexName
    PROPERTY Name
    PROPERTY RecNo WRITE SetRecNo

    PROPERTY doc

    PROPERTY indexList

    METHOD collection INLINE ::Fcollection

ENDCLASS

/*
    New
*/
METHOD New( table ) CLASS EngineMongoDb

   IF Empty( table ) .OR. ! HB_ISOBJECT( table )
      RAISE ERROR "TAlias: Empty Table parameter."
   ENDIF

   ::Fname := table:tableFileName

   IF !::DbOpen( table )
      // RAISE ERROR "TAlias: Cannot Open Table '" + table:TableFileName + "'"
      Break( "TAlias: Cannot Open Collection '" + table:tableFileName + "'" )
   ENDIF

   ::SyncFromDataEngine()

RETURN self

/*
   __DbZap
*/
METHOD FUNCTION __dbZap() CLASS EngineMongoDb
  RETURN ( ::workArea )->( __dbZap() )

/*
   AddRec
*/
METHOD FUNCTION AddRec( index ) CLASS EngineMongoDb

  LOCAL Result

  Result := ( ::workArea )->( AddRec(, index ) )
  ::SyncFromDataEngine()

  RETURN Result

/*
   DbCloseArea
*/
METHOD PROCEDURE dbCloseArea() CLASS EngineMongoDb

RETURN

/*
   DbDelete
*/
METHOD PROCEDURE dbDelete() CLASS EngineMongoDb

  ::SyncFromRecNo()
  ( ::workArea )->( dbDelete() )

  RETURN

/*
   DbGoBottom
*/
METHOD FUNCTION dbGoBottom( indexName ) CLASS EngineMongoDb

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
METHOD FUNCTION dbGoto( RecNo ) CLASS EngineMongoDb

  LOCAL Result

  Result := ( ::workArea )->( dbGoto( RecNo ) )
  ::SyncFromDataEngine()

  RETURN Result

/*
   DbGoTop
*/
METHOD FUNCTION dbGoTop( indexName ) CLASS EngineMongoDb

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
METHOD FUNCTION dbInfo( ... ) CLASS EngineMongoDb

RETURN ( ::workArea )->( dbInfo( ... ) )

/*
   DbOpen
*/
METHOD FUNCTION DbOpen( table ) CLASS EngineMongoDb
    LOCAL cursor
    LOCAL error
    LOCAL doc

    ::Fcollection := mongoc_database_get_collection(table:dataBase:getMongoDatabase(), table:tableFileName)

    cursor := mongoc_collection_find_indexes_with_opts(::Fcollection)

    ::FindexList := {}

    IF ! mongoc_cursor_error(cursor,@error)
        WHILE mongoc_cursor_next(cursor,@doc)
            doc := hb_bson_as_hash(doc)
            aAdd(::FindexList, {doc["name"],doc})
        ENDDO
    ELSE
        RAISE ERROR error["message"]
    ENDIF

RETURN .T.

/*
   DbOrderInfo
*/
METHOD FUNCTION dbOrderInfo( ... ) CLASS EngineMongoDb

RETURN ( ::workArea )->( dbOrderInfo( ... ) )

/*
   DbRecall
*/
METHOD PROCEDURE dbRecall() CLASS EngineMongoDb

  ::SyncFromRecNo()
  ( ::workArea )->( dbRecall() )

  RETURN

/*
   DbSkip
*/
METHOD FUNCTION dbSkip( nRecords, indexName ) CLASS EngineMongoDb

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
METHOD FUNCTION dbStruct() CLASS EngineMongoDb
  RETURN ( ::workArea )->( dbStruct() )

/*
   Deleted
*/
METHOD FUNCTION Deleted() CLASS EngineMongoDb

  ::SyncFromRecNo()

  RETURN ( ::workArea )->( Deleted() )

/*
   Eval
*/
METHOD FUNCTION Eval( codeBlock, ... ) CLASS EngineMongoDb

  ::SyncFromRecNo()

  RETURN ( ::workArea )->( codeBlock:Eval( ... ) )

/*
   existsKey
*/
METHOD FUNCTION existsKey( KeyValue, IndexName, RecNo ) CLASS EngineMongoDb
  RETURN ( ::workArea )->( existsKey( KeyValue, IndexName, RecNo ) )

/*
   fieldValueGet
*/
METHOD FUNCTION fieldValueGet( fieldName ) CLASS EngineMongoDb

  ::SyncFromRecNo()

  RETURN ( ::workArea )->( FieldGet( FieldPos( fieldName ) ) )

/*
   fieldValueSet
*/
METHOD FUNCTION fieldValueSet( fieldName, value ) CLASS EngineMongoDb

  ::SyncFromRecNo()

  RETURN ( ::workArea )->( FieldPut( FieldPos( fieldName ), value ) )

/*
   Get4Seek
*/
METHOD FUNCTION Get4Seek( xVal, keyVal, indexName, softSeek ) CLASS EngineMongoDb
  RETURN ::RawGet4Seek( 1, xVal, keyVal, indexName, softSeek )

/*
   Get4SeekLast
*/
METHOD FUNCTION Get4SeekLast( xVal, keyVal, indexName, softSeek ) CLASS EngineMongoDb
  RETURN ::RawGet4Seek( 0, xVal, keyVal, indexName, softSeek )

/*
   IsLocked
*/
METHOD FUNCTION IsLocked( RecNo ) CLASS EngineMongoDb
  RETURN ( ::workArea )->( IsLocked( iif( RecNo == NIL, ::FRecNo, RecNo ) ) )

/*
   OrdCondSet
*/
METHOD FUNCTION ordCondSet( ... ) CLASS EngineMongoDb

  ::SyncFromRecNo()

  RETURN ( ::workArea )->( ordCondSet( ... ) )

/*
   OrdCreate
*/
METHOD FUNCTION ordCreate( ... ) CLASS EngineMongoDb

  ::SyncFromRecNo()

  RETURN ( ::workArea )->( ordCreate( ... ) )

/*
   OrdCustom
*/
METHOD FUNCTION ordCustom( Name, cBag, KeyVal ) CLASS EngineMongoDb

  ::SyncFromRecNo()

  RETURN ( ::workArea )->( ordCustom( Name, cBag, KeyVal ) )

/*
   ordDescend
*/
METHOD FUNCTION ordDescend( Name, cBag, lDescend ) CLASS EngineMongoDb

  ::SyncFromRecNo()

  RETURN ( ::workArea )->( ordDescend( Name, cBag, lDescend ) )

/*
   ordDestroy
*/
METHOD FUNCTION ordDestroy( tagName, bagName ) CLASS EngineMongoDb

  ::SyncFromRecNo()

  RETURN ( ::workArea )->( ordDestroy( tagName, bagName ) )

/*
   OrdKeyAdd
*/
METHOD FUNCTION ordKeyAdd( Name, cBag, KeyVal ) CLASS EngineMongoDb

  ::SyncFromRecNo()

  RETURN ( ::workArea )->( ordKeyAdd( Name, cBag, KeyVal ) )

/*
   OrdKeyDel
*/
METHOD FUNCTION ordKeyDel( Name, cBag, KeyVal ) CLASS EngineMongoDb

  ::SyncFromRecNo()

  RETURN ( ::workArea )->( ordKeyDel( Name, cBag, KeyVal ) )

/*
   OrdKeyNo
*/
METHOD FUNCTION ordKeyNo( ... ) CLASS EngineMongoDb

  ::SyncFromRecNo()

  RETURN ( ::workArea )->( ordKeyNo( ... ) )

/*
   OrdKeyVal
*/
METHOD FUNCTION ordKeyVal() CLASS EngineMongoDb

  ::SyncFromRecNo()

  RETURN ( ::workArea )->( ordKeyVal() )

/*
   OrdNumber
*/
METHOD FUNCTION ordNumber( ordName ) CLASS EngineMongoDb

RETURN aScan(::FindexList,{|e| e[1] == ordName })

/*
   OrdSetFocus
*/
METHOD FUNCTION ordSetFocus(name) CLASS EngineMongoDb
    LOCAL oldIndexName

    ::SyncFromRecNo()

    oldIndexName := ::FindexName
    ::FindexName := name

RETURN oldIndexName

/*
   Pop
*/
METHOD PROCEDURE Pop() CLASS EngineMongoDb

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
METHOD PROCEDURE Push() CLASS EngineMongoDb

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
METHOD FUNCTION RawGet4Seek( direction, xVal, keyVal, indexName, softSeek ) CLASS EngineMongoDb

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
METHOD FUNCTION RecLock( recNo, lNoRetry ) CLASS EngineMongoDb

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
METHOD FUNCTION RecUnLock( RecNo ) CLASS EngineMongoDb

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
   seek
*/
METHOD FUNCTION seek(doc, indexName, softSeek) CLASS EngineMongoDb
    LOCAL pipeline
    LOCAL result
    LOCAL cursor
    LOCAL error
    LOCAL p
    LOCAL a

    HB_SYMBOL_UNUSED(indexName)
    HB_SYMBOL_UNUSED(softSeek)

    pipeline := bson_new()
    a := {}

    p := bson_new()
    BSON_APPEND_DOCUMENT(p, "$match", doc)
    aAdd(a, p)

    HB_BSON_APPEND(pipeline, "pipeline", a)

    cursor := mongoc_collection_aggregate(::Fcollection, nil, pipeline)
    result := mongoc_cursor_next(cursor, @::Fdoc)

    IF mongoc_cursor_error(cursor, @error)
        ::FRecNo := nil
        outErr(e"\nerror:", error["message"])
    ELSE
        ::FrecNo := hb_bson_value(::Fdoc, "_id")
    ENDIF

    ::SyncFromDataEngine(.F., !result, result, )

RETURN result

/*
   SeekLast
*/
METHOD FUNCTION SeekLast( cKey, indexName, softSeek ) CLASS EngineMongoDb
    LOCAL cursor
    LOCAL result
    LOCAL doc

    HB_SYMBOL_UNUSED(indexName + softSeek)

    cursor := mongoc_collection_find_with_opts( ::Fcollection, cKey, nil )
    result := mongoc_cursor_next(cursor, @doc)
      //::SyncFromDataEngine()

RETURN result

/*
   SyncFromDataEngine
*/
METHOD PROCEDURE SyncFromDataEngine(bof, eof, found, recNo) CLASS EngineMongoDb

    ::FBof := bof
    ::FEof := eof
    ::FFound := found
    ::FRecNo := recNo

RETURN

/*
   SyncFromRecNo
*/
METHOD PROCEDURE SyncFromRecNo CLASS EngineMongoDb

    /*
    IF ( ::workArea )->( RecNo() ) != ::FRecNo
        ::dbGoto( ::FRecNo )
    ENDIF
    */

RETURN
