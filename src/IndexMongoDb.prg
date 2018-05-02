/*
    IndexMongoDb
*/

#include "oordb.ch"

/*
    CLASS IndexMongoDb
*/
CLASS IndexMongoDb FROM TIndexBase

PROTECTED:

    METHOD dbGoBottomTop(n)
    METHOD createIndex()
    METHOD indexDocument()
    METHOD keyDocument(doc)
    METHOD keyValInitValue INLINE bson_new()
    METHOD getMasterKeyDoc()
    METHOD getScopeTop(doc)

PUBLIC:

    METHOD dbGoBottom INLINE ::dbGoBottomTop( -1 )
    METHOD dbGoTop INLINE ::dbGoBottomTop( 1 )
    METHOD openIndex()

ENDCLASS

/*
    createIndex
*/
METHOD FUNCTION createIndex() CLASS IndexMongoDb
    LOCAL create_indexes
    LOCAL keys
    LOCAL result
    LOCAL opts
    LOCAL reply
    LOCAL error

    opts := nil

    keys := bson_new()
    HB_BSON_APPEND(keys, "key", ::indexDocument())
    HB_BSON_APPEND(keys, "name", ::name)
    HB_BSON_APPEND(keys, "background", .T.)

    create_indexes := bson_new()
    HB_BSON_APPEND(create_indexes, "createIndexes", ::Ftable:tableFileName)
    HB_BSON_APPEND(create_indexes, "indexes", {keys})

    outStd(e"\ncreate_indexes:", bson_as_json(create_indexes))

    result := mongoc_database_write_command_with_opts(::Ftable:database:getMongoDatabase(), create_indexes, opts, @reply, @error)

    outStd(e"\nreply:", bson_as_json(reply))

    IF ! result
        outErr(e"\nerror:", error["message"])
    ENDIF

RETURN .t.

/*
    dbGoBottomTop
*/
METHOD FUNCTION dbGoBottomTop(n) CLASS IndexMongoDb
    LOCAL masterKeyVal
    LOCAL dataEngine

    masterKeyVal := ::getMasterKeyVal()

    dataEngine := ::table:DataEngine

    IF n = 1
        dataEngine:seek(masterKeyVal, ::Fname, .T.)
    ELSE
      IF ::GetScopeTop() == ::GetScopeBottom()
         dataEngine:SeekLast( masterKeyVal + ::GetScopeBottom(), ::FTagName )
      ELSE
         dataEngine:SeekLast( masterKeyVal + ::GetScopeBottom(), ::FTagName, .T. )
      ENDIF
    ENDIF

    IF ::HasFilter() .OR. ::table:HasFilter()
      ::DbFilterPush()
      ::GetCurrentRecord()
      ::DbFilterPull()
      IF ::Eof() .OR. ( !::table:FilterEval( Self ) .AND. !::table:SkipFilter( n, Self ) )
         ::table:dbGoto( 0 )
         RETURN .F.
      ENDIF
    ENDIF

RETURN ::GetCurrentRecord()

/*
    indexDocument
*/
METHOD FUNCTION indexDocument() CLASS IndexMongoDb
    LOCAL doc

    doc := ::getMasterKeyDoc
    ::keyDocument(doc)

RETURN doc

/*
    keyDocument
*/
METHOD FUNCTION keyDocument(doc) CLASS IndexMongoDb

    IF ::FKeyField != NIL
        RETURN ::FKeyField:indexDocument(nil,nil,nil,doc)
    ENDIF

RETURN nil

/*
    getMasterKeyDoc
*/
METHOD FUNCTION getMasterKeyDoc() CLASS IndexMongoDb

    IF ::FMasterKeyField != NIL
        RETURN ::FMasterKeyField:indexDocument( NIL, .T., ::KeyFlags )
    ENDIF

RETURN bson_new()

/*
    getScopeTop
*/
METHOD FUNCTION getScopeTop(doc) CLASS IndexMongoDb
    IF doc = nil
        doc := bson_new()
    ENDIF
RETURN doc

/*
    openIndex
*/
METHOD PROCEDURE openIndex() CLASS IndexMongoDb

    /* primary index opens by default */
    IF ::Ftable:baseKeyIndex == self
        RETURN
    ENDIF

    ::super:openIndex()

RETURN
