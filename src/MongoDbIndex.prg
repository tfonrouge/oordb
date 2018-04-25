/*
    MongoDbIndex
*/

#include "oordb.ch"

/*
    CLASS MongoDbIndex
*/
CLASS MongoDbIndex FROM TIndexBase

PROTECTED:

    METHOD createIndex()
    METHOD indexDocument()
    METHOD keyDocument(doc)
    METHOD masterKeyDocument()

PUBLIC:

    METHOD dbGoTop()
    METHOD openIndex()

ENDCLASS

/*
    createIndex
*/
METHOD FUNCTION createIndex() CLASS MongoDbIndex
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
    dbGoTop
*/
METHOD FUNCTION dbGoTop() CLASS MongoDbIndex
    XAltD()
RETURN .T.

/*
    indexDocument
*/
METHOD FUNCTION indexDocument() CLASS MongoDbIndex
    LOCAL doc

    doc := ::masterKeyDocument
    ::keyDocument(doc)

RETURN doc

/*
    keyDocument
*/
METHOD FUNCTION keyDocument(doc) CLASS MongoDbIndex

   IF ::FKeyField != NIL
      RETURN ::FKeyField:indexDocument(nil,nil,nil,doc)
   ENDIF

 RETURN nil

/*
    masterKeyDocument
*/
METHOD FUNCTION masterKeyDocument() CLASS MongoDbIndex

    IF ::FMasterKeyField != NIL
        RETURN ::FMasterKeyField:indexDocument( NIL, .T., ::KeyFlags )
    ENDIF

RETURN bson_new()

/*
    openIndex
*/
METHOD PROCEDURE openIndex() CLASS MongoDbIndex

    /* primary index opens by default */
    IF ::Ftable:baseKeyIndex == self
        RETURN
    ENDIF

    ::super:openIndex()

RETURN
