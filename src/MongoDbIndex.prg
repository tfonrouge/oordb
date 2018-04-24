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
    METHOD keyDocument()
    METHOD masterKeyDocument()

PUBLIC:

    METHOD openIndex()

ENDCLASS

/*
    createIndex
*/
METHOD FUNCTION createIndex() CLASS MongoDbIndex
    LOCAL create_indexes
    LOCAL result
    LOCAL opts
    LOCAL reply
    LOCAL error

    opts := nil

    XAltD()
    create_indexes := {=>}
    create_indexes["createIndexes"] := ::Ftable:tableFileName
    create_indexes["indexes"] := { "key" => { ::indexDocument() } }

    result := mongoc_database_write_command_with_opts(::Ftable:database:getMongoDatabase(), create_indexes, opts, @reply, @error)

    IF result
    ENDIF

RETURN .t.

/*
    indexDocument
*/
METHOD FUNCTION indexDocument() CLASS MongoDbIndex
    LOCAL exp
    LOCAL keyExp

    exp := ::masterKeyDocument
    keyExp := ::keyDocument

    IF !Empty( keyExp )
        exp += iif( Len( exp ) = 0, "", "+" ) + keyExp
    ENDIF

RETURN exp

/*
    keyDocument
*/
METHOD FUNCTION keyDocument() CLASS MongoDbIndex

   IF ::FKeyField != NIL
      RETURN ::FKeyField:indexDocument
   ENDIF

   RETURN ""

/*
    masterKeyDocument
*/
METHOD FUNCTION masterKeyDocument() CLASS MongoDbIndex

    IF ::FMasterKeyField != NIL
        RETURN ::FMasterKeyField:indexDocument( NIL, .T., ::KeyFlags )
    ENDIF

RETURN ""

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
