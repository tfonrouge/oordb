
#include "oordb.ch"

CLASS TTable FROM TBaseTable
PROTECTED:
    METHOD GetDataEngine()
PUBLIC:
    METHOD Alias()
ENDCLASS

/*
    Alias
*/
METHOD FUNCTION Alias() CLASS TTable
RETURN ::DataEngine

/*
    getDataEngine
*/
METHOD FUNCTION getDataEngine() CLASS TTable
    SWITCH ::FdataEngineType
    CASE "XBASE"
        RETURN TAlias():New(self)
    CASE "MONGODB"
        RETURN MongoDbEngine():new(self)
    ENDSWITCH
RETURN nil
