
#include "oordb.ch"

CLASS TTable FROM TableBase
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
        RETURN EngineXBase():New(self)
    CASE "MONGODB"
        RETURN EngineMongoDb():new(self)
    ENDSWITCH
RETURN nil
