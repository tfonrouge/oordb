
#include "oordb.ch"

CLASS TTable FROM TBaseTable
PUBLIC:
    METHOD Alias()
ENDCLASS

/*
    Alias
*/
METHOD FUNCTION Alias() CLASS TTable
RETURN ::DataEngine
