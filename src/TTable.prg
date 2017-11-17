
#include "oordb.ch"

CLASS TTable FROM TBaseTable
    METHOD GetDataEngine INLINE TAlias():New( Self )
PUBLIC:
    METHOD Alias()
ENDCLASS

/*
    Alias
*/
METHOD FUNCTION Alias() CLASS TTable
RETURN ::DataEngine
