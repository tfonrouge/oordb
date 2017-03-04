/*
 *
 */

#include "hbclass.ch"

CLASS OORDBBASE
PROTECTED:
    METHOD __warnDescriptor()
EXPORTED:
   METHOD ObjectH

ENDCLASS

/*
    __warnDescriptor
*/
METHOD FUNCTION __warnDescriptor() CLASS OORDBBASE
    LOCAL descriptor

    IF ::isDerivedFrom( "TField" )

        descriptor := ;
            e"Field Name: \"" + ::name + e"\";" + ;
            e"Table Name: \"" + ::table:className + e"\";" + ;
            e""

    ELSEIF ::isDerivedFrom( "TTable" )

        descriptor := ;
            e"Table Name: \"" + ::className + e"\";" + ;
            e""

    ELSE

        descriptor := "unknown origin"

    ENDIF

RETURN descriptor
