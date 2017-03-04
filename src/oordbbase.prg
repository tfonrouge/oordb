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

#pragma BEGINDUMP

#include "hbapi.h"
#include "hbdefs.h"

HB_FUNC( OORDBBASE_OBJECTH )
{
  PHB_ITEM pSelf = hb_stackSelfItem();
  if( pSelf )
  {
    hb_retptr( hb_arrayId( pSelf ) );
  }
}

HB_FUNC( HB_HSETAUTOADD )     { HB_FUNC_EXEC( HB_HAUTOADD ); hb_itemReturn( hb_param( 1, HB_IT_HASH ) ); }
HB_FUNC( HB_HSETCASEMATCH )   { HB_FUNC_EXEC( HB_HCASEMATCH ); hb_itemReturn( hb_param( 1, HB_IT_HASH ) ); }
HB_FUNC( HB_HSETBINARY )      { HB_FUNC_EXEC( HB_HBINARY ); hb_itemReturn( hb_param( 1, HB_IT_HASH ) ); }
HB_FUNC( HB_HSETORDER )       { HB_FUNC_EXEC( HB_HKEEPORDER ); hb_itemReturn( hb_param( 1, HB_IT_HASH ) ); }

#pragma ENDDUMP
