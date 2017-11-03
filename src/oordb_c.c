/*
 *
 */

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbstack.h"

#if HB_VER_MAJOR > 3 || (HB_VER_MAJOR == 3 && HB_VER_MINOR >= 4)
    HARBOUR HB_FUNC_EXEC( HB_HAUTOADD );
    HARBOUR HB_FUNC_EXEC( HB_HCASEMATCH );
    HARBOUR HB_FUNC_EXEC( HB_HBINARY );
    HARBOUR HB_FUNC_EXEC( HB_HKEEPORDER );

    HB_FUNC( HB_HSETAUTOADD )     { HB_FUNC_EXEC( HB_HAUTOADD ); hb_itemReturn( hb_param( 1, HB_IT_HASH ) ); }
    HB_FUNC( HB_HSETCASEMATCH )   { HB_FUNC_EXEC( HB_HCASEMATCH ); hb_itemReturn( hb_param( 1, HB_IT_HASH ) ); }
    HB_FUNC( HB_HSETBINARY )      { HB_FUNC_EXEC( HB_HBINARY ); hb_itemReturn( hb_param( 1, HB_IT_HASH ) ); }
    HB_FUNC( HB_HSETORDER )       { HB_FUNC_EXEC( HB_HKEEPORDER ); hb_itemReturn( hb_param( 1, HB_IT_HASH ) ); }
#endif

HB_FUNC( OORDBBASE_OBJECTID )
{
  PHB_ITEM pSelf = hb_stackSelfItem();
  if( pSelf )
  {
    hb_retptr( hb_arrayId( pSelf ) );
  }
}

HB_FUNC( OORDBBASE_OBJECTFROMID )
{
    void * pObjectId = hb_parptr( 1 );

    if ( pObjectId ) {
        PHB_ITEM pItem = hb_arrayFromId( NULL, pObjectId );
        if ( hb_arrayIsObject( pItem ) ) {
            hb_itemReturnRelease( pItem );
        }
        else {
            hb_itemRelease( pItem );
        }
    }
}
