/*
 *
 */

#include "oordb.ch"
#include "xerror.ch"

/*
    TFieldRowVer
*/
CLASS TFieldRowVer FROM TFieldInteger

   PRIVATE:

   PROTECTED:
   DATA FDBS_TYPE INIT "^"
   DATA FFieldType INIT ftRowVer
   DATA FType INIT "RowVer"
   DATA FtypeNameList INIT HB_HSetCaseMatch( {"es"=>"RowVer"} )

   METHOD getReadOnly INLINE .T.

   PUBLIC:
   PUBLISHED:

ENDCLASS
/*
    ENDCLASS TFieldRowVer
*/
