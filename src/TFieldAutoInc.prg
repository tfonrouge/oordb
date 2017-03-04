/*
 *
 */

#include "oordb.ch"
#include "xerror.ch"

/*
    TFieldAutoInc
*/
CLASS TFieldAutoInc FROM TFieldInteger

   PRIVATE:

   PROTECTED:
   DATA FDBS_TYPE INIT "+"
   DATA FFieldType INIT ftAutoInc
   DATA FType INIT "AutoInc"
   DATA FtypeNameList INIT HB_HSetCaseMatch( {"es"=>"AutoInc"} )

   METHOD getReadOnly INLINE .T.

   PUBLIC:
   PUBLISHED:

ENDCLASS
/*
    ENDCLASS TFieldAutoInc
*/
