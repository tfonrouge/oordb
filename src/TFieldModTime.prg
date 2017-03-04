/*
 *
 */

#include "oordb.ch"
#include "xerror.ch"

/*
    TFieldModTime
*/
CLASS TFieldModTime FROM TFieldDateTime

   PRIVATE:

   PROTECTED:
   DATA FDBS_LEN INIT 8
   DATA FDBS_DEC INIT 0
   DATA FDBS_TYPE INIT "="
   DATA FFieldType INIT ftModTime
   DATA FModStamp INIT .T.        // Field is automatically mantained (dbf layer)
   DATA FType INIT "ModTime"
   DATA FtypeNameList INIT HB_HSetCaseMatch( {"es"=>"ModTime"} )

   METHOD getReadOnly INLINE .T.

   PUBLIC:
   PUBLISHED:

ENDCLASS

/*
    EndClass TFieldModTime
*/
