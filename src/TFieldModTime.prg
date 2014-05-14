/*
 *
 */

#include "oordb.ch"
#include "xerror.ch"

/*
    TModTimeField
*/
CLASS TModTimeField FROM TDateTimeField

   PRIVATE:

   PROTECTED:
   DATA FDBS_LEN INIT 8
   DATA FDBS_DEC INIT 0
   DATA FDBS_TYPE INIT "="
   DATA FModStamp INIT .T.        // Field is automatically mantained (dbf layer)
   DATA FType INIT "ModTime"
   DATA FtypeNameList INIT hb_hSetCaseMatch( {"es"=>"ModTime"} )
   PUBLIC:
   PUBLISHED:

ENDCLASS

/*
    EndClass TModTimeField
*/