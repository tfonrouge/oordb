/*
 *
 */

#include "oordb.ch"
#include "xerror.ch"

/*
    TFieldMemo
*/
CLASS TFieldMemo FROM TFieldString

PROTECTED:

    DATA FDBS_LEN INIT 4
    DATA FDBS_DEC INIT 0
    DATA FDBS_TYPE INIT "M"
    DATA FFieldType INIT ftMemo
    DATA FSize INIT 0
    DATA FType INIT "Memo"
    DATA FtypeNameList INIT HB_HSetCaseMatch( {"es"=>"Memo"} )

PUBLIC:

    PROPERTY KeySize READ Size

ENDCLASS

/*
    ENDCLASS TFieldMemo
*/
