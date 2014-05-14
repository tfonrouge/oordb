/*
 *
 */

#include "oordb.ch"
#include "xerror.ch"

/*
    TArrayField
*/
CLASS TArrayField FROM TField

   PROTECTED:

   DATA FDBS_LEN INIT 4
   DATA FDBS_DEC INIT 0
   DATA FDBS_TYPE INIT "M"
   DATA FFieldType INIT ftArray
   DATA FSize INIT 0
   DATA FType INIT "Array"
   DATA FtypeNameList INIT hb_hSetCaseMatch( {"es"=>"Array"} )

   METHOD GetAsVariant( ... )
   METHOD SetAsVariant( array )

   PUBLIC:

ENDCLASS

/*
    GetAsVariant
*/
METHOD FUNCTION GetAsVariant( ... ) CLASS TArrayField

   LOCAL array

   array := HB_DeSerialize( ::Super:GetAsVariant( ... ) )

   IF ValType( array ) != "A"
      array := {}
   ENDIF

RETURN array

/*
    SetAsVariant
*/
METHOD PROCEDURE SetAsVariant( array ) CLASS TArrayField

    LOCAL ser

    IF ValType( array ) = "A"
        ser := HB_Serialize( array )
    ELSE
        ser := HB_Serialize( {} )
    ENDIF

    ::Super:SetAsVariant( ser )

RETURN

/*
    ENDCLASS TArrayField
*/