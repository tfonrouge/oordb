/*
 *
 */

#include "oordb.ch"
#include "xerror.ch"

/*
    TFieldVariant
*/
CLASS TFieldVariant FROM TField

PROTECTED:

    DATA FDBS_LEN INIT 4
    DATA FDBS_DEC INIT 0
    DATA FDBS_TYPE INIT "M"
    DATA FFieldType INIT ftVariant

    DATA FgetKeyValCodeBlock
    DATA FindexExpressionCodeBlock

    DATA FSize INIT 0
    DATA FType INIT "Variant"
    DATA FtypeNameList INIT HB_HSetCaseMatch( {"es"=>"Variante"} )

    METHOD indexExpression

    METHOD setgetKeyValCodeBlock( getKeyValCodeBlock ) INLINE ::FgetKeyValCodeBlock := getKeyValCodeBlock
    METHOD setindexExpressionCodeBlock( indexExpressionCodeBlock ) INLINE ::FindexExpressionCodeBlock := indexExpressionCodeBlock

PUBLIC:

    METHOD getKeyVal( keyVal )

    PROPERTY getKeyValCodeBlock READ FgetKeyValCodeBlock WRITE setgetKeyValCodeBlock
    PROPERTY indexExpressionCodeBlock READ FindexExpressionCodeBlock WRITE setindexExpressionCodeBlock

ENDCLASS

/*
    getKeyVal
*/
METHOD FUNCTION getKeyVal( keyVal ) CLASS TFieldVariant
    IF __objHasMsgAssigned( ::table, "getKeyVal_Field_" + ::FName ) .AND. ::FgetKeyValCodeBlock = nil
        ::FgetKeyValCodeBlock := &( "{|self,...|" + "::getKeyVal_Field_" + ::FName + "( ... ) }" )
    ENDIF
    IF ::FgetKeyValCodeBlock = nil
        THROW ERROR getKeyVal_Message_OR_CodeBlock_NOT_DEFINED
    ENDIF
RETURN ::FgetKeyValCodeBlock:eval( ::table, keyVal )

/*
    indexExpression
*/
METHOD FUNCTION indexExpression CLASS TFieldVariant
RETURN ::FindexExpressionCodeBlock:eval( ::table )

/*
    ENDCLASS TFieldVariant
*/
