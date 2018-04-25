#include "oordb.ch"
#include "xerror.ch"
#include "inkey.ch"

#include "dbinfo.ch"

/*
    indexNew
*/
FUNCTION indexNew(...)
    LOCAL index
    LOCAL table

    table := hb_pValue(1)

    DO CASE
    CASE table:dataEngineType == "XBASE"
        index := TIndex()
    CASE table:dataEngineType == "MONGODB"
        index := MongoDbIndex()
    ENDCASE

RETURN index:new(...)

/*
    CLASS TIndexBase
*/
CLASS TIndexBase FROM OORDBBASE
PROTECTED:
    CLASSDATA indexCreationList INIT {}
    DATA FAutoIncrementKeyField
    DATA FDbFilter
    DATA FDbFilterStack INIT {}
    DATA FCaseSensitive INIT .T.
    DATA FCustom INIT .F.
    DATA FDescend INIT .F.
    DATA FForKey
    DATA FKeyField
    DATA FKeyFlags
    DATA FMasterKeyField
    DATA FRightJustified INIT .F.
    DATA FTable
    DATA FUniqueKeyField
    METHOD closeTemporary() VIRTUAL
    METHOD GetField
    METHOD SetCaseSensitive( CaseSensitive ) INLINE ::FCaseSensitive := CaseSensitive
    METHOD SetDescend( Descend ) INLINE ::FDescend := Descend
    METHOD SetField( nIndex, XField )
    METHOD SetForKey(forKey) INLINE ::FForKey := forKey
    METHOD SetRightJustified( rightJust ) INLINE ::FRightJustified := rightJust

PUBLIC:

    DATA temporary INIT .F.
    DATA WarnMsg

    METHOD New( Table, tagName, name, indexType, curClass, warnMsg ) CONSTRUCTOR

    METHOD AddIndex
    METHOD bindIndex( reusing, indexType, curClass )
    METHOD DbFilterPull()
    METHOD DbFilterPush( ignoreMasterKey )
    METHOD GetCurrentRecord()
    METHOD HasFilter() INLINE ::FDbFilter != NIL
    METHOD openIndex()
    METHOD SetDbFilter( dbFilter )
    METHOD Table
    PROPERTY __autoIncrementBase
    PROPERTY AutoIncrementKeyField INDEX 1 READ GetField WRITE SetField
    PROPERTY DbFilterRAW
    PROPERTY CaseSensitive READ FCaseSensitive WRITE SetCaseSensitive
    PROPERTY Custom READ FCustom
    PROPERTY DbFilter READ FDbFilter WRITE SetDbFilter
    PROPERTY Descend READ FDescend WRITE SetDescend
    PROPERTY ForKey READ FForKey WRITE SetForKey
    PROPERTY IndexType
    PROPERTY IsPrimaryIndex
    PROPERTY KeyField INDEX 3 READ GetField WRITE SetField
    PROPERTY KeyFlags READ FKeyFlags
    PROPERTY MasterKeyField INDEX 0 READ GetField WRITE SetField
    PROPERTY Name
    PROPERTY opened INIT .F.
    PROPERTY RightJustified READ FRightJustified WRITE SetRightJustified
    PROPERTY TableBaseClass
    PROPERTY TagName
    PROPERTY UniqueKeyField INDEX 2 READ GetField WRITE SetField
    PROPERTY useIndex READWRITE /* index to use when poblating a custom index */

ENDCLASS

/*
    New
*/
METHOD New( Table, tagName, name, indexType, curClass, warnMsg ) CLASS TIndexBase

    ::FTable := Table
    ::WarnMsg := warnMsg

    IF Len( tagName ) > 10
        RAISE ERROR "TagName '" + tagName + "' exceeds lenght of 10..."
    ENDIF

    ::FTagName := tagName

    IF Empty( name )
        name := tagName
    ENDIF

    ::FName := name

    ::bindIndex( .f., indexType, curClass )

RETURN Self

/*
    AddIndex
*/
METHOD AddIndex( cMasterKeyField, ai, un, cKeyField, keyFlags, ForKey, cs, de, acceptEmptyUnique, useIndex, temporary, rightJust, cu, default ) CLASS TIndexBase

    IF hb_isHash( keyFlags )
        ::FKeyFlags := keyFlags
    ENDIF

    ::MasterKeyField := cMasterKeyField

    /* Check if needs to add the primary index key */
    IF ::table:PrimaryIndex == Self
        IF ai != nil
            SWITCH ai
            CASE "AUTOINCREMENT"
                ::F__autoIncrementBase := 36
                EXIT
            CASE "AUTOINCREMENT_BASE64"
                ::F__autoIncrementBase := 64
                EXIT
            ENDSWITCH
            ::AutoIncrementKeyField := cKeyField
        ELSE
            ::UniqueKeyField := cKeyField
        ENDIF
    ELSE
        /* this may change in assigning ::keyField value bellow */
        ::FCustom := iif( cu = nil, .F. , cu )
        DO CASE
        /* Check if index key is AutoIncrement */
        CASE ai != nil
            SWITCH ai
            CASE "AUTOINCREMENT"
                ::F__autoIncrementBase := 36
                EXIT
            CASE "AUTOINCREMENT_BASE64"
                ::F__autoIncrementBase := 64
                EXIT
            ENDSWITCH
            ::AutoIncrementKeyField := cKeyField
            /* Check if index key is Unique */
        CASE un == .T.
            ::UniqueKeyField := cKeyField
        /* Check if index key is a simple index */
        OTHERWISE
            ::KeyField := cKeyField
        ENDCASE
    ENDIF

    IF acceptEmptyUnique != NIL
        ::UniqueKeyField:AcceptEmptyUnique := acceptEmptyUnique
    ENDIF

    ::ForKey := ForKey
    ::CaseSensitive := iif( cs = nil, .F., cs )
    ::RightJustified := rightJust == .T.
    ::Descend := iif( de = nil, .F., de )
    ::FuseIndex := useIndex
    ::temporary := temporary == .T.

    ::table:addIndexMessage( ::name, default )

RETURN Self

/*
    bindIndex
*/
METHOD PROCEDURE bindIndex( reusing, indexType, curClass ) CLASS TIndexBase
    LOCAL class
    LOCAL index

    /* on reusing must remove index on parent classes */
    IF reusing
        FOR EACH class IN ::table:indexList
            FOR EACH index IN class
                IF index:__enumKey == ::Fname .AND. index == self
//                    hb_hDel( class, index:__enumKey )
                    EXIT
                ENDIF
            NEXT
        NEXT
        FOR EACH index IN ::table:primaryIndexList
            IF index == ::Fname
//                hb_hDel( ::table:primaryIndexList, index:__enumKey )
                EXIT
            ENDIF
        NEXT
    ENDIF

    ::FIndexType := indexType

    IF curClass = NIL
        curClass := ::table:ClassName()
    ENDIF

    ::FTableBaseClass := curClass

    IF !hb_HHasKey( ::table:IndexList, curClass )
        ::table:IndexList[ curClass ] := HB_HSetOrder( HB_HSetCaseMatch( { => }, .F. ), .T. )
    ENDIF

    ::table:IndexList[ curClass, ::Fname ] := Self

    ::FIsPrimaryIndex := indexType = "PRIMARY"

    IF ::FIsPrimaryIndex
        ::table:SetPrimaryIndexList( curClass, ::Fname )
        ::table:SetPrimaryIndex( Self )
    ENDIF

RETURN

/*
    DbFilterPull
*/
METHOD PROCEDURE DbFilterPull() CLASS TIndexBase

    ::FDbFilter := ATail( ::FDbFilterStack )
    hb_ADel( ::FDbFilterStack, Len( ::FDbFilterStack ), .T. )
    ::table:DbFilterPull()

RETURN

/*
    DbFilterPush
*/
METHOD PROCEDURE DbFilterPush( ignoreMasterKey ) CLASS TIndexBase

    AAdd( ::FDbFilterStack, ::FDbFilter )
    ::FDbFilter := NIL
    ::table:DbFilterPush( ignoreMasterKey )

RETURN

/*
    GetCurrentRecord
*/
METHOD FUNCTION GetCurrentRecord() CLASS TIndexBase
    LOCAL result
    LOCAL index := ::table:Index

    ::table:Index := Self

    result := ::table:GetCurrentRecord()

    ::table:Index := index

RETURN result

/*
    GetField
*/
METHOD FUNCTION GetField( nIndex ) CLASS TIndexBase
    LOCAL AField

    SWITCH nIndex
    CASE 0
        AField := ::FMasterKeyField
        EXIT
    CASE 1
        AField := ::FAutoIncrementKeyField
        EXIT
    CASE 2
        AField := ::FUniqueKeyField
        EXIT
    CASE 3
        AField := ::FKeyField
        EXIT
    ENDSWITCH

RETURN AField

/*
    openIndex
*/
METHOD PROCEDURE openIndex() CLASS TIndexBase
    LOCAL index
    LOCAL processing

    IF ::temporary .AND. ::Fopened
        ::closeTemporary()
    ENDIF

    IF ! ::Fopened
        IF empty( ::tagName ) .OR. ::table:DataEngine:ordNumber( ::TagName ) = 0
            processing := .F.
            FOR EACH index IN ::indexCreationList
                processing := ::TableBaseClass == index:TableBaseClass .AND. ::tagName == index:tagName
                IF processing
                    ::Fopened := .T.
                    EXIT
                ENDIF
            NEXT
            IF ! processing
                aAdd( ::indexCreationList, self )
                IF ! ::CreateIndex()
                    RAISE ERROR "Failure to create Index '" + ::Name + "'"
                ENDIF
                hb_aDel( ::indexCreationList, len( ::indexCreationList ), .T. )
            ENDIF
        ELSE
            ::Fopened := .T.
        ENDIF
    ENDIF
RETURN

/*
    SetDbFilter
*/
METHOD FUNCTION SetDbFilter( dbFilter ) CLASS TIndexBase
    SWITCH ValType( dbFilter )
    CASE "B"
        ::FDbFilterRAW := nil
        ::FDbFilter := dbFilter
        EXIT
    CASE "M"
    CASE "C"
        ::FDbFilterRAW := dbFilter
        ::FDbFilter := hb_macroBlock( dbFilter )
        EXIT
    CASE "U"
        ::FDbFilterRAW := nil
        ::FDbFilter := nil
        EXIT
    OTHERWISE
        ::Invalid_DbFilter_Value()
    ENDSWITCH
RETURN ::FDbFilter

/*
    SetField
*/
METHOD PROCEDURE SetField( nIndex, XField ) CLASS TIndexBase
    LOCAL AField
    LOCAL fld
    LOCAL fieldBlock
    LOCAL isCustomIndex := .F.

    SWITCH ValType( XField )
    CASE 'C'
        AField := ::table:FieldByName( XField )
        IF AField = NIL .AND. ":" $ XField

            fieldBlock := ::table:BuildFieldBlockFromFieldExpression( XField, "Value", @fld )

            IF fieldBlock = NIL
                RAISE ERROR "Error building (COMPOUND) Index Field '" + XField + "' ..."
                RETURN
            ENDIF

            AField := __DynSN2Sym( fld:ClassName ):Exec():New( ::table, ::FTableBaseClass )
            AField:Name := StrTran( XField, ":", "_" )
            AField:FieldMethod := fieldBlock
            AField:Published := .F.

            isCustomIndex := .T.

        ENDIF
        IF AField = NIL
            RAISE ERROR "Declared Index Field '" + XField + "' doesn't exist..."
            RETURN
        ENDIF
        EXIT
    CASE 'O'
        IF !XField:IsDerivedFrom( "TField" )
            ::Error_Not_TField_Type_Table()
            RETURN
        ENDIF
        AField := XField
        EXIT
    CASE 'A'
        /* Array of fields are stored in a TFieldString (for the index nature) */
        AField := TFieldString():New( ::table, ::FTableBaseClass )
        AField:FieldMethod := XField
        AField:Published := .F.
        fld := ::table:FieldByName( AField:Name )
        IF fld = NIL
            AField:AddFieldMessage()
        ELSE
            AField := fld
        ENDIF
        EXIT
    CASE 'U'
        AField := NIL
        EXIT
    OTHERWISE
        SHOW WARN "! : Not a Valid Field Identifier..."
        RETURN
    ENDSWITCH

    /* Assign PrimaryKeyComponent value */
    IF ::table:PrimaryIndex == Self /* check if index is the Primary index */
        IF !AField = nil
            AField:PrimaryKeyComponent := .T.
        ENDIF
    ENDIF

    IF AField = nil
        RETURN
    ENDIF

    /* if not custom index ( by clause CUSTOM in index definition ) then checks if it must be custom */
    IF ! ::FCustom
        IF ! isCustomIndex
            isCustomIndex := AField:Calculated .AND. AField:customIndexExpression = nil
        ENDIF

        IF isCustomIndex
            ::SetCustomIndexExpression( XField )
        ELSE
            ::FCustom := .F.
        ENDIF
    ENDIF

    /* Assign MasterField value to the TBaseTable object field */
    IF nIndex = 0
        AField:IsMasterFieldComponent := .T.
    ENDIF

    SWITCH nIndex
    CASE 0  /* MasterKeyField */
        ::FMasterKeyField := AField
        EXIT
    CASE 1  /* AutoIncrementKeyField */
        IF AField:FieldMethodType = 'A'
            RAISE ERROR "Array of Fields are not Allowed as AutoIncrement Index Key..."
        ENDIF
        IF AField:IsDerivedFrom( "TFieldTable" )
            RAISE ERROR "TFieldTable's are not Allowed as AutoIncrement Index Key..."
        ENDIF
        AField:AutoIncrementKeyIndex := Self
        ::FAutoIncrementKeyField := AField
        IF ::FMasterKeyField != nil
            ::FMasterKeyField:setIndexMasterAutoIncKey( self )
        ENDIF
    CASE 2  /* UniqueKeyField */
        AAdd( AField:UniqueKeyIndexList, Self )
        IF AField:FieldMethodType = 'A'
            AField:Table:FieldByName( AField:Name, @fld )
            AField:Table:FieldList[ AField:FieldArrayIndex[ Len( AField:FieldArrayIndex ) ] ]:LastUniqueFieldList := fld
        ENDIF
        ::FUniqueKeyField := AField
    CASE 3  /* KeyField */
        IF AField:IsDerivedFrom( "TFieldString" ) .AND. Len( AField ) = 0
            RAISE ERROR ::table:ClassName + ": Master key field <" + AField:Name + "> needs a size > zero..."
        ENDIF
        AField:AddIndexKey( Self )
        ::FKeyField := AField
        IF ::FIsPrimaryIndex .AND. ::table:BaseKeyIndex = NIL
            ::table:SetBaseKeyIndex( Self )
        ENDIF
        EXIT
    ENDSWITCH

RETURN

/*
    table
*/
METHOD FUNCTION table CLASS TIndexBase
RETURN ::FTable
