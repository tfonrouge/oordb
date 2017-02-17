/*
 *
 */

/*
    TIndex
*/

#include "oordb.ch"
#include "xerror.ch"
#include "inkey.ch"

#include "dbinfo.ch"

/*
    CLASS TIndex
*/
CLASS TIndex FROM OORDBBASE

PRIVATE:

   DATA FAutoIncrementKeyField
   DATA FCaseSensitive INIT .T.
   DATA FCustom INIT .F.
   DATA FDescend INIT .F.
   DATA FDbFilter
   DATA FDbFilterStack INIT {}
   DATA FForKey
   DATA FForKeyBlock
   DATA FKeyField
   DATA FName
   DATA FMasterKeyField
   DATA FRightJustified INIT .F.
   DATA FScopeBottom
   DATA FScopeTop
   DATA FUniqueKeyField
   METHOD DbGoBottomTop( n )
   METHOD GetArrayKeyFields INLINE ::KeyField:FieldMethod
   METHOD GetAutoIncrement INLINE ::FAutoIncrementKeyField != NIL
   METHOD GetField
   METHOD GetMasterKeyVal( keyField )
   METHOD GetScope INLINE iif( ::FScopeBottom == NIL .AND. ::FScopeTop == NIL, NIL, { ::FScopeTop, ::FScopeBottom } )
   METHOD GetScopeBottom INLINE iif( !Empty( ::FScopeBottom ), ::FScopeBottom, "" )
   METHOD GetScopeTop INLINE iif( !Empty( ::FScopeTop ), ::FScopeTop, "" )
   METHOD GetUnique INLINE ::FUniqueKeyField != NIL
   METHOD SetCaseSensitive( CaseSensitive ) INLINE ::FCaseSensitive := CaseSensitive
   METHOD SetCustom( Custom )
   METHOD SetDescend( Descend ) INLINE ::FDescend := Descend
   METHOD SetField( nIndex, XField )
   METHOD SetForKey( ForKey ) BLOCK ;
        {|Self,ForKey|
            IF valType( ForKey ) = "B"
                ::FForKeyBlock := ForKey
            ELSE
                IF Empty( ForKey )
                    ::FForKeyBlock := NIL
                ELSE
                    ::FForKeyBlock := &("{||" + ForKey + "}")
                ENDIF
            ENDIF
            ::FForKey := ForKey
            RETURN ForKey
        }
   METHOD SetRightJustified( rightJust ) INLINE ::FRightJustified := rightJust
   METHOD SetScope( value )
   METHOD SetScopeBottom( value )
   METHOD SetScopeTop( value )

PROTECTED:

   CLASSDATA indexCreationList INIT {}

   DATA FCustomIndexExpression
   DATA FIndexType
   DATA FKeyFlags

   METHOD getBagName INLINE ::FTable:alias:dbOrderInfo( DBOI_BAGNAME, ::FtagName )

   METHOD closeTemporary()

   METHOD CreateIndex()

   METHOD CustomKeyExpValue()

   METHOD SetCustomIndexExpression( customIndexExpression )

PUBLIC:

    DATA customKeyBlock
    DATA customKeyLen
   DATA temporary INIT .F.

   DATA WarnMsg

   METHOD New( Table, tagName, name, indexType, curClass, warnMsg ) CONSTRUCTOR

   DESTRUCTOR onDestruct()

   METHOD __Seek( direction, keyValue, lSoftSeek )
   METHOD AddIndex

   METHOD bindIndex( reusing, indexType, curClass )

   METHOD closeIndex()
   METHOD COUNT( bForCondition, bWhileCondition )
   METHOD CustomKeyUpdate
   METHOD DbFilterPush( ignoreMasterKey )
   METHOD DbFilterPull()
   METHOD DbGoBottom INLINE ::DbGoBottomTop( -1 )
   METHOD DbGoTop INLINE ::DbGoBottomTop( 1 )
   METHOD dbSkip( numRecs, lSkipUnique )
   METHOD existsKey( keyValue, recNo )
   METHOD GetKeyVal( keyVal )
   METHOD FillCustomIndex()
   METHOD Get4Seek( blk, keyVal, softSeek )
   METHOD Get4SeekLast( blk, keyVal, softSeek )
   METHOD GetCurrentRecord()
   METHOD HasFilter() INLINE ::FDbFilter != NIL
   METHOD IndexExpression()
   METHOD InsideScope( ignoreFilters )
   METHOD KeyExpression()
   METHOD MasterKeyExpression()

   METHOD openIndex()

   METHOD ordKeyNo() INLINE ::FTable:alias:ordKeyNo()

   METHOD RawGet4Seek( direction, blk, keyVal, softSeek )
   METHOD RawSeek( Value )

   METHOD SetDbFilter( dbFilter )
   METHOD SetKeyVal( keyVal, lSoftSeek )

   PROPERTY __autoIncrementBase
   PROPERTY Bof READ FTable:Bof
   PROPERTY DbFilter READ FDbFilter WRITE SetDbFilter
   PROPERTY DbFilterRAW
   PROPERTY Eof READ FTable:Eof
   PROPERTY Found READ FTable:Found
   PROPERTY IndexType READ FIndexType
   PROPERTY KeyFlags READ FKeyFlags
   PROPERTY KeyVal READ GetKeyVal WRITE SetKeyVal
   PROPERTY opened INIT .F.
   PROPERTY RecNo READ FTable:RecNo
   PROPERTY Scope READ GetScope WRITE SetScope
   PROPERTY ScopeBottom READ GetScopeBottom WRITE SetScopeBottom
   PROPERTY ScopeTop READ GetScopeTop WRITE SetScopeTop

   METHOD SEEK( keyValue, lSoftSeek ) INLINE ::__Seek( 0, keyValue, lSoftSeek )
   METHOD SeekLast( keyValue, lSoftSeek ) INLINE ::__Seek( 1, keyValue, lSoftSeek )

   PROPERTY useIndex READWRITE /* index to use when poblating a custom index */

PUBLISHED:

   PROPERTY AutoIncrement READ GetAutoIncrement
   PROPERTY AutoIncrementKeyField INDEX 1 READ GetField WRITE SetField
   PROPERTY bagName READ getBagName
   PROPERTY CaseSensitive READ FCaseSensitive WRITE SetCaseSensitive
   PROPERTY Custom READ FCustom
   PROPERTY CustomIndexExpression READ FCustomIndexExpression
   PROPERTY Descend READ FDescend WRITE SetDescend
   PROPERTY fileName
   PROPERTY ForKey READ FForKey WRITE SetForKey
   PROPERTY IsPrimaryIndex
   PROPERTY KeyField INDEX 3 READ GetField WRITE SetField
   PROPERTY UniqueKeyField INDEX 2 READ GetField WRITE SetField
   PROPERTY Name READ FName
   PROPERTY MasterKeyField INDEX 0 READ GetField WRITE SetField
   PROPERTY MasterKeyVal READ GetMasterKeyVal
   PROPERTY RightJustified READ FRightJustified WRITE SetRightJustified
   PROPERTY Table
   PROPERTY TableBaseClass
   PROPERTY TagName
   PROPERTY UNIQUE READ GetUnique

ENDCLASS

/*
    New
*/
METHOD New( Table, tagName, name, indexType, curClass, warnMsg ) CLASS TIndex

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
    onDestruct
*/
METHOD PROCEDURE onDestruct() CLASS TIndex

    ::closeTemporary()

RETURN

/*
    __Seek
*/
METHOD FUNCTION __Seek( direction, keyValue, lSoftSeek ) CLASS TIndex

   LOCAL ALIAS

   alias := ::FTable:alias

   IF AScan( { dsEdit, dsInsert }, ::FTable:State ) > 0
      ::FTable:Post()
   ENDIF

   keyValue := ::KeyField:GetKeyVal( keyValue )

   IF direction = 0
      alias:Seek( ::getMasterKeyVal + keyValue, ::FTagName, lSoftSeek )
   ELSE
      alias:SeekLast( ::getMasterKeyVal + keyValue, ::FTagName, lSoftSeek )
   ENDIF

   ::GetCurrentRecord()

   RETURN ::Found

/*
    AddIndex
*/
METHOD AddIndex( cMasterKeyField, ai, un, cKeyField, keyFlags, ForKey, cs, de, acceptEmptyUnique, useIndex, temporary, rightJust, cu, default )

   IF hb_isHash( keyFlags )
      ::FKeyFlags := keyFlags
   ENDIF

   ::MasterKeyField := cMasterKeyField

   /* Check if needs to add the primary index key */
   IF ::FTable:PrimaryIndex == Self
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
      ::FCustom := iif( HB_ISNIL( cu ), .F. , cu )
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
   ::CaseSensitive := iif( HB_ISNIL( cs ), .F., cs )
   ::RightJustified := rightJust == .T.
   ::Descend := iif( HB_ISNIL( de ), .F., de )
   ::FuseIndex := useIndex
   ::temporary := temporary == .T.

   ::FTable:addIndexMessage( ::name, default )

   RETURN Self

/*
    bindIndex
*/
METHOD PROCEDURE bindIndex( reusing, indexType, curClass ) CLASS TIndex
    LOCAL class
    LOCAL index

    /* on reusing must remove index on parent classes */
    IF reusing
        FOR EACH class IN ::Ftable:indexList
            FOR EACH index IN class
                IF index:__enumKey == ::Fname .AND. index == self
//                    hb_hDel( class, index:__enumKey )
                    EXIT
                ENDIF
            NEXT
        NEXT
        FOR EACH index IN ::Ftable:primaryIndexList
            IF index == ::Fname
//                hb_hDel( ::Ftable:primaryIndexList, index:__enumKey )
                EXIT
            ENDIF
        NEXT
    ENDIF

    ::FIndexType := indexType

    IF curClass = NIL
        curClass := ::FTable:ClassName()
    ENDIF

    ::FTableBaseClass := curClass

    IF !hb_HHasKey( ::FTable:IndexList, curClass )
        ::FTable:IndexList[ curClass ] := hb_HSetOrder( hb_HSetCaseMatch( { => }, .F. ), .T. )
    ENDIF

    ::FTable:IndexList[ curClass, ::Fname ] := Self

    ::FIsPrimaryIndex := indexType = "PRIMARY"

    IF ::FIsPrimaryIndex
        ::FTable:SetPrimaryIndexList( curClass, ::Fname )
        ::FTable:SetPrimaryIndex( Self )
    ENDIF

RETURN

/*
    closeIndex
*/
METHOD PROCEDURE closeIndex() CLASS TIndex
    IF ::temporary
        ::closeTemporary()
    ENDIF
RETURN

/*
    closeTemporary
*/
METHOD PROCEDURE closeTemporary() CLASS TIndex
    LOCAL fileName

    IF ::temporary .AND. ::Fopened
        IF hb_isObject( ::FTable )
            fileName := ::FTable:alias:dbOrderInfo( DBOI_FULLPATH, nil, ::FtagName )
            ::FTable:alias:ordDestroy( ::FtagName )
            ::FTagName := nil
            IF hb_fileExists( fileName )
                fErase( fileName )
            ENDIF
            ::Fopened := .f.
        ENDIF
    ENDIF

RETURN

/*
    Count
*/
METHOD FUNCTION COUNT( bForCondition, bWhileCondition ) CLASS TIndex

   LOCAL nCount := 0

   ::FTable:dbEval( {|| ++nCount }, bForCondition, bWhileCondition, Self )

   RETURN nCount

/*
    CreateIndex
*/
METHOD FUNCTION CreateIndex() CLASS TIndex
    LOCAL indexExp
    LOCAL recNo
    LOCAL forKey
    LOCAL forKeyBlock
    LOCAL whileBlock := NIL
    LOCAL evalBlock := NIL
    LOCAL intervalVal := NIL
    LOCAL additive := .T.
    LOCAL useCurrent := .F.
    LOCAL bagFileName
    LOCAL unique := .F.
    LOCAL oErr

    recNo := ::FTable:Alias:RecNo

    IF ::custom
        IF ::customKeyLen = nil
            indexExp := E"\"" + Replicate( "#", Len( ::getMasterKeyVal ) + Len( ::KeyVal ) ) + E"\""
        ELSE
            indexExp := E"\"" + Replicate( "#", ::customKeyLen ) + E"\""
        ENDIF
    ELSE
        indexExp := ::IndexExpression()
    ENDIF

    IF indexExp != nil

        IF !Empty( ::ForKey )
            IF valType( ::ForKey ) != "B" /* on custom index, for key is a codeblock evaluated on FillCustomIndex  */
                forKey := ::ForKey
                forKeyBlock := &( "{||" + ::ForKey + "}" )
            ENDIF
        ENDIF

        dbSelectArea( ::FTable:Alias:Name )  // here because ::IndexExpression() may change active WA

        ordCondSet( ;
            forKey, ;
            forKeyBlock, ;
            NIL, ;
            whileBlock, ;
            evalBlock, ;
            intervalVal, ;
            NIL, ;
            NIL, ;
            NIL, ;
            NIL, ;
            ::Descend, ;
            NIL, ;
            additive, ;
            useCurrent, ;
            ::Custom, ;
            NIL, ;
            NIL, ;
            ::temporary )

        BEGIN SEQUENCE WITH ::FTable:ErrorBlock

            IF ::temporary
                fClose( hb_fTempCreateEx( @bagFileName, nil, "tmp", ::FTable:alias:dbOrderInfo( DBOI_BAGEXT ) ) )
                hb_FNameSplit( bagFileName, nil, @::FtagName, nil, nil )
            ENDIF

            ordCreate( bagFileName, ::tagName, indexExp, indexExp, unique )

            ::FfileName := ::FTable:alias:dbOrderInfo( DBOI_FULLPATH, nil, ::tagName )

            ::Fopened := .t.

            IF ::Custom
                ::FillCustomIndex()
            ENDIF

        RECOVER USING oErr

            ui_Alert( ;
                "CreateIndex() Error in " + ::FTable:ClassName + ", Table: " + ::FTable:TableFileName + ";" + ;
                " Index Tag Name: " + ::TagName + ";" + ;
                "IndexExpression: " + indexExp + ";" + ;
                "  Index For Key: " + AsString( forKey ) ;
                )

            Break( oErr )

        END SEQUENCE

    ENDIF

    ::FTable:Alias:RecNo := recNo

RETURN .t.

/*
    CustomKeyExpValue
*/
METHOD FUNCTION CustomKeyExpValue() CLASS TIndex

   IF ::MasterKeyField = NIL
      RETURN ::KeyVal
   ENDIF

   RETURN ::MasterKeyField:KeyVal + ::KeyVal

/*
    CustomKeyUpdate
*/
METHOD PROCEDURE CustomKeyUpdate CLASS TIndex
    LOCAL customKeyValue

    IF ::FCustom .AND. ::Fopened
        WHILE ::FTable:Alias:ordKeyDel( ::FTagName ) ; ENDDO
        IF ::customKeyBlock = nil
            customKeyValue := ::CustomKeyExpValue()
        ELSE
            customKeyValue := ::FTable:alias:eval( ::customKeyBlock, ::FTable:displayFieldList )
        ENDIF
        IF Empty( ::FForKeyBlock ) .OR. ::FTable:Alias:Eval( ::FForKeyBlock, ::FTable )
            ::FTable:Alias:ordKeyAdd( ::FTagName, , customKeyValue )
        ENDIF
    ENDIF

RETURN

/*
    DbFilterPull
*/
METHOD PROCEDURE DbFilterPull() CLASS TIndex

   ::FDbFilter := ATail( ::FDbFilterStack )
   hb_ADel( ::FDbFilterStack, Len( ::FDbFilterStack ), .T. )
   ::FTable:DbFilterPull()

   RETURN

/*
    DbFilterPush
*/
METHOD PROCEDURE DbFilterPush( ignoreMasterKey ) CLASS TIndex

   AAdd( ::FDbFilterStack, ::FDbFilter )
   ::FDbFilter := NIL
   ::FTable:DbFilterPush( ignoreMasterKey )

   RETURN

/*
    DbGoBottomTop
*/
METHOD FUNCTION DbGoBottomTop( n ) CLASS TIndex

   LOCAL masterKeyVal := ::getMasterKeyVal
   LOCAL alias

   alias := ::FTable:alias

   IF n = 1
      IF ::GetScopeTop() == ::GetScopeBottom()
         alias:Seek( masterKeyVal + ::GetScopeTop(), ::FTagName )
      ELSE
         alias:Seek( masterKeyVal + ::GetScopeTop(), ::FTagName, .T. )
      ENDIF
   ELSE
      IF ::GetScopeTop() == ::GetScopeBottom()
         alias:SeekLast( masterKeyVal + ::GetScopeBottom(), ::FTagName )
      ELSE
         alias:SeekLast( masterKeyVal + ::GetScopeBottom(), ::FTagName, .T. )
      ENDIF
   ENDIF

   IF ::HasFilter() .OR. ::FTable:HasFilter()
      ::DbFilterPush()
      ::GetCurrentRecord()
      ::DbFilterPull()
      IF ::Eof() .OR. ( !::FTable:FilterEval( Self ) .AND. !::FTable:SkipFilter( n, Self ) )
         ::FTable:dbGoto( 0 )
         RETURN .F.
      ENDIF
   ENDIF

   RETURN ::GetCurrentRecord()

/*
    DbSkip
*/
METHOD FUNCTION dbSkip( numRecs, lSkipUnique ) CLASS TIndex

   LOCAL result
   LOCAL n

   IF !::HasFilter() .AND. !::FTable:HasFilter()
      IF lSkipUnique = .T.
         result := .T.
         WHILE numRecs != 0 .AND. ! ::Ftable:eof() .AND. result
            IF numRecs > 0
                ::seekLast( ::keyVal )
                n := 1
            ELSE
                ::seek( ::keyVal )
                n := -1
            ENDIF
            result := ::FTable:alias:dbSkip( n, ::FTagName )
            numRecs += - ( n )
         ENDDO
      ELSE
         result := ::FTable:alias:dbSkip( numRecs, ::FTagName ) /* because on Bof returns .F. */
      ENDIF
      ::GetCurrentRecord()
      RETURN result .AND. ::InsideScope()
   ENDIF

   RETURN ::FTable:SkipFilter( numRecs, Self )

/*
    existsKey
*/
METHOD FUNCTION existsKey( keyValue, recNo ) CLASS TIndex
   RETURN ::FTable:alias:existsKey( ::getMasterKeyVal + ::KeyField:GetKeyVal( keyValue ), ::FTagName, recNo )

/*
    FillCustomIndex
*/
METHOD PROCEDURE FillCustomIndex() CLASS TIndex
    LOCAL index

    ::FTable:StatePush()

    IF ::FuseIndex = nil
        index := ::FTable:BaseKeyIndex
        ::FTable:DbFilterPush( .T. )
    ELSE
        SWITCH valType( ::FuseIndex )
        CASE 'C'
            index := ::FTable:indexByName( ::FuseIndex )
            EXIT
        CASE 'O'
            index := ::FuseIndex
            EXIT
        ENDSWITCH
    ENDIF

    IF index != NIL
        index:dbGoTop()
        WHILE !index:Eof() .AND. inkey() != K_ESC
            ::CustomKeyUpdate()
            index:dbSkip()
        ENDDO
    ENDIF

    IF ::FuseIndex = nil
        ::FTable:DbFilterPull()
    ENDIF

    ::FTable:StatePull()

RETURN

/*
    Get4Seek
*/
METHOD FUNCTION Get4Seek( blk, keyVal, softSeek ) CLASS TIndex
   RETURN ::RawGet4Seek( 1, blk, keyVal, softSeek )

/*
    Get4SeekLast
*/
METHOD FUNCTION Get4SeekLast( blk, keyVal, softSeek ) CLASS TIndex
   RETURN ::RawGet4Seek( 0, blk, keyVal, softSeek )

/*
    GetCurrentRecord
*/
METHOD FUNCTION GetCurrentRecord() CLASS TIndex
   LOCAL result
   LOCAL index := ::FTable:Index

   ::FTable:Index := Self

   result := ::FTable:GetCurrentRecord()

   ::FTable:Index := index

RETURN result

/*
    GetField
*/
METHOD FUNCTION GetField( nIndex ) CLASS TIndex

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
    GetKeyVal
*/
METHOD FUNCTION GetKeyVal( keyVal ) CLASS TIndex

   IF ::FKeyField == NIL
      RETURN ""
   ENDIF

   RETURN ::FKeyField:GetKeyVal( keyVal, ::FKeyFlags )

/*
    GetMasterKeyVal
*/
METHOD FUNCTION GetMasterKeyVal( keyField ) CLASS TIndex

   LOCAL itm
   LOCAL FIELD
   LOCAL keyVal := ""

   IF keyField = NIL
      keyField := ::FMasterKeyField
   ENDIF

   IF keyField == NIL
      RETURN keyVal // ::FTable:MasterKeyString
   ENDIF

   IF keyField:FieldMethodType = "A"
      FOR EACH itm IN keyField:FieldArrayIndex
         keyVal += ::GetMasterKeyVal( ::FTable:FieldList[ itm ] )
      NEXT
   ELSE
      keyVal := keyField:defaultValue
      IF ::FTable:MasterSource != nil .AND. keyVal = NIL .AND. ( field := ::FTable:FindMasterSourceField( keyField ) ) != NIL .AND. ! field:Calculated
         /* field has to be not calculated */
         keyVal := field:GetKeyVal( NIL, ::FKeyFlags )
      ELSE
         IF keyVal = nil
            IF ::eof()
                keyVal := keyField:emptyValue
            ELSE
                keyVal := keyField:keyVal
            ENDIF
         ENDIF
         keyVal := keyField:GetKeyVal( keyVal, ::FKeyFlags )
      ENDIF
   ENDIF

   RETURN keyVal

/*
    IndexExpression
*/
METHOD FUNCTION IndexExpression() CLASS TIndex

   LOCAL exp
   LOCAL keyExp

   IF ::FCustomIndexExpression = NIL
      exp := ::MasterKeyExpression
      keyExp := ::KeyExpression
      IF !Empty( keyExp )
         exp += iif( Len( exp ) = 0, "", "+" ) + keyExp
      ENDIF
   ELSE
      exp := ::FCustomIndexExpression
   ENDIF

   RETURN exp

/*
    InsideScope
*/
METHOD FUNCTION InsideScope( ignoreFilters ) CLASS TIndex

   LOCAL masterKeyVal
   LOCAL scopeVal
   LOCAL keyValue

   IF ::FTable:Alias:KeyVal( ::FTagName ) = NIL
      RETURN .F.
   ENDIF

   keyValue := ::FTable:alias:KeyVal( ::FTagName )

   IF keyValue == NIL .OR. ( !ignoreFilters == .T. .AND. !::FTable:FilterEval( Self ) )
      RETURN .F.
   ENDIF

   masterKeyVal := ::getMasterKeyVal

   scopeVal := ::GetScope()

   IF scopeVal == NIL
      RETURN masterKeyVal == "" .OR. keyValue = masterKeyVal
   ENDIF

RETURN keyValue >= ( masterKeyVal + ::GetScopeTop() ) .AND. ;
      keyValue <= ( masterKeyVal + ::GetScopeBottom() )

/*
    KeyExpression
*/
METHOD FUNCTION KeyExpression() CLASS TIndex

   IF ::FKeyField != NIL
      RETURN ::FKeyField:IndexExpression
   ENDIF

   RETURN ""

/*
    MasterKeyExpression
*/
METHOD FUNCTION MasterKeyExpression() CLASS TIndex

   IF ::FMasterKeyField != NIL
      RETURN ::FMasterKeyField:IndexExpression( NIL, .T., ::KeyFlags )
   ENDIF

   RETURN ""

/*
    openIndex
*/
METHOD PROCEDURE openIndex() CLASS TIndex
    LOCAL index
    LOCAL processing

    IF ::temporary .AND. ::Fopened
        ::closeTemporary()
    ENDIF

    IF ! ::Fopened
        IF empty( ::tagName ) .OR. ::FTable:Alias:ordNumber( ::TagName ) = 0
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
    RawGet4Seek
*/
METHOD FUNCTION RawGet4Seek( direction, blk, keyVal, softSeek ) CLASS TIndex

   IF keyVal = NIL
      keyVal := ::getMasterKeyVal
   ELSE
      keyVal := ::getMasterKeyVal + keyVal
   ENDIF

   RETURN ::FTable:alias:RawGet4Seek( direction, blk, keyVal, ::FTagName, softSeek )

/*
    RawSeek
*/
METHOD FUNCTION RawSeek( Value ) CLASS TIndex

   IF AScan( { dsEdit, dsInsert }, ::FTable:State ) > 0
      ::FTable:Post()
   ENDIF

   ::FTable:alias:Seek( Value, ::FTagName )

   ::GetCurrentRecord()

   RETURN ::FTable:Found()

/*
    SetCustom
*/
METHOD PROCEDURE SetCustom( Custom ) CLASS TIndex

   ::FCustom := Custom

   ::FTable:Alias:ordCustom( ::FTagName, , Custom )

   RETURN

/*
    SetCustomIndexExpression
*/
METHOD PROCEDURE SetCustomIndexExpression( customIndexExpression ) CLASS TIndex

   ::FCustomIndexExpression := customIndexExpression
   ::FCustom := .T.
   ::FTable:AddCustomIndex( Self )

   RETURN

/*
    SetDbFilter
*/
METHOD FUNCTION SetDbFilter( dbFilter ) CLASS TIndex
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
    OTHERWISE
        ::Invalid_DbFilter_Value()
    ENDSWITCH
RETURN ::FDbFilter

/*
    SetField
*/
METHOD PROCEDURE SetField( nIndex, XField ) CLASS TIndex

   LOCAL AField
   LOCAL fld
   LOCAL fieldBlock
   LOCAL isCustomIndex := .F.

   SWITCH ValType( XField )
   CASE 'C'
      AField := ::FTable:FieldByName( XField )
      IF AField = NIL .AND. ":" $ XField

          fieldBlock := ::FTable:BuildFieldBlockFromFieldExpression( XField, "Value", @fld )

         IF fieldBlock = NIL
            RAISE ERROR "Error building (COMPOUND) Index Field '" + XField + "' ..."
            RETURN
         ENDIF

         AField := __DynSN2Sym( fld:ClassName ):Exec():New( ::FTable, ::FTableBaseClass )
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
      AField := TFieldString():New( ::FTable, ::FTableBaseClass )
      AField:FieldMethod := XField
      AField:Published := .F.
      fld := ::FTable:FieldByName( AField:Name )
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
   IF ::FTable:PrimaryIndex == Self /* check if index is the Primary index */
      IF !HB_ISNIL( AField )
         AField:PrimaryKeyComponent := .T.
      ENDIF
   ENDIF

   IF hb_isNil( AField )
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

   /* Assign MasterField value to the TTable object field */
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
   CASE 2  /* UniqueKeyField */
      AAdd( AField:UniqueKeyIndexList, Self )
      IF AField:FieldMethodType = 'A'
         AField:Table:FieldByName( AField:Name, @fld )
         AField:Table:FieldList[ AField:FieldArrayIndex[ Len( AField:FieldArrayIndex ) ] ]:LastUniqueFieldList := fld
      ENDIF
      ::FUniqueKeyField := AField
   CASE 3  /* KeyField */
      IF AField:IsDerivedFrom( "TFieldString" ) .AND. Len( AField ) = 0
         RAISE ERROR ::FTable:ClassName + ": Master key field <" + AField:Name + "> needs a size > zero..."
      ENDIF
      AField:AddKeyIndex( Self )
      ::FKeyField := AField
      IF ::FIsPrimaryIndex .AND. ::FTable:BaseKeyIndex = NIL
         ::FTable:SetBaseKeyIndex( Self )
      ENDIF
      EXIT
   ENDSWITCH

   RETURN

/*
    SetKeyVal
*/
METHOD FUNCTION SetKeyVal( keyVal, lSoftSeek ) CLASS TIndex

   RETURN ::FKeyField:SetKeyVal( keyVal, lSoftSeek )

/*
    SetScope
*/
METHOD FUNCTION SetScope( value ) CLASS TIndex

   LOCAL oldValue := { ::FScopeTop, ::FScopeBottom }

   IF ValType( value ) = "A" // scope by field
      ::FScopeTop := value[ 1 ]
      ::FScopeBottom := value[ 2 ]
   ELSE
      ::FScopeTop := value
      ::FScopeBottom := value
   ENDIF

   RETURN oldValue

/*
    SetScopeBottom
*/
METHOD FUNCTION SetScopeBottom( value ) CLASS TIndex

   LOCAL oldValue := ::FScopeBottom

   ::FScopeBottom := value

   RETURN oldValue

/*
    SetScopeTop
*/
METHOD FUNCTION SetScopeTop( value ) CLASS TIndex

   LOCAL oldValue := ::FScopeTop

   ::FScopeTop := value

   RETURN oldValue

/*
    End Class TIndex
*/
