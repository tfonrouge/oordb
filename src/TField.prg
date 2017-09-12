/*
 *
 */

#include "oordb.ch"
#include "xerror.ch"

/*
    TODO: Check for a correct validation for FieldExpression, it can contain any valid
                Harbour statement/formula, and loose checking is done on SetFieldMethod
*/

/*
    TField
*/
CLASS TField FROM OORDBBASE

   PRIVATE:

   DATA FEnabled INIT .F.
   DATA FAutoIncrementKeyIndex
   DATA FDescription INIT ""
   DATA FFieldCodeBlock         // Code Block
   DATA FFieldWriteBlock         // Code Block to do WRITE
   DATA FGroup              // A Text label for grouping
   DATA FIsMasterFieldComponent INIT .F. // Field is a MasterField component
   DATA FPrimaryKeyComponent INIT .F.  // Field is included in a Array of fields for a Primary Index Key
   DATA FPublished INIT .T.       // Logical: Appears in user field selection
   DATA FReadOnly INIT .F.
   DATA FRequired INIT .F.
   /* TODO: remove or fix validations, i.e. when reusing a primary index field */
   DATA FReUseField INIT .F.
   DATA FReUseFieldIndex
   DATA FUniqueKeyIndexList INIT {}

   METHOD GetAutoIncrement INLINE ::FAutoIncrementKeyIndex != NIL
   METHOD GetAutoIncrementValue()
   METHOD GetFieldMethod
   METHOD GetIsPrimaryKeyField INLINE ::Table:KeyField == Self
   METHOD GetUnique INLINE !Empty( ::FUniqueKeyIndexList )
   METHOD SetAutoIncrementKeyIndex( index ) INLINE ::FAutoIncrementKeyIndex := index
   METHOD SetDescription( Description ) INLINE ::FDescription := Description
   METHOD SetGroup( Group ) INLINE ::FGroup := Group
   METHOD SetIsMasterFieldComponent( IsMasterFieldComponent )
   METHOD SetName( name )
   METHOD SetPrimaryKeyComponent( PrimaryKeyComponent )
   METHOD SetPublished( Published ) INLINE ::FPublished := Published
   METHOD SetReadOnly( ReadOnly ) INLINE ::FReadOnly := ReadOnly
   METHOD SetUsingField( usingField )

   PROTECTED:

   DATA FBuffer
   DATA Fbuffered INIT .F.
   DATA FcalcResult
   DATA FCalculated INIT .F.
   DATA FcalculatedRecNo
   DATA FChanged INIT .F.
   DATA FdefaultIndexName
   DATA FDefaultValue
   DATA FDBS_DEC INIT 0
   DATA FDBS_LEN
   DATA FDBS_NAME
   DATA FDBS_TYPE
   DATA FdefaultValueList
   DATA FEditable
   DATA FhasBinary
   DATA FonAfterChangeAssigned
   DATA FonBeforeChangeAssigned
   DATA FFieldArrayIndex        // Array of TField's indexes in FieldList
   DATA FFieldExpression         // Literal Field expression on the Database
   DATA FFieldMethodType
   DATA FFieldReadBlock        // Code Block to do READ
   DATA FFieldType     INIT ftBase
   DATA FIndexExpression
   DATA FIndexKeyList  INIT {}
   DATA FIndexMasterAutoIncKey INIT .F.
   DATA FLabel
   DATA FLastUniqueFieldList
   DATA FModStamp INIT .F.       // Field is automatically mantained (dbf layer)
   DATA FName INIT ""
   DATA FNewValue
   DATA FOnEvalFieldWriteBlock
   DATA FOnReset INIT .F.
   DATA FOnSetKeyValBlock
   DATA FOnSetValue
   DATA FrevertingValue
   DATA FTable
   DATA FTableBaseClass
   DATA FType INIT "TField"
   DATA FtypeNameList
   DATA FUsingField      // Field used on Calculated Field
   DATA FValidValues
   DATA FValType INIT "U"
   DATA FWrittenValue

   METHOD CheckForValidValue( value, showAlert, errorStr )
   METHOD GetAsExpression INLINE hb_StrToExp( ::GetAsString )
   METHOD GetCloneData( cloneData )
   METHOD GetDBS_LEN INLINE ::FDBS_LEN
   METHOD GetDBS_TYPE INLINE ::FDBS_TYPE
   METHOD GetDefaultNewValue( index )
   METHOD GetEditable
   METHOD GetEnabled()
   METHOD GetEmptyValue BLOCK {|| NIL }
   METHOD GetFieldArray()
   METHOD GetFieldReadBlock()
   METHOD GetKeyIndex()
   METHOD GetLabel INLINE iif( ::FLabel == NIL, ::FName, ::FLabel )
   METHOD GetLinkedTable() INLINE NIL
   METHOD GetReadOnly INLINE ::FReadOnly
   METHOD GetUndoValue()
   METHOD GetValidValues()
   METHOD OnSetKeyVal( lSeek, keyVal )
   METHOD SetAsString( string ) INLINE ::SetAsVariant( string )
   METHOD SetBuffer( value, lNoCheckValidValue )
   METHOD SetDBS_DEC( dec ) INLINE ::FDBS_DEC := dec
   METHOD SetDBS_LEN( dbs_Len ) INLINE ::FDBS_LEN := dbs_Len
   METHOD SetDBS_TYPE( dbs_Type ) INLINE ::FDBS_TYPE := dbs_Type
   METHOD SetCloneData( cloneData )
   METHOD SetDefaultNewValue( index, value )
   METHOD SetEditable( editable ) INLINE ::FEditable := editable
   METHOD SetEnabled( enabled )
   METHOD SetLabel( label ) INLINE ::FLabel := label
   METHOD SetLastUniqueFieldList( fld ) INLINE AAdd( iif( ::FLastUniqueFieldList = NIL, ::FLastUniqueFieldList := {}, ::FLastUniqueFieldList ), fld )
   METHOD SetRequired( Required ) INLINE ::FRequired := Required
   METHOD SetReUseField( reUseField ) INLINE ::FReUseField := reUseField
   METHOD WriteToTable( value, initialize )

   PUBLIC:

   DATA AcceptEmptyUnique INIT .F.

   DATA cargo

   DATA nameAlias
   DATA PICTURE

   CONSTRUCTOR New( Table, curBaseClass )

   // ON ERROR FUNCTION OODB_ErrorHandler( ... )

   METHOD AddFieldMessage()
   METHOD AddIndexKey( index )
   METHOD CheckForKeyViolation( value )
   METHOD Clear( clearToNIL )
   METHOD dbGoTop( ... ) INLINE ::keyIndex:dbGoTop( ... )
   METHOD dbGoBottom( ... ) INLINE ::keyIndex:dbGoBottom( ... )
   METHOD DefaultValuePull()
   METHOD DefaultValuePush( newDefaultValue )
   METHOD DELETE()
   METHOD GetAsString() INLINE "<" + ::ClassName + ">"
   METHOD GetAsUTF8 INLINE hb_StrToUTF8( ::GetAsString() )
   METHOD GetAsDisplay( ... )
   METHOD GetAsDisplayEmptyValue INLINE ::GetEmptyValue
   METHOD GetAsVariant( ... )
   METHOD GetBuffer()
   METHOD GetData( initialize )
   METHOD GetKeyVal( keyVal, keyFlags )
   METHOD hasAsDisplay INLINE  valType( ::ValidValues ) = "H" .OR. ::DisplayBlock != NIL
   METHOD IndexExpression VIRTUAL
   METHOD IsReadOnly()
   METHOD IsTableField()
   METHOD Reset( initialize )
   METHOD revertValue()
   METHOD seek( ... ) INLINE ::keyIndex:seek( ... )
   METHOD seekLast( ... ) INLINE ::keyIndex:seekLast( ... )
   METHOD SetAsVariant( value )
   METHOD setBuffered( buffered ) INLINE ::Fbuffered := buffered
   METHOD SetData( value, initialize )
   METHOD SetDbStruct( aStruct )
   METHOD SetDefaultIndexName( defaultIndexName )
   METHOD SetFieldMethod( FieldMethod, calculated )
   METHOD SetFieldReadBlock( readBlock ) INLINE ::FFieldReadBlock := readBlock
   METHOD SetFieldWriteBlock( writeBlock )
   METHOD setHasBinary( bin ) INLINE ::FhasBinary := bin
   METHOD SetIndexExpression( indexExpression ) INLINE ::FIndexExpression := indexExpression
   METHOD setIndexMasterAutoIncKey( index )
   METHOD SetKeyVal( keyVal, lSoftSeek )
   METHOD SetKeyValBlock( keyValBlock ) INLINE ::FOnSetKeyValBlock := keyValBlock
   METHOD SetValidValues( validValues, ignoreUndetermined )
   METHOD SetValueToLinkedObjField( value )
   METHOD TranslateToFieldValue( value ) INLINE value
   METHOD TranslateToValue( value ) INLINE value
   METHOD Validate( showAlert ) INLINE ::ValidateResult( showAlert ) = NIL
   METHOD ValidateResult( showAlert, value ) BLOCK ;
      {|Self,showAlert,value|
         LOCAL result

         IF value = NIL
            value := ::GetAsVariant()
         ENDIF

         result := ::ValidateResult_TableLogic( showAlert, value )

         IF Empty( result )
            result := ::ValidateResult_OnValidate( showAlert, value )
         ENDIF

         RETURN result
      }

   METHOD ValidateResult_TableLogic( showAlert, value )
   METHOD ValidateResult_OnValidate( showAlert, value )
   METHOD ValidateFieldInfo VIRTUAL

   PROPERTY AsDisplay READ GetAsDisplay
   PROPERTY AsExpression READ GetAsExpression
   PROPERTY AsString READ GetAsString WRITE SetAsString
   PROPERTY AsUTF8 READ GetAsUTF8
   PROPERTY AsVariant READ GetAsVariant WRITE SetAsVariant
   PROPERTY Calculated READ FCalculated
   PROPERTY CloneData READ GetCloneData WRITE SetCloneData
   PROPERTY customIndexExpression READ FIndexExpression
   PROPERTY DisplayBlock READWRITE
   PROPERTY EmptyValue READ GetEmptyValue
   PROPERTY FieldArrayIndex READ FFieldArrayIndex
   PROPERTY hasBinary READ FhasBinary
   PROPERTY ignoreUndetermined
   PROPERTY KeyVal READ GetKeyVal WRITE SetKeyVal
   PROPERTY LastUniqueFieldList READ FLastUniqueFieldList WRITE SetLastUniqueFieldList
   PROPERTY LinkedTable READ GetLinkedTable
   PROPERTY ReUseField READ FReUseField WRITE SetReUseField
   PROPERTY ReUseFieldIndex READ FReUseFieldIndex
   PROPERTY IsKeyIndex READ KeyIndex != NIL
   PROPERTY IsMasterFieldComponent READ FIsMasterFieldComponent WRITE SetIsMasterFieldComponent
   PROPERTY IsPrimaryKeyField READ GetIsPrimaryKeyField
   PROPERTY RawDefaultValue READ FDefaultValue
   PROPERTY RawNewValue READ FNewValue
   PROPERTY Size
   PROPERTY UndoValue READ GetUndoValue
   PROPERTY ValidValues READ GetValidValues WRITE SetValidValues
   PROPERTY Value READ GetAsVariant( ... ) WRITE SetAsVariant
   PROPERTY WrittenValue READ FWrittenValue

   PUBLISHED:

   DATA IncrementBlock
   DATA nameAliasPublished INIT .T.
    /*
     * Event holders
     */
   DATA OnGetText   // Params: Sender: TField, Text: String
   DATA OnSearch   // Search in indexed field
   DATA OnSetText   // Params: Sender: TField, Text: String
   DATA OnAfterChange  // Params: Sender: Table
   DATA OnAfterPostChange  // executes after Table post and only if field has been changed
   DATA OnBeforeChange     // executes before assign value to buffer, must return logical value on success
   DATA OnValidate   // Params: Sender: Table
   DATA OnValidateWarn     // message if OnValidate == FALSE

   PROPERTY AutoIncrement READ GetAutoIncrement
   PROPERTY AutoIncrementKeyIndex READ FAutoIncrementKeyIndex WRITE SetAutoIncrementKeyIndex
   PROPERTY AutoIncrementValue READ GetAutoIncrementValue
   PROPERTY Buffered READ Fbuffered WRITE setBuffered
   PROPERTY Changed READ FChanged
   PROPERTY DBS_DEC READ FDBS_DEC WRITE SetDBS_DEC
   PROPERTY DBS_LEN READ GetDBS_LEN WRITE SetDBS_LEN
   PROPERTY DBS_NAME READ FDBS_NAME
   PROPERTY DBS_TYPE READ GetDBS_TYPE WRITE SetDBS_TYPE
   PROPERTY DefaultValue INDEX 1 READ GetDefaultNewValue WRITE SetDefaultNewValue
   PROPERTY Description READ FDescription WRITE SetDescription
   PROPERTY Editable READ GetEditable WRITE SetEditable
   PROPERTY Enabled READ GetEnabled WRITE SetEnabled
   PROPERTY FieldArray READ GetFieldArray WRITE SetFieldMethod
   PROPERTY FieldCodeBlock READ FFieldCodeBlock WRITE SetFieldMethod
   PROPERTY FieldExpression READ FFieldExpression WRITE SetFieldMethod
   PROPERTY FieldMethod READ GetFieldMethod WRITE SetFieldMethod
   PROPERTY FieldMethodType READ FFieldMethodType
   PROPERTY FieldReadBlock READ GetFieldReadBlock WRITE SetFieldReadBlock
   PROPERTY FieldType READ FFieldType
   PROPERTY FieldWriteBlock READ FFieldWriteBlock WRITE SetFieldWriteBlock
   PROPERTY Group READ FGroup WRITE SetGroup
   PROPERTY IndexKeyList READ FIndexKeyList
   PROPERTY IndexMasterAutoIncKey READ FIndexMasterAutoIncKey
   PROPERTY KeyIndex READ GetKeyIndex
   PROPERTY LABEL READ GetLabel WRITE SetLabel
   PROPERTY Name READ FName WRITE SetName
   PROPERTY NewValue INDEX 2 READ GetDefaultNewValue WRITE SetDefaultNewValue
   PROPERTY PrimaryKeyComponent READ FPrimaryKeyComponent WRITE SetPrimaryKeyComponent
   PROPERTY Published READ FPublished WRITE SetPublished
   PROPERTY READONLY READ GetReadOnly WRITE SetReadOnly
   PROPERTY Required READ FRequired WRITE SetRequired
   PROPERTY superFieldType READ fieldType
   PROPERTY Table READ FTable
   PROPERTY TableBaseClass READ FTableBaseClass
   METHOD Type( locale )
   PROPERTY UNIQUE READ GetUnique
   PROPERTY UniqueKeyIndexList READ FUniqueKeyIndexList
   PROPERTY UsingField READ FUsingField WRITE SetUsingField
   PROPERTY ValType READ FValType

ENDCLASS

/*
    New
*/
METHOD New( Table, curBaseClass ) CLASS TField

   ::FTable := Table
   ::FTableBaseClass := curBaseClass

   ::FEnabled := .T.

   RETURN Self

/*
    AddFieldMessage
*/
METHOD PROCEDURE AddFieldMessage() CLASS TField

   ::FTable:AddFieldMessage( ::Name, Self )

   RETURN

/*
   AddIndexKey
*/
METHOD PROCEDURE AddIndexKey( index ) CLASS TField

   IF AScan( ::FIndexKeyList, {| e| e == index } ) = 0
      hb_AIns( ::FIndexKeyList, 1, index, .T. )
      IF ::FdefaultIndexName = nil
        ::FdefaultIndexName := index:name
      ENDIF
   ELSE
      ::ERROR_ATTEMPT_TO_REASIGN_INDEX_TO_FIELD()
   ENDIF

   RETURN

/*
    CheckForKeyViolation
*/
METHOD PROCEDURE CheckForKeyViolation( value ) CLASS TField
    LOCAL index
    LOCAL itm
    LOCAL oldBuffer

    FOR EACH index IN ::FUniqueKeyIndexList
        IF ::IsPrimaryKeyField .AND. index:existsKey( ::GetKeyVal( value, index:KeyFlags ), ::FTable:RecNo )
            RAISE TFIELD ::Name ERROR "Primary Key violation."
        ENDIF
    NEXT

    IF ::FLastUniqueFieldList != NIL
        FOR EACH itm IN ::FLastUniqueFieldList
            IF value != NIL
                oldBuffer := ::GetBuffer()
                ::SetBuffer( value )
            ENDIF
            ::FTable:FieldList[ itm ]:CheckForKeyViolation( )
            IF oldBuffer != NIL
                ::SetBuffer( oldBuffer )
            ENDIF
        NEXT
    ENDIF

RETURN

/*
    CheckForValidValue
*/
METHOD FUNCTION CheckForValidValue( value, showAlert, errorStr ) CLASS TField
    LOCAL result := .T.
    LOCAL validValues

    IF ! ::FTable:Eof()

        IF ::FValidValues != NIL

            BEGIN SEQUENCE WITH ::FTable:ErrorBlock

                validValues := ::GetValidValues()

                SWITCH ValType( validValues )
                CASE 'A'
                    result := AScan( validValues, {| e| e == value } ) > 0
                    EXIT
                CASE 'H'
                    result := AScan( hb_HKeys( validValues ), {| e| e == value } ) > 0
                    EXIT
                OTHERWISE
                    result := NIL
                ENDSWITCH

            RECOVER

                result := NIL

            END SEQUENCE

        ENDIF

        IF ! result == .T.

            IF result = NIL
                errorStr := ::FTable:ClassName + ": '" + ::Name + "' <Illegal data in 'ValidValues'> "
                IF showAlert == .T.
                    SHOW WARN errorStr
                ENDIF
            ELSE
                errorStr := ::FTable:ClassName + ": '" + ::Name + "' <value given not in 'ValidValues'> : '" + AsString( value ) + "'"
                IF showAlert == .T. .AND. !::FignoreUndetermined
                    SHOW WARN errorStr
                ENDIF
            ENDIF

        ENDIF

    ENDIF

RETURN result

/*
    Clear
*/
METHOD PROCEDURE Clear( clearToNIL ) CLASS TField

    IF clearToNIL = .T.
        ::FBuffer := nil
    ELSE
        ::SetBuffer( ::EmptyValue, .t. )
    ENDIF

    ::FChanged := .F.
    ::FWrittenValue := NIL

RETURN

/*
    DefaultValuePull
*/
METHOD FUNCTION DefaultValuePull() CLASS TField
    LOCAL oldDefaultValue := ::FDefaultValue

    ::DefaultValue := ATail( ::FdefaultValueList )

    HB_ADel( ::FdefaultValueList, Len( ::FdefaultValueList ), .T. )

RETURN oldDefaultValue

/*
    DefaultValuePush
*/
METHOD PROCEDURE DefaultValuePush( newDefaultValue ) CLASS TField

    IF ::FdefaultValueList = NIL
        ::FdefaultValueList := {}
    ENDIF

    AAdd( ::FdefaultValueList, ::FDefaultValue )

    IF PCount() > 0
        ::DefaultValue := newDefaultValue
    ENDIF

RETURN

/*
    Delete
*/
METHOD PROCEDURE DELETE() CLASS TField

   LOCAL errObj

   IF AScan( { dsEdit, dsInsert }, ::Table:State ) = 0
      ::Table:Table_Not_In_Edit_or_Insert_mode()
      RETURN
   ENDIF

   IF !::FFieldMethodType = 'C' .OR. ::FCalculated .OR. ::FFieldWriteBlock == NIL .OR. ::FModStamp .OR. ::FUsingField != NIL
      RETURN
   ENDIF

   BEGIN SEQUENCE WITH ::FTable:ErrorBlock

      ::WriteToTable( ::EmptyValue )

   RECOVER USING errObj

      SHOW ERROR errObj

   END SEQUENCE

   RETURN

/*
    GetAsDisplay
*/
METHOD FUNCTION GetAsDisplay( ... ) CLASS TField
    LOCAL validValues
    LOCAL value

    value := ::GetAsVariant( ... )

    IF ::FDisplayBlock = NIL

        IF ::FcalcResult = NIL
            validValues := ::GetValidValues()
        ELSE
            validValues := ::FcalcResult:ValidValues()
        ENDIF

        IF HB_ISHASH( validValues )

            IF hb_HHasKey( validValues, value )
                RETURN validValues[ value ]
            ENDIF

            RETURN "<!>"

        ENDIF

    ENDIF

RETURN ::FDisplayBlock:Eval( ::FTable, value )

/*
    GetAsVariant
*/
METHOD FUNCTION GetAsVariant( ... ) CLASS TField
    LOCAL AField
    LOCAL i
    LOCAL result
    LOCAL value

    IF ::FTable:isMetaTable
        ::FTable:isMetaTable := .F.
    ENDIF

    ::FcalcResult := NIL

    IF ::FUsingField != NIL
        RETURN ::FUsingField:GetAsVariant( ... )
    ENDIF

    // ::SyncToContainerField()

    IF ::FFieldMethodType = "B" .OR. ::FCalculated
        IF ::FTable:Alias != nil
            IF ! ::buffered .OR. pCount() > 0 .OR. ( result := ::FTable:bufferedField( ::name ) ) = nil
                result := ::FTable:Alias:Eval( ::FieldReadBlock, ::FTable, ... )
                IF ::buffered == .T.
                    ::FTable:bufferedField( ::name, result )
                ENDIF
            ENDIF
            IF HB_ISOBJECT( result ) .AND. result:IsDerivedFrom( "TField" )
                ::FcalcResult := result
                result := result:Value
            ENDIF
        ENDIF
    ELSE
        SWITCH ::FFieldMethodType
        CASE "A"
            /*
             * This will ONLY work when all the items are of TFieldString type
             */
            result := ""
            FOR EACH i IN ::FFieldArrayIndex
                AField := ::FTable:FieldList[ i ]
                /* REVIEW: GetAsVariant in a array of fields is non sense  */
                IF ::FFieldType = ftString
                    result += AField:KeyVal
                ELSE
                    value := AField:GetAsVariant()
                    IF !HB_ISSTRING( value )
                       result += AField:AsString
                    ELSE
                       result += value
                    ENDIF
                ENDIF
            NEXT
            EXIT
        CASE "C"
            result := ::TranslateToValue( ::GetBuffer() )
            EXIT
        OTHERWISE
            THROW ERROR OODB_ERR__FIELD_METHOD_TYPE_NOT_SUPPORTED ARGS ::FFieldMethodType
        ENDSWITCH
    ENDIF

RETURN result

/*
    GetAutoIncrementValue
*/
METHOD FUNCTION GetAutoIncrementValue() CLASS TField

   LOCAL index
   LOCAL value

   IF ::FAutoIncrementKeyIndex = NIL
      index := ::KeyIndex
   ELSE
      index := ::FAutoIncrementKeyIndex
   ENDIF

   value := ::Table:Alias:Get4SeekLast( ::FieldReadBlock, index:MasterKeyVal, index:TagName )

   IF HB_ISCHAR( value ) .AND. Len( value ) > ::Size
      value := Left( value, ::Size )
   ENDIF

   IF ::IncrementBlock = NIL
      SWITCH ::FfieldType
      CASE ftString
      CASE ftMemo
         SWITCH index:__autoIncrementBase
         CASE 36
            RETURN inc( value )
         CASE 64
            RETURN nToBase64( base64ToN( value ) + 1, len( value ) )
         ENDSWITCH
         EXIT
      OTHERWISE
         value += 1
      ENDSWITCH
   ELSE
      value := ::IncrementBlock:Eval( value )
   ENDIF

   RETURN value

/*
    GetBuffer
*/
METHOD FUNCTION GetBuffer() CLASS TField

   LOCAL i

   IF !::FCalculated
      /* FieldArray's doesn't have a absolute FBuffer */
      IF ::FFieldMethodType = "A"
         IF ::FBuffer = NIL
            ::FBuffer := Array( Len( ::FFieldArrayIndex ) )
         ENDIF
         FOR EACH i IN ::FFieldArrayIndex
            ::FBuffer[ i:__enumIndex ] := ::FTable:FieldList[ i ]:GetBuffer()
         NEXT
         RETURN ::FBuffer
      ENDIF

      IF ::FBuffer == NIL
         ::Reset()
      ENDIF
   ENDIF

   RETURN ::FBuffer

/*
    GetCloneData
*/
METHOD FUNCTION GetCloneData( cloneData ) CLASS TField

   IF cloneData = NIL
      cloneData := { => }
   ENDIF

   cloneData[ "Buffer" ] := ::FBuffer
   cloneData[ "Changed" ] := ::FChanged
   cloneData[ "DefaultValue" ] := ::FDefaultValue
   cloneData[ "NewValue" ] := ::FNewValue
   cloneData[ "WrittenValue" ] := ::FWrittenValue

   RETURN cloneData

/*
    GetData
*/
METHOD FUNCTION GetData( initialize ) CLASS TField
   LOCAL i
   LOCAL result := .T.

   /* this is called for Table:GetCurrentRecord, used field has been read */
   IF ::FUsingField != NIL
      RETURN result
   ENDIF

   SWITCH ::FFieldMethodType
   CASE 'B'
      result := ::SetBuffer( ::GetAsVariant() )
      EXIT
   CASE 'C'
      IF ::FCalculated
         result := ::SetBuffer( ::GetAsVariant() )
      ELSE
         result := ::SetBuffer( ::Table:Alias:Eval( ::FieldReadBlock ) )
         ::FChanged := .F.
      ENDIF
      EXIT
   CASE 'A'
      FOR EACH i IN ::FFieldArrayIndex
         IF !::FTable:FieldList[ i ]:GetData()
            result := .F.
            EXIT
         ENDIF
      NEXT
      EXIT
   ENDSWITCH

   IF result .AND. ! initialize = .T.
      ::FWrittenValue := NIL
   ENDIF

   RETURN result

/*
    GetDefaultNewValue
*/
METHOD FUNCTION GetDefaultNewValue( index ) CLASS TField

   LOCAL i
   LOCAL validValues
   LOCAL value
   LOCAL FValue
   LOCAL labelValue

   IF index = 1
      FValue := ::FDefaultValue
      labelValue := "default"
   ELSEIF index = 2
      IF ::FNewValue = NIL
         FValue := ::FDefaultValue
      ELSE
         FValue := ::FNewValue
      ENDIF
      labelValue := "new"
   ENDIF

   IF ::FFieldMethodType = 'A'
      FOR EACH i IN ::FFieldArrayIndex
         IF index = 1
            ::FTable:FieldList[ i ]:DefaultValue()
         ELSEIF index = 2
            ::FTable:FieldList[ i ]:NewValue()
         ENDIF
      NEXT
      // RETURN NIL
   ENDIF

   value := ::TranslateToValue( iif( ValType( FValue ) = "B", FValue:Eval( Self:FTable ), FValue ) )

   validValues := ::GetValidValues()

   IF ! Empty( validValues )
      SWITCH ValType( validValues )
      CASE 'A'
         IF value != NIL .AND. AScan( validValues, {| e| e == value } ) = 0
            RAISE ERROR "On field <" + ::Table:ClassName() + ":" + ::Name + ">, " + labelValue + " value '" + value + "' is not in valid values array list"
         ENDIF
         EXIT
      CASE 'H'
         IF value != NIL .AND. !hb_HHasKey( validValues, value )
            RAISE ERROR "On field <" + ::Table:ClassName() + ":" + ::Name + ">, " + labelValue + " value '" + value + "' is not in valid values hash list"
         ENDIF
         EXIT
      ENDSWITCH
   ENDIF

   RETURN value

/*
    GetEditable
*/
METHOD FUNCTION GetEditable CLASS TField

   IF HB_ISLOGICAL( ::FEditable )
      RETURN ::FEditable
   ENDIF
   IF HB_ISBLOCK( ::FEditable )
      RETURN ::FEditable:Eval( ::FTable )
   ENDIF

   RETURN .T.

/*
    GetEnabled
*/
METHOD FUNCTION GetEnabled() CLASS TField
   LOCAL itm

   SWITCH valType( ::FEnabled )
   CASE "U"
      RETURN .T.
   CASE "L"
      RETURN ::FEnabled
   CASE "B"
      RETURN ::FEnabled:eval( ::FTable )
   CASE "C"
      RETURN ::FTable:isDerivedFrom( ::FEnabled )
   CASE "A"
      FOR EACH itm IN ::FEnabled
         IF ::FTable:isDerivedFrom( itm )
            RETURN .T.
         ENDIF
      NEXT
   ENDSWITCH

RETURN .F.

/*
    GetFieldArray
*/
METHOD FUNCTION GetFieldArray() CLASS TField

   LOCAL a := {}
   LOCAL i

   FOR EACH i IN ::FFieldArrayIndex
      AAdd( a, ::FTable:FieldList[ i ] )
   NEXT

   RETURN a

/*
    GetFieldMethod
*/
METHOD FUNCTION GetFieldMethod CLASS TField

   SWITCH ::FFieldMethodType
   CASE 'A'
      RETURN ::FFieldArrayIndex
   CASE 'B'
      RETURN ::FFieldCodeBlock
   CASE 'C'
      RETURN ::FFieldExpression
   ENDSWITCH

   RETURN NIL

/*
    GetFieldReadBlock
*/
METHOD FUNCTION GetFieldReadBlock() CLASS TField

   LOCAL block

   IF ::FFieldReadBlock == NIL .AND. ::FCalculated
      IF !Empty( ::FFieldExpression ) .AND. ":" $ ::FFieldExpression
         block := ::FTable:BuildFieldBlockFromFieldExpression( ::FFieldExpression, iif( ::FieldType = ftTable, NIL, "Value" ) )
         IF block != NIL
            ::FFieldReadBlock := block
            RETURN ::FFieldReadBlock
         ENDIF
      ENDIF
      IF ::FFieldCodeBlock = NIL
         IF __objHasMsgAssigned( ::FTable, "CalcField_" + ::FName )
            ::FFieldReadBlock := &( "{|o,...|" + "o:CalcField_" + ::FName + "( ... ) }" )
         ELSE
            IF !::IsDerivedFrom( "TFieldTable" )
               THROW ERROR OODB_ERR__CALCULATED_FIELD_CANNOT_BE_SOLVED
            ENDIF
         ENDIF
      ELSE
         ::FFieldReadBlock := ::FFieldCodeBlock
      ENDIF
   ENDIF

   RETURN ::FFieldReadBlock

/*
    GetKeyIndex
*/
METHOD FUNCTION GetKeyIndex() CLASS TField

    IF ::FdefaultIndexName = NIL
        IF Len( ::FIndexKeyList ) > 0
            RETURN ::FIndexKeyList[ 1 ]
        ENDIF
    ELSE
        RETURN ::FTable:IndexByName( ::FdefaultIndexName )
    ENDIF

RETURN NIL

/*
    GetKeyVal
*/
METHOD FUNCTION GetKeyVal( keyVal, keyFlags ) CLASS TField

    LOCAL AField
    LOCAL i, start, value

    IF keyFlags = NIL .AND. ::IsKeyIndex
        keyFlags := ::KeyIndex:KeyFlags
    ENDIF

    IF ::FFieldMethodType = "A"
        IF keyVal = NIL
            keyVal := ""
            FOR EACH i IN ::FFieldArrayIndex
                keyVal += ::FTable:FieldList[ i ]:GetKeyVal( NIL, keyFlags )
            NEXT
        ELSE
            value := ""
            start := 1
            FOR EACH i IN ::FFieldArrayIndex
                AField := ::FTable:FieldList[ i ]
                value += AField:GetKeyVal( SubStr( keyVal, start, AField:KeySize ), keyFlags )
                start += AField:KeySize
            NEXT
        ENDIF
    ELSE
        IF keyVal = NIL
            keyVal := ::GetAsVariant()
        ENDIF
        IF HB_ISCHAR( keyVal )
            IF keyFlags != NIL .AND. hb_HHasKey( keyFlags, ::Name )
                SWITCH keyFlags[ ::Name ]
                CASE "U"
                    keyVal := Upper( keyVal )
                    EXIT
                ENDSWITCH
            ELSEIF ::IsKeyIndex
                IF !::KeyIndex:CaseSensitive
                    keyVal := Upper( keyVal )
                ENDIF
                IF ::KeyIndex:RightJustified
                    keyVal := PadL( RTrim( keyVal ), ::DBS_LEN )
                ENDIF
            ENDIF
            IF Len( keyVal ) < ::DBS_LEN
                keyVal := PadR( keyVal, ::DBS_LEN )
            ENDIF
        ENDIF
    ENDIF

RETURN keyVal

/*
    GetUndoValue
*/
METHOD FUNCTION GetUndoValue() CLASS TField

   IF !Empty( ::FTable:UndoList ) .AND. hb_HHasKey( ::FTable:UndoList, ::FName )
      RETURN ::FTable:UndoList[ ::FName ]
   ENDIF

   RETURN NIL

/*
    GetValidValues
*/
METHOD FUNCTION GetValidValues() CLASS TField

   LOCAL validValues

   SWITCH ValType( ::FValidValues )
   CASE "A"
   CASE "H"
      RETURN ::FValidValues
   CASE "B"
      validValues := ::FValidValues:Eval( Self:FTable )
      IF HB_ISOBJECT( validValues )
         validValues := validValues:ValidValues()
      ENDIF
      RETURN validValues
   CASE "O"
      IF ::FValidValues:IsDerivedFrom( "TFieldTable" )
         RETURN ::FValidValues:ValidValues()
      ENDIF
      EXIT
   ENDSWITCH

   RETURN NIL

/*
    isReadOnly
*/
METHOD FUNCTION isReadOnly() CLASS TField
    LOCAL result

    result := ;
        ::FTable:READONLY .OR. ;
        ::FReadOnly .OR. ;
        ( ::FTable:State != dsBrowse .AND. ::AutoIncrement ) .OR. ;
        ( ::FIndexMasterAutoIncKey .AND. ( ::FTable:state != dsInsert .OR. ! ::value == ::getEmptyValue() ) )

RETURN result

/*
    IsTableField
*/
METHOD FUNCTION IsTableField() CLASS TField
   RETURN ::FFieldMethodType = "C" .AND. !::FCalculated .AND. ::FUsingField = NIL

/*
    OnSetKeyVal
*/
METHOD PROCEDURE OnSetKeyVal( lSeek, keyVal ) CLASS TField

    IF ::FTable:BaseKeyField == Self .AND. __objHasMsgAssigned( ::FTable, "OnSetKeyVal_BaseKeyField" )
        __objSendMsg( ::FTable, "OnSetKeyVal_BaseKeyField", lSeek, keyVal )
    ELSEIF __objHasMsgAssigned( ::FTable, "OnSetKeyVal_Field" )
        __objSendMsg( ::FTable, "OnSetKeyVal_Field", Self, lSeek, keyVal )
    ELSEIF __objHasMsgAssigned( ::FTable, "OnSetKeyVal_Field_" + ::Name )
        __objSendMsg( ::FTable, "OnSetKeyVal_Field_" + ::Name, lSeek, keyVal )
    ELSEIF ::FOnSetKeyValBlock != NIL
        ::FOnSetKeyValBlock:Eval( ::FTable, Self, lSeek, keyVal )
    ENDIF

RETURN

/*
    Reset
*/
METHOD FUNCTION Reset( initialize ) CLASS TField

   LOCAL AField
   LOCAL i
   LOCAL value
   LOCAL result := .F.
   LOCAL FValue

   IF ::FOnReset
      RETURN .F. /* ::Value of field can be NIL */
   ENDIF

   /* do nothing if initializing and previously written value to table  */
   IF initialize = .T. .AND. ::FWrittenValue != nil
      RETURN .F.
   ENDIF

   ::FOnReset := .T.

   IF ::FTable:State = dsInsert .AND. ::FNewValue != NIL
      FValue := ::FNewValue
   ELSE
      FValue := ::FDefaultValue
   ENDIF

   IF !::FCalculated .OR. FValue != nil .OR. hb_isChar( ::FIndexExpression )

      IF FValue = NIL

         /* if is a masterfield component, then *must* resolve it in the MasterSource(s) */
         IF ( result := ::IsMasterFieldComponent )

            result := ( AField := ::FTable:FindMasterSourceField( Self ) ) != NIL

            IF !result .AND. ::FFieldType == ftTable .AND. ::FTable:MasterSource:ClassName == Upper( ::ObjClass )
               result := ( AField := ::FTable:MasterSource:KeyField ) != NIL
            ENDIF

            IF result

               value := ::TranslateToFieldValue( AField:GetBuffer() )
                    /*
                     * if there is a DefaultValue this is ignored (may be a warning is needed)
                     */
            ENDIF

         ENDIF

         /* reset was not succesfull yet */
         IF !result
            /* resolve each field on a array of fields */
            IF ::FFieldMethodType = 'A'

               result := .T.

               FOR EACH i IN ::FFieldArrayIndex
                  result := ::FTable:FieldList[ i ]:Reset( initialize ) .AND. result
               NEXT

               ::FOnReset := .F.

               RETURN result

            ELSE

               value := ::getEmptyValue()

               result := .T.

            ENDIF

         ENDIF

      ELSE

         /* On a TFieldTable with a Table with MasterSource in the same Table, allows to Reset it first */
         IF ::FieldType = ftTable .AND. ::LinkedTable:MasterSource != NIL .AND. ::LinkedTable:MasterSource:LinkedObjField != NIL .AND. ::LinkedTable:MasterSource:LinkedObjField:Table == ::FTable
            ::LinkedTable:MasterSource:LinkedObjField:Reset()
         ENDIF

         IF ::FTable:State = dsInsert
            value := ::NewValue
         ELSE
            value := ::DefaultValue
         ENDIF

         result := .T.

      ENDIF

      ::SetBuffer( value )

      ::FChanged := .F.
      ::FWrittenValue := NIL

   ENDIF

   ::FOnReset := .F.

   RETURN result

/*
    revertValue
*/
METHOD FUNCTION revertValue() CLASS TField
    LOCAL undoValue

    IF ::FrevertingValue = nil

        ::FrevertingValue := .T.

        undoValue := ::GetUndoValue()

        IF undoValue != NIL
            ::SetData( undoValue )
            ::FChanged := .F.
        ENDIF

        ::FrevertingValue := nil

        RETURN .T.

    ENDIF

RETURN .F.

/*
    SetAsVariant
*/
METHOD FUNCTION SetAsVariant( value ) CLASS TField

    IF ::FTable:isMetaTable
        ::FTable:isMetaTable := .F.
    ENDIF

    IF ::FCalculated .AND. ::FFieldWriteBlock = nil
        RETURN value
    ENDIF

    IF ::IsReadOnly .OR. ::FTable:State = dsInactive .OR. !::Enabled
        RETURN value
    ENDIF

    IF ::FOnSetValue == NIL
        ::FOnSetValue := __objHasMsg( ::FTable, "OnSetValue_Field_" + ::FName )
    ENDIF

    IF ::FOnSetValue
        __objSendMsg( ::FTable, "OnSetValue_Field_" + ::Name, @value )
    ENDIF

    SWITCH ::FTable:State
    CASE dsBrowse

        IF ::FCalculated
            ::writeToTable( value )
        ELSE
            ::SetKeyVal( value )
        ENDIF

        EXIT

    CASE dsEdit
    CASE dsInsert

        SWITCH ::FFieldMethodType
        CASE "A"

            RAISE TFIELD ::Name ERROR "Trying to assign a value to a compound TField."

            EXIT

        CASE "B"
        CASE "C"

            /* Check if we are really changing values here */
            IF !::GetBuffer() == ::TranslateToFieldValue( value )
                ::SetData( value )
            ENDIF

        ENDSWITCH

        EXIT

    OTHERWISE

        RAISE TFIELD ::Name ERROR "Table not in Edit or Insert or Reading mode"

    ENDSWITCH

RETURN value

/*
    SetBuffer
*/
METHOD FUNCTION SetBuffer( value, lNoCheckValidValue ) CLASS TField
    LOCAL result
    LOCAL itm

    result := lNoCheckValidValue = .t. .OR. ::CheckForValidValue( ::TranslateToValue( value ), .T. ) == .T.

    IF result

        IF !::FCalculated
            /* FieldArray's doesn't have a absolute FBuffer */
            IF ::FFieldMethodType = "A"
                SWITCH ValType( value )
                CASE 'A'
                    FOR EACH itm IN value
                       IF !::FTable:FieldList[ ::FFieldArrayIndex[ itm:__enumIndex ] ]:SetBuffer( itm, lNoCheckValidValue )
                          result := .F.
                          EXIT
                       ENDIF
                     NEXT
                    EXIT
                ENDSWITCH
                RETURN result
            ENDIF
            IF !( value = nil .OR. ValType( value ) = ::FValType ) .AND. ( ::IsDerivedFrom( "TFieldString" ) .AND. AScan( { "C", "M" }, ValType( value ) ) = 0 )
                RAISE TFIELD ::Name ERROR "Wrong Type Assign: [" + value:ClassName + "] to <" + ::ClassName + ">"
            ENDIF
        ENDIF

        ::FBuffer := ::TranslateToFieldValue( value )

    ENDIF

    RETURN result

/*
    SetCloneData
*/
METHOD PROCEDURE SetCloneData( cloneData ) CLASS TField

   ::FBuffer := cloneData[ "Buffer" ]
   ::FChanged := cloneData[ "Changed" ]
   ::FDefaultValue := cloneData[ "DefaultValue" ]
   ::FNewValue := cloneData[ "NewValue" ]
   ::FWrittenValue := cloneData[ "WrittenValue" ]

   RETURN

/*
    SetData
*/
METHOD PROCEDURE SetData( value, initialize ) CLASS TField
   LOCAL i
   LOCAL nTries
   LOCAL errObj
   LOCAL result

   IF ::FUsingField != NIL
      ::FUsingField:SetData( value )
      RETURN
   ENDIF

   /* do nothing if initializing and previously written value to table  */
   IF initialize = .T. .AND. ::FWrittenValue != nil
      RETURN
   ENDIF

   /* SetData is only for the physical fields on the database */
   SWITCH ::FFieldMethodType
   CASE 'A'

      IF value != NIL
         RAISE TFIELD ::Name ERROR "SetData: Not allowed custom value in a compound TField..."
      ENDIF

      FOR EACH i IN ::FFieldArrayIndex
         ::FTable:FieldList[ i ]:SetData( , initialize )
      NEXT

      RETURN

   CASE 'B'

      ::FTable:Alias:Eval( ::FFieldCodeBlock, ::FTable, value )

      RETURN

   CASE 'C'

      EXIT

   OTHERWISE

      RETURN

   ENDSWITCH

   IF AScan( { dsEdit, dsInsert }, ::Table:State ) = 0
      RAISE TFIELD ::Name ERROR "SetData(): Table not in Edit or Insert mode..."
      RETURN
   ENDIF

   IF ::FModStamp
      RETURN
   ENDIF

   IF ::AutoIncrement

      IF value != NIL
         RAISE TFIELD ::Name ERROR "Not allowed custom value in AutoIncrement Field..."
      ENDIF

        /*
         *AutoIncrement field writting allowed only in Adding
         */
      IF !( ::FTable:SubState = dssAdding )
         RETURN
      ENDIF

      /* Try to obtain a unique key */
      nTries := 1000
      WHILE .T.
         value := ::GetAutoIncrementValue()
         IF !::FAutoIncrementKeyIndex:existsKey( ::GetKeyVal( value, ::FAutoIncrementKeyIndex:KeyFlags ) )
            EXIT
         ENDIF
         IF ( --nTries = 0 )
            RAISE TFIELD ::Name ERROR "Can't create AutoIncrement Value..."
            RETURN
         ENDIF
      ENDDO

   ELSE

      IF value == NIL
         value := ::TranslateToValue( ::GetBuffer() )
      ENDIF

   ENDIF

   /* Don't bother... all except ftArray, ftHash : always must be written */
   IF !(::FieldType = ftArray .OR. ::FieldType = ftHash) .AND. value == ::FWrittenValue
      RETURN
   ENDIF

   IF ! initialize == .T. .AND. !::Editable()
      SHOW WARN "<field is not editable>"
      RETURN
   ENDIF

    IF ! initialize == .T.

        IF ::OnBeforeChange != NIL
            BEGIN SEQUENCE WITH ::FTable:ErrorBlock
                result := ::OnBeforeChange:Eval( ::FTable, @value )
            RECOVER
                SHOW WARN "<Error at 'OnBeforeChange()'>"
                result := .F.
            END SEQUENCE
            IF !result
                RETURN
            ENDIF
        ELSE
            IF ::FonBeforeChangeAssigned = NIL
                ::FonBeforeChangeAssigned := __objHasMsgAssigned( ::FTable, "OnBeforeChange_Field_" + ::Name )
            ENDIF
            IF ::FonBeforeChangeAssigned .AND. !__objSendMsg( ::FTable, "OnBeforeChange_Field_" + ::Name, @value )
                RETURN
            ENDIF
        ENDIF

        IF !Empty( ::ValidateResult_TableLogic( .T., value ) )
            RETURN
        ENDIF

        IF ! ::FTable:onBeforeChange_Field( Self, @value )
            RETURN
        ENDIF

    ENDIF

    BEGIN SEQUENCE WITH ::FTable:ErrorBlock

        /*
         * Check for a key violation
         */
        ::CheckForKeyViolation( value )

        ::WriteToTable( value, initialize )

        IF ! initialize == .T. .AND. !Empty( result := ::ValidateResult_OnValidate( .F. ) ) .AND. ::revertValue()

            SHOW WARN result

        ELSE
            IF ::FTable:BaseKeyField == Self

                ::SetValueToLinkedObjField( ::GetAsVariant() )

            ENDIF

            /* sync with re-used field in db */
            IF ::FReUseFieldIndex != NIL
                ::FTable:FieldList[ ::FReUseFieldIndex ]:GetData()
            ENDIF

            IF !initialize == .T.
                IF ::onAfterChange != nil
                    BEGIN SEQUENCE WITH ::FTable:ErrorBlock
                        ::onAfterChange:Eval( ::FTable )
                    RECOVER
                        SHOW WARN "<Error at 'OnAfterChange()'>"
                    END SEQUENCE
                ELSE
                    IF ::FonAfterChangeAssigned = nil
                        ::FonAfterChangeAssigned := __objHasMsgAssigned( ::FTable, "onAfterChange_Field_" + ::name )
                    ENDIF
                    IF ::FonAfterChangeAssigned
                        __objSendMsg( ::FTable, "onAfterChange_Field_" + ::name )
                    ENDIF
                ENDIF
            ENDIF
        ENDIF

    RECOVER USING errObj

        SHOW ERROR errObj

    END SEQUENCE

RETURN

/*
    SetDbStruct
*/
METHOD PROCEDURE SetDbStruct( aStruct ) CLASS TField

    ::FModStamp := aStruct[ 2 ] $ "=^+"

    IF !::IsDerivedFrom("TFieldFloat")
    ::SetDBS_LEN( aStruct[ 3 ] )
    ENDIF

    ::SetDBS_DEC( aStruct[ 4 ] )

RETURN

/*
    SetDefaultIndexName
*/
METHOD PROCEDURE SetDefaultIndexName( defaultIndexName ) CLASS TField
    ::FdefaultIndexName := defaultIndexName
RETURN

/*
    SetDefaultNewValue
*/
METHOD PROCEDURE SetDefaultNewValue( index, value ) CLASS TField

    IF index = 1
        IF ::fieldType = ftTable .AND. ::isMasterFieldComponent .AND. ::table:masterSource != nil .AND. ::linkedTable:isDerivedFrom( ::table:masterSource:getMasterSourceClassName )
            RAISE TFIELD ::FName ERROR "Illegal to assign defaultValue to a fieldTable which is a masterSource field component..."
        ELSE
            ::FDefaultValue := value
        ENDIF
    ELSEIF index = 2
        ::FNewValue := value
    ENDIF

RETURN

/*
    SetEnabled
*/
METHOD PROCEDURE SetEnabled( enabled ) CLASS TField

   IF ::FIsMasterFieldComponent .OR. ::FPrimaryKeyComponent
      RAISE TFIELD ::FName ERROR "Cannot disable a Master/Primary key component..."
   ENDIF

   ::FEnabled := enabled

   RETURN

/*
    SetFieldMethod
*/
METHOD PROCEDURE SetFieldMethod( FieldMethod, calculated ) CLASS TField

   LOCAL itm, fieldName
   LOCAL AField
   LOCAL i
   LOCAL fieldBlock

   SWITCH ( ::FFieldMethodType := ValType( FieldMethod ) )
   CASE "A"

      // ::FReadOnly := .T.
      ::FFieldArrayIndex := {}
      fieldName := ""
      FOR EACH itm IN FieldMethod
         AField := ::FTable:FieldByName( itm, @i )
         IF AField != NIL
            AAdd( ::FFieldArrayIndex, i )
            fieldName += itm + ";"
         ELSE
            RAISE TFIELD itm ERROR "Field is not defined yet..."
         ENDIF
      NEXT
      ::Name := Left( fieldName, Len( fieldName ) - 1 )
      ::FFieldCodeBlock  := NIL
      ::FFieldReadBlock  := NIL
      ::FFieldWriteBlock := NIL
      ::FFieldExpression := NIL

      EXIT

   CASE "B"
      // ::FReadOnly := .T.
      ::FFieldArrayIndex := NIL
      ::FFieldCodeBlock  := FieldMethod
      ::FFieldReadBlock  := NIL
      ::FFieldWriteBlock := NIL
      ::FFieldExpression := NIL

      ::FCalculated := .T.
      ::Fbuffered := .T.

      EXIT

   CASE "C"

      ::FFieldArrayIndex := NIL
      ::FFieldCodeBlock := NIL

      FieldMethod := LTrim( RTrim( FieldMethod ) )

      IF calculated = .T.
         ::Fbuffered := .T.
      ENDIF

      ::FCalculated := calculated == .T. .OR. ":" $ FieldMethod

      IF ! ::FCalculated

         ::FDBS_NAME := FieldMethod

         /* Check if the same FieldExpression is declared redeclared in the same table baseclass */
         FOR EACH AField IN ::FTable:FieldList
            IF !Empty( AField:FieldExpression ) .AND. !AField:Calculated .AND. ;
                  Upper( AField:FieldExpression ) == Upper( FieldMethod ) .AND. ;
                  AField:TableBaseClass == ::FTableBaseClass
               IF !::FReUseField
                  RAISE TFIELD ::Name ERROR "Atempt to Re-Use FieldExpression (same field on db) <" + ::ClassName + ":" + FieldMethod + ">"
               ELSE
                  ::FReUseFieldIndex := AField:__enumIndex()
               ENDIF
            ENDIF
         NEXT

         fieldBlock := FieldBlock( FieldMethod )

         IF Empty( fieldBlock )
            /* this can happen when creating/adding fields to table */
            ::FFieldReadBlock := &( "{|| FIELD->" + FieldMethod + " }" )
            ::FFieldWriteBlock := &( "{|__x_val| FIELD->" + FieldMethod + " := __x_val }" )
         ELSE
            ::FFieldReadBlock := fieldBlock
            ::FFieldWriteBlock := fieldBlock
         ENDIF

      ENDIF

      fieldName := iif( Empty( ::FName ), FieldMethod, ::FName )

      IF Empty( fieldName )
         RAISE TFIELD "<Empty>" ERROR "Empty field name and field method."
      ENDIF

      ::FFieldExpression := FieldMethod
      ::Name := FieldMethod

      EXIT

   ENDSWITCH

   RETURN

/*
    SetFieldWriteBlock
*/
METHOD PROCEDURE SetFieldWriteBlock( writeBlock ) CLASS TField

   SWITCH ValType( writeBlock )
   CASE "C"
      writeBlock := &( "{|Self,value| ::CalcField_" + writeBlock + "( value )}" )
      EXIT
   CASE "B"
      EXIT
   OTHERWISE
      RETURN
   ENDSWITCH

   ::FFieldWriteBlock := writeBlock

   RETURN

/*
   setIndexMasterAutoIncKey
*/
METHOD PROCEDURE setIndexMasterAutoIncKey( index ) CLASS TField
    LOCAL i

    IF index:AutoIncrement

        ::FIndexMasterAutoIncKey := .T.

        IF ::FFieldMethodType = "A"
            FOR EACH i IN ::FFieldArrayIndex
                ::FTable:FieldList[ i ]:setIndexMasterAutoIncKey( index )
            NEXT
        ENDIF
    ENDIF

RETURN

/*
    SetIsMasterFieldComponent
*/
METHOD PROCEDURE SetIsMasterFieldComponent( IsMasterFieldComponent ) CLASS TField

   LOCAL i

   SWITCH ::FFieldMethodType
   CASE 'A'
      FOR EACH i IN ::FFieldArrayIndex
         ::FTable:FieldList[ i ]:IsMasterFieldComponent := IsMasterFieldComponent
      NEXT
   CASE 'C'
      ::FIsMasterFieldComponent := IsMasterFieldComponent
      ::FEnabled := .T.
   ENDSWITCH

   IF ::IsDerivedFrom( "TFieldTable" ) .AND. Empty( ::FTable:GetMasterSourceClassName() )
      // RAISE TFIELD ::Name ERROR "FieldTable's needs a valid MasterSource table."
   ENDIF

   RETURN

/*
    SetKeyVal
*/
METHOD FUNCTION SetKeyVal( keyVal, lSoftSeek ) CLASS TField

    IF ::FTable:OnActiveSetKeyVal()

        outErr(e"\nIgnoring setKeyVal() call with \"" + keyVal + e"\"")

    ELSE

        ::FTable:OnActiveSetKeyVal( .T. )

        IF ::IsKeyIndex

            IF ::OnSearch != NIL
                ::OnSearch:Eval( Self )
            ENDIF

            IF !Empty( keyVal )
                keyVal := ::GetKeyVal( keyVal, ::KeyIndex:KeyFlags )
                IF ::FTable:Eof() .OR. ! ::KeyIndex:KeyVal == keyVal
                    ::OnSetKeyVal( ::KeyIndex:Seek( keyVal, lSoftSeek ), keyVal )
                ENDIF
            ELSE
                ::FTable:dbGoto( 0 )
                ::OnSetKeyVal( .F., keyVal )
            ENDIF

            ::SetValueToLinkedObjField( ::FTable:BaseKeyField:GetAsVariant() )

        ELSE

            SHOW WARN "Field '" + ::Name + "' has no Index in the '" + ::FTable:ClassName() + "' Table..."

        ENDIF

        ::FTable:OnActiveSetKeyVal( .F. )

    ENDIF

RETURN !::FTable:Eof

/*
    SetName
*/
METHOD PROCEDURE SetName( name ) CLASS TField

   IF Empty( name ) .OR. !Empty( ::FName )
      RETURN
   ENDIF

   IF ":" $ name
      name := StrTran( name, ":", "_" )
   ENDIF

   ::FName := name

   RETURN

/*
    SetPrimaryKeyComponent
*/
METHOD PROCEDURE SetPrimaryKeyComponent( PrimaryKeyComponent ) CLASS TField

   LOCAL i

   SWITCH ::FFieldMethodType
   CASE 'A'
      FOR EACH i IN ::FFieldArrayIndex
         ::FTable:FieldList[ i ]:PrimaryKeyComponent := PrimaryKeyComponent
      NEXT
      EXIT
   CASE 'C'
      ::FPrimaryKeyComponent := PrimaryKeyComponent
      ::FEnabled := .T.
   ENDSWITCH

   RETURN

/*
    SetUsingField
*/
METHOD PROCEDURE SetUsingField( usingField ) CLASS TField

   LOCAL AField := ::FTable:FieldByName( usingField )

   IF AField != NIL
      ::FUsingField := AField
   ENDIF

   RETURN

/*
    SetValidValues
*/
METHOD PROCEDURE SetValidValues( validValues, ignoreUndetermined ) CLASS TField

   ::FValidValues := validValues
   ::FignoreUndetermined := ignoreUndetermined

   RETURN

/*
    SetValueToLinkedObjField
*/
METHOD PROCEDURE SetValueToLinkedObjField( value ) CLASS TField
    IF ::FTable:LinkedObjField != NIL .AND. ::FTable:OnSetValueToLinkedObjField( ::FTable:LinkedObjField, value )
        IF ::FTable:LinkedObjField:Table:State > dsBrowse
            ::FTable:LinkedObjField:SetAsVariant( value )
        ENDIF
    ENDIF
RETURN

/*
    Type
*/
METHOD Type( locale ) CLASS TField
    LOCAL type := ::FType

    IF !Empty( locale ) .AND. !Empty( ::FtypeNameList ) .AND. hb_HHasKey( ::FtypeNameList, locale )
        type := ::FtypeNameList[ locale ]
    ENDIF

RETURN type

/*
    ValidateResult_TableLogic
*/
METHOD FUNCTION ValidateResult_TableLogic( showAlert, value ) CLASS TField
    LOCAL result
    LOCAL index
    LOCAL indexWarnMsg

    IF value = NIL
        value := ::GetAsVariant()
    ENDIF

    IF ::FRequired .AND. Empty( value )
        result := ::FTable:ClassName + ": '" + ::Name + "' <empty field required value>"
        IF showAlert == .T.
            SHOW WARN result
        ENDIF
        RETURN result
    ENDIF

    IF ::Unique
        IF Empty( value ) .AND. !::AcceptEmptyUnique
            result := ::FTable:ClassName + ": '" + ::Name + "' <empty UNIQUE INDEX key value>"
            IF showAlert == .T.
                SHOW WARN result
            ENDIF
            RETURN result
        ENDIF
        FOR EACH index IN ::FUniqueKeyIndexList
            indexWarnMsg := index:WarnMsg
            IF !Empty( value ) .AND. index:existsKey( ::GetKeyVal( value, index:KeyFlags ), ::FTable:RecNo )
                result := iif( !Empty( indexWarnMsg ), indexWarnMsg, "'" + ::Name + "' <key value already exists> '" + AsString( value ) + "'" )
                result += ";Table: " + ::FTable:ClassName
                result += ";Index: " + index:name
                IF showAlert == .T.
                    SHOW WARN result
                ENDIF
                RETURN result
            ENDIF
        NEXT
    ENDIF

    /*
     * Check for a key violation
     */
    FOR EACH index IN ::FUniqueKeyIndexList
        IF ::IsPrimaryKeyField .AND. index:existsKey( ::GetKeyVal( value, index:KeyFlags ), ::FTable:RecNo )
            RAISE TFIELD ::Name ERROR "Key violation."
        ENDIF
    NEXT

RETURN result

/*
    ValidateResult_OnValidate
*/
METHOD FUNCTION ValidateResult_OnValidate( showAlert, value ) CLASS TField
    LOCAL result
    LOCAL l

    IF value = NIL
        value := ::GetAsVariant()
    ENDIF

    l := ::CheckForValidValue( value, showAlert, @result )

    IF ! l == .T.
        RETURN result
    ENDIF

    IF ::OnValidate != NIL
        BEGIN SEQUENCE WITH ::FTable:ErrorBlock
            l := ::OnValidate:Eval( ::FTable )
        RECOVER
            l := NIL
        END SEQUENCE
        IF l = NIL
            result := ::FTable:ClassName + ": '" + ::Name + "' <Error at 'OnValidate'> "
            IF showAlert == .T.
                SHOW WARN result
            ENDIF
        ELSEIF !l
            result := ::FTable:ClassName + ": '" + ::Name + E"' OnValidate:\n<" + iif( ::OnValidateWarn = NIL, "Value Not Valid", ::OnValidateWarn ) + "> "
            IF showAlert == .T.
            SHOW WARN result
            ENDIF
        ENDIF
    ENDIF

RETURN result

/*
    WriteToTable
*/
METHOD PROCEDURE WriteToTable( value, initialize ) CLASS TField
    LOCAL oldBuffer

    oldBuffer := ::GetBuffer()

    value := ::TranslateToFieldValue( value )

    ::SetBuffer( value )

    /* The physical write to the  field */
    IF ::Fcalculated
        ::FTable:Alias:Eval( ::FFieldWriteBlock, ::Ftable, value )
        IF ::buffered
            ::Ftable:bufferedField( ::name, nil )
        ENDIF
    ELSE
        ::FTable:Alias:Eval( ::FFieldWriteBlock, value )
    ENDIF

    ::FWrittenValue := ::GetBuffer()

    /* fill undolist */
    IF ! initialize == .T.
        IF oldBuffer != nil .AND. ::FTable:UndoList != NIL .AND. !hb_HHasKey( ::FTable:UndoList, ::FName )
            ::FTable:UndoList[ ::FName ] := oldBuffer
            ::FChanged := ! value == oldBuffer
        ENDIF
    ENDIF

RETURN

/*
    ENDCLASS TField
*/
