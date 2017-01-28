/*
 *
 */

/*
    TTable
*/

#include "oordb.ch"
#include "error.ch"
#include "xerror.ch"

#include "dbinfo.ch"

#define rxMasterSourceTypeNone     0
#define rxMasterSourceTypeTTable   1
#define rxMasterSourceTypeTField   2
#define rxMasterSourceTypeBlock    3

#define OORDB_DEFAULT_AUTOCREATE    .T.

THREAD STATIC FErrorBlock
THREAD STATIC BaseKeyFieldList := {}
THREAD STATIC __S_Instances
THREAD STATIC __s_fieldList
THREAD STATIC __s_indexList
THREAD STATIC __S_dataBase
THREAD STATIC FmemTempFileCount := 0

STATIC errorStringList := { ;
    "trying edit on browse state",;
    "trying edit at eof",;
    "edit denied by onBeforeEdit()",;
    "edit denied by recLock()" }

REQUEST HB_MEMIO

REQUEST TField

FUNCTION OordbErrorNew( Self, description, args )

   LOCAL oErr := ErrorNew()

   oErr:cargo := Self
   oErr:description := description
   oErr:args := args

   IF ::IsDerivedFrom( "TField" )
      oErr:operation := "Table: " + ::Table:ClassName() + E"\nField: " + ::Name
   ENDIF

   RETURN oErr

/*
    baseKeyFieldList
*/
FUNCTION baseKeyFieldList( className )
    LOCAL n

    SWITCH valType( className )
    CASE "C"
        n :=  aScan( BaseKeyFieldList, {|e| upper( e[ 1 ] ) == upper( className ) } )
        IF n > 0
            RETURN BaseKeyFieldList[ n ]
        ENDIF
        EXIT
    ENDSWITCH

RETURN BaseKeyFieldList

/*
    __ClsInstFromName (Just UpperCase in __ClsInstName)
*/
FUNCTION __ClsInstFromName( ClassName )
   RETURN __ClsInstName( Upper( ClassName ) )

/*
    ErrorBlockOORDB
*/
FUNCTION ErrorBlockOORDB( oErr )

   // By default, division by zero results in zero
   IF oErr:genCode == EG_ZERODIV .AND. oErr:canSubstitute
      RETURN 0
   ENDIF

   // By default, retry on RDD lock error failure */
   IF oErr:genCode == EG_LOCK .AND. oErr:canRetry
      // oErr:tries++
      RETURN .T.
   ENDIF

   // Set NetErr() of there was a database open error
   IF oErr:genCode == EG_OPEN .AND. ;
         oErr:osCode == 32 .AND. ;
         oErr:canDefault
      NetErr( .T. )
      RETURN .F.
   ENDIF

   // Set NetErr() if there was a lock error on dbAppend()
   IF oErr:genCode == EG_APPENDLOCK .AND. oErr:canDefault
      NetErr( .T. )
      RETURN .F.
   ENDIF

   IF .T.
      Break( oErr )
   ENDIF

   RETURN NIL

/*
    TTable
*/
CLASS TTable FROM OORDBBASE

PRIVATE:

   CLASSDATA FFieldTypes

   DATA FActive    INIT .F.
   DATA FAddress
   DATA FAlias
   DATA FDisplayFieldList          // Contains a Object
   DATA FHasDeletedOrder INIT .F.
   DATA FIndex              // Current TIndex in Table
   DATA FMasterSource
   DATA FMasterSourceType  INIT rxMasterSourceTypeNone
   DATA FPort

   DATA FRecNoBeforeInsert

   DATA FReadOnly              INIT .F.
   DATA FRemote    INIT .F.
   DATA FState INIT dsInactive
   DATA FSubState INIT dssNone
   DATA FSyncingToContainerField INIT .F.
   DATA FTimer INIT 0
   DATA FUndoList

   METHOD DbGoBottomTop( n )
   METHOD GetAlias
   METHOD GetDbStruct
   METHOD GetFieldTypes
   METHOD GetIndexName() INLINE iif( ::GetIndex() = NIL, "", ::GetIndex():Name )
   METHOD getInstance
   METHOD GetKeyExpression()
   METHOD GetKeyField()
   METHOD GetKeyString INLINE iif( ::GetKeyField == NIL, "", ::GetKeyField:AsString )
   METHOD GetMasterKeyExpression()
   METHOD GetMasterKeyField()
   METHOD GetMasterKeyString INLINE iif( ::GetMasterKeyField == NIL, "", ::GetMasterKeyField:AsString )
   METHOD GetMasterKeyVal INLINE iif( ::GetMasterKeyField == NIL, "", ::GetMasterKeyField:GetKeyVal )
   METHOD GetMasterSource()
   METHOD SetIndex( index )
   METHOD SetIndexName( IndexName )
   METHOD SetMasterSource( masterSource )
   METHOD SetReadOnly( readOnly )
   METHOD SetState( state )
   METHOD SetSyncingToContainerField( value ) INLINE ::FSyncingToContainerField := value
   METHOD Process_TableName( tableName )
   // METHOD SendToServer

PROTECTED:

   DATA FAutoCreate
   DATA FBaseKeyField
   DATA FBaseKeyIndex
   DATA FDbFilterStack INIT {}
   DATA FbaseDocument INIT ""
   DATA FBof    INIT .T.
   DATA FcanCreateInstance INIT .F.
   DATA FCustomIndexList   INIT {}
   DATA FDataBaseClass
   DATA FEof    INIT .T.
   DATA FFieldList         INIT {}
   DATA FFilledFieldList   INIT .F.
   DATA FfullFileName
   DATA FDbFilter
   DATA FIndexList   INIT hb_HSetOrder( hb_HSetCaseMatch( { => }, .F. ), .T. )  // <className> => <indexName> => <indexObject>
   DATA FInitialized       INIT .F.
   DATA FisMetaTable       INIT .T.
   DATA FFound    INIT .F.
   DATA FmainIndex
   DATA FMasterSourceFieldBuffer INIT hb_HSetCaseMatch( { => }, .F. )
   DATA FOnActiveSetKeyVal  INIT .F.
   DATA FPrimaryIndex
   DATA FPrimaryIndexList INIT hb_HSetOrder( hb_HSetCaseMatch( { => }, .F. ), .T. )  // <className> => <indexName>
   DATA FRecNo    INIT 0
   DATA FRecordList
   DATA FTableFileName     INIT "" // to be assigned (INIT) on inherited classes
   DATA tableState INIT {}
   DATA tableStateLen INIT 0

   METHOD __CheckIndexes()
   METHOD AddRec( origin )

   METHOD bindIndex( reusing, indexName, indexType, curClass, default )

   METHOD CheckDbStruct()
   METHOD Clear()
   METHOD CreateTableInstance()
   METHOD DefineFieldsFromDb()
   METHOD FillFieldList()
   METHOD FillPrimaryIndexes( curClass, origin )
   METHOD FixDbStruct( aNewStruct, message )
   METHOD GetAutoCreate() INLINE iif( ::FAutoCreate = NIL, iif( ::DataBase = NIL, OORDB_DEFAULT_AUTOCREATE, ::DataBase:TableAutoCreate ), ::FAutoCreate )
   METHOD getBaseDocument() BLOCK ;
      {|self|
         IF empty( ::FbaseDocument )
            RETURN ::tableBaseClass
         ENDIF
         RETURN ::FbaseDocument
      }
   METHOD GetBof()
   METHOD GetDataBase()
   METHOD GetEof()
   METHOD GetErrorBlock() INLINE iif( FErrorBlock = NIL, FErrorBlock := {| oErr| ErrorBlockOORDB( oErr ) }, FErrorBlock )
   METHOD GetFound()
   METHOD getFullFileName()
   METHOD GetId() INLINE ::FBaseKeyField:KeyVal()
   METHOD GetIndex()
   METHOD getMainIndex() BLOCK ;
      {|self|
         IF ::FisMetaTable
            ::isMetaTable := .F.
         ENDIF
         RETURN ::FmainIndex
      }

   METHOD GetRecNo()
   METHOD GetRecordList
   METHOD getTableBaseClass INLINE iif( ::FbaseKeyIndex = nil, "", ::FbaseKeyIndex:TableBaseClass )
   METHOD InitDataBase INLINE TDataBase():New()
   METHOD InitTable()
   METHOD onAfterChangeIndex() VIRTUAL
   METHOD onBeforeChangeIndex() INLINE .T.
   METHOD RawGet4Seek( direction, xField, keyVal, index, softSeek )
   METHOD SetDataBase( dataBase )
   METHOD SetErrorBlock( errorBlock ) INLINE FErrorBlock := errorBlock
   METHOD SetisMetaTable( isMetaTable )
   METHOD SetTableFileName( tableFileName ) BLOCK ;
        {|self,tableFileName|
            ::FTableFileName := tableFileName
            RETURN ::FTableFileName
        }

PUBLIC:

   DATA aliasTmp
   DATA allowOnDataChange  INIT .F.
   DATA autoMasterSource   INIT .F.
   DATA autoOpen           INIT .T.
   DATA dataIsOEM          INIT .T.
    /*!
        array of possible TFieldTable's that have this (SELF) object referenced
     */
   DATA DetailSourceList INIT { => }
   DATA fieldNamePrefix INIT "Field_" // Table Field Name prefix
   DATA FUnderReset INIT .F.
   DATA indexNamePrefix INIT "Index_"
   DATA LinkedObjField

   DATA OnDataChangeBlock
   DATA OnDataChangeBlock_Param

   DATA validateDbStruct INIT .T.      // On Open, Check for a valid struct dbf (against DEFINE FIELDS )

   CONSTRUCTOR New( MasterSource, tableName )
   DESTRUCTOR OnDestruct()
   // ON ERROR FUNCTION OODB_ErrorHandler( ... )

   METHOD _( syncFromAlias ) INLINE ::GetDisplayFieldList( syncFromAlias )

   METHOD __DefineFields() VIRTUAL         // DEFINE FIELDS
   METHOD __DefineFields_Exit() VIRTUAL         // DEFINE FIELDS
   METHOD __DefinePrimaryIndex() VIRTUAL   // DEFINE PRIMARY INDEX
   METHOD __DefineSecondaryIndexes() VIRTUAL        // DEFINE SECONDARY INDEX

   METHOD __Seek( direction, Value, index, lSoftSeek )
   METHOD BaseSeek( baseKeyValue ) INLINE ::FBaseKeyIndex:Seek( baseKeyValue )
   METHOD BuildFieldBlockFromFieldExpression( fieldExp, returnMode, field, index )
   METHOD AddCustomIndex( index )
   METHOD AddFieldAlias( nameAlias, fld, private )
   METHOD AddFieldMessage( messageName, AField, isAlias )
   METHOD addIndexMessage( indexName, default )
   METHOD Cancel
   METHOD Childs( ignoreAutoDelete, block, curClass, childs )
   METHOD ChildSource( tableName, destroyChild )
   METHOD COUNT( bForCondition, bWhileCondition, index, scope )
   METHOD CreateTable( fullFileName )
   METHOD DbFilterPull()
   METHOD DbFilterPush( ignoreMasterKey )
   METHOD DefineRelations       VIRTUAL
   METHOD Destroy()
   METHOD dbEval( bBlock, bForCondition, bWhileCondition, index, scope )
   METHOD DbGoBottom INLINE ::DbGoBottomTop( -1 )
   METHOD DbGoInsideScope() INLINE iif( ! ::InsideScope(), ::DbGoTop(), .T. )
   METHOD dbGoto( RecNo )
   METHOD DbGoTop INLINE ::DbGoBottomTop( 1 )
   METHOD dbSkip( numRecs, lSkipUnique )
   METHOD DELETE( lDeleteChilds )
   METHOD DeleteChilds()
   METHOD Edit( lNoRetry )
   METHOD FieldByName( name, index )
   METHOD FieldByObjClass( objClass, derived, index )
   METHOD FilterEval( index )
   METHOD FindIndex( index )
   METHOD FindMasterSourceField( detailField )
   METHOD Get4Seek( xField, keyVal, index, softSeek ) INLINE ::RawGet4Seek( 1, xField, keyVal, index, softSeek )
   METHOD Get4SeekLast( xField, keyVal, index, softSeek ) INLINE ::RawGet4Seek( 0, xField, keyVal, index, softSeek )
   METHOD GetAsString
   METHOD GetCurrentRecord()
   METHOD GetDisplayFieldBlock( index, asDisplay )
   METHOD GetDisplayFieldList( syncFromAlias )
   METHOD GetErrorString( errorNumber )
   METHOD GetField( fld )
   METHOD GetKeyVal( value )
   METHOD GetMasterSourceClassName()
   METHOD GetPublishedFieldNameList( typeList )
   METHOD GetTableFileName()
   METHOD __GetValue
   METHOD HasFilter() INLINE ::FDbFilter != NIL
   METHOD ImportField( fromField, fieldDbName, fieldName )
   METHOD IndexByName( IndexName, aPos, curClass )
   METHOD Insert( origin )
   METHOD InsideScope( ignoreFilters )
   METHOD Open
   METHOD ordKeyNo() INLINE ::GetIndex():ordKeyNo()
   METHOD Post()
   METHOD RawSeek( Value, index )
   METHOD RecLock( lNoRetry )
   METHOD recordValueList( origin )
   METHOD RecUnLock()
   METHOD Refresh
   METHOD Reset() // Set Field Record to their default values, Sync MasterKeyVal Value
   METHOD SEEK( Value, AIndex, SoftSeek ) INLINE ::__Seek( 0, Value, AIndex, SoftSeek )
   METHOD SeekLast( Value, AIndex, SoftSeek ) INLINE ::__Seek( 1, Value, AIndex, SoftSeek )
   METHOD serializeRecord() INLINE hb_serialize( ::recordValueList() )
   METHOD serializeTable( index ) INLINE hb_serialize( ::tableValueList( index ) )
   METHOD SetAsString( Value ) INLINE ::GetKeyField():AsString := Value
   METHOD SetBaseKeyIndex( baseKeyIndex )
   METHOD SetDbFilter( filter ) INLINE ::FDbFilter := filter
   METHOD SetKeyVal( keyVal )
   METHOD SetPrimaryIndex( primaryIndex )
   METHOD SetPrimaryIndexList( clsName, name )
   METHOD SetId( id ) INLINE ::FBaseKeyField:SetKeyVal( id )
   METHOD __SetValue( value )
   METHOD SkipBrowse( n, lSkipUnique )
   METHOD SkipFilter( n, index )
   METHOD StatePull()
   METHOD StatePush()
   METHOD SyncFromMasterSourceFields()
   METHOD SyncRecNo( fromAlias )
   METHOD TableFileName_Path() INLINE ::DataBase:Directory
   METHOD TableClass INLINE ::ClassName + "@" + ::TableFileName

   METHOD tableValueList( index )

   METHOD UpdateCustomIndexes()

   METHOD Validate( showAlert )

   METHOD OnClassInitializing() VIRTUAL
   METHOD OnCreate() VIRTUAL
   METHOD OnActiveSetKeyVal( value )
   METHOD OnAfterCancel() VIRTUAL
   METHOD OnAfterDelete() VIRTUAL
   METHOD OnAfterEdit() VIRTUAL
   METHOD OnAfterInsert() VIRTUAL
   METHOD OnAfterOpen() VIRTUAL
   METHOD OnAfterPost( changedFieldList ) VIRTUAL
   METHOD OnAfterPostEdit() VIRTUAL
   METHOD OnAfterPostInsert() VIRTUAL
   METHOD OnBeforeCancel() INLINE .T.
   METHOD onBeforeChange_Field() INLINE .T.
   METHOD OnBeforeDelete() INLINE .T.
   METHOD OnBeforeEdit() INLINE .T.
   METHOD OnBeforeInsert() INLINE .T.
   METHOD OnBeforeLock INLINE .T.
   METHOD OnBeforePost() INLINE .T.
   METHOD OnDataChange()
   METHOD OnSetValueToLinkedObjField( /* linkedObjField, value, field */ ) INLINE .T.
   METHOD OnStateChange( oldState ) VIRTUAL
   METHOD OnSyncFromMasterSource() VIRTUAL

   PROPERTY Active READ FActive
   PROPERTY ALIAS READ GetAlias
   PROPERTY AsString READ GetAsString WRITE SetAsString
   PROPERTY AutoCreate READ GetAutoCreate
   PROPERTY baseDocument READ getBaseDocument
   PROPERTY BaseKeyField READ FBaseKeyField
   PROPERTY BaseKeyIndex READ FBaseKeyIndex
   PROPERTY BaseKeyVal READ BaseKeyField:GetKeyVal WRITE BaseKeyField:SetKeyVal
   PROPERTY Bof READ GetBof
   PROPERTY DataBase READ GetDataBase WRITE SetDataBase
   PROPERTY DbFilter READ FDbFilter WRITE SetDbFilter
   PROPERTY DbStruct READ GetDbStruct
   PROPERTY defaultIndexName
   PROPERTY DELETED READ Alias:Deleted()
   PROPERTY DeletingChilds INIT .F.
   PROPERTY DisplayFieldList READ GetDisplayFieldList
   PROPERTY ErrorBlock READ GetErrorBlock WRITE SetErrorBlock
   PROPERTY Eof READ GetEof
   PROPERTY FieldList READ FFieldList
   PROPERTY Found READ GetFound
   PROPERTY fullFileName READ getFullFileName
   PROPERTY FieldTypes READ GetFieldTypes
   PROPERTY GetErrorNumber INIT OORDB_ERROR_NONE
   PROPERTY Id READ GetId WRITE SetId
   PROPERTY indexFieldListByClass INIT hb_HSetCaseMatch( { => }, .F. )  /* list of field index number by table class name */
   PROPERTY Initialized READ FInitialized
   PROPERTY instance READ getInstance
   METHOD   Instances INLINE __S_Instances
   PROPERTY isMemTable INIT .F.
   PROPERTY isMetaTable READ FisMetaTable WRITE SetisMetaTable
   PROPERTY IsTempTable INIT .F.
   PROPERTY KeyExpression READ GetKeyExpression
   PROPERTY KeyField READ GetKeyField
   PROPERTY KeyString READ GetKeyString
   PROPERTY KeyVal READ GetKeyVal WRITE SetKeyVal
   PROPERTY MasterKeyExpression READ GetMasterKeyExpression
   PROPERTY MasterKeyString READ GetMasterKeyString
   PROPERTY MasterKeyVal READ GetMasterKeyVal
   PROPERTY previousEditState
   PROPERTY PrimaryIndexList READ FPrimaryIndexList
   PROPERTY RecCount READ GetAlias:RecCount()
   PROPERTY RecNo READ GetRecNo WRITE DbGoTo
   PROPERTY RecordList READ GetRecordList
   PROPERTY State READ FState
   PROPERTY SubState READ FSubState
   PROPERTY SyncingToContainerField READ FSyncingToContainerField WRITE SetSyncingToContainerField
   PROPERTY TableBaseClass READ getTableBaseClass
   PROPERTY TableFileName READ GetTableFileName WRITE SetTableFileName
   PROPERTY UndoList READ FUndoList

PUBLISHED:

   DATA Cargo

   METHOD   ChildReferenceList INLINE __S_Instances[ ::TableClass, "ChildReferenceList" ]
   PROPERTY CustomIndexList READ FCustomIndexList
   PROPERTY INDEX READ GetIndex WRITE SetIndex
   PROPERTY IndexList READ FIndexList
   PROPERTY IndexName READ GetIndexName WRITE SetIndexName
   PROPERTY MainIndex READ getMainIndex       /* child table can miss a primary index but a secondary one sets dependency */
   PROPERTY MasterKeyField READ GetMasterKeyField
   PROPERTY MasterSource READ GetMasterSource WRITE SetMasterSource
   PROPERTY PrimaryIndex READ FPrimaryIndex
   PROPERTY PublishedFieldNameList READ GetPublishedFieldNameList
   PROPERTY READONLY READ FReadOnly WRITE SetReadOnly
   PROPERTY Value READ __GetValue( ... ) WRITE __SetValue

ENDCLASS

/*
    New
*/
METHOD New( masterSource, tableName ) CLASS TTable

   LOCAL ms

   IF __S_Instances = nil
      __S_Instances := hb_HSetCaseMatch( { => }, .F. )
      __S_dataBase := hb_HSetCaseMatch( { => }, .F. )
   ENDIF

   ::FInitialized := .T.

   ::Process_TableName( tableName )

   IF ::DataBase == NIL
      ::DataBase := ::InitDataBase()
   ENDIF

   IF masterSource = NIL .AND. !Empty( ms := ::GetMasterSourceClassName() ) .AND. ::autoMasterSource
      masterSource := __ClsInstName( ms )
      masterSource:autoMasterSource := .T.
      masterSource:New()
   ENDIF

    /*!
     * Sets the MasterSource (maybe will be needed in the fields definitions ahead )
     */
   IF masterSource != NIL

        /*
         * As we have not fields defined yet, this will not execute SyncFromMasterSourceFields()
         */
      ::SetMasterSource( masterSource )

   ENDIF

    /*!
     * Load definitions for Fields
     */
   ::FillFieldList()

   /* Load Primary indexes */
   ::__DefinePrimaryIndex()

   /* Load Secondary Indexes */
   ::__DefineSecondaryIndexes()

   /* now create instance is allowed */
   ::FcanCreateInstance := .T.

   IF !::isMetaTable
      ::CreateTableInstance()
   ENDIF

   RETURN Self

/*
    OnDestruct
*/
METHOD PROCEDURE OnDestruct() CLASS TTable
   LOCAL dbfName, indexName
   LOCAL curCLass
   LOCAL index

    FOR EACH curClass IN ::FIndexList
        FOR EACH INDEX IN curClass
            IF index:temporary .AND. !empty( ::index:fileName )
//                ::alias:ordDestroy( index:tagName )
                IF hb_fileExists( ::index:fileName )
                    fErase( ::index:fileName )
                ENDIF
            ENDIF
        NEXT
    NEXT

   IF ::aliasTmp != NIL
      dbfName := ::aliasTmp:dbInfo( DBI_FULLPATH )
      indexName := ::aliasTmp:dbOrderInfo( DBOI_FULLPATH )
      ::aliasTmp:dbCloseArea()
      FErase( dbfName )
      FErase( indexName )
   ENDIF

   // ::Destroy()

   RETURN

/*
    __CheckIndexes
*/
METHOD PROCEDURE __CheckIndexes() CLASS TTable

   LOCAL curClass
   LOCAL index

   FOR EACH curClass IN ::FIndexList
      FOR EACH index IN curClass
         IF ! index:temporary
            index:openIndex()
         ENDIF
      NEXT
   NEXT

   RETURN

/*
    __Seek
*/
METHOD FUNCTION __Seek( direction, Value, index, lSoftSeek ) CLASS TTable

   LOCAL AIndex

   AIndex := ::FindIndex( index )

   RETURN AIndex:__Seek( direction, Value, lSoftSeek )

/*
    AddCustomIndex
*/
METHOD PROCEDURE AddCustomIndex( index ) CLASS TTable

   IF AScan( ::FCustomIndexList, {| e| e == index } ) = 0
      AAdd( ::FCustomIndexList, index )
   ENDIF

   RETURN

/*
    AddFieldAlias
*/
METHOD PROCEDURE AddFieldAlias( nameAlias, fld, private ) CLASS TTable

   LOCAL AField

   SWITCH ValType( fld )
   CASE 'C'
      AField := ::FieldByName( fld )
      EXIT
   CASE 'O'
      IF fld:IsDerivedFrom( "TField" )
         AField := fld
         EXIT
      ENDIF
   ENDSWITCH

   IF AField != NIL
      IF AField:nameAlias != NIL
         RAISE ERROR "Alias Field Name '" + nameAlias + "' attempt to re-declare alias name on Field"
      ENDIF
      ::AddFieldMessage( nameAlias, AField, .T. )
      AField:nameAlias := nameAlias
      IF private != NIL
         AField:nameAliasPublished := !private
      ENDIF
   ELSE
      RAISE ERROR "Alias Field Name '" + nameAlias + "' not valid Field from"
   ENDIF

   RETURN

/*
    AddFieldMessage
*/
METHOD PROCEDURE AddFieldMessage( messageName, AField, isAlias ) CLASS TTable
    LOCAL i
    LOCAL index
    LOCAL fld

    fld := ::FieldByName( messageName, @index )

    IF index = 0
        IF isAlias == .T.
            ::FieldByName( AField:Name, @index )
        ELSE
            AAdd( ::FFieldList, AField )
            index := Len( ::FFieldList )
            IF ! hb_hHasKey( ::FindexFieldListByClass, AField:tableBaseClass )
                ::FindexFieldListByClass[ AField:tableBaseClass ] := {}
            ENDIF
            AAdd( ::FindexFieldListByClass[ AField:tableBaseClass ], index )
        ENDIF
    ELSE
        IF fld:IsKeyIndex
            RAISE ERROR "Attempt to overwrite key index Field '" + messageName + "' on Class <" + ::ClassName + ">"
        ENDIF
        IF AField:TableBaseClass == fld:TableBaseClass
            RAISE ERROR "Attempt to Re-Declare Field '" + messageName + "' on Class <" + ::ClassName + ">"
        ENDIF
        ::FFieldList[ index ] := AField
    ENDIF

    IF __s_fieldList = nil
        __s_fieldList := hb_HSetCaseMatch( { => }, .F. )
    ENDIF

    IF ! hb_hHasKey( __s_fieldList, ::className, @i )
        __s_fieldList[ ::className ] := hb_HSetCaseMatch( { => }, .F. )
        i := hb_hPos( __s_fieldList, ::className )
    ENDIF

    /* check if need to add the field message to the class */
    IF ! hb_hHasKey( hb_hValueAt( __s_fieldList, i ), messageName )

        hb_hValueAt( __s_fieldList, i )[ messageName ] := nil

        IF index < 1 .OR. index > Len( ::FieldList )
            RAISE ERROR "Illegal index field for '" + messageName + "' on Class <" + ::ClassName + ">"
        ENDIF

        EXTEND OBJECT Self WITH MESSAGE ::fieldNamePrefix + messageName INLINE ::FieldList[ index ]

    ENDIF

RETURN

/*
    addIndexMessage
*/
METHOD PROCEDURE addIndexMessage( indexName, default ) CLASS TTable
    LOCAL aPos
    LOCAL i
    LOCAL x
    LOCAL y

    IF __s_indexList = nil
        __s_indexList := hb_hSetCaseMatch( {=>}, .F. )
    ENDIF

    IF ! hb_hHasKey( __s_indexList, ::className, @i )
        __s_indexList[ ::className ] := hb_hSetCaseMatch( {=>}, .F. )
        i := hb_hPos( __s_indexList, ::className )
    ENDIF

    IF ! hb_hHasKey( hb_hValueAt( __s_indexList, i ), indexName ) .AND. ::indexByName( indexName, @aPos ) != nil

        hb_hValueAt( __s_indexList, i )[ indexName ] := nil

        x := aPos[ 1 ]
        y := aPos[ 2 ]

        EXTEND OBJECT self WITH MESSAGE ::indexNamePrefix + indexName INLINE hb_hValueAt( hb_hValueAt( ::FIndexList, x ), y )

    ENDIF

    IF default = .T.
        ::FdefaultIndexName := indexName
    ENDIF

RETURN

/*
    AddRec
*/
METHOD FUNCTION AddRec( origin ) CLASS TTable

   LOCAL Result
   LOCAL field
   LOCAL errObj
   LOCAL INDEX
   LOCAL newValue
   LOCAL aKeyFields := {}
   LOCAL itm
   LOCAL originatedFields
   LOCAL filledFieldList

   IF ::FReadOnly
      SHOW WARN "Table is marked as READONLY..."
      RETURN .F.
   ENDIF

   IF ::FHasDeletedOrder
      index := "__AVAIL"
   ELSEIF ::FPrimaryIndex != NIL
      index := ::FPrimaryIndex:Name
   ENDIF

   ::FRecNoBeforeInsert := ::RecNo()

   IF !( Result := ::Alias:AddRec( index ) )
      RETURN Result
   ENDIF

   ::FEof := .F.
   ::FBof := .F.

   ::FRecNo := ::Alias:RecNo

   ::SetState( dsInsert )
   ::FpreviousEditState := dsInsert
   ::FSubState := dssAdding

   // Clear fields to empty values
   ::Clear()

    /*
     * Write the MasterKeyField
     * Write the PrimaryKeyField
     * Write the Fields that have a NewValue
     */
   BEGIN SEQUENCE WITH ::ErrorBlock

      IF origin != nil .AND. ! empty( origin := ::recordValueList( origin ) )
        originatedFields := {}
         FOR EACH itm IN origin
            field := ::fieldByName( itm:__enumKey )
            IF field != nil .AND. !field:calculated .AND. !field:autoIncrement .AND. field:fieldMethodType = "C" .AND. ! field:readOnly
               ::alias:eval( field:fieldWriteBlock, field:translateToFieldValue( itm:__enumValue ) )
               aAdd( originatedFields, field:name )
            ENDIF
         NEXT
      ENDIF

      filledFieldList := ::FillPrimaryIndexes( self, origin )

      IF originatedFields != nil
         FOR EACH itm IN originatedFields
            IF ! hb_hHasKey( filledFieldList, itm )
               ::fieldByName( itm ):getData()
            ENDIF
         NEXT
      ENDIF

      FOR EACH field IN ::FFieldList
         IF !field:Calculated .AND. field:FieldMethodType = 'C' .AND. !field:PrimaryKeyComponent .AND. field:WrittenValue == NIL .AND. field:Enabled
            newValue := field:NewValue
            IF newValue != NIL .OR. field:AutoIncrement
               IF !field:IsKeyIndex
                  field:SetData( newValue, .T. )
               ELSE
                  AAdd( aKeyFields, { field, newValue } )
               ENDIF
            ENDIF
         ENDIF
      NEXT

      /* poblate key field's AFTER all other fields are filled */
      FOR EACH itm IN aKeyFields
         itm[ 1 ]:SetData( itm[ 2 ], .T. )
      NEXT

      IF origin != nil
         FOR EACH itm IN origin
            field := ::fieldByName( itm:__enumKey )
            IF field != nil .AND. !field:calculated .AND. !field:autoIncrement .AND. field:fieldMethodType = "C" .AND. ! field:readOnly
               field:getData()
            ENDIF
         NEXT
      ENDIF

   RECOVER USING errObj

      ::TTable:Delete()
      ::RecUnLock()

      SHOW ERROR errObj

      Result := .F.

   END SEQUENCE

   ::FSubState := dssNone

   RETURN Result

/*
    bindIndex
*/
METHOD PROCEDURE bindIndex( reusing, indexName, indexType, curClass, default ) CLASS TTable
    LOCAL index

    index := ::indexByName( indexName )

    IF index != nil
        index:bindIndex( reusing, indexType, curClass )
        ::addIndexMessage( indexName, default )
    ENDIF

RETURN

/*
    BuildFieldBlockFromFieldExpression
*/
METHOD FUNCTION BuildFieldBlockFromFieldExpression( fieldExp, returnMode, field, index ) CLASS TTable

   LOCAL nTokens
   LOCAL i
   LOCAL s
   LOCAL table
   LOCAL fldName
   LOCAL block
   LOCAL lAsDisplay

   fieldExp := AllTrim( fieldExp )

   nTokens := NumToken( fieldExp, ":" )

   table := Self

   FOR i := 1 TO nTokens
      fldName := Token( fieldExp, ":", i )
      IF ( field := table:FieldByName( fldName, @index ) ) = NIL .AND. i = nTokens .AND. Upper( Right( fldName, 10 ) ) == "_ASDISPLAY"
         field := table:FieldByName( Left( fldName, Len( fldName ) - 10 ), @index )
         lAsDisplay := field != NIL
      ELSE
         lAsDisplay := .F.
      ENDIF
      IF field != NIL
         IF i = 1
            s := "::FieldList[" + hb_nToS( index ) + "]"
         ELSE
            s += ":DataObj:FieldList[" + hb_nToS( index ) + "]"
         ENDIF
         IF field:IsDerivedFrom( "TFieldTable" )
            IF field:LinkedTable:isMetaTable
               table := field:LinkedTable
            ELSE
               table := field:DataObj
            ENDIF
         ENDIF
      ELSE
         RETURN NIL
      ENDIF
   NEXT

   BEGIN SEQUENCE WITH {| oErr| Break( oErr ) }
      IF lAsDisplay
         block := &( "{|Self|" + s + ":AsDisplay}" )
      ELSE
         IF Empty( returnMode ) // returns the TField object
            block := &( "{|Self|" + s + "}" )
         ELSE
            block := &( "{|Self|" + s + ":" + returnMode + "}" )
         ENDIF
      ENDIF
   RECOVER
      block := NIL
   END SEQUENCE

   RETURN block

/*
    Cancel
*/
METHOD PROCEDURE Cancel CLASS TTable

   LOCAL AField

   IF AScan( { dsInsert, dsEdit }, ::State ) = 0
      // ::Error_Table_Not_In_Edit_or_Insert_mode()
      RETURN
   ENDIF

   IF ::OnBeforeCancel()

      SWITCH ::State
      CASE dsInsert
         // FOR EACH AField IN ::FFieldList
         // IF !AField:Calculated .AND. AField:FieldMethodType = "C" .AND. !Empty( AField:Value ) .AND. AField:Enabled .AND. AField:Validate( .F. ) != NIL
         // AField:Reset()
         // ENDIF
         // NEXT
         ::TTable:Delete( .T. )
         EXIT
      CASE dsEdit
         FOR EACH AField IN ::FieldList
            IF !AField:Calculated .AND. AField:FieldMethodType = "C" .AND. AField:Changed
               AField:revertValue()
            ENDIF
         NEXT
         EXIT
      OTHERWISE

      ENDSWITCH

      ::RecUnLock()

      ::OnAfterCancel()

      IF ::FRecNoBeforeInsert != NIL
         ::RecNo := ::FRecNoBeforeInsert
         ::FRecNoBeforeInsert := NIL
      ENDIF

   ENDIF

   RETURN

/*
    CheckDbStruct
*/
METHOD FUNCTION CheckDbStruct() CLASS TTable

   LOCAL AField
   LOCAL n
   LOCAL i
   LOCAL aDb
   LOCAL dbsType
   LOCAL dbsLen
   LOCAL dbsDec
   LOCAL dbsSize
   LOCAL sResult := ""

   IF !hb_HHasKey( __S_Instances[ ::TableClass ], "DbStructValidating" )

      aDb := AClone( ::dbStruct() )

      __S_Instances[ ::TableClass, "DbStructValidating" ] := NIL

      FOR EACH AField IN ::FieldList
         IF AField:FieldMethodType = "C" .AND. !AField:Calculated .AND. AField:UsingField = NIL

            n := AScan( aDb, {| e| Upper( e[ 1 ] ) == Upper( AField:DBS_NAME ) } )

            IF AField:FieldType = ftTable .AND. ( i := FindTableBaseClass( AField ) ) > 0
               dbsType := BaseKeyFieldList[ i, 2 ]
               dbsLen  := BaseKeyFieldList[ i, 3 ]
               dbsDec  := BaseKeyFieldList[ i, 4 ]
               dbsSize := BaseKeyFieldList[ i, 5 ]
            ELSE
               dbsType := AField:DBS_TYPE
               dbsLen  := AField:DBS_LEN
               dbsDec  := AField:DBS_DEC
               IF dbsType = "C"
                  dbsSize := AField:Size
               ENDIF
            ENDIF

            /* TFieldTable wants assignable field type */
            IF AField:FieldType = ftTable .AND. dbsType = "+"
               dbsType := "I"
            ENDIF

            IF n = 0
               AAdd( aDb, { AField:DBS_NAME, dbsType, dbsLen, dbsDec } )
               sResult += "Field not found '" + AField:DBS_NAME + E"'\n"
            ELSEIF AField:FieldType = ftTable .AND. aDb[ n, 2 ] = "+"
               sResult += "Wrong type ('" + aDb[ n, 2 ] + "') on TFieldTable '" + AField:DBS_NAME + "', must be '" + dbsType + E"'\n"
               aDb[ n, 2 ] := dbsType
            ELSEIF !aDb[ n, 2 ] == dbsType .AND. !( aDb[ n, 2 ] $ "+I" .AND. dbsType $ "I" )
               sResult += "Wrong type ('" + aDb[ n, 2 ] + "') on field '" + AField:DBS_NAME + "', must be '" + dbsType + E"'\n"
               aDb[ n, 2 ] := dbsType
               aDb[ n, 3 ] := dbsLen
               aDb[ n, 4 ] := dbsDec
            ELSEIF aDb[ n, 2 ] = "C" .AND. aDb[ n, 3 ] < dbsSize
               sResult += "Wrong len value (" + hb_nToS( aDb[ n, 3 ] ) + ") on 'C' field '" + AField:DBS_NAME + E"', must be " + hb_nToS( dbsLen ) + E"\n"
               aDb[ n, 3 ] := dbsLen
            ELSEIF aDb[ n, 2 ] $ "+I" .AND. ! aDb[ n, 3 ] = dbsLen
               sResult += "Wrong len value (" + hb_nToS( aDb[ n, 3 ] ) + ") on '" + aDb[ n, 2 ] + "' field '" + AField:DBS_NAME + E"', must be " + hb_nToS( dbsLen ) + E"\n"
               aDb[ n, 3 ] := dbsLen
            ELSEIF aDb[ n, 2 ] = "N" .AND. ( !aDb[ n, 3 ] == dbsLen .OR. !aDb[ n, 4 ] == dbsDec )
               sResult += "Wrong len/dec values (" + hb_nToS( aDb[ n, 3 ] ) + "," + hb_nToS( aDb[ n, 4 ] ) + ") on 'N' field '" + AField:DBS_NAME + E"', must be " + hb_nToS( dbsLen ) + "," + hb_nToS( dbsDec ) + E"\n"
               aDb[ n, 3 ] := dbsLen
               aDb[ n, 4 ] := dbsDec
            ENDIF

         ENDIF
      NEXT

      __S_Instances[ ::TableClass, "DbStructValidated" ] := .T.

      IF ! Empty( sResult )
         sResult := "Error on Db structure." + ;
            E"\nClass: " + ::ClassName() + ", Table: " + ::Alias:Name + ;
            E"\n\n-----\n" + ;
            sResult + ;
            E"-----\n\n"
         ? sResult

         __S_Instances[ ::TableClass, "DbStructValidated" ] := ::FixDbStruct( aDb, sResult )

      ENDIF

      hb_HDel( __S_Instances[ ::TableClass ], "DbStructValidating" )

   ENDIF

   RETURN .T.

/*
    Childs
*/
METHOD FUNCTION Childs( ignoreAutoDelete, block, curClass, childs ) CLASS TTable
   RETURN F_Childs( Self, ignoreAutoDelete, block, curClass, childs )

/*
    F_Childs
*/
STATIC FUNCTION F_Childs( Self, ignoreAutoDelete, block, curClass, childs )

   LOCAL childTableName
   LOCAL ChildDB
   LOCAL clsName
   LOCAL destroyChild

   IF curClass = NIL
      curClass := Self
   ENDIF

   IF childs = NIL
      childs := {}
   ENDIF

   clsName := curClass:ClassName

   IF clsName == "TTABLE"
      RETURN childs
   ENDIF

   IF hb_HHasKey( ::DataBase:ParentChildList, clsName )

      FOR EACH childTableName IN ::DataBase:GetParentChildList( clsName )

         IF !ignoreAutoDelete == .T. .OR. !::DataBase:TableList[ childTableName, "AutoDelete" ]

            ChildDB := ::ChildSource( childTableName, @destroyChild )

            IF ChildDB != NIL

               IF ::DataBase:TableList[ childTableName, "IndexName" ] != NIL
                  ChildDB:IndexName := ::DataBase:TableList[ childTableName, "IndexName" ]
               ENDIF

               IF ChildDB:isMetaTable
                  ChildDB:isMetaTable := .F.
               ENDIF

               ChildDB:StatePush()

               ChildDB:MainIndex:Scope := NIL

               IF ChildDB:MainIndex:Descend
                  ChildDB:MainIndex:dbGoTop()
               ELSE
                  ChildDB:MainIndex:dbGoBottom()
               ENDIF

               IF !ChildDB:Eof() .AND. !Empty( ChildDB:BaseKeyField:Value )
                  AAdd( childs, iif( block == NIL, ChildDB:ClassName, block:Eval( ChildDB ) ) )
                  ChildDB:StatePull()
                  IF destroyChild
                     ChildDB:Destroy()
                  ENDIF
                  LOOP
               ENDIF

               ChildDB:StatePull()

               IF destroyChild
                  ChildDB:Destroy()
               ENDIF

            ENDIF

         ENDIF

      NEXT

   ENDIF

   F_Childs( Self, ignoreAutoDelete, block, curClass:Super, childs )

   RETURN childs

/*
    ChildSource
*/
METHOD FUNCTION ChildSource( tableName, destroyChild ) CLASS TTable

   LOCAL itm
   LOCAL childDb

   tableName := Upper( tableName )

   /* tableName is in the DetailSourceList */
   FOR EACH itm IN ::DetailSourceList
      IF itm:ClassName() == tableName
         itm:Reset()
         destroyChild := .F.
         RETURN itm
      ENDIF
   NEXT

   destroyChild := .T.

   childDb := __ClsInstFromName( tableName ):New( Self )

   RETURN childDb

/*
    Clear
*/
METHOD PROCEDURE Clear() CLASS TTable
    LOCAL AFIELD

    FOR EACH AField IN ::FFieldList
        IF AField:IsTableField
            AField:Clear()
        ENDIF
    NEXT

RETURN

/*
    Count : number of records
*/
METHOD FUNCTION COUNT( bForCondition, bWhileCondition, index, scope ) CLASS TTable

   LOCAL nCount := 0

   ::dbEval( {|| ++nCount }, bForCondition, bWhileCondition, index, scope )

   RETURN nCount

/*
    CreateTable
*/
METHOD FUNCTION CreateTable( fullFileName ) CLASS TTable

   LOCAL aDbs := {}
   LOCAL fld
   LOCAL n
   LOCAL dbsType
   LOCAL dbsLen
   LOCAL dbsDec

   ::FillFieldList()

   FOR EACH fld IN ::FieldList
      IF fld:IsTableField .AND. !fld:ReUseField
         IF fld:FieldType = ftTable
            n := FindTableBaseClass( fld )
            IF n > 0
               dbsType := iif( BaseKeyFieldList[ n, 2 ] = "+", "I", BaseKeyFieldList[ n, 2 ] )
               dbsLen  := BaseKeyFieldList[ n, 3 ]
               dbsDec  := BaseKeyFieldList[ n, 4 ]
            ELSE
               dbsType := NIL
            ENDIF
         ELSE
            dbsType := fld:DBS_TYPE
            dbsLen  := fld:DBS_LEN
            dbsDec  := fld:DBS_DEC
         ENDIF
         IF dbsType != NIL
            AAdd( aDbs, { fld:DBS_NAME, dbsType, dbsLen, dbsDec } )
         ENDIF
      ENDIF
   NEXT

   IF Empty( aDbs )
      SHOW WARN "createTable(): Cannot create table class with empty data..."
      RETURN .F.
   ENDIF

   IF fullFileName = NIL
      fullFileName := ::fullFileName
   ENDIF

RETURN dbCreate( fullFileName, aDbs )

/*
    CreateTableInstance
*/
METHOD PROCEDURE CreateTableInstance() CLASS TTable

   LOCAL itm
   LOCAL n

   ::FisMetaTable := .F.

   ::InitTable()

   /* Check for a valid db structure (based on definitions on DEFINE FIELDS) */
   IF !Empty( ::TableFileName ) .AND. ::validateDbStruct .AND. !hb_HHasKey( __S_Instances[ ::TableClass ], "DbStructValidated" )
      ::CheckDbStruct()
   ENDIF

   /* sets the DBS field info for each table field */
   FOR EACH itm IN ::FFieldList
      IF itm:IsTableField()
         n := Upper( itm:DBS_NAME )
         n := AScan( ::DbStruct, {| e| e[ 1 ] == n } )
         IF n > 0
            itm:SetDbStruct( ::DbStruct[ n ] )
         ENDIF
         itm:Clear()
      ENDIF
   NEXT

   ::FState := dsBrowse

   ::__CheckIndexes()

   ::FState := dsInactive

    /* get default index by default index name */
    ::FMainIndex := getDefaultIndexByDefaultIndexName( self )

    IF ::FMainIndex = nil
        IF hb_HHasKey( ::FIndexList, ::ClassName )
            ::FMainIndex := HB_HValueAt( ::FIndexList[ ::ClassName ], 1 )
        ELSE
            IF ::PrimaryIndex != NIL
                ::FMainIndex := ::PrimaryIndex
            ENDIF
        ENDIF
    ENDIF

   IF Empty( ::IndexName )
      ::SetIndex( ::FMainIndex )
   ENDIF

   ::OnCreate()

   IF ::autoOpen
      ::Open()
   ENDIF

   RETURN

/*
    DbEval
*/
METHOD PROCEDURE dbEval( bBlock, bForCondition, bWhileCondition, index, scope ) CLASS TTable

   LOCAL oldIndex
   LOCAL oldScope

   ::StatePush()

   IF index != NIL
      oldIndex := ::IndexName
      index := ::FindIndex( index )
      IF index != NIL
         ::IndexName := index:Name
      ENDIF
   ENDIF

   IF scope != NIL
      oldScope := ::GetIndex():Scope
      ::GetIndex():Scope := scope
   ENDIF

   ::dbGoTop()

   WHILE !::Eof() .AND. ( bWhileCondition == NIL .OR. bWhileCondition:Eval( Self ) )

      IF bForCondition == NIL .OR. bForCondition:Eval( Self )
         bBlock:Eval( Self )
      ENDIF

      IF !::dbSkip( 1 )
         EXIT
      ENDIF

   ENDDO

   IF oldScope != NIL
      ::GetIndex():Scope := oldScope
   ENDIF

   IF oldIndex != NIL
      ::IndexName := oldIndex
   ENDIF

   ::StatePull()

   RETURN

/*
    DbFilterPull
*/
METHOD PROCEDURE DbFilterPull() CLASS TTable

   ::FDbFilter := ATail( ::FDbFilterStack )[ 1 ]
   IF !ATail( ::FDbFilterStack )[ 2 ] == NIL
      ::FMasterSource := ATail( ::FDbFilterStack )[ 2 ]
   ENDIF
   hb_ADel( ::FDbFilterStack, Len( ::FDbFilterStack ), .T. )

   RETURN

/*
    DbFilterPush
*/
METHOD PROCEDURE DbFilterPush( ignoreMasterKey ) CLASS TTable

   AAdd( ::FDbFilterStack, { ::FDbFilter, iif( ignoreMasterKey == .T., ::FMasterSource, NIL ) } )
   ::FDbFilter := NIL
   IF ignoreMasterKey == .T.
      ::FMasterSource := NIL
   ENDIF

   RETURN

/*
    DbGoBottomTop
*/
METHOD FUNCTION DbGoBottomTop( n ) CLASS TTable

   IF AScan( { dsEdit, dsInsert }, ::FState ) > 0
      ::Post()
   ENDIF

   IF ::GetIndex() != NIL
      IF n = 1
         RETURN ::GetIndex():dbGoTop()
      ELSE
         RETURN ::GetIndex():dbGoBottom()
      ENDIF
   ELSE
      IF n = 1
         ::Alias:dbGoTop()
      ELSE
         ::Alias:dbGoBottom()
      ENDIF

      IF ::HasFilter()
         ::DbFilterPush()
         ::GetCurrentRecord()
         ::DbFilterPull()
         IF !::FilterEval() .AND. !::SkipFilter( n )
            ::dbGoto( 0 )
            RETURN .F.
         ENDIF
      ENDIF

   ENDIF

   RETURN ::GetCurrentRecord()

/*
    DbGoTo
*/
METHOD FUNCTION dbGoto( RecNo ) CLASS TTable

   ::Alias:dbGoto( RecNo )

   RETURN ::GetCurrentRecord()

/*
    DbSkip
*/
METHOD FUNCTION dbSkip( numRecs, lSkipUnique ) CLASS TTable

   LOCAL result

   IF AScan( { dsEdit, dsInsert }, ::FState ) > 0
      ::Post()
   ENDIF

   IF ::GetIndex() != NIL
      RETURN ::GetIndex():dbSkip( numRecs, lSkipUnique )
   ELSE
      IF !::HasFilter
         result := ::Alias:dbSkip( numRecs ) /* because on Bof returns .F. */
         ::GetCurrentRecord()
         RETURN result
      ENDIF
   ENDIF

   RETURN ::SkipFilter( numRecs )

/*
    FIELDS END
*/
METHOD PROCEDURE DefineFieldsFromDb() CLASS TTable

   LOCAL dbStruct
   LOCAL fld
   LOCAL AField

   IF ::Alias != NIL .AND. Empty( ::FFieldList ) .AND. !Empty( dbStruct := ::GetDbStruct() )
      FOR EACH fld IN dbStruct

         AField := __ClsInstFromName( ::FieldTypes[ fld[ 2 ] ] ):New( Self )

         AField:FieldMethod := fld[ 1 ]
         IF AField:IsDerivedFrom( "TFieldString" )
            AField:Size := fld[ 3 ]
         ENDIF
         IF AField:IsDerivedFrom( "TFieldNumeric" )
            AField:DBS_LEN := fld[ 3 ]
            AField:DBS_DEC := fld[ 4 ]
         ENDIF
         AField:AddFieldMessage()

      NEXT

   ENDIF

   RETURN

/*
    delete
*/
METHOD FUNCTION delete( lDeleteChilds ) CLASS TTable

   LOCAL AField
   LOCAL aChilds
   LOCAL child
   LOCAL lDel
   LOCAL allowOnDataChange
   LOCAL result := .F.

   IF AScan( { dsBrowse, dsEdit, dsInsert }, ::State ) = 0
      ::Error_Table_Not_In_Browse_or_Edit_Insert_State()
      outErr( e"\nError on delete() : Table not in browse or edit/insert state" )
      RETURN .F.
   ENDIF

   IF ::State = dsBrowse .AND. !::RecLock()
      outErr( e"\nError on delete() : recLock()" )
      RETURN .F.
   ENDIF

   allowOnDataChange := ::allowOnDataChange
   ::allowOnDataChange := .F.

   IF ::OnBeforeDelete()

      aChilds := ::Childs()

      lDel := .T.

      IF !Empty( aChilds ) .AND. !Empty( ::BaseKeyField:Value )
         FOR EACH child IN aChilds
            IF ! ::DataBase:TableList[ child, "AutoDelete" ]
               lDel := .F.
            ENDIF
         NEXT
         IF !lDel .AND. !lDeleteChilds == .T.
            SHOW WARN "Error_Table_Has_Childs"
            ::RecUnLock()
            ::allowOnDataChange := allowOnDataChange
            outErr( e"\nError on delete() : Table has childs" )
            RETURN .F.
         ENDIF
         IF !::DeleteChilds()
            SHOW WARN "Error_Deleting_Childs"
            ::RecUnLock()
            ::allowOnDataChange := allowOnDataChange
            outErr( e"\nError on delete() : Deleting childs" )
            RETURN .F.
         ENDIF
      ENDIF

      FOR EACH AField IN ::FieldList
         AField:delete()
      NEXT

      IF ::FHasDeletedOrder()
         ::Alias:dbDelete()
      ENDIF

      result := .T.

   ELSE
      outErr( e"\nError on delete() : canceled by onBeforeDelete()" )
   ENDIF

   ::RecUnLock()

   ::allowOnDataChange := allowOnDataChange

   IF result
      ::GetCurrentRecord()
      ::OnAfterDelete()
   ENDIF

   RETURN result

/*
    DeleteChilds
*/
METHOD FUNCTION DeleteChilds() CLASS TTable
   LOCAL result

   ::FdeletingChilds := .T.

   result := F_DeleteChilds( Self )

   ::FdeletingChilds := .F.

   RETURN result

/*
    F_DeleteChilds
*/
STATIC FUNCTION F_DeleteChilds( Self, curClass )

   LOCAL childTableName
   LOCAL ChildDB
   LOCAL clsName
   LOCAL destroyChild
   LOCAL nrec

   IF curClass = NIL
      curClass := Self
   ENDIF

   clsName := curClass:ClassName

   IF clsName == "TTABLE"
      RETURN .T.
   ENDIF

   IF hb_HHasKey( ::DataBase:ParentChildList, clsName )

      nrec := ::Alias:RecNo()

      FOR EACH childTableName IN ::DataBase:GetParentChildList( clsName )

         ChildDB := ::ChildSource( childTableName, @destroyChild )

         IF ChildDB != NIL

            IF ::DataBase:TableList[ childTableName, "IndexName" ] != NIL
               ChildDB:IndexName := ::DataBase:TableList[ childTableName, "IndexName" ]
            ENDIF

            ChildDB:StatePush()

            ChildDB:MainIndex:Scope := NIL

            WHILE .T.
               IF ChildDB:MainIndex:Descend
                  ChildDB:MainIndex:dbGoTop()
               ELSE
                  ChildDB:MainIndex:dbGoBottom()
               ENDIF
               IF ChildDB:Eof() .OR. Empty( ChildDB:BaseKeyField:Value )
                  EXIT
               ENDIF
               IF !ChildDB:TTable:Delete( .T. )
                  ChildDB:StatePull()
                  IF destroyChild
                     ChildDB:Destroy()
                  ENDIF
                  RETURN .F.
               ENDIF
            ENDDO

            ChildDB:StatePull()

            IF destroyChild
               ChildDB:Destroy()
            ENDIF

         ENDIF

      NEXT

      ::Alias:dbGoto( nrec )

   ENDIF

   RETURN F_DeleteChilds( Self, curClass:Super )

/*
    Destroy
*/
METHOD PROCEDURE Destroy() CLASS TTable

   LOCAL table

   IF HB_ISOBJECT( ::MasterSource )
      IF hb_HHasKey( ::MasterSource:DetailSourceList, ::ObjectH )
         hb_HDel( ::MasterSource:DetailSourceList, ::ObjectH )
      ENDIF
   ENDIF

   IF !HB_ISARRAY( ::FFieldList )
      // WLOG("ERROR!: " + ::ClassName + ":Destroy - :FieldList is not a array...")
      RETURN
   ENDIF

   FOR EACH table IN ::DetailSourceList
      table:Destroy()
   NEXT

   ::FFieldList := NIL
   ::FDisplayFieldList := NIL
   ::tableState := NIL

   ::FActive := .F.

   IF ::IsTempTable
      ::Alias:dbCloseArea()
      hb_dbDrop( ::TableFileName )
   ENDIF

   RETURN

/*
    Edit
*/
METHOD FUNCTION Edit( lNoRetry ) CLASS TTable

   IF !::State = dsBrowse
      ::Error_TableNotInBrowseState()
      ::FGetErrorNumber := OORDB_ERROR_EDIT_BROWSE_STATE
      RETURN .F.
   ENDIF

   IF ::Eof()
      ::FGetErrorNumber := OORDB_ERROR_EDIT_EOF
      RETURN .F.
   ENDIF
   IF !::OnBeforeEdit()
      ::FGetErrorNumber := OORDB_ERROR_ONBEFOREEDIT
      RETURN .F.
   ENDIF
   IF !::RecLock( lNoRetry )
      ::FGetErrorNumber := OORDB_ERROR_EDIT_RECLOCK
      RETURN .F.
   ENDIF

   ::FGetErrorNumber := OORDB_ERROR_NONE

   ::OnAfterEdit()

   RETURN .T.

/*
    FieldByName
*/
METHOD FUNCTION FieldByName( name, index ) CLASS TTable

   LOCAL AField

   index := 0

   IF Empty( name )
      RETURN NIL
   ENDIF

   name := Upper( name )

   FOR EACH AField IN ::FFieldList
      IF name == Upper( AField:Name ) .OR. ( AField:nameAlias != NIL .AND. name == Upper( AField:nameAlias ) )
         index := AField:__enumIndex
         RETURN AField
      ENDIF
   NEXT

   RETURN NIL

/*
    FieldByObjClass
*/
METHOD FUNCTION FieldByObjClass( objClass, derived, index ) CLASS TTable

   LOCAL fld

   objClass := Upper( objClass )

   IF derived == .T.
      FOR EACH fld IN ::FFieldList
         IF fld:FieldType = ftTable
            IF fld:LinkedTable:IsDerivedFrom( objClass )
               index := fld:__enumIndex
               RETURN fld
            ENDIF
         ENDIF
      NEXT
   ELSE
      FOR EACH fld IN ::FFieldList
         IF fld:FieldType = ftTable
            IF fld:LinkedTable:ClassName() == objClass
               index := fld:__enumIndex
               RETURN fld
            ENDIF
         ENDIF
      NEXT
   ENDIF

   RETURN NIL

/*
    FillFieldList
*/
METHOD PROCEDURE FillFieldList() CLASS TTable

   IF ::FFilledFieldList = .F.
      ::FFilledFieldList := .T.
      ::__DefineFields()
      IF Empty( ::FFieldList )
         ::DefineFieldsFromDb()
      ENDIF
   ENDIF

   RETURN

/*
    FillPrimaryIndexes
*/
METHOD FUNCTION FillPrimaryIndexes( curClass, origin ) CLASS TTable
   LOCAL filledFieldList := {=>}

   F_FillPrimaryIndexes( self, curClass, filledFieldList, origin )

RETURN filledFieldList

/*
    F_FillPrimaryIndexes
*/
STATIC PROCEDURE F_FillPrimaryIndexes( self, curClass, filledFieldList, origin )
    LOCAL className
    LOCAL field
    LOCAL iTypes := {"PRIMARY","SECONDARY"}
    LOCAL iType
    LOCAL index
    LOCAL i

    className := curClass:ClassName()

    IF !className == "TTABLE"

        F_FillPrimaryIndexes( Self, curClass:Super, filledFieldList, origin )

        FOR EACH iType IN iTypes
            IF hb_HHasKey( ::indexList, className )
                FOR EACH index IN ::indexList[ className ]
                    IF index:indexType == iType
                        field := index:MasterKeyField
                        IF field != nil
                            IF field:FieldMethodType = "A"
                                FOR EACH i IN field:fieldArrayIndex
                                    field := ::FieldList[ i ]
                                    IF ! hb_HHasKey( filledFieldList, field:name )
                                        field:reset( .T. )
                                        field:setData( , .T. )
                                        filledFieldList[ field:name ] := nil
                                    ENDIF
                                NEXT
                            ELSE
                                IF ! hb_HHasKey( filledFieldList, field:name )
                                    field:reset( .T. )
                                    field:setData( , .T. )
                                    filledFieldList[ field:name ] := nil
                                ENDIF
                            ENDIF
                        ENDIF
                        /*!
                         * AutoIncrement fields always need to be written (to set a value)
                         */
                        field := index:UniqueKeyField
                        IF field != nil
                            IF ! hb_HHasKey( filledFieldList, field:name )
                                IF field:FieldType = ftAutoInc
                                    field:GetData( .T. )
                                ELSE
                                    IF origin != nil .AND. hb_hHasKey( origin, field:name ) .AND. ! field:autoIncrement
                                        field:GetData( .T. )
                                    ELSE
                                        field:Reset( .T. )
                                        field:SetData(, .T. )
                                    ENDIF
                                ENDIF
                                filledFieldList[ field:name ] := nil
                            ENDIF
                        ENDIF
                    ENDIF
                NEXT
            ENDIF
        NEXT
    ENDIF

RETURN

/*
    FilterEval
*/
METHOD FUNCTION FilterEval( index ) CLASS TTable

   LOCAL table

   IF PCount() = 0
      index := ::GetIndex()
   ENDIF

   table := Self

   IF index != NIL .AND. index:DbFilter != NIL .AND. !index:DbFilter:Eval( table )
      RETURN .F.
   ENDIF

   RETURN table:DbFilter = NIL .OR. table:DbFilter:Eval( table )

/*
    FindIndex
*/
METHOD FUNCTION FindIndex( index ) CLASS TTable

   LOCAL AIndex

   SWITCH ValType( index )
   CASE 'U'
      AIndex := ::GetIndex()
      EXIT
   CASE 'C'
      IF Empty( index )
         AIndex := ::FPrimaryIndex
      ELSE
         AIndex := ::IndexByName( index )
      ENDIF
      EXIT
   CASE 'O'
      IF index:IsDerivedFrom( "TIndex" )
         AIndex := index
         EXIT
      ENDIF
   OTHERWISE
      RAISE ERROR "Unknown index reference..."
   ENDSWITCH

   RETURN AIndex

/*
    FindMasterSourceField
*/
METHOD FUNCTION FindMasterSourceField( detailField ) CLASS TTable

   LOCAL itm
   LOCAL name
   LOCAL vt
   LOCAL INDEX
   LOCAL masterSource

   IF ::FMasterSource == NIL
      RETURN NIL
   ENDIF

   masterSource := ::MasterSource

   vt := ValType( detailField )

   IF vt = "C"
      name := detailField
   ELSEIF vt = "O"
      name := detailField:Name
   ELSE
      RETURN NIL
   ENDIF

   IF hb_HHasKey( ::FMasterSourceFieldBuffer, name )
      index := ::FMasterSourceFieldBuffer[ name ]
      IF index > 0
         RETURN masterSource:FieldList[ index ]
      ENDIF
      RETURN NIL
   ENDIF

   IF vt = "C"
      itm := 0
      masterSource:FieldByName( name, @itm )
      ::FMasterSourceFieldBuffer[ name ] := itm
      IF !Empty( itm )
         RETURN masterSource:FieldList[ itm ]
      ENDIF
   ELSEIF vt = "O"
      IF detailField:FieldType = ftTable
         IF detailField:LinkedTable:IsDerivedFrom( masterSource:BaseKeyField:TableBaseClass )
            masterSource:FieldByName( masterSource:BaseKeyField:Name, @index )
            ::FMasterSourceFieldBuffer[ name ] := index
            RETURN masterSource:BaseKeyField
         ENDIF
         FOR EACH itm IN masterSource:FieldList
            IF itm:FieldType = ftTable .AND. detailField:LinkedTable:IsDerivedFrom( itm:BaseKeyField:TableBaseClass )
               ::FMasterSourceFieldBuffer[ name ] := itm:__enumIndex
               RETURN itm
            ENDIF
         NEXT
         ::FMasterSourceFieldBuffer[ name ] := 0
      ELSE
         RETURN ::FindMasterSourceField( detailField:Name )
      ENDIF
   ENDIF

   RETURN NIL

/*
    FixDbStruct
*/
METHOD FUNCTION FixDbStruct( aNewStruct, message ) CLASS TTable

   LOCAL fileName
   LOCAL indexName
   LOCAL tempName
   LOCAL sPath, sName, sExt, sDrv
   LOCAL sPath2, sName2, sExt2, sDrv2
   LOCAL result
   LOCAL recNo
   LOCAL errObj

   IF message = NIL
      message := ""
   ENDIF

   IF ::dataBase != NIL .AND. ::dataBase:NetIO
      SHOW WARN "Cannot run FixDbStruct on NetIO tables..."
      RETURN .F.
   ENDIF

   IF ui_AlertYesNo( message + "Proceed to update Db Structure ?" ) = 1

      fileName := ::fullFileName

      sExt := dbInfo( DBI_TABLEEXT )

      hb_FNameSplit( fileName, @sPath, @sName, NIL, @sDrv )

      recNo := ::Alias:RecNo

      indexName := ::Alias:dbOrderInfo( DBOI_FULLPATH )

      ::Alias:dbCloseArea()

      FErase( indexName )

      FClose( hb_FTempCreateEx( @tempName, sPath, "tmp", sExt ) )

      dbCreate( tempName, aNewStruct )

      USE ( tempName ) NEW

      BEGIN SEQUENCE WITH ;
            {| oErr|

         IF oErr:GenCode = EG_DATATYPE
            RETURN .F.
         ENDIF

         IF .T.
            Break( oErr )
         ENDIF

         RETURN NIL
         }

         APPEND FROM ( fileName )

         CLOSE ( Alias() )

         FRename( hb_FNameMerge( sPath, sName, sExt, sDrv ), hb_FNameMerge( sPath, "_" + sName, sExt, sDrv ) )

         FRename( hb_FNameMerge( sPath, sName, ".fpt", sDrv ), hb_FNameMerge( sPath, "_" + sName, ".fpt", sDrv ) )

         hb_FNameSplit( tempName, @sPath2, @sName2, @sExt2, @sDrv2 )

         FRename( hb_FNameMerge( sPath2, sName2, sExt2, sDrv2 ), hb_FNameMerge( sPath, sName, sExt, sDrv ) )

         FRename( hb_FNameMerge( sPath2, sName2, ".fpt", sDrv2 ), hb_FNameMerge( sPath, sName, ".fpt", sDrv ) )

         result := ::Alias:DbOpen( Self )

         ::Alias:RecNo := recNo

         hb_HDel( __S_Instances[ ::TableClass ], "DbStruct" )

      RECOVER USING errObj

         result := .F.

        SHOW ERROR errObj

      END SEQUENCE

   ELSE
      ::CancelAtFixDbStruct()
      result := .F.
   ENDIF

   RETURN result

/*
    GetAlias
*/
METHOD FUNCTION GetAlias CLASS TTable

   IF ::FisMetaTable
      ::isMetaTable := .F.
   ENDIF

   RETURN ::FAlias

/*
    GetAsString
*/
METHOD FUNCTION GetAsString() CLASS TTable

   LOCAL pkField := ::GetKeyField()

   IF pkField == NIL
      RETURN ""
   ENDIF

   RETURN pkField:AsString

/*
    GetBof
*/
METHOD FUNCTION GetBof() CLASS TTable

   IF ::FisMetaTable
      ::isMetaTable := .F.
   ENDIF

   RETURN ::FBof

/*
    GetCurrentRecord
*/
METHOD FUNCTION GetCurrentRecord() CLASS TTable
    LOCAL field
    LOCAL Result
    LOCAL table

    ::FBof   := ::Alias:Bof()
    ::FEof   := ::Alias:Eof()
    ::FFound := ::Alias:Found()

    ::FRecNo := ::Alias:RecNo

    IF ::FState = dsBrowse

        IF ( Result := ::InsideScope( .T. ) )

            FOR EACH field IN ::FFieldList
                IF field:FieldMethodType = "C" .AND. !field:Calculated // .AND. !field:IsMasterFieldComponent
                    IF !field:GetData() .AND. field:IsMasterFieldComponent
                        Result := .F.
                        EXIT
                    ENDIF
                ENDIF
                IF field:FieldType = ftTable .AND. field:Calculated .AND. field:LinkedTableAssigned
                    table := field:LinkedTable
                    IF table:LinkedObjField != NIL .AND. table:LinkedObjField:Calculated .AND. !table:MasterSource == Self .AND. table:MasterSource == table:LinkedObjField:Table:KeyField:LinkedTable
                        table:LinkedObjField:Table:KeyField:DataObj()
                    ENDIF
                ENDIF
            NEXT

        ENDIF

        IF !Result .OR. !::FilterEval()
            ::FEof := .T.
            ::FBof := .T.
            ::FFound := .F.
            ::Reset()
        ENDIF

    ELSE
        // RAISE ERROR "Table not in dsBrowse mode..."
        Result := .F.
    ENDIF

    IF ::allowOnDataChange
        IF ::LinkedObjField != NIL
            ::LinkedObjField:BaseKeyField:SetValueToLinkedObjField( ::BaseKeyField:GetAsVariant() )
        ENDIF
        ::OnDataChange()
    ENDIF

RETURN Result

/*
    GetDataBase
*/
METHOD FUNCTION GetDataBase() CLASS TTable

   IF ::FDataBaseClass = NIL
      RETURN NIL
   ENDIF

   RETURN __S_dataBase[ ::FDataBaseClass ]

/*
    GetDbStruct
*/
METHOD FUNCTION GetDbStruct CLASS TTable

   IF ! hb_HHasKey( __S_Instances[ ::TableClass ], "DbStruct" )
      __S_Instances[ ::TableClass, "DbStruct" ] := ::Alias:DbStruct
   ENDIF

   RETURN __S_Instances[ ::TableClass, "DbStruct" ]

/*
    getDefaultIndexByDefaultIndexName
*/
STATIC FUNCTION getDefaultIndexByDefaultIndexName( self, indexName )
    LOCAL curClass

    IF indexName = nil
        indexName := ::defaultIndexName
    ENDIF

    IF indexName != nil
        curClass := self
        WHILE ! curClass:className == "TTABLE"
            IF hb_HHasKey( ::indexList, curClass:className )
                IF hb_HHasKey( ::indexList[ curClass:className ], indexName )
                    RETURN ::indexList[ curClass:className ][indexName]
                ENDIF
                RETURN hb_hValueAt( ::indexList[ curClass:className ], 1 )
            ENDIF
            curClass := curClass:super
        ENDDO
    ENDIF

RETURN nil

/*
    GetDisplayFieldBlock
*/
METHOD FUNCTION GetDisplayFieldBlock( index, asDisplay ) CLASS TTable

   LOCAL field

   field := ::FFieldList[ index ]

   IF ! field:IsDerivedFrom( "TFieldTable" )
      RETURN ;
         {| o, ...|
      LOCAL AField
      LOCAL result

      AField := o:__FObj:FieldList[ index ]

      IF o:__FSyncFromAlias
         o:__FObj:SyncRecNo( .T. )
      ENDIF

      IF o:__FObj:Eof()
         IF asDisplay = .T.
            RETURN ""
         ENDIF
         RETURN AField:EmptyValue
      ENDIF

      IF !asDisplay == .T.
         result := AField:Value( ... )
      ELSE
         result := AField:AsDisplay( ... )
      ENDIF

      o:__FObj:Alias:SyncFromRecNo()

      RETURN result
      }

   ENDIF

   RETURN ;
      {| o|
         LOCAL AField

         AField := o:__FObj:FieldList[ index ]

         IF o:__FSyncFromAlias
            o:__FObj:SyncRecNo( .T. )
         ENDIF

         RETURN AField:DataObj:GetDisplayFieldList( NIL )
      }

METHOD FUNCTION GetDisplayFieldList( syncFromAlias ) CLASS TTable

   LOCAL DisplayFieldListClass
   LOCAL field
   LOCAL index
   LOCAL msgName
   LOCAL fieldList
   LOCAL itm

   IF ::FDisplayFieldList == NIL

      IF ::FisMetaTable
         ::isMetaTable := .F.
      ENDIF

      IF __S_Instances[ ::TableClass, "DisplayFieldListClass" ] == NIL

         fieldList := {=>}
         hb_HKeepOrder( fieldList, .T. )

         DisplayFieldListClass := HBClass():New( ::ClassName + "DisplayFieldList", { @TDisplayFieldList() } )

         FOR EACH itm IN ::GetPublishedFieldNameList

            msgName := itm[ 1 ]

            /* TODO: Check for a duplicate message name */
            IF !Empty( msgName ) // .AND. ! __ObjHasMsg( ef, msgName )

               field := ::FieldByName( msgName, @index )

               DisplayFieldListClass:AddInline( msgName, ::GetDisplayFieldBlock( index ) )
               fieldList[ msgName ] := index

               IF field != NIL .AND. field:hasAsDisplay
                  msgName += "_AsDisplay"
                  DisplayFieldListClass:AddInline( msgName, ::GetDisplayFieldBlock( index, .T. ) )
                  fieldList[ msgName ] := index
               ENDIF

            ENDIF

         NEXT

         DisplayFieldListClass:AddMultiClsData( , fieldList, , {"__IndexFieldList"}, .F. )
         DisplayFieldListClass:AddInline( "__FieldByIndex", {| Self,index| ::__FObj:FieldList[ index ] } )

         // Create the MasterSource field access reference
         IF ::FMasterSource != NIL
            DisplayFieldListClass:AddInline( "MasterSource", {| Self| ::__FObj:MasterSource:GetDisplayFieldList() } )
         ENDIF

         DisplayFieldListClass:Create()

         __S_Instances[ ::TableClass, "DisplayFieldListClass" ] := DisplayFieldListClass

      ENDIF

      ::FDisplayFieldList := __S_Instances[ ::TableClass, "DisplayFieldListClass" ]:instance()
      ::FDisplayFieldList:__FObj := Self
      ::FDisplayFieldList:__FSyncFromAlias := .F.

   ENDIF

   IF syncFromAlias != NIL
      ::FDisplayFieldList:__FSyncFromAlias := syncFromAlias
   ENDIF

   RETURN ::FDisplayFieldList

/*
    GetEof
*/
METHOD FUNCTION GetEof() CLASS TTable

   IF ::FisMetaTable
      ::isMetaTable := .F.
   ENDIF

   RETURN ::FEof

/*
    GetErrorString
*/
METHOD FUNCTION GetErrorString( errorNumber ) CLASS TTable

    IF ! pCount() > 0
        errorNumber := ::FGetErrorNumber
    ENDIF

    IF ! empty( errorNumber ) .AND. errorNumber > 0 .AND. errorNumber <= len( errorStringList )
        RETURN errorStringList[ errorNumber ]
    ENDIF

RETURN ""

/*
    GetFieldTypes
*/
METHOD FUNCTION GetFieldTypes CLASS TTable

   /* obtained from Harbour's src/rdd/workarea.c hb_waCreateFields */

   IF ::FFieldTypes == NIL
      ::FFieldTypes := { => }
      ::FFieldTypes[ 'C' ] := "TFieldString"  /* HB_FT_STRING */
      ::FFieldTypes[ 'L' ] := "TFieldLogical"  /* HB_FT_LOGICAL */
      ::FFieldTypes[ 'D' ] := "TFieldDate"   /* HB_FT_DATE */
      ::FFieldTypes[ 'I' ] := "TFieldInteger"  /* HB_FT_INTEGER */
      ::FFieldTypes[ 'Y' ] := "TFieldNumeric"  /* HB_FT_CURRENCY */
      ::FFieldTypes[ '2' ] := "TFieldInteger"  /* HB_FT_INTEGER */
      ::FFieldTypes[ '4' ] := "TFieldInteger"  /* HB_FT_INTEGER */
      ::FFieldTypes[ 'N' ] := "TFieldNumeric"  /* HB_FT_LONG */
      ::FFieldTypes[ 'F' ] := "TFieldNumeric"  /* HB_FT_FLOAT */
      ::FFieldTypes[ '8' ] := "TFieldFloat"   /* HB_FT_DOUBLE */
      ::FFieldTypes[ 'B' ] := "TFieldFloat"   /* HB_FT_DOUBLE */

      ::FFieldTypes[ 'T' ] := "TFieldTime"   /* HB_FT_TIME(4) */
      ::FFieldTypes[ '@' ] := "TFieldDateTime"  /* HB_FT_TIMESTAMP */
      ::FFieldTypes[ '=' ] := "TFieldModTime"  /* HB_FT_MODTIME */
      ::FFieldTypes[ '^' ] := "TFieldRowVer"  /* HB_FT_ROWVER */
      ::FFieldTypes[ '+' ] := "TFieldAutoInc"  /* HB_FT_AUTOINC */
      ::FFieldTypes[ 'Q' ] := "TFieldVarLength"  /* HB_FT_VARLENGTH */
      ::FFieldTypes[ 'V' ] := "TFieldVarLength"  /* HB_FT_VARLENGTH */
      ::FFieldTypes[ 'M' ] := "TFieldMemo"   /* HB_FT_MEMO */
      ::FFieldTypes[ 'P' ] := "TFieldImage"   /* HB_FT_IMAGE */
      ::FFieldTypes[ 'W' ] := "TFieldBlob"   /* HB_FT_BLOB */
      ::FFieldTypes[ 'G' ] := "TFieldOle"   /* HB_FT_OLE */
      ::FFieldTypes[ '0' ] := "TFieldVarLength"  /* HB_FT_VARLENGTH (NULLABLE) */
   ENDIF

   RETURN ::FFieldTypes

/*
    GetField
*/
METHOD FUNCTION GetField( fld ) CLASS TTable

   LOCAL AField

   SWITCH ValType( fld )
   CASE 'C'
      AField := ::FieldByName( fld )
      EXIT
   CASE 'O'
      AField := fld
      EXIT
   OTHERWISE
      RAISE ERROR "Unknown field reference..."
   ENDSWITCH

   RETURN AField

/*
    GetFound
*/
METHOD FUNCTION GetFound() CLASS TTable

   IF ::FisMetaTable
      ::isMetaTable := .F.
   ENDIF

   RETURN ::FFound

/*
    getFullFileName
*/
METHOD FUNCTION getFullFileName() CLASS TTable
   LOCAL path

   IF ::FfullFileName = nil
      IF ! ::isMemTable .AND. ! ::IsTempTable .AND. !empty( path := lTrim( rTrim( ::TableFileName_Path ) ) )
         IF ! right( path, 1 ) == hb_osPathSeparator()
            path += hb_osPathSeparator()
         ENDIF
      ELSE
         path := ""
      ENDIF

      ::FfullFileName := path + ::TableFileName

      IF ::isMemTable .AND. ! upper( ::FfullFileName ) = "MEM:"
         ::FfullFileName := "mem:" + ::FfullFileName
      ENDIF

   ENDIF

RETURN ::FfullFileName

/*
    GetIndex
*/
METHOD FUNCTION GetIndex() CLASS TTable

   IF ::FisMetaTable
      ::isMetaTable := .F.
   ENDIF

   RETURN ::FIndex

/*
    getInstance
*/
METHOD FUNCTION getInstance CLASS TTable
    LOCAL nPos

    nPos := hb_hPos( __S_Instances, ::TableClass )

    IF nPos = 0

        RETURN ( __S_Instances[ ::TableClass ] := hb_HSetCaseMatch( { "Initializing" => .T. }, .F. ) )

    ENDIF

RETURN hb_hValueAt( __S_Instances, nPos )

/*
    GetKeyExpression
*/
METHOD FUNCTION GetKeyExpression() CLASS TTable

   IF ::FPrimaryIndex != NIL
      RETURN ::FPrimaryIndex:KeyExpression
   ENDIF

   RETURN ""

/*
    GetKeyField
*/
METHOD FUNCTION GetKeyField() CLASS TTable

   IF ::FPrimaryIndex != NIL
      RETURN ::FPrimaryIndex:KeyField
   ENDIF

   RETURN NIL

/*
    GetKeyVal
*/
METHOD FUNCTION GetKeyVal( value ) CLASS TTable

   LOCAL fld

   fld := ::GetKeyField()
   IF fld = NIL
      RETURN NIL
   ENDIF

   RETURN fld:GetKeyVal( value )

/*
    GetMasterKeyExpression
*/
METHOD FUNCTION GetMasterKeyExpression() CLASS TTable

   IF ::FPrimaryIndex != NIL
      RETURN ::FPrimaryIndex:MasterKeyExpression
   ENDIF

   RETURN ""

/*
    GetMasterKeyField
*/
METHOD FUNCTION GetMasterKeyField() CLASS TTable

   IF ::FPrimaryIndex != NIL
      RETURN ::FPrimaryIndex:MasterKeyField
   ENDIF

   RETURN NIL

/*
    GetMasterSource
*/
METHOD FUNCTION GetMasterSource() CLASS TTable

   SWITCH ::FMasterSourceType
   CASE rxMasterSourceTypeTTable
      RETURN ::FMasterSource
   CASE rxMasterSourceTypeTField
      RETURN ::FMasterSource:LinkedTable
   CASE rxMasterSourceTypeBlock
      RETURN ::FMasterSource:Eval()
   ENDSWITCH

   RETURN NIL

/*
    GetMasterSourceClassName
*/
METHOD FUNCTION GetMasterSourceClassName() CLASS TTable

   IF ::DataBase = NIL
      ::DataBase := ::InitDataBase()
   ENDIF

RETURN ::DataBase:getMasterSourceClassName( ::className )

/*
    GetPublishedFieldNameList
*/
METHOD FUNCTION GetPublishedFieldNameList( typeList ) CLASS TTable

   LOCAL result := {}
   LOCAL AField
   LOCAL itm

   FOR EACH AField IN ::FFieldList
      IF !Empty( AField:Name )
         IF Empty( typeList )
            IF AField:Published
               AAdd( result, { AField:Name, AField } )
            ENDIF
            IF !Empty( AField:nameAlias ) .AND. AField:nameAliasPublished
               AAdd( result, { AField:nameAlias, AField } )
            ENDIF
         ELSE
            FOR EACH itm IN typeList
               IF AField:IsDerivedFrom( itm )
                  IF AField:Published
                     AAdd( result, { AField:Name, AField } )
                  ENDIF
                  IF !Empty( AField:nameAlias ) .AND. AField:nameAliasPublished
                     AAdd( result, { AField:nameAlias, AField } )
                  ENDIF
               ENDIF
            NEXT
         ENDIF
      ENDIF
   NEXT

   ASort( result,,, {| x, y| iif( x[ 2 ]:FieldType = ftTable, "1", "0" ) + x[ 1 ] < iif( y[ 2 ]:FieldType = ftTable, "1", "0" ) + y[ 1 ] } )

   RETURN result

/*
    GetRecNo
*/
METHOD FUNCTION GetRecNo() CLASS TTable

   IF ::FisMetaTable
      ::isMetaTable := .F.
   ENDIF

   RETURN ::FRecNo

/*
    GetRecordList
*/
METHOD FUNCTION GetRecordList() CLASS TTable
    IF ::FRecordList = NIL
        ::FRecordList := TRecordList():New( Self )
    ENDIF
RETURN ::FRecordList

/*
    GetTableFileName
*/
METHOD FUNCTION GetTableFileName() CLASS TTable

   IF ::FisMemTable
      IF empty( ::FTableFileName )
         ::FTableFileName := "mem:__mem__" + hb_numToHex( ++ FmemTempFileCount )
         ::FIsTempTable := .T.
      ELSE
         IF ! upper( ::FTableFileName ) = "MEM:"
            ::FTableFileName := "mem:" + ::FTableFileName
         ENDIF
      ENDIF
   ELSE
      IF Empty( ::FTableFileName )
         IF ::AutoCreate
            FClose( hb_FTempCreateEx( @::FTableFileName, NIL, "t", ".dbf" ) )
            ::FIsTempTable := .T.
         ENDIF
      ENDIF
   ENDIF

   RETURN ::FTableFileName

/*
    __GetValue
*/
METHOD FUNCTION __GetValue CLASS TTable
   RETURN ::FBaseKeyField:Value

/*
    ImportField
*/
METHOD FUNCTION ImportField( fromField, fieldDbName, fieldName ) CLASS TTable

   LOCAL fld

   IF Empty( fieldDbName )
      fieldDbName := fromField:FieldMethod
   ELSEIF Empty( fieldName )
      fieldName := fieldDbName
   ENDIF

   IF Empty( fieldName )
      fieldName := fromField:Name
   ENDIF

   fld :=  __clsInst( fromField:ClassH() )

   fld:New( Self )

   fld:Name := fieldName
   fld:FieldMethod := fieldDbName

   IF fld:IsDerivedFrom( "TFieldTable" )
      fld:ObjClass := fromField:ObjClass
   ENDIF

   AAdd( ::FFieldList, fld )

   RETURN fld

/*
    IndexByName
*/
METHOD FUNCTION IndexByName( indexName, aPos, curClass ) CLASS TTable

   IF ::FisMetaTable
      ::isMetaTable := .F.
   ENDIF

   RETURN F_IndexByName( Self, indexName, @aPos, curClass )

/*
    F_IndexByName
*/
STATIC FUNCTION F_IndexByName( Self, indexName, aPos, curClass )
   LOCAL className
   LOCAL x,y

   curClass := iif( curClass = NIL, Self, curClass )
   className := curClass:ClassName()

   IF ! className == "TTABLE"
      IF hb_HHasKey( ::IndexList, className, @x )
         IF hb_HHasKey( hb_hValueAt( ::IndexList, x ), indexName, @y )
            aPos := { x, y }
            RETURN hb_hValueAt( hb_hValueAt( ::indexList , x ), y )
         ENDIF
      ENDIF
      RETURN F_IndexByName( Self, indexName, @aPos, curClass:Super )
   ENDIF

   RETURN NIL

/*
    InitTable
*/
METHOD PROCEDURE InitTable() CLASS TTable
    LOCAL instance

    instance := ::getInstance()

    /*!
    * Make sure that database is open here
    */
    IF ::FAlias == NIL
        ::FAlias := TAlias():New( Self )
    ENDIF

    IF instance[ "Initializing" ]

        ::OnClassInitializing()

        instance[ "ChildReferenceList" ] := {}

        ::DefineRelations()

        instance[ "DisplayFieldListClass" ] := NIL

        instance[ "Initializing" ] := .F.

    ENDIF

RETURN

/*
    Insert
*/
METHOD FUNCTION Insert( origin ) CLASS TTable
   LOCAL result := .F.
   LOCAL itm

   IF ::FisMetaTable
      ::isMetaTable := .F.
   ENDIF

   IF !::State = dsBrowse
      ::Error_TableNotInBrowseState()
      RETURN .F.
   ENDIF

   /* checks for a serialized value (array of records / hash of record) */
   IF hb_isChar( origin )
      origin := hb_deSerialize( origin )
      IF ! hb_isArray( origin ) .AND. ! hb_isHash( origin )
         origin := nil
      ENDIF
   ENDIF

   IF hb_isArray( origin )
      result := .T.
      FOR EACH itm IN origin
         IF ::onBeforeInsert()
            result := ::addRec( itm )
            IF result
               ::alias:dbSkip( 0 )
               ::onAfterInsert()
               result := ::post()
            ENDIF
            IF ! result
               EXIT
            ENDIF
         ENDIF
      NEXT
   ELSE
      IF ::OnBeforeInsert() .AND. ::AddRec( origin )

         /* To Flush !!! */
         ::Alias:dbSkip( 0 )

         ::OnAfterInsert()

         result := .T.

      ENDIF
   ENDIF

RETURN result

/*
    InsideScope
*/
METHOD FUNCTION InsideScope( ignoreFilters ) CLASS TTable

   IF ::Eof() .OR. ( ::MasterSource != NIL .AND. ::MasterSource:Eof() )
      RETURN .F.
   ENDIF

   RETURN ::GetIndex() = NIL .OR. ::GetIndex():InsideScope( ignoreFilters )

/*
    OnActiveSetKeyVal
*/
METHOD FUNCTION OnActiveSetKeyVal( value ) CLASS TTable

   IF value == NIL
      RETURN ::FOnActiveSetKeyVal
   ENDIF
   ::FOnActiveSetKeyVal := value

   RETURN value

/*
    OnDataChange
*/
METHOD PROCEDURE OnDataChange() CLASS TTable

   IF ::OnDataChangeBlock != NIL
      ::OnDataChangeBlock:Eval( iif( ::OnDataChangeBlock_Param = NIL, Self, ::OnDataChangeBlock_Param ) )
   ENDIF

   RETURN

/*
    Open
*/
METHOD FUNCTION Open() CLASS TTable

   LOCAL masterSource := ::GetMasterSourceClassName()

   IF ::MasterSource == NIL .AND. !Empty( masterSource )

      RAISE ERROR "Table '" + ::ClassName() + "' needs a MasterSource of type '" + masterSource  + "'..."

   ENDIF

   ::FActive := .T.

   ::SetState( dsBrowse )

    /*
     * Try to sync with MasterSource (if any)
     */
   ::SyncFromMasterSourceFields()

   ::allowOnDataChange := .T.

   IF ::Alias != NIL
      ::FHasDeletedOrder := ::Alias:ordNumber( "__AVAIL" ) > 0
   ENDIF

   ::OnDataChange()

   ::OnAfterOpen()

   RETURN .T.

/*
    Post
*/
METHOD FUNCTION Post() CLASS TTable

   LOCAL AField
   LOCAL errObj
   LOCAL postOk := .F.
   LOCAL changedFieldList := {}
   LOCAL changed := .F.
   LOCAL result

   IF AScan( { dsEdit, dsInsert }, ::State ) = 0
      ::Error_Table_Not_In_Edit_or_Insert_mode()
   ENDIF

   BEGIN SEQUENCE WITH ::ErrorBlock

      ::FSubState := dssPosting

      IF ::OnBeforePost()

         FOR EACH AField IN ::FieldList

            IF AField:Enabled .AND. !AField:Calculated
               IF AField:Changed
                  IF AField:OnAfterPostChange != NIL
                     AAdd( changedFieldList, AField )
                  ENDIF
                  changed := .T.
               ENDIF
               result := AField:ValidateResult()
               IF result != NIL
                  RAISE ERROR "Post: Invalid data on Field: " + result
               ENDIF
            ENDIF

         NEXT

         ::RecUnLock()

         postOk := .T.

      ENDIF

   RECOVER USING errObj

      ::Cancel()

      SHOW ERROR errObj

   ALWAYS

      ::FSubState := dssNone

   END SEQUENCE

   IF postOk
      ::OnAfterPost( changedFieldList )
      IF Len( changedFieldList ) > 0
         FOR EACH AField IN changedFieldList
            AField:OnAfterPostChange:Eval( Self )
         NEXT
      ENDIF
      IF ::FpreviousEditState = dsEdit .AND. changed
         ::OnAfterPostEdit()
      ENDIF
      IF ::FpreviousEditState = dsInsert
         ::OnAfterPostInsert()
      ENDIF
   ENDIF

   ::FpreviousEditState := NIL

   RETURN postOk

/*
    Process_TableName
*/
METHOD PROCEDURE Process_TableName( tableName ) CLASS TTable

   IF tableName == NIL
      tableName := ::TableFileName
   ELSE
      ::FTableFileName := tableName
   ENDIF

RETURN

/*
    RawGet4Seek
*/
METHOD FUNCTION RawGet4Seek( direction, xField, keyVal, index, softSeek ) CLASS TTable

   LOCAL AIndex := ::FindIndex( index )

   RETURN AIndex:RawGet4Seek( direction, ::GetField( xField ):FieldReadBlock, keyVal, softSeek )

/*
    RawSeek
*/
METHOD FUNCTION RawSeek( Value, index ) CLASS TTable
   RETURN ::FindIndex( index ):RawSeek( Value )

/*
    RecLock
*/
METHOD FUNCTION RecLock( lNoRetry ) CLASS TTable

   LOCAL allowOnDataChange
   LOCAL result

   IF ::FState != dsBrowse
      ::Error_Table_Not_In_Browse_Mode()
      RETURN .F.
   ENDIF

   IF !::OnBeforeLock()
      RETURN .F.
   ENDIF

   IF ::FReadOnly
      SHOW WARN "Table is marked as READONLY..."
      RETURN .F.
   ENDIF

   IF ::Eof()
      RAISE ERROR "Attempt to lock record at EOF..."
   ENDIF

   IF !::InsideScope() .OR. !::Alias:RecLock( nil, lNoRetry )
      RETURN .F.
   ENDIF

   allowOnDataChange := ::allowOnDataChange
   ::allowOnDataChange := .F.

   result := ::GetCurrentRecord()

   IF result
      ::SetState( dsEdit )
      ::FpreviousEditState := dsEdit
   ELSE
      ::Alias:RecUnLock()
   ENDIF

   ::allowOnDataChange := allowOnDataChange

   RETURN result

/*
    recordValueList
*/
METHOD FUNCTION recordValueList( origin ) CLASS TTable
    LOCAL field
    LOCAL h := {=>}

    IF origin = nil
        origin := self
    ENDIF

    SWITCH valType( origin )
    CASE "O"
        IF origin:isDerivedFrom( "TTable" ) .AND. ! origin:eof()
            FOR EACH field IN origin:fieldList
                IF !field:calculated .AND. field:fieldMethodType = "C"
                    h[ field:name ] := field:value
                ENDIF
            NEXT
        ENDIF
        EXIT
    CASE "H"
        h := origin
        EXIT
    ENDSWITCH

RETURN h

/*
    RecUnLock
*/
METHOD FUNCTION RecUnLock() CLASS TTable

   LOCAL Result

   IF ( Result := ::Alias:RecUnLock() )
      ::SetState( dsBrowse )
      ::OnDataChange()
   ENDIF

   ::UpdateCustomIndexes()

   RETURN Result

/*
    Refresh
*/
METHOD PROCEDURE Refresh CLASS TTable

   IF ::FRecNo = ::Alias:RecNo
      RETURN
   ENDIF

   ::GetCurrentRecord()

   RETURN

/*
    Reset
*/
METHOD PROCEDURE Reset() CLASS TTable

   LOCAL AField

   ::FUnderReset := .T.

   FOR EACH AField IN ::FFieldList

      IF !AField:Calculated .AND. AField:FieldMethodType = "C" .AND. AField:Enabled
         AField:Reset()
      ENDIF

   NEXT

   ::FUnderReset := .F.

   RETURN

/*
    SetBaseKeyIndex
*/
METHOD FUNCTION SetBaseKeyIndex( baseKeyIndex ) CLASS TTable

   LOCAL className
   LOCAL tableBaseClass
   LOCAL baseKeyField

   baseKeyField := baseKeyIndex:KeyField

   ::FBaseKeyIndex := baseKeyIndex
   ::FBaseKeyField := baseKeyField

   className := ::ClassName
   tableBaseClass := baseKeyField:TableBaseClass

   IF AScan( BaseKeyFieldList, {| e| Upper( e[ 1 ] ) == Upper( className ) } ) = 0
      AAdd( BaseKeyFieldList, { className, baseKeyField:DBS_TYPE, baseKeyField:DBS_LEN, baseKeyField:DBS_DEC, baseKeyField:size, baseKeyField:emptyValue } )
   ENDIF

   IF !Upper( className ) == Upper( tableBaseClass ) .AND. AScan( BaseKeyFieldList, {| e| Upper( e[ 1 ] ) == Upper( tableBaseClass ) } ) = 0
      AAdd( BaseKeyFieldList, { tableBaseClass, baseKeyField:DBS_TYPE, baseKeyField:DBS_LEN, baseKeyField:DBS_DEC, baseKeyField:size, baseKeyField:emptyValue } )
   ENDIF

   RETURN baseKeyIndex

/*
    SetDataBase
*/
METHOD FUNCTION SetDataBase( dataBase ) CLASS TTable

   IF dataBase = NIL
      ::FDataBaseClass := NIL
   ELSE
      ::FDataBaseClass := dataBase:ClassName
      IF !hb_HHasKey( __S_dataBase, ::FDataBaseClass )
         __S_dataBase[ dataBase:ClassName ] := dataBase
      ENDIF
   ENDIF

   RETURN dataBase

/*
    SetIndex
*/
METHOD FUNCTION SetIndex( index ) CLASS TTable

    IF !Empty( index ) .AND. !::GetIndex() == index .AND. ::onBeforeChangeIndex( index )
        ::FIndex := index
        ::onAfterChangeIndex()
    ENDIF

RETURN ::FIndex

/*
    SetIndexName
*/
METHOD FUNCTION SetIndexName( indexName ) CLASS TTable

   LOCAL INDEX

   IF ! Upper( indexName ) == Upper( ::IndexName )

      index := ::IndexByName( indexName )

      IF index != NIL
         ::SetIndex( index )
         RETURN ::GetIndexName
      ENDIF

      RAISE ERROR  "<" + ::ClassName + ">: Index name '" + indexName + "' doesn't exist..."

   ENDIF

   RETURN ::GetIndexName

/*
    SetisMetaTable
*/
METHOD FUNCTION SetisMetaTable( isMetaTable ) CLASS TTable

   IF ::FisMetaTable .AND. !isMetaTable

      IF ::FcanCreateInstance
         ::CreateTableInstance()
      ENDIF

   ENDIF

   RETURN isMetaTable

/*
    SetKeyVal
*/
METHOD FUNCTION SetKeyVal( keyVal ) CLASS TTable

   RETURN ::GetKeyField():SetKeyVal( keyVal )

/*
    SetMasterSource
*/
METHOD FUNCTION SetMasterSource( masterSource ) CLASS TTable

   IF ::FMasterSource == masterSource
      RETURN ::FMasterSource
   ENDIF

   ::FMasterSource := masterSource

   SWITCH ValType( masterSource )
   CASE 'O'
      IF masterSource:IsDerivedFrom( "TTable" )
         ::FMasterSourceType := rxMasterSourceTypeTTable
      ELSEIF masterSource:IsDerivedFrom( "TFieldTable" )
         ::FMasterSourceType := rxMasterSourceTypeTField
      ELSEIF masterSource:IsDerivedFrom( "TField" )
         RAISE ERROR "need to specify TField generic syncing..."
      ELSE
         RAISE ERROR "Invalid object in assigning MasterSource..."
      ENDIF
      EXIT
   CASE 'B'
      ::FMasterSourceType := rxMasterSourceTypeBlock
      EXIT
   CASE 'U'
      ::FMasterSourceType := rxMasterSourceTypeNone
      RETURN masterSource
   OTHERWISE
      RAISE ERROR "Invalid type in assigning MasterSource..."
   ENDSWITCH

    /*!
     * Check for a valid GetMasterSourceClassName (if any)
     */
   IF !Empty( ::GetMasterSourceClassName() )
      // IF !::MasterSource:IsDerivedFrom( ::GetMasterSourceClassName ) .AND. !::DataBase:TableIsChildOf( ::GetMasterSourceClassName, ::MasterSource:ClassName )
      // IF ! Upper( ::GetMasterSourceClassName ) == ::MasterSource:ClassName
      IF ! ::MasterSource:IsDerivedFrom( ::GetMasterSourceClassName() )
         RAISE ERROR "Table <" + ::TableClass + "> Invalid MasterSource Class Name: " + ::MasterSource:ClassName + ";Expected class type: <" + ::GetMasterSourceClassName() + ">"
      ENDIF
   ELSE
      RAISE ERROR "Table '" + ::ClassName() + "' has not declared the MasterSource '" + ::MasterSource:ClassName() + "' in the DataBase structure..."
   ENDIF

    /*
     * Check if another Self is already in the MasterSource DetailSourceList
     * and RAISE ERROR if another Self is trying to break the previous link
     */
   IF hb_HHasKey( ::MasterSource:DetailSourceList, Self:ObjectH )
      RAISE ERROR "Cannot re-assign DetailSourceList:<" + ::ClassName + ">"
   ENDIF

   ::MasterSource:DetailSourceList[ Self:ObjectH ] := Self

   ::SyncFromMasterSourceFields()

   RETURN masterSource

/*
    SetPrimaryIndex
*/
METHOD FUNCTION SetPrimaryIndex( primaryIndex ) CLASS TTable

   ::FPrimaryIndex := primaryIndex

   RETURN primaryIndex

/*
    SetPrimaryIndexList
*/
METHOD PROCEDURE SetPrimaryIndexList( clsName, name ) CLASS TTable

   ::FPrimaryIndexList[ clsName ] := name

   RETURN

/*
    SetReadOnly
*/
METHOD FUNCTION SetReadOnly( readOnly ) CLASS TTable

   IF ! HB_ISLOGICAL( readOnly )
      RAISE ERROR "Invalid value on SetReadOnly..."
   ENDIF
   IF ::FState = dsBrowse
      ::FReadOnly := readOnly
   ENDIF

   RETURN readOnly

/*
    SetState
*/
METHOD FUNCTION SetState( state ) CLASS TTable

   LOCAL oldState

   IF !::FState == state
      oldState := ::FState
      ::FState := state
      IF state = dsEdit .OR. state = dsInsert
         ::FUndoList := hb_HSetCaseMatch( { => }, .F. )
      ENDIF
      ::OnStateChange( oldState )
   ENDIF

   RETURN state

/*
    __SetValue
*/
METHOD FUNCTION __SetValue( value ) CLASS TTable

   ::FBaseKeyField:Value := value

   RETURN ::FBaseKeyField:Value

/*
    SkipBrowse : BROWSE skipblock
*/
METHOD FUNCTION SkipBrowse( n, lSkipUnique ) CLASS TTable

   LOCAL num_skipped := 0
   LOCAL recNo

   IF n = 0
      ::dbSkip( 0 )
      RETURN 0
   ENDIF

   IF n > 0
      WHILE !::Eof() .AND. num_skipped < n
         recNo := ::RecNo
         IF !::dbSkip( 1, lSkipUnique ) .OR. ::Eof()
            ::dbGoto( recNo )
            EXIT
         ENDIF
         num_skipped++
      ENDDO
   ELSE
      WHILE !::Bof() .AND. num_skipped > n
         recNo := ::RecNo
         IF !::dbSkip( -1, lSkipUnique ) .OR. ::Bof()
            ::dbGoto( recNo )
            EXIT
         ENDIF
         num_skipped--
      ENDDO
   ENDIF

   RETURN num_skipped

/*
    SkipFilter
*/
METHOD FUNCTION SkipFilter( n, index ) CLASS TTable

   LOCAL i
   LOCAL tagName
   LOCAL o
   LOCAL ALIAS

   IF n = NIL
      n := 1
   ENDIF

   IF n > 0
      i := 1
   ELSEIF n < 0
      i := -1
   ELSE
      i := 0
   ENDIF

   n := Abs( n )

   IF index = NIL
      tagName := NIL
      o := Self
      alias := ::Alias
   ELSE
      tagName := index:TagName
      o := index
      alias := index:table:alias
   ENDIF

   WHILE .T.
      o:DbFilterPush()
      IF !alias:dbSkip( i, tagName ) .OR. ! o:GetCurrentRecord()
         o:DbFilterPull()
         ::dbGoto( 0 )
         RETURN .F.
      ENDIF
      o:DbFilterPull()
      IF ::FilterEval( index )
         --n
      ENDIF
      IF n <= 0
         EXIT
      ENDIF
   ENDDO

   RETURN .T.

/*
    StatePull
*/
METHOD PROCEDURE StatePull() CLASS TTable

   LOCAL cloneData
   LOCAL tbl
   LOCAL hData

   hData := ::tableState[ ::tableStateLen ]

   IF ! empty( hData )

      IF AScan( { dsInsert, dsEdit }, ::State ) > 0
         ::Cancel()
      ENDIF

      FOR EACH cloneData IN hData[ "CloneData" ]
         ::FFieldList[ cloneData:__enumIndex ]:CloneData := cloneData
      NEXT

      ::FRecNo           := hData[ "RecNo" ]
      ::FBof             := hData[ "Bof" ]
      ::FEof             := hData[ "Eof" ]
      ::FFound           := hData[ "Found" ]
      ::FState           := hData[ "State" ]
      ::FpreviousEditState  := hData[ "previousEditState" ]
      IF !Empty( hData[ "IndexName" ] )
         ::IndexName        := hData[ "IndexName" ]
      ENDIF

      FOR EACH tbl IN ::DetailSourceList
         IF hb_HHasKey( hData[ "DetailSourceList" ], tbl:ObjectH )
            tbl:StatePull()
         ENDIF
      NEXT

      ::FUndoList := hData[ "UndoList" ]
      ::FOnActiveSetKeyVal := hData[ "OnActiveSetKeyVal" ]
      ::LinkedObjField := hData[ "LinkedObjField" ]

      ::Alias:Pop()

   ENDIF

   --::tableStateLen

   RETURN

/*
    StatePush
*/
METHOD PROCEDURE StatePush() CLASS TTable

   LOCAL fld
   LOCAL aCloneData := {}
   LOCAL hDSL := { => }
   LOCAL tbl
   LOCAL hData

   IF Len( ::tableState ) < ++::tableStateLen
      AAdd( ::tableState, { => } )
   ENDIF

   IF ! ::FisMetaTable

      hData := ::tableState[ ::tableStateLen ]

      FOR EACH fld IN ::FFieldList
         AAdd( aCloneData, fld:CloneData )
      NEXT

      hData[ "CloneData" ]           := aCloneData
      hData[ "RecNo" ]               := ::FRecNo
      hData[ "Bof" ]                 := ::FBof
      hData[ "Eof" ]                 := ::FEof
      hData[ "Found" ]               := ::FFound
      hData[ "State" ]               := ::FState
      hData[ "previousEditState" ]   := ::FpreviousEditState
      hData[ "IndexName" ]           := ::IndexName
      hData[ "DetailSourceList" ]    := hDSL
      hData[ "UndoList" ]            := ::FUndoList
      hData[ "OnActiveSetKeyVal" ]   := ::FOnActiveSetKeyVal

      /* unlinks possible linked field to avoid possible changes in linked table */
      hData[ "LinkedObjField" ]   := ::LinkedObjField
      ::LinkedObjField := nil

      FOR EACH tbl IN ::DetailSourceList
         hDSL[ tbl:ObjectH ] := NIL
         tbl:StatePush()
      NEXT

      ::FState := dsBrowse
      ::FpreviousEditState := NIL
      ::FUndoList := NIL
      ::FOnActiveSetKeyVal := .F.

      ::Alias:Push()

   ENDIF

   RETURN

/*
    SyncFromMasterSourceFields
*/
METHOD PROCEDURE SyncFromMasterSourceFields() CLASS TTable

   IF ::FActive

      IF ::MasterSource != NIL

         IF ::MasterSource:Active

            ::OnSyncFromMasterSource()

            IF !::MasterSource:Eof() .AND. ::Alias != NIL

               IF ::InsideScope()
                  ::GetCurrentRecord()
               ELSE
                  ::dbGoTop()
               ENDIF

            ELSE

               ::FEof := .T.
               ::FBof := .T.
               ::FFound := .F.

               ::Reset()

               IF ::allowOnDataChange
                  ::OnDataChange()
               ENDIF

            ENDIF

         ENDIF

      ELSE

         ::GetCurrentRecord()

      ENDIF

   ENDIF

   RETURN

/*
    SyncRecNo
*/
METHOD PROCEDURE SyncRecNo( fromAlias ) CLASS TTable

   IF fromAlias == .T.
      ::Alias:SyncFromAlias()
   ELSE
      ::Alias:SyncFromRecNo()
   ENDIF
   IF ::FRecNo = ::Alias:RecNo
      RETURN
   ENDIF
   ::GetCurrentRecord()

   RETURN

/*
   tableValueList
*/
METHOD FUNCTION tableValueList( index ) CLASS TTable
   LOCAL tableList := {}

   ::statePush()

   IF index = nil
      index := ::index
   ENDIF

   IF index:dbGoTop()
      WHILE ! index:eof()
         aAdd( tableList, ::recordValueList() )
         index:dbSkip()
      ENDDO
   ENDIF

   ::statePull()

RETURN tableList

/*
    UpdateCustomIndexes
*/
METHOD PROCEDURE UpdateCustomIndexes() CLASS TTable

   LOCAL INDEX

   FOR EACH INDEX IN ::FCustomIndexList
      index:CustomKeyUpdate()
   NEXT

   RETURN

/*
    Validate
*/
METHOD FUNCTION Validate( showAlert ) CLASS TTable

   LOCAL AField

   FOR EACH AField IN ::FFieldList
      IF AField:Enabled .AND. ! AField:Validate( showAlert )
         RETURN .F.
      ENDIF
   NEXT

   RETURN .T.

/*
    End Class TTable
*/

STATIC FUNCTION findParentClass( self )
    LOCAL n
    LOCAL clsName

    clsName := ::className

    n := aScan( BaseKeyFieldList, {|e| upper( e[ 1 ] ) == clsName } )

    IF n = 0 .AND. ! clsName == "TTABLE"
        findParentClass( ::super )
    ENDIF

RETURN n

/*
    FindTableBaseClass
*/
STATIC FUNCTION FindTableBaseClass( AField )

   LOCAL n
   LOCAL t
   LOCAL clsName

   clsName := Upper( AField:ObjClass )

   n := AScan( BaseKeyFieldList, {| e| Upper( e[ 1 ] ) == clsName } )

   IF n = 0
      t := AField:LinkedTable
      n := findParentClass( t )
   ENDIF

   RETURN n
