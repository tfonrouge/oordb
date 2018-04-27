/*
 *
 */

#include "oordb.ch"
#include "xerror.ch"

/*
    TFieldTable
*/
CLASS TFieldTable FROM TField

PROTECTED:

   DATA buildingLinkedTable
   DATA FCalcMethod
   DATA FcalculatingLinkedTable INIT .F.
   DATA FClassInit
   DATA FFieldType INIT ftTable
   DATA FlinkedTable          /* holds the Table object */
   DATA FlinkedTableMasterSource
   DATA FObjClass
   DATA FonDataChangeBlock
   DATA FonDataObj
   DATA FType INIT "TableField"
   DATA FtypeNameList INIT HB_HSetCaseMatch( {"es"=>"Documento"} )
   DATA FValidValuesLabelField
   DATA FValType INIT "O"
   METHOD GetDBS_LEN INLINE ::BaseKeyField():DBS_LEN
   METHOD GetDBS_TYPE INLINE iif( ::BaseKeyField():DBS_TYPE = "+", "I", ::BaseKeyField():DBS_TYPE )
   METHOD GetLabel()
   METHOD GetLinkedTable( ... )
   METHOD GetEmptyValue()
   METHOD GetFieldReadBlock()
   METHOD GetOnDataChange()
   METHOD GetValidValues()
   METHOD SetLinkedTableMasterSource( linkedTable )
   METHOD SetObjClass( objClass ) INLINE ::FObjClass := objClass
   METHOD SetOnDataChange( onDataChangeBlock )

PUBLIC:

   METHOD BaseKeyField() // Returns the non-TFieldTable associated to this obj
   METHOD BuildLinkedTable()
   METHOD DataObj( ... )
   METHOD GetKeyVal( keyVal, ... )
   METHOD GetAsString(value)       // INLINE ::LinkedTable:KeyField:AsString()
   METHOD GetAsVariant( ... )
   METHOD hasLinkedTable() INLINE ::FlinkedTable != nil
   METHOD IndexExpression( fieldName )
   METHOD SetClassInit( clsInit ) INLINE ::FClassInit := clsInit
   METHOD SetValidValues( validValues, ignoreUndetermined, labelField )
   PROPERTY KeySize READ BaseKeyField():KeySize
   PROPERTY LinkedTable READ GetLinkedTable
   PROPERTY LinkedTableAssigned READ FlinkedTableMasterSource != NIL
   PROPERTY LinkedTableMasterSource READ FlinkedTableMasterSource WRITE SetLinkedTableMasterSource
   PROPERTY MasterKeyVal
   PROPERTY ObjClass READ FObjClass WRITE SetObjClass
   PROPERTY OnDataChange READ GetOnDataChange WRITE SetOnDataChange
   PROPERTY Size READ BaseKeyField():Size
   PROPERTY ValidValuesLabelField READ FValidValuesLabelField

ENDCLASS

/*
    BaseKeyField
*/
METHOD FUNCTION BaseKeyField() CLASS TFieldTable

   LOCAL baseKeyField

   IF ::FlinkedTable != NIL .OR. !Empty( ::GetLinkedTable() )
      baseKeyField := ::FlinkedTable:BaseKeyField
      IF baseKeyField = NIL
         ::No_BaseKeyField_Defined()
      ENDIF
   ENDIF

   RETURN baseKeyField

/*
    BuildLinkedTable
*/
METHOD FUNCTION BuildLinkedTable() CLASS TFieldTable

   LOCAL masterSource
   LOCAL className
   LOCAL fld
   LOCAL classInit

   IF ::FlinkedTable = NIL .AND. ::buildingLinkedTable = NIL

      ::buildingLinkedTable := .T.

      IF Empty( ::FObjClass )
         RAISE TFIELD ::Name ERROR "TFieldTable has not a ObjClass value."
      ENDIF

        /*
         * Solve using the default ObjClass
         */
      IF ::table:MasterSource != NIL .AND. ::table:MasterSource:IsDerivedFrom( ::FObjClass ) .AND. ::IsMasterFieldComponent
         ::FlinkedTable := ::table:MasterSource
      ELSE
         IF ::FlinkedTableMasterSource != NIL
            IF ValType( ::FlinkedTableMasterSource ) = "B"
                masterSource := ::FlinkedTableMasterSource
            ELSE
                masterSource := ::FlinkedTableMasterSource
            ENDIF
         ENDIF

         ::FlinkedTable := __ClsInstFromName( ::FObjClass )

         IF !::FlinkedTable:IsDerivedFrom( "TBaseTable" )
            RAISE TFIELD ::Name ERROR "Denied: To create TFieldTable's linked table NOT derived from a TBaseTable class."
         ENDIF

         /* check if we still need a mastersource and it exists in TFieldTable's Table */
         IF Empty( masterSource )
            className := ::FlinkedTable:GetMasterSourceClassName()
            IF ::table:IsDerivedFrom( className )
               masterSource := ::table
            ELSEIF !Empty( className ) .AND. ! Empty( fld := ::table:FieldByObjClass( className, .T. ) )
               masterSource := fld
            ENDIF
         ENDIF
         ::FlinkedTable:New( masterSource )
         classInit := ::FClassInit
      ENDIF

      IF !HB_ISOBJECT( ::FlinkedTable ) .OR. ! ::FlinkedTable:IsDerivedFrom( "TBaseTable" )
         RAISE TFIELD ::Name ERROR "Default value is not a TBaseTable object."
      ENDIF

        /*
         * Attach the current DataObj to the one in table to sync when table changes
         * MasterFieldComponents are ignored, a child cannot change his parent :)
         */
      IF ::FlinkedTable:LinkedObjField == NIL
            /*
             * LinkedObjField is linked to the FIRST TFieldTable were it is referenced
             * this has to be the most top level MasterSource table
             */
         ::FlinkedTable:LinkedObjField := Self
      ELSE
            /*
             * We need to set this field as READONLY, because their LinkedTable
             * belongs to a some TFieldTable in some MasterSource table
             * so this TFieldTable cannot modify the physical database here
             */
         // ::ReadOnly := .T.
      ENDIF

      IF classInit != NIL
         classInit:Eval( ::FlinkedTable )
      ENDIF

      ::buildingLinkedTable := NIL

   ENDIF

RETURN ::FLinkedTable

/*
    DataObj
    Syncs the Table with the key in buffer
*/
METHOD FUNCTION DataObj( ... ) CLASS TFieldTable

   LOCAL linkedTable
   LOCAL linkedObjField
   LOCAL keyVal

   linkedTable := ::GetLinkedTable( ... )

   IF ::FonDataObj = NIL

      ::FonDataObj := .T.

      IF linkedTable != NIL

         IF linkedTable:isMetaTable
            linkedTable:isMetaTable := .F.
         ENDIF

         IF ::FonDataChangeBlock != NIL
            linkedTable:OnDataChangeBlock := ::FonDataChangeBlock
            linkedTable:OnDataChangeBlock_Param := ::Table
            ::FonDataChangeBlock := NIL
         ENDIF

         IF linkedTable:State = dsBrowse
            IF ::IsMasterFieldComponent .AND. ::table:FUnderReset

            ELSE
                /*
                    to sure a resync with linkedTable mastersource table
                    on TFieldTable's that have a mastersource field (another TFieldTable)
                    in the same table
                */
               IF !Empty( linkedTable:MasterSource ) .AND. !Empty( linkedTable:MasterSource:LinkedObjField ) .AND. linkedTable:MasterSource:LinkedObjField:Table == ::table
                  linkedTable:MasterSource:LinkedObjField:DataObj()
               ENDIF
               /* to be sure of mastersource synced with linkedTable */
               /* don't try to move linkedTable if it is the current ::table:masterSource */
               IF ( ::table:masterSource = nil .OR. ! ::table:masterSource == linkedTable ) .AND. linkedTable:MasterSource != NIL .AND. !linkedTable:MasterSource:BaseKeyField:KeyVal == ::FMasterKeyVal
                  IF ! linkedTable:InsideScope()
                     /* to don't attempt to write into a LinkedObjField */
                     linkedObjField := linkedTable:LinkedObjField
                     linkedTable:LinkedObjField := NIL
                     IF ::table:eof() .OR. ! linkedTable:index:opened
                        linkedTable:dbGoTo( 0 )
                     ELSE
                        linkedTable:dbGoTop()
                     ENDIF
                     linkedTable:LinkedObjField := linkedObjField
                  ENDIF
                  ::FMasterKeyVal := linkedTable:MasterSource:BaseKeyField:KeyVal
               ENDIF
               /* Syncs with the current value (if valid) */
               IF !::table:MasterSource == linkedTable
                  IF empty( ::value )
                     IF ! linkedTable:eof()
                        linkedObjField := linkedTable:LinkedObjField
                        linkedTable:LinkedObjField := NIL
                        linkedTable:dbGoTo( 0 )
                        linkedTable:LinkedObjField := linkedObjField
                     ENDIF
                  ELSE
                     keyVal := ::GetKeyVal( , ... )
                     IF ! linkedTable:BaseKeyField:KeyVal == keyVal
                        linkedObjField := linkedTable:LinkedObjField
                        linkedTable:LinkedObjField := NIL
                        linkedTable:BaseKeyField:SetKeyVal( keyVal )
                        linkedTable:LinkedObjField := linkedObjField
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ELSE
            IF linkedTable:MasterSource != NIL .AND. AScan( { dsEdit, dsInsert }, linkedTable:State ) > 0
               ::FMasterKeyVal := linkedTable:MasterSource:BaseKeyField:KeyVal
            ENDIF
         ENDIF
      ENDIF

      ::FonDataObj := NIL

   ENDIF

   RETURN linkedTable

/*
    GetAsString
*/
METHOD FUNCTION GetAsString(value) CLASS TFieldTable
    LOCAL descriptor
    IF pCount() > 0
        ::dataObj:statePush()
        ::dataObj:value := value
        descriptor := ::dataObj:asString()
        ::dataObj:statePull()
        RETURN descriptor
    ENDIF
RETURN ::dataObj:asString()

/*
    GetAsVariant
*/
METHOD FUNCTION GetAsVariant( ... ) CLASS TFieldTable

   LOCAL variant

   variant := ::Super:GetAsVariant( ... )

   IF HB_ISOBJECT( variant )

      IF variant:IsDerivedFrom( "TFieldTable" )
         // RETURN variant:DataObj:GetAsVariant()
         RETURN variant:GetAsVariant()
      ELSEIF variant:IsDerivedFrom( "TBaseTable" )
         IF variant:BaseKeyField = NIL

            THROW ERROR OODB_ERR__NO_BASEKEYFIELD ON variant

         ENDIF
         RETURN variant:BaseKeyField:GetAsVariant()
      ENDIF

   ENDIF

   RETURN variant

/*
    GetEmptyValue
*/
METHOD FUNCTION GetEmptyValue() CLASS TFieldTable
RETURN baseKeyFieldList( ::getLinkedTable():className )[ 6 ]

/*
    GetFieldReadBlock
*/
METHOD FUNCTION GetFieldReadBlock() CLASS TFieldTable

   IF ::FFieldReadBlock = NIL .AND. ::Super:GetFieldReadBlock() = NIL
      IF ::FlinkedTable = NIL
         IF ::FcalculatingLinkedTable
            ::BuildLinkedTable() /* no result from calculating, so create table from ObjClass name */
         ELSE
            ::GetLinkedTable()
         ENDIF
      ENDIF
      ::FFieldReadBlock := {|| ::FlinkedTable }
   ENDIF

   RETURN ::FFieldReadBlock

/*
    GetKeyVal
*/
METHOD FUNCTION GetKeyVal( keyVal, ... ) CLASS TFieldTable
   LOCAL baseKeyField

   IF keyVal = NIL
      keyVal := ::GetAsVariant( ... )
   ENDIF

   baseKeyField := ::baseKeyField

   keyVal := baseKeyField:getKeyVal( keyVal )

   IF ::FDBS_LEN != nil .AND. baseKeyField:DBS_TYPE = "C" .AND. len( keyVal ) > ::FDBS_LEN
      /*
        len can be different because this field, in the linked table can be of diferent size (greater size)
        i.e. 10 characteres in linked table vs 3 chars in this field, and 3 chars is the relevant size of the field
        then if this field is used in a masterKey it will be of the greater size of the logical field length
      */
      keyVal := left( keyVal, ::FDBS_LEN )
   ENDIF

   RETURN keyVal

/*
    GetLabel
*/
METHOD FUNCTION GetLabel() CLASS TFieldTable

   IF !Empty( ::FLabel )
      RETURN ::FLabel
   ENDIF
   IF ::BaseKeyField != NIL
      RETURN ::BaseKeyField:Label
   ENDIF

   RETURN ""

/*
    GetLinkedTable
*/
METHOD FUNCTION GetLinkedTable( ... ) CLASS TFieldTable

   LOCAL result

   IF ::FCalculated

      IF !::FcalculatingLinkedTable

         ::FcalculatingLinkedTable := .T.

         /* DataEngine can be NIL if table cannot be instanced yet */
         IF ::table:DataEngine == NIL
            result := ::FieldReadBlock:Eval( ::table, ... )
         ELSE
            result := ::table:DataEngine:Eval( ::FieldReadBlock, ::table, ... )
         ENDIF

         IF result != NIL
            IF HB_ISOBJECT( result )
               IF result:IsDerivedFrom( "TBaseTable" )
                  ::FlinkedTable := result
               ELSEIF result:IsDerivedFrom( "TFieldTable" )
                  ::FlinkedTable := result:DataObj()
               ENDIF
            ELSE /* the basekey field value is returned for the calculated field */
               IF ::FlinkedTable = NIL
                  ::BuildLinkedTable()
               ENDIF
               ::FlinkedTable:BaseKeyField:Value := result
            ENDIF
         ENDIF

         IF ::FlinkedTable != NIL .AND. ::FlinkedTable:LinkedObjField == NIL
            ::FlinkedTable:LinkedObjField := Self
         ENDIF

         ::FcalculatingLinkedTable := .F.

         IF result = nil
            RETURN nil
         ENDIF

      ELSE

         ::BuildLinkedTable()

      ENDIF

   ELSE

      IF ::FlinkedTable == NIL
         ::BuildLinkedTable()
      ENDIF

   ENDIF

   RETURN ::FlinkedTable

/*
    GetOnDataChange
*/
METHOD FUNCTION GetOnDataChange() CLASS TFieldTable
   RETURN ::GetLinkedTable:OnDataChangeBlock

/*
    GetValidValues
*/
METHOD FUNCTION GetValidValues() CLASS TFieldTable

   LOCAL hValues
   LOCAL fld

   IF ::FValidValuesLabelField == .T.
      hValues := { => }
      fld := ::LinkedTable:FieldByName( ::FValidValues )
      ::LinkedTable:StatePush()
      ::LinkedTable:dbGoTop()
      WHILE !::LinkedTable:Eof()
         hValues[ ::LinkedTable:KeyVal ] := fld:Value
         ::LinkedTable:dbSkip()
      ENDDO
      ::LinkedTable:StatePull()
      RETURN hValues
   ENDIF

   RETURN ::Super:GetValidValues()

/*
    IndexExpression
*/
METHOD FUNCTION IndexExpression( fieldName ) CLASS TFieldTable
   LOCAL itm

   IF ::FIndexExpression != NIL
      RETURN ::FIndexExpression
   ENDIF

   IF ::Calculated
      RETURN NIL
   ENDIF

   IF fieldName = NIL
      IF ::FUsingField = NIL
         fieldName := ::FFieldExpression
      ELSE
         fieldName := ::FUsingField:FieldExpression
      ENDIF
   ENDIF

   IF fieldName = NIL .AND. ::FFieldMethodType = "A"
      fieldName := {}
      FOR EACH itm IN ::FFieldArrayIndex
         AAdd( fieldName, ::table:FieldList[ itm ]:IndexExpression )
      NEXT
   ENDIF

   RETURN ::BaseKeyField:IndexExpression( fieldName )

/*
    SetLinkedTableMasterSource
*/
METHOD PROCEDURE SetLinkedTableMasterSource( linkedTable ) CLASS TFieldTable

    ::FlinkedTableMasterSource := linkedTable

RETURN

/*
    SetOnDataChange
*/
METHOD PROCEDURE SetOnDataChange( onDataChangeBlock ) CLASS TFieldTable

   ::FonDataChangeBlock := onDataChangeBlock

   RETURN

/*
    SetValidValues
*/
METHOD PROCEDURE SetValidValues( validValues, ignoreUndetermined, labelField ) CLASS TFieldTable

   ::Super:SetValidValues( validValues, ignoreUndetermined )
   ::FValidValuesLabelField := labelField

   RETURN

/*
    ENDCLASS TFieldTable
*/
