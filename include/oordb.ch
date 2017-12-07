/*
 *
 */

/*
    Arelds.ch
*/

#ifndef _OORDB_H_
#define _OORDB_H_

#include "hbclass.ch"
#include "property.ch"

#include "oordbdefs.ch"

/* Events for TBaseTable */
#xtranslate EVENT ONAFTEROPEN => METHOD OnAfterOpen()
#xtranslate EVENT ONAFTEROPEN CLASS <className> => METHOD PROCEDURE OnAfterOpen() CLASS <className>

#xtranslate EVENT ONAFTERINSERT => METHOD OnAfterInsert()
#xtranslate EVENT ONAFTERINSERT CLASS <className> => METHOD PROCEDURE OnAfterInsert() CLASS <className>

/* Events for TField's */
#xtranslate EVENT ONBEFORECHANGE FIELD <!name!> ;
    => ;
    METHOD OnBeforeChange_Field_<name>( field_<name>, value )

#xtranslate DEFINE RELATIONS ;
    => ;
    METHOD DefineRelations()

#xtranslate BEGIN RELATIONS CLASS <className> ;
    => ;
    METHOD PROCEDURE DefineRelations() CLASS <className>

#xtranslate END RELATIONS [CLASS] ;
    => ;
    RETURN

#xtranslate ADD TABLE <tableName> [ <vt: VIRTUAL> ] [ INDEX <indexName> ] [ <auto: AUTODELETE> ] ;
            => ;
            ::cmdAddTable( <tableName>, [ <indexName> ], <.vt.>, <.auto.> )

#xtranslate DEFINE CHILD ;
            => ;
            ::cmdDefineChild()

#xtranslate END CHILD ;
            => ;
            ::cmdEndChild()

#xtranslate ADD TABLE <parentTableName> CHILD <childTableName> [ INDEX <indexName> ];
            => ;
            ::AddParentChild( <parentTableName>, <childTableName>, [ <indexName> ] )

/* To REQUIRE FORMAT in TFieldTime */
#xtranslate ADD [<clauses0,...>] TIME FIELD <xFieldMethod> [<clauses1,...>] FORMAT <timeFormat> [<clauses2,...>] ;
                        => ;
                        ADD [<clauses0>] _TIME FIELD <xFieldMethod> [<clauses1>] FORMAT <timeFormat> [<clauses2>]
#xtranslate TField_Time => TFieldTime

/* HTIME fields (float type based time) */
#xtranslate ADD [<clauses0,...>] HTIME FIELD <xFieldMethod> [<clauses1,...>] ;
                        => ;
                        ADD [<clauses0>] TIME FIELD <xFieldMethod> FTYPE "B" LEN 8 DEC 2 [<clauses1>]

/* To REQUIRE SIZE in TFieldString */
#xtranslate ADD [<clauses0,...>] STRING FIELD <xFieldMethod> [<clauses1,...>] SIZE <nsize> [<clauses2,...>] ;
                        => ;
                        ADD [<clauses0>] _STRING FIELD <xFieldMethod> [<clauses1>] SIZE <nsize> [<clauses2>]
#xtranslate TField_String => TFieldString

/* To REQUIRE LEN DEC in TFieldNumeric */
#xtranslate ADD [<clauses0,...>] NUMERIC FIELD <xFieldMethod> [<clauses1,...>] LEN <nLen> DEC <nDec> [<clauses2,...>] ;
                        => ;
                        ADD [<clauses0>] _NUMERIC FIELD <xFieldMethod> [<clauses1>] LEN <nLen> DEC <nDec> [<clauses2>]
#xtranslate TField_Numeric => TFieldNumeric

/* To REQUIRE CLASS in TFieldTable */
#xtranslate ADD [<clauses0,...>] TABLE FIELD <xFieldMethod> [<clauses1,...>] CLASS <objClass> [<clauses2,...>] ;
                        => ;
                        ADD [<clauses0>] _TABLE FIELD <xFieldMethod> [<clauses1>] CLASS <objClass> [<clauses2>]
#xtranslate TField_Table => TFieldTable

#xtranslate ADD [<calc: CALCULATED>] <type: _STRING, MEMO, _NUMERIC, FLOAT, INTEGER, AUTOINC, ROWVER, LOGICAL, _TIME, DATE, DATETIME, MODTIME, _TABLE, VARIANT, ARRAY, HASH> FIELD [<xFieldMethod>] ;
                        [ NAME <cName> ] ;
                        [ LABEL <label> ] ;
                        [ <ro: READONLY> ] ;
                        [ DEFAULTVALUE <xDefaultValue> ] ;
                        [ NEWVALUE <xNewValue> ] ;
                        [ READ <readblock,...> ] ;
                        [ WRITE <writeblock,...> ] ;
                        [ <rq: REQUIRED> ] ;
                        [ GROUP <cGroup> ] ;
                        [ DESCRIPTION <cDesc> ] ;
                        [ SIZE <nSize> ] ;
                        [ FORMAT <timeFormat> ] ;
                        [ FTYPE <fType> ] ;
                        [ LEN <nLen> ] ;
                        [ DEC <nDec> ] ;
                        [ PICTURE <pict> ] ;
                        [ FORMATDATE <fmtDate> ] ;
                        [ FORMATTIME <fmtTime> ] ;
                        [ <pv: PRIVATE> ] ;
                        [ INCREMENT <incrementBlock> ] ;
                        [ MASTERSOURCE <linkedTableMasterSource> ] ;
                        [ CLASS <objClass> ] ;
                        [ CLASSINIT <clsInit> ] ;
                        [ ON GETTEXT <bOnGetText> ] ;
                        [ ON SETTEXT <bOnSetText> ] ;
                        [ ON VALIDATE <bOnValidate> [ WARN <warnMsg> ] ] ;
                        [ ON SEARCH <bOnSearch> ] ;
                        [ ON BEFORE CHANGE <bOnBeforeChange> ] ;
                        [ ON AFTER CHANGE <bOnAfterChange> ] ;
                        [ ON AFTER POST CHANGE <bOnAfterPostChange> ] ;
                        [ ON DATA CHANGE <bOnDataChange> ] ;
                        [ ON SETKEY <bOnSetKey> ] ;
                        [ VALIDVALUES [<vvl: LABEL>] <validValues> [<igUndet: IGNORE_UNDETERMINED>] ] ;
                        [ USING <usingField> ] ;
                        [ <ruf: REUSEFIELD> ] ;
                        [ ENABLED <enabled> ] ;
                        [ EDITABLE <editable> ] ;
                        [ INDEXEXPRESSION <indexExp> ] ;
                        [ DEFAULTINDEX <defaultIndexName> ] ;
                        [ DISPLAY <dispBlock> ] ;
                        [ <aeu: ACCEPT_EMPTY_UNIQUE> ] ;
                        [ CARGO <cargoValue>  ] ;
                        [ <nbff: NO_BUFFERED> ] ;
                        [ <bin: HAS_BINARY> ] ;
                     => ;
                        WITH OBJECT TField<type>():New( Self, ::curClassField ) ;;
                            [ :Name := <cName> ] ;;
                            [ :Label := <label> ] ;;
                            [ :ReadOnly := <.ro.> ] ;;
                            [ :ReUseField := <.ruf.> ] ;;
                            [ :ObjClass := <objClass> ] ;;
                            [ :SetClassInit( <clsInit> ) ] ;;
                            [ :Size := <nSize> ] ;;
                            [ :TimeFormat := <timeFormat> ] ;;
                            [ :SetFieldMethod( <xFieldMethod>, <.calc.> ) ] ;;
                            [ :FieldReadBlock := <readblock> ] ;;
                            [ :FieldWriteBlock := <writeblock> ] ;;
                            [ :DefaultValue := <xDefaultValue> ] ;;
                            [ :NewValue := <xNewValue> ] ;;
                            [ :Required := <.rq.> ] ;;
                            [ :Group := <cGroup> ] ;;
                            [ :Description := <cDesc> ] ;;
                            [ :DBS_TYPE := <fType> ] ;;
                            [ :DBS_LEN := <nLen> ] ;;
                            [ :DBS_DEC := <nDec> ] ;;
                            [ :Picture := <pict> ] ;;
                            [ :FormatDate := <fmtDate> ] ;;
                            [ :FormatTime := <fmtTime> ] ;;
                            [ :Published := !<.pv.> ] ;;
                            [ :IncrementBlock := <incrementBlock> ] ;;
                            [ :LinkedTableMasterSource := <linkedTableMasterSource> ] ;;
                            [ :OnGetText := {|field,Text| <bOnGetText> } ] ;;
                            [ :OnSetText := {|field,Text| <bOnSetText> } ] ;;
                            [ :OnValidate := <bOnValidate> ] ;;
                            [ :OnValidateWarn := <warnMsg> ] ;;
                            [ :OnSearch := <bOnSearch> ] ;;
                            [ :OnBeforeChange := <bOnBeforeChange> ] ;;
                            [ :OnAfterChange := <bOnAfterChange> ] ;;
                            [ :OnAfterPostChange := <bOnAfterPostChange> ] ;;
                            [ :OnDataChange := <bOnDataChange> ] ;;
                            [ :SetKeyValBlock( <bOnSetKey> ) ] ;;
                            [ :SetValidValues( <validValues>, <.igUndet.>, <.vvl.> ) ] ;;
                            [ :UsingField := <usingField> ] ;;
                            [ :Enabled := <enabled> ] ;;
                            [ :Editable := <editable> ] ;;
                            [ :SetIndexExpression( <indexExp> ) ] ;;
                            [ :SetDefaultIndexName( <defaultIndexName> ) ] ;;
                            [ :DisplayBlock := <dispBlock> ] ;;
                            [ :acceptEmptyUnique := <.aeu.> ] ;;
                            [ :cargo := <cargoValue> ] ;;
                            [ :setBuffered( ! <.nbff.> ) ] ;;
                            [ :setHasBinary( <.bin.> ) ] ;;
                            :AddFieldMessage() ;;
                            :ValidateFieldInfo() ;;
                        ENDWITH

/* Calculated field with METHOD_READ method */
#xtranslate ADD CALCULATED [<clauses0,...>] FIELD <fldName> METHOD_READ <mthd> [<clauses1,...>] ;
                        => ;
                        ADD CALCULATED [<clauses0>] FIELD <fldName> READ {|Self,...| ::<mthd>(<fldName>,...) } [<clauses1>]

/* Calculated field with METHOD method */
#xtranslate ADD CALCULATED [<clauses0,...>] FIELD <fldName> METHOD <mthd> [<clauses1,...>] ;
                        => ;
                        ADD CALCULATED [<clauses0>] FIELD <fldName> READ {|Self| ::<mthd>(<fldName>) } WRITE {|Self,...| ::<mthd>( <fldName>,... ) } [<clauses1>]

/* Calculated field with ALLOW_WRITE method */
#xtranslate ADD CALCULATED [<clauses0,...>] FIELD <fldName> ALLOW_WRITE [<clauses1,...>] ;
                        => ;
                        ADD CALCULATED [<clauses0>] FIELD <fldName> WRITE <fldName> [<clauses1>]

#xtranslate ADD ALIAS NAME <aliasFldName> TO FIELD <fld> [<noPub> PRIVATE ] ;
            => ;
            ::AddFieldAlias( <aliasFldName>, <fld>, [!<.noPub.>] )

#xtranslate DEFINE DATABASE WITH <methodDatabase> CLASS ;
    => ;
    METHOD InitDataBase INLINE <methodDatabase>():New()

#xtranslate DEFINE BASEDOCUMENT ;
    => ;
    DATA FbaseDocument INIT oClass:cName

#xtranslate DEFINE ROOTDOCUMENT ;
    => ;
    DATA FrootDocument INIT oClass:cName

#xtranslate DEFINE FIELDS ;
    => ;
    DATA curClassField HIDDEN ;;
    METHOD __DefineFields()

#xtranslate DEFINE PRIMARY INDEX ;
    => ;
    DATA curClassPrimaryIndex HIDDEN ;;
    METHOD __DefinePrimaryIndex()

#xtranslate DEFINE SECONDARY INDEX ;
    => ;
    DATA curClassIndex HIDDEN ;;
    METHOD __DefineSecondaryIndexes()

#xtranslate BEGIN FIELDS CLASS <className>;
                        => ;
                        METHOD PROCEDURE __DefineFields() CLASS <className> ;;
                        ::curClassField := <(className)> ;;
                        ::Super:__DefineFields()

#xtranslate END FIELDS [CLASS] ;
                        => ;
                        ::__DefineFields_Exit() ;;
                        RETURN

#xtranslate BEGIN PRIMARY INDEX CLASS <className> ;
                        => ;
            METHOD PROCEDURE __DefinePrimaryIndex() CLASS <className> ;;
            LOCAL __typeIndex__ := "PRIMARY" ;;
            ::curClassPrimaryIndex := <(className)> ;;
            ::Super:__DefinePrimaryIndex() ;;

#xtranslate END PRIMARY INDEX [CLASS] ;
                        => ;
            HB_SYMBOL_UNUSED( __typeIndex__ ) ;;
            RETURN

#xtranslate BEGIN SECONDARY INDEX CLASS <className> ;
                        => ;
            METHOD PROCEDURE __DefineSecondaryIndexes() CLASS <className> ;;
            LOCAL __typeIndex__ := "SECONDARY" ;;
            ::curClassIndex := <(className)> ;;
            ::Super:__DefineSecondaryIndexes() ;;

#xtranslate END SECONDARY INDEX [CLASS] ;
                        => ;
            RETURN

#xtranslate METHOD CALCFIELD <clcField> => METHOD CalcField_<clcField>
#xtranslate METHOD CALCFIELD <clcField> CLASS <className> ;
    => ;
    METHOD FUNCTION CalcField_<clcField> CLASS <className>
#xtranslate METHOD CALCFIELD <clcField>( [<params,...>] ) CLASS <className> ;
    => ;
    METHOD FUNCTION CalcField_<clcField>( [<params>] ) CLASS <className>

#xtranslate ON SETVALUE FIELD <svField> ;
    => ;
    METHOD OnSetValue_Field_<svField>
#xtranslate ON SETVALUE FIELD <svField> CLASS <className> ;
    => ;
    METHOD PROCEDURE OnSetValue_Field_<svField>( value ) CLASS <className>

/* TODO: Implement this, needs to use a index declared in ancestor class
#xtranslate DEFINE PRIMARY INDEX <cName> ;
            => ;
            TIndex():New( Self, <cName>, "PRIMARY" )
*/

#xtranslate DEFINE INDEX [TAG] <tagName> [NAME <name>] ;
                        [ MASTERKEYFIELD <cMasterKeyField> ] ;
                        [ KEYFIELD <cKeyField> ] ;
                        [ KEYFLAGS <keyFlags> ] ;
                        [ FOR <ForKey> ] ;
                        [ <ncs: NO_CASE_SENSITIVE> ] ;
                        [ <rj: RJUST> ] ;
                        [ <de: DESCEND> ] ;
                        [ <cu: CUSTOM> ] ;
                        [ <un: UNIQUE> ] ;
                        [ <ai: AUTOINCREMENT,AUTOINCREMENT_BASE64> ] ;
                        [ <tm: TEMPORARY> ] ;
                        [ USEINDEX <useIndex> ] ;
                        [ <acceptEmptyUnique: ACCEPT_EMPTY_UNIQUE> ] ;
                        [ ON KEYVIOLATION WARN <errorMsg> ] ;
                        [ <def: DEFAULT > ] ;
                        => ;
                        WITH OBJECT TIndex():New( Self , <tagName>, [<name>], __typeIndex__, iif( __typeIndex__ == "PRIMARY", ::curClassPrimaryIndex, ::curClassIndex ), <errorMsg> ) ;;
                            :AddIndex( [<cMasterKeyField>], [<"ai">], [<.un.>], [<cKeyField>], [<keyFlags>], [<ForKey>], !<.ncs.>, [<.de.>], [<.acceptEmptyUnique.>], [<useIndex>], [<.tm.>], [<.rj.>], [<.cu.>], [<.def.>] ) ;;
                        ENDWITH

#xtranslate USE INDEX <name> [<def: DEFAULT>] => ::bindIndex( .t., <name>, __typeIndex__, iif( __typeIndex__ == "PRIMARY", ::curClassPrimaryIndex, ::curClassIndex ), [<.def.>] )

#xtranslate CREATE <type: PRIMARY,SECONDARY> INDEX [TAG] <tagName> [NAME <name>] ;
                        [ MASTERKEYFIELD <cMasterKeyField> ] ;
                        [ KEYFIELD <cKeyField> ] ;
                        [ FOR <ForKey> ] ;
                        [ KEYFLAGS <keyFlags> ] ;
                        [ <ncs: NO_CASE_SENSITIVE> ] ;
                        [ <rj: RJUST> ] ;
                        [ <de: DESCEND> ] ;
                        [ <cu: CUSTOM> ] ;
                        [ <un: UNIQUE> ] ;
                        [ <ai: AUTOINCREMENT,AUTOINCREMENT_BASE64> ] ;
                        [ <tm: TEMPORARY> ] ;
                        [ USEINDEX <useIndex> ] ;
                        [ <acceptEmptyUnique: ACCEPT_EMPTY_UNIQUE> ] ;
                        => ;
                        WITH OBJECT TIndex():New( Self , <tagName>, [<name>], <"type"> ) ;;
                                :AddIndex( [<cMasterKeyField>], [<"ai">], [<.un.>], [<cKeyField>], [<keyFlags>], [<ForKey>], !<.ncs.>, [<.de.>], [<.acceptEmptyUnique.>], [<useIndex>], [<.tm.>], [<.rj.>], [<.cu.>] ) ;;
                        ENDWITH
