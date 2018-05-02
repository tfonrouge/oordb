/*
 *
 */

/*
    IndexXBase
*/

#include "oordb.ch"
#include "xerror.ch"
#include "inkey.ch"

#include "dbinfo.ch"

/*
    CLASS IndexXBase
*/
CLASS IndexXBase FROM TIndexBase

PROTECTED:

   DATA FForKeyBlock
   DATA FScopeBottom
   DATA FScopeTop
   METHOD DbGoBottomTop( n )
   METHOD GetArrayKeyFields INLINE ::KeyField:FieldMethod
   METHOD GetScope INLINE iif( ::FScopeBottom == NIL .AND. ::FScopeTop == NIL, NIL, { ::FScopeTop, ::FScopeBottom } )
   METHOD GetScopeBottom INLINE iif( !Empty( ::FScopeBottom ), ::FScopeBottom, "" )
   METHOD GetScopeTop INLINE iif( !Empty( ::FScopeTop ), ::FScopeTop, "" )
   METHOD GetUnique INLINE ::FUniqueKeyField != NIL
   METHOD keyValInitValue INLINE ""
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
   METHOD SetScope( value )
   METHOD SetScopeBottom( value )
   METHOD SetScopeTop( value )

PROTECTED:

   METHOD getBagName INLINE ::table:DataEngine:dbOrderInfo( DBOI_BAGNAME, ::FtagName )

   METHOD closeTemporary()

   METHOD CreateIndex()

   METHOD CustomKeyExpValue()

PUBLIC:

    DATA customKeyBlock
    DATA customKeyLen

   DESTRUCTOR onDestruct()

   METHOD __Seek( direction, keyValue, lSoftSeek )

   METHOD closeIndex()
   METHOD COUNT( bForCondition, bWhileCondition )
   METHOD CustomKeyUpdate
   METHOD DbGoBottom INLINE ::DbGoBottomTop( -1 )
   METHOD DbGoTop INLINE ::DbGoBottomTop( 1 )
   METHOD dbSkip( numRecs, lSkipUnique )
   METHOD existsKey( keyValue, recNo )
   METHOD GetKeyVal( keyVal )
   METHOD FillCustomIndex()
   METHOD Get4Seek( blk, keyVal, softSeek )
   METHOD Get4SeekLast( blk, keyVal, softSeek )
   METHOD IndexExpression()
   METHOD InsideScope( ignoreFilters )
   METHOD KeyExpression()
   METHOD MasterKeyExpression()

   METHOD ordKeyNo() INLINE ::table:DataEngine:ordKeyNo()

   METHOD RawGet4Seek( direction, blk, keyVal, softSeek )
   METHOD RawSeek( Value )

   METHOD SetKeyVal( keyVal, lSoftSeek )

   PROPERTY Bof READ table:Bof
   PROPERTY Eof READ table:Eof
   PROPERTY Found READ table:Found
   PROPERTY KeyVal READ GetKeyVal WRITE SetKeyVal
   PROPERTY RecNo READ table:RecNo WRITE table:DbGoTo
   PROPERTY Scope READ GetScope WRITE SetScope
   PROPERTY ScopeBottom READ GetScopeBottom WRITE SetScopeBottom
   PROPERTY ScopeTop READ GetScopeTop WRITE SetScopeTop

   METHOD SEEK( keyValue, lSoftSeek ) INLINE ::__Seek( 0, keyValue, lSoftSeek )
   METHOD SeekLast( keyValue, lSoftSeek ) INLINE ::__Seek( 1, keyValue, lSoftSeek )

PUBLISHED:

   PROPERTY bagName READ getBagName
   PROPERTY fileName
   PROPERTY MasterKeyVal READ GetMasterKeyVal
   PROPERTY ordNumber READ table:DataEngine:ordNumber( ::tagName )
   PROPERTY UNIQUE READ GetUnique

ENDCLASS

/*
    onDestruct
*/
METHOD PROCEDURE onDestruct() CLASS IndexXBase

    ::closeTemporary()

RETURN

/*
    __Seek
*/
METHOD FUNCTION __Seek( direction, keyValue, lSoftSeek ) CLASS IndexXBase

   LOCAL dataEngine

   dataEngine := ::table:DataEngine

   IF AScan( { dsEdit, dsInsert }, ::table:State ) > 0
      ::table:Post()
   ENDIF

   keyValue := ::KeyField:GetKeyVal( keyValue )

   IF direction = 0
      dataEngine:Seek( ::getMasterKeyVal + keyValue, ::FTagName, lSoftSeek )
   ELSE
      dataEngine:SeekLast( ::getMasterKeyVal + keyValue, ::FTagName, lSoftSeek )
   ENDIF

   ::GetCurrentRecord()

   RETURN ::Found

/*
    closeIndex
*/
METHOD PROCEDURE closeIndex() CLASS IndexXBase
    IF ::temporary
        ::closeTemporary()
    ENDIF
RETURN

/*
    closeTemporary
*/
METHOD PROCEDURE closeTemporary() CLASS IndexXBase
    LOCAL fileName

    IF ::temporary .AND. ::Fopened
        IF hb_isObject( ::table )
            fileName := ::table:DataEngine:dbOrderInfo( DBOI_FULLPATH, nil, ::FtagName )
            ::table:DataEngine:ordDestroy( ::FtagName )
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
METHOD FUNCTION COUNT( bForCondition, bWhileCondition ) CLASS IndexXBase

   LOCAL nCount := 0

   ::table:dbEval( {|| ++nCount }, bForCondition, bWhileCondition, Self )

   RETURN nCount

/*
    CreateIndex
*/
METHOD FUNCTION CreateIndex() CLASS IndexXBase
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

    recNo := ::table:DataEngine:RecNo

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

        dbSelectArea( ::table:DataEngine:Name )  // here because ::IndexExpression() may change active WA

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

        BEGIN SEQUENCE WITH ::table:ErrorBlock

            IF ::temporary
                fClose( hb_fTempCreateEx( @bagFileName, nil, "tmp", ::table:DataEngine:dbOrderInfo( DBOI_BAGEXT ) ) )
                hb_FNameSplit( bagFileName, nil, @::FtagName, nil, nil )
            ENDIF

            ordCreate( bagFileName, ::tagName, indexExp, indexExp, unique )

            ::FfileName := ::table:DataEngine:dbOrderInfo( DBOI_FULLPATH, nil, ::tagName )

            ::Fopened := .t.

            IF ::Custom
                ::FillCustomIndex()
            ENDIF

        RECOVER USING oErr

            ui_Alert( ;
                "CreateIndex() Error in " + ::table:ClassName + ", Table: " + ::table:TableFileName + ";" + ;
                " Index Tag Name: " + ::TagName + ";" + ;
                "IndexExpression: " + indexExp + ";" + ;
                "  Index For Key: " + AsString( forKey ) ;
                )

            Break( oErr )

        END SEQUENCE

    ENDIF

    ::table:DataEngine:RecNo := recNo

RETURN .t.

/*
    CustomKeyExpValue
*/
METHOD FUNCTION CustomKeyExpValue() CLASS IndexXBase

   IF ::MasterKeyField = NIL
      RETURN ::KeyVal
   ENDIF

   RETURN ::MasterKeyField:KeyVal + ::KeyVal

/*
    CustomKeyUpdate
*/
METHOD PROCEDURE CustomKeyUpdate CLASS IndexXBase
    LOCAL customKeyValue

    IF ::FCustom .AND. ::Fopened
        WHILE ::table:DataEngine:ordKeyDel( ::FTagName ) ; ENDDO
        IF ::customKeyBlock = nil
            customKeyValue := ::CustomKeyExpValue()
        ELSE
            customKeyValue := ::table:DataEngine:eval( ::customKeyBlock, ::table:displayFieldList )
        ENDIF
        IF Empty( ::FForKeyBlock ) .OR. ::table:DataEngine:Eval( ::FForKeyBlock, ::table )
            ::table:DataEngine:ordKeyAdd( ::FTagName, , customKeyValue )
        ENDIF
    ENDIF

RETURN

/*
    DbGoBottomTop
*/
METHOD FUNCTION DbGoBottomTop( n ) CLASS IndexXBase

   LOCAL masterKeyVal := ::getMasterKeyVal
   LOCAL dataEngine

   dataEngine := ::table:DataEngine

   IF n = 1
      IF ::GetScopeTop() == ::GetScopeBottom()
         dataEngine:Seek( masterKeyVal + ::GetScopeTop(), ::FTagName )
      ELSE
         dataEngine:Seek( masterKeyVal + ::GetScopeTop(), ::FTagName, .T. )
      ENDIF
   ELSE
      IF ::GetScopeTop() == ::GetScopeBottom()
         dataEngine:SeekLast( masterKeyVal + ::GetScopeBottom(), ::FTagName )
      ELSE
         dataEngine:SeekLast( masterKeyVal + ::GetScopeBottom(), ::FTagName, .T. )
      ENDIF
   ENDIF

   IF ::HasFilter() .OR. ::table:HasFilter()
      ::DbFilterPush()
      ::GetCurrentRecord()
      ::DbFilterPull()
      IF ::Eof() .OR. ( !::table:FilterEval( Self ) .AND. !::table:SkipFilter( n, Self ) )
         ::table:dbGoto( 0 )
         RETURN .F.
      ENDIF
   ENDIF

   RETURN ::GetCurrentRecord()

/*
    DbSkip
*/
METHOD FUNCTION dbSkip( numRecs, lSkipUnique ) CLASS IndexXBase

   LOCAL result
   LOCAL n

   IF !::HasFilter() .AND. !::table:HasFilter()
      IF lSkipUnique = .T.
         result := .T.
         WHILE numRecs != 0 .AND. ! ::table:eof() .AND. result
            IF numRecs > 0
                ::seekLast( ::keyVal )
                n := 1
            ELSE
                ::seek( ::keyVal )
                n := -1
            ENDIF
            result := ::table:DataEngine:dbSkip( n, ::FTagName )
            numRecs += - ( n )
         ENDDO
      ELSE
         result := ::table:DataEngine:dbSkip( numRecs, ::FTagName ) /* because on Bof returns .F. */
      ENDIF
      ::GetCurrentRecord()
      RETURN result .AND. ::InsideScope()
   ENDIF

   RETURN ::table:SkipFilter( numRecs, Self )

/*
    existsKey
*/
METHOD FUNCTION existsKey( keyValue, recNo ) CLASS IndexXBase
   RETURN ::table:DataEngine:existsKey( ::getMasterKeyVal + ::KeyField:GetKeyVal( keyValue ), ::FTagName, recNo )

/*
    FillCustomIndex
*/
METHOD PROCEDURE FillCustomIndex() CLASS IndexXBase
    LOCAL index

    ::table:StatePush()

    IF ::FuseIndex = nil
        index := ::table:BaseKeyIndex
        ::table:DbFilterPush( .T. )
    ELSE
        SWITCH valType( ::FuseIndex )
        CASE 'C'
            index := ::table:indexByName( ::FuseIndex )
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
        ::table:DbFilterPull()
    ENDIF

    ::table:StatePull()

RETURN

/*
    Get4Seek
*/
METHOD FUNCTION Get4Seek( blk, keyVal, softSeek ) CLASS IndexXBase
   RETURN ::RawGet4Seek( 1, blk, keyVal, softSeek )

/*
    Get4SeekLast
*/
METHOD FUNCTION Get4SeekLast( blk, keyVal, softSeek ) CLASS IndexXBase
   RETURN ::RawGet4Seek( 0, blk, keyVal, softSeek )

/*
    GetKeyVal
*/
METHOD FUNCTION GetKeyVal( keyVal ) CLASS IndexXBase

   IF ::FKeyField == NIL
      RETURN ""
   ENDIF

   RETURN ::FKeyField:GetKeyVal( keyVal, ::FKeyFlags )

/*
    IndexExpression
*/
METHOD FUNCTION IndexExpression() CLASS IndexXBase

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
METHOD FUNCTION InsideScope( ignoreFilters ) CLASS IndexXBase

   LOCAL masterKeyVal
   LOCAL scopeVal
   LOCAL keyValue

   IF ::table:DataEngine:KeyVal( ::FTagName ) = NIL
      RETURN .F.
   ENDIF

   keyValue := ::table:DataEngine:KeyVal( ::FTagName )

   IF keyValue == NIL .OR. ( !ignoreFilters == .T. .AND. !::table:FilterEval( Self ) )
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
METHOD FUNCTION KeyExpression() CLASS IndexXBase

   IF ::FKeyField != NIL
      RETURN ::FKeyField:IndexExpression
   ENDIF

   RETURN ""

/*
    MasterKeyExpression
*/
METHOD FUNCTION MasterKeyExpression() CLASS IndexXBase

   IF ::FMasterKeyField != NIL
      RETURN ::FMasterKeyField:IndexExpression( NIL, .T., ::KeyFlags )
   ENDIF

   RETURN ""

/*
    RawGet4Seek
*/
METHOD FUNCTION RawGet4Seek( direction, blk, keyVal, softSeek ) CLASS IndexXBase

   IF keyVal = NIL
      keyVal := ::getMasterKeyVal
   ELSE
      keyVal := ::getMasterKeyVal + keyVal
   ENDIF

   RETURN ::table:DataEngine:RawGet4Seek( direction, blk, keyVal, ::FTagName, softSeek )

/*
    RawSeek
*/
METHOD FUNCTION RawSeek( Value ) CLASS IndexXBase

   IF AScan( { dsEdit, dsInsert }, ::table:State ) > 0
      ::table:Post()
   ENDIF

   ::table:DataEngine:Seek( Value, ::FTagName )

   ::GetCurrentRecord()

   RETURN ::table:Found()

/*
    SetKeyVal
*/
METHOD FUNCTION SetKeyVal( keyVal, lSoftSeek ) CLASS IndexXBase

   RETURN ::FKeyField:SetKeyVal( keyVal, lSoftSeek )

/*
    SetScope
*/
METHOD FUNCTION SetScope( value ) CLASS IndexXBase

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
METHOD FUNCTION SetScopeBottom( value ) CLASS IndexXBase

   LOCAL oldValue := ::FScopeBottom

   ::FScopeBottom := value

   RETURN oldValue

/*
    SetScopeTop
*/
METHOD FUNCTION SetScopeTop( value ) CLASS IndexXBase

   LOCAL oldValue := ::FScopeTop

   ::FScopeTop := value

   RETURN oldValue

/*
    End Class IndexXBase
*/
