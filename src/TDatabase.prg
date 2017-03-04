/*
 *
 * TDataBase
 */

#include "hbclass.ch"
#include "property.ch"
#include "oordb.ch"
#include "xerror.ch"

#define OORDB_DEFAULT_TABLEAUTOCREATE   .T.

/*
    TDataBase
*/
CLASS TDataBase FROM OORDBBASE

PRIVATE:

   DATA cmdLevel     INIT { NIL }

PROTECTED:

   DATA FName
   METHOD DefineRelations VIRTUAL
   METHOD GetName() INLINE iif( Empty( ::FName ), ::ClassName, ::FName )
   METHOD SetDirectory( directory ) INLINE ::FDirectory := directory
   METHOD SetName( name ) INLINE ::FName := name
   METHOD SetNetIO( netIO ) INLINE ::FNetIO := netIO

PUBLIC:

   CONSTRUCTOR New( databaseName )

   METHOD AddParentChild( parentTableName, childTableName, indexName, virtual, autoDelete )

   METHOD cmdAddTable( tableName, indexName, virtual, autoDelete )
   METHOD cmdDefineChild()
   METHOD cmdEndChild()

   METHOD getMasterSourceClassName( className )
   METHOD GetParentChildList( tableName, Result )
   METHOD TableIsChildOf( table, fromTable )

   PROPERTY Name            READ GetName WRITE SetName
   PROPERTY NetIO           WRITE SetNetIO INIT .F.
   PROPERTY TableAutoCreate INIT OORDB_DEFAULT_TABLEAUTOCREATE
   PROPERTY ChildParentList INIT HB_HSetCaseMatch( { => }, .F. )
   PROPERTY Directory       WRITE SetDirectory INIT ""
   PROPERTY ParentChildList INIT HB_HSetCaseMatch( { => }, .F. )
   PROPERTY TableList       INIT HB_HSetCaseMatch( { => }, .F. )

ENDCLASS

/*
    New
*/
METHOD New( databaseName ) CLASS TDataBase

   IF databaseName == NIL
      ::FName := ::ClassName
   ELSE
      ::FName := databaseName
   ENDIF
   IF !Empty( ::FDirectory ) .AND. !hb_DirExists( ::FDirectory )
      MakeDir( ::FDirectory )
   ENDIF
   ::DefineRelations()

   RETURN Self

/*
    AddParentChild
*/
METHOD AddParentChild( parentTableName, childTableName, indexName, virtual, autoDelete ) CLASS TDataBase

   IF ! hb_HHasKey( ::FParentChildList, parentTableName )
      ::FParentChildList[ parentTableName ] := {}
   ENDIF

   IF hb_HHasKey( ::FTableList, childTableName )
      ::Error_Table_already_defined()
   ENDIF

   ::FTableList[ childTableName ] := HB_HSetCaseMatch( { => }, .F. )
   ::FTableList[ childTableName, "IndexName" ]  := indexName
   ::FTableList[ childTableName, "Virtual"   ]  := virtual == .T.
   ::FTableList[ childTableName, "AutoDelete" ] := autoDelete == .T.

   AAdd( ::FParentChildList[ parentTableName ], Upper( childTableName ) )

   ::FChildParentList[ childTableName ] := parentTableName

   RETURN Self

/*
    cmdAddTable
*/
METHOD PROCEDURE cmdAddTable( tableName, indexName, virtual, autoDelete ) CLASS TDataBase

   ::cmdLevel[ Len( ::cmdLevel ) ] := { tableName, indexName, virtual }

   IF Len( ::cmdLevel ) > 1
      ::AddParentChild( ::cmdLevel[ Len( ::cmdLevel ) - 1, 1 ], tableName, indexName, virtual, autoDelete )
   ENDIF

   RETURN

/*
    cmdDefineChild
*/
METHOD PROCEDURE cmdDefineChild() CLASS TDataBase

   AAdd( ::cmdLevel, { NIL } )

   RETURN

/*
    cmdEndChild
*/
METHOD PROCEDURE cmdEndChild() CLASS TDataBase

   ASize( ::cmdLevel, Len( ::cmdLevel ) - 1 )

   RETURN

/*
    getMasterSourceClassName
*/
METHOD FUNCTION getMasterSourceClassName( className ) CLASS TDataBase

   LOCAL Result := ""

   className := rTrim( className )

   IF hb_HHasKey( ::ChildParentList, className )
      Result := ::ChildParentList[ className ]
      WHILE hb_HHasKey( ::TableList, Result ) .AND. ::TableList[ Result, "Virtual" ]
         IF !hb_HHasKey ( ::ChildParentList, Result )
            EXIT
         ENDIF
         Result := ::ChildParentList[ Result ]
      ENDDO
   ENDIF

RETURN Result

/*
    GetParentChildList
    Exclude VIRTUAL Tables
*/
METHOD FUNCTION GetParentChildList( tableName, Result ) CLASS TDataBase

   LOCAL childTableName

   IF Result == NIL
      Result := {}
        /* If Table is VIRTUAL then search for childs in next child level
             Parent tables without parent aren't in the TableList
        */
      IF hb_HHasKey( ::FTableList, tableName ) .AND. ::FTableList[ tableName, "Virtual" ]
         IF hb_HHasKey( ::FParentChildList, tableName )
            FOR EACH childTableName IN ::FParentChildList[ tableName ]
               ::GetParentChildList( childTableName, Result )
            NEXT
         ENDIF
         RETURN Result
      ENDIF
   ENDIF

   IF hb_HHasKey( ::FParentChildList, tableName )
      FOR EACH childTableName IN ::FParentChildList[ tableName ]
         IF ::FTableList[ childTableName, "Virtual" ]
            IF hb_HHasKey( ::FParentChildList, childTableName )
               ::GetParentChildList( childTableName, Result )
            ENDIF
         ELSE
            AAdd( Result, childTableName )
         ENDIF
      NEXT
   ENDIF

   RETURN Result

/*
    TableIsChildOf
*/
METHOD FUNCTION TableIsChildOf( table, fromTable ) CLASS TDataBase

   LOCAL Result

   Result := hb_HHasKey( ::FParentChildList, fromTable ) .AND. AScan( ::FParentChildList[ fromTable ], {| e| e == Upper( table ) } ) > 0

   RETURN Result

/*
    EndClass TDataBase
*/
