/*
 *
 */

#include "hbclass.ch"
#include "oordb.ch"
#include "property.ch"

/*
    TDisplayFieldList : Simple Class to display fields
*/
CLASS TDisplayFieldList

   PUBLIC:

   DATA __FObj
   DATA __FSyncFromDataEngine

   DATA isSubField

   METHOD _ INLINE ::__FObj
   METHOD __FieldList

ENDCLASS

/*
    __FieldList
*/
METHOD FUNCTION __FieldList() CLASS TDisplayFieldList
   LOCAL a := {}
   LOCAL itm

   FOR EACH itm IN ::__IndexFieldList
      AAdd( a, { itm:__enumKey, ::__FObj:FieldList[ itm:__enumValue ] } )
   NEXT

RETURN a
