/*
 *
 */

/*
    OORDB Copyright (C) 2012 Teo Fonrouge

    This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

    (C) 2008 Teo Fonrouge <teo@windtelsoft.com>
*/

#ifndef _RDODEFS_H_
#define _RDODEFS_H_

#define RDO_OPEN		1

#define SND_BUFFERSIZE  4096

#define dsInactive  0
#define dsBrowse    1
#define dsInsert    2
#define dsEdit      4

#define dssNone     0
#define dssAdding   1
#define dssPosting  2

#define ftBase      0
#define ftArray     1
#define ftDate      2
#define ftDateTime  3
#define ftHash      4
#define ftLogical   5
#define ftNumeric   6
#define ftString    7
#define ftTable     8
#define ftMemo      9
#define ftTime      10
#define ftVariant   11

#define ftAutoInc   101
#define ftRowVer    102
#define ftFloat     103
#define ftInteger   104
#define ftModTime   105

#define noRetry     .T.

#define OORDB_ERROR_NONE                0
#define OORDB_ERROR_EDIT_BROWSE_STATE   1
#define OORDB_ERROR_EDIT_EOF            2
#define OORDB_ERROR_ONBEFOREEDIT        3
#define OORDB_ERROR_EDIT_RECLOCK        4

#endif
