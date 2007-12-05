/* ``The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved via the world wide web at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * The Initial Developer of the Original Code is Ericsson Utvecklings AB.
 * Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 * 
 *     $Id$
 */

#ifndef __egd_port_driver_h__
#define __egd_port_driver_h__

#include <erl_driver.h>

#define IM_CREATE (1)
#define IM_COLOR (5)
#define IM_PIXEL (10)
#define IM_LINE (13)
#define IM_RECTANGLE (14)
#define IM_FILLEDRECTANGLE (15)
#define IM_POLYGON (16)
#define IM_FILLEDPOLYGON (17)
#define IM_ARC (18)
#define IM_FILLEDARC (19)
#define IM_FILLEDELLIPSE (20)
#define IM_TEXT (21)
#define IM_TEXTUP (22)
#define IM_FILL (23)

#define IM_RESAMPLE (60)
#define IM_ROTATE (61)

#define IM_FONT_SIZE (62)

#define IM_GIF (100)
#define IM_JPEG (101)
#define IM_PNG (102)

typedef struct {
    ErlDrvPort port;
    gdImagePtr im;
} egd_data;


#endif
