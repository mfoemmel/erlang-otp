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

#ifndef __erl_image_h__
#define __erl_image_h__

#include <gd.h>
#include <gdfontt.h>
#include <gdfonts.h>
#include <gdfontmb.h>
#include <gdfontl.h>
#include <gdfontg.h>

#include "egd_port_driver.h"

#define EGD_ARC_STYLE_ARC (1)
#define EGD_ARC_STYLE_CHORD (2)
#define EGD_ARC_STYLE_NO_FILL (4)
#define EGD_ARC_STYLE_EDGED (8)

void image_create(egd_data *data, char *buffer, int l);

void image_destroy(egd_data *data);

void image_pixel(egd_data *data, char *buffer, int l);

void image_line(egd_data *data, char *buffer, int l);

void image_color(egd_data *data, char *buffer, int l);

void image_rectangle(egd_data *data, char *buffer, int l);

void image_filled_rectangle(egd_data *data, char *buffer, int l);

void image_polygon(egd_data *data, char *buffer, int l);

void image_filled_polygon(egd_data *data, char *buffer, int l);

void image_arc(egd_data *data, char *buffer, int l);

void image_filled_arc(egd_data *data, char *buffer, int l);

void image_filled_ellipse(egd_data *data, char *buffer, int l);

void image_text_up(egd_data *data, char *buffer, int l);

void image_text(egd_data *data, char *buffer, int l);

void image_text(egd_data *data, char *buffer, int l);

void image_resample(egd_data *data, char *buffer, int l);

void image_rotate(egd_data *data, char *buffer, int l);

void image_fill(egd_data *data, char *buffer, int l);

/* image fetchers */

void image_gif(egd_data *data, char *buffer, int l);
void image_jpeg(egd_data *data, char *buffer, int l);
void image_png(egd_data *data, char *buffer, int l);

/* Font */

gdFontPtr image_font(int size);

/* Alloc helper */

gdPoint *image_alloc_points(char *buffer, int n);
void image_free_points(gdPoint *pts);

char *image_alloc_string(char *buffer, int n);
void image_free_string(char *str);
#endif
