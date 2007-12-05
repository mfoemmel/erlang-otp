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

#include "egd_image.h"
#include "egd_coding.h"
#include "egd_port_driver.h"

static ErlDrvData egd_drv_start(ErlDrvPort port, char *buffer) {
    egd_data *d = (egd_data*)driver_alloc(sizeof(egd_data));
    d->port = port;
    d->im = NULL;
    return (ErlDrvData)d;
}

static void egd_drv_stop(ErlDrvData handle) {
    egd_data* d = (egd_data*)handle;
    image_destroy(d);
    driver_free((char*)handle);
}

static void egd_drv_output(ErlDrvData handle, char *buffer, int n) {
    egd_data* d = (egd_data*)handle;
    int command = 0;
    unsigned char error_code[4];
    
    /* Error Control  */
    /* get command */
    command = decode(&buffer); n -= 2;

    switch(command) {
    	case IM_CREATE: 
	image_create(d, buffer, n);
	break;
    	
	case IM_PIXEL:
	image_pixel(d, buffer, n);	
    	break;
	
	case IM_LINE:
	image_line(d, buffer, n);	
    	break;

	case IM_RECTANGLE:
	image_rectangle(d, buffer, n);
	break;
	
	case IM_FILLEDRECTANGLE:
	image_filled_rectangle(d, buffer, n);
	break;
	
	case IM_POLYGON:
	image_polygon(d, buffer, n);
	break;
	
	case IM_FILLEDPOLYGON:
	image_filled_polygon(d, buffer, n);
	break;
	
	case IM_COLOR:
	image_color(d, buffer, n);
	break;

	case IM_ARC:
	image_arc(d, buffer, n);
	break;

	case IM_FILLEDARC:
	image_filled_arc(d, buffer, n);
	break;

	case IM_FILLEDELLIPSE:
	image_filled_ellipse(d, buffer, n);
	break;

	case IM_TEXT:
	image_text(d, buffer, n);
	break;
	
	case IM_TEXTUP:
	image_text_up(d, buffer, n);
	break;
	
	case IM_FONT_SIZE:
	image_font_size(d, buffer, n);
	break;

	case IM_FILL:
	image_fill(d, buffer, n);
	break;

	/* Resampling och Rotate */

	case IM_RESAMPLE:
	image_resample(d, buffer, n);
	break;
	
	case IM_ROTATE:
	image_rotate(d, buffer, n);
	break;

	/* Fetching images */
	case IM_GIF:
	image_gif(d,buffer,n);
	break;

	case IM_JPEG:
	image_jpeg(d,buffer,n);
	break;

	case IM_PNG:
	image_png(d,buffer,n);
	break;
	
	default:
    	driver_output(d->port, (char *)encode((char *)error_code, 1), 4);
	break;
    }

}

ErlDrvEntry egd_driver_entry = {
    NULL, 		/* F_PTR init, N/A */
    egd_drv_start,	/* L_PTR start, called when port is opened */
    egd_drv_stop,	/* F_PTR stop, called when port is closed */
    egd_drv_output,	/* F_PTR output, called when erlang has sent */
    NULL, 		/* F_PTR ready_input, called when input descriptor ready */
    NULL,		/* F_PTR ready_output, called when output descriptor ready */
    "egd_drv",		/* char *driver_name, the argument to open_port */
    NULL,		/* F_PTR finish, called when unloaded */
    NULL,		/* F_PTR control, port_command callback */
    NULL, 		/* F_PTR timeout, reserved */
    NULL		/* F_PTR outputv, reserved */
};

DRIVER_INIT(egd_drv) {
    return &egd_driver_entry;
}


