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
#include <gd.h>
#include <gdfontt.h>
#include <gdfonts.h>
#include <gdfontmb.h>
#include <gdfontl.h>
#include <gdfontg.h>

#include <erl_driver.h>

#include <math.h>
#include "egd_image.h"
#include "egd_coding.h"

int egd_options_to_gd_arc_style(int options);

void send_result(egd_data *d, unsigned int result) {
    unsigned char buffer[4];
    driver_output(d->port, (char *)encode((char *)buffer, result), 4);
}

void image_create(egd_data *d, char *buffer, int l){
    int w,h;
    w = decode(&buffer);
    h = decode(&buffer);
    d->im = gdImageCreateTrueColor(w, h);
    send_result(d, 0);
}

void image_destroy(egd_data *d)  {
    if (d != NULL && d->im != NULL) gdImageDestroy(d->im); 	
}

/* X, Y, Color */
void image_pixel(egd_data *d, char *buffer, int l) {
    int x = decode(&buffer);
    int y = decode(&buffer);
    int c = decode_color(&buffer);
    gdImageSetPixel(d->im, x, y, c); 
    send_result(d, 0);
}

/* X1, Y1, X2, Y2, Color */
void image_line(egd_data *d, char *buffer, int l) {
    int x1 = decode(&buffer);
    int y1 = decode(&buffer);
    int x2 = decode(&buffer);
    int y2 = decode(&buffer);
    int c = decode_color(&buffer);
    gdImageLine(d->im, x1, y1, x2, y2, c);
    send_result(d, 0);
}

/* r, g, b */
void image_color(egd_data *d, char *buffer, int l) {
    int r = decode(&buffer);
    int g = decode(&buffer);
    int b = decode(&buffer);
    int color = gdImageColorAllocate(d->im, r,g,b);
    send_result(d, color);
}

void image_rectangle(egd_data *d, char *buffer, int l) {
    int x1 = decode(&buffer);
    int y1 = decode(&buffer);
    int x2 = decode(&buffer);
    int y2 = decode(&buffer);
    int c = decode_color(&buffer);
    gdImageRectangle(d->im, x1, y1, x2, y2, c);
    send_result(d, 0);
}

void image_filled_rectangle(egd_data *d, char *buffer, int l) {
    int x1 = decode(&buffer);
    int y1 = decode(&buffer);
    int x2 = decode(&buffer);
    int y2 = decode(&buffer);
    int c = decode_color(&buffer);
    gdImageFilledRectangle(d->im, x1, y1, x2, y2, c);
    send_result(d, 0);
}

/* color, points */
void image_polygon(egd_data *d, char *buffer, int l) {
    int color = decode_color(&buffer);
    int n = decode(&buffer);
    gdPoint *pts = image_alloc_points(buffer, n);
    
    gdImagePolygon(d->im, pts, n, color);
    image_free_points(pts);
    send_result(d, 0);
}

void image_filled_polygon(egd_data *d, char *buffer, int l) {
    int color = decode_color(&buffer);
    int n = decode(&buffer);
    gdPoint *pts = image_alloc_points(buffer, n);
    
    gdImageFilledPolygon(d->im, pts, n, color);
    image_free_points(pts);

    send_result(d, 0);
}

/* cx, cy, w, h, s, e. color*/
void image_arc(egd_data *d, char *buffer, int l) {
    int cx = decode(&buffer);
    int cy = decode(&buffer);
    int w = decode(&buffer);
    int h = decode(&buffer);
    int s = decode(&buffer);
    int e = decode(&buffer);
    int c = decode_color(&buffer);
    gdImageArc(d->im, cx, cy, w, h, s, e, c);
    send_result(d, 0);
}


/* cx, cy, w, h, s, e. color, style_options */
void image_filled_arc(egd_data *d, char *buffer, int l) {
    int cx = decode(&buffer);
    int cy = decode(&buffer);
    int w = decode(&buffer);
    int h = decode(&buffer);
    int s = decode(&buffer);
    int e = decode(&buffer);
    int c = decode_color(&buffer);
    int options = decode(&buffer);
    int style = egd_options_to_gd_arc_style(options);
    gdImageFilledArc(d->im, cx, cy, w, h, s, e, c, style);
    send_result(d, 0);
}


void image_filled_ellipse(egd_data *d, char *buffer , int l) {
    int cx = decode(&buffer);
    int cy = decode(&buffer);
    int w = decode(&buffer);
    int h = decode(&buffer);
    int c = decode_color(&buffer);
    gdImageFilledEllipse(d->im, cx, cy, w, h, c);
    send_result(d, 0);
}

/* Texting */


void image_text(egd_data *d, char *buffer, int l) {
    int x = decode(&buffer);
    int y = decode(&buffer);
    int size = decode(&buffer);
    int c = decode_color(&buffer);
    int str_len = decode(&buffer);
    unsigned char *str = (unsigned char*)image_alloc_string((char*)buffer, str_len);
    gdImageString(d->im, image_font(size), x,y, str, c);
    image_free_string((char *)str);
    send_result(d, 0);
}

void image_text_up(egd_data *d, char *buffer, int l) {
    int x = decode(&buffer);
    int y = decode(&buffer);
    int size = decode(&buffer);
    int c = decode_color(&buffer);
    int str_len = decode(&buffer);
    unsigned char *str = (unsigned char*)image_alloc_string((char *)buffer, str_len);
    gdImageStringUp(d->im, image_font(size), x,y, str, c);
    image_free_string((char *)str);
    send_result(d, 0);
}

void image_font_size(egd_data *d, char *buffer, int l) {
    int size = decode(&buffer);
    unsigned char output[8];
    gdFontPtr font = image_font(size);
    int w = font->w;
    int h = font->h;
    /* dangerous */
    encode((char*)output, w);
    encode((char*)output + 4, h);
    driver_output(d->port, output, 8);
}

void image_fill(egd_data *d, char *buffer, int l) {
    int x = decode(&buffer);
    int y = decode(&buffer);
    int c = decode_color(&buffer);
    gdImageFill(d->im, x, y, c);
    send_result(d, 0);
}

/* rotate and resample */

/* void image_rotate
 * In: 
 * 	egd_data *d, image pointer, port data
 *	char *buffer, incoming data from erlang
 *	int l, length of data
 * Abstract:
 * 	The buffer only contains the angle of rotation.
 */

void image_rotate(egd_data *d, char *buffer, int l){
    int angle = decode(&buffer);
    float alpha = angle * .0174532925f;
    int w = fabs((d->im->sx)*cos(alpha)) + fabs((d->im->sy)*sin(alpha));
    int h = fabs((d->im->sx)*sin(alpha)) + fabs((d->im->sy)*cos(alpha));
    gdImagePtr im = gdImageCreateTrueColor(w,h);
    gdImageCopyRotated(im, d->im, w/2.0, h/2.0, 0,0, d->im->sx, d->im->sy, angle);
    gdImageDestroy(d->im);
    d->im = im;
    send_result(d, 0);
}

void image_resample(egd_data *d, char *buffer, int l){
    int new_w = decode(&buffer);
    int new_h = decode(&buffer);
    gdImagePtr im = gdImageCreateTrueColor(new_w, new_h);
    gdImageCopyResampled(im, d->im, 0, 0, 0, 0, new_w, new_h, d->im->sx, d->im->sy);
    gdImageDestroy(d->im);
    d->im = im;
    send_result(d, 0);
}


/* Fetch images */

void image_gif(egd_data *d, char *buffer, int l){
    int size = 0;
    void *gif = NULL; 
    
    if ( (gif = gdImageGifPtr(d->im, &size)) == NULL) {
	fprintf(stderr, "Gif fetch error\n");
    	send_result(d, 1);
	return;
    } 
	
    driver_output(d->port, (char*)gif, size);
    gdFree(gif);
}

void image_jpeg(egd_data *d, char *buffer, int l){
    int size = 0;
    void *jpeg = NULL;
    int quality = decode(&buffer);

    if ( (jpeg = gdImageJpegPtr(d->im, &size, quality)) == NULL) {
	fprintf(stderr, "Jpeg fetch error\n");
    	send_result(d, 1);
	return;
    } 

    driver_output(d->port, (char*)jpeg, size);
    gdFree(jpeg);
}

void image_png(egd_data *d, char *buffer, int l){
    int size = 0;
    void *png = NULL;

    if ( (png = gdImagePngPtr(d->im, &size)) == NULL) {
	fprintf(stderr, "Png fetch error\n");
    	send_result(d, 1);
	return;
    }
    driver_output(d->port, (char*)png, size);
    gdFree(png);
}


/* Helpers */

gdFontPtr image_font(int size) {
    if (size == 1) {
    	return gdFontGetTiny();
    } else if (size == 2) {
    	return gdFontGetSmall();
    } else if (size == 3) {
    	return gdFontGetMediumBold();
    } else if (size == 4) {
    	return gdFontGetLarge();
    } else if (size == 5) {
    	return gdFontGetGiant();
    }
    return gdFontGetSmall();
}

int egd_options_to_gd_arc_style(int options) {
    int style = 0;

    if (options & EGD_ARC_STYLE_ARC) style |= gdArc;
    if (options & EGD_ARC_STYLE_CHORD) style |= gdChord;
    if (options & EGD_ARC_STYLE_NO_FILL) style |= gdNoFill;
    if (options & EGD_ARC_STYLE_EDGED) style |= gdEdged;

    return style;
}
/* Allocators */

gdPoint *image_alloc_points(char *buffer, int n) {
    int i;
    gdPoint *pts = (gdPoint*)driver_alloc(sizeof(gdPoint)*n);

    for (i = 0; i < n; i++) {
	pts[i].x = decode(&buffer);
	pts[i].y = decode(&buffer);
    }
    return pts;
}

void image_free_points(gdPoint *pts) {
    driver_free(pts);
}

char *image_alloc_string(char *buffer, int n) {
    int i;
    char *str = (char *)driver_alloc(sizeof(char)*(n + 1));
    for (i = 0; i < n; i++) {
	str[i] = (char)decode(&buffer);
    }
    str[n] = '\0';
    return str;
}

void image_free_string(char *str) {
    driver_free(str);
}
