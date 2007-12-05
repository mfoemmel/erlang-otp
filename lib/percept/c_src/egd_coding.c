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
unsigned int decode16(unsigned char**);
unsigned char *encode16(unsigned char*, unsigned int);
unsigned int decode32(unsigned char**);
unsigned char *encode32(unsigned char*, unsigned int);


int decode(char **data) {
    return decode32((unsigned char**) data);
}

unsigned char *encode(unsigned char *buffer, int data) {
    return encode32(buffer, (unsigned int)data);
}

unsigned int decode_color(char **data) {
    return decode(data);
}

unsigned char *encode_color(unsigned char *buffer, unsigned int color) {
    return encode(buffer, color);
}

unsigned int decode16(unsigned char **data) {
    unsigned int value = 0;
    value = (*data)[0];
    value = (value << 8) + (*data)[1];
    *data += 2;
    return value;
}

unsigned char *encode16(unsigned char *buffer, unsigned int data) {
    buffer[1] = data & 0xff;
    buffer[0] = (data >> 8) & 0xff;
    return buffer;
}

unsigned int decode32(unsigned char **data) {
    unsigned int value = 0;
    value = (*data)[0];
    value = (value << 8) + (*data)[1];
    value = (value << 8) + (*data)[2];
    value = (value << 8) + (*data)[3];
    *data += 4;
    return value;
}

unsigned char *encode32(unsigned char *buffer, unsigned int data) {
    buffer[3] = data & 0xff;
    buffer[2] = (data >> 8) & 0xff;
    buffer[1] = (data >> 16) & 0xff;
    buffer[0] = (data >> 24) & 0xff;
    return buffer;
}
