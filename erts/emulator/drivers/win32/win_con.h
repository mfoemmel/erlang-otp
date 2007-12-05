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
/* 
 * External API for the windows console (aka werl window)
 * used by ttsl_drv.c
 */
#ifndef _WIN_CON_H_VISITED
#define _WIN_CON_H_VISITED 1
void ConNormalExit(void);
void ConWaitForExit(void);
void ConSetCtrlHandler(BOOL (WINAPI *handler)(DWORD));
int ConPutChar(int c);
void ConSetCursor(int from, int to);
void ConPrintf(char *format, ...);
void ConVprintf(char *format, va_list va);
void ConBeep(void);
int ConReadInput(unsigned char *data, int nbytes);
int ConGetKey(void);
int ConGetColumns(void);
int ConGetRows(void);
void ConInit(void);
#endif /* _WIN_CON_H_VISITED */
