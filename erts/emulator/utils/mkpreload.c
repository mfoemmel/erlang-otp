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
 * Description:
 * Creates a file that includes modules to be preloaded in the
 * emulator JAM or BEAM
 *
 * Usage: mkpreload [-f file] [-rc] files  ...
 *
 * Output:  
 *     const char module1[] = {
 *         0xXX, ....,          ascii output 
 *          ...
 *     }
 *
 * 
 *     struct preload {
 *           { { "name1", module1, size },
 *             { "name2", module2, size },
 *             ...
 *     }
 *
 * Options:
 *        -rc        generate an windows resource file
 *        -f file    read files list from file
 *
 * Note:  file.beam.c  generate a function entry where size is 0
 *        file.beam    generate a preload entry
 *        file.jam     generate a preload entry
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include <stdio.h> 
#include <string.h>
#include <ctype.h>
#include <stdarg.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <time.h>

#ifdef __WIN32__
#  define strcasecmp stricmp
#endif

typedef unsigned char uchar;

#define get_int32(s) ((((uchar*) (s))[0] << 24) | \
                      (((uchar*) (s))[1] << 16) | \
                      (((uchar*) (s))[2] << 8)  | \
                      (((uchar*) (s))[3]))

#define get_int24(s) ((((uchar*) (s))[0] << 16) | \
                      (((uchar*) (s))[1] << 8)  | \
                      (((uchar*) (s))[2]))

#define get_int16(s) ((((uchar*)  (s))[0] << 8) | \
                      (((uchar*)  (s))[1]))

#define get_int8(s) ((((uchar*)  (s))[0] ))

#define GET8(ptr)      ((ptr) = (ptr) + 1, get_int8(ptr-1))
#define GET16(ptr)     ((ptr) = (ptr) + 2, get_int16(ptr-2))
#define GET24(ptr)     ((ptr) = (ptr) + 3, get_int24(ptr-3))
#define GET32(ptr)     ((ptr) = (ptr) + 4, get_int32(ptr-4))

#define SKIP(ptr, n)   (ptr) += (n)

#define SKIP_STR(ptr) do { \
       int __len; \
       if (GET8(ptr) != L_STRING) \
          error("bad string op"); \
       __len = GET16(ptr); \
       SKIP(ptr, __len); \
 } while(0)     

extern char* malloc();
extern void free();
extern char* getcwd();
extern int read();
extern int close();

#define FILE_JAM    1
#define FILE_BEAM   2
#define FILE_BEAM_C 3

#define JAM_SUFFIX    ".jam"
#define BEAM_SUFFIX   ".beam"
#define BEAM_C_SUFFIX ".beam.c"

typedef struct file_entry {
    struct file_entry* next;
    int type;
    char* module_name;
    unsigned int size;
    int code_num;
    char file_name[1];
} FileEntry;

FileEntry* first_file;
FileEntry* last_file;
int code_num;


char* program = "mkpreload";

void error(char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
   
    fprintf(stderr, "%s : ", program);
    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n");
    exit(1);
}


void* alloc(size)
int size;
{
    void* p = (void*) malloc(size);
    if (p == NULL)
	error("Can't allocate %d bytes of memory", size);
    return p;
}

void add_file_name(name)
char* name;
{
    FileEntry* ent;
    int type;
    int n;

    /* match .jam, .beam & .beam.c suffux */
    n = strlen(name);
    if ((n > strlen(JAM_SUFFIX)) && 
	(strcasecmp(name+n-strlen(JAM_SUFFIX), JAM_SUFFIX) == 0))
	type = FILE_JAM;
    else if ((n > strlen(BEAM_SUFFIX)) &&
	     (strcasecmp(name+n-strlen(BEAM_SUFFIX), BEAM_SUFFIX) == 0))
	type = FILE_BEAM;
    else if ((n > strlen(BEAM_C_SUFFIX)) &&
	     (strcasecmp(name+n-strlen(BEAM_C_SUFFIX), BEAM_C_SUFFIX) == 0))
	type = FILE_BEAM_C;
    else
	error("bad file suffix, use .jam, .beam or .beam.c");

    ent = (FileEntry*) alloc(sizeof(FileEntry) + strlen(name) + 1);
    strcpy(ent->file_name, name);
    ent->next = NULL;
    ent->type = type;
    ent->size = 0;
    ent->module_name = NULL;

    if (first_file == NULL)
	first_file = ent;
    else
	last_file->next	= ent;
    last_file = ent;
}


void load_file_names(file)
char* file;
{
    int c;
    FILE *fl;
    char filebuf[1024];   /* filename buf */
    char* ptr;
    
    if ((fl = fopen(file, "r")) == NULL)
	error("Can't open file %s", file);

    c = '@'; /* DUMMY, NOT EOF */
    while((c != EOF)) {
	ptr = filebuf;
	while(1) {
	    c = getc(fl);
	    if (c == EOF)
		break;
	    if ( (c!='\n') && (c!=' ') && (c!='\t')) {
		*ptr++ = c;
		break;
	    }
	}
	if (c == EOF)
	    break;
	while(( (c=getc(fl)) != EOF) && (c != '\n'))
	    *ptr++ = c;
	*ptr++ = '\0';
	add_file_name(filebuf);
    }
    fclose(fl);
}


/* Emit module code */
void emit_module(out, ent, code, rc)
FILE* out; FileEntry* ent; uchar* code; int rc;
{
    int size = ent->size;
    uchar* ptr = code;
    int i;

    if (code == NULL)
	return;

    if (rc)
	fprintf(out, "%d  ERLANG_CODE \"%s\"\n",
		ent->code_num, ent->file_name);
    else {
	fprintf(out, "static char code_%d[] = {\n", ent->code_num);

	while(size) {
	    int n = (size > 12) ? 12 : size;
	    size -= n;
	    for (i = 0; i < n; i++)
		fprintf(out, "0x%02x,", ptr[i]);
	    fprintf(out, " /* ");
	    for (i = 0; i < n; i++) {
		/* special case emit * & / ends a comment !!! (add a .) */
		if ((i > 0) && (ptr[i-1] == '*') && (ptr[i] == '/'))
		    fprintf(out, "./");
		else if (isprint(ptr[i]))
		    fprintf(out, "%c", ptr[i]);
		else
		    fprintf(out, ".");
	    }
	    fprintf(out, " */\n");
	    ptr += n;
	}
	fprintf(out, "0 };\n");
    }
}


void emit_preload(out, rc)
FILE* out; int rc;
{
    FileEntry* ent;
    int beam_c_count;

    if (rc) {
	int n;
	fprintf(out, "\n#include <jam.rc>\n");
	fprintf(out, "\n0  ERLANG_DICT\n");
	fprintf(out, "BEGIN\n");
	for (n = 0, ent = first_file; ent != NULL; ent = ent->next)
	    n++;
	fprintf(out, "    %d,\n", n);
	for (ent = first_file; ent != NULL; ent = ent->next) 
	    fprintf(out, "    %dL, %d, %d, \"%s\",\n",
		    ent->size, ent->code_num, 
		    strlen(ent->module_name), ent->module_name);
	fprintf(out, "END\n\n");
    }
    else {
	/* Make compatible with global Preload structure 
	 ** I do not want to inlcude global.h here
	 ** since we whould like to generate preload files
	 ** and link with emulator outside clearcase !!!
	 */
	beam_c_count = 0;
	for (ent = first_file; ent != NULL; ent = ent->next) {
	    if (ent->type == FILE_BEAM_C) {
		fprintf(out, "extern unsigned long (* %s_)();\n", 
			ent->module_name);
		fprintf(out, "extern unsigned long (* %s_load_part)();\n",
			ent->module_name);
		beam_c_count++;
	    }
	}
	fprintf(out, "struct {\n");
	fprintf(out, "   char* name;\n");
	fprintf(out, "   int size;\n");
	fprintf(out, "   void* ptr1;\n");
	fprintf(out, "   void* ptr2;\n");
	fprintf(out, "} pre_loaded[] = {\n");
	for (ent = first_file; ent != NULL; ent = ent->next) {
	    if (ent->type == FILE_BEAM_C)
		fprintf(out, "\t{ \"%s\", 0, 0, 0},\n",
			ent->module_name);
	    else
		fprintf(out, "\t{ \"%s\", %d, code_%d, 0},\n",
			ent->module_name, ent->size, ent->code_num);
	}
	fprintf(out, "\t{ 0, 0, 0, 0}\n");
	fprintf(out, "};\n");
    }
}


void check_module(ent, code)
FileEntry* ent; uchar* code;
{
    char* mod;
    int len;
    int i;

    if ( ((mod = strrchr(ent->file_name, '/')) == NULL)
#ifdef __WIN32__
	&& ((mod = strrchr(ent->file_name, '\\')) == NULL)
#endif
	)
	mod = ent->file_name;
    else
	mod++;
    len = strlen(mod);

    switch (ent->type) {
    case FILE_JAM:
	len -= strlen(JAM_SUFFIX);
	ent->code_num = code_num++;
	break;
    case FILE_BEAM:
	len -= strlen(BEAM_SUFFIX);
	ent->code_num = code_num++;
	break;
    case FILE_BEAM_C:
	len -= strlen(BEAM_C_SUFFIX);
	break;
    }
    ent->module_name = (char*) alloc(len + 1);
    /* copy & convert to lowercase */
    for (i = 0; i < len; i++)
	ent->module_name[i] = tolower(mod[i]);
    ent->module_name[len] = '\0';
}


int main(int argc, char *argv[]) 
{ 
    int fd;
    int windows_rc;
    FILE *out;
    time_t now;
    char buf[BUFSIZ];
    struct stat statbuf;
    char* outfile = NULL;
    uchar* code;
    int n = 1;
    int type;
    FileEntry* ent;
    FileEntry* ent2;

    first_file = NULL;
    last_file = NULL;
    code_num = 1;
    windows_rc = 0;

    while (n < argc) {
	if ( (strcmp(argv[n], "-f") == 0) && (n < argc-1)) {
	    load_file_names(argv[n+1]);
	    n += 2;
	}
	else if ((strcmp(argv[n], "-o") == 0) && (n < argc-1)) {
	    outfile = argv[n+1];
	    n += 2;
	}
	else if (strcmp(argv[n], "-rc") == 0) {
	    windows_rc = 1;
	    n++;
	}
	else if (argv[n][0] != '-') {
	    add_file_name(argv[n]);
	    n++;
	}
	else 
	    error("unkown option %s", argv[n]);
    }

    if (outfile == NULL) {
	if (windows_rc)
	    outfile = "preload.rc";
	else
	    outfile = "preload.c";
    }

    /* Check file type */
    type = 0;
    for (ent = first_file; ent != NULL; ent = ent->next) {
	switch(type) {
	case 0: 
	    type = ent->type;
	    break;
	case FILE_JAM:
	    if (ent->type != FILE_JAM)
		error("Can't mix beam and jam file");
	    break;
	case FILE_BEAM:
	case FILE_BEAM_C:
	    if (ent->type == FILE_JAM)
		error("Can't mix beam and jam file");
	}
    }

    if ((out = fopen(outfile,"w")) == NULL) {
	fprintf(stderr, "%s: Can't open %s\n", program, outfile);
	return(1);
    }
    fprintf(out,"/* This code is generated by the mkpreload program */\n");
    fprintf(out,"/* The files contained in this code are:\n");
    for (ent = first_file; ent != NULL; ent = ent->next)
	fprintf(out," * %s\n", ent->file_name);
    
    time(&now);
    fprintf(out," * Generated on %s",ctime(&now));
    fprintf(out," * Current work directory is %s",getcwd(buf,BUFSIZ));
    fprintf(out," */\n\n\n");

    for (ent = first_file; ent != NULL; ent = ent->next) {
	if (stat(ent->file_name,&statbuf) < 0)
	    error("Can't stat %s", ent->file_name);
	if (ent->type == FILE_BEAM_C) {
	    code = NULL;       /* no code */
	    ent->size = 0;     /* size = 0 */
	}
	else {
	    ent->size = statbuf.st_size;
	    code = (unsigned char*) alloc(ent->size);
	    /* open and read the file */
#ifdef __WIN32__
	    if ((fd = open(ent->file_name,O_RDONLY|O_BINARY)) < 0) /* sigh */
#else
		if ((fd = open(ent->file_name,O_RDONLY,0)) < 0)
#endif
		    error("Can't open %s", ent->file_name);
	    if (read(fd, code, ent->size) != ent->size)
		error("Failed in reading %s", ent->file_name);

	    close(fd);
	    /* Find out what the module name is by looking in the file */
	}
	check_module(ent, code);
	for (ent2 = first_file; ent2 != ent; ent2 = ent2->next) {
	    if (strcmp(ent->module_name, ent2->module_name) == 0)
		error("duplicate modules %s", ent2->module_name);
	}
	emit_module(out,ent,code,windows_rc);
	if (code != NULL)
	    free(code);
    }

    emit_preload(out, windows_rc);
    fclose(out);
    return(0);
}
