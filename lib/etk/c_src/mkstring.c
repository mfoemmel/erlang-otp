/*
**  Create string tables
**  for erlang and c
**
**  in erlang: tkstr.erl
**
**  [ {str, Name, Index}, ... , {opt, Name, Index} ]
**
**  in c:
**            tkstr.c
**              tkstr[ix] = "abcd"
**              tkopt[ix] = "-abcd"
**
**            tkstr.h
**              #define OPT_ABCD    ix
**              #define STR_ABCD    ix
**
** Usage:
**     mkstring [table]
**   
**  
*/

#include <stdio.h>
#include <ctype.h>
#include <string.h>

#define MAX_NAME 80


typedef struct String {
    struct String* next;
    char str[1];
} String;

String* first_str = NULL;
String* last_str = NULL;

char* program = NULL;


void add_string(name) 
char* name;
{
    String* ptr = (String*) malloc(sizeof(String) + strlen(name));

    strcpy(ptr->str, name);

    ptr->next = NULL;
    if (last_str != NULL)
	last_str->next = ptr;
    last_str = ptr;
    if (first_str == NULL)
	first_str = ptr;
}

int quote_string(str)
char* str;
{
    if (strcmp(str, "end") == 0)
	return 1;
    else if (strcmp(str, "after") == 0)
	return 1;
    else if (strcmp(str, "fun") == 0)
	return 1;
    else if (strcmp(str, "receive") == 0)
	return 1;
    return 0;
}

/*
** Load string in format
** str   [ # comment ]
** -str  [ # comment ]
** <blank line>
** # comment
*/

void load_strings(fname)
char* fname;
{
    FILE* fin;
    int line = 1;
    char name[MAX_NAME+1];
    char* ptr;
    int len;

    if ((fin = fopen(fname, "r")) == NULL) {
	fprintf(stderr, "%s : Could not open %s\n", program, fname);
	exit(1);
    }

    while(!feof(fin)) {
        int c = fgetc(fin);

        /* Skip comments */
        if (c == '#') {
            while((c = fgetc(fin)) != '\n' && c != EOF)
                ;
            line++;
        }
        else {
            while(c == ' ' || c == '\t' || c == '\n') {
                if (c == '\n')
                    line++;
                c = fgetc(fin);
            }
            if (c == EOF)
                break;

	    len = 0;
	    ptr = name;

	    if (c == '-') {
		*ptr++ = c;
		len++;
		c = fgetc(fin);
	    }
	    if (islower(c)) {
		*ptr++ = c;
		len++;
		c = fgetc(fin);
	    }
	    else {
                fprintf(stderr, "bad format in %s line %d\n", fname, line);
		exit(1);
	    }
	    while(isalnum(c) && (len <= MAX_NAME)) {
		*ptr++ = c;
		len++;
		c = fgetc(fin);
	    }
	    *ptr = '\0';
	    while (c == ' ' || c == '\t') {
		c = fgetc(fin);
	    }
	    if ((c != '\n') && (c != EOF)) {
                fprintf(stderr, "bad format in %s line %d\n", fname, line);
		exit(1);
	    }
	    add_string(name);
            line++;
        }
    }
    fclose(fin);
}


void main(argc, argv)
int argc; char** argv;
{
    FILE* ferl;
    FILE* fc;
    FILE* fh;
    char* infile;
    int strix;
    int optix;
    String* ptr;

    program = argv[0];

    if (argc == 1)
	infile = "strings.tab";
    else if (argc == 2)
	infile = argv[1];
    else {
	fprintf(stderr, "%s : Too many args", argv[0]);
	exit(1);
    }

    load_strings(infile);

    if ((ferl = fopen("tkstr.erl", "w")) == NULL) {
	fprintf(stderr, "%s : Could not open tkstr.erl\n", argv[0]);
	exit(1);	
    }
    if ((fc = fopen("tkstr.c", "w")) == NULL) {
	fprintf(stderr, "%s : Could not open tkstr.c\n", argv[0]);
	exit(1);	
    }
    if ((fh = fopen("tkstr.h", "w")) == NULL) {
	fprintf(stderr, "%s : Could not open tkstr.h\n", argv[0]);
	exit(1);	
    }

    /* PRELUDE */

    fprintf(ferl, "%% File: tkstr.erl generated from %s\n", infile);
    fprintf(ferl, "-module(tkstr).\n");
    fprintf(ferl, "-export([strings/0]).\n\n");
    fprintf(ferl, "strings() -> [\n");

    fprintf(fh, "/* File: tkstr.h generated from %s */\n", infile);
    fprintf(fh, "#ifndef __TK_STR_H__\n");
    fprintf(fh, "#define __TK_STR_H__\n\n");
    fprintf(fh, "extern char* tkstr[];\n");
    fprintf(fh, "extern char* tkopt[];\n\n");
    
    fprintf(fc, "/* File: tkstr.c generated from %s */\n", infile);
    fprintf(fc, "char* tkstr[] = {\n");

    ptr = first_str;
    strix = 0;
    while(ptr) {
	if (ptr->str[0] != '-') {
	    if (quote_string(ptr->str))
		fprintf(ferl, " { str, \'%s\', %d },\n", ptr->str, strix);
	    else
		fprintf(ferl, " { str, %s, %d },\n", ptr->str, strix);
	    fprintf(fc, " \"%s\",\n", ptr->str);
	    fprintf(fh, "#define STR_%s %d\n", ptr->str, strix);
	    strix++;
	}
	ptr = ptr->next;
    }
    fprintf(fc, " 0 };\n\n");

    fprintf(fc, "char* tkopt[] = {\n");
    ptr = first_str;
    optix = 0;
    while(ptr) {
	if (ptr->str[0] == '-') {
	    if (quote_string(ptr->str+1)) 
		fprintf(ferl, " { opt, \'%s\', %d },\n", ptr->str+1, optix);
	    else
		fprintf(ferl, " { opt, %s, %d },\n", ptr->str+1, optix);
	    fprintf(fc, " \"%s\",\n", ptr->str);
	    fprintf(fh, "#define OPT_%s %d\n", ptr->str+1, optix);
	    optix++;
	}
	ptr = ptr->next;
    }
    fprintf(fc, " 0 };\n");

    fprintf(ferl, " eof ].\n");

    fprintf(fh, "#define OPT_MAX %d\n", optix);
    fprintf(fh, "#define STR_MAX %d\n", strix);
    fprintf(fh, "#endif\n");

    fclose(fh);
    fclose(fc);
    fclose(ferl);

    exit(0);
}


