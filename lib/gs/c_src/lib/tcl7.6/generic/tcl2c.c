#include <tcl.h>

#ifdef __cplusplus
extern C {
#endif

extern void exit _ANSI_ARGS_((int status));

#ifdef __cplusplus
}
#endif

#define MAX_STRING_LEN 8192 /* give warning if string length exceeds this value */

#ifdef NO_STRING_H
#   include <strings.h>
#   define strchr index
#   define strrchr rindex
#else
#   include <string.h>
#endif

typedef struct tableitem {
    char *package;/* corresponding packagename */
    char *option; /* option expected */
    int flag ;    /* which packages are needed */
} tableitem;

static tableitem table[] = {
{"Tcl"  ,"-tcl"  ,  1},
{"Tk"   ,"-tk"   ,  3},
{"Tclx" ,"-tclx" ,  5},
{"Itcl" ,"-itcl" ,  9},
{"Otcl" ,"-otcl" , 17},	/* not tested yet */
{"Pvm"  ,"-pvm"  , 33},
{"Tkx"  ,"-tkx"  , 71},
{"Itk"  ,"-itk" ,  139},
{"Iwidgets" ,"-iwidgets" ,  395},
{"Img"  ,"-img"  ,515},
{"Tix"  ,"-tix"  ,1027}, /* not tested yet */
{"Blt"  ,"-blt"  ,2051}, /* not tested yet */
{"Vtcl" ,"-vtcl" ,4103}, /* not tested yet */
};

static char *verbose = "\n\
*************************** tcl2c ********************************\n\
written by: Jan Nijtmans\n\
            NICI (Nijmegen Institute of Cognition and Information)\n\
	    email: nijtmans@nici.kun.nl\n\
	    url:   http://www.cogsci.kun.nl/~nijtmans/\n\n\
usage:	tcl2c -o file source1 source2 ... ?options?\n\
	tcl2c -help\n\
";

static char *help = "\n\
available options:\n\
        -a              use character array instead of string for script\n\
        -n script_name  name of script variable\n\
        -o filename	output file (default is stdout)\n\
	-tcl		use Tcl\n\
	-tclx		use Tclx\n\
	-itcl		use Itcl\n\
	-otcl		use Otcl (not tested yet)\n\
	-pvm		use tkPvm\n\
	-tk		use Tk\n\
	-tkx		use Tkx (not really usefull)\n\
	-itk		use Itk\n\
	-iwidgets	use Iwidgets\n\
	-tix		use Tix (not tested yet)\n\
	-blt		use Blt (not tested yet)\n\
	-vtcl		use Vtcl (not tested yet)\n\
	-index		convert tclIndex files\n\n\
Other command line arguments are assumed to be tcl script-files. It is \n\
possible to include C-files (with the extension .c), which are already\n\
converted tcl-scripts. These are included using the \"#include\".\n\n\
The output file can be compiled with any C or C++ compiler.\n\
";

static char *part1 = "\n\
/* This file is created by the \"tcl2c\" utility, which is included in\n\
 * most \"plus\"-patches (e.g. for Tcl7.6 and Tcl8.0). Standalone\n\
 * executables can be made by providing alternative initialization\n\
 * functions which don't read files any more. Sometimes, small\n\
 * adaptations to the original libraries are needed to get the\n\
 * application truly standalone. The \"plus\"-patches contain these\n\
 * adaptations for Tcl and Tk. If you just create your own\n\
 * Xxx_InitStandAlone() function for your package, you can\n\
 * easyly extend the \"tcl2c\" utility to your own requirements.\n\
 *\n\
 *	Jan Nijtmans\n\
 *	NICI (Nijmegen Institute of Cognition and Information)\n\
 *	email: nijtmans@nici.kun.nl\n\
 *	url:   http://www.cogsci.kun.nl/~nijtmans/\n\
 */\n\
#include \"tcl.h\"\n\
\n\
/*\n\
 * Defines to replace the standard Xxx_Init calls to Xxx_InitStandAlone.\n\
 * If you don't have this function, just delete the corresponding\n\
 * define such that the normal initialization function is used.\n\
 * Similar: If SafeInit functions exists, you can use these\n\
 * by commenting out the corresponding lines below.\n\
 */\n\
\n\
#ifdef TCL_ACTIVE\n\
";

static char *part2 = "#endif\n\
\n\
";

static char *part3 = "\n\
/*\n\
 * Prototypes of all initialization functions and the free() function.\n\
 * So, only \"tcl.h\" needs to be included now.\n\
 */\n\
\n\
#ifdef __cplusplus\n\
extern \"C\" {\n\
#endif\n\
\n\
extern void free _ANSI_ARGS_((void *));\n\
extern int  Tcl_Init _ANSI_ARGS_((Tcl_Interp *interp));\n\
";
static char *part4 = "\n\
\n\
#ifdef __cplusplus\n\
}\n\
#endif\n\
\n\
/*\n\
 * The array \"%s\" contains the script that is compiled in.\n\
 * It will be executed in tclAppInit() after the other initializations.\n\
 */\n\
\n\
static int line = (__LINE__ + 1);\n\
";

static char *part5 = "\
/*\n\
 *----------------------------------------------------------------------\n\
 *\n\
 * main --\n\
 *\n\
 *	This is the main program for the application.\n\
 *\n\
 * Results:\n\
 *	None.\n\
 *\n\
 * Side effects:\n\
 *	Whatever the application does.\n\
 *\n\
 *----------------------------------------------------------------------\n\
 */\n\
\n\
void\n\
#ifdef _USING_PROTOTYPES_\n\
main (int    argc,		/* Number of command-line arguments. */\n\
      char **argv)		/* Values of command-line arguments. */\n\
#else\n\
main(argc, argv)\n\
    int argc;			/* Number of command-line arguments. */\n\
    char **argv;		/* Values of command-line arguments. */\n\
#endif\n\
{\n\
    Tcl_Interp *interp;\n\
    char **p = %s;\n\
    char *q, buffer[10];\n\
    Tcl_DString data;\n\
    Tcl_Channel inChannel, outChannel, errChannel;\n\
\n\
    Tcl_FindExecutable(argv[0]);\n\
    interp = Tcl_CreateInterp();\n\
\n\
    q = Tcl_Merge(argc-1, argv+1);\n\
    Tcl_SetVar(interp, \"argv\", q, TCL_GLOBAL_ONLY);\n\
    ckfree(q);\n\
    sprintf(buffer, \"%%d\", argc-1);\n\
    Tcl_SetVar(interp, \"argc\", buffer, TCL_GLOBAL_ONLY);\n\
    Tcl_SetVar(interp, \"argv0\", argv[0],TCL_GLOBAL_ONLY);\n\
    Tcl_SetVar(interp, \"tcl_interactive\",\"0\", TCL_GLOBAL_ONLY);\n\
\n\
";

static char *part6 = "\n\
    /*\n\
     * Execute the script that is compiled in.\n\
     */\n\
\n\
    inChannel = Tcl_GetStdChannel(TCL_STDIN);\n\
    outChannel = Tcl_GetStdChannel(TCL_STDOUT);\n\
    Tcl_DStringInit(&data);\n\
    while(*p) {\n\
	Tcl_DStringSetLength(&data,0);\n\
	Tcl_DStringAppend(&data,*p++,-1);\n\
	if (Tcl_Eval(interp,Tcl_DStringValue(&data)) != TCL_OK) {\n\
	    Tcl_DStringFree(&data);\n\
	    while (p-- != %s) {\n\
		for (q = *p;*q; q++) {\n\
		    if (*q=='\\n') line++;\n\
		}\n\
		line++;\n\
	    }\n\
	    sprintf(buffer,\"%%d\",line);\n\
	    Tcl_AddErrorInfo(interp,\"\\n	( Error in file: \\\"\");\n\
	    Tcl_AddErrorInfo(interp,__FILE__);\n\
	    Tcl_AddErrorInfo(interp,\"\\\", line: \");\n\
	    Tcl_AddErrorInfo(interp,buffer);\n\
	    Tcl_AddErrorInfo(interp,\")\");\n\
	    errChannel = Tcl_GetStdChannel(TCL_STDERR);\n\
	    if (errChannel) {\n\
		Tcl_Write(errChannel,\n\
			Tcl_GetVar(interp, \"errorInfo\", TCL_GLOBAL_ONLY), -1);\n\
		Tcl_Write(errChannel, \"\\n\", 1);\n\
	    }\n\
	    sprintf(buffer, \"exit %%d\", 1);\n\
	    Tcl_Eval(interp, buffer);\n\
	}\n\
    }\n\
    Tcl_DStringFree(&data);\n\
\n\
    while (Tcl_DoOneEvent(0)) {\n\
	/* empty loop body */ ;\n\
    }\n\
    sprintf(buffer, \"exit %%d\", 0);\n\
    Tcl_Eval(interp, buffer);\n\
\n\
error:\n\
    errChannel = Tcl_GetStdChannel(TCL_STDERR);\n\
    if (errChannel) {\n\
	Tcl_Write(errChannel,\n\
		\"application-specific initialization failed: \", -1);\n\
	Tcl_Write(errChannel, interp->result, -1);\n\
	Tcl_Write(errChannel, \"\\n\", 1);\n\
    }\n\
    sprintf(buffer, \"exit %%d\", 1);\n\
    Tcl_Eval(interp, buffer);\n\
}\n\
";

static char *defineproto1 = "\
#define %s_Init %s_InitStandAlone\n\
";

static char *defineproto2 = "\
#define %s_SafeInit (Tcl_PackageInitProc *) NULL\n\
";

static char *initproto = "\
extern int  %s_Init _ANSI_ARGS_((Tcl_Interp *interp));\n\
#ifndef %s_SafeInit\n\
extern int  %s_SafeInit _ANSI_ARGS_((Tcl_Interp *interp));\n\
#endif\n\
";

static char *packageproto = "\
    Tcl_StaticPackage(interp, \"%s\", %s_Init, %s_SafeInit);\n\
";

static char *callinit = "\
    if (%s_Init(interp) != TCL_OK) {\n\
        goto error;\n\
    }\n\
";

static char buffer[32768];
static unsigned int max_buffer = 0;
static char max_buffer_content[80];

static char array_instead_of_string = 0;
static unsigned int num_lines = 0;

static char script_name[256];

int printline _ANSI_ARGS_((FILE *f, char *buf, char *dir, int flags));
int printfile _ANSI_ARGS_((FILE *fout, char *filename, char *dir, int flags));

int
#ifdef _USING_PROTOTYPES_
printline (
    FILE *f,
    char *buf,
    char *dir,
    int flags)
#else
printline(f,buf,dir,flags)
    FILE *f;
    char *buf;
    char *dir;
    int flags;
#endif
{
    char *p,*q;
    char path[128];
    unsigned int l;

    p=buf; while (*p=='\t' || *p==' ') p++;
    if (!strncmp(p,"catch",5)) {
	q=p+5; while (*q=='\t' || *q==' ') q++;
	if (*q++=='{') {
	    while (*q=='\t' || *q==' ') q++;
	    if (strncmp(q,"source",6)) q=(char *)NULL;
	} else {
	    q=(char *)NULL;
	}
    } else {
	q=(char *)NULL;
    }
    if (!strncmp(p,"source",6) || q) {
	if (q!=(char *)NULL) {
	    p = q;	    
	}
	p += 6;
	while(*p=='\t' || *p==' ') p++;
	if (*p=='/') {
	    strcpy(path,p);
	} else {
	    strcpy(path,dir);
	    strcat(path,p);
	}
	if (q) {
	    q=strrchr(p,'}');
	    if (q) {
		*q=0;
	    }
	}

	if (!printfile(f, path, dir, flags)) {
	    return 0;
	} else {
	    p = strrchr(p,'/');
	    if (p) {
		strcpy(path,dir);
		strcat(path,p+1);
		if (!printfile(f ,path ,dir, flags)) {
		    return 0;
		}
	    }
	}
	if (q) {
	    *q='}';
	}
    }
    p = buf;
    while ((p = strstr(p, "[list source [file join $dir")) != NULL) {
	q = strstr(p,".tcl]]");
	if (q != NULL) {
	    memcpy(p,"{source -rsrc",13);
	    memcpy(p+13,p+28,q-p-28);
	    memcpy(q-15,"}",1);
	    strcpy(q-14,q+6);
	} else {
	    p++;
	}
    }
    if (array_instead_of_string) {
      fprintf(f, "\nstatic char %s_line%d[] = {\n   ",
         script_name, ++num_lines);
      while (*buf) {
         for (l = 0; *buf && l < 14; l++) {
            fputc('\'', f);
            if (*buf == '\n') { fprintf(f,"\\n',"); buf++; continue; }
            if (*buf == '\'' || *buf == '\\') fputc('\\', f);
            fprintf(f, "%c',", *buf++);
         }
         fprintf(f, "\n   ");
      }
      fprintf(f, "'\\0' };\n");
    } else {
      fputc('\"',f);
      l = strlen(buf);
      if (l>max_buffer) {
	max_buffer = l;
	p = (strchr(buf,'\n'));
	if (p) {
	    l = p - buf;
	} else {
	    l = strlen(buf);
	}
	if (l>72) {l = 72;}
	memcpy(max_buffer_content,buf,l);
	max_buffer_content[l] = 0;
      }
      while(*buf) {
	if (*buf=='\"'||*buf=='\\') fputc('\\',f);
	if (*buf=='\n') {fputc('\\',f); fputc('n',f); fputc('\\',f); }
	fputc(*buf++,f);
      }
      fprintf(f, "\",\n");
    }
    return 0;
}

int
#ifdef _USING_PROTOTYPES_
printfile (
    FILE *fout,
    char *filename,
    char *dir,
    int flags)
#else
printfile(fout,filename,dir, flags)
    FILE *fout;
    char *filename;
    char *dir;
    int flags;
#endif
{
    FILE *fin;
    char *p, *q;
    int c;

    if (!(fin=fopen(filename,"r"))) {
	return 1 /* cannot open file */;
    }
    p = q = buffer;
    while ((c=fgetc(fin))!=EOF) {
	*p = 0;
	if (c=='\n') {
	    if (!strncmp(buffer,"if {[info exists tk_library] && [string compare $tk_library {}]} {",66)) {
		int flag = 1;
		while (((c=fgetc(fin))!=EOF) && flag) {
		    if (c=='{') {
			flag++;
		    } else if (c=='}') {
			flag--;
		    }
		}
		flag=0;
		p=q=buffer;
	    } else if ((p==buffer)||(*q=='\n')||(*q=='#')) {
		if ((*q=='#') && (*(p-1)=='\\')) {
		    p=q+1;
		} else {
		    p=q;
		}
	    } else {
		*p++ = '\n'; *p=0;
		if (Tcl_CommandComplete(buffer)) {
		    p--; *p = 0; printline(fout,buffer,dir,flags);
		    p = q = buffer;
		} else {
		    q=p;
		}
	    }
	} else {
	    *p++ = c;
	}
    }
    if (p!=buffer) {
	*p=0; printline(fout,buffer,dir,flags);
    }
    fclose(fin);
    return 0; /* O.K. */
}

int
#ifdef _USING_PROTOTYPES_
main (
	int argc,
	char *argv[])
#else
main(argc, argv)
	int argc;
	char *argv[];
#endif
{
    FILE *fout;
    char *p,*q, *filename=NULL;
    char dir[128];
    tableitem *t;
    int c,i, flags=0;

    if (argc==1) {
	printf(verbose);
	exit(0);
    }
    if (argc==2&&!strcmp(argv[1],"-help")) {
	printf(verbose);
	printf(help);
	exit(0);
    }
    script_name[0] = 0;
/* parse all command line arguments */
    for (i=1; i<argc; i++) {
	if (!strcmp(argv[i],"-a")) {
	    array_instead_of_string = 1;
	} else if (!strcmp(argv[i],"-n")) {
	    i++; strcpy(script_name,argv[i]);
	} else if (!strcmp(argv[i],"-o")) {
	    i++; filename = argv[i];
	} else if (!strcmp(argv[i],"-index")) {
	    flags = -1;
	} else {
	    for (t=table;t<table+(sizeof(table)/sizeof(tableitem));t++) {
		if (!strcmp(argv[i],t->option)) {
		    flags |= t->flag;
		}
	    }
	}
    }
/* open output file, if not stdout */
    if (filename) {
	fout = fopen(filename,"w");
	if (fout==NULL) {
	    fprintf(stderr,"error opening file %s\n",filename);
	    exit(1);
	}
    } else {
	fout = stdout;
    }
    p = script_name;
    if ((q = strrchr(p,':')) != NULL) {
	p = q+1;
    }
    if ((q = strrchr(p,'/')) != NULL) {
	p = q+1;
    }
    if ((q = strrchr(p,'\\')) != NULL) {
	p = q+1;
    }
    strcpy(script_name,p);
    while ((q = strchr(script_name,'.')) != NULL) {
	*q = '_';
    }
/* create prototypes for all initialization functions that are used */
    if (flags  && (flags != -1)) {
	if (script_name[0] == 0) {
	    strcpy(script_name,"script");
	}
	fprintf(fout, part1);
	for (i=0,c=1;i<(sizeof(table)/sizeof(tableitem));i++,c<<=1) {
	    if (flags & c) {
		fprintf(fout,defineproto1,table[i].package,
			table[i].package);
	    }
	}
	fprintf(fout, part2);
	for (i=1,c=2;i<(sizeof(table)/sizeof(tableitem));i++,c<<=1) {
	    if (flags & c) {
		fprintf(fout,defineproto2,table[i].package);
	    }
	}
	fprintf(fout, part3);
	for (i=1,c=2;i<(sizeof(table)/sizeof(tableitem));i++,c<<=1) {
	    if (flags & c) {
		fprintf(fout,initproto,table[i].package,
			table[i].package,table[i].package);
	    }
	}
	fprintf(fout, part4, script_name);
    }
    if ( !array_instead_of_string && script_name[0]) {
	fprintf(fout, "static char *%s[] = {\n", script_name);
    }
/* handle all remaining arguments */
    if (argc) {argc--; argv++;}
    while(argc) {
	if ((*argv)[0]=='-') {
	    if ((((*argv)[1]=='o')||((*argv)[1]=='n'))&&((*argv)[2]==0)) {
		argc--; argv++;
	    }
	} else if ((p=strstr(*argv,".c"))&&(p[2]==0)) {
	    fprintf(fout,"#include \"%s\"\n",*argv);
	} else {
	    strcpy(dir,*argv);
	    if ((p=strrchr(dir,'/'))!= NULL) { *(p+1)=0; } else {*dir=0;}
	    if (printfile(fout,*argv,dir,flags)) {
		fprintf(stderr,"Error: cannot open file %s\n",*argv);
	    }
	}
	argc--; argv++;
    }
    if ( array_instead_of_string ) {
      fprintf(fout, "static char *%s[] = {\n", script_name);
      for (i = 0; (unsigned int)i < num_lines;)
          fprintf(fout, "%s_line%d,\n", script_name, ++i);
    }
    if (script_name[0]) {
      fprintf(fout, "(char *) NULL\n};\n\n");
    }
/* end of scripts, finally the functions main() and tclAppInit()  */
    if (flags  && (flags != -1)) {
	fprintf(fout, part5, script_name);
	fprintf(fout,callinit,table[0].package);
	for (i=1,c=2;i<(sizeof(table)/sizeof(tableitem));i++,c<<=1) {
	    if (flags & c) {
		fprintf(fout,packageproto,table[i].package,table[i].package,table[i].package);
		fprintf(fout,callinit,table[i].package);
	    }
	}
	p=filename?filename:"app";
	if ((q=strrchr(p,'/')) != NULL) p=q+1;
	if ((q=strchr(p,'.')) != NULL) *q=0;
	if (!*p) p="app";
	fprintf(fout, part6,script_name,p,p);
    }
/* close output-file, if not stdout */
    if (fout!=stdout) {
	fclose(fout);
    }
    if (max_buffer>MAX_STRING_LEN) {
	fprintf(stderr,"warning: largest sting in output file is %d bytes\n\
         many compilers can only handle %d characters in a string\n\
	 first line: %s\n",max_buffer,MAX_STRING_LEN,max_buffer_content);
    }
    exit(0);
    return 0;
}
