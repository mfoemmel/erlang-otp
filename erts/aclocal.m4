dnl
dnl aclocal.m4
dnl
dnl Local macros used in configure.in. The Local Macros which
dnl could/should be part of autoconf are prefixed LM_, macros specific
dnl to the Erlang system are prefixed ERL_.
dnl

dnl ----------------------------------------------------------------------
dnl
dnl LM_FIND_EMU_CC
dnl
dnl
dnl Tries fairly hard to find a C compiler that can handle jump tables.
dnl Defines the @EMU_CC@ variable for the makefiles and 
dnl inserts NO_JUMP_TABLE in the header if one cannot be found...
dnl

AC_DEFUN(LM_FIND_EMU_CC,
	[AC_CACHE_CHECK(for a compiler that handles jumptables,
			ac_cv_prog_emu_cc,
			[
AC_TRY_COMPILE([],[
    __label__ lbl1;
    __label__ lbl2;
    int x = magic();
    static void *jtab[2];

    jtab[0] = &&lbl1;
    jtab[1] = &&lbl2;
    goto *jtab[x];
lbl1:
    return 1;
lbl2:
    return 2;
],ac_cv_prog_emu_cc=$CC,ac_cv_prog_emu_cc=no)

if test $ac_cv_prog_emu_cc = no; then
	for ac_progname in emu_cc.sh gcc; do
  		IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS=":"
  		ac_dummy="$PATH"
  		for ac_dir in $ac_dummy; do
    			test -z "$ac_dir" && ac_dir=.
    			if test -f $ac_dir/$ac_progname; then
      				ac_cv_prog_emu_cc=$ac_dir/$ac_progname
      				break
    			fi
  		done
  		IFS="$ac_save_ifs"
		if test $ac_cv_prog_emu_cc != no; then
			break
		fi
	done
fi

if test $ac_cv_prog_emu_cc != no; then
	save_CC=$CC
	save_CFLAGS=$CFLAGS
	save_CPPFLAGS=$CPPFLAGS
	CC=$ac_cv_prog_emu_cc
	CFLAGS=""
	CPPFLAGS=""
	AC_TRY_COMPILE([],[
    	__label__ lbl1;
    	__label__ lbl2;
    	int x = magic();
    	static void *jtab[2];

    	jtab[0] = &&lbl1;
    	jtab[1] = &&lbl2;
    	goto *jtab[x];
	lbl1:
    	return 1;
	lbl2:
    	return 2;
	],ac_cv_prog_emu_cc=$CC,ac_cv_prog_emu_cc=no)
	CC=$save_CC
	CFLAGS=$save_CFLAGS
	CPPFLAGS=$save_CPPFLAGS
fi
])
if test $ac_cv_prog_emu_cc = no; then
	AC_DEFINE(NO_JUMP_TABLE)
	EMU_CC=$CC
else
	EMU_CC=$ac_cv_prog_emu_cc
fi
AC_SUBST(EMU_CC)
])		
			


dnl ----------------------------------------------------------------------
dnl
dnl LM_PROG_INSTALL_DIR
dnl
dnl Figure out how to create directories with parents.
dnl (In my opinion INSTALL_DIR is a bad name, MKSUBDIRS or something is better)
dnl
dnl We prefer 'install -d', but use 'mkdir -p' if it exists.
dnl If none of these methods works, we give up.

AC_DEFUN(LM_PROG_INSTALL_DIR,
[AC_CACHE_CHECK(how to create a directory including parents,
ac_cv_prog_mkdir_p,
[
temp_name_base=config.$$
temp_name=$temp_name_base/x/y/z
$INSTALL -d $temp_name >/dev/null 2>&1
ac_cv_prog_mkdir_p=none
if test -d $temp_name; then
        ac_cv_prog_mkdir_p="$INSTALL -d"
else
        mkdir -p $temp_name >/dev/null 2>&1
        if test -d $temp_name; then
                ac_cv_prog_mkdir_p="mkdir -p"
        fi
fi
rm -fr $temp_name_base           
])

case "${ac_cv_prog_mkdir_p}" in
  none) AC_MSG_ERROR(don't know how create directories with parents) ;;
  *)    INSTALL_DIR="$ac_cv_prog_mkdir_p" AC_SUBST(INSTALL_DIR)     ;;
esac
])


dnl ----------------------------------------------------------------------
dnl
dnl LM_PROG_PERL5
dnl
dnl Try to find perl version 5. If found set PERL to the absolute path
dnl of the program, if not found set PERL to false.
dnl
dnl On some systems /usr/bin/perl is perl 4 and e.g.
dnl /usr/local/bin/perl is perl 5. We try to handle this case by
dnl putting a couple of 
dnl Tries to handle the case that there are two programs called perl
dnl in the path and one of them is perl 5 and the other isn't. 
dnl
AC_DEFUN(LM_PROG_PERL5,
[AC_PATH_PROGS(PERL, perl5 perl, false,
   /usr/local/bin:/opt/local/bin:/usr/local/gnu/bin:${PATH})
changequote(, )dnl
dnl[ That bracket is needed to balance the right bracket below
if test "$PERL" = "false" || $PERL -e 'exit ($] >= 5)'; then
changequote([, ])dnl
  ac_cv_path_PERL=false
  PERL=false
dnl  AC_MSG_WARN(perl version 5 not found)
fi
])dnl


dnl ----------------------------------------------------------------------
dnl
dnl LM_DECL_SO_BSDCOMPAT
dnl
dnl Check if the system has the SO_BSDCOMPAT flag on sockets (linux) 
dnl
AC_DEFUN(LM_DECL_SO_BSDCOMPAT,
[AC_CACHE_CHECK([for SO_BSDCOMPAT declaration], ac_cv_decl_so_bsdcompat,
AC_TRY_COMPILE([#include <sys/socket.h>], [int i = SO_BSDCOMPAT;],
               ac_cv_decl_so_bsdcompat=yes,
               ac_cv_decl_so_bsdcompat=no))

case "${ac_cv_decl_so_bsdcompat}" in
  "yes" ) AC_DEFINE(HAVE_SO_BSDCOMPAT) ;;
  * ) ;;
esac
])


dnl ----------------------------------------------------------------------
dnl
dnl LM_DECL_INADDR_LOOPBACK
dnl
dnl Try to find declaration of INADDR_LOOPBACK, if nowhere provide a default
dnl

AC_DEFUN(LM_DECL_INADDR_LOOPBACK,
[AC_CACHE_CHECK([for INADDR_LOOPBACK in netinet/in.h],
 ac_cv_decl_inaddr_loopback,
[AC_TRY_COMPILE([#include <sys/types.h>
#include <netinet/in.h>], [int i = INADDR_LOOPBACK;],
ac_cv_decl_inaddr_loopback=yes, ac_cv_decl_inaddr_loopback=no)
])

if test ${ac_cv_decl_inaddr_loopback} = no; then
  AC_CACHE_CHECK([for INADDR_LOOPBACK in rpc/types.h],
                   ac_cv_decl_inaddr_loopback_rpc,
                   AC_TRY_COMPILE([#include <rpc/types.h>],
                                   [int i = INADDR_LOOPBACK;],
                                   ac_cv_decl_inaddr_loopback_rpc=yes,
                                   ac_cv_decl_inaddr_loopback_rpc=no))

   case "${ac_cv_decl_inaddr_loopback_rpc}" in
     "yes" )
        AC_DEFINE(DEF_INADDR_LOOPBACK_IN_RPC_TYPES_H) ;;
      * )
  	AC_CACHE_CHECK([for INADDR_LOOPBACK in winsock2.h],
                   ac_cv_decl_inaddr_loopback_winsock2,
                   AC_TRY_COMPILE([#include <winsock2.h>],
                                   [int i = INADDR_LOOPBACK;],
                                   ac_cv_decl_inaddr_loopback_winsock2=yes,
                                   ac_cv_decl_inaddr_loopback_winsock2=no))
	case "${ac_cv_decl_inaddr_loopback_winsock2}" in
     		"yes" )
			AC_DEFINE(DEF_INADDR_LOOPBACK_IN_WINSOCK2_H) ;;
		* )
			# couldn't find it anywhere
        		AC_DEFINE(HAVE_NO_INADDR_LOOPBACK) ;;
	esac;;
   esac
fi
])


dnl ----------------------------------------------------------------------
dnl
dnl LM_STRUCT_SOCKADDR_SA_LEN
dnl
dnl Check if the sockaddr structure has the field sa_len
dnl

AC_DEFUN(LM_STRUCT_SOCKADDR_SA_LEN,
[AC_CACHE_CHECK([whether struct sockaddr has sa_len field],
                ac_cv_struct_sockaddr_sa_len,
AC_TRY_COMPILE([#include <sys/types.h>
#include <sys/socket.h>], [struct sockaddr s; s.sa_len = 10;],
  ac_cv_struct_sockaddr_sa_len=yes, ac_cv_struct_sockaddr_sa_len=no))

dnl FIXME convbreak
case ${ac_cv_struct_sockaddr_sa_len} in
  "no" ) AC_DEFINE(NO_SA_LEN) ;;
  *) ;;
esac
])

dnl ----------------------------------------------------------------------
dnl
dnl LM_STRUCT_EXCEPTION
dnl
dnl Check to see whether the system supports the matherr function
dnl and its associated type "struct exception".
dnl

AC_DEFUN(LM_STRUCT_EXCEPTION,
[AC_CACHE_CHECK([for struct exception (and matherr function)],
 ac_cv_struct_exception,
AC_TRY_COMPILE([#include <math.h>],
  [struct exception x; x.type = DOMAIN; x.type = SING;],
  ac_cv_struct_exception=yes, ac_cv_struct_exception=no))

case "${ac_cv_struct_exception}" in
  "yes" ) AC_DEFINE(USE_MATHERR) ;;
  *  ) ;;
esac
])


dnl ----------------------------------------------------------------------
dnl
dnl LM_SYS_IPV6
dnl
dnl Check for ipv6 support and what the in6_addr structure is called.
dnl (early linux used in_addr6 insted of in6_addr)
dnl

AC_DEFUN(LM_SYS_IPV6,
[AC_MSG_CHECKING(for IP version 6 support)
AC_CACHE_VAL(ac_cv_sys_ipv6_support,
[ok_so_far=yes
 AC_TRY_COMPILE([#include <sys/types.h>
#include <netinet/in.h>],
   [struct in6_addr a6; struct sockaddr_in6 s6;], ok_so_far=yes, ok_so_far=no)

if test $ok_so_far = yes; then
  ac_cv_sys_ipv6_support=yes
else
  AC_TRY_COMPILE([#include <sys/types.h>
#include <netinet/in.h>],
    [struct in_addr6 a6; struct sockaddr_in6 s6;],
    ac_cv_sys_ipv6_support=in_addr6, ac_cv_sys_ipv6_support=no)
fi
])dnl

case ${ac_cv_sys_ipv6_support} in
  yes)
    AC_MSG_RESULT(yes)
    AC_DEFINE(HAVE_IN6)
    ;;
  in_addr6)
    AC_MSG_RESULT([yes (but I am redefining in_addr6 to in6_addr)])
    AC_DEFINE(HAVE_IN6)
    AC_DEFINE(HAVE_IN_ADDR6_STRUCT)
    ;;
  *)
    AC_MSG_RESULT(no)
    ;;
esac
])


dnl ----------------------------------------------------------------------
dnl
dnl LM_SYS_MULTICAST
dnl
dnl Check for multicast support. Only checks for multicast options in
dnl setsockopt(), no check is performed that multicasting actually works.
dnl If options are found defines HAVE_MULTICAST_SUPPORT
dnl

AC_DEFUN(LM_SYS_MULTICAST,
[AC_CACHE_CHECK([for multicast support], ac_cv_sys_multicast_support,
[AC_EGREP_CPP(yes,
[#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#if defined(IP_MULTICAST_TTL) && defined(IP_MULTICAST_LOOP) && defined(IP_MULTICAST_IF) && defined(IP_ADD_MEMBERSHIP) && defined(IP_DROP_MEMBERSHIP)
yes
#endif
], ac_cv_sys_multicast_support=yes, ac_cv_sys_multicast_support=no)])
if test $ac_cv_sys_multicast_support = yes; then
  AC_DEFINE(HAVE_MULTICAST_SUPPORT)
fi
])dnl


dnl ----------------------------------------------------------------------
dnl
dnl LM_DECL_SYS_ERRLIST
dnl
dnl Define SYS_ERRLIST_DECLARED if the variable sys_errlist is declared
dnl in a system header file, stdio.h or errno.h.
dnl

AC_DEFUN(LM_DECL_SYS_ERRLIST,
[AC_CACHE_CHECK([for sys_errlist declaration in stdio.h or errno.h],
  ac_cv_decl_sys_errlist,
[AC_TRY_COMPILE([#include <stdio.h>
#include <errno.h>], [char *msg = *(sys_errlist + 1);],
  ac_cv_decl_sys_errlist=yes, ac_cv_decl_sys_errlist=no)])
if test $ac_cv_decl_sys_errlist = yes; then
  AC_DEFINE(SYS_ERRLIST_DECLARED)
fi
])


dnl ----------------------------------------------------------------------
dnl
dnl LM_CHECK_FUNC_DECL( funname, declaration [, extra includes 
dnl                     [, action-if-found [, action-if-not-found]]] )
dnl
dnl Checks if the declaration "declaration" of "funname" conflicts
dnl with the header files idea of how the function should be
dnl declared. It is useful on systems which lack prototypes and you
dnl need to provide your own (e.g. when you want to take the address
dnl of a function). If the declaration conflicts define the
dnl preprocessor symbol HAVE_CONFLICTING_funname_DECLARATION is
dnl defined.
dnl
dnl XXX Sigh. It doesn't work together with autoheader (which has
dnl special definitions for the other _CHECK_ macros).
dnl

AC_DEFUN(LM_CHECK_FUNC_DECL,
[AC_MSG_CHECKING([for conflicting declaration of $1])
AC_CACHE_VAL(ac_cv_func_decl_$1,
[AC_TRY_COMPILE([#include <stdio.h>
$3],[$2
char *c = (char *)$1;
], eval "ac_cv_func_decl_$1=no", eval "ac_cv_func_decl_$1=yes")])
if eval "test \"`echo '$ac_cv_func_decl_'$1`\" = yes"; then
  AC_MSG_RESULT(yes)
changequote(, )dnl
  ac_tr_func=HAVE_CONFLICTING_`echo $1 | tr 'abcdefghijklmnopqrstuvwxyz' 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'`_DECLARATION
changequote([, ])dnl
  AC_DEFINE_UNQUOTED($ac_tr_func)
  ifelse([$4], , :, [$4])
else
  AC_MSG_RESULT(no)
ifelse([$5], , , [$5
])dnl
fi
])


dnl ----------------------------------------------------------------------
dnl
dnl LM_FIND_ETHR_LIB
dnl
dnl Find a thread library to use. Sets ETHR_LIBS to libraries to link
dnl with, ETHR_X_LIBS to extra libraries to link with (same as ETHR_LIBS
dnl except that the ethread lib itself is not included), ETHR_DEFS to
dnl defines to compile with, ETHR_THR_LIB_BASE to the name of the
dnl thread library which the ethread library is based on, and ETHR_LIB_NAME
dnl to the name of the library where the ethread implementation is located.
dnl  LM_FIND_ETHR_LIB currently searches for 'pthreads', and
dnl 'win32_threads'. If no thread library was found ETHR_LIBS, ETHR_X_LIBS,
dnl ETHR_DEFS, ETHR_THR_LIB_BASE, and ETHR_LIB_NAME are all set to the
dnl empty string.
dnl

AC_DEFUN(LM_FIND_ETHR_LIB,
[

dnl Name of lib where ethread implementation is located
ethr_lib_name=ethread

ETHR_THR_LIB_BASE=
ETHR_X_LIBS=
ETHR_LIBS=
ETHR_LIB_NAME=
ETHR_DEFS=

dnl if test "x$host_os" = "x"; then
dnl    AC_CANONICAL_HOST
dnl fi

dnl win32?
AC_MSG_CHECKING([for native win32 threads])
if test "X$host_os" = "Xwin32"; then
    ETHR_X_LIBS=
    ETHR_DEFS=
    ETHR_THR_LIB_BASE=win32_threads
    AC_DEFINE(ETHR_WIN32_THREADS, 1, [Define if you have win32 threads])
    AC_MSG_RESULT(yes)
else
    AC_MSG_RESULT(no)

dnl Try to find POSIX threads

dnl The usual pthread lib...
    AC_CHECK_LIB(pthread, pthread_create, ETHR_X_LIBS="-lpthread")

dnl FreeBSD has pthreads in special c library, c_r...
    if test "x$ETHR_X_LIBS" = "x"; then
	AC_CHECK_LIB(c_r, pthread_create, ETHR_X_LIBS="-lc_r")
    fi

dnl On ofs1 the '-pthread' switch should be used
    if test "x$ETHR_X_LIBS" = "x"; then
	AC_MSG_CHECKING([if the '-pthread' switch can be used])
	saved_cflags=$CFLAGS
	CFLAGS="$CFLAGS -pthread"
	AC_TRY_LINK([#include <pthread.h>],
		    pthread_create((void*)0,(void*)0,(void*)0,(void*)0);,
		    [ETHR_DEFS="-pthread"
		     ETHR_X_LIBS="-pthread"])
	CFLAGS=$saved_cflags
	if test "x$ETHR_X_LIBS" != "x"; then
	    AC_MSG_RESULT(yes)
	else
	    AC_MSG_RESULT(no)
	fi
    fi

    if test "x$ETHR_X_LIBS" != "x"; then
	ETHR_DEFS="$ETHR_DEFS -D_THREAD_SAFE -D_REENTRANT"
	ETHR_THR_LIB_BASE=pthread
    	AC_DEFINE(ETHR_PTHREADS, 1, [Define if you have pthreads])
	case $host_os in
	    solaris*)
		ETHR_DEFS="$ETHR_DEFS -D_POSIX_PTHREAD_SEMANTICS" ;;
	    linux*)
		ETHR_DEFS="$ETHR_DEFS -D_GNU_SOURCE"
		linux_kernel_vsn_=`uname -r` # FIXME: for cross compilation.

		usable_sigusrx=no
		usable_sigaltstack=no

		# FIXME: Test for actual problems instead of kernel versions.
		case $linux_kernel_vsn_ in
		    0.*|1.*|2.0|2.0.*|2.1|2.1.*)
			;;
		    2.2|2.2.*|2.3|2.3.*)
			usable_sigusrx=yes
			;;
		    *)
			usable_sigusrx=yes
			usable_sigaltstack=yes
			;;
		esac

		AC_MSG_CHECKING(if SIGUSR1 and SIGUSR2 can be used)
		AC_MSG_RESULT($usable_sigusrx)
		if test $usable_sigusrx = no; then
		    ETHR_DEFS="$ETHR_DEFS -DETHR_UNUSABLE_SIGUSRX"
		fi

		AC_MSG_CHECKING(if sigaltstack can be used)
		AC_MSG_RESULT($usable_sigaltstack)
		if test $usable_sigaltstack = no; then
		    ETHR_DEFS="$ETHR_DEFS -DETHR_UNUSABLE_SIGALTSTACK"
		fi

		AC_DEFINE(ETHR_INIT_MUTEX_IN_CHILD_AT_FORK, 1, \
[Define if mutexes should be reinitialized (instead of unlocked) in child at fork.]) ;;
	    *) ;;
	esac

	dnl We sometimes need ETHR_DEFS in order to find certain headers
	dnl (at least for pthread.h on osf1).
	saved_cppflags=$CPPFLAGS
	CPPFLAGS="$CPPFLAGS $ETHR_DEFS"

	dnl We need the thread library in order to find some functions
	saved_libs=$LIBS
	LIBS="$LIBS $ETHR_X_LIBS"



	dnl
	dnl Check for headers
	dnl

	AC_CHECK_HEADER(pthread.h,
			AC_DEFINE(ETHR_HAVE_PTHREAD_H, 1, \
[Define if you have the <pthread.h> header file.]))

	dnl Some Linuxes have <pthread/mit/pthread.h> instead of <pthread.h>
	AC_CHECK_HEADER(pthread/mit/pthread.h, \
			AC_DEFINE(ETHR_HAVE_MIT_PTHREAD_H, 1, \
[Define if the pthread.h header file is in pthread/mit directory.]))

	AC_CHECK_HEADER(sys/time.h, \
			AC_DEFINE(ETHR_HAVE_SYS_TIME_H, 1, \
[Define if you have the <sys/time.h> header file.]))

	AC_TRY_COMPILE([#include <time.h>
			#include <sys/time.h>], 
			[struct timeval *tv; return 0;],
			AC_DEFINE(ETHR_TIME_WITH_SYS_TIME, 1, \
[Define if you can safely include both <sys/time.h> and <time.h>.]))


	dnl
	dnl Check for functions
	dnl

	AC_CHECK_FUNC(pthread_atfork, \
			AC_DEFINE(ETHR_HAVE_PTHREAD_ATFORK, 1, \
[Define if you have the pthread_atfork function.]))
	AC_CHECK_FUNC(pthread_mutexattr_settype, \
			AC_DEFINE(ETHR_HAVE_PTHREAD_MUTEXATTR_SETTYPE, 1, \
[Define if you have the pthread_mutexattr_settype function.]))
	AC_CHECK_FUNC(pthread_mutexattr_setkind_np, \
			AC_DEFINE(ETHR_HAVE_PTHREAD_MUTEXATTR_SETKIND_NP, 1, \
[Define if you have the pthread_mutexattr_setkind_np function.]))



	dnl Restore LIBS
	LIBS=$saved_libs
	dnl restore CPPFLAGS
	CPPFLAGS=$saved_cppflags

    fi
fi

if test "x$ETHR_THR_LIB_BASE" != "x"; then
	ETHR_LIBS="-l$ethr_lib_name $ETHR_X_LIBS"
	ETHR_LIB_NAME=$ethr_lib_name
fi

AC_DEFINE(ETHR_HAVE_ETHREAD_DEFINES, 1, \
[Define if you have all ethread defines])

AC_SUBST(ETHR_X_LIBS)
AC_SUBST(ETHR_LIBS)
AC_SUBST(ETHR_LIB_NAME)
AC_SUBST(ETHR_DEFS)
AC_SUBST(ETHR_THR_LIB_BASE)

])



dnl ----------------------------------------------------------------------
dnl
dnl ERL_TIME_CORRECTION
dnl
dnl In the presence of a high resolution realtime timer Erlang can adapt
dnl its view of time relative to this timer. On solaris such a timer is
dnl available with the syscall gethrtime(). On other OS's a fallback
dnl solution using times() is implemented. (However on e.g. FreeBSD times()
dnl is implemented using gettimeofday so it doesn't make much sense to
dnl use it there...) On second thought, it seems to be safer to do it the
dnl other way around. I.e. only use times() on OS's where we know it will
dnl work...
dnl

AC_DEFUN(ERL_TIME_CORRECTION,
[if test x$ac_cv_func_gethrtime = x; then
  AC_CHECK_FUNC(gethrtime)
fi
AC_CACHE_CHECK([how to correct for time adjustments], erl_cv_time_correction,
[case $ac_cv_func_gethrtime in
  yes)
    erl_cv_time_correction=hrtime ;;
  no)
    case $host_os in
      linux*)
        erl_cv_time_correction=times ;;
      *)
        erl_cv_time_correction=none ;;
    esac
    ;;
esac
])
case $erl_cv_time_correction in
  times)
    AC_DEFINE(CORRECT_USING_TIMES)
    ;;
esac
])dnl

dnl ERL_TRY_LINK_JAVA(CLASSES, FUNCTION-BODY
dnl                   [ACTION_IF_FOUND [, ACTION-IF-NOT-FOUND]])
dnl Freely inspired by AC_TRY_LINK. (Maybe better to create a 
dnl AC_LANG_JAVA instead...)
AC_DEFUN(ERL_TRY_LINK_JAVA,
[java_link='$JAVAC conftest.java 1>&AC_FD_CC'
changequote(«, »)dnl
cat > conftest.java <<EOF
«$1»
class conftest { public static void main(String[] args) {
   «$2»
   ; return; }}
EOF
changequote([, ])dnl
if AC_TRY_EVAL(java_link) && test -s conftest.class; then
   ifelse([$3], , :, [rm -rf conftest*
   $3])
else
   echo "configure: failed program was:" 1>&AC_FD_CC
   cat conftest.java 1>&AC_FD_CC
   echo "configure: PATH was $PATH" 1>&AC_FD_CC
ifelse([$4], , , [  rm -rf conftest*
  $4
])dnl
fi
rm -f conftest*])
#define UNSAFE_MASK  0xc0000000 /* Mask for bits that must be constant */


