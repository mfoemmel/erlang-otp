dnl
dnl aclocal.m4
dnl
dnl Local macros used in configure.in. The Local Macros which
dnl could/should be part of autoconf are prefixed LM_, macros specific
dnl to the Erlang system are prefixed ERL_.
dnl

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
	# couldn't find it anywhere
        AC_DEFINE(HAVE_NO_INADDR_LOOPBACK) ;;
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

dnl ----------------------------------------------------------------------
dnl
dnl ERL_EXTRA_POINTER_BITS
dnl
dnl checks if a constant pattern is present in the top four bits of
dnl pointers to allocated memory. If so, defines EXTRA_POINTER_BITS to
dnl a mask that has to applied to all pointers.
dnl

AC_DEFUN(ERL_EXTRA_POINTER_BITS,
[AC_PROVIDE([$0])
AC_MSG_CHECKING(whether pointers contains extra bits)
AC_CACHE_VAL(erl_cv_extra_pointer_bits,
AC_TRY_RUN([#include <stdio.h>
#if STDC_HEADERS
#  include <stdlib.h>
#endif
#define UNSAFE_MASK  0xf0000000 /* Mask for bits that must be constant */
main()
{
    int i;
    int n;
    void* ptr;
    unsigned long ptr_val;
    FILE *f = fopen("conftestdata", "w");

    if (!f) exit(1);
    ptr_val = ((unsigned long) malloc(1)) & UNSAFE_MASK;
    for (n = 2, i = 1; i < 20; i++) { /* allocate some Meg */
	ptr = (void*) malloc(n);
	if ((((unsigned long) ptr) & UNSAFE_MASK) != ptr_val) {
	    fprintf(f, "variable");
	    exit(0);
	}
	n *= 2;
    }
    if (ptr_val == 0)
	fprintf(f, "no");
    else
	fprintf(f, "0x%8x", ptr_val);
    fclose(f);
    exit(0);
}],
[erl_cv_extra_pointer_bits="`cat conftestdata`"], ,
erl_cv_extra_pointer_bits=no))

case ${erl_cv_extra_pointer_bits} in
   no|variable) erl_extra_pointer_bits_msg=${erl_cv_extra_pointer_bits} ;;
   *)  erl_extra_pointer_bits_msg="yes, (mask is ${erl_cv_extra_pointer_bits})"
       AC_DEFINE_UNQUOTED(EXTRA_POINTER_BITS, ${erl_cv_extra_pointer_bits});;
esac

AC_MSG_RESULT([${erl_extra_pointer_bits_msg}])
])


