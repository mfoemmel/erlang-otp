dnl X # generated automatically by aclocal 1.7.1 -*- Autoconf -*-
dnl X 
dnl X # Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002
dnl X # Free Software Foundation, Inc.
dnl X # This file is free software; the Free Software Foundation
dnl X # gives unlimited permission to copy and/or distribute it,
dnl X # with or without modifications, as long as this notice is preserved.
dnl X 
dnl X # This program is distributed in the hope that it will be useful,
dnl X # but WITHOUT ANY WARRANTY, to the extent permitted by law; without
dnl X # even the implied warranty of MERCHANTABILITY or FITNESS FOR A
dnl X # PARTICULAR PURPOSE.
dnl X 
dnl X # Do all the work for Automake.                            -*- Autoconf -*-
dnl X 
dnl X # This macro actually does too much some checks are only needed if
dnl X # your package does certain things.  But this isn't really a big deal.
dnl X 
dnl X # Copyright 1996, 1997, 1998, 1999, 2000, 2001, 2002
dnl X # Free Software Foundation, Inc.
dnl X 
dnl X # This program is free software; you can redistribute it and/or modify
dnl X # it under the terms of the GNU General Public License as published by
dnl X # the Free Software Foundation; either version 2, or (at your option)
dnl X # any later version.
dnl X 
dnl X # This program is distributed in the hope that it will be useful,
dnl X # but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl X # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl X # GNU General Public License for more details.
dnl X 
dnl X # You should have received a copy of the GNU General Public License
dnl X # along with this program; if not, write to the Free Software
dnl X # Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
dnl X # 02111-1307, USA.
dnl X 
dnl X # serial 8
dnl X 
dnl X # There are a few dirty hacks below to avoid letting `AC_PROG_CC' be
dnl X # written in clear, in which case automake, when reading aclocal.m4,
dnl X # will think it sees a *use*, and therefore will trigger all it's
dnl X # C support machinery.  Also note that it means that autoscan, seeing
dnl X # CC etc. in the Makefile, will ask for an AC_PROG_CC use...
dnl X 
dnl X 
dnl X AC_PREREQ([2.54])
dnl X 
dnl X # Autoconf 2.50 wants to disallow AM_ names.  We explicitly allow
dnl X # the ones we care about.
dnl X m4_pattern_allow([^AM_[A-Z]+FLAGS$])dnl
dnl X 
dnl X # AM_INIT_AUTOMAKE(PACKAGE, VERSION, [NO-DEFINE])
dnl X # AM_INIT_AUTOMAKE([OPTIONS])
dnl X # -----------------------------------------------
dnl X # The call with PACKAGE and VERSION arguments is the old style
dnl X # call (pre autoconf-2.50), which is being phased out.  PACKAGE
dnl X # and VERSION should now be passed to AC_INIT and removed from
dnl X # the call to AM_INIT_AUTOMAKE.
dnl X # We support both call styles for the transition.  After
dnl X # the next Automake release, Autoconf can make the AC_INIT
dnl X # arguments mandatory, and then we can depend on a new Autoconf
dnl X # release and drop the old call support.
dnl X AC_DEFUN([AM_INIT_AUTOMAKE],
dnl X [AC_REQUIRE([AM_SET_CURRENT_AUTOMAKE_VERSION])dnl
dnl X  AC_REQUIRE([AC_PROG_INSTALL])dnl
dnl X # test to see if srcdir already configured
dnl X if test "`cd $srcdir && pwd`" != "`pwd`" &&
dnl X    test -f $srcdir/config.status; then
dnl X   AC_MSG_ERROR([source directory already configured; run "make distclean" there first])
dnl X fi
dnl X 
dnl X # test whether we have cygpath
dnl X if test -z "$CYGPATH_W"; then
dnl X   if (cygpath --version) >/dev/null 2>/dev/null; then
dnl X     CYGPATH_W='cygpath -w'
dnl X   else
dnl X     CYGPATH_W=echo
dnl X   fi
dnl X fi
dnl X AC_SUBST([CYGPATH_W])
dnl X 
dnl X # Define the identity of the package.
dnl X dnl Distinguish between old-style and new-style calls.
dnl X m4_ifval([$2],
dnl X [m4_ifval([$3], [_AM_SET_OPTION([no-define])])dnl
dnl X  AC_SUBST([PACKAGE], [$1])dnl
dnl X  AC_SUBST([VERSION], [$2])],
dnl X [_AM_SET_OPTIONS([$1])dnl
dnl X  AC_SUBST([PACKAGE], [AC_PACKAGE_TARNAME])dnl
dnl X  AC_SUBST([VERSION], [AC_PACKAGE_VERSION])])dnl
dnl X 
dnl X _AM_IF_OPTION([no-define],,
dnl X [AC_DEFINE_UNQUOTED(PACKAGE, "$PACKAGE", [Name of package])
dnl X  AC_DEFINE_UNQUOTED(VERSION, "$VERSION", [Version number of package])])dnl
dnl X 
dnl X # Some tools Automake needs.
dnl X AC_REQUIRE([AM_SANITY_CHECK])dnl
dnl X AC_REQUIRE([AC_ARG_PROGRAM])dnl
dnl X AM_MISSING_PROG(ACLOCAL, aclocal-${am__api_version})
dnl X AM_MISSING_PROG(AUTOCONF, autoconf)
dnl X AM_MISSING_PROG(AUTOMAKE, automake-${am__api_version})
dnl X AM_MISSING_PROG(AUTOHEADER, autoheader)
dnl X AM_MISSING_PROG(MAKEINFO, makeinfo)
dnl X AM_MISSING_PROG(AMTAR, tar)
dnl X AM_PROG_INSTALL_SH
dnl X AM_PROG_INSTALL_STRIP
dnl X # We need awk for the "check" target.  The system "awk" is bad on
dnl X # some platforms.
dnl X AC_REQUIRE([AC_PROG_AWK])dnl
dnl X AC_REQUIRE([AC_PROG_MAKE_SET])dnl
dnl X 
dnl X _AM_IF_OPTION([no-dependencies],,
dnl X [AC_PROVIDE_IFELSE([AC_PROG_CC],
dnl X                   [_AM_DEPENDENCIES(CC)],
dnl X                   [define([AC_PROG_CC],
dnl X                           defn([AC_PROG_CC])[_AM_DEPENDENCIES(CC)])])dnl
dnl X AC_PROVIDE_IFELSE([AC_PROG_CXX],
dnl X                   [_AM_DEPENDENCIES(CXX)],
dnl X                   [define([AC_PROG_CXX],
dnl X                           defn([AC_PROG_CXX])[_AM_DEPENDENCIES(CXX)])])dnl
dnl X ])
dnl X ])
dnl X 
dnl X 
dnl X # When config.status generates a header, we must update the stamp-h file.
dnl X # This file resides in the same directory as the config header
dnl X # that is generated.  The stamp files are numbered to have different names.
dnl X 
dnl X # Autoconf calls _AC_AM_CONFIG_HEADER_HOOK (when defined) in the
dnl X # loop where config.status creates the headers, so we can generate
dnl X # our stamp files there.
dnl X AC_DEFUN([_AC_AM_CONFIG_HEADER_HOOK],
dnl X [_am_stamp_count=`expr ${_am_stamp_count-0} + 1`
dnl X echo "timestamp for $1" >`AS_DIRNAME([$1])`/stamp-h[]$_am_stamp_count])
dnl X 
dnl X # Copyright 2002  Free Software Foundation, Inc.
dnl X 
dnl X # This program is free software; you can redistribute it and/or modify
dnl X # it under the terms of the GNU General Public License as published by
dnl X # the Free Software Foundation; either version 2, or (at your option)
dnl X # any later version.
dnl X 
dnl X # This program is distributed in the hope that it will be useful,
dnl X # but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl X # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl X # GNU General Public License for more details.
dnl X 
dnl X # You should have received a copy of the GNU General Public License
dnl X # along with this program; if not, write to the Free Software
dnl X # Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
dnl X 
dnl X # AM_AUTOMAKE_VERSION(VERSION)
dnl X # ----------------------------
dnl X # Automake X.Y traces this macro to ensure aclocal.m4 has been
dnl X # generated from the m4 files accompanying Automake X.Y.
dnl X AC_DEFUN([AM_AUTOMAKE_VERSION],[am__api_version="1.7"])
dnl X 
dnl X # AM_SET_CURRENT_AUTOMAKE_VERSION
dnl X # -------------------------------
dnl X # Call AM_AUTOMAKE_VERSION so it can be traced.
dnl X # This function is AC_REQUIREd by AC_INIT_AUTOMAKE.
dnl X AC_DEFUN([AM_SET_CURRENT_AUTOMAKE_VERSION],
dnl X 	 [AM_AUTOMAKE_VERSION([1.7.1])])
dnl X 
dnl X # Helper functions for option handling.                    -*- Autoconf -*-
dnl X 
dnl X # Copyright 2001, 2002  Free Software Foundation, Inc.
dnl X 
dnl X # This program is free software; you can redistribute it and/or modify
dnl X # it under the terms of the GNU General Public License as published by
dnl X # the Free Software Foundation; either version 2, or (at your option)
dnl X # any later version.
dnl X 
dnl X # This program is distributed in the hope that it will be useful,
dnl X # but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl X # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl X # GNU General Public License for more details.
dnl X 
dnl X # You should have received a copy of the GNU General Public License
dnl X # along with this program; if not, write to the Free Software
dnl X # Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
dnl X # 02111-1307, USA.
dnl X 
dnl X # serial 2
dnl X 
dnl X # _AM_MANGLE_OPTION(NAME)
dnl X # -----------------------
dnl X AC_DEFUN([_AM_MANGLE_OPTION],
dnl X [[_AM_OPTION_]m4_bpatsubst($1, [[^a-zA-Z0-9_]], [_])])
dnl X 
dnl X # _AM_SET_OPTION(NAME)
dnl X # ------------------------------
dnl X # Set option NAME.  Presently that only means defining a flag for this option.
dnl X AC_DEFUN([_AM_SET_OPTION],
dnl X [m4_define(_AM_MANGLE_OPTION([$1]), 1)])
dnl X 
dnl X # _AM_SET_OPTIONS(OPTIONS)
dnl X # ----------------------------------
dnl X # OPTIONS is a space-separated list of Automake options.
dnl X AC_DEFUN([_AM_SET_OPTIONS],
dnl X [AC_FOREACH([_AM_Option], [$1], [_AM_SET_OPTION(_AM_Option)])])
dnl X 
dnl X # _AM_IF_OPTION(OPTION, IF-SET, [IF-NOT-SET])
dnl X # -------------------------------------------
dnl X # Execute IF-SET if OPTION is set, IF-NOT-SET otherwise.
dnl X AC_DEFUN([_AM_IF_OPTION],
dnl X [m4_ifset(_AM_MANGLE_OPTION([$1]), [$2], [$3])])
dnl X 
dnl X #
dnl X # Check to make sure that the build environment is sane.
dnl X #
dnl X 
dnl X # Copyright 1996, 1997, 2000, 2001 Free Software Foundation, Inc.
dnl X 
dnl X # This program is free software; you can redistribute it and/or modify
dnl X # it under the terms of the GNU General Public License as published by
dnl X # the Free Software Foundation; either version 2, or (at your option)
dnl X # any later version.
dnl X 
dnl X # This program is distributed in the hope that it will be useful,
dnl X # but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl X # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl X # GNU General Public License for more details.
dnl X 
dnl X # You should have received a copy of the GNU General Public License
dnl X # along with this program; if not, write to the Free Software
dnl X # Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
dnl X # 02111-1307, USA.
dnl X 
dnl X # serial 3
dnl X 
dnl X # AM_SANITY_CHECK
dnl X # ---------------
dnl X AC_DEFUN([AM_SANITY_CHECK],
dnl X [AC_MSG_CHECKING([whether build environment is sane])
dnl X # Just in case
dnl X sleep 1
dnl X echo timestamp > conftest.file
dnl X # Do `set' in a subshell so we don't clobber the current shell's
dnl X # arguments.  Must try -L first in case configure is actually a
dnl X # symlink; some systems play weird games with the mod time of symlinks
dnl X # (eg FreeBSD returns the mod time of the symlink's containing
dnl X # directory).
dnl X if (
dnl X    set X `ls -Lt $srcdir/configure conftest.file 2> /dev/null`
dnl X    if test "$[*]" = "X"; then
dnl X       # -L didn't work.
dnl X       set X `ls -t $srcdir/configure conftest.file`
dnl X    fi
dnl X    rm -f conftest.file
dnl X    if test "$[*]" != "X $srcdir/configure conftest.file" \
dnl X       && test "$[*]" != "X conftest.file $srcdir/configure"; then
dnl X 
dnl X       # If neither matched, then we have a broken ls.  This can happen
dnl X       # if, for instance, CONFIG_SHELL is bash and it inherits a
dnl X       # broken ls alias from the environment.  This has actually
dnl X       # happened.  Such a system could not be considered "sane".
dnl X       AC_MSG_ERROR([ls -t appears to fail.  Make sure there is not a broken
dnl X alias in your environment])
dnl X    fi
dnl X 
dnl X    test "$[2]" = conftest.file
dnl X    )
dnl X then
dnl X    # Ok.
dnl X    :
dnl X else
dnl X    AC_MSG_ERROR([newly created file is older than distributed files!
dnl X Check your system clock])
dnl X fi
dnl X AC_MSG_RESULT(yes)])
dnl X 
dnl X #  -*- Autoconf -*-
dnl X 
dnl X 
dnl X # Copyright 1997, 1999, 2000, 2001 Free Software Foundation, Inc.
dnl X 
dnl X # This program is free software; you can redistribute it and/or modify
dnl X # it under the terms of the GNU General Public License as published by
dnl X # the Free Software Foundation; either version 2, or (at your option)
dnl X # any later version.
dnl X 
dnl X # This program is distributed in the hope that it will be useful,
dnl X # but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl X # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl X # GNU General Public License for more details.
dnl X 
dnl X # You should have received a copy of the GNU General Public License
dnl X # along with this program; if not, write to the Free Software
dnl X # Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
dnl X # 02111-1307, USA.
dnl X 
dnl X # serial 3
dnl X 
dnl X # AM_MISSING_PROG(NAME, PROGRAM)
dnl X # ------------------------------
dnl X AC_DEFUN([AM_MISSING_PROG],
dnl X [AC_REQUIRE([AM_MISSING_HAS_RUN])
dnl X $1=${$1-"${am_missing_run}$2"}
dnl X AC_SUBST($1)])
dnl X 
dnl X 
dnl X # AM_MISSING_HAS_RUN
dnl X # ------------------
dnl X # Define MISSING if not defined so far and test if it supports --run.
dnl X # If it does, set am_missing_run to use it, otherwise, to nothing.
dnl X AC_DEFUN([AM_MISSING_HAS_RUN],
dnl X [AC_REQUIRE([AM_AUX_DIR_EXPAND])dnl
dnl X test x"${MISSING+set}" = xset || MISSING="\${SHELL} $am_aux_dir/missing"
dnl X # Use eval to expand $SHELL
dnl X if eval "$MISSING --run true"; then
dnl X   am_missing_run="$MISSING --run "
dnl X else
dnl X   am_missing_run=
dnl X   AC_MSG_WARN([`missing' script is too old or missing])
dnl X fi
dnl X ])
dnl X 
dnl X # AM_AUX_DIR_EXPAND
dnl X 
dnl X # Copyright 2001 Free Software Foundation, Inc.
dnl X 
dnl X # This program is free software; you can redistribute it and/or modify
dnl X # it under the terms of the GNU General Public License as published by
dnl X # the Free Software Foundation; either version 2, or (at your option)
dnl X # any later version.
dnl X 
dnl X # This program is distributed in the hope that it will be useful,
dnl X # but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl X # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl X # GNU General Public License for more details.
dnl X 
dnl X # You should have received a copy of the GNU General Public License
dnl X # along with this program; if not, write to the Free Software
dnl X # Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
dnl X # 02111-1307, USA.
dnl X 
dnl X # For projects using AC_CONFIG_AUX_DIR([foo]), Autoconf sets
dnl X # $ac_aux_dir to `$srcdir/foo'.  In other projects, it is set to
dnl X # `$srcdir', `$srcdir/..', or `$srcdir/../..'.
dnl X #
dnl X # Of course, Automake must honor this variable whenever it calls a
dnl X # tool from the auxiliary directory.  The problem is that $srcdir (and
dnl X # therefore $ac_aux_dir as well) can be either absolute or relative,
dnl X # depending on how configure is run.  This is pretty annoying, since
dnl X # it makes $ac_aux_dir quite unusable in subdirectories: in the top
dnl X # source directory, any form will work fine, but in subdirectories a
dnl X # relative path needs to be adjusted first.
dnl X #
dnl X # $ac_aux_dir/missing
dnl X #    fails when called from a subdirectory if $ac_aux_dir is relative
dnl X # $top_srcdir/$ac_aux_dir/missing
dnl X #    fails if $ac_aux_dir is absolute,
dnl X #    fails when called from a subdirectory in a VPATH build with
dnl X #          a relative $ac_aux_dir
dnl X #
dnl X # The reason of the latter failure is that $top_srcdir and $ac_aux_dir
dnl X # are both prefixed by $srcdir.  In an in-source build this is usually
dnl X # harmless because $srcdir is `.', but things will broke when you
dnl X # start a VPATH build or use an absolute $srcdir.
dnl X #
dnl X # So we could use something similar to $top_srcdir/$ac_aux_dir/missing,
dnl X # iff we strip the leading $srcdir from $ac_aux_dir.  That would be:
dnl X #   am_aux_dir='\$(top_srcdir)/'`expr "$ac_aux_dir" : "$srcdir//*\(.*\)"`
dnl X # and then we would define $MISSING as
dnl X #   MISSING="\${SHELL} $am_aux_dir/missing"
dnl X # This will work as long as MISSING is not called from configure, because
dnl X # unfortunately $(top_srcdir) has no meaning in configure.
dnl X # However there are other variables, like CC, which are often used in
dnl X # configure, and could therefore not use this "fixed" $ac_aux_dir.
dnl X #
dnl X # Another solution, used here, is to always expand $ac_aux_dir to an
dnl X # absolute PATH.  The drawback is that using absolute paths prevent a
dnl X # configured tree to be moved without reconfiguration.
dnl X 
dnl X # Rely on autoconf to set up CDPATH properly.
dnl X AC_PREREQ([2.50])
dnl X 
dnl X AC_DEFUN([AM_AUX_DIR_EXPAND], [
dnl X # expand $ac_aux_dir to an absolute path
dnl X am_aux_dir=`cd $ac_aux_dir && pwd`
dnl X ])
dnl X 
dnl X # AM_PROG_INSTALL_SH
dnl X # ------------------
dnl X # Define $install_sh.
dnl X 
dnl X # Copyright 2001 Free Software Foundation, Inc.
dnl X 
dnl X # This program is free software; you can redistribute it and/or modify
dnl X # it under the terms of the GNU General Public License as published by
dnl X # the Free Software Foundation; either version 2, or (at your option)
dnl X # any later version.
dnl X 
dnl X # This program is distributed in the hope that it will be useful,
dnl X # but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl X # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl X # GNU General Public License for more details.
dnl X 
dnl X # You should have received a copy of the GNU General Public License
dnl X # along with this program; if not, write to the Free Software
dnl X # Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
dnl X # 02111-1307, USA.
dnl X 
dnl X AC_DEFUN([AM_PROG_INSTALL_SH],
dnl X [AC_REQUIRE([AM_AUX_DIR_EXPAND])dnl
dnl X install_sh=${install_sh-"$am_aux_dir/install-sh"}
dnl X AC_SUBST(install_sh)])
dnl X 
dnl X # AM_PROG_INSTALL_STRIP
dnl X 
dnl X # Copyright 2001 Free Software Foundation, Inc.
dnl X 
dnl X # This program is free software; you can redistribute it and/or modify
dnl X # it under the terms of the GNU General Public License as published by
dnl X # the Free Software Foundation; either version 2, or (at your option)
dnl X # any later version.
dnl X 
dnl X # This program is distributed in the hope that it will be useful,
dnl X # but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl X # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl X # GNU General Public License for more details.
dnl X 
dnl X # You should have received a copy of the GNU General Public License
dnl X # along with this program; if not, write to the Free Software
dnl X # Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
dnl X # 02111-1307, USA.
dnl X 
dnl X # One issue with vendor `install' (even GNU) is that you can't
dnl X # specify the program used to strip binaries.  This is especially
dnl X # annoying in cross-compiling environments, where the build's strip
dnl X # is unlikely to handle the host's binaries.
dnl X # Fortunately install-sh will honor a STRIPPROG variable, so we
dnl X # always use install-sh in `make install-strip', and initialize
dnl X # STRIPPROG with the value of the STRIP variable (set by the user).
dnl X AC_DEFUN([AM_PROG_INSTALL_STRIP],
dnl X [AC_REQUIRE([AM_PROG_INSTALL_SH])dnl
dnl X # Installed binaries are usually stripped using `strip' when the user
dnl X # run `make install-strip'.  However `strip' might not be the right
dnl X # tool to use in cross-compilation environments, therefore Automake
dnl X # will honor the `STRIP' environment variable to overrule this program.
dnl X dnl Don't test for $cross_compiling = yes, because it might be `maybe'.
dnl X if test "$cross_compiling" != no; then
dnl X   AC_CHECK_TOOL([STRIP], [strip], :)
dnl X fi
dnl X INSTALL_STRIP_PROGRAM="\${SHELL} \$(install_sh) -c -s"
dnl X AC_SUBST([INSTALL_STRIP_PROGRAM])])
dnl X 
dnl X # serial 4						-*- Autoconf -*-
dnl X 
dnl X # Copyright 1999, 2000, 2001 Free Software Foundation, Inc.
dnl X 
dnl X # This program is free software; you can redistribute it and/or modify
dnl X # it under the terms of the GNU General Public License as published by
dnl X # the Free Software Foundation; either version 2, or (at your option)
dnl X # any later version.
dnl X 
dnl X # This program is distributed in the hope that it will be useful,
dnl X # but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl X # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl X # GNU General Public License for more details.
dnl X 
dnl X # You should have received a copy of the GNU General Public License
dnl X # along with this program; if not, write to the Free Software
dnl X # Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
dnl X # 02111-1307, USA.
dnl X 
dnl X 
dnl X # There are a few dirty hacks below to avoid letting `AC_PROG_CC' be
dnl X # written in clear, in which case automake, when reading aclocal.m4,
dnl X # will think it sees a *use*, and therefore will trigger all it's
dnl X # C support machinery.  Also note that it means that autoscan, seeing
dnl X # CC etc. in the Makefile, will ask for an AC_PROG_CC use...
dnl X 
dnl X 
dnl X 
dnl X # _AM_DEPENDENCIES(NAME)
dnl X # ----------------------
dnl X # See how the compiler implements dependency checking.
dnl X # NAME is "CC", "CXX", "GCJ", or "OBJC".
dnl X # We try a few techniques and use that to set a single cache variable.
dnl X #
dnl X # We don't AC_REQUIRE the corresponding AC_PROG_CC since the latter was
dnl X # modified to invoke _AM_DEPENDENCIES(CC); we would have a circular
dnl X # dependency, and given that the user is not expected to run this macro,
dnl X # just rely on AC_PROG_CC.
dnl X AC_DEFUN([_AM_DEPENDENCIES],
dnl X [AC_REQUIRE([AM_SET_DEPDIR])dnl
dnl X AC_REQUIRE([AM_OUTPUT_DEPENDENCY_COMMANDS])dnl
dnl X AC_REQUIRE([AM_MAKE_INCLUDE])dnl
dnl X AC_REQUIRE([AM_DEP_TRACK])dnl
dnl X 
dnl X ifelse([$1], CC,   [depcc="$CC"   am_compiler_list=],
dnl X        [$1], CXX,  [depcc="$CXX"  am_compiler_list=],
dnl X        [$1], OBJC, [depcc="$OBJC" am_compiler_list='gcc3 gcc'],
dnl X        [$1], GCJ,  [depcc="$GCJ"  am_compiler_list='gcc3 gcc'],
dnl X                    [depcc="$$1"   am_compiler_list=])
dnl X 
dnl X AC_CACHE_CHECK([dependency style of $depcc],
dnl X                [am_cv_$1_dependencies_compiler_type],
dnl X [if test -z "$AMDEP_TRUE" && test -f "$am_depcomp"; then
dnl X   # We make a subdir and do the tests there.  Otherwise we can end up
dnl X   # making bogus files that we don't know about and never remove.  For
dnl X   # instance it was reported that on HP-UX the gcc test will end up
dnl X   # making a dummy file named `D' -- because `-MD' means `put the output
dnl X   # in D'.
dnl X   mkdir conftest.dir
dnl X   # Copy depcomp to subdir because otherwise we won't find it if we're
dnl X   # using a relative directory.
dnl X   cp "$am_depcomp" conftest.dir
dnl X   cd conftest.dir
dnl X 
dnl X   am_cv_$1_dependencies_compiler_type=none
dnl X   if test "$am_compiler_list" = ""; then
dnl X      am_compiler_list=`sed -n ['s/^#*\([a-zA-Z0-9]*\))$/\1/p'] < ./depcomp`
dnl X   fi
dnl X   for depmode in $am_compiler_list; do
dnl X     # We need to recreate these files for each test, as the compiler may
dnl X     # overwrite some of them when testing with obscure command lines.
dnl X     # This happens at least with the AIX C compiler.
dnl X     echo '#include "conftest.h"' > conftest.c
dnl X     echo 'int i;' > conftest.h
dnl X     echo "${am__include} ${am__quote}conftest.Po${am__quote}" > confmf
dnl X 
dnl X     case $depmode in
dnl X     nosideeffect)
dnl X       # after this tag, mechanisms are not by side-effect, so they'll
dnl X       # only be used when explicitly requested
dnl X       if test "x$enable_dependency_tracking" = xyes; then
dnl X 	continue
dnl X       else
dnl X 	break
dnl X       fi
dnl X       ;;
dnl X     none) break ;;
dnl X     esac
dnl X     # We check with `-c' and `-o' for the sake of the "dashmstdout"
dnl X     # mode.  It turns out that the SunPro C++ compiler does not properly
dnl X     # handle `-M -o', and we need to detect this.
dnl X     if depmode=$depmode \
dnl X        source=conftest.c object=conftest.o \
dnl X        depfile=conftest.Po tmpdepfile=conftest.TPo \
dnl X        $SHELL ./depcomp $depcc -c -o conftest.o conftest.c >/dev/null 2>&1 &&
dnl X        grep conftest.h conftest.Po > /dev/null 2>&1 &&
dnl X        ${MAKE-make} -s -f confmf > /dev/null 2>&1; then
dnl X       am_cv_$1_dependencies_compiler_type=$depmode
dnl X       break
dnl X     fi
dnl X   done
dnl X 
dnl X   cd ..
dnl X   rm -rf conftest.dir
dnl X else
dnl X   am_cv_$1_dependencies_compiler_type=none
dnl X fi
dnl X ])
dnl X AC_SUBST([$1DEPMODE], [depmode=$am_cv_$1_dependencies_compiler_type])
dnl X AM_CONDITIONAL([am__fastdep$1], [
dnl X   test "x$enable_dependency_tracking" != xno \
dnl X   && test "$am_cv_$1_dependencies_compiler_type" = gcc3])
dnl X ])
dnl X 
dnl X 
dnl X # AM_SET_DEPDIR
dnl X # -------------
dnl X # Choose a directory name for dependency files.
dnl X # This macro is AC_REQUIREd in _AM_DEPENDENCIES
dnl X AC_DEFUN([AM_SET_DEPDIR],
dnl X [rm -f .deps 2>/dev/null
dnl X mkdir .deps 2>/dev/null
dnl X if test -d .deps; then
dnl X   DEPDIR=.deps
dnl X else
dnl X   # MS-DOS does not allow filenames that begin with a dot.
dnl X   DEPDIR=_deps
dnl X fi
dnl X rmdir .deps 2>/dev/null
dnl X AC_SUBST([DEPDIR])
dnl X ])
dnl X 
dnl X 
dnl X # AM_DEP_TRACK
dnl X # ------------
dnl X AC_DEFUN([AM_DEP_TRACK],
dnl X [AC_ARG_ENABLE(dependency-tracking,
dnl X [  --disable-dependency-tracking Speeds up one-time builds
dnl X   --enable-dependency-tracking  Do not reject slow dependency extractors])
dnl X if test "x$enable_dependency_tracking" != xno; then
dnl X   am_depcomp="$ac_aux_dir/depcomp"
dnl X   AMDEPBACKSLASH='\'
dnl X fi
dnl X AM_CONDITIONAL([AMDEP], [test "x$enable_dependency_tracking" != xno])
dnl X AC_SUBST([AMDEPBACKSLASH])
dnl X ])
dnl X 
dnl X # Generate code to set up dependency tracking.   -*- Autoconf -*-
dnl X 
dnl X # Copyright 1999, 2000, 2001, 2002 Free Software Foundation, Inc.
dnl X 
dnl X # This program is free software; you can redistribute it and/or modify
dnl X # it under the terms of the GNU General Public License as published by
dnl X # the Free Software Foundation; either version 2, or (at your option)
dnl X # any later version.
dnl X 
dnl X # This program is distributed in the hope that it will be useful,
dnl X # but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl X # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl X # GNU General Public License for more details.
dnl X 
dnl X # You should have received a copy of the GNU General Public License
dnl X # along with this program; if not, write to the Free Software
dnl X # Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
dnl X # 02111-1307, USA.
dnl X 
dnl X #serial 2
dnl X 
dnl X # _AM_OUTPUT_DEPENDENCY_COMMANDS
dnl X # ------------------------------
dnl X AC_DEFUN([_AM_OUTPUT_DEPENDENCY_COMMANDS],
dnl X [for mf in $CONFIG_FILES; do
dnl X   # Strip MF so we end up with the name of the file.
dnl X   mf=`echo "$mf" | sed -e 's/:.*$//'`
dnl X   # Check whether this is an Automake generated Makefile or not.
dnl X   # We used to match only the files named `Makefile.in', but
dnl X   # some people rename them; so instead we look at the file content.
dnl X   # Grep'ing the first line is not enough: some people post-process
dnl X   # each Makefile.in and add a new line on top of each file to say so.
dnl X   # So let's grep whole file.
dnl X   if grep '^#.*generated by automake' $mf > /dev/null 2>&1; then
dnl X     dirpart=`AS_DIRNAME("$mf")`
dnl X   else
dnl X     continue
dnl X   fi
dnl X   grep '^DEP_FILES *= *[[^ @%:@]]' < "$mf" > /dev/null || continue
dnl X   # Extract the definition of DEP_FILES from the Makefile without
dnl X   # running `make'.
dnl X   DEPDIR=`sed -n -e '/^DEPDIR = / s///p' < "$mf"`
dnl X   test -z "$DEPDIR" && continue
dnl X   # When using ansi2knr, U may be empty or an underscore; expand it
dnl X   U=`sed -n -e '/^U = / s///p' < "$mf"`
dnl X   test -d "$dirpart/$DEPDIR" || mkdir "$dirpart/$DEPDIR"
dnl X   # We invoke sed twice because it is the simplest approach to
dnl X   # changing $(DEPDIR) to its actual value in the expansion.
dnl X   for file in `sed -n -e '
dnl X     /^DEP_FILES = .*\\\\$/ {
dnl X       s/^DEP_FILES = //
dnl X       :loop
dnl X 	s/\\\\$//
dnl X 	p
dnl X 	n
dnl X 	/\\\\$/ b loop
dnl X       p
dnl X     }
dnl X     /^DEP_FILES = / s/^DEP_FILES = //p' < "$mf" | \
dnl X        sed -e 's/\$(DEPDIR)/'"$DEPDIR"'/g' -e 's/\$U/'"$U"'/g'`; do
dnl X     # Make sure the directory exists.
dnl X     test -f "$dirpart/$file" && continue
dnl X     fdir=`AS_DIRNAME(["$file"])`
dnl X     AS_MKDIR_P([$dirpart/$fdir])
dnl X     # echo "creating $dirpart/$file"
dnl X     echo '# dummy' > "$dirpart/$file"
dnl X   done
dnl X done
dnl X ])# _AM_OUTPUT_DEPENDENCY_COMMANDS
dnl X 
dnl X 
dnl X # AM_OUTPUT_DEPENDENCY_COMMANDS
dnl X # -----------------------------
dnl X # This macro should only be invoked once -- use via AC_REQUIRE.
dnl X #
dnl X # This code is only required when automatic dependency tracking
dnl X # is enabled.  FIXME.  This creates each `.P' file that we will
dnl X # need in order to bootstrap the dependency handling code.
dnl X AC_DEFUN([AM_OUTPUT_DEPENDENCY_COMMANDS],
dnl X [AC_CONFIG_COMMANDS([depfiles],
dnl X      [test x"$AMDEP_TRUE" != x"" || _AM_OUTPUT_DEPENDENCY_COMMANDS],
dnl X      [AMDEP_TRUE="$AMDEP_TRUE" ac_aux_dir="$ac_aux_dir"])
dnl X ])
dnl X 
dnl X # Check to see how 'make' treats includes.	-*- Autoconf -*-
dnl X 
dnl X # Copyright (C) 2001, 2002 Free Software Foundation, Inc.
dnl X 
dnl X # This program is free software; you can redistribute it and/or modify
dnl X # it under the terms of the GNU General Public License as published by
dnl X # the Free Software Foundation; either version 2, or (at your option)
dnl X # any later version.
dnl X 
dnl X # This program is distributed in the hope that it will be useful,
dnl X # but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl X # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl X # GNU General Public License for more details.
dnl X 
dnl X # You should have received a copy of the GNU General Public License
dnl X # along with this program; if not, write to the Free Software
dnl X # Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
dnl X # 02111-1307, USA.
dnl X 
dnl X # serial 2
dnl X 
dnl X # AM_MAKE_INCLUDE()
dnl X # -----------------
dnl X # Check to see how make treats includes.
dnl X AC_DEFUN([AM_MAKE_INCLUDE],
dnl X [am_make=${MAKE-make}
dnl X cat > confinc << 'END'
dnl X doit:
dnl X 	@echo done
dnl X END
dnl X # If we don't find an include directive, just comment out the code.
dnl X AC_MSG_CHECKING([for style of include used by $am_make])
dnl X am__include="#"
dnl X am__quote=
dnl X _am_result=none
dnl X # First try GNU make style include.
dnl X echo "include confinc" > confmf
dnl X # We grep out `Entering directory' and `Leaving directory'
dnl X # messages which can occur if `w' ends up in MAKEFLAGS.
dnl X # In particular we don't look at `^make:' because GNU make might
dnl X # be invoked under some other name (usually "gmake"), in which
dnl X # case it prints its new name instead of `make'.
dnl X if test "`$am_make -s -f confmf 2> /dev/null | grep -v 'ing directory'`" = "done"; then
dnl X    am__include=include
dnl X    am__quote=
dnl X    _am_result=GNU
dnl X fi
dnl X # Now try BSD make style include.
dnl X if test "$am__include" = "#"; then
dnl X    echo '.include "confinc"' > confmf
dnl X    if test "`$am_make -s -f confmf 2> /dev/null`" = "done"; then
dnl X       am__include=.include
dnl X       am__quote="\""
dnl X       _am_result=BSD
dnl X    fi
dnl X fi
dnl X AC_SUBST(am__include)
dnl X AC_SUBST(am__quote)
dnl X AC_MSG_RESULT($_am_result)
dnl X rm -f confinc confmf
dnl X ])
dnl X 
dnl X # AM_CONDITIONAL                                              -*- Autoconf -*-
dnl X 
dnl X # Copyright 1997, 2000, 2001 Free Software Foundation, Inc.
dnl X 
dnl X # This program is free software; you can redistribute it and/or modify
dnl X # it under the terms of the GNU General Public License as published by
dnl X # the Free Software Foundation; either version 2, or (at your option)
dnl X # any later version.
dnl X 
dnl X # This program is distributed in the hope that it will be useful,
dnl X # but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl X # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl X # GNU General Public License for more details.
dnl X 
dnl X # You should have received a copy of the GNU General Public License
dnl X # along with this program; if not, write to the Free Software
dnl X # Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
dnl X # 02111-1307, USA.
dnl X 
dnl X # serial 5
dnl X 
dnl X AC_PREREQ(2.52)
dnl X 
dnl X # AM_CONDITIONAL(NAME, SHELL-CONDITION)
dnl X # -------------------------------------
dnl X # Define a conditional.
dnl X AC_DEFUN([AM_CONDITIONAL],
dnl X [ifelse([$1], [TRUE],  [AC_FATAL([$0: invalid condition: $1])],
dnl X         [$1], [FALSE], [AC_FATAL([$0: invalid condition: $1])])dnl
dnl X AC_SUBST([$1_TRUE])
dnl X AC_SUBST([$1_FALSE])
dnl X if $2; then
dnl X   $1_TRUE=
dnl X   $1_FALSE='#'
dnl X else
dnl X   $1_TRUE='#'
dnl X   $1_FALSE=
dnl X fi
dnl X AC_CONFIG_COMMANDS_PRE(
dnl X [if test -z "${$1_TRUE}" && test -z "${$1_FALSE}"; then
dnl X   AC_MSG_ERROR([conditional "$1" was never defined.
dnl X Usually this means the macro was only invoked conditionally.])
dnl X fi])])
dnl X 
dnl X # libtool.m4 - Configure libtool for the host system. -*-Shell-script-*-
dnl X ## Copyright 1996, 1997, 1998, 1999, 2000, 2001
dnl X ## Free Software Foundation, Inc.
dnl X ## Originally by Gordon Matzigkeit <gord@gnu.ai.mit.edu>, 1996
dnl X ##
dnl X ## This program is free software; you can redistribute it and/or modify
dnl X ## it under the terms of the GNU General Public License as published by
dnl X ## the Free Software Foundation; either version 2 of the License, or
dnl X ## (at your option) any later version.
dnl X ##
dnl X ## This program is distributed in the hope that it will be useful, but
dnl X ## WITHOUT ANY WARRANTY; without even the implied warranty of
dnl X ## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl X ## General Public License for more details.
dnl X ##
dnl X ## You should have received a copy of the GNU General Public License
dnl X ## along with this program; if not, write to the Free Software
dnl X ## Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
dnl X ##
dnl X ## As a special exception to the GNU General Public License, if you
dnl X ## distribute this file as part of a program that contains a
dnl X ## configuration script generated by Autoconf, you may include it under
dnl X ## the same distribution terms that you use for the rest of that program.
dnl X 
dnl X # serial 46 AC_PROG_LIBTOOL
dnl X 
dnl X AC_DEFUN([AC_PROG_LIBTOOL],
dnl X [AC_REQUIRE([AC_LIBTOOL_SETUP])dnl
dnl X 
dnl X # This can be used to rebuild libtool when needed
dnl X LIBTOOL_DEPS="$ac_aux_dir/ltmain.sh"
dnl X 
dnl X # Always use our own libtool.
dnl X LIBTOOL='$(SHELL) $(top_builddir)/libtool'
dnl X AC_SUBST(LIBTOOL)dnl
dnl X 
dnl X # Prevent multiple expansion
dnl X define([AC_PROG_LIBTOOL], [])
dnl X ])
dnl X 
dnl X AC_DEFUN([AC_LIBTOOL_SETUP],
dnl X [AC_PREREQ(2.13)dnl
dnl X AC_REQUIRE([AC_ENABLE_SHARED])dnl
dnl X AC_REQUIRE([AC_ENABLE_STATIC])dnl
dnl X AC_REQUIRE([AC_ENABLE_FAST_INSTALL])dnl
dnl X AC_REQUIRE([AC_CANONICAL_HOST])dnl
dnl X AC_REQUIRE([AC_CANONICAL_BUILD])dnl
dnl X AC_REQUIRE([AC_PROG_CC])dnl
dnl X AC_REQUIRE([AC_PROG_LD])dnl
dnl X AC_REQUIRE([AC_PROG_LD_RELOAD_FLAG])dnl
dnl X AC_REQUIRE([AC_PROG_NM])dnl
dnl X AC_REQUIRE([LT_AC_PROG_SED])dnl
dnl X 
dnl X AC_REQUIRE([AC_PROG_LN_S])dnl
dnl X AC_REQUIRE([AC_DEPLIBS_CHECK_METHOD])dnl
dnl X AC_REQUIRE([AC_OBJEXT])dnl
dnl X AC_REQUIRE([AC_EXEEXT])dnl
dnl X dnl
dnl X 
dnl X _LT_AC_PROG_ECHO_BACKSLASH
dnl X # Only perform the check for file, if the check method requires it
dnl X case $deplibs_check_method in
dnl X file_magic*)
dnl X   if test "$file_magic_cmd" = '$MAGIC_CMD'; then
dnl X     AC_PATH_MAGIC
dnl X   fi
dnl X   ;;
dnl X esac
dnl X 
dnl X AC_CHECK_TOOL(RANLIB, ranlib, :)
dnl X AC_CHECK_TOOL(STRIP, strip, :)
dnl X 
dnl X ifdef([AC_PROVIDE_AC_LIBTOOL_DLOPEN], enable_dlopen=yes, enable_dlopen=no)
dnl X ifdef([AC_PROVIDE_AC_LIBTOOL_WIN32_DLL],
dnl X enable_win32_dll=yes, enable_win32_dll=no)
dnl X 
dnl X AC_ARG_ENABLE(libtool-lock,
dnl X   [  --disable-libtool-lock  avoid locking (might break parallel builds)])
dnl X test "x$enable_libtool_lock" != xno && enable_libtool_lock=yes
dnl X 
dnl X # Some flags need to be propagated to the compiler or linker for good
dnl X # libtool support.
dnl X case $host in
dnl X *-*-irix6*)
dnl X   # Find out which ABI we are using.
dnl X   echo '[#]line __oline__ "configure"' > conftest.$ac_ext
dnl X   if AC_TRY_EVAL(ac_compile); then
dnl X     case `/usr/bin/file conftest.$ac_objext` in
dnl X     *32-bit*)
dnl X       LD="${LD-ld} -32"
dnl X       ;;
dnl X     *N32*)
dnl X       LD="${LD-ld} -n32"
dnl X       ;;
dnl X     *64-bit*)
dnl X       LD="${LD-ld} -64"
dnl X       ;;
dnl X     esac
dnl X   fi
dnl X   rm -rf conftest*
dnl X   ;;
dnl X 
dnl X *-*-sco3.2v5*)
dnl X   # On SCO OpenServer 5, we need -belf to get full-featured binaries.
dnl X   SAVE_CFLAGS="$CFLAGS"
dnl X   CFLAGS="$CFLAGS -belf"
dnl X   AC_CACHE_CHECK([whether the C compiler needs -belf], lt_cv_cc_needs_belf,
dnl X     [AC_LANG_SAVE
dnl X      AC_LANG_C
dnl X      AC_TRY_LINK([],[],[lt_cv_cc_needs_belf=yes],[lt_cv_cc_needs_belf=no])
dnl X      AC_LANG_RESTORE])
dnl X   if test x"$lt_cv_cc_needs_belf" != x"yes"; then
dnl X     # this is probably gcc 2.8.0, egcs 1.0 or newer; no need for -belf
dnl X     CFLAGS="$SAVE_CFLAGS"
dnl X   fi
dnl X   ;;
dnl X 
dnl X ifdef([AC_PROVIDE_AC_LIBTOOL_WIN32_DLL],
dnl X [*-*-cygwin* | *-*-mingw* | *-*-pw32*)
dnl X   AC_CHECK_TOOL(DLLTOOL, dlltool, false)
dnl X   AC_CHECK_TOOL(AS, as, false)
dnl X   AC_CHECK_TOOL(OBJDUMP, objdump, false)
dnl X 
dnl X   # recent cygwin and mingw systems supply a stub DllMain which the user
dnl X   # can override, but on older systems we have to supply one
dnl X   AC_CACHE_CHECK([if libtool should supply DllMain function], lt_cv_need_dllmain,
dnl X     [AC_TRY_LINK([],
dnl X       [extern int __attribute__((__stdcall__)) DllMain(void*, int, void*);
dnl X       DllMain (0, 0, 0);],
dnl X       [lt_cv_need_dllmain=no],[lt_cv_need_dllmain=yes])])
dnl X 
dnl X   case $host/$CC in
dnl X   *-*-cygwin*/gcc*-mno-cygwin*|*-*-mingw*)
dnl X     # old mingw systems require "-dll" to link a DLL, while more recent ones
dnl X     # require "-mdll"
dnl X     SAVE_CFLAGS="$CFLAGS"
dnl X     CFLAGS="$CFLAGS -mdll"
dnl X     AC_CACHE_CHECK([how to link DLLs], lt_cv_cc_dll_switch,
dnl X       [AC_TRY_LINK([], [], [lt_cv_cc_dll_switch=-mdll],[lt_cv_cc_dll_switch=-dll])])
dnl X     CFLAGS="$SAVE_CFLAGS" ;;
dnl X   *-*-cygwin* | *-*-pw32*)
dnl X     # cygwin systems need to pass --dll to the linker, and not link
dnl X     # crt.o which will require a WinMain@16 definition.
dnl X     lt_cv_cc_dll_switch="-Wl,--dll -nostartfiles" ;;
dnl X   esac
dnl X   ;;
dnl X   ])
dnl X esac
dnl X 
dnl X _LT_AC_LTCONFIG_HACK
dnl X 
dnl X ])
dnl X 
dnl X # AC_LIBTOOL_HEADER_ASSERT
dnl X # ------------------------
dnl X AC_DEFUN([AC_LIBTOOL_HEADER_ASSERT],
dnl X [AC_CACHE_CHECK([whether $CC supports assert without backlinking],
dnl X     [lt_cv_func_assert_works],
dnl X     [case $host in
dnl X     *-*-solaris*)
dnl X       if test "$GCC" = yes && test "$with_gnu_ld" != yes; then
dnl X         case `$CC --version 2>/dev/null` in
dnl X         [[12]].*) lt_cv_func_assert_works=no ;;
dnl X         *)        lt_cv_func_assert_works=yes ;;
dnl X         esac
dnl X       fi
dnl X       ;;
dnl X     esac])
dnl X 
dnl X if test "x$lt_cv_func_assert_works" = xyes; then
dnl X   AC_CHECK_HEADERS(assert.h)
dnl X fi
dnl X ])# AC_LIBTOOL_HEADER_ASSERT
dnl X 
dnl X # _LT_AC_CHECK_DLFCN
dnl X # --------------------
dnl X AC_DEFUN([_LT_AC_CHECK_DLFCN],
dnl X [AC_CHECK_HEADERS(dlfcn.h)
dnl X ])# _LT_AC_CHECK_DLFCN
dnl X 
dnl X # AC_LIBTOOL_SYS_GLOBAL_SYMBOL_PIPE
dnl X # ---------------------------------
dnl X AC_DEFUN([AC_LIBTOOL_SYS_GLOBAL_SYMBOL_PIPE],
dnl X [AC_REQUIRE([AC_CANONICAL_HOST])
dnl X AC_REQUIRE([AC_PROG_NM])
dnl X AC_REQUIRE([AC_OBJEXT])
dnl X # Check for command to grab the raw symbol name followed by C symbol from nm.
dnl X AC_MSG_CHECKING([command to parse $NM output])
dnl X AC_CACHE_VAL([lt_cv_sys_global_symbol_pipe], [dnl
dnl X 
dnl X # These are sane defaults that work on at least a few old systems.
dnl X # [They come from Ultrix.  What could be older than Ultrix?!! ;)]
dnl X 
dnl X # Character class describing NM global symbol codes.
dnl X symcode='[[BCDEGRST]]'
dnl X 
dnl X # Regexp to match symbols that can be accessed directly from C.
dnl X sympat='\([[_A-Za-z]][[_A-Za-z0-9]]*\)'
dnl X 
dnl X # Transform the above into a raw symbol and a C symbol.
dnl X symxfrm='\1 \2\3 \3'
dnl X 
dnl X # Transform an extracted symbol line into a proper C declaration
dnl X lt_cv_global_symbol_to_cdecl="sed -n -e 's/^. .* \(.*\)$/extern char \1;/p'"
dnl X 
dnl X # Transform an extracted symbol line into symbol name and symbol address
dnl X lt_cv_global_symbol_to_c_name_address="sed -n -e 's/^: \([[^ ]]*\) $/  {\\\"\1\\\", (lt_ptr) 0},/p' -e 's/^$symcode \([[^ ]]*\) \([[^ ]]*\)$/  {\"\2\", (lt_ptr) \&\2},/p'"
dnl X 
dnl X # Define system-specific variables.
dnl X case $host_os in
dnl X aix*)
dnl X   symcode='[[BCDT]]'
dnl X   ;;
dnl X cygwin* | mingw* | pw32*)
dnl X   symcode='[[ABCDGISTW]]'
dnl X   ;;
dnl X hpux*) # Its linker distinguishes data from code symbols
dnl X   lt_cv_global_symbol_to_cdecl="sed -n -e 's/^T .* \(.*\)$/extern char \1();/p' -e 's/^$symcode* .* \(.*\)$/extern char \1;/p'"
dnl X   lt_cv_global_symbol_to_c_name_address="sed -n -e 's/^: \([[^ ]]*\) $/  {\\\"\1\\\", (lt_ptr) 0},/p' -e 's/^$symcode* \([[^ ]]*\) \([[^ ]]*\)$/  {\"\2\", (lt_ptr) \&\2},/p'"
dnl X   ;;
dnl X irix* | nonstopux*)
dnl X   symcode='[[BCDEGRST]]'
dnl X   ;;
dnl X osf*)
dnl X   symcode='[[BCDEGQRST]]'
dnl X   ;;
dnl X solaris* | sysv5*)
dnl X   symcode='[[BDT]]'
dnl X   ;;
dnl X sysv4)
dnl X   symcode='[[DFNSTU]]'
dnl X   ;;
dnl X esac
dnl X 
dnl X # Handle CRLF in mingw tool chain
dnl X opt_cr=
dnl X case $host_os in
dnl X mingw*)
dnl X   opt_cr=`echo 'x\{0,1\}' | tr x '\015'` # option cr in regexp
dnl X   ;;
dnl X esac
dnl X 
dnl X # If we're using GNU nm, then use its standard symbol codes.
dnl X if $NM -V 2>&1 | egrep '(GNU|with BFD)' > /dev/null; then
dnl X   symcode='[[ABCDGISTW]]'
dnl X fi
dnl X 
dnl X # Try without a prefix undercore, then with it.
dnl X for ac_symprfx in "" "_"; do
dnl X 
dnl X   # Write the raw and C identifiers.
dnl X lt_cv_sys_global_symbol_pipe="sed -n -e 's/^.*[[ 	]]\($symcode$symcode*\)[[ 	]][[ 	]]*\($ac_symprfx\)$sympat$opt_cr$/$symxfrm/p'"
dnl X 
dnl X   # Check to see that the pipe works correctly.
dnl X   pipe_works=no
dnl X   rm -f conftest*
dnl X   cat > conftest.$ac_ext <<EOF
dnl X #ifdef __cplusplus
dnl X extern "C" {
dnl X #endif
dnl X char nm_test_var;
dnl X void nm_test_func(){}
dnl X #ifdef __cplusplus
dnl X }
dnl X #endif
dnl X int main(){nm_test_var='a';nm_test_func();return(0);}
dnl X EOF
dnl X 
dnl X   if AC_TRY_EVAL(ac_compile); then
dnl X     # Now try to grab the symbols.
dnl X     nlist=conftest.nm
dnl X     if AC_TRY_EVAL(NM conftest.$ac_objext \| $lt_cv_sys_global_symbol_pipe \> $nlist) && test -s "$nlist"; then
dnl X       # Try sorting and uniquifying the output.
dnl X       if sort "$nlist" | uniq > "$nlist"T; then
dnl X 	mv -f "$nlist"T "$nlist"
dnl X       else
dnl X 	rm -f "$nlist"T
dnl X       fi
dnl X 
dnl X       # Make sure that we snagged all the symbols we need.
dnl X       if egrep ' nm_test_var$' "$nlist" >/dev/null; then
dnl X 	if egrep ' nm_test_func$' "$nlist" >/dev/null; then
dnl X 	  cat <<EOF > conftest.$ac_ext
dnl X #ifdef __cplusplus
dnl X extern "C" {
dnl X #endif
dnl X 
dnl X EOF
dnl X 	  # Now generate the symbol file.
dnl X 	  eval "$lt_cv_global_symbol_to_cdecl"' < "$nlist" >> conftest.$ac_ext'
dnl X 
dnl X 	  cat <<EOF >> conftest.$ac_ext
dnl X #if defined (__STDC__) && __STDC__
dnl X # define lt_ptr void *
dnl X #else
dnl X # define lt_ptr char *
dnl X # define const
dnl X #endif
dnl X 
dnl X /* The mapping between symbol names and symbols. */
dnl X const struct {
dnl X   const char *name;
dnl X   lt_ptr address;
dnl X }
dnl X lt_preloaded_symbols[[]] =
dnl X {
dnl X EOF
dnl X 	  sed "s/^$symcode$symcode* \(.*\) \(.*\)$/  {\"\2\", (lt_ptr) \&\2},/" < "$nlist" >> conftest.$ac_ext
dnl X 	  cat <<\EOF >> conftest.$ac_ext
dnl X   {0, (lt_ptr) 0}
dnl X };
dnl X 
dnl X #ifdef __cplusplus
dnl X }
dnl X #endif
dnl X EOF
dnl X 	  # Now try linking the two files.
dnl X 	  mv conftest.$ac_objext conftstm.$ac_objext
dnl X 	  save_LIBS="$LIBS"
dnl X 	  save_CFLAGS="$CFLAGS"
dnl X 	  LIBS="conftstm.$ac_objext"
dnl X 	  CFLAGS="$CFLAGS$no_builtin_flag"
dnl X 	  if AC_TRY_EVAL(ac_link) && test -s conftest$ac_exeext; then
dnl X 	    pipe_works=yes
dnl X 	  fi
dnl X 	  LIBS="$save_LIBS"
dnl X 	  CFLAGS="$save_CFLAGS"
dnl X 	else
dnl X 	  echo "cannot find nm_test_func in $nlist" >&AC_FD_CC
dnl X 	fi
dnl X       else
dnl X 	echo "cannot find nm_test_var in $nlist" >&AC_FD_CC
dnl X       fi
dnl X     else
dnl X       echo "cannot run $lt_cv_sys_global_symbol_pipe" >&AC_FD_CC
dnl X     fi
dnl X   else
dnl X     echo "$progname: failed program was:" >&AC_FD_CC
dnl X     cat conftest.$ac_ext >&5
dnl X   fi
dnl X   rm -f conftest* conftst*
dnl X 
dnl X   # Do not use the global_symbol_pipe unless it works.
dnl X   if test "$pipe_works" = yes; then
dnl X     break
dnl X   else
dnl X     lt_cv_sys_global_symbol_pipe=
dnl X   fi
dnl X done
dnl X ])
dnl X global_symbol_pipe="$lt_cv_sys_global_symbol_pipe"
dnl X if test -z "$lt_cv_sys_global_symbol_pipe"; then
dnl X   global_symbol_to_cdecl=
dnl X   global_symbol_to_c_name_address=
dnl X else
dnl X   global_symbol_to_cdecl="$lt_cv_global_symbol_to_cdecl"
dnl X   global_symbol_to_c_name_address="$lt_cv_global_symbol_to_c_name_address"
dnl X fi
dnl X if test -z "$global_symbol_pipe$global_symbol_to_cdec$global_symbol_to_c_name_address";
dnl X then
dnl X   AC_MSG_RESULT(failed)
dnl X else
dnl X   AC_MSG_RESULT(ok)
dnl X fi
dnl X ]) # AC_LIBTOOL_SYS_GLOBAL_SYMBOL_PIPE
dnl X 
dnl X # _LT_AC_LIBTOOL_SYS_PATH_SEPARATOR
dnl X # ---------------------------------
dnl X AC_DEFUN([_LT_AC_LIBTOOL_SYS_PATH_SEPARATOR],
dnl X [# Find the correct PATH separator.  Usually this is `:', but
dnl X # DJGPP uses `;' like DOS.
dnl X if test "X${PATH_SEPARATOR+set}" != Xset; then
dnl X   UNAME=${UNAME-`uname 2>/dev/null`}
dnl X   case X$UNAME in
dnl X     *-DOS) lt_cv_sys_path_separator=';' ;;
dnl X     *)     lt_cv_sys_path_separator=':' ;;
dnl X   esac
dnl X   PATH_SEPARATOR=$lt_cv_sys_path_separator
dnl X fi
dnl X ])# _LT_AC_LIBTOOL_SYS_PATH_SEPARATOR
dnl X 
dnl X # _LT_AC_PROG_ECHO_BACKSLASH
dnl X # --------------------------
dnl X # Add some code to the start of the generated configure script which
dnl X # will find an echo command which doesn't interpret backslashes.
dnl X AC_DEFUN([_LT_AC_PROG_ECHO_BACKSLASH],
dnl X [ifdef([AC_DIVERSION_NOTICE], [AC_DIVERT_PUSH(AC_DIVERSION_NOTICE)],
dnl X 			      [AC_DIVERT_PUSH(NOTICE)])
dnl X _LT_AC_LIBTOOL_SYS_PATH_SEPARATOR
dnl X 
dnl X # Check that we are running under the correct shell.
dnl X SHELL=${CONFIG_SHELL-/bin/sh}
dnl X 
dnl X case X$ECHO in
dnl X X*--fallback-echo)
dnl X   # Remove one level of quotation (which was required for Make).
dnl X   ECHO=`echo "$ECHO" | sed 's,\\\\\[$]\\[$]0,'[$]0','`
dnl X   ;;
dnl X esac
dnl X 
dnl X echo=${ECHO-echo}
dnl X if test "X[$]1" = X--no-reexec; then
dnl X   # Discard the --no-reexec flag, and continue.
dnl X   shift
dnl X elif test "X[$]1" = X--fallback-echo; then
dnl X   # Avoid inline document here, it may be left over
dnl X   :
dnl X elif test "X`($echo '\t') 2>/dev/null`" = 'X\t'; then
dnl X   # Yippee, $echo works!
dnl X   :
dnl X else
dnl X   # Restart under the correct shell.
dnl X   exec $SHELL "[$]0" --no-reexec ${1+"[$]@"}
dnl X fi
dnl X 
dnl X if test "X[$]1" = X--fallback-echo; then
dnl X   # used as fallback echo
dnl X   shift
dnl X   cat <<EOF
dnl X $*
dnl X EOF
dnl X   exit 0
dnl X fi
dnl X 
dnl X # The HP-UX ksh and POSIX shell print the target directory to stdout
dnl X # if CDPATH is set.
dnl X if test "X${CDPATH+set}" = Xset; then CDPATH=:; export CDPATH; fi
dnl X 
dnl X if test -z "$ECHO"; then
dnl X if test "X${echo_test_string+set}" != Xset; then
dnl X # find a string as large as possible, as long as the shell can cope with it
dnl X   for cmd in 'sed 50q "[$]0"' 'sed 20q "[$]0"' 'sed 10q "[$]0"' 'sed 2q "[$]0"' 'echo test'; do
dnl X     # expected sizes: less than 2Kb, 1Kb, 512 bytes, 16 bytes, ...
dnl X     if (echo_test_string="`eval $cmd`") 2>/dev/null &&
dnl X        echo_test_string="`eval $cmd`" &&
dnl X        (test "X$echo_test_string" = "X$echo_test_string") 2>/dev/null
dnl X     then
dnl X       break
dnl X     fi
dnl X   done
dnl X fi
dnl X 
dnl X if test "X`($echo '\t') 2>/dev/null`" = 'X\t' &&
dnl X    echo_testing_string=`($echo "$echo_test_string") 2>/dev/null` &&
dnl X    test "X$echo_testing_string" = "X$echo_test_string"; then
dnl X   :
dnl X else
dnl X   # The Solaris, AIX, and Digital Unix default echo programs unquote
dnl X   # backslashes.  This makes it impossible to quote backslashes using
dnl X   #   echo "$something" | sed 's/\\/\\\\/g'
dnl X   #
dnl X   # So, first we look for a working echo in the user's PATH.
dnl X 
dnl X   IFS="${IFS= 	}"; save_ifs="$IFS"; IFS=$PATH_SEPARATOR
dnl X   for dir in $PATH /usr/ucb; do
dnl X     if (test -f $dir/echo || test -f $dir/echo$ac_exeext) &&
dnl X        test "X`($dir/echo '\t') 2>/dev/null`" = 'X\t' &&
dnl X        echo_testing_string=`($dir/echo "$echo_test_string") 2>/dev/null` &&
dnl X        test "X$echo_testing_string" = "X$echo_test_string"; then
dnl X       echo="$dir/echo"
dnl X       break
dnl X     fi
dnl X   done
dnl X   IFS="$save_ifs"
dnl X 
dnl X   if test "X$echo" = Xecho; then
dnl X     # We didn't find a better echo, so look for alternatives.
dnl X     if test "X`(print -r '\t') 2>/dev/null`" = 'X\t' &&
dnl X        echo_testing_string=`(print -r "$echo_test_string") 2>/dev/null` &&
dnl X        test "X$echo_testing_string" = "X$echo_test_string"; then
dnl X       # This shell has a builtin print -r that does the trick.
dnl X       echo='print -r'
dnl X     elif (test -f /bin/ksh || test -f /bin/ksh$ac_exeext) &&
dnl X 	 test "X$CONFIG_SHELL" != X/bin/ksh; then
dnl X       # If we have ksh, try running configure again with it.
dnl X       ORIGINAL_CONFIG_SHELL=${CONFIG_SHELL-/bin/sh}
dnl X       export ORIGINAL_CONFIG_SHELL
dnl X       CONFIG_SHELL=/bin/ksh
dnl X       export CONFIG_SHELL
dnl X       exec $CONFIG_SHELL "[$]0" --no-reexec ${1+"[$]@"}
dnl X     else
dnl X       # Try using printf.
dnl X       echo='printf %s\n'
dnl X       if test "X`($echo '\t') 2>/dev/null`" = 'X\t' &&
dnl X 	 echo_testing_string=`($echo "$echo_test_string") 2>/dev/null` &&
dnl X 	 test "X$echo_testing_string" = "X$echo_test_string"; then
dnl X 	# Cool, printf works
dnl X 	:
dnl X       elif echo_testing_string=`($ORIGINAL_CONFIG_SHELL "[$]0" --fallback-echo '\t') 2>/dev/null` &&
dnl X 	   test "X$echo_testing_string" = 'X\t' &&
dnl X 	   echo_testing_string=`($ORIGINAL_CONFIG_SHELL "[$]0" --fallback-echo "$echo_test_string") 2>/dev/null` &&
dnl X 	   test "X$echo_testing_string" = "X$echo_test_string"; then
dnl X 	CONFIG_SHELL=$ORIGINAL_CONFIG_SHELL
dnl X 	export CONFIG_SHELL
dnl X 	SHELL="$CONFIG_SHELL"
dnl X 	export SHELL
dnl X 	echo="$CONFIG_SHELL [$]0 --fallback-echo"
dnl X       elif echo_testing_string=`($CONFIG_SHELL "[$]0" --fallback-echo '\t') 2>/dev/null` &&
dnl X 	   test "X$echo_testing_string" = 'X\t' &&
dnl X 	   echo_testing_string=`($CONFIG_SHELL "[$]0" --fallback-echo "$echo_test_string") 2>/dev/null` &&
dnl X 	   test "X$echo_testing_string" = "X$echo_test_string"; then
dnl X 	echo="$CONFIG_SHELL [$]0 --fallback-echo"
dnl X       else
dnl X 	# maybe with a smaller string...
dnl X 	prev=:
dnl X 
dnl X 	for cmd in 'echo test' 'sed 2q "[$]0"' 'sed 10q "[$]0"' 'sed 20q "[$]0"' 'sed 50q "[$]0"'; do
dnl X 	  if (test "X$echo_test_string" = "X`eval $cmd`") 2>/dev/null
dnl X 	  then
dnl X 	    break
dnl X 	  fi
dnl X 	  prev="$cmd"
dnl X 	done
dnl X 
dnl X 	if test "$prev" != 'sed 50q "[$]0"'; then
dnl X 	  echo_test_string=`eval $prev`
dnl X 	  export echo_test_string
dnl X 	  exec ${ORIGINAL_CONFIG_SHELL-${CONFIG_SHELL-/bin/sh}} "[$]0" ${1+"[$]@"}
dnl X 	else
dnl X 	  # Oops.  We lost completely, so just stick with echo.
dnl X 	  echo=echo
dnl X 	fi
dnl X       fi
dnl X     fi
dnl X   fi
dnl X fi
dnl X fi
dnl X 
dnl X # Copy echo and quote the copy suitably for passing to libtool from
dnl X # the Makefile, instead of quoting the original, which is used later.
dnl X ECHO=$echo
dnl X if test "X$ECHO" = "X$CONFIG_SHELL [$]0 --fallback-echo"; then
dnl X    ECHO="$CONFIG_SHELL \\\$\[$]0 --fallback-echo"
dnl X fi
dnl X 
dnl X AC_SUBST(ECHO)
dnl X AC_DIVERT_POP
dnl X ])# _LT_AC_PROG_ECHO_BACKSLASH
dnl X 
dnl X # _LT_AC_TRY_DLOPEN_SELF (ACTION-IF-TRUE, ACTION-IF-TRUE-W-USCORE,
dnl X #                           ACTION-IF-FALSE, ACTION-IF-CROSS-COMPILING)
dnl X # ------------------------------------------------------------------
dnl X AC_DEFUN([_LT_AC_TRY_DLOPEN_SELF],
dnl X [if test "$cross_compiling" = yes; then :
dnl X   [$4]
dnl X else
dnl X   AC_REQUIRE([_LT_AC_CHECK_DLFCN])dnl
dnl X   lt_dlunknown=0; lt_dlno_uscore=1; lt_dlneed_uscore=2
dnl X   lt_status=$lt_dlunknown
dnl X   cat > conftest.$ac_ext <<EOF
dnl X [#line __oline__ "configure"
dnl X #include "confdefs.h"
dnl X 
dnl X #if HAVE_DLFCN_H
dnl X #include <dlfcn.h>
dnl X #endif
dnl X 
dnl X #include <stdio.h>
dnl X 
dnl X #ifdef RTLD_GLOBAL
dnl X #  define LT_DLGLOBAL		RTLD_GLOBAL
dnl X #else
dnl X #  ifdef DL_GLOBAL
dnl X #    define LT_DLGLOBAL		DL_GLOBAL
dnl X #  else
dnl X #    define LT_DLGLOBAL		0
dnl X #  endif
dnl X #endif
dnl X 
dnl X /* We may have to define LT_DLLAZY_OR_NOW in the command line if we
dnl X    find out it does not work in some platform. */
dnl X #ifndef LT_DLLAZY_OR_NOW
dnl X #  ifdef RTLD_LAZY
dnl X #    define LT_DLLAZY_OR_NOW		RTLD_LAZY
dnl X #  else
dnl X #    ifdef DL_LAZY
dnl X #      define LT_DLLAZY_OR_NOW		DL_LAZY
dnl X #    else
dnl X #      ifdef RTLD_NOW
dnl X #        define LT_DLLAZY_OR_NOW	RTLD_NOW
dnl X #      else
dnl X #        ifdef DL_NOW
dnl X #          define LT_DLLAZY_OR_NOW	DL_NOW
dnl X #        else
dnl X #          define LT_DLLAZY_OR_NOW	0
dnl X #        endif
dnl X #      endif
dnl X #    endif
dnl X #  endif
dnl X #endif
dnl X 
dnl X #ifdef __cplusplus
dnl X extern "C" void exit (int);
dnl X #endif
dnl X 
dnl X void fnord() { int i=42;}
dnl X int main ()
dnl X {
dnl X   void *self = dlopen (0, LT_DLGLOBAL|LT_DLLAZY_OR_NOW);
dnl X   int status = $lt_dlunknown;
dnl X 
dnl X   if (self)
dnl X     {
dnl X       if (dlsym (self,"fnord"))       status = $lt_dlno_uscore;
dnl X       else if (dlsym( self,"_fnord")) status = $lt_dlneed_uscore;
dnl X       /* dlclose (self); */
dnl X     }
dnl X 
dnl X     exit (status);
dnl X }]
dnl X EOF
dnl X   if AC_TRY_EVAL(ac_link) && test -s conftest${ac_exeext} 2>/dev/null; then
dnl X     (./conftest; exit; ) 2>/dev/null
dnl X     lt_status=$?
dnl X     case x$lt_status in
dnl X       x$lt_dlno_uscore) $1 ;;
dnl X       x$lt_dlneed_uscore) $2 ;;
dnl X       x$lt_unknown|x*) $3 ;;
dnl X     esac
dnl X   else :
dnl X     # compilation failed
dnl X     $3
dnl X   fi
dnl X fi
dnl X rm -fr conftest*
dnl X ])# _LT_AC_TRY_DLOPEN_SELF
dnl X 
dnl X # AC_LIBTOOL_DLOPEN_SELF
dnl X # -------------------
dnl X AC_DEFUN([AC_LIBTOOL_DLOPEN_SELF],
dnl X [if test "x$enable_dlopen" != xyes; then
dnl X   enable_dlopen=unknown
dnl X   enable_dlopen_self=unknown
dnl X   enable_dlopen_self_static=unknown
dnl X else
dnl X   lt_cv_dlopen=no
dnl X   lt_cv_dlopen_libs=
dnl X 
dnl X   case $host_os in
dnl X   beos*)
dnl X     lt_cv_dlopen="load_add_on"
dnl X     lt_cv_dlopen_libs=
dnl X     lt_cv_dlopen_self=yes
dnl X     ;;
dnl X 
dnl X   cygwin* | mingw* | pw32*)
dnl X     lt_cv_dlopen="LoadLibrary"
dnl X     lt_cv_dlopen_libs=
dnl X    ;;
dnl X 
dnl X   *)
dnl X     AC_CHECK_FUNC([shl_load],
dnl X           [lt_cv_dlopen="shl_load"],
dnl X       [AC_CHECK_LIB([dld], [shl_load],
dnl X             [lt_cv_dlopen="shl_load" lt_cv_dlopen_libs="-dld"],
dnl X 	[AC_CHECK_FUNC([dlopen],
dnl X 	      [lt_cv_dlopen="dlopen"],
dnl X 	  [AC_CHECK_LIB([dl], [dlopen],
dnl X 	        [lt_cv_dlopen="dlopen" lt_cv_dlopen_libs="-ldl"],
dnl X 	    [AC_CHECK_LIB([svld], [dlopen],
dnl X 	          [lt_cv_dlopen="dlopen" lt_cv_dlopen_libs="-lsvld"],
dnl X 	      [AC_CHECK_LIB([dld], [dld_link],
dnl X 	            [lt_cv_dlopen="dld_link" lt_cv_dlopen_libs="-dld"])
dnl X 	      ])
dnl X 	    ])
dnl X 	  ])
dnl X 	])
dnl X       ])
dnl X     ;;
dnl X   esac
dnl X 
dnl X   if test "x$lt_cv_dlopen" != xno; then
dnl X     enable_dlopen=yes
dnl X   else
dnl X     enable_dlopen=no
dnl X   fi
dnl X 
dnl X   case $lt_cv_dlopen in
dnl X   dlopen)
dnl X     save_CPPFLAGS="$CPPFLAGS"
dnl X     AC_REQUIRE([_LT_AC_CHECK_DLFCN])dnl
dnl X     test "x$ac_cv_header_dlfcn_h" = xyes && CPPFLAGS="$CPPFLAGS -DHAVE_DLFCN_H"
dnl X 
dnl X     save_LDFLAGS="$LDFLAGS"
dnl X     eval LDFLAGS=\"\$LDFLAGS $export_dynamic_flag_spec\"
dnl X 
dnl X     save_LIBS="$LIBS"
dnl X     LIBS="$lt_cv_dlopen_libs $LIBS"
dnl X 
dnl X     AC_CACHE_CHECK([whether a program can dlopen itself],
dnl X 	  lt_cv_dlopen_self, [dnl
dnl X 	  _LT_AC_TRY_DLOPEN_SELF(
dnl X 	    lt_cv_dlopen_self=yes, lt_cv_dlopen_self=yes,
dnl X 	    lt_cv_dlopen_self=no, lt_cv_dlopen_self=cross)
dnl X     ])
dnl X 
dnl X     if test "x$lt_cv_dlopen_self" = xyes; then
dnl X       LDFLAGS="$LDFLAGS $link_static_flag"
dnl X       AC_CACHE_CHECK([whether a statically linked program can dlopen itself],
dnl X     	  lt_cv_dlopen_self_static, [dnl
dnl X 	  _LT_AC_TRY_DLOPEN_SELF(
dnl X 	    lt_cv_dlopen_self_static=yes, lt_cv_dlopen_self_static=yes,
dnl X 	    lt_cv_dlopen_self_static=no,  lt_cv_dlopen_self_static=cross)
dnl X       ])
dnl X     fi
dnl X 
dnl X     CPPFLAGS="$save_CPPFLAGS"
dnl X     LDFLAGS="$save_LDFLAGS"
dnl X     LIBS="$save_LIBS"
dnl X     ;;
dnl X   esac
dnl X 
dnl X   case $lt_cv_dlopen_self in
dnl X   yes|no) enable_dlopen_self=$lt_cv_dlopen_self ;;
dnl X   *) enable_dlopen_self=unknown ;;
dnl X   esac
dnl X 
dnl X   case $lt_cv_dlopen_self_static in
dnl X   yes|no) enable_dlopen_self_static=$lt_cv_dlopen_self_static ;;
dnl X   *) enable_dlopen_self_static=unknown ;;
dnl X   esac
dnl X fi
dnl X ])# AC_LIBTOOL_DLOPEN_SELF
dnl X 
dnl X AC_DEFUN([_LT_AC_LTCONFIG_HACK],
dnl X [AC_REQUIRE([AC_LIBTOOL_SYS_GLOBAL_SYMBOL_PIPE])dnl
dnl X # Sed substitution that helps us do robust quoting.  It backslashifies
dnl X # metacharacters that are still active within double-quoted strings.
dnl X Xsed='sed -e s/^X//'
dnl X sed_quote_subst='s/\([[\\"\\`$\\\\]]\)/\\\1/g'
dnl X 
dnl X # Same as above, but do not quote variable references.
dnl X double_quote_subst='s/\([[\\"\\`\\\\]]\)/\\\1/g'
dnl X 
dnl X # Sed substitution to delay expansion of an escaped shell variable in a
dnl X # double_quote_subst'ed string.
dnl X delay_variable_subst='s/\\\\\\\\\\\$/\\\\\\$/g'
dnl X 
dnl X # Constants:
dnl X rm="rm -f"
dnl X 
dnl X # Global variables:
dnl X default_ofile=libtool
dnl X can_build_shared=yes
dnl X 
dnl X # All known linkers require a `.a' archive for static linking (except M$VC,
dnl X # which needs '.lib').
dnl X libext=a
dnl X ltmain="$ac_aux_dir/ltmain.sh"
dnl X ofile="$default_ofile"
dnl X with_gnu_ld="$lt_cv_prog_gnu_ld"
dnl X need_locks="$enable_libtool_lock"
dnl X 
dnl X old_CC="$CC"
dnl X old_CFLAGS="$CFLAGS"
dnl X 
dnl X # Set sane defaults for various variables
dnl X test -z "$AR" && AR=ar
dnl X test -z "$AR_FLAGS" && AR_FLAGS=cru
dnl X test -z "$AS" && AS=as
dnl X test -z "$CC" && CC=cc
dnl X test -z "$DLLTOOL" && DLLTOOL=dlltool
dnl X test -z "$LD" && LD=ld
dnl X test -z "$LN_S" && LN_S="ln -s"
dnl X test -z "$MAGIC_CMD" && MAGIC_CMD=file
dnl X test -z "$NM" && NM=nm
dnl X test -z "$OBJDUMP" && OBJDUMP=objdump
dnl X test -z "$RANLIB" && RANLIB=:
dnl X test -z "$STRIP" && STRIP=:
dnl X test -z "$ac_objext" && ac_objext=o
dnl X 
dnl X if test x"$host" != x"$build"; then
dnl X   ac_tool_prefix=${host_alias}-
dnl X else
dnl X   ac_tool_prefix=
dnl X fi
dnl X 
dnl X # Transform linux* to *-*-linux-gnu*, to support old configure scripts.
dnl X case $host_os in
dnl X linux-gnu*) ;;
dnl X linux*) host=`echo $host | sed 's/^\(.*-.*-linux\)\(.*\)$/\1-gnu\2/'`
dnl X esac
dnl X 
dnl X case $host_os in
dnl X aix3*)
dnl X   # AIX sometimes has problems with the GCC collect2 program.  For some
dnl X   # reason, if we set the COLLECT_NAMES environment variable, the problems
dnl X   # vanish in a puff of smoke.
dnl X   if test "X${COLLECT_NAMES+set}" != Xset; then
dnl X     COLLECT_NAMES=
dnl X     export COLLECT_NAMES
dnl X   fi
dnl X   ;;
dnl X esac
dnl X 
dnl X # Determine commands to create old-style static archives.
dnl X old_archive_cmds='$AR $AR_FLAGS $oldlib$oldobjs$old_deplibs'
dnl X old_postinstall_cmds='chmod 644 $oldlib'
dnl X old_postuninstall_cmds=
dnl X 
dnl X if test -n "$RANLIB"; then
dnl X   case $host_os in
dnl X   openbsd*)
dnl X     old_postinstall_cmds="\$RANLIB -t \$oldlib~$old_postinstall_cmds"
dnl X     ;;
dnl X   *)
dnl X     old_postinstall_cmds="\$RANLIB \$oldlib~$old_postinstall_cmds"
dnl X     ;;
dnl X   esac
dnl X   old_archive_cmds="$old_archive_cmds~\$RANLIB \$oldlib"
dnl X fi
dnl X 
dnl X # Allow CC to be a program name with arguments.
dnl X set dummy $CC
dnl X compiler="[$]2"
dnl X 
dnl X ## FIXME: this should be a separate macro
dnl X ##
dnl X AC_MSG_CHECKING([for objdir])
dnl X rm -f .libs 2>/dev/null
dnl X mkdir .libs 2>/dev/null
dnl X if test -d .libs; then
dnl X   objdir=.libs
dnl X else
dnl X   # MS-DOS does not allow filenames that begin with a dot.
dnl X   objdir=_libs
dnl X fi
dnl X rmdir .libs 2>/dev/null
dnl X AC_MSG_RESULT($objdir)
dnl X ##
dnl X ## END FIXME
dnl X 
dnl X 
dnl X ## FIXME: this should be a separate macro
dnl X ##
dnl X AC_ARG_WITH(pic,
dnl X [  --with-pic              try to use only PIC/non-PIC objects [default=use both]],
dnl X pic_mode="$withval", pic_mode=default)
dnl X test -z "$pic_mode" && pic_mode=default
dnl X 
dnl X # We assume here that the value for lt_cv_prog_cc_pic will not be cached
dnl X # in isolation, and that seeing it set (from the cache) indicates that
dnl X # the associated values are set (in the cache) correctly too.
dnl X AC_MSG_CHECKING([for $compiler option to produce PIC])
dnl X AC_CACHE_VAL(lt_cv_prog_cc_pic,
dnl X [ lt_cv_prog_cc_pic=
dnl X   lt_cv_prog_cc_shlib=
dnl X   lt_cv_prog_cc_wl=
dnl X   lt_cv_prog_cc_static=
dnl X   lt_cv_prog_cc_no_builtin=
dnl X   lt_cv_prog_cc_can_build_shared=$can_build_shared
dnl X 
dnl X   if test "$GCC" = yes; then
dnl X     lt_cv_prog_cc_wl='-Wl,'
dnl X     lt_cv_prog_cc_static='-static'
dnl X 
dnl X     case $host_os in
dnl X     aix*)
dnl X       # Below there is a dirty hack to force normal static linking with -ldl
dnl X       # The problem is because libdl dynamically linked with both libc and
dnl X       # libC (AIX C++ library), which obviously doesn't included in libraries
dnl X       # list by gcc. This cause undefined symbols with -static flags.
dnl X       # This hack allows C programs to be linked with "-static -ldl", but
dnl X       # not sure about C++ programs.
dnl X       lt_cv_prog_cc_static="$lt_cv_prog_cc_static ${lt_cv_prog_cc_wl}-lC"
dnl X       ;;
dnl X     amigaos*)
dnl X       # FIXME: we need at least 68020 code to build shared libraries, but
dnl X       # adding the `-m68020' flag to GCC prevents building anything better,
dnl X       # like `-m68040'.
dnl X       lt_cv_prog_cc_pic='-m68020 -resident32 -malways-restore-a4'
dnl X       ;;
dnl X     beos* | irix5* | irix6* | nonstopux* | osf3* | osf4* | osf5*)
dnl X       # PIC is the default for these OSes.
dnl X       ;;
dnl X     darwin* | rhapsody*)
dnl X       # PIC is the default on this platform
dnl X       # Common symbols not allowed in MH_DYLIB files
dnl X       lt_cv_prog_cc_pic='-fno-common'
dnl X       ;;
dnl X     cygwin* | mingw* | pw32* | os2*)
dnl X       # This hack is so that the source file can tell whether it is being
dnl X       # built for inclusion in a dll (and should export symbols for example).
dnl X       lt_cv_prog_cc_pic='-DDLL_EXPORT'
dnl X       ;;
dnl X     sysv4*MP*)
dnl X       if test -d /usr/nec; then
dnl X 	 lt_cv_prog_cc_pic=-Kconform_pic
dnl X       fi
dnl X       ;;
dnl X     *)
dnl X       lt_cv_prog_cc_pic='-fPIC'
dnl X       ;;
dnl X     esac
dnl X   else
dnl X     # PORTME Check for PIC flags for the system compiler.
dnl X     case $host_os in
dnl X     aix3* | aix4* | aix5*)
dnl X       lt_cv_prog_cc_wl='-Wl,'
dnl X       # All AIX code is PIC.
dnl X       if test "$host_cpu" = ia64; then
dnl X 	# AIX 5 now supports IA64 processor
dnl X 	lt_cv_prog_cc_static='-Bstatic'
dnl X       else
dnl X 	lt_cv_prog_cc_static='-bnso -bI:/lib/syscalls.exp'
dnl X       fi
dnl X       ;;
dnl X 
dnl X     hpux9* | hpux10* | hpux11*)
dnl X       # Is there a better lt_cv_prog_cc_static that works with the bundled CC?
dnl X       lt_cv_prog_cc_wl='-Wl,'
dnl X       lt_cv_prog_cc_static="${lt_cv_prog_cc_wl}-a ${lt_cv_prog_cc_wl}archive"
dnl X       lt_cv_prog_cc_pic='+Z'
dnl X       ;;
dnl X 
dnl X     irix5* | irix6* | nonstopux*)
dnl X       lt_cv_prog_cc_wl='-Wl,'
dnl X       lt_cv_prog_cc_static='-non_shared'
dnl X       # PIC (with -KPIC) is the default.
dnl X       ;;
dnl X 
dnl X     cygwin* | mingw* | pw32* | os2*)
dnl X       # This hack is so that the source file can tell whether it is being
dnl X       # built for inclusion in a dll (and should export symbols for example).
dnl X       lt_cv_prog_cc_pic='-DDLL_EXPORT'
dnl X       ;;
dnl X 
dnl X     newsos6)
dnl X       lt_cv_prog_cc_pic='-KPIC'
dnl X       lt_cv_prog_cc_static='-Bstatic'
dnl X       ;;
dnl X 
dnl X     osf3* | osf4* | osf5*)
dnl X       # All OSF/1 code is PIC.
dnl X       lt_cv_prog_cc_wl='-Wl,'
dnl X       lt_cv_prog_cc_static='-non_shared'
dnl X       ;;
dnl X 
dnl X     sco3.2v5*)
dnl X       lt_cv_prog_cc_pic='-Kpic'
dnl X       lt_cv_prog_cc_static='-dn'
dnl X       lt_cv_prog_cc_shlib='-belf'
dnl X       ;;
dnl X 
dnl X     solaris*)
dnl X       lt_cv_prog_cc_pic='-KPIC'
dnl X       lt_cv_prog_cc_static='-Bstatic'
dnl X       lt_cv_prog_cc_wl='-Wl,'
dnl X       ;;
dnl X 
dnl X     sunos4*)
dnl X       lt_cv_prog_cc_pic='-PIC'
dnl X       lt_cv_prog_cc_static='-Bstatic'
dnl X       lt_cv_prog_cc_wl='-Qoption ld '
dnl X       ;;
dnl X 
dnl X     sysv4 | sysv4.2uw2* | sysv4.3* | sysv5*)
dnl X       lt_cv_prog_cc_pic='-KPIC'
dnl X       lt_cv_prog_cc_static='-Bstatic'
dnl X       lt_cv_prog_cc_wl='-Wl,'
dnl X       ;;
dnl X 
dnl X     uts4*)
dnl X       lt_cv_prog_cc_pic='-pic'
dnl X       lt_cv_prog_cc_static='-Bstatic'
dnl X       ;;
dnl X 
dnl X     sysv4*MP*)
dnl X       if test -d /usr/nec ;then
dnl X 	lt_cv_prog_cc_pic='-Kconform_pic'
dnl X 	lt_cv_prog_cc_static='-Bstatic'
dnl X       fi
dnl X       ;;
dnl X 
dnl X     *)
dnl X       lt_cv_prog_cc_can_build_shared=no
dnl X       ;;
dnl X     esac
dnl X   fi
dnl X ])
dnl X if test -z "$lt_cv_prog_cc_pic"; then
dnl X   AC_MSG_RESULT([none])
dnl X else
dnl X   AC_MSG_RESULT([$lt_cv_prog_cc_pic])
dnl X 
dnl X   # Check to make sure the pic_flag actually works.
dnl X   AC_MSG_CHECKING([if $compiler PIC flag $lt_cv_prog_cc_pic works])
dnl X   AC_CACHE_VAL(lt_cv_prog_cc_pic_works, [dnl
dnl X     save_CFLAGS="$CFLAGS"
dnl X     CFLAGS="$CFLAGS $lt_cv_prog_cc_pic -DPIC"
dnl X     AC_TRY_COMPILE([], [], [dnl
dnl X       case $host_os in
dnl X       hpux9* | hpux10* | hpux11*)
dnl X 	# On HP-UX, both CC and GCC only warn that PIC is supported... then
dnl X 	# they create non-PIC objects.  So, if there were any warnings, we
dnl X 	# assume that PIC is not supported.
dnl X 	if test -s conftest.err; then
dnl X 	  lt_cv_prog_cc_pic_works=no
dnl X 	else
dnl X 	  lt_cv_prog_cc_pic_works=yes
dnl X 	fi
dnl X 	;;
dnl X       *)
dnl X 	lt_cv_prog_cc_pic_works=yes
dnl X 	;;
dnl X       esac
dnl X     ], [dnl
dnl X       lt_cv_prog_cc_pic_works=no
dnl X     ])
dnl X     CFLAGS="$save_CFLAGS"
dnl X   ])
dnl X 
dnl X   if test "X$lt_cv_prog_cc_pic_works" = Xno; then
dnl X     lt_cv_prog_cc_pic=
dnl X     lt_cv_prog_cc_can_build_shared=no
dnl X   else
dnl X     lt_cv_prog_cc_pic=" $lt_cv_prog_cc_pic"
dnl X   fi
dnl X 
dnl X   AC_MSG_RESULT([$lt_cv_prog_cc_pic_works])
dnl X fi
dnl X ##
dnl X ## END FIXME
dnl X 
dnl X # Check for any special shared library compilation flags.
dnl X if test -n "$lt_cv_prog_cc_shlib"; then
dnl X   AC_MSG_WARN([\`$CC' requires \`$lt_cv_prog_cc_shlib' to build shared libraries])
dnl X   if echo "$old_CC $old_CFLAGS " | egrep -e "[[ 	]]$lt_cv_prog_cc_shlib[[ 	]]" >/dev/null; then :
dnl X   else
dnl X    AC_MSG_WARN([add \`$lt_cv_prog_cc_shlib' to the CC or CFLAGS env variable and reconfigure])
dnl X     lt_cv_prog_cc_can_build_shared=no
dnl X   fi
dnl X fi
dnl X 
dnl X ## FIXME: this should be a separate macro
dnl X ##
dnl X AC_MSG_CHECKING([if $compiler static flag $lt_cv_prog_cc_static works])
dnl X AC_CACHE_VAL([lt_cv_prog_cc_static_works], [dnl
dnl X   lt_cv_prog_cc_static_works=no
dnl X   save_LDFLAGS="$LDFLAGS"
dnl X   LDFLAGS="$LDFLAGS $lt_cv_prog_cc_static"
dnl X   AC_TRY_LINK([], [], [lt_cv_prog_cc_static_works=yes])
dnl X   LDFLAGS="$save_LDFLAGS"
dnl X ])
dnl X 
dnl X # Belt *and* braces to stop my trousers falling down:
dnl X test "X$lt_cv_prog_cc_static_works" = Xno && lt_cv_prog_cc_static=
dnl X AC_MSG_RESULT([$lt_cv_prog_cc_static_works])
dnl X 
dnl X pic_flag="$lt_cv_prog_cc_pic"
dnl X special_shlib_compile_flags="$lt_cv_prog_cc_shlib"
dnl X wl="$lt_cv_prog_cc_wl"
dnl X link_static_flag="$lt_cv_prog_cc_static"
dnl X no_builtin_flag="$lt_cv_prog_cc_no_builtin"
dnl X can_build_shared="$lt_cv_prog_cc_can_build_shared"
dnl X ##
dnl X ## END FIXME
dnl X 
dnl X 
dnl X ## FIXME: this should be a separate macro
dnl X ##
dnl X # Check to see if options -o and -c are simultaneously supported by compiler
dnl X AC_MSG_CHECKING([if $compiler supports -c -o file.$ac_objext])
dnl X AC_CACHE_VAL([lt_cv_compiler_c_o], [
dnl X $rm -r conftest 2>/dev/null
dnl X mkdir conftest
dnl X cd conftest
dnl X echo "int some_variable = 0;" > conftest.$ac_ext
dnl X mkdir out
dnl X # According to Tom Tromey, Ian Lance Taylor reported there are C compilers
dnl X # that will create temporary files in the current directory regardless of
dnl X # the output directory.  Thus, making CWD read-only will cause this test
dnl X # to fail, enabling locking or at least warning the user not to do parallel
dnl X # builds.
dnl X chmod -w .
dnl X save_CFLAGS="$CFLAGS"
dnl X CFLAGS="$CFLAGS -o out/conftest2.$ac_objext"
dnl X compiler_c_o=no
dnl X if { (eval echo configure:__oline__: \"$ac_compile\") 1>&5; (eval $ac_compile) 2>out/conftest.err; } && test -s out/conftest2.$ac_objext; then
dnl X   # The compiler can only warn and ignore the option if not recognized
dnl X   # So say no if there are warnings
dnl X   if test -s out/conftest.err; then
dnl X     lt_cv_compiler_c_o=no
dnl X   else
dnl X     lt_cv_compiler_c_o=yes
dnl X   fi
dnl X else
dnl X   # Append any errors to the config.log.
dnl X   cat out/conftest.err 1>&AC_FD_CC
dnl X   lt_cv_compiler_c_o=no
dnl X fi
dnl X CFLAGS="$save_CFLAGS"
dnl X chmod u+w .
dnl X $rm conftest* out/*
dnl X rmdir out
dnl X cd ..
dnl X rmdir conftest
dnl X $rm -r conftest 2>/dev/null
dnl X ])
dnl X compiler_c_o=$lt_cv_compiler_c_o
dnl X AC_MSG_RESULT([$compiler_c_o])
dnl X 
dnl X if test x"$compiler_c_o" = x"yes"; then
dnl X   # Check to see if we can write to a .lo
dnl X   AC_MSG_CHECKING([if $compiler supports -c -o file.lo])
dnl X   AC_CACHE_VAL([lt_cv_compiler_o_lo], [
dnl X   lt_cv_compiler_o_lo=no
dnl X   save_CFLAGS="$CFLAGS"
dnl X   CFLAGS="$CFLAGS -c -o conftest.lo"
dnl X   save_objext="$ac_objext"
dnl X   ac_objext=lo
dnl X   AC_TRY_COMPILE([], [int some_variable = 0;], [dnl
dnl X     # The compiler can only warn and ignore the option if not recognized
dnl X     # So say no if there are warnings
dnl X     if test -s conftest.err; then
dnl X       lt_cv_compiler_o_lo=no
dnl X     else
dnl X       lt_cv_compiler_o_lo=yes
dnl X     fi
dnl X   ])
dnl X   ac_objext="$save_objext"
dnl X   CFLAGS="$save_CFLAGS"
dnl X   ])
dnl X   compiler_o_lo=$lt_cv_compiler_o_lo
dnl X   AC_MSG_RESULT([$compiler_o_lo])
dnl X else
dnl X   compiler_o_lo=no
dnl X fi
dnl X ##
dnl X ## END FIXME
dnl X 
dnl X ## FIXME: this should be a separate macro
dnl X ##
dnl X # Check to see if we can do hard links to lock some files if needed
dnl X hard_links="nottested"
dnl X if test "$compiler_c_o" = no && test "$need_locks" != no; then
dnl X   # do not overwrite the value of need_locks provided by the user
dnl X   AC_MSG_CHECKING([if we can lock with hard links])
dnl X   hard_links=yes
dnl X   $rm conftest*
dnl X   ln conftest.a conftest.b 2>/dev/null && hard_links=no
dnl X   touch conftest.a
dnl X   ln conftest.a conftest.b 2>&5 || hard_links=no
dnl X   ln conftest.a conftest.b 2>/dev/null && hard_links=no
dnl X   AC_MSG_RESULT([$hard_links])
dnl X   if test "$hard_links" = no; then
dnl X     AC_MSG_WARN([\`$CC' does not support \`-c -o', so \`make -j' may be unsafe])
dnl X     need_locks=warn
dnl X   fi
dnl X else
dnl X   need_locks=no
dnl X fi
dnl X ##
dnl X ## END FIXME
dnl X 
dnl X ## FIXME: this should be a separate macro
dnl X ##
dnl X if test "$GCC" = yes; then
dnl X   # Check to see if options -fno-rtti -fno-exceptions are supported by compiler
dnl X   AC_MSG_CHECKING([if $compiler supports -fno-rtti -fno-exceptions])
dnl X   echo "int some_variable = 0;" > conftest.$ac_ext
dnl X   save_CFLAGS="$CFLAGS"
dnl X   CFLAGS="$CFLAGS -fno-rtti -fno-exceptions -c conftest.$ac_ext"
dnl X   compiler_rtti_exceptions=no
dnl X   AC_TRY_COMPILE([], [int some_variable = 0;], [dnl
dnl X     # The compiler can only warn and ignore the option if not recognized
dnl X     # So say no if there are warnings
dnl X     if test -s conftest.err; then
dnl X       compiler_rtti_exceptions=no
dnl X     else
dnl X       compiler_rtti_exceptions=yes
dnl X     fi
dnl X   ])
dnl X   CFLAGS="$save_CFLAGS"
dnl X   AC_MSG_RESULT([$compiler_rtti_exceptions])
dnl X 
dnl X   if test "$compiler_rtti_exceptions" = "yes"; then
dnl X     no_builtin_flag=' -fno-builtin -fno-rtti -fno-exceptions'
dnl X   else
dnl X     no_builtin_flag=' -fno-builtin'
dnl X   fi
dnl X fi
dnl X ##
dnl X ## END FIXME
dnl X 
dnl X ## FIXME: this should be a separate macro
dnl X ##
dnl X # See if the linker supports building shared libraries.
dnl X AC_MSG_CHECKING([whether the linker ($LD) supports shared libraries])
dnl X 
dnl X allow_undefined_flag=
dnl X no_undefined_flag=
dnl X need_lib_prefix=unknown
dnl X need_version=unknown
dnl X # when you set need_version to no, make sure it does not cause -set_version
dnl X # flags to be left without arguments
dnl X archive_cmds=
dnl X archive_expsym_cmds=
dnl X old_archive_from_new_cmds=
dnl X old_archive_from_expsyms_cmds=
dnl X export_dynamic_flag_spec=
dnl X whole_archive_flag_spec=
dnl X thread_safe_flag_spec=
dnl X hardcode_into_libs=no
dnl X hardcode_libdir_flag_spec=
dnl X hardcode_libdir_separator=
dnl X hardcode_direct=no
dnl X hardcode_minus_L=no
dnl X hardcode_shlibpath_var=unsupported
dnl X runpath_var=
dnl X link_all_deplibs=unknown
dnl X always_export_symbols=no
dnl X export_symbols_cmds='$NM $libobjs $convenience | $global_symbol_pipe | sed '\''s/.* //'\'' | sort | uniq > $export_symbols'
dnl X # include_expsyms should be a list of space-separated symbols to be *always*
dnl X # included in the symbol list
dnl X include_expsyms=
dnl X # exclude_expsyms can be an egrep regular expression of symbols to exclude
dnl X # it will be wrapped by ` (' and `)$', so one must not match beginning or
dnl X # end of line.  Example: `a|bc|.*d.*' will exclude the symbols `a' and `bc',
dnl X # as well as any symbol that contains `d'.
dnl X exclude_expsyms="_GLOBAL_OFFSET_TABLE_"
dnl X # Although _GLOBAL_OFFSET_TABLE_ is a valid symbol C name, most a.out
dnl X # platforms (ab)use it in PIC code, but their linkers get confused if
dnl X # the symbol is explicitly referenced.  Since portable code cannot
dnl X # rely on this symbol name, it's probably fine to never include it in
dnl X # preloaded symbol tables.
dnl X extract_expsyms_cmds=
dnl X 
dnl X case $host_os in
dnl X cygwin* | mingw* | pw32*)
dnl X   # FIXME: the MSVC++ port hasn't been tested in a loooong time
dnl X   # When not using gcc, we currently assume that we are using
dnl X   # Microsoft Visual C++.
dnl X   if test "$GCC" != yes; then
dnl X     with_gnu_ld=no
dnl X   fi
dnl X   ;;
dnl X openbsd*)
dnl X   with_gnu_ld=no
dnl X   ;;
dnl X esac
dnl X 
dnl X ld_shlibs=yes
dnl X if test "$with_gnu_ld" = yes; then
dnl X   # If archive_cmds runs LD, not CC, wlarc should be empty
dnl X   wlarc='${wl}'
dnl X 
dnl X   # See if GNU ld supports shared libraries.
dnl X   case $host_os in
dnl X   aix3* | aix4* | aix5*)
dnl X     # On AIX, the GNU linker is very broken
dnl X     # Note:Check GNU linker on AIX 5-IA64 when/if it becomes available.
dnl X     ld_shlibs=no
dnl X     cat <<EOF 1>&2
dnl X 
dnl X *** Warning: the GNU linker, at least up to release 2.9.1, is reported
dnl X *** to be unable to reliably create shared libraries on AIX.
dnl X *** Therefore, libtool is disabling shared libraries support.  If you
dnl X *** really care for shared libraries, you may want to modify your PATH
dnl X *** so that a non-GNU linker is found, and then restart.
dnl X 
dnl X EOF
dnl X     ;;
dnl X 
dnl X   amigaos*)
dnl X     archive_cmds='$rm $output_objdir/a2ixlibrary.data~$echo "#define NAME $libname" > $output_objdir/a2ixlibrary.data~$echo "#define LIBRARY_ID 1" >> $output_objdir/a2ixlibrary.data~$echo "#define VERSION $major" >> $output_objdir/a2ixlibrary.data~$echo "#define REVISION $revision" >> $output_objdir/a2ixlibrary.data~$AR $AR_FLAGS $lib $libobjs~$RANLIB $lib~(cd $output_objdir && a2ixlibrary -32)'
dnl X     hardcode_libdir_flag_spec='-L$libdir'
dnl X     hardcode_minus_L=yes
dnl X 
dnl X     # Samuel A. Falvo II <kc5tja@dolphin.openprojects.net> reports
dnl X     # that the semantics of dynamic libraries on AmigaOS, at least up
dnl X     # to version 4, is to share data among multiple programs linked
dnl X     # with the same dynamic library.  Since this doesn't match the
dnl X     # behavior of shared libraries on other platforms, we can use
dnl X     # them.
dnl X     ld_shlibs=no
dnl X     ;;
dnl X 
dnl X   beos*)
dnl X     if $LD --help 2>&1 | egrep ': supported targets:.* elf' > /dev/null; then
dnl X       allow_undefined_flag=unsupported
dnl X       # Joseph Beckenbach <jrb3@best.com> says some releases of gcc
dnl X       # support --undefined.  This deserves some investigation.  FIXME
dnl X       archive_cmds='$CC -nostart $libobjs $deplibs $compiler_flags ${wl}-soname $wl$soname -o $lib'
dnl X     else
dnl X       ld_shlibs=no
dnl X     fi
dnl X     ;;
dnl X 
dnl X   cygwin* | mingw* | pw32*)
dnl X     # hardcode_libdir_flag_spec is actually meaningless, as there is
dnl X     # no search path for DLLs.
dnl X     hardcode_libdir_flag_spec='-L$libdir'
dnl X     allow_undefined_flag=unsupported
dnl X     always_export_symbols=yes
dnl X 
dnl X     extract_expsyms_cmds='test -f $output_objdir/impgen.c || \
dnl X       sed -e "/^# \/\* impgen\.c starts here \*\//,/^# \/\* impgen.c ends here \*\// { s/^# //;s/^# *$//; p; }" -e d < $''0 > $output_objdir/impgen.c~
dnl X       test -f $output_objdir/impgen.exe || (cd $output_objdir && \
dnl X       if test "x$HOST_CC" != "x" ; then $HOST_CC -o impgen impgen.c ; \
dnl X       else $CC -o impgen impgen.c ; fi)~
dnl X       $output_objdir/impgen $dir/$soroot > $output_objdir/$soname-def'
dnl X 
dnl X     old_archive_from_expsyms_cmds='$DLLTOOL --as=$AS --dllname $soname --def $output_objdir/$soname-def --output-lib $output_objdir/$newlib'
dnl X 
dnl X     # cygwin and mingw dlls have different entry points and sets of symbols
dnl X     # to exclude.
dnl X     # FIXME: what about values for MSVC?
dnl X     dll_entry=__cygwin_dll_entry@12
dnl X     dll_exclude_symbols=DllMain@12,_cygwin_dll_entry@12,_cygwin_noncygwin_dll_entry@12~
dnl X     case $host_os in
dnl X     mingw*)
dnl X       # mingw values
dnl X       dll_entry=_DllMainCRTStartup@12
dnl X       dll_exclude_symbols=DllMain@12,DllMainCRTStartup@12,DllEntryPoint@12~
dnl X       ;;
dnl X     esac
dnl X 
dnl X     # mingw and cygwin differ, and it's simplest to just exclude the union
dnl X     # of the two symbol sets.
dnl X     dll_exclude_symbols=DllMain@12,_cygwin_dll_entry@12,_cygwin_noncygwin_dll_entry@12,DllMainCRTStartup@12,DllEntryPoint@12
dnl X 
dnl X     # recent cygwin and mingw systems supply a stub DllMain which the user
dnl X     # can override, but on older systems we have to supply one (in ltdll.c)
dnl X     if test "x$lt_cv_need_dllmain" = "xyes"; then
dnl X       ltdll_obj='$output_objdir/$soname-ltdll.'"$ac_objext "
dnl X       ltdll_cmds='test -f $output_objdir/$soname-ltdll.c || sed -e "/^# \/\* ltdll\.c starts here \*\//,/^# \/\* ltdll.c ends here \*\// { s/^# //; p; }" -e d < $''0 > $output_objdir/$soname-ltdll.c~
dnl X 	test -f $output_objdir/$soname-ltdll.$ac_objext || (cd $output_objdir && $CC -c $soname-ltdll.c)~'
dnl X     else
dnl X       ltdll_obj=
dnl X       ltdll_cmds=
dnl X     fi
dnl X 
dnl X     # Extract the symbol export list from an `--export-all' def file,
dnl X     # then regenerate the def file from the symbol export list, so that
dnl X     # the compiled dll only exports the symbol export list.
dnl X     # Be careful not to strip the DATA tag left be newer dlltools.
dnl X     export_symbols_cmds="$ltdll_cmds"'
dnl X       $DLLTOOL --export-all --exclude-symbols '$dll_exclude_symbols' --output-def $output_objdir/$soname-def '$ltdll_obj'$libobjs $convenience~
dnl X       sed -e "1,/EXPORTS/d" -e "s/ @ [[0-9]]*//" -e "s/ *;.*$//" < $output_objdir/$soname-def > $export_symbols'
dnl X 
dnl X     # If the export-symbols file already is a .def file (1st line
dnl X     # is EXPORTS), use it as is.
dnl X     # If DATA tags from a recent dlltool are present, honour them!
dnl X     archive_expsym_cmds='if test "x`sed 1q $export_symbols`" = xEXPORTS; then
dnl X 	cp $export_symbols $output_objdir/$soname-def;
dnl X       else
dnl X 	echo EXPORTS > $output_objdir/$soname-def;
dnl X 	_lt_hint=1;
dnl X 	cat $export_symbols | while read symbol; do
dnl X 	 set dummy \$symbol;
dnl X 	 case \[$]# in
dnl X 	   2) echo "   \[$]2 @ \$_lt_hint ; " >> $output_objdir/$soname-def;;
dnl X 	   4) echo "   \[$]2 \[$]3 \[$]4 ; " >> $output_objdir/$soname-def; _lt_hint=`expr \$_lt_hint - 1`;;
dnl X 	   *) echo "     \[$]2 @ \$_lt_hint \[$]3 ; " >> $output_objdir/$soname-def;;
dnl X 	 esac;
dnl X 	 _lt_hint=`expr 1 + \$_lt_hint`;
dnl X 	done;
dnl X       fi~
dnl X       '"$ltdll_cmds"'
dnl X       $CC -Wl,--base-file,$output_objdir/$soname-base '$lt_cv_cc_dll_switch' -Wl,-e,'$dll_entry' -o $output_objdir/$soname '$ltdll_obj'$libobjs $deplibs $compiler_flags~
dnl X       $DLLTOOL --as=$AS --dllname $soname --exclude-symbols '$dll_exclude_symbols' --def $output_objdir/$soname-def --base-file $output_objdir/$soname-base --output-exp $output_objdir/$soname-exp~
dnl X       $CC -Wl,--base-file,$output_objdir/$soname-base $output_objdir/$soname-exp '$lt_cv_cc_dll_switch' -Wl,-e,'$dll_entry' -o $output_objdir/$soname '$ltdll_obj'$libobjs $deplibs $compiler_flags~
dnl X       $DLLTOOL --as=$AS --dllname $soname --exclude-symbols '$dll_exclude_symbols' --def $output_objdir/$soname-def --base-file $output_objdir/$soname-base --output-exp $output_objdir/$soname-exp --output-lib $output_objdir/$libname.dll.a~
dnl X       $CC $output_objdir/$soname-exp '$lt_cv_cc_dll_switch' -Wl,-e,'$dll_entry' -o $output_objdir/$soname '$ltdll_obj'$libobjs $deplibs $compiler_flags'
dnl X     ;;
dnl X 
dnl X   netbsd*)
dnl X     if echo __ELF__ | $CC -E - | grep __ELF__ >/dev/null; then
dnl X       archive_cmds='$LD -Bshareable $libobjs $deplibs $linker_flags -o $lib'
dnl X       wlarc=
dnl X     else
dnl X       archive_cmds='$CC -shared -nodefaultlibs $libobjs $deplibs $compiler_flags ${wl}-soname $wl$soname -o $lib'
dnl X       archive_expsym_cmds='$CC -shared -nodefaultlibs $libobjs $deplibs $compiler_flags ${wl}-soname $wl$soname ${wl}-retain-symbols-file $wl$export_symbols -o $lib'
dnl X     fi
dnl X     ;;
dnl X 
dnl X   solaris* | sysv5*)
dnl X     if $LD -v 2>&1 | egrep 'BFD 2\.8' > /dev/null; then
dnl X       ld_shlibs=no
dnl X       cat <<EOF 1>&2
dnl X 
dnl X *** Warning: The releases 2.8.* of the GNU linker cannot reliably
dnl X *** create shared libraries on Solaris systems.  Therefore, libtool
dnl X *** is disabling shared libraries support.  We urge you to upgrade GNU
dnl X *** binutils to release 2.9.1 or newer.  Another option is to modify
dnl X *** your PATH or compiler configuration so that the native linker is
dnl X *** used, and then restart.
dnl X 
dnl X EOF
dnl X     elif $LD --help 2>&1 | egrep ': supported targets:.* elf' > /dev/null; then
dnl X       archive_cmds='$CC -shared $libobjs $deplibs $compiler_flags ${wl}-soname $wl$soname -o $lib'
dnl X       archive_expsym_cmds='$CC -shared $libobjs $deplibs $compiler_flags ${wl}-soname $wl$soname ${wl}-retain-symbols-file $wl$export_symbols -o $lib'
dnl X     else
dnl X       ld_shlibs=no
dnl X     fi
dnl X     ;;
dnl X 
dnl X   sunos4*)
dnl X     archive_cmds='$LD -assert pure-text -Bshareable -o $lib $libobjs $deplibs $linker_flags'
dnl X     wlarc=
dnl X     hardcode_direct=yes
dnl X     hardcode_shlibpath_var=no
dnl X     ;;
dnl X 
dnl X   *)
dnl X     if $LD --help 2>&1 | egrep ': supported targets:.* elf' > /dev/null; then
dnl X       archive_cmds='$CC -shared $libobjs $deplibs $compiler_flags ${wl}-soname $wl$soname -o $lib'
dnl X       archive_expsym_cmds='$CC -shared $libobjs $deplibs $compiler_flags ${wl}-soname $wl$soname ${wl}-retain-symbols-file $wl$export_symbols -o $lib'
dnl X     else
dnl X       ld_shlibs=no
dnl X     fi
dnl X     ;;
dnl X   esac
dnl X 
dnl X   if test "$ld_shlibs" = yes; then
dnl X     runpath_var=LD_RUN_PATH
dnl X     hardcode_libdir_flag_spec='${wl}--rpath ${wl}$libdir'
dnl X     export_dynamic_flag_spec='${wl}--export-dynamic'
dnl X     case $host_os in
dnl X     cygwin* | mingw* | pw32*)
dnl X       # dlltool doesn't understand --whole-archive et. al.
dnl X       whole_archive_flag_spec=
dnl X       ;;
dnl X     *)
dnl X       # ancient GNU ld didn't support --whole-archive et. al.
dnl X       if $LD --help 2>&1 | egrep 'no-whole-archive' > /dev/null; then
dnl X 	whole_archive_flag_spec="$wlarc"'--whole-archive$convenience '"$wlarc"'--no-whole-archive'
dnl X       else
dnl X 	whole_archive_flag_spec=
dnl X       fi
dnl X       ;;
dnl X     esac
dnl X   fi
dnl X else
dnl X   # PORTME fill in a description of your system's linker (not GNU ld)
dnl X   case $host_os in
dnl X   aix3*)
dnl X     allow_undefined_flag=unsupported
dnl X     always_export_symbols=yes
dnl X     archive_expsym_cmds='$LD -o $output_objdir/$soname $libobjs $deplibs $linker_flags -bE:$export_symbols -T512 -H512 -bM:SRE~$AR $AR_FLAGS $lib $output_objdir/$soname'
dnl X     # Note: this linker hardcodes the directories in LIBPATH if there
dnl X     # are no directories specified by -L.
dnl X     hardcode_minus_L=yes
dnl X     if test "$GCC" = yes && test -z "$link_static_flag"; then
dnl X       # Neither direct hardcoding nor static linking is supported with a
dnl X       # broken collect2.
dnl X       hardcode_direct=unsupported
dnl X     fi
dnl X     ;;
dnl X 
dnl X   aix4* | aix5*)
dnl X     if test "$host_cpu" = ia64; then
dnl X       # On IA64, the linker does run time linking by default, so we don't
dnl X       # have to do anything special.
dnl X       aix_use_runtimelinking=no
dnl X       exp_sym_flag='-Bexport'
dnl X       no_entry_flag=""
dnl X     else
dnl X       aix_use_runtimelinking=no
dnl X 
dnl X       # Test if we are trying to use run time linking or normal
dnl X       # AIX style linking. If -brtl is somewhere in LDFLAGS, we
dnl X       # need to do runtime linking.
dnl X       case $host_os in aix4.[[23]]|aix4.[[23]].*|aix5*)
dnl X 	for ld_flag in $LDFLAGS; do
dnl X 	  case $ld_flag in
dnl X 	  *-brtl*)
dnl X 	    aix_use_runtimelinking=yes
dnl X 	    break
dnl X 	  ;;
dnl X 	  esac
dnl X 	done
dnl X       esac
dnl X 
dnl X       exp_sym_flag='-bexport'
dnl X       no_entry_flag='-bnoentry'
dnl X     fi
dnl X 
dnl X     # When large executables or shared objects are built, AIX ld can
dnl X     # have problems creating the table of contents.  If linking a library
dnl X     # or program results in "error TOC overflow" add -mminimal-toc to
dnl X     # CXXFLAGS/CFLAGS for g++/gcc.  In the cases where that is not
dnl X     # enough to fix the problem, add -Wl,-bbigtoc to LDFLAGS.
dnl X 
dnl X     hardcode_direct=yes
dnl X     archive_cmds=''
dnl X     hardcode_libdir_separator=':'
dnl X     if test "$GCC" = yes; then
dnl X       case $host_os in aix4.[[012]]|aix4.[[012]].*)
dnl X 	collect2name=`${CC} -print-prog-name=collect2`
dnl X 	if test -f "$collect2name" && \
dnl X 	  strings "$collect2name" | grep resolve_lib_name >/dev/null
dnl X 	then
dnl X 	  # We have reworked collect2
dnl X 	  hardcode_direct=yes
dnl X 	else
dnl X 	  # We have old collect2
dnl X 	  hardcode_direct=unsupported
dnl X 	  # It fails to find uninstalled libraries when the uninstalled
dnl X 	  # path is not listed in the libpath.  Setting hardcode_minus_L
dnl X 	  # to unsupported forces relinking
dnl X 	  hardcode_minus_L=yes
dnl X 	  hardcode_libdir_flag_spec='-L$libdir'
dnl X 	  hardcode_libdir_separator=
dnl X 	fi
dnl X       esac
dnl X 
dnl X       shared_flag='-shared'
dnl X     else
dnl X       # not using gcc
dnl X       if test "$host_cpu" = ia64; then
dnl X 	shared_flag='${wl}-G'
dnl X       else
dnl X 	if test "$aix_use_runtimelinking" = yes; then
dnl X 	  shared_flag='${wl}-G'
dnl X 	else
dnl X 	  shared_flag='${wl}-bM:SRE'
dnl X 	fi
dnl X       fi
dnl X     fi
dnl X 
dnl X     # It seems that -bexpall can do strange things, so it is better to
dnl X     # generate a list of symbols to export.
dnl X     always_export_symbols=yes
dnl X     if test "$aix_use_runtimelinking" = yes; then
dnl X       # Warning - without using the other runtime loading flags (-brtl),
dnl X       # -berok will link without error, but may produce a broken library.
dnl X       allow_undefined_flag='-berok'
dnl X       hardcode_libdir_flag_spec='${wl}-blibpath:$libdir:/usr/lib:/lib'
dnl X       archive_expsym_cmds="\$CC"' -o $output_objdir/$soname $libobjs $deplibs $compiler_flags `if test "x${allow_undefined_flag}" != "x"; then echo "${wl}${allow_undefined_flag}"; else :; fi` '"\${wl}$no_entry_flag \${wl}$exp_sym_flag:\$export_symbols $shared_flag"
dnl X     else
dnl X       if test "$host_cpu" = ia64; then
dnl X 	hardcode_libdir_flag_spec='${wl}-R $libdir:/usr/lib:/lib'
dnl X 	allow_undefined_flag="-z nodefs"
dnl X 	archive_expsym_cmds="\$CC $shared_flag"' -o $output_objdir/$soname ${wl}-h$soname $libobjs $deplibs $compiler_flags ${wl}${allow_undefined_flag} '"\${wl}$no_entry_flag \${wl}$exp_sym_flag:\$export_symbols"
dnl X       else
dnl X 	hardcode_libdir_flag_spec='${wl}-bnolibpath ${wl}-blibpath:$libdir:/usr/lib:/lib'
dnl X 	# Warning - without using the other run time loading flags,
dnl X 	# -berok will link without error, but may produce a broken library.
dnl X 	allow_undefined_flag='${wl}-berok'
dnl X 	# This is a bit strange, but is similar to how AIX traditionally builds
dnl X 	# it's shared libraries.
dnl X 	archive_expsym_cmds="\$CC $shared_flag"' -o $output_objdir/$soname $libobjs $deplibs $compiler_flags ${allow_undefined_flag} '"\${wl}$no_entry_flag \${wl}$exp_sym_flag:\$export_symbols"' ~$AR -crlo $objdir/$libname$release.a $objdir/$soname'
dnl X       fi
dnl X     fi
dnl X     ;;
dnl X 
dnl X   amigaos*)
dnl X     archive_cmds='$rm $output_objdir/a2ixlibrary.data~$echo "#define NAME $libname" > $output_objdir/a2ixlibrary.data~$echo "#define LIBRARY_ID 1" >> $output_objdir/a2ixlibrary.data~$echo "#define VERSION $major" >> $output_objdir/a2ixlibrary.data~$echo "#define REVISION $revision" >> $output_objdir/a2ixlibrary.data~$AR $AR_FLAGS $lib $libobjs~$RANLIB $lib~(cd $output_objdir && a2ixlibrary -32)'
dnl X     hardcode_libdir_flag_spec='-L$libdir'
dnl X     hardcode_minus_L=yes
dnl X     # see comment about different semantics on the GNU ld section
dnl X     ld_shlibs=no
dnl X     ;;
dnl X 
dnl X   cygwin* | mingw* | pw32*)
dnl X     # When not using gcc, we currently assume that we are using
dnl X     # Microsoft Visual C++.
dnl X     # hardcode_libdir_flag_spec is actually meaningless, as there is
dnl X     # no search path for DLLs.
dnl X     hardcode_libdir_flag_spec=' '
dnl X     allow_undefined_flag=unsupported
dnl X     # Tell ltmain to make .lib files, not .a files.
dnl X     libext=lib
dnl X     # FIXME: Setting linknames here is a bad hack.
dnl X     archive_cmds='$CC -o $lib $libobjs $compiler_flags `echo "$deplibs" | sed -e '\''s/ -lc$//'\''` -link -dll~linknames='
dnl X     # The linker will automatically build a .lib file if we build a DLL.
dnl X     old_archive_from_new_cmds='true'
dnl X     # FIXME: Should let the user specify the lib program.
dnl X     old_archive_cmds='lib /OUT:$oldlib$oldobjs$old_deplibs'
dnl X     fix_srcfile_path='`cygpath -w "$srcfile"`'
dnl X     ;;
dnl X 
dnl X   darwin* | rhapsody*)
dnl X     case "$host_os" in
dnl X     rhapsody* | darwin1.[[012]])
dnl X       allow_undefined_flag='-undefined suppress'
dnl X       ;;
dnl X     *) # Darwin 1.3 on
dnl X       allow_undefined_flag='-flat_namespace -undefined suppress'
dnl X       ;;
dnl X     esac
dnl X     # FIXME: Relying on posixy $() will cause problems for
dnl X     #        cross-compilation, but unfortunately the echo tests do not
dnl X     #        yet detect zsh echo's removal of \ escapes.  Also zsh mangles
dnl X     #	     `"' quotes if we put them in here... so don't!
dnl X     archive_cmds='$CC -r -keep_private_externs -nostdlib -o ${lib}-master.o $libobjs && $CC $(test .$module = .yes && echo -bundle || echo -dynamiclib) $allow_undefined_flag -o $lib ${lib}-master.o $deplibs$linker_flags $(test .$module != .yes && echo -install_name $rpath/$soname $verstring)'
dnl X     # We need to add '_' to the symbols in $export_symbols first
dnl X     #archive_expsym_cmds="$archive_cmds"' && strip -s $export_symbols'
dnl X     hardcode_direct=yes
dnl X     hardcode_shlibpath_var=no
dnl X     whole_archive_flag_spec='-all_load $convenience'
dnl X     ;;
dnl X 
dnl X   freebsd1*)
dnl X     ld_shlibs=no
dnl X     ;;
dnl X 
dnl X   # FreeBSD 2.2.[012] allows us to include c++rt0.o to get C++ constructor
dnl X   # support.  Future versions do this automatically, but an explicit c++rt0.o
dnl X   # does not break anything, and helps significantly (at the cost of a little
dnl X   # extra space).
dnl X   freebsd2.2*)
dnl X     archive_cmds='$LD -Bshareable -o $lib $libobjs $deplibs $linker_flags /usr/lib/c++rt0.o'
dnl X     hardcode_libdir_flag_spec='-R$libdir'
dnl X     hardcode_direct=yes
dnl X     hardcode_shlibpath_var=no
dnl X     ;;
dnl X 
dnl X   # Unfortunately, older versions of FreeBSD 2 do not have this feature.
dnl X   freebsd2*)
dnl X     archive_cmds='$LD -Bshareable -o $lib $libobjs $deplibs $linker_flags'
dnl X     hardcode_direct=yes
dnl X     hardcode_minus_L=yes
dnl X     hardcode_shlibpath_var=no
dnl X     ;;
dnl X 
dnl X   # FreeBSD 3 and greater uses gcc -shared to do shared libraries.
dnl X   freebsd*)
dnl X     archive_cmds='$CC -shared -o $lib $libobjs $deplibs $compiler_flags'
dnl X     hardcode_libdir_flag_spec='-R$libdir'
dnl X     hardcode_direct=yes
dnl X     hardcode_shlibpath_var=no
dnl X     ;;
dnl X 
dnl X   hpux9* | hpux10* | hpux11*)
dnl X     case $host_os in
dnl X     hpux9*) archive_cmds='$rm $output_objdir/$soname~$LD -b +b $install_libdir -o $output_objdir/$soname $libobjs $deplibs $linker_flags~test $output_objdir/$soname = $lib || mv $output_objdir/$soname $lib' ;;
dnl X     *) archive_cmds='$LD -b +h $soname +b $install_libdir -o $lib $libobjs $deplibs $linker_flags' ;;
dnl X     esac
dnl X     hardcode_libdir_flag_spec='${wl}+b ${wl}$libdir'
dnl X     hardcode_libdir_separator=:
dnl X     hardcode_direct=yes
dnl X     hardcode_minus_L=yes # Not in the search PATH, but as the default
dnl X 			 # location of the library.
dnl X     export_dynamic_flag_spec='${wl}-E'
dnl X     ;;
dnl X 
dnl X   irix5* | irix6* | nonstopux*)
dnl X     if test "$GCC" = yes; then
dnl X       archive_cmds='$CC -shared $libobjs $deplibs $compiler_flags ${wl}-soname ${wl}$soname `test -n "$verstring" && echo ${wl}-set_version ${wl}$verstring` ${wl}-update_registry ${wl}${output_objdir}/so_locations -o $lib'
dnl X       hardcode_libdir_flag_spec='${wl}-rpath ${wl}$libdir'
dnl X     else
dnl X       archive_cmds='$LD -shared $libobjs $deplibs $linker_flags -soname $soname `test -n "$verstring" && echo -set_version $verstring` -update_registry ${output_objdir}/so_locations -o $lib'
dnl X       hardcode_libdir_flag_spec='-rpath $libdir'
dnl X     fi
dnl X     hardcode_libdir_separator=:
dnl X     link_all_deplibs=yes
dnl X     ;;
dnl X 
dnl X   netbsd*)
dnl X     if echo __ELF__ | $CC -E - | grep __ELF__ >/dev/null; then
dnl X       archive_cmds='$LD -Bshareable -o $lib $libobjs $deplibs $linker_flags'  # a.out
dnl X     else
dnl X       archive_cmds='$LD -shared -o $lib $libobjs $deplibs $linker_flags'      # ELF
dnl X     fi
dnl X     hardcode_libdir_flag_spec='-R$libdir'
dnl X     hardcode_direct=yes
dnl X     hardcode_shlibpath_var=no
dnl X     ;;
dnl X 
dnl X   newsos6)
dnl X     archive_cmds='$LD -G -h $soname -o $lib $libobjs $deplibs $linker_flags'
dnl X     hardcode_direct=yes
dnl X     hardcode_libdir_flag_spec='${wl}-rpath ${wl}$libdir'
dnl X     hardcode_libdir_separator=:
dnl X     hardcode_shlibpath_var=no
dnl X     ;;
dnl X 
dnl X   openbsd*)
dnl X     hardcode_direct=yes
dnl X     hardcode_shlibpath_var=no
dnl X     if test -z "`echo __ELF__ | $CC -E - | grep __ELF__`" || test "$host_os-$host_cpu" = "openbsd2.8-powerpc"; then
dnl X       archive_cmds='$CC -shared $pic_flag -o $lib $libobjs $deplibs $compiler_flags'
dnl X       hardcode_libdir_flag_spec='${wl}-rpath,$libdir'
dnl X       export_dynamic_flag_spec='${wl}-E'
dnl X     else
dnl X       case "$host_os" in
dnl X       openbsd[[01]].* | openbsd2.[[0-7]] | openbsd2.[[0-7]].*)
dnl X 	archive_cmds='$LD -Bshareable -o $lib $libobjs $deplibs $linker_flags'
dnl X 	hardcode_libdir_flag_spec='-R$libdir'
dnl X         ;;
dnl X       *)
dnl X         archive_cmds='$CC -shared $pic_flag -o $lib $libobjs $deplibs $compiler_flags'
dnl X         hardcode_libdir_flag_spec='${wl}-rpath,$libdir'
dnl X         ;;
dnl X       esac
dnl X     fi
dnl X     ;;
dnl X 
dnl X   os2*)
dnl X     hardcode_libdir_flag_spec='-L$libdir'
dnl X     hardcode_minus_L=yes
dnl X     allow_undefined_flag=unsupported
dnl X     archive_cmds='$echo "LIBRARY $libname INITINSTANCE" > $output_objdir/$libname.def~$echo "DESCRIPTION \"$libname\"" >> $output_objdir/$libname.def~$echo DATA >> $output_objdir/$libname.def~$echo " SINGLE NONSHARED" >> $output_objdir/$libname.def~$echo EXPORTS >> $output_objdir/$libname.def~emxexp $libobjs >> $output_objdir/$libname.def~$CC -Zdll -Zcrtdll -o $lib $libobjs $deplibs $compiler_flags $output_objdir/$libname.def'
dnl X     old_archive_from_new_cmds='emximp -o $output_objdir/$libname.a $output_objdir/$libname.def'
dnl X     ;;
dnl X 
dnl X   osf3*)
dnl X     if test "$GCC" = yes; then
dnl X       allow_undefined_flag=' ${wl}-expect_unresolved ${wl}\*'
dnl X       archive_cmds='$CC -shared${allow_undefined_flag} $libobjs $deplibs $compiler_flags ${wl}-soname ${wl}$soname `test -n "$verstring" && echo ${wl}-set_version ${wl}$verstring` ${wl}-update_registry ${wl}${output_objdir}/so_locations -o $lib'
dnl X     else
dnl X       allow_undefined_flag=' -expect_unresolved \*'
dnl X       archive_cmds='$LD -shared${allow_undefined_flag} $libobjs $deplibs $linker_flags -soname $soname `test -n "$verstring" && echo -set_version $verstring` -update_registry ${output_objdir}/so_locations -o $lib'
dnl X     fi
dnl X     hardcode_libdir_flag_spec='${wl}-rpath ${wl}$libdir'
dnl X     hardcode_libdir_separator=:
dnl X     ;;
dnl X 
dnl X   osf4* | osf5*)	# as osf3* with the addition of -msym flag
dnl X     if test "$GCC" = yes; then
dnl X       allow_undefined_flag=' ${wl}-expect_unresolved ${wl}\*'
dnl X       archive_cmds='$CC -shared${allow_undefined_flag} $libobjs $deplibs $compiler_flags ${wl}-msym ${wl}-soname ${wl}$soname `test -n "$verstring" && echo ${wl}-set_version ${wl}$verstring` ${wl}-update_registry ${wl}${output_objdir}/so_locations -o $lib'
dnl X       hardcode_libdir_flag_spec='${wl}-rpath ${wl}$libdir'
dnl X     else
dnl X       allow_undefined_flag=' -expect_unresolved \*'
dnl X       archive_cmds='$LD -shared${allow_undefined_flag} $libobjs $deplibs $linker_flags -msym -soname $soname `test -n "$verstring" && echo -set_version $verstring` -update_registry ${output_objdir}/so_locations -o $lib'
dnl X       archive_expsym_cmds='for i in `cat $export_symbols`; do printf "-exported_symbol " >> $lib.exp; echo "\$i" >> $lib.exp; done; echo "-hidden">> $lib.exp~
dnl X       $LD -shared${allow_undefined_flag} -input $lib.exp $linker_flags $libobjs $deplibs -soname $soname `test -n "$verstring" && echo -set_version $verstring` -update_registry ${objdir}/so_locations -o $lib~$rm $lib.exp'
dnl X 
dnl X       #Both c and cxx compiler support -rpath directly
dnl X       hardcode_libdir_flag_spec='-rpath $libdir'
dnl X     fi
dnl X     hardcode_libdir_separator=:
dnl X     ;;
dnl X 
dnl X   sco3.2v5*)
dnl X     archive_cmds='$LD -G -h $soname -o $lib $libobjs $deplibs $linker_flags'
dnl X     hardcode_shlibpath_var=no
dnl X     runpath_var=LD_RUN_PATH
dnl X     hardcode_runpath_var=yes
dnl X     export_dynamic_flag_spec='${wl}-Bexport'
dnl X     ;;
dnl X 
dnl X   solaris*)
dnl X     # gcc --version < 3.0 without binutils cannot create self contained
dnl X     # shared libraries reliably, requiring libgcc.a to resolve some of
dnl X     # the object symbols generated in some cases.  Libraries that use
dnl X     # assert need libgcc.a to resolve __eprintf, for example.  Linking
dnl X     # a copy of libgcc.a into every shared library to guarantee resolving
dnl X     # such symbols causes other problems:  According to Tim Van Holder
dnl X     # <tim.van.holder@pandora.be>, C++ libraries end up with a separate
dnl X     # (to the application) exception stack for one thing.
dnl X     no_undefined_flag=' -z defs'
dnl X     if test "$GCC" = yes; then
dnl X       case `$CC --version 2>/dev/null` in
dnl X       [[12]].*)
dnl X 	cat <<EOF 1>&2
dnl X 
dnl X *** Warning: Releases of GCC earlier than version 3.0 cannot reliably
dnl X *** create self contained shared libraries on Solaris systems, without
dnl X *** introducing a dependency on libgcc.a.  Therefore, libtool is disabling
dnl X *** -no-undefined support, which will at least allow you to build shared
dnl X *** libraries.  However, you may find that when you link such libraries
dnl X *** into an application without using GCC, you have to manually add
dnl X *** \`gcc --print-libgcc-file-name\` to the link command.  We urge you to
dnl X *** upgrade to a newer version of GCC.  Another option is to rebuild your
dnl X *** current GCC to use the GNU linker from GNU binutils 2.9.1 or newer.
dnl X 
dnl X EOF
dnl X         no_undefined_flag=
dnl X 	;;
dnl X       esac
dnl X     fi
dnl X     # $CC -shared without GNU ld will not create a library from C++
dnl X     # object files and a static libstdc++, better avoid it by now
dnl X     archive_cmds='$LD -G${allow_undefined_flag} -h $soname -o $lib $libobjs $deplibs $linker_flags'
dnl X     archive_expsym_cmds='$echo "{ global:" > $lib.exp~cat $export_symbols | sed -e "s/\(.*\)/\1;/" >> $lib.exp~$echo "local: *; };" >> $lib.exp~
dnl X 		$LD -G${allow_undefined_flag} -M $lib.exp -h $soname -o $lib $libobjs $deplibs $linker_flags~$rm $lib.exp'
dnl X     hardcode_libdir_flag_spec='-R$libdir'
dnl X     hardcode_shlibpath_var=no
dnl X     case $host_os in
dnl X     solaris2.[[0-5]] | solaris2.[[0-5]].*) ;;
dnl X     *) # Supported since Solaris 2.6 (maybe 2.5.1?)
dnl X       whole_archive_flag_spec='-z allextract$convenience -z defaultextract' ;;
dnl X     esac
dnl X     link_all_deplibs=yes
dnl X     ;;
dnl X 
dnl X   sunos4*)
dnl X     if test "x$host_vendor" = xsequent; then
dnl X       # Use $CC to link under sequent, because it throws in some extra .o
dnl X       # files that make .init and .fini sections work.
dnl X       archive_cmds='$CC -G ${wl}-h $soname -o $lib $libobjs $deplibs $compiler_flags'
dnl X     else
dnl X       archive_cmds='$LD -assert pure-text -Bstatic -o $lib $libobjs $deplibs $linker_flags'
dnl X     fi
dnl X     hardcode_libdir_flag_spec='-L$libdir'
dnl X     hardcode_direct=yes
dnl X     hardcode_minus_L=yes
dnl X     hardcode_shlibpath_var=no
dnl X     ;;
dnl X 
dnl X   sysv4)
dnl X     case $host_vendor in
dnl X       sni)
dnl X         archive_cmds='$LD -G -h $soname -o $lib $libobjs $deplibs $linker_flags'
dnl X         hardcode_direct=yes # is this really true???
dnl X         ;;
dnl X       siemens)
dnl X         ## LD is ld it makes a PLAMLIB
dnl X         ## CC just makes a GrossModule.
dnl X         archive_cmds='$LD -G -o $lib $libobjs $deplibs $linker_flags'
dnl X         reload_cmds='$CC -r -o $output$reload_objs'
dnl X         hardcode_direct=no
dnl X         ;;
dnl X       motorola)
dnl X         archive_cmds='$LD -G -h $soname -o $lib $libobjs $deplibs $linker_flags'
dnl X         hardcode_direct=no #Motorola manual says yes, but my tests say they lie
dnl X         ;;
dnl X     esac
dnl X     runpath_var='LD_RUN_PATH'
dnl X     hardcode_shlibpath_var=no
dnl X     ;;
dnl X 
dnl X   sysv4.3*)
dnl X     archive_cmds='$LD -G -h $soname -o $lib $libobjs $deplibs $linker_flags'
dnl X     hardcode_shlibpath_var=no
dnl X     export_dynamic_flag_spec='-Bexport'
dnl X     ;;
dnl X 
dnl X   sysv5*)
dnl X     no_undefined_flag=' -z text'
dnl X     # $CC -shared without GNU ld will not create a library from C++
dnl X     # object files and a static libstdc++, better avoid it by now
dnl X     archive_cmds='$LD -G${allow_undefined_flag} -h $soname -o $lib $libobjs $deplibs $linker_flags'
dnl X     archive_expsym_cmds='$echo "{ global:" > $lib.exp~cat $export_symbols | sed -e "s/\(.*\)/\1;/" >> $lib.exp~$echo "local: *; };" >> $lib.exp~
dnl X 		$LD -G${allow_undefined_flag} -M $lib.exp -h $soname -o $lib $libobjs $deplibs $linker_flags~$rm $lib.exp'
dnl X     hardcode_libdir_flag_spec=
dnl X     hardcode_shlibpath_var=no
dnl X     runpath_var='LD_RUN_PATH'
dnl X     ;;
dnl X 
dnl X   uts4*)
dnl X     archive_cmds='$LD -G -h $soname -o $lib $libobjs $deplibs $linker_flags'
dnl X     hardcode_libdir_flag_spec='-L$libdir'
dnl X     hardcode_shlibpath_var=no
dnl X     ;;
dnl X 
dnl X   dgux*)
dnl X     archive_cmds='$LD -G -h $soname -o $lib $libobjs $deplibs $linker_flags'
dnl X     hardcode_libdir_flag_spec='-L$libdir'
dnl X     hardcode_shlibpath_var=no
dnl X     ;;
dnl X 
dnl X   sysv4*MP*)
dnl X     if test -d /usr/nec; then
dnl X       archive_cmds='$LD -G -h $soname -o $lib $libobjs $deplibs $linker_flags'
dnl X       hardcode_shlibpath_var=no
dnl X       runpath_var=LD_RUN_PATH
dnl X       hardcode_runpath_var=yes
dnl X       ld_shlibs=yes
dnl X     fi
dnl X     ;;
dnl X 
dnl X   sysv4.2uw2*)
dnl X     archive_cmds='$LD -G -o $lib $libobjs $deplibs $linker_flags'
dnl X     hardcode_direct=yes
dnl X     hardcode_minus_L=no
dnl X     hardcode_shlibpath_var=no
dnl X     hardcode_runpath_var=yes
dnl X     runpath_var=LD_RUN_PATH
dnl X     ;;
dnl X 
dnl X   sysv5uw7* | unixware7*)
dnl X     no_undefined_flag='${wl}-z ${wl}text'
dnl X     if test "$GCC" = yes; then
dnl X       archive_cmds='$CC -shared ${wl}-h ${wl}$soname -o $lib $libobjs $deplibs $compiler_flags'
dnl X     else
dnl X       archive_cmds='$CC -G ${wl}-h ${wl}$soname -o $lib $libobjs $deplibs $compiler_flags'
dnl X     fi
dnl X     runpath_var='LD_RUN_PATH'
dnl X     hardcode_shlibpath_var=no
dnl X     ;;
dnl X 
dnl X   *)
dnl X     ld_shlibs=no
dnl X     ;;
dnl X   esac
dnl X fi
dnl X AC_MSG_RESULT([$ld_shlibs])
dnl X test "$ld_shlibs" = no && can_build_shared=no
dnl X ##
dnl X ## END FIXME
dnl X 
dnl X ## FIXME: this should be a separate macro
dnl X ##
dnl X # Check hardcoding attributes.
dnl X AC_MSG_CHECKING([how to hardcode library paths into programs])
dnl X hardcode_action=
dnl X if test -n "$hardcode_libdir_flag_spec" || \
dnl X    test -n "$runpath_var"; then
dnl X 
dnl X   # We can hardcode non-existant directories.
dnl X   if test "$hardcode_direct" != no &&
dnl X      # If the only mechanism to avoid hardcoding is shlibpath_var, we
dnl X      # have to relink, otherwise we might link with an installed library
dnl X      # when we should be linking with a yet-to-be-installed one
dnl X      ## test "$hardcode_shlibpath_var" != no &&
dnl X      test "$hardcode_minus_L" != no; then
dnl X     # Linking always hardcodes the temporary library directory.
dnl X     hardcode_action=relink
dnl X   else
dnl X     # We can link without hardcoding, and we can hardcode nonexisting dirs.
dnl X     hardcode_action=immediate
dnl X   fi
dnl X else
dnl X   # We cannot hardcode anything, or else we can only hardcode existing
dnl X   # directories.
dnl X   hardcode_action=unsupported
dnl X fi
dnl X AC_MSG_RESULT([$hardcode_action])
dnl X ##
dnl X ## END FIXME
dnl X 
dnl X ## FIXME: this should be a separate macro
dnl X ##
dnl X striplib=
dnl X old_striplib=
dnl X AC_MSG_CHECKING([whether stripping libraries is possible])
dnl X if test -n "$STRIP" && $STRIP -V 2>&1 | grep "GNU strip" >/dev/null; then
dnl X   test -z "$old_striplib" && old_striplib="$STRIP --strip-debug"
dnl X   test -z "$striplib" && striplib="$STRIP --strip-unneeded"
dnl X   AC_MSG_RESULT([yes])
dnl X else
dnl X   AC_MSG_RESULT([no])
dnl X fi
dnl X ##
dnl X ## END FIXME
dnl X 
dnl X reload_cmds='$LD$reload_flag -o $output$reload_objs'
dnl X test -z "$deplibs_check_method" && deplibs_check_method=unknown
dnl X 
dnl X ## FIXME: this should be a separate macro
dnl X ##
dnl X # PORTME Fill in your ld.so characteristics
dnl X AC_MSG_CHECKING([dynamic linker characteristics])
dnl X library_names_spec=
dnl X libname_spec='lib$name'
dnl X soname_spec=
dnl X postinstall_cmds=
dnl X postuninstall_cmds=
dnl X finish_cmds=
dnl X finish_eval=
dnl X shlibpath_var=
dnl X shlibpath_overrides_runpath=unknown
dnl X version_type=none
dnl X dynamic_linker="$host_os ld.so"
dnl X sys_lib_dlsearch_path_spec="/lib /usr/lib"
dnl X sys_lib_search_path_spec="/lib /usr/lib /usr/local/lib"
dnl X 
dnl X case $host_os in
dnl X aix3*)
dnl X   version_type=linux
dnl X   library_names_spec='${libname}${release}.so$versuffix $libname.a'
dnl X   shlibpath_var=LIBPATH
dnl X 
dnl X   # AIX has no versioning support, so we append a major version to the name.
dnl X   soname_spec='${libname}${release}.so$major'
dnl X   ;;
dnl X 
dnl X aix4* | aix5*)
dnl X   version_type=linux
dnl X   need_lib_prefix=no
dnl X   need_version=no
dnl X   hardcode_into_libs=yes
dnl X   if test "$host_cpu" = ia64; then
dnl X     # AIX 5 supports IA64
dnl X     library_names_spec='${libname}${release}.so$major ${libname}${release}.so$versuffix $libname.so'
dnl X     shlibpath_var=LD_LIBRARY_PATH
dnl X   else
dnl X     # With GCC up to 2.95.x, collect2 would create an import file
dnl X     # for dependence libraries.  The import file would start with
dnl X     # the line `#! .'.  This would cause the generated library to
dnl X     # depend on `.', always an invalid library.  This was fixed in
dnl X     # development snapshots of GCC prior to 3.0.
dnl X     case $host_os in
dnl X       aix4 | aix4.[[01]] | aix4.[[01]].*)
dnl X 	if { echo '#if __GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 97)'
dnl X 	     echo ' yes '
dnl X 	     echo '#endif'; } | ${CC} -E - | grep yes > /dev/null; then
dnl X 	  :
dnl X 	else
dnl X 	  can_build_shared=no
dnl X 	fi
dnl X 	;;
dnl X     esac
dnl X     # AIX (on Power*) has no versioning support, so currently we can
dnl X     # not hardcode correct soname into executable. Probably we can
dnl X     # add versioning support to collect2, so additional links can
dnl X     # be useful in future.
dnl X     if test "$aix_use_runtimelinking" = yes; then
dnl X       # If using run time linking (on AIX 4.2 or later) use lib<name>.so
dnl X       # instead of lib<name>.a to let people know that these are not
dnl X       # typical AIX shared libraries.
dnl X       library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so$major $libname.so'
dnl X     else
dnl X       # We preserve .a as extension for shared libraries through AIX4.2
dnl X       # and later when we are not doing run time linking.
dnl X       library_names_spec='${libname}${release}.a $libname.a'
dnl X       soname_spec='${libname}${release}.so$major'
dnl X     fi
dnl X     shlibpath_var=LIBPATH
dnl X   fi
dnl X   hardcode_into_libs=yes
dnl X   ;;
dnl X 
dnl X amigaos*)
dnl X   library_names_spec='$libname.ixlibrary $libname.a'
dnl X   # Create ${libname}_ixlibrary.a entries in /sys/libs.
dnl X   finish_eval='for lib in `ls $libdir/*.ixlibrary 2>/dev/null`; do libname=`$echo "X$lib" | $Xsed -e '\''s%^.*/\([[^/]]*\)\.ixlibrary$%\1%'\''`; test $rm /sys/libs/${libname}_ixlibrary.a; $show "(cd /sys/libs && $LN_S $lib ${libname}_ixlibrary.a)"; (cd /sys/libs && $LN_S $lib ${libname}_ixlibrary.a) || exit 1; done'
dnl X   ;;
dnl X 
dnl X beos*)
dnl X   library_names_spec='${libname}.so'
dnl X   dynamic_linker="$host_os ld.so"
dnl X   shlibpath_var=LIBRARY_PATH
dnl X   ;;
dnl X 
dnl X bsdi4*)
dnl X   version_type=linux
dnl X   need_version=no
dnl X   library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so$major $libname.so'
dnl X   soname_spec='${libname}${release}.so$major'
dnl X   finish_cmds='PATH="\$PATH:/sbin" ldconfig $libdir'
dnl X   shlibpath_var=LD_LIBRARY_PATH
dnl X   sys_lib_search_path_spec="/shlib /usr/lib /usr/X11/lib /usr/contrib/lib /lib /usr/local/lib"
dnl X   sys_lib_dlsearch_path_spec="/shlib /usr/lib /usr/local/lib"
dnl X   export_dynamic_flag_spec=-rdynamic
dnl X   # the default ld.so.conf also contains /usr/contrib/lib and
dnl X   # /usr/X11R6/lib (/usr/X11 is a link to /usr/X11R6), but let us allow
dnl X   # libtool to hard-code these into programs
dnl X   ;;
dnl X 
dnl X cygwin* | mingw* | pw32*)
dnl X   version_type=windows
dnl X   need_version=no
dnl X   need_lib_prefix=no
dnl X   case $GCC,$host_os in
dnl X   yes,cygwin*)
dnl X     library_names_spec='$libname.dll.a'
dnl X     soname_spec='`echo ${libname} | sed -e 's/^lib/cyg/'``echo ${release} | sed -e 's/[[.]]/-/g'`${versuffix}.dll'
dnl X     postinstall_cmds='dlpath=`bash 2>&1 -c '\''. $dir/${file}i;echo \$dlname'\''`~
dnl X       dldir=$destdir/`dirname \$dlpath`~
dnl X       test -d \$dldir || mkdir -p \$dldir~
dnl X       $install_prog .libs/$dlname \$dldir/$dlname'
dnl X     postuninstall_cmds='dldll=`bash 2>&1 -c '\''. $file; echo \$dlname'\''`~
dnl X       dlpath=$dir/\$dldll~
dnl X        $rm \$dlpath'
dnl X     ;;
dnl X   yes,mingw*)
dnl X     library_names_spec='${libname}`echo ${release} | sed -e 's/[[.]]/-/g'`${versuffix}.dll'
dnl X     sys_lib_search_path_spec=`$CC -print-search-dirs | grep "^libraries:" | sed -e "s/^libraries://" -e "s/;/ /g" -e "s,=/,/,g"`
dnl X     ;;
dnl X   yes,pw32*)
dnl X     library_names_spec='`echo ${libname} | sed -e 's/^lib/pw/'``echo ${release} | sed -e 's/[.]/-/g'`${versuffix}.dll'
dnl X     ;;
dnl X   *)
dnl X     library_names_spec='${libname}`echo ${release} | sed -e 's/[[.]]/-/g'`${versuffix}.dll $libname.lib'
dnl X     ;;
dnl X   esac
dnl X   dynamic_linker='Win32 ld.exe'
dnl X   # FIXME: first we should search . and the directory the executable is in
dnl X   shlibpath_var=PATH
dnl X   ;;
dnl X 
dnl X darwin* | rhapsody*)
dnl X   dynamic_linker="$host_os dyld"
dnl X   version_type=darwin
dnl X   need_lib_prefix=no
dnl X   need_version=no
dnl X   # FIXME: Relying on posixy $() will cause problems for
dnl X   #        cross-compilation, but unfortunately the echo tests do not
dnl X   #        yet detect zsh echo's removal of \ escapes.
dnl X   library_names_spec='${libname}${release}${versuffix}.$(test .$module = .yes && echo so || echo dylib) ${libname}${release}${major}.$(test .$module = .yes && echo so || echo dylib) ${libname}.$(test .$module = .yes && echo so || echo dylib)'
dnl X   soname_spec='${libname}${release}${major}.$(test .$module = .yes && echo so || echo dylib)'
dnl X   shlibpath_overrides_runpath=yes
dnl X   shlibpath_var=DYLD_LIBRARY_PATH
dnl X   ;;
dnl X 
dnl X freebsd1*)
dnl X   dynamic_linker=no
dnl X   ;;
dnl X 
dnl X freebsd*)
dnl X   objformat=`test -x /usr/bin/objformat && /usr/bin/objformat || echo aout`
dnl X   version_type=freebsd-$objformat
dnl X   case $version_type in
dnl X     freebsd-elf*)
dnl X       library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so $libname.so'
dnl X       need_version=no
dnl X       need_lib_prefix=no
dnl X       ;;
dnl X     freebsd-*)
dnl X       library_names_spec='${libname}${release}.so$versuffix $libname.so$versuffix'
dnl X       need_version=yes
dnl X       ;;
dnl X   esac
dnl X   shlibpath_var=LD_LIBRARY_PATH
dnl X   case $host_os in
dnl X   freebsd2*)
dnl X     shlibpath_overrides_runpath=yes
dnl X     ;;
dnl X   *)
dnl X     shlibpath_overrides_runpath=no
dnl X     hardcode_into_libs=yes
dnl X     ;;
dnl X   esac
dnl X   ;;
dnl X 
dnl X gnu*)
dnl X   version_type=linux
dnl X   need_lib_prefix=no
dnl X   need_version=no
dnl X   library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so${major} ${libname}.so'
dnl X   soname_spec='${libname}${release}.so$major'
dnl X   shlibpath_var=LD_LIBRARY_PATH
dnl X   hardcode_into_libs=yes
dnl X   ;;
dnl X 
dnl X hpux9* | hpux10* | hpux11*)
dnl X   # Give a soname corresponding to the major version so that dld.sl refuses to
dnl X   # link against other versions.
dnl X   dynamic_linker="$host_os dld.sl"
dnl X   version_type=sunos
dnl X   need_lib_prefix=no
dnl X   need_version=no
dnl X   shlibpath_var=SHLIB_PATH
dnl X   shlibpath_overrides_runpath=no # +s is required to enable SHLIB_PATH
dnl X   library_names_spec='${libname}${release}.sl$versuffix ${libname}${release}.sl$major $libname.sl'
dnl X   soname_spec='${libname}${release}.sl$major'
dnl X   # HP-UX runs *really* slowly unless shared libraries are mode 555.
dnl X   postinstall_cmds='chmod 555 $lib'
dnl X   ;;
dnl X 
dnl X irix5* | irix6* | nonstopux*)
dnl X   case $host_os in
dnl X     nonstopux*) version_type=nonstopux ;;
dnl X     *)          version_type=irix ;;
dnl X   esac
dnl X   need_lib_prefix=no
dnl X   need_version=no
dnl X   soname_spec='${libname}${release}.so$major'
dnl X   library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so$major ${libname}${release}.so $libname.so'
dnl X   case $host_os in
dnl X   irix5* | nonstopux*)
dnl X     libsuff= shlibsuff=
dnl X     ;;
dnl X   *)
dnl X     case $LD in # libtool.m4 will add one of these switches to LD
dnl X     *-32|*"-32 ") libsuff= shlibsuff= libmagic=32-bit;;
dnl X     *-n32|*"-n32 ") libsuff=32 shlibsuff=N32 libmagic=N32;;
dnl X     *-64|*"-64 ") libsuff=64 shlibsuff=64 libmagic=64-bit;;
dnl X     *) libsuff= shlibsuff= libmagic=never-match;;
dnl X     esac
dnl X     ;;
dnl X   esac
dnl X   shlibpath_var=LD_LIBRARY${shlibsuff}_PATH
dnl X   shlibpath_overrides_runpath=no
dnl X   sys_lib_search_path_spec="/usr/lib${libsuff} /lib${libsuff} /usr/local/lib${libsuff}"
dnl X   sys_lib_dlsearch_path_spec="/usr/lib${libsuff} /lib${libsuff}"
dnl X   ;;
dnl X 
dnl X # No shared lib support for Linux oldld, aout, or coff.
dnl X linux-gnuoldld* | linux-gnuaout* | linux-gnucoff*)
dnl X   dynamic_linker=no
dnl X   ;;
dnl X 
dnl X # This must be Linux ELF.
dnl X linux-gnu*)
dnl X   version_type=linux
dnl X   need_lib_prefix=no
dnl X   need_version=no
dnl X   library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so$major $libname.so'
dnl X   soname_spec='${libname}${release}.so$major'
dnl X   finish_cmds='PATH="\$PATH:/sbin" ldconfig -n $libdir'
dnl X   shlibpath_var=LD_LIBRARY_PATH
dnl X   shlibpath_overrides_runpath=no
dnl X   # This implies no fast_install, which is unacceptable.
dnl X   # Some rework will be needed to allow for fast_install
dnl X   # before this can be enabled.
dnl X   hardcode_into_libs=yes
dnl X 
dnl X   # We used to test for /lib/ld.so.1 and disable shared libraries on
dnl X   # powerpc, because MkLinux only supported shared libraries with the
dnl X   # GNU dynamic linker.  Since this was broken with cross compilers,
dnl X   # most powerpc-linux boxes support dynamic linking these days and
dnl X   # people can always --disable-shared, the test was removed, and we
dnl X   # assume the GNU/Linux dynamic linker is in use.
dnl X   dynamic_linker='GNU/Linux ld.so'
dnl X   ;;
dnl X 
dnl X netbsd*)
dnl X   version_type=sunos
dnl X   need_lib_prefix=no
dnl X   need_version=no
dnl X   if echo __ELF__ | $CC -E - | grep __ELF__ >/dev/null; then
dnl X     library_names_spec='${libname}${release}.so$versuffix ${libname}.so$versuffix'
dnl X     finish_cmds='PATH="\$PATH:/sbin" ldconfig -m $libdir'
dnl X     dynamic_linker='NetBSD (a.out) ld.so'
dnl X   else
dnl X     library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so$major ${libname}${release}.so ${libname}.so'
dnl X     soname_spec='${libname}${release}.so$major'
dnl X     dynamic_linker='NetBSD ld.elf_so'
dnl X   fi
dnl X   shlibpath_var=LD_LIBRARY_PATH
dnl X   shlibpath_overrides_runpath=yes
dnl X   hardcode_into_libs=yes
dnl X   ;;
dnl X 
dnl X newsos6)
dnl X   version_type=linux
dnl X   library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so$major $libname.so'
dnl X   shlibpath_var=LD_LIBRARY_PATH
dnl X   shlibpath_overrides_runpath=yes
dnl X   ;;
dnl X 
dnl X openbsd*)
dnl X   version_type=sunos
dnl X   need_lib_prefix=no
dnl X   need_version=no
dnl X   if test -z "`echo __ELF__ | $CC -E - | grep __ELF__`" || test "$host_os-$host_cpu" = "openbsd2.8-powerpc"; then
dnl X     case "$host_os" in
dnl X     openbsd2.[[89]] | openbsd2.[[89]].*)
dnl X       shlibpath_overrides_runpath=no
dnl X       ;;
dnl X     *)
dnl X       shlibpath_overrides_runpath=yes
dnl X       ;;
dnl X     esac
dnl X   else
dnl X     shlibpath_overrides_runpath=yes
dnl X   fi
dnl X   library_names_spec='${libname}${release}.so$versuffix ${libname}.so$versuffix'
dnl X   finish_cmds='PATH="\$PATH:/sbin" ldconfig -m $libdir'
dnl X   shlibpath_var=LD_LIBRARY_PATH
dnl X   ;;
dnl X 
dnl X os2*)
dnl X   libname_spec='$name'
dnl X   need_lib_prefix=no
dnl X   library_names_spec='$libname.dll $libname.a'
dnl X   dynamic_linker='OS/2 ld.exe'
dnl X   shlibpath_var=LIBPATH
dnl X   ;;
dnl X 
dnl X osf3* | osf4* | osf5*)
dnl X   version_type=osf
dnl X   need_version=no
dnl X   soname_spec='${libname}${release}.so$major'
dnl X   library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so$major $libname.so'
dnl X   shlibpath_var=LD_LIBRARY_PATH
dnl X   sys_lib_search_path_spec="/usr/shlib /usr/ccs/lib /usr/lib/cmplrs/cc /usr/lib /usr/local/lib /var/shlib"
dnl X   sys_lib_dlsearch_path_spec="$sys_lib_search_path_spec"
dnl X   hardcode_into_libs=yes
dnl X   ;;
dnl X 
dnl X sco3.2v5*)
dnl X   version_type=osf
dnl X   soname_spec='${libname}${release}.so$major'
dnl X   library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so$major $libname.so'
dnl X   shlibpath_var=LD_LIBRARY_PATH
dnl X   ;;
dnl X 
dnl X solaris*)
dnl X   version_type=linux
dnl X   need_lib_prefix=no
dnl X   need_version=no
dnl X   library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so$major $libname.so'
dnl X   soname_spec='${libname}${release}.so$major'
dnl X   shlibpath_var=LD_LIBRARY_PATH
dnl X   shlibpath_overrides_runpath=yes
dnl X   hardcode_into_libs=yes
dnl X   # ldd complains unless libraries are executable
dnl X   postinstall_cmds='chmod +x $lib'
dnl X   ;;
dnl X 
dnl X sunos4*)
dnl X   version_type=sunos
dnl X   library_names_spec='${libname}${release}.so$versuffix ${libname}.so$versuffix'
dnl X   finish_cmds='PATH="\$PATH:/usr/etc" ldconfig $libdir'
dnl X   shlibpath_var=LD_LIBRARY_PATH
dnl X   shlibpath_overrides_runpath=yes
dnl X   if test "$with_gnu_ld" = yes; then
dnl X     need_lib_prefix=no
dnl X   fi
dnl X   need_version=yes
dnl X   ;;
dnl X 
dnl X sysv4 | sysv4.2uw2* | sysv4.3* | sysv5*)
dnl X   version_type=linux
dnl X   library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so$major $libname.so'
dnl X   soname_spec='${libname}${release}.so$major'
dnl X   shlibpath_var=LD_LIBRARY_PATH
dnl X   case $host_vendor in
dnl X     sni)
dnl X       shlibpath_overrides_runpath=no
dnl X       need_lib_prefix=no
dnl X       export_dynamic_flag_spec='${wl}-Blargedynsym'
dnl X       runpath_var=LD_RUN_PATH
dnl X       ;;
dnl X     siemens)
dnl X       need_lib_prefix=no
dnl X       ;;
dnl X     motorola)
dnl X       need_lib_prefix=no
dnl X       need_version=no
dnl X       shlibpath_overrides_runpath=no
dnl X       sys_lib_search_path_spec='/lib /usr/lib /usr/ccs/lib'
dnl X       ;;
dnl X   esac
dnl X   ;;
dnl X 
dnl X uts4*)
dnl X   version_type=linux
dnl X   library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so$major $libname.so'
dnl X   soname_spec='${libname}${release}.so$major'
dnl X   shlibpath_var=LD_LIBRARY_PATH
dnl X   ;;
dnl X 
dnl X dgux*)
dnl X   version_type=linux
dnl X   need_lib_prefix=no
dnl X   need_version=no
dnl X   library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so$major $libname.so'
dnl X   soname_spec='${libname}${release}.so$major'
dnl X   shlibpath_var=LD_LIBRARY_PATH
dnl X   ;;
dnl X 
dnl X sysv4*MP*)
dnl X   if test -d /usr/nec ;then
dnl X     version_type=linux
dnl X     library_names_spec='$libname.so.$versuffix $libname.so.$major $libname.so'
dnl X     soname_spec='$libname.so.$major'
dnl X     shlibpath_var=LD_LIBRARY_PATH
dnl X   fi
dnl X   ;;
dnl X 
dnl X *)
dnl X   dynamic_linker=no
dnl X   ;;
dnl X esac
dnl X AC_MSG_RESULT([$dynamic_linker])
dnl X test "$dynamic_linker" = no && can_build_shared=no
dnl X ##
dnl X ## END FIXME
dnl X 
dnl X ## FIXME: this should be a separate macro
dnl X ##
dnl X # Report the final consequences.
dnl X AC_MSG_CHECKING([if libtool supports shared libraries])
dnl X AC_MSG_RESULT([$can_build_shared])
dnl X ##
dnl X ## END FIXME
dnl X 
dnl X ## FIXME: this should be a separate macro
dnl X ##
dnl X AC_MSG_CHECKING([whether to build shared libraries])
dnl X test "$can_build_shared" = "no" && enable_shared=no
dnl X 
dnl X # On AIX, shared libraries and static libraries use the same namespace, and
dnl X # are all built from PIC.
dnl X case "$host_os" in
dnl X aix3*)
dnl X   test "$enable_shared" = yes && enable_static=no
dnl X   if test -n "$RANLIB"; then
dnl X     archive_cmds="$archive_cmds~\$RANLIB \$lib"
dnl X     postinstall_cmds='$RANLIB $lib'
dnl X   fi
dnl X   ;;
dnl X 
dnl X aix4*)
dnl X   if test "$host_cpu" != ia64 && test "$aix_use_runtimelinking" = no ; then
dnl X     test "$enable_shared" = yes && enable_static=no
dnl X   fi
dnl X   ;;
dnl X esac
dnl X AC_MSG_RESULT([$enable_shared])
dnl X ##
dnl X ## END FIXME
dnl X 
dnl X ## FIXME: this should be a separate macro
dnl X ##
dnl X AC_MSG_CHECKING([whether to build static libraries])
dnl X # Make sure either enable_shared or enable_static is yes.
dnl X test "$enable_shared" = yes || enable_static=yes
dnl X AC_MSG_RESULT([$enable_static])
dnl X ##
dnl X ## END FIXME
dnl X 
dnl X if test "$hardcode_action" = relink; then
dnl X   # Fast installation is not supported
dnl X   enable_fast_install=no
dnl X elif test "$shlibpath_overrides_runpath" = yes ||
dnl X      test "$enable_shared" = no; then
dnl X   # Fast installation is not necessary
dnl X   enable_fast_install=needless
dnl X fi
dnl X 
dnl X variables_saved_for_relink="PATH $shlibpath_var $runpath_var"
dnl X if test "$GCC" = yes; then
dnl X   variables_saved_for_relink="$variables_saved_for_relink GCC_EXEC_PREFIX COMPILER_PATH LIBRARY_PATH"
dnl X fi
dnl X 
dnl X AC_LIBTOOL_DLOPEN_SELF
dnl X 
dnl X ## FIXME: this should be a separate macro
dnl X ##
dnl X if test "$enable_shared" = yes && test "$GCC" = yes; then
dnl X   case $archive_cmds in
dnl X   *'~'*)
dnl X     # FIXME: we may have to deal with multi-command sequences.
dnl X     ;;
dnl X   '$CC '*)
dnl X     # Test whether the compiler implicitly links with -lc since on some
dnl X     # systems, -lgcc has to come before -lc. If gcc already passes -lc
dnl X     # to ld, don't add -lc before -lgcc.
dnl X     AC_MSG_CHECKING([whether -lc should be explicitly linked in])
dnl X     AC_CACHE_VAL([lt_cv_archive_cmds_need_lc],
dnl X     [$rm conftest*
dnl X     echo 'static int dummy;' > conftest.$ac_ext
dnl X 
dnl X     if AC_TRY_EVAL(ac_compile); then
dnl X       soname=conftest
dnl X       lib=conftest
dnl X       libobjs=conftest.$ac_objext
dnl X       deplibs=
dnl X       wl=$lt_cv_prog_cc_wl
dnl X       compiler_flags=-v
dnl X       linker_flags=-v
dnl X       verstring=
dnl X       output_objdir=.
dnl X       libname=conftest
dnl X       save_allow_undefined_flag=$allow_undefined_flag
dnl X       allow_undefined_flag=
dnl X       if AC_TRY_EVAL(archive_cmds 2\>\&1 \| grep \" -lc \" \>/dev/null 2\>\&1)
dnl X       then
dnl X 	lt_cv_archive_cmds_need_lc=no
dnl X       else
dnl X 	lt_cv_archive_cmds_need_lc=yes
dnl X       fi
dnl X       allow_undefined_flag=$save_allow_undefined_flag
dnl X     else
dnl X       cat conftest.err 1>&5
dnl X     fi])
dnl X     AC_MSG_RESULT([$lt_cv_archive_cmds_need_lc])
dnl X     ;;
dnl X   esac
dnl X fi
dnl X need_lc=${lt_cv_archive_cmds_need_lc-yes}
dnl X ##
dnl X ## END FIXME
dnl X 
dnl X ## FIXME: this should be a separate macro
dnl X ##
dnl X # The second clause should only fire when bootstrapping the
dnl X # libtool distribution, otherwise you forgot to ship ltmain.sh
dnl X # with your package, and you will get complaints that there are
dnl X # no rules to generate ltmain.sh.
dnl X if test -f "$ltmain"; then
dnl X   :
dnl X else
dnl X   # If there is no Makefile yet, we rely on a make rule to execute
dnl X   # `config.status --recheck' to rerun these tests and create the
dnl X   # libtool script then.
dnl X   test -f Makefile && make "$ltmain"
dnl X fi
dnl X 
dnl X if test -f "$ltmain"; then
dnl X   trap "$rm \"${ofile}T\"; exit 1" 1 2 15
dnl X   $rm -f "${ofile}T"
dnl X 
dnl X   echo creating $ofile
dnl X 
dnl X   # Now quote all the things that may contain metacharacters while being
dnl X   # careful not to overquote the AC_SUBSTed values.  We take copies of the
dnl X   # variables and quote the copies for generation of the libtool script.
dnl X   for var in echo old_CC old_CFLAGS SED \
dnl X     AR AR_FLAGS CC LD LN_S NM SHELL \
dnl X     reload_flag reload_cmds wl \
dnl X     pic_flag link_static_flag no_builtin_flag export_dynamic_flag_spec \
dnl X     thread_safe_flag_spec whole_archive_flag_spec libname_spec \
dnl X     library_names_spec soname_spec \
dnl X     RANLIB old_archive_cmds old_archive_from_new_cmds old_postinstall_cmds \
dnl X     old_postuninstall_cmds archive_cmds archive_expsym_cmds postinstall_cmds \
dnl X     postuninstall_cmds extract_expsyms_cmds old_archive_from_expsyms_cmds \
dnl X     old_striplib striplib file_magic_cmd export_symbols_cmds \
dnl X     deplibs_check_method allow_undefined_flag no_undefined_flag \
dnl X     finish_cmds finish_eval global_symbol_pipe global_symbol_to_cdecl \
dnl X     global_symbol_to_c_name_address \
dnl X     hardcode_libdir_flag_spec hardcode_libdir_separator  \
dnl X     sys_lib_search_path_spec sys_lib_dlsearch_path_spec \
dnl X     compiler_c_o compiler_o_lo need_locks exclude_expsyms include_expsyms; do
dnl X 
dnl X     case $var in
dnl X     reload_cmds | old_archive_cmds | old_archive_from_new_cmds | \
dnl X     old_postinstall_cmds | old_postuninstall_cmds | \
dnl X     export_symbols_cmds | archive_cmds | archive_expsym_cmds | \
dnl X     extract_expsyms_cmds | old_archive_from_expsyms_cmds | \
dnl X     postinstall_cmds | postuninstall_cmds | \
dnl X     finish_cmds | sys_lib_search_path_spec | sys_lib_dlsearch_path_spec)
dnl X       # Double-quote double-evaled strings.
dnl X       eval "lt_$var=\\\"\`\$echo \"X\$$var\" | \$Xsed -e \"\$double_quote_subst\" -e \"\$sed_quote_subst\" -e \"\$delay_variable_subst\"\`\\\""
dnl X       ;;
dnl X     *)
dnl X       eval "lt_$var=\\\"\`\$echo \"X\$$var\" | \$Xsed -e \"\$sed_quote_subst\"\`\\\""
dnl X       ;;
dnl X     esac
dnl X   done
dnl X 
dnl X   cat <<__EOF__ > "${ofile}T"
dnl X #! $SHELL
dnl X 
dnl X # `$echo "$ofile" | sed 's%^.*/%%'` - Provide generalized library-building support services.
dnl X # Generated automatically by $PROGRAM (GNU $PACKAGE $VERSION$TIMESTAMP)
dnl X # NOTE: Changes made to this file will be lost: look at ltmain.sh.
dnl X #
dnl X # Copyright (C) 1996-2000 Free Software Foundation, Inc.
dnl X # Originally by Gordon Matzigkeit <gord@gnu.ai.mit.edu>, 1996
dnl X #
dnl X # This program is free software; you can redistribute it and/or modify
dnl X # it under the terms of the GNU General Public License as published by
dnl X # the Free Software Foundation; either version 2 of the License, or
dnl X # (at your option) any later version.
dnl X #
dnl X # This program is distributed in the hope that it will be useful, but
dnl X # WITHOUT ANY WARRANTY; without even the implied warranty of
dnl X # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl X # General Public License for more details.
dnl X #
dnl X # You should have received a copy of the GNU General Public License
dnl X # along with this program; if not, write to the Free Software
dnl X # Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
dnl X #
dnl X # As a special exception to the GNU General Public License, if you
dnl X # distribute this file as part of a program that contains a
dnl X # configuration script generated by Autoconf, you may include it under
dnl X # the same distribution terms that you use for the rest of that program.
dnl X 
dnl X # A sed that does not truncate output.
dnl X SED=$lt_SED
dnl X 
dnl X # Sed that helps us avoid accidentally triggering echo(1) options like -n.
dnl X Xsed="${SED} -e s/^X//"
dnl X 
dnl X # The HP-UX ksh and POSIX shell print the target directory to stdout
dnl X # if CDPATH is set.
dnl X if test "X\${CDPATH+set}" = Xset; then CDPATH=:; export CDPATH; fi
dnl X 
dnl X # ### BEGIN LIBTOOL CONFIG
dnl X 
dnl X # Libtool was configured on host `(hostname || uname -n) 2>/dev/null | sed 1q`:
dnl X 
dnl X # Shell to use when invoking shell scripts.
dnl X SHELL=$lt_SHELL
dnl X 
dnl X # Whether or not to build shared libraries.
dnl X build_libtool_libs=$enable_shared
dnl X 
dnl X # Whether or not to build static libraries.
dnl X build_old_libs=$enable_static
dnl X 
dnl X # Whether or not to add -lc for building shared libraries.
dnl X build_libtool_need_lc=$need_lc
dnl X 
dnl X # Whether or not to optimize for fast installation.
dnl X fast_install=$enable_fast_install
dnl X 
dnl X # The host system.
dnl X host_alias=$host_alias
dnl X host=$host
dnl X 
dnl X # An echo program that does not interpret backslashes.
dnl X echo=$lt_echo
dnl X 
dnl X # The archiver.
dnl X AR=$lt_AR
dnl X AR_FLAGS=$lt_AR_FLAGS
dnl X 
dnl X # The default C compiler.
dnl X CC=$lt_CC
dnl X 
dnl X # Is the compiler the GNU C compiler?
dnl X with_gcc=$GCC
dnl X 
dnl X # The linker used to build libraries.
dnl X LD=$lt_LD
dnl X 
dnl X # Whether we need hard or soft links.
dnl X LN_S=$lt_LN_S
dnl X 
dnl X # A BSD-compatible nm program.
dnl X NM=$lt_NM
dnl X 
dnl X # A symbol stripping program
dnl X STRIP=$STRIP
dnl X 
dnl X # Used to examine libraries when file_magic_cmd begins "file"
dnl X MAGIC_CMD=$MAGIC_CMD
dnl X 
dnl X # Used on cygwin: DLL creation program.
dnl X DLLTOOL="$DLLTOOL"
dnl X 
dnl X # Used on cygwin: object dumper.
dnl X OBJDUMP="$OBJDUMP"
dnl X 
dnl X # Used on cygwin: assembler.
dnl X AS="$AS"
dnl X 
dnl X # The name of the directory that contains temporary libtool files.
dnl X objdir=$objdir
dnl X 
dnl X # How to create reloadable object files.
dnl X reload_flag=$lt_reload_flag
dnl X reload_cmds=$lt_reload_cmds
dnl X 
dnl X # How to pass a linker flag through the compiler.
dnl X wl=$lt_wl
dnl X 
dnl X # Object file suffix (normally "o").
dnl X objext="$ac_objext"
dnl X 
dnl X # Old archive suffix (normally "a").
dnl X libext="$libext"
dnl X 
dnl X # Executable file suffix (normally "").
dnl X exeext="$exeext"
dnl X 
dnl X # Additional compiler flags for building library objects.
dnl X pic_flag=$lt_pic_flag
dnl X pic_mode=$pic_mode
dnl X 
dnl X # Does compiler simultaneously support -c and -o options?
dnl X compiler_c_o=$lt_compiler_c_o
dnl X 
dnl X # Can we write directly to a .lo ?
dnl X compiler_o_lo=$lt_compiler_o_lo
dnl X 
dnl X # Must we lock files when doing compilation ?
dnl X need_locks=$lt_need_locks
dnl X 
dnl X # Do we need the lib prefix for modules?
dnl X need_lib_prefix=$need_lib_prefix
dnl X 
dnl X # Do we need a version for libraries?
dnl X need_version=$need_version
dnl X 
dnl X # Whether dlopen is supported.
dnl X dlopen_support=$enable_dlopen
dnl X 
dnl X # Whether dlopen of programs is supported.
dnl X dlopen_self=$enable_dlopen_self
dnl X 
dnl X # Whether dlopen of statically linked programs is supported.
dnl X dlopen_self_static=$enable_dlopen_self_static
dnl X 
dnl X # Compiler flag to prevent dynamic linking.
dnl X link_static_flag=$lt_link_static_flag
dnl X 
dnl X # Compiler flag to turn off builtin functions.
dnl X no_builtin_flag=$lt_no_builtin_flag
dnl X 
dnl X # Compiler flag to allow reflexive dlopens.
dnl X export_dynamic_flag_spec=$lt_export_dynamic_flag_spec
dnl X 
dnl X # Compiler flag to generate shared objects directly from archives.
dnl X whole_archive_flag_spec=$lt_whole_archive_flag_spec
dnl X 
dnl X # Compiler flag to generate thread-safe objects.
dnl X thread_safe_flag_spec=$lt_thread_safe_flag_spec
dnl X 
dnl X # Library versioning type.
dnl X version_type=$version_type
dnl X 
dnl X # Format of library name prefix.
dnl X libname_spec=$lt_libname_spec
dnl X 
dnl X # List of archive names.  First name is the real one, the rest are links.
dnl X # The last name is the one that the linker finds with -lNAME.
dnl X library_names_spec=$lt_library_names_spec
dnl X 
dnl X # The coded name of the library, if different from the real name.
dnl X soname_spec=$lt_soname_spec
dnl X 
dnl X # Commands used to build and install an old-style archive.
dnl X RANLIB=$lt_RANLIB
dnl X old_archive_cmds=$lt_old_archive_cmds
dnl X old_postinstall_cmds=$lt_old_postinstall_cmds
dnl X old_postuninstall_cmds=$lt_old_postuninstall_cmds
dnl X 
dnl X # Create an old-style archive from a shared archive.
dnl X old_archive_from_new_cmds=$lt_old_archive_from_new_cmds
dnl X 
dnl X # Create a temporary old-style archive to link instead of a shared archive.
dnl X old_archive_from_expsyms_cmds=$lt_old_archive_from_expsyms_cmds
dnl X 
dnl X # Commands used to build and install a shared archive.
dnl X archive_cmds=$lt_archive_cmds
dnl X archive_expsym_cmds=$lt_archive_expsym_cmds
dnl X postinstall_cmds=$lt_postinstall_cmds
dnl X postuninstall_cmds=$lt_postuninstall_cmds
dnl X 
dnl X # Commands to strip libraries.
dnl X old_striplib=$lt_old_striplib
dnl X striplib=$lt_striplib
dnl X 
dnl X # Method to check whether dependent libraries are shared objects.
dnl X deplibs_check_method=$lt_deplibs_check_method
dnl X 
dnl X # Command to use when deplibs_check_method == file_magic.
dnl X file_magic_cmd=$lt_file_magic_cmd
dnl X 
dnl X # Flag that allows shared libraries with undefined symbols to be built.
dnl X allow_undefined_flag=$lt_allow_undefined_flag
dnl X 
dnl X # Flag that forces no undefined symbols.
dnl X no_undefined_flag=$lt_no_undefined_flag
dnl X 
dnl X # Commands used to finish a libtool library installation in a directory.
dnl X finish_cmds=$lt_finish_cmds
dnl X 
dnl X # Same as above, but a single script fragment to be evaled but not shown.
dnl X finish_eval=$lt_finish_eval
dnl X 
dnl X # Take the output of nm and produce a listing of raw symbols and C names.
dnl X global_symbol_pipe=$lt_global_symbol_pipe
dnl X 
dnl X # Transform the output of nm in a proper C declaration
dnl X global_symbol_to_cdecl=$lt_global_symbol_to_cdecl
dnl X 
dnl X # Transform the output of nm in a C name address pair
dnl X global_symbol_to_c_name_address=$lt_global_symbol_to_c_name_address
dnl X 
dnl X # This is the shared library runtime path variable.
dnl X runpath_var=$runpath_var
dnl X 
dnl X # This is the shared library path variable.
dnl X shlibpath_var=$shlibpath_var
dnl X 
dnl X # Is shlibpath searched before the hard-coded library search path?
dnl X shlibpath_overrides_runpath=$shlibpath_overrides_runpath
dnl X 
dnl X # How to hardcode a shared library path into an executable.
dnl X hardcode_action=$hardcode_action
dnl X 
dnl X # Whether we should hardcode library paths into libraries.
dnl X hardcode_into_libs=$hardcode_into_libs
dnl X 
dnl X # Flag to hardcode \$libdir into a binary during linking.
dnl X # This must work even if \$libdir does not exist.
dnl X hardcode_libdir_flag_spec=$lt_hardcode_libdir_flag_spec
dnl X 
dnl X # Whether we need a single -rpath flag with a separated argument.
dnl X hardcode_libdir_separator=$lt_hardcode_libdir_separator
dnl X 
dnl X # Set to yes if using DIR/libNAME.so during linking hardcodes DIR into the
dnl X # resulting binary.
dnl X hardcode_direct=$hardcode_direct
dnl X 
dnl X # Set to yes if using the -LDIR flag during linking hardcodes DIR into the
dnl X # resulting binary.
dnl X hardcode_minus_L=$hardcode_minus_L
dnl X 
dnl X # Set to yes if using SHLIBPATH_VAR=DIR during linking hardcodes DIR into
dnl X # the resulting binary.
dnl X hardcode_shlibpath_var=$hardcode_shlibpath_var
dnl X 
dnl X # Variables whose values should be saved in libtool wrapper scripts and
dnl X # restored at relink time.
dnl X variables_saved_for_relink="$variables_saved_for_relink"
dnl X 
dnl X # Whether libtool must link a program against all its dependency libraries.
dnl X link_all_deplibs=$link_all_deplibs
dnl X 
dnl X # Compile-time system search path for libraries
dnl X sys_lib_search_path_spec=$lt_sys_lib_search_path_spec
dnl X 
dnl X # Run-time system search path for libraries
dnl X sys_lib_dlsearch_path_spec=$lt_sys_lib_dlsearch_path_spec
dnl X 
dnl X # Fix the shell variable \$srcfile for the compiler.
dnl X fix_srcfile_path="$fix_srcfile_path"
dnl X 
dnl X # Set to yes if exported symbols are required.
dnl X always_export_symbols=$always_export_symbols
dnl X 
dnl X # The commands to list exported symbols.
dnl X export_symbols_cmds=$lt_export_symbols_cmds
dnl X 
dnl X # The commands to extract the exported symbol list from a shared archive.
dnl X extract_expsyms_cmds=$lt_extract_expsyms_cmds
dnl X 
dnl X # Symbols that should not be listed in the preloaded symbols.
dnl X exclude_expsyms=$lt_exclude_expsyms
dnl X 
dnl X # Symbols that must always be exported.
dnl X include_expsyms=$lt_include_expsyms
dnl X 
dnl X # ### END LIBTOOL CONFIG
dnl X 
dnl X __EOF__
dnl X 
dnl X   case $host_os in
dnl X   aix3*)
dnl X     cat <<\EOF >> "${ofile}T"
dnl X 
dnl X # AIX sometimes has problems with the GCC collect2 program.  For some
dnl X # reason, if we set the COLLECT_NAMES environment variable, the problems
dnl X # vanish in a puff of smoke.
dnl X if test "X${COLLECT_NAMES+set}" != Xset; then
dnl X   COLLECT_NAMES=
dnl X   export COLLECT_NAMES
dnl X fi
dnl X EOF
dnl X     ;;
dnl X   esac
dnl X 
dnl X   case $host_os in
dnl X   cygwin* | mingw* | pw32* | os2*)
dnl X     cat <<'EOF' >> "${ofile}T"
dnl X       # This is a source program that is used to create dlls on Windows
dnl X       # Don't remove nor modify the starting and closing comments
dnl X # /* ltdll.c starts here */
dnl X # #define WIN32_LEAN_AND_MEAN
dnl X # #include <windows.h>
dnl X # #undef WIN32_LEAN_AND_MEAN
dnl X # #include <stdio.h>
dnl X #
dnl X # #ifndef __CYGWIN__
dnl X # #  ifdef __CYGWIN32__
dnl X # #    define __CYGWIN__ __CYGWIN32__
dnl X # #  endif
dnl X # #endif
dnl X #
dnl X # #ifdef __cplusplus
dnl X # extern "C" {
dnl X # #endif
dnl X # BOOL APIENTRY DllMain (HINSTANCE hInst, DWORD reason, LPVOID reserved);
dnl X # #ifdef __cplusplus
dnl X # }
dnl X # #endif
dnl X #
dnl X # #ifdef __CYGWIN__
dnl X # #include <cygwin/cygwin_dll.h>
dnl X # DECLARE_CYGWIN_DLL( DllMain );
dnl X # #endif
dnl X # HINSTANCE __hDllInstance_base;
dnl X #
dnl X # BOOL APIENTRY
dnl X # DllMain (HINSTANCE hInst, DWORD reason, LPVOID reserved)
dnl X # {
dnl X #   __hDllInstance_base = hInst;
dnl X #   return TRUE;
dnl X # }
dnl X # /* ltdll.c ends here */
dnl X 	# This is a source program that is used to create import libraries
dnl X 	# on Windows for dlls which lack them. Don't remove nor modify the
dnl X 	# starting and closing comments
dnl X # /* impgen.c starts here */
dnl X # /*   Copyright (C) 1999-2000 Free Software Foundation, Inc.
dnl X #
dnl X #  This file is part of GNU libtool.
dnl X #
dnl X #  This program is free software; you can redistribute it and/or modify
dnl X #  it under the terms of the GNU General Public License as published by
dnl X #  the Free Software Foundation; either version 2 of the License, or
dnl X #  (at your option) any later version.
dnl X #
dnl X #  This program is distributed in the hope that it will be useful,
dnl X #  but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl X #  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl X #  GNU General Public License for more details.
dnl X #
dnl X #  You should have received a copy of the GNU General Public License
dnl X #  along with this program; if not, write to the Free Software
dnl X #  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
dnl X #  */
dnl X #
dnl X # #include <stdio.h>		/* for printf() */
dnl X # #include <unistd.h>		/* for open(), lseek(), read() */
dnl X # #include <fcntl.h>		/* for O_RDONLY, O_BINARY */
dnl X # #include <string.h>		/* for strdup() */
dnl X #
dnl X # /* O_BINARY isn't required (or even defined sometimes) under Unix */
dnl X # #ifndef O_BINARY
dnl X # #define O_BINARY 0
dnl X # #endif
dnl X #
dnl X # static unsigned int
dnl X # pe_get16 (fd, offset)
dnl X #      int fd;
dnl X #      int offset;
dnl X # {
dnl X #   unsigned char b[2];
dnl X #   lseek (fd, offset, SEEK_SET);
dnl X #   read (fd, b, 2);
dnl X #   return b[0] + (b[1]<<8);
dnl X # }
dnl X #
dnl X # static unsigned int
dnl X # pe_get32 (fd, offset)
dnl X #     int fd;
dnl X #     int offset;
dnl X # {
dnl X #   unsigned char b[4];
dnl X #   lseek (fd, offset, SEEK_SET);
dnl X #   read (fd, b, 4);
dnl X #   return b[0] + (b[1]<<8) + (b[2]<<16) + (b[3]<<24);
dnl X # }
dnl X #
dnl X # static unsigned int
dnl X # pe_as32 (ptr)
dnl X #      void *ptr;
dnl X # {
dnl X #   unsigned char *b = ptr;
dnl X #   return b[0] + (b[1]<<8) + (b[2]<<16) + (b[3]<<24);
dnl X # }
dnl X #
dnl X # int
dnl X # main (argc, argv)
dnl X #     int argc;
dnl X #     char *argv[];
dnl X # {
dnl X #     int dll;
dnl X #     unsigned long pe_header_offset, opthdr_ofs, num_entries, i;
dnl X #     unsigned long export_rva, export_size, nsections, secptr, expptr;
dnl X #     unsigned long name_rvas, nexp;
dnl X #     unsigned char *expdata, *erva;
dnl X #     char *filename, *dll_name;
dnl X #
dnl X #     filename = argv[1];
dnl X #
dnl X #     dll = open(filename, O_RDONLY|O_BINARY);
dnl X #     if (dll < 1)
dnl X # 	return 1;
dnl X #
dnl X #     dll_name = filename;
dnl X #
dnl X #     for (i=0; filename[i]; i++)
dnl X # 	if (filename[i] == '/' || filename[i] == '\\'  || filename[i] == ':')
dnl X # 	    dll_name = filename + i +1;
dnl X #
dnl X #     pe_header_offset = pe_get32 (dll, 0x3c);
dnl X #     opthdr_ofs = pe_header_offset + 4 + 20;
dnl X #     num_entries = pe_get32 (dll, opthdr_ofs + 92);
dnl X #
dnl X #     if (num_entries < 1) /* no exports */
dnl X # 	return 1;
dnl X #
dnl X #     export_rva = pe_get32 (dll, opthdr_ofs + 96);
dnl X #     export_size = pe_get32 (dll, opthdr_ofs + 100);
dnl X #     nsections = pe_get16 (dll, pe_header_offset + 4 +2);
dnl X #     secptr = (pe_header_offset + 4 + 20 +
dnl X # 	      pe_get16 (dll, pe_header_offset + 4 + 16));
dnl X #
dnl X #     expptr = 0;
dnl X #     for (i = 0; i < nsections; i++)
dnl X #     {
dnl X # 	char sname[8];
dnl X # 	unsigned long secptr1 = secptr + 40 * i;
dnl X # 	unsigned long vaddr = pe_get32 (dll, secptr1 + 12);
dnl X # 	unsigned long vsize = pe_get32 (dll, secptr1 + 16);
dnl X # 	unsigned long fptr = pe_get32 (dll, secptr1 + 20);
dnl X # 	lseek(dll, secptr1, SEEK_SET);
dnl X # 	read(dll, sname, 8);
dnl X # 	if (vaddr <= export_rva && vaddr+vsize > export_rva)
dnl X # 	{
dnl X # 	    expptr = fptr + (export_rva - vaddr);
dnl X # 	    if (export_rva + export_size > vaddr + vsize)
dnl X # 		export_size = vsize - (export_rva - vaddr);
dnl X # 	    break;
dnl X # 	}
dnl X #     }
dnl X #
dnl X #     expdata = (unsigned char*)malloc(export_size);
dnl X #     lseek (dll, expptr, SEEK_SET);
dnl X #     read (dll, expdata, export_size);
dnl X #     erva = expdata - export_rva;
dnl X #
dnl X #     nexp = pe_as32 (expdata+24);
dnl X #     name_rvas = pe_as32 (expdata+32);
dnl X #
dnl X #     printf ("EXPORTS\n");
dnl X #     for (i = 0; i<nexp; i++)
dnl X #     {
dnl X # 	unsigned long name_rva = pe_as32 (erva+name_rvas+i*4);
dnl X # 	printf ("\t%s @ %ld ;\n", erva+name_rva, 1+ i);
dnl X #     }
dnl X #
dnl X #     return 0;
dnl X # }
dnl X # /* impgen.c ends here */
dnl X 
dnl X EOF
dnl X     ;;
dnl X   esac
dnl X 
dnl X   # We use sed instead of cat because bash on DJGPP gets confused if
dnl X   # if finds mixed CR/LF and LF-only lines.  Since sed operates in
dnl X   # text mode, it properly converts lines to CR/LF.  This bash problem
dnl X   # is reportedly fixed, but why not run on old versions too?
dnl X   sed '$q' "$ltmain" >> "${ofile}T" || (rm -f "${ofile}T"; exit 1)
dnl X 
dnl X   mv -f "${ofile}T" "$ofile" || \
dnl X     (rm -f "$ofile" && cp "${ofile}T" "$ofile" && rm -f "${ofile}T")
dnl X   chmod +x "$ofile"
dnl X fi
dnl X ##
dnl X ## END FIXME
dnl X 
dnl X ])# _LT_AC_LTCONFIG_HACK
dnl X 
dnl X # AC_LIBTOOL_DLOPEN - enable checks for dlopen support
dnl X AC_DEFUN([AC_LIBTOOL_DLOPEN], [AC_BEFORE([$0],[AC_LIBTOOL_SETUP])])
dnl X 
dnl X # AC_LIBTOOL_WIN32_DLL - declare package support for building win32 dll's
dnl X AC_DEFUN([AC_LIBTOOL_WIN32_DLL], [AC_BEFORE([$0], [AC_LIBTOOL_SETUP])])
dnl X 
dnl X # AC_ENABLE_SHARED - implement the --enable-shared flag
dnl X # Usage: AC_ENABLE_SHARED[(DEFAULT)]
dnl X #   Where DEFAULT is either `yes' or `no'.  If omitted, it defaults to
dnl X #   `yes'.
dnl X AC_DEFUN([AC_ENABLE_SHARED],
dnl X [define([AC_ENABLE_SHARED_DEFAULT], ifelse($1, no, no, yes))dnl
dnl X AC_ARG_ENABLE(shared,
dnl X changequote(<<, >>)dnl
dnl X <<  --enable-shared[=PKGS]  build shared libraries [default=>>AC_ENABLE_SHARED_DEFAULT],
dnl X changequote([, ])dnl
dnl X [p=${PACKAGE-default}
dnl X case $enableval in
dnl X yes) enable_shared=yes ;;
dnl X no) enable_shared=no ;;
dnl X *)
dnl X   enable_shared=no
dnl X   # Look at the argument we got.  We use all the common list separators.
dnl X   IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}:,"
dnl X   for pkg in $enableval; do
dnl X     if test "X$pkg" = "X$p"; then
dnl X       enable_shared=yes
dnl X     fi
dnl X   done
dnl X   IFS="$ac_save_ifs"
dnl X   ;;
dnl X esac],
dnl X enable_shared=AC_ENABLE_SHARED_DEFAULT)dnl
dnl X ])
dnl X 
dnl X # AC_DISABLE_SHARED - set the default shared flag to --disable-shared
dnl X AC_DEFUN([AC_DISABLE_SHARED],
dnl X [AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
dnl X AC_ENABLE_SHARED(no)])
dnl X 
dnl X # AC_ENABLE_STATIC - implement the --enable-static flag
dnl X # Usage: AC_ENABLE_STATIC[(DEFAULT)]
dnl X #   Where DEFAULT is either `yes' or `no'.  If omitted, it defaults to
dnl X #   `yes'.
dnl X AC_DEFUN([AC_ENABLE_STATIC],
dnl X [define([AC_ENABLE_STATIC_DEFAULT], ifelse($1, no, no, yes))dnl
dnl X AC_ARG_ENABLE(static,
dnl X changequote(<<, >>)dnl
dnl X <<  --enable-static[=PKGS]  build static libraries [default=>>AC_ENABLE_STATIC_DEFAULT],
dnl X changequote([, ])dnl
dnl X [p=${PACKAGE-default}
dnl X case $enableval in
dnl X yes) enable_static=yes ;;
dnl X no) enable_static=no ;;
dnl X *)
dnl X   enable_static=no
dnl X   # Look at the argument we got.  We use all the common list separators.
dnl X   IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}:,"
dnl X   for pkg in $enableval; do
dnl X     if test "X$pkg" = "X$p"; then
dnl X       enable_static=yes
dnl X     fi
dnl X   done
dnl X   IFS="$ac_save_ifs"
dnl X   ;;
dnl X esac],
dnl X enable_static=AC_ENABLE_STATIC_DEFAULT)dnl
dnl X ])
dnl X 
dnl X # AC_DISABLE_STATIC - set the default static flag to --disable-static
dnl X AC_DEFUN([AC_DISABLE_STATIC],
dnl X [AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
dnl X AC_ENABLE_STATIC(no)])
dnl X 
dnl X 
dnl X # AC_ENABLE_FAST_INSTALL - implement the --enable-fast-install flag
dnl X # Usage: AC_ENABLE_FAST_INSTALL[(DEFAULT)]
dnl X #   Where DEFAULT is either `yes' or `no'.  If omitted, it defaults to
dnl X #   `yes'.
dnl X AC_DEFUN([AC_ENABLE_FAST_INSTALL],
dnl X [define([AC_ENABLE_FAST_INSTALL_DEFAULT], ifelse($1, no, no, yes))dnl
dnl X AC_ARG_ENABLE(fast-install,
dnl X changequote(<<, >>)dnl
dnl X <<  --enable-fast-install[=PKGS]  optimize for fast installation [default=>>AC_ENABLE_FAST_INSTALL_DEFAULT],
dnl X changequote([, ])dnl
dnl X [p=${PACKAGE-default}
dnl X case $enableval in
dnl X yes) enable_fast_install=yes ;;
dnl X no) enable_fast_install=no ;;
dnl X *)
dnl X   enable_fast_install=no
dnl X   # Look at the argument we got.  We use all the common list separators.
dnl X   IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}:,"
dnl X   for pkg in $enableval; do
dnl X     if test "X$pkg" = "X$p"; then
dnl X       enable_fast_install=yes
dnl X     fi
dnl X   done
dnl X   IFS="$ac_save_ifs"
dnl X   ;;
dnl X esac],
dnl X enable_fast_install=AC_ENABLE_FAST_INSTALL_DEFAULT)dnl
dnl X ])
dnl X 
dnl X # AC_DISABLE_FAST_INSTALL - set the default to --disable-fast-install
dnl X AC_DEFUN([AC_DISABLE_FAST_INSTALL],
dnl X [AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
dnl X AC_ENABLE_FAST_INSTALL(no)])
dnl X 
dnl X # AC_LIBTOOL_PICMODE - implement the --with-pic flag
dnl X # Usage: AC_LIBTOOL_PICMODE[(MODE)]
dnl X #   Where MODE is either `yes' or `no'.  If omitted, it defaults to
dnl X #   `both'.
dnl X AC_DEFUN([AC_LIBTOOL_PICMODE],
dnl X [AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
dnl X pic_mode=ifelse($#,1,$1,default)])
dnl X 
dnl X 
dnl X # AC_PATH_TOOL_PREFIX - find a file program which can recognise shared library
dnl X AC_DEFUN([AC_PATH_TOOL_PREFIX],
dnl X [AC_MSG_CHECKING([for $1])
dnl X AC_CACHE_VAL(lt_cv_path_MAGIC_CMD,
dnl X [case $MAGIC_CMD in
dnl X   /*)
dnl X   lt_cv_path_MAGIC_CMD="$MAGIC_CMD" # Let the user override the test with a path.
dnl X   ;;
dnl X   ?:/*)
dnl X   lt_cv_path_MAGIC_CMD="$MAGIC_CMD" # Let the user override the test with a dos path.
dnl X   ;;
dnl X   *)
dnl X   ac_save_MAGIC_CMD="$MAGIC_CMD"
dnl X   IFS="${IFS=   }"; ac_save_ifs="$IFS"; IFS=":"
dnl X dnl $ac_dummy forces splitting on constant user-supplied paths.
dnl X dnl POSIX.2 word splitting is done only on the output of word expansions,
dnl X dnl not every word.  This closes a longstanding sh security hole.
dnl X   ac_dummy="ifelse([$2], , $PATH, [$2])"
dnl X   for ac_dir in $ac_dummy; do
dnl X     test -z "$ac_dir" && ac_dir=.
dnl X     if test -f $ac_dir/$1; then
dnl X       lt_cv_path_MAGIC_CMD="$ac_dir/$1"
dnl X       if test -n "$file_magic_test_file"; then
dnl X 	case $deplibs_check_method in
dnl X 	"file_magic "*)
dnl X 	  file_magic_regex="`expr \"$deplibs_check_method\" : \"file_magic \(.*\)\"`"
dnl X 	  MAGIC_CMD="$lt_cv_path_MAGIC_CMD"
dnl X 	  if eval $file_magic_cmd \$file_magic_test_file 2> /dev/null |
dnl X 	    egrep "$file_magic_regex" > /dev/null; then
dnl X 	    :
dnl X 	  else
dnl X 	    cat <<EOF 1>&2
dnl X 
dnl X *** Warning: the command libtool uses to detect shared libraries,
dnl X *** $file_magic_cmd, produces output that libtool cannot recognize.
dnl X *** The result is that libtool may fail to recognize shared libraries
dnl X *** as such.  This will affect the creation of libtool libraries that
dnl X *** depend on shared libraries, but programs linked with such libtool
dnl X *** libraries will work regardless of this problem.  Nevertheless, you
dnl X *** may want to report the problem to your system manager and/or to
dnl X *** bug-libtool@gnu.org
dnl X 
dnl X EOF
dnl X 	  fi ;;
dnl X 	esac
dnl X       fi
dnl X       break
dnl X     fi
dnl X   done
dnl X   IFS="$ac_save_ifs"
dnl X   MAGIC_CMD="$ac_save_MAGIC_CMD"
dnl X   ;;
dnl X esac])
dnl X MAGIC_CMD="$lt_cv_path_MAGIC_CMD"
dnl X if test -n "$MAGIC_CMD"; then
dnl X   AC_MSG_RESULT($MAGIC_CMD)
dnl X else
dnl X   AC_MSG_RESULT(no)
dnl X fi
dnl X ])
dnl X 
dnl X 
dnl X # AC_PATH_MAGIC - find a file program which can recognise a shared library
dnl X AC_DEFUN([AC_PATH_MAGIC],
dnl X [AC_REQUIRE([AC_CHECK_TOOL_PREFIX])dnl
dnl X AC_PATH_TOOL_PREFIX(${ac_tool_prefix}file, /usr/bin:$PATH)
dnl X if test -z "$lt_cv_path_MAGIC_CMD"; then
dnl X   if test -n "$ac_tool_prefix"; then
dnl X     AC_PATH_TOOL_PREFIX(file, /usr/bin:$PATH)
dnl X   else
dnl X     MAGIC_CMD=:
dnl X   fi
dnl X fi
dnl X ])
dnl X 
dnl X 
dnl X # AC_PROG_LD - find the path to the GNU or non-GNU linker
dnl X AC_DEFUN([AC_PROG_LD],
dnl X [AC_ARG_WITH(gnu-ld,
dnl X [  --with-gnu-ld           assume the C compiler uses GNU ld [default=no]],
dnl X test "$withval" = no || with_gnu_ld=yes, with_gnu_ld=no)
dnl X AC_REQUIRE([AC_PROG_CC])dnl
dnl X AC_REQUIRE([AC_CANONICAL_HOST])dnl
dnl X AC_REQUIRE([AC_CANONICAL_BUILD])dnl
dnl X AC_REQUIRE([_LT_AC_LIBTOOL_SYS_PATH_SEPARATOR])dnl
dnl X ac_prog=ld
dnl X if test "$GCC" = yes; then
dnl X   # Check if gcc -print-prog-name=ld gives a path.
dnl X   AC_MSG_CHECKING([for ld used by GCC])
dnl X   case $host in
dnl X   *-*-mingw*)
dnl X     # gcc leaves a trailing carriage return which upsets mingw
dnl X     ac_prog=`($CC -print-prog-name=ld) 2>&5 | tr -d '\015'` ;;
dnl X   *)
dnl X     ac_prog=`($CC -print-prog-name=ld) 2>&5` ;;
dnl X   esac
dnl X   case $ac_prog in
dnl X     # Accept absolute paths.
dnl X     [[\\/]]* | [[A-Za-z]]:[[\\/]]*)
dnl X       re_direlt='/[[^/]][[^/]]*/\.\./'
dnl X       # Canonicalize the path of ld
dnl X       ac_prog=`echo $ac_prog| sed 's%\\\\%/%g'`
dnl X       while echo $ac_prog | grep "$re_direlt" > /dev/null 2>&1; do
dnl X 	ac_prog=`echo $ac_prog| sed "s%$re_direlt%/%"`
dnl X       done
dnl X       test -z "$LD" && LD="$ac_prog"
dnl X       ;;
dnl X   "")
dnl X     # If it fails, then pretend we aren't using GCC.
dnl X     ac_prog=ld
dnl X     ;;
dnl X   *)
dnl X     # If it is relative, then search for the first ld in PATH.
dnl X     with_gnu_ld=unknown
dnl X     ;;
dnl X   esac
dnl X elif test "$with_gnu_ld" = yes; then
dnl X   AC_MSG_CHECKING([for GNU ld])
dnl X else
dnl X   AC_MSG_CHECKING([for non-GNU ld])
dnl X fi
dnl X AC_CACHE_VAL(lt_cv_path_LD,
dnl X [if test -z "$LD"; then
dnl X   IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS=$PATH_SEPARATOR
dnl X   for ac_dir in $PATH; do
dnl X     test -z "$ac_dir" && ac_dir=.
dnl X     if test -f "$ac_dir/$ac_prog" || test -f "$ac_dir/$ac_prog$ac_exeext"; then
dnl X       lt_cv_path_LD="$ac_dir/$ac_prog"
dnl X       # Check to see if the program is GNU ld.  I'd rather use --version,
dnl X       # but apparently some GNU ld's only accept -v.
dnl X       # Break only if it was the GNU/non-GNU ld that we prefer.
dnl X       if "$lt_cv_path_LD" -v 2>&1 < /dev/null | egrep '(GNU|with BFD)' > /dev/null; then
dnl X 	test "$with_gnu_ld" != no && break
dnl X       else
dnl X 	test "$with_gnu_ld" != yes && break
dnl X       fi
dnl X     fi
dnl X   done
dnl X   IFS="$ac_save_ifs"
dnl X else
dnl X   lt_cv_path_LD="$LD" # Let the user override the test with a path.
dnl X fi])
dnl X LD="$lt_cv_path_LD"
dnl X if test -n "$LD"; then
dnl X   AC_MSG_RESULT($LD)
dnl X else
dnl X   AC_MSG_RESULT(no)
dnl X fi
dnl X test -z "$LD" && AC_MSG_ERROR([no acceptable ld found in \$PATH])
dnl X AC_PROG_LD_GNU
dnl X ])
dnl X 
dnl X # AC_PROG_LD_GNU -
dnl X AC_DEFUN([AC_PROG_LD_GNU],
dnl X [AC_CACHE_CHECK([if the linker ($LD) is GNU ld], lt_cv_prog_gnu_ld,
dnl X [# I'd rather use --version here, but apparently some GNU ld's only accept -v.
dnl X if $LD -v 2>&1 </dev/null | egrep '(GNU|with BFD)' 1>&5; then
dnl X   lt_cv_prog_gnu_ld=yes
dnl X else
dnl X   lt_cv_prog_gnu_ld=no
dnl X fi])
dnl X with_gnu_ld=$lt_cv_prog_gnu_ld
dnl X ])
dnl X 
dnl X # AC_PROG_LD_RELOAD_FLAG - find reload flag for linker
dnl X #   -- PORTME Some linkers may need a different reload flag.
dnl X AC_DEFUN([AC_PROG_LD_RELOAD_FLAG],
dnl X [AC_CACHE_CHECK([for $LD option to reload object files], lt_cv_ld_reload_flag,
dnl X [lt_cv_ld_reload_flag='-r'])
dnl X reload_flag=$lt_cv_ld_reload_flag
dnl X test -n "$reload_flag" && reload_flag=" $reload_flag"
dnl X ])
dnl X 
dnl X # AC_DEPLIBS_CHECK_METHOD - how to check for library dependencies
dnl X #  -- PORTME fill in with the dynamic library characteristics
dnl X AC_DEFUN([AC_DEPLIBS_CHECK_METHOD],
dnl X [AC_CACHE_CHECK([how to recognise dependent libraries],
dnl X lt_cv_deplibs_check_method,
dnl X [lt_cv_file_magic_cmd='$MAGIC_CMD'
dnl X lt_cv_file_magic_test_file=
dnl X lt_cv_deplibs_check_method='unknown'
dnl X # Need to set the preceding variable on all platforms that support
dnl X # interlibrary dependencies.
dnl X # 'none' -- dependencies not supported.
dnl X # `unknown' -- same as none, but documents that we really don't know.
dnl X # 'pass_all' -- all dependencies passed with no checks.
dnl X # 'test_compile' -- check by making test program.
dnl X # 'file_magic [[regex]]' -- check by looking for files in library path
dnl X # which responds to the $file_magic_cmd with a given egrep regex.
dnl X # If you have `file' or equivalent on your system and you're not sure
dnl X # whether `pass_all' will *always* work, you probably want this one.
dnl X 
dnl X case $host_os in
dnl X aix4* | aix5*)
dnl X   lt_cv_deplibs_check_method=pass_all
dnl X   ;;
dnl X 
dnl X beos*)
dnl X   lt_cv_deplibs_check_method=pass_all
dnl X   ;;
dnl X 
dnl X bsdi4*)
dnl X   lt_cv_deplibs_check_method='file_magic ELF [[0-9]][[0-9]]*-bit [[ML]]SB (shared object|dynamic lib)'
dnl X   lt_cv_file_magic_cmd='/usr/bin/file -L'
dnl X   lt_cv_file_magic_test_file=/shlib/libc.so
dnl X   ;;
dnl X 
dnl X cygwin* | mingw* | pw32*)
dnl X   lt_cv_deplibs_check_method='file_magic file format pei*-i386(.*architecture: i386)?'
dnl X   lt_cv_file_magic_cmd='$OBJDUMP -f'
dnl X   ;;
dnl X 
dnl X darwin* | rhapsody*)
dnl X   lt_cv_deplibs_check_method='file_magic Mach-O dynamically linked shared library'
dnl X   lt_cv_file_magic_cmd='/usr/bin/file -L'
dnl X   case "$host_os" in
dnl X   rhapsody* | darwin1.[[012]])
dnl X     lt_cv_file_magic_test_file=`echo /System/Library/Frameworks/System.framework/Versions/*/System | head -1`
dnl X     ;;
dnl X   *) # Darwin 1.3 on
dnl X     lt_cv_file_magic_test_file='/usr/lib/libSystem.dylib'
dnl X     ;;
dnl X   esac
dnl X   ;;
dnl X 
dnl X freebsd*)
dnl X   if echo __ELF__ | $CC -E - | grep __ELF__ > /dev/null; then
dnl X     case $host_cpu in
dnl X     i*86 )
dnl X       # Not sure whether the presence of OpenBSD here was a mistake.
dnl X       # Let's accept both of them until this is cleared up.
dnl X       lt_cv_deplibs_check_method='file_magic (FreeBSD|OpenBSD)/i[[3-9]]86 (compact )?demand paged shared library'
dnl X       lt_cv_file_magic_cmd=/usr/bin/file
dnl X       lt_cv_file_magic_test_file=`echo /usr/lib/libc.so.*`
dnl X       ;;
dnl X     esac
dnl X   else
dnl X     lt_cv_deplibs_check_method=pass_all
dnl X   fi
dnl X   ;;
dnl X 
dnl X gnu*)
dnl X   lt_cv_deplibs_check_method=pass_all
dnl X   ;;
dnl X 
dnl X hpux10.20*|hpux11*)
dnl X   lt_cv_deplibs_check_method='file_magic (s[[0-9]][[0-9]][[0-9]]|PA-RISC[[0-9]].[[0-9]]) shared library'
dnl X   lt_cv_file_magic_cmd=/usr/bin/file
dnl X   lt_cv_file_magic_test_file=/usr/lib/libc.sl
dnl X   ;;
dnl X 
dnl X irix5* | irix6* | nonstopux*)
dnl X   case $host_os in
dnl X   irix5* | nonstopux*)
dnl X     # this will be overridden with pass_all, but let us keep it just in case
dnl X     lt_cv_deplibs_check_method="file_magic ELF 32-bit MSB dynamic lib MIPS - version 1"
dnl X     ;;
dnl X   *)
dnl X     case $LD in
dnl X     *-32|*"-32 ") libmagic=32-bit;;
dnl X     *-n32|*"-n32 ") libmagic=N32;;
dnl X     *-64|*"-64 ") libmagic=64-bit;;
dnl X     *) libmagic=never-match;;
dnl X     esac
dnl X     # this will be overridden with pass_all, but let us keep it just in case
dnl X     lt_cv_deplibs_check_method="file_magic ELF ${libmagic} MSB mips-[[1234]] dynamic lib MIPS - version 1"
dnl X     ;;
dnl X   esac
dnl X   lt_cv_file_magic_test_file=`echo /lib${libsuff}/libc.so*`
dnl X   lt_cv_deplibs_check_method=pass_all
dnl X   ;;
dnl X 
dnl X # This must be Linux ELF.
dnl X linux-gnu*)
dnl X   case $host_cpu in
dnl X   alpha* | hppa* | i*86 | mips | mipsel | powerpc* | sparc* | ia64*)
dnl X     lt_cv_deplibs_check_method=pass_all ;;
dnl X   *)
dnl X     # glibc up to 2.1.1 does not perform some relocations on ARM
dnl X     lt_cv_deplibs_check_method='file_magic ELF [[0-9]][[0-9]]*-bit [[LM]]SB (shared object|dynamic lib )' ;;
dnl X   esac
dnl X   lt_cv_file_magic_test_file=`echo /lib/libc.so* /lib/libc-*.so`
dnl X   ;;
dnl X 
dnl X netbsd*)
dnl X   if echo __ELF__ | $CC -E - | grep __ELF__ > /dev/null; then
dnl X     lt_cv_deplibs_check_method='match_pattern /lib[[^/\.]]+\.so\.[[0-9]]+\.[[0-9]]+$'
dnl X   else
dnl X     lt_cv_deplibs_check_method='match_pattern /lib[[^/\.]]+\.so$'
dnl X   fi
dnl X   ;;
dnl X 
dnl X newos6*)
dnl X   lt_cv_deplibs_check_method='file_magic ELF [[0-9]][[0-9]]*-bit [[ML]]SB (executable|dynamic lib)'
dnl X   lt_cv_file_magic_cmd=/usr/bin/file
dnl X   lt_cv_file_magic_test_file=/usr/lib/libnls.so
dnl X   ;;
dnl X 
dnl X openbsd*)
dnl X   lt_cv_file_magic_cmd=/usr/bin/file
dnl X   lt_cv_file_magic_test_file=`echo /usr/lib/libc.so.*`
dnl X   if test -z "`echo __ELF__ | $CC -E - | grep __ELF__`" || test "$host_os-$host_cpu" = "openbsd2.8-powerpc"; then
dnl X     lt_cv_deplibs_check_method='file_magic ELF [[0-9]][[0-9]]*-bit [[LM]]SB shared object'
dnl X   else
dnl X     lt_cv_deplibs_check_method='file_magic OpenBSD.* shared library'
dnl X   fi
dnl X   ;;
dnl X 
dnl X osf3* | osf4* | osf5*)
dnl X   # this will be overridden with pass_all, but let us keep it just in case
dnl X   lt_cv_deplibs_check_method='file_magic COFF format alpha shared library'
dnl X   lt_cv_file_magic_test_file=/shlib/libc.so
dnl X   lt_cv_deplibs_check_method=pass_all
dnl X   ;;
dnl X 
dnl X sco3.2v5*)
dnl X   lt_cv_deplibs_check_method=pass_all
dnl X   ;;
dnl X 
dnl X solaris*)
dnl X   lt_cv_deplibs_check_method=pass_all
dnl X   lt_cv_file_magic_test_file=/lib/libc.so
dnl X   ;;
dnl X 
dnl X sysv5uw[[78]]* | sysv4*uw2*)
dnl X   lt_cv_deplibs_check_method=pass_all
dnl X   ;;
dnl X 
dnl X sysv4 | sysv4.2uw2* | sysv4.3* | sysv5*)
dnl X   case $host_vendor in
dnl X   motorola)
dnl X     lt_cv_deplibs_check_method='file_magic ELF [[0-9]][[0-9]]*-bit [[ML]]SB (shared object|dynamic lib) M[[0-9]][[0-9]]* Version [[0-9]]'
dnl X     lt_cv_file_magic_test_file=`echo /usr/lib/libc.so*`
dnl X     ;;
dnl X   ncr)
dnl X     lt_cv_deplibs_check_method=pass_all
dnl X     ;;
dnl X   sequent)
dnl X     lt_cv_file_magic_cmd='/bin/file'
dnl X     lt_cv_deplibs_check_method='file_magic ELF [[0-9]][[0-9]]*-bit [[LM]]SB (shared object|dynamic lib )'
dnl X     ;;
dnl X   sni)
dnl X     lt_cv_file_magic_cmd='/bin/file'
dnl X     lt_cv_deplibs_check_method="file_magic ELF [[0-9]][[0-9]]*-bit [[LM]]SB dynamic lib"
dnl X     lt_cv_file_magic_test_file=/lib/libc.so
dnl X     ;;
dnl X   siemens)
dnl X     lt_cv_deplibs_check_method=pass_all
dnl X     ;;
dnl X   esac
dnl X   ;;
dnl X esac
dnl X ])
dnl X file_magic_cmd=$lt_cv_file_magic_cmd
dnl X deplibs_check_method=$lt_cv_deplibs_check_method
dnl X ])
dnl X 
dnl X 
dnl X # AC_PROG_NM - find the path to a BSD-compatible name lister
dnl X AC_DEFUN([AC_PROG_NM],
dnl X [AC_REQUIRE([_LT_AC_LIBTOOL_SYS_PATH_SEPARATOR])dnl
dnl X AC_MSG_CHECKING([for BSD-compatible nm])
dnl X AC_CACHE_VAL(lt_cv_path_NM,
dnl X [if test -n "$NM"; then
dnl X   # Let the user override the test.
dnl X   lt_cv_path_NM="$NM"
dnl X else
dnl X   IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS=$PATH_SEPARATOR
dnl X   for ac_dir in $PATH /usr/ccs/bin /usr/ucb /bin; do
dnl X     test -z "$ac_dir" && ac_dir=.
dnl X     tmp_nm=$ac_dir/${ac_tool_prefix}nm
dnl X     if test -f $tmp_nm || test -f $tmp_nm$ac_exeext ; then
dnl X       # Check to see if the nm accepts a BSD-compat flag.
dnl X       # Adding the `sed 1q' prevents false positives on HP-UX, which says:
dnl X       #   nm: unknown option "B" ignored
dnl X       # Tru64's nm complains that /dev/null is an invalid object file
dnl X       if ($tmp_nm -B /dev/null 2>&1 | sed '1q'; exit 0) | egrep '(/dev/null|Invalid file or object type)' >/dev/null; then
dnl X 	lt_cv_path_NM="$tmp_nm -B"
dnl X 	break
dnl X       elif ($tmp_nm -p /dev/null 2>&1 | sed '1q'; exit 0) | egrep /dev/null >/dev/null; then
dnl X 	lt_cv_path_NM="$tmp_nm -p"
dnl X 	break
dnl X       else
dnl X 	lt_cv_path_NM=${lt_cv_path_NM="$tmp_nm"} # keep the first match, but
dnl X 	continue # so that we can try to find one that supports BSD flags
dnl X       fi
dnl X     fi
dnl X   done
dnl X   IFS="$ac_save_ifs"
dnl X   test -z "$lt_cv_path_NM" && lt_cv_path_NM=nm
dnl X fi])
dnl X NM="$lt_cv_path_NM"
dnl X AC_MSG_RESULT([$NM])
dnl X ])
dnl X 
dnl X # AC_CHECK_LIBM - check for math library
dnl X AC_DEFUN([AC_CHECK_LIBM],
dnl X [AC_REQUIRE([AC_CANONICAL_HOST])dnl
dnl X LIBM=
dnl X case $host in
dnl X *-*-beos* | *-*-cygwin* | *-*-pw32*)
dnl X   # These system don't have libm
dnl X   ;;
dnl X *-ncr-sysv4.3*)
dnl X   AC_CHECK_LIB(mw, _mwvalidcheckl, LIBM="-lmw")
dnl X   AC_CHECK_LIB(m, main, LIBM="$LIBM -lm")
dnl X   ;;
dnl X *)
dnl X   AC_CHECK_LIB(m, main, LIBM="-lm")
dnl X   ;;
dnl X esac
dnl X ])
dnl X 
dnl X # AC_LIBLTDL_CONVENIENCE[(dir)] - sets LIBLTDL to the link flags for
dnl X # the libltdl convenience library and LTDLINCL to the include flags for
dnl X # the libltdl header and adds --enable-ltdl-convenience to the
dnl X # configure arguments.  Note that LIBLTDL and LTDLINCL are not
dnl X # AC_SUBSTed, nor is AC_CONFIG_SUBDIRS called.  If DIR is not
dnl X # provided, it is assumed to be `libltdl'.  LIBLTDL will be prefixed
dnl X # with '${top_builddir}/' and LTDLINCL will be prefixed with
dnl X # '${top_srcdir}/' (note the single quotes!).  If your package is not
dnl X # flat and you're not using automake, define top_builddir and
dnl X # top_srcdir appropriately in the Makefiles.
dnl X AC_DEFUN([AC_LIBLTDL_CONVENIENCE],
dnl X [AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
dnl X   case $enable_ltdl_convenience in
dnl X   no) AC_MSG_ERROR([this package needs a convenience libltdl]) ;;
dnl X   "") enable_ltdl_convenience=yes
dnl X       ac_configure_args="$ac_configure_args --enable-ltdl-convenience" ;;
dnl X   esac
dnl X   LIBLTDL='${top_builddir}/'ifelse($#,1,[$1],['libltdl'])/libltdlc.la
dnl X   LTDLINCL='-I${top_srcdir}/'ifelse($#,1,[$1],['libltdl'])
dnl X   # For backwards non-gettext consistent compatibility...
dnl X   INCLTDL="$LTDLINCL"
dnl X ])
dnl X 
dnl X # AC_LIBLTDL_INSTALLABLE[(dir)] - sets LIBLTDL to the link flags for
dnl X # the libltdl installable library and LTDLINCL to the include flags for
dnl X # the libltdl header and adds --enable-ltdl-install to the configure
dnl X # arguments.  Note that LIBLTDL and LTDLINCL are not AC_SUBSTed, nor is
dnl X # AC_CONFIG_SUBDIRS called.  If DIR is not provided and an installed
dnl X # libltdl is not found, it is assumed to be `libltdl'.  LIBLTDL will
dnl X # be prefixed with '${top_builddir}/' and LTDLINCL will be prefixed
dnl X # with '${top_srcdir}/' (note the single quotes!).  If your package is
dnl X # not flat and you're not using automake, define top_builddir and
dnl X # top_srcdir appropriately in the Makefiles.
dnl X # In the future, this macro may have to be called after AC_PROG_LIBTOOL.
dnl X AC_DEFUN([AC_LIBLTDL_INSTALLABLE],
dnl X [AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
dnl X   AC_CHECK_LIB(ltdl, main,
dnl X   [test x"$enable_ltdl_install" != xyes && enable_ltdl_install=no],
dnl X   [if test x"$enable_ltdl_install" = xno; then
dnl X      AC_MSG_WARN([libltdl not installed, but installation disabled])
dnl X    else
dnl X      enable_ltdl_install=yes
dnl X    fi
dnl X   ])
dnl X   if test x"$enable_ltdl_install" = x"yes"; then
dnl X     ac_configure_args="$ac_configure_args --enable-ltdl-install"
dnl X     LIBLTDL='${top_builddir}/'ifelse($#,1,[$1],['libltdl'])/libltdl.la
dnl X     LTDLINCL='-I${top_srcdir}/'ifelse($#,1,[$1],['libltdl'])
dnl X   else
dnl X     ac_configure_args="$ac_configure_args --enable-ltdl-install=no"
dnl X     LIBLTDL="-lltdl"
dnl X     LTDLINCL=
dnl X   fi
dnl X   # For backwards non-gettext consistent compatibility...
dnl X   INCLTDL="$LTDLINCL"
dnl X ])
dnl X 
dnl X # old names
dnl X AC_DEFUN([AM_PROG_LIBTOOL],   [AC_PROG_LIBTOOL])
dnl X AC_DEFUN([AM_ENABLE_SHARED],  [AC_ENABLE_SHARED($@)])
dnl X AC_DEFUN([AM_ENABLE_STATIC],  [AC_ENABLE_STATIC($@)])
dnl X AC_DEFUN([AM_DISABLE_SHARED], [AC_DISABLE_SHARED($@)])
dnl X AC_DEFUN([AM_DISABLE_STATIC], [AC_DISABLE_STATIC($@)])
dnl X AC_DEFUN([AM_PROG_LD],        [AC_PROG_LD])
dnl X AC_DEFUN([AM_PROG_NM],        [AC_PROG_NM])
dnl X 
dnl X # This is just to silence aclocal about the macro not being used
dnl X ifelse([AC_DISABLE_FAST_INSTALL])
dnl X 
dnl X ############################################################
dnl X # NOTE: This macro has been submitted for inclusion into   #
dnl X #  GNU Autoconf as AC_PROG_SED.  When it is available in   #
dnl X #  a released version of Autoconf we should remove this    #
dnl X #  macro and use it instead.                               #
dnl X ############################################################
dnl X # LT_AC_PROG_SED
dnl X # --------------
dnl X # Check for a fully-functional sed program, that truncates
dnl X # as few characters as possible.  Prefer GNU sed if found.
dnl X AC_DEFUN([LT_AC_PROG_SED],
dnl X [AC_MSG_CHECKING([for a sed that does not truncate output])
dnl X AC_CACHE_VAL(lt_cv_path_SED,
dnl X [# Loop through the user's path and test for sed and gsed.
dnl X # Then use that list of sed's as ones to test for truncation.
dnl X as_executable_p="test -f"
dnl X as_save_IFS=$IFS; IFS=$PATH_SEPARATOR
dnl X for as_dir in $PATH
dnl X do
dnl X   IFS=$as_save_IFS
dnl X   test -z "$as_dir" && as_dir=.
dnl X   for ac_prog in sed gsed; do
dnl X     for ac_exec_ext in '' $ac_executable_extensions; do
dnl X       if $as_executable_p "$as_dir/$ac_prog$ac_exec_ext"; then
dnl X         _sed_list="$_sed_list $as_dir/$ac_prog$ac_exec_ext"
dnl X       fi
dnl X     done
dnl X   done
dnl X done
dnl X 
dnl X   # Create a temporary directory, and hook for its removal unless debugging.
dnl X $debug ||
dnl X {
dnl X   trap 'exit_status=$?; rm -rf $tmp && exit $exit_status' 0
dnl X   trap '{ (exit 1); exit 1; }' 1 2 13 15
dnl X }
dnl X 
dnl X # Create a (secure) tmp directory for tmp files.
dnl X : ${TMPDIR=/tmp}
dnl X {
dnl X   tmp=`(umask 077 && mktemp -d -q "$TMPDIR/sedXXXXXX") 2>/dev/null` &&
dnl X   test -n "$tmp" && test -d "$tmp"
dnl X }  ||
dnl X {
dnl X   tmp=$TMPDIR/sed$$-$RANDOM
dnl X   (umask 077 && mkdir $tmp)
dnl X } ||
dnl X {
dnl X    echo "$me: cannot create a temporary directory in $TMPDIR" >&2
dnl X    { (exit 1); exit 1; }
dnl X }
dnl X   _max=0
dnl X   _count=0
dnl X   # Add /usr/xpg4/bin/sed as it is typically found on Solaris
dnl X   # along with /bin/sed that truncates output.
dnl X   for _sed in $_sed_list /usr/xpg4/bin/sed; do
dnl X     test ! -f ${_sed} && break
dnl X     cat /dev/null > "$tmp/sed.in"
dnl X     _count=0
dnl X     echo ${ECHO_N-$ac_n} "0123456789${ECHO_C-$ac_c}" >"$tmp/sed.in"
dnl X     # Check for GNU sed and select it if it is found.
dnl X     if "${_sed}" --version 2>&1 < /dev/null | egrep '(GNU)' > /dev/null; then
dnl X       lt_cv_path_SED=${_sed}
dnl X       break
dnl X     fi
dnl X     while true; do
dnl X       cat "$tmp/sed.in" "$tmp/sed.in" >"$tmp/sed.tmp"
dnl X       mv "$tmp/sed.tmp" "$tmp/sed.in"
dnl X       cp "$tmp/sed.in" "$tmp/sed.nl"
dnl X       echo >>"$tmp/sed.nl"
dnl X       ${_sed} -e 's/a$//' < "$tmp/sed.nl" >"$tmp/sed.out" || break
dnl X       cmp -s "$tmp/sed.out" "$tmp/sed.nl" || break
dnl X       # 40000 chars as input seems more than enough
dnl X       test $_count -gt 10 && break
dnl X       _count=`expr $_count + 1`
dnl X       if test $_count -gt $_max; then
dnl X         _max=$_count
dnl X         lt_cv_path_SED=$_sed
dnl X       fi
dnl X     done
dnl X   done
dnl X   rm -rf "$tmp"
dnl X ])
dnl X if test "X$SED" != "X"; then
dnl X   lt_cv_path_SED=$SED
dnl X else
dnl X   SED=$lt_cv_path_SED
dnl X fi
dnl X AC_MSG_RESULT([$SED])
dnl X ])
