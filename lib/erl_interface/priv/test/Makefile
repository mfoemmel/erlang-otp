#
# Created:  30 May 1996 by tobbe@erix.ericsson.se
#


MAKE = make					# Make program
CC = gcc					# C compiler to use
FASTOPT = -O2					# Optimizer flags
DEBUGOPT = -g					# Optimizer flags
LIBS = 						# Needed libraries
SYSFLAGS = -I../../src				# System specific defines
BINDIR = .
OBJDIR = .
ROOTDIR = ../../../..
SYSDIR = .
RUNDIR = ../../src
LD_DIR = 
ETCDIR = .
RANLIB = echo
RESOLVF =
LDFLAGS = 

# The target programs
PROGRAMS = $(OBJDIR)/test_eterm $(OBJDIR)/test_publish

OFILES =$(OBJDIR)/test_eterm.o $(OBJDIR)/test_publish.o 

CFLAGS = $(DEBUG) $(OPT) $(MACHINE) $(SYSFLAGS) $(RESOLVF) $(LDFLAGS)

# SunOS 4.x is our default
default:	opt.sunos4
release:	release.sunos4

test_eterm: test_eterm.c
	$(CC) $(CFLAGS) -o $@ test_eterm.c $(LIBS)

test_publish: test_publish.c
	$(CC) $(CFLAGS) -o $@ test_publish.c $(LIBS)

opt.test_eterm opt.test_publish:
	@$(MAKE) $(MFLAGS) CC="$(CC)" DEBUG= \
	SYSFLAGS="$(SYSFLAGS)" MACHINE="$(MACHINE)" \
	LDFLAGS="-L$(ROOTDIR)/obj$(SUFFIX)/$(TARGET)" \
	OPT="$(FASTOPT)" LIBS="$(LIBS)" \
	SYSDIR=$(SYSDIR) \
	RUNDIR=$(RUNDIR) \
	OBJDIR=$(OBJDIR) \
	BINDIR=$(BINDIR) \
	RESOLVF="$(RESOLVF)" \
	`echo $@ | sed "s/opt\.//"`

debug.test_eterm debug.test_publish:
	@$(MAKE) $(MFLAGS) CC="$(CC)" DEBUG= \
	SYSFLAGS="$(SYSFLAGS)" MACHINE="$(MACHINE)" \
	LDFLAGS="-L$(ROOTDIR)/obj$(SUFFIX)/$(TARGET)" \
	OPT="$(DEBUGOPT)" LIBS="$(LIBS)" \
	SYSDIR=$(SYSDIR) \
	RUNDIR=$(RUNDIR) \
	OBJDIR=$(OBJDIR) \
	BINDIR=$(BINDIR) \
	RESOLVF="$(RESOLVF)" \
	`echo $@ | sed "s/debug\.//"`

clean:
	rm -f $(PROGRAMS) *.o


opt.sunos4 debug.sunos4:
	@$(MAKE) $(MFLAGS) LIBS="-lerl_interface -lresolv" \
		TARGET=sunos4 \
		SUFFIX=`echo $@ | sed "s/\.*sunos4//" | sed "s/debug/\.debug/" | sed "s/opt/\./"` \
		SYSDIR=$(SYSDIR) \
		RUNDIR=$(RUNDIR) \
		ROOTDIR=$(ROOTDIR) \
		RANLIB=ranlib \
		`echo $@ | sed "s/\.*sunos4//"`.test_eterm
	@$(MAKE) $(MFLAGS) LIBS="-lerl_interface -lresolv" \
		TARGET=sunos4 \
		SUFFIX=`echo $@ | sed "s/\.*sunos4//" | sed "s/debug/\.debug/" | sed "s/opt/\./"` \
		SYSDIR=$(SYSDIR) \
		RUNDIR=$(RUNDIR) \
		ROOTDIR=$(ROOTDIR) \
		RANLIB=ranlib \
		`echo $@ | sed "s/\.*sunos4//"`.test_publish

opt.sunos5 debug.sunos5:
	@$(MAKE) $(MFLAGS) \
		LIBS="-lerl_interface -lsocket -lnsl -lresolv" \
		TARGET=sunos5 \
		SUFFIX=`echo $@ | sed "s/\.*sunos5//" | sed "s/debug/\.debug/" | sed "s/opt//"` \
		SYSDIR=$(SYSDIR) \
		RUNDIR=$(RUNDIR) \
		ROOTDIR=$(ROOTDIR) \
		RESOLVF='-Dgethostbyname=res_gethostbyname \
		-Dgethostbyaddr=res_gethostbyaddr' \
		`echo $@ | sed "s/\.*sunos5//"`.test_eterm
	@$(MAKE) $(MFLAGS) \
		LIBS="-lerl_interface -lsocket -lnsl -lresolv" \
		TARGET=sunos5 \
		SUFFIX=`echo $@ | sed "s/\.*sunos5//" | sed "s/debug/\.debug/" | sed "s/opt//"` \
		SYSDIR=$(SYSDIR) \
		RUNDIR=$(RUNDIR) \
		ROOTDIR=$(ROOTDIR) \
		RESOLVF='-Dgethostbyname=res_gethostbyname \
		-Dgethostbyaddr=res_gethostbyaddr' \
		`echo $@ | sed "s/\.*sunos5//"`.test_publish

opt.freebsd release.freebsd debug.freebsd:
	@$(MAKE) $(MFLAGS) \
		TARGET=freebsd \
		SYSDIR=$(SYSDIR) \
		RUNDIR=$(RUNDIR) \
		ROOTDIR=$(ROOTDIR) \
		`echo $@ | sed "s/\.*freebsd//"`.erl_interface

opt.linux release.linux debug.linux:
	@$(MAKE) $(MFLAGS) \
		TARGET=linux \
		SYSDIR=$(SYSDIR) \
		RUNDIR=$(RUNDIR) \
		ROOTDIR=$(ROOTDIR) \
		`echo $@ | sed "s/\.*linux//"`.erl_interface

# This is a real mess. It appears that libcposix.a contains a 
# buggy malloc() so instead of linking -lcposix we link a 
# stripped version of that lib :-(

opt.isc32 release.isc32 debug.isc32:
	@$(MAKE) $(MFLAGS) \
		SYSFLAGS="-DISC32" \
		LIBS="-linet -lcposix" \
		TARGET=isc32 \
		SYSDIR=$(SYSDIR) \
		RUNDIR=$(RUNDIR) \
		ROOTDIR=$(ROOTDIR) \
		`echo $@ | sed "s/\.*isc32//"`.erl_interface

opt.aix release.aix debug.aix:
	@$(MAKE) $(MFLAGS) \
		SYSFLAGS="-DSYS_SELECT_H" \
		TARGET=aix \
		SYSDIR=$(SYSDIR) \
		RUNDIR=$(RUNDIR) \
		ROOTDIR=$(ROOTDIR) \
		`echo $@ | sed "s/\.*aix//"`.erl_interface

opt.osf release.osf debug.osf:
	@$(MAKE) $(MFLAGS) CC=cc \
		TARGET=osf \
		SYSDIR=$(SYSDIR) \
		RUNDIR=$(RUNDIR) \
		ROOTDIR=$(ROOTDIR) \
		`echo $@ | sed "s/\.*osf//"`.erl_interface

opt.hpux release.hpux debug.hpux:
	@$(MAKE) $(MFLAGS) FASTOPT="+DA1.0" \
		TARGET=hpux \
		SYSDIR=$(SYSDIR) \
		RUNDIR=$(RUNDIR) \
		ROOTDIR=$(ROOTDIR) \
		`echo $@ | sed "s/\.*hpux//"`.erl_interface

# XXX The NO_SYSLOG, NO_FTRUNCATE, and NO_PWD should be in ../emu_src/sys.h
opt.qnx release.qnx debug.qnx:
	@$(MAKE) $(MFLAGS) CC=cc FASTOPT="-Oatx" \
		SYSFLAGS="-DQNX -DSYS_SELECT_H" \
		TARGET=qnx \
		SYSDIR=$(SYSDIR) \
		RUNDIR=$(RUNDIR) \
		ROOTDIR=$(ROOTDIR) \
		`echo $@ | sed "s/\.*qnx//"`.erl_interface




