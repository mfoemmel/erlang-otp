#############################################################################
# Makefile for building an ERTS load module
#############################################################################

#############################################################################
# CONFIGURATION
#############################################################################

# Main (Ethernet) IP address and default gateway.

# target IP
IP_ADDRESS	= 134.138.176.239

# eth1
ETH_ADDR        = 0x00,0x80,0x42,0x0e,0xeb,0xc5 
# eth2
#ETH_ADDR        = 0x00,0x80,0x42,0x0e,0xeb,0xc6 

ETH_IP_ADDR	= $(IP_ADDRESS)
ETH_SUBNETMASK  = 255.255.252.0
DEFAULT_GATEWAY	= 134.138.176.1

# OSE kernel library.
KRN		= 750

# LM target name
TARGET		= lm_ose_ppc$(KRN)

#############################################################################
# PATHS
#############################################################################

OSEROOT		= /usr/local/share/pgm/ose-4.4
DIABROOT        = /usr/local/share/pgm/diab-4.3f
PLATFORMROOT	= $(OSEROOT)/powerpc
PLATFORMSRC	= $(PLATFORMROOT)/src
PLATFORMLIB	= $(PLATFORMROOT)/lib
KRNROOT		= $(PLATFORMROOT)/krn-$(KRN)
KRNSRC		= $(KRNROOT)/src
KRNLIB		= $(KRNROOT)/lib
HOSTROOT	= $(OSEROOT)/solaris2
HOSTBIN		= $(HOSTROOT)/bin

EXAMPLES_COMMON 	= $(OSEROOT)/examples/common
EXAMPLES_COMMON_INCLUDE = $(EXAMPLES_COMMON)/include
EXAMPLES_COMMON_SRC 	= $(EXAMPLES_COMMON)/src

RELDIR		= /tmp/erl_ose_release
BUILD_DIR	= $(RELDIR)/build_erl_ose
MAKEROOT	= $(BUILD_DIR)/lm/erl
USER_LIBDIR	= $(BUILD_DIR)/erl_utils/lib

# Output directory.
OBJDIR		= $(MAKEROOT)/obj

#############################################################################
# HOST TOOLS
#############################################################################
CP              = cp
RM              = rm -f
MKDIR           = mkdir
RMDIR           = rm -rf
ECHO		= @echo
AR		= ar cq

#############################################################################
# CROSS COMPILER.
#############################################################################
# DIAB compiler, assembler, and linker.
DIABBIN		= $(DIABROOT)/SUNS/bin
OSEDIABLIB	= $(OSEROOT)/powerpc/lib/diab
OSEDIABSRC	= $(OSEROOT)/powerpc/src/diab
# HW float
#COMPTARGET	= -tPPC750EH:ose
# SW float
COMPTARGET	= -tPPC750ES:ose
# no float
#COMPTARGET	= -tPPC750EN:ose
#COMPTARGET	= -tPPC750EN:rtasim

# Assembler
ASM 		= DIABLIB=$(DIABROOT) PATH=$(DIABBIN) das
ASMFLAGS	= -Xmnem-all $(COMPTARGET)
ASMOUT		= -o $@			# name of the output file

# C preprocessor
CPP		= DIABLIB=$(DIABROOT) PATH=$(DIABBIN) dcc -E
CPPTOFILE	= DIABLIB=$(DIABROOT) PATH=$(DIABBIN) dcc -P
CPPFLAGS	+=  $(COMPTARGET)

# C compiler
CC 		= DIABLIB=$(DIABROOT) PATH=$(DIABBIN) dcc -c

FLAGS		+= $(COMPTARGET)
#FLAGS		+= -XO -Xkill-opt=0x2000080
FLAGS		+= -XO
#FLAGS		+= -g
#FLAGS		+= -Xansi
FLAGS		+= -Xdebug-struct-all -Xforce-declarations \
		-Xforce-prototypes -Xsmall-const=0 -Xstrings-in-text \
		-Xsmall-data=0

CFLAGS		+= $(FLAGS)
CCOUT           = -o $@			 # name of the output-file

# ELF linker
LD		= DIABLIB=$(DIABROOT) PATH=$(DIABBIN) dld
LDFLAGS         += -r2 -e zzmain -m6 $(COMPTARGET) $(LCF)

LCD		= $(LCF)
LCFDEFINES	+= -DPOWERPC -DIMAGE_START=0x00400000 -DIMAGE_MAX_LENGTH=8196000

LDOUT		= -o $@			# name of the output-file
LCFSRC		= lm_diab.lcf
LCF		= $(OBJDIR)/lm.lcf

# Tool for inserting conf data into elf
INSERTCONF	= $(HOSTBIN)/insert_conf -i

# ELF to SREC converter
ELF2SREC	= DIABLIB=$(DIABROOT) PATH=$(DIABBIN) ddump -R -v
ELF2SRECFLAGS	+= -y1000000		# maximum gap-size permitted
ELF2SRECOUT	= -o $@

# ELF to binary converter
ELF2BIN		= DIABLIB=$(DIABROOT) PATH=$(DIABBIN) ddump -R -u
ELF2BINFLAGS	+= -y1000000		# maximum gap-size permitted
ELF2BINOUT	= -o $@

### Libraries ###
#LIBDIRS		+= -L$(OSEDIABLIB)/PPC/ose
#LIBDIRS		+= -L$(OSEDIABLIB)/PPCE/ose
# no float
#LIBDIRS		+= -L$(OSEDIABLIB)/PPCEN/ose
# HW float
#LIBDIRS		+= -L$(OSEDIABLIB)/PPCEH/ose
# SW float
LIBDIRS		+= -L$(OSEDIABLIB)/PPCES/ose

INCLUDES	+= -I$(OSEROOT)/powerpc/include/diab
LIBRARIES	+= -limpfp
LIBRARIES	+= -lc
LIBRARIES	+= -lm


#############################################################################
# CRT
#############################################################################

OSEDEF_H_DEFINES	+= -DINFMT_FLOATING_POINT
OBJECTS			+= $(OBJDIR)/infmt.o
OBJECTS			+= $(OBJDIR)/outfmt.o
LIBRARIES		+= -lcrt

#############################################################################
# DBGPRINTF
#############################################################################

OBJECTS	+= $(OBJDIR)/dbgprintf.o

# use serial
OSEDEF_H_DEFINES	+= -DDBGPRINTF_SERUNIT=$(COM1_UNIT)
OSEDEF_H_DEFINES	+= -DDBGPRINTF_SERNAME=$(COM1_NAME)
OSEDEF_H_DEFINES	+= -DUSE_DEBUG_PRINTF


#############################################################################
# HEAP
#############################################################################

SIGFILES  	+= $(PLATFORMROOT)/include/heap.sig
OBJECTS 	+= $(OBJDIR)/heapcon.o
LIBRARIES  	+= -lheap

#############################################################################
# INET
#############################################################################
SIGFILES 	+= $(PLATFORMROOT)/include/inet.sig
SIGFILES 	+= $(PLATFORMROOT)/include/inetmib.sig
SIGFILES 	+= $(PLATFORMROOT)/include/inetlink.sig
LIBRARIES 	+= -linett -linetutil

LIBRARIES	+= -llnh

#############################################################################
# EFS
#############################################################################

SIGFILES 	+= $(PLATFORMROOT)/include/ddb.sig
SIGFILES 	+= $(PLATFORMROOT)/include/ddc.sig
SIGFILES 	+= $(PLATFORMROOT)/include/efs.sig
SIGFILES 	+= $(PLATFORMROOT)/include/fm.sig
LIBRARIES  	+= -lefs

#############################################################################
# KERNEL
#############################################################################

INCLUDES	+= -I$(KRNROOT)/include
DEFINES		+= -DOSE_DELTA -D_OSE_ 
DEFINES  	+= -DBIG_ENDIAN
OBJECTS 	+= $(OBJDIR)/osemain.o
LIBDIRS		+= -L$(KRNROOT)/lib 
LIBRARIES	+= -lkrnflib 
# opt for speed
# LIBRARIES	+= -lkrn0xxx
# opt for safety 
#LIBRARIES	+= -lkrn1xxx

#############################################################################
# RTC
#############################################################################

SIGFILES 	+= $(PLATFORMROOT)/include/rtc.sig
LIBRARIES 	+= -lrtc

#############################################################################
# PRH
#############################################################################

# for LMs
OBJECTS		+= $(OBJDIR)/lmcon.o
LIBRARIES	+= -lprh

#############################################################################
# SHELL
#############################################################################
LIBRARIES	+= -lshell

#############################################################################
# TOSV
#############################################################################

LIBRARIES  += -ltosv

#############################################################################
# PTHREADS
#############################################################################

LIBRARIES  += -losepthread

#############################################################################
# ERLANG
#############################################################################

ERL_PLATFORM	= ose_ppc750
ERTS_VER	= 5.3
ERL_OBJ_DIR	= $(RELDIR)/erts-$(ERTS_VER)/bin

EI_VER		= 3.3.2
EI_LIBDIR	= $(RELDIR)/lib/erl_interface-$(EI_VER)/lib
EI_INCLDIR	= $(RELDIR)/lib/erl_interface-$(EI_VER)/include

ERL_UTILS_SRC	= $(BUILD_DIR)/erl_utils
ERL_PORT_SRC	= $(BUILD_DIR)/port_progs
ERL_DRV_SRC	= $(BUILD_DIR)/drivers

ERL_PORT_OBJS	= $(OBJDIR)/erl_user_pgm.o \
                  $(OBJDIR)/erl_stat_port_ex.o \
                  $(OBJDIR)/erl_stat_portdrv_ex.o

ERL_MISC_OBJS	= $(OBJDIR)/erl_inet_cfg.o $(OBJDIR)/erl_ppl_drv.o
ERL_DBG_OBJS	= $(ERL_PORT_OBJS) $(ERL_MISC_OBJS) $(OBJDIR)/erl.exec.dbg.o
ERL_OBJS	= $(ERL_PORT_OBJS) $(ERL_MISC_OBJS) $(OBJDIR)/erl.exec.o 

ERL_TARGET	= erl_$(TARGET)
ERL_DBG_TARGET	= erl_$(TARGET).dbg

INCLUDES	+= -I$(ERL_UTILS_SRC) -I$(RELDIR)/erts-$(ERTS_VER)/src -I$(EI_INCLDIR)

LIBDIRS		+= -L$(USER_LIBDIR) -L$(EI_LIBDIR)
LIBRARIES	+= -lei

#############################################################################
# COMPILATION FLAGS
#############################################################################

STDINCLUDES	= -I$(PLATFORMROOT)/std-include

INCLS		= -I. -I$(PLATFORMROOT)/include $(STDINCLUDES) $(INCLUDES) \
		-I$(EXAMPLES_COMMON_INCLUDE) $(BSPINC) $(SIGFILES)

DEFS		= $(DEFINES) $(BSPDEFINE) $(OSEDEF_H_DEFINES) $(LCFDEFINES)

LIBDIRS		+=

LIBS		= -L$(PLATFORMLIB) $(LIBRARIES)

OBJS		= $(OBJECTS) $(TEST_OBJS)

#############################################################################
# MAKE USAGE
#############################################################################
# This section contains the default tag evaluated by make if no command line
# parameters are entered.
usage:
	$(ECHO) "Available make targets are:"
	$(ECHO) "clean   - delete the directory $(OBJDIR) and all its files."
	$(ECHO) "erlopt  - opt compile Erlang load module."
	$(ECHO) "erldbg  - debug compile Erlang load module."
	$(ECHO) "erllib  - build user library."
	$(ECHO) ""


#############################################################################
# COMPILE RULES
#############################################################################

$(OBJDIR)/erl.exec.dbg.o:	$(ERL_UTILS_SRC)/erl.exec.c
	$(CC) $(CFLAGS) $(DEFS) -DDEBUG $(INCLS) $(CCOUT) $<

$(OBJDIR)/erl.exec.o:		$(ERL_UTILS_SRC)/erl.exec.c
	$(CC) $(CFLAGS) $(DEFS) $(INCLS) $(CCOUT) $<

$(OBJDIR)/%.o: 			$(ERL_UTILS_SRC)/%.c
				$(CC) $(CFLAGS) $(DEFS) $(INCLS) $(CCOUT) $<

$(OBJDIR)/%.o: 			$(ERL_PORT_SRC)/%.c
				$(CC) $(CFLAGS) $(DEFS) $(INCLS) $(CCOUT) $<

$(OBJDIR)/%.o: 			$(ERL_DRV_SRC)/%.c
				$(CC) $(CFLAGS) $(DEFS) $(INCLS) $(CCOUT) $<

# Generic compile directives.
$(OBJDIR)/%.o: 		%.c
			$(CC) $(CFLAGS) $(DEFS) $(INCLS) $(CCOUT) $<

$(OBJDIR)/%.o: 		$(SRCDIR)/%.c
			$(CC) $(CFLAGS) $(DEFS) $(INCLS) $(CCOUT) $<

$(OBJDIR)/%.o: 		$(TARGETROOT)/src/%.c
			$(CC) $(CFLAGS) $(DEFS) $(INCLS) $(CCOUT) $<

$(OBJDIR)/%.o: 		$(BSPSRC)/%.c
			$(CC) $(CFLAGS) $(DEFS) $(INCLS) $(CCOUT) $<

$(OBJDIR)/%.o: 		$(BSPDBG)/%.c
			$(CC) $(CFLAGS) $(DEFS) $(INCLS) $(CCOUT) $<

$(OBJDIR)/%.o: 		$(KRNSRC)/%.c
			$(CC) $(CFLAGS) $(DEFS) $(INCLS) $(CCOUT) $<

$(OBJDIR)/%.o: 		$(OBJDIR)/%.c
			$(CC) $(CFLAGS) $(DEFS) $(INCLS) $(CCOUT) $<

$(OBJDIR)/%.o: 		$(PLATFORMSRC)/%.c
			$(CC) $(CFLAGS) $(DEFS) $(INCLS) $(CCOUT) $<

$(OBJDIR)/%.o: 		$(OSEDIABSRC)/%.c
			$(CC) $(CFLAGS) $(DEFS) $(INCLS) $(CCOUT) $<

$(OBJDIR)/osemain.o:	$(KRNSRC)/osemain.c osemain.con
			$(CC) $(CFLAGS) $(DEFS) $(INCLS) $(CCOUT) $<


# Generic assembly directives.
$(OBJDIR)/%.o: 		$(KRNSRC)/%.s
			$(ASM) $(ASMFLAGS) $(CCOUT) $<

$(OBJDIR)/%.o: 		$(BSPSRC)/%.s
			$(ASM) $(ASMFLAGS) $(CCOUT) $<

$(OBJDIR)/%.o: 		$(TARGETROOT)/lib/%.s
			$(ASM) $(ASMFLAGS) $(CCOUT) $<


#############################################################################
# LINKING AND GENERATING BINARY
#############################################################################

$(OBJDIR)/$(ERL_DBG_TARGET).elf:	$(OBJS) $(ERL_DBG_OBJS) $(LCF)
	$(CP) $(ERL_OBJ_DIR)/beam.debug $(OBJDIR)/beam.debug.o
	$(CP) $(ERL_OBJ_DIR)/epmd $(OBJDIR)/epmd.o
	$(LD) $(LDFLAGS) -o $@ $(CRT) $(OBJS) $(ERL_DBG_OBJS) $(OBJDIR)/beam.debug.o \
	$(OBJDIR)/epmd.o $(LIBDIRS) $(LIBS)  > $(OBJDIR)/$(ERL_DBG_TARGET).map

$(OBJDIR)/$(ERL_TARGET).elf:	$(OBJS) $(ERL_OBJS) $(LCF)
	$(CP) /clearcase/otp/erts/bin/ose_ppc750/beam $(OBJDIR)/beam.o
	$(CP) $(ERL_OBJ_DIR)/epmd $(OBJDIR)/epmd.o
	$(LD) $(LDFLAGS) -o $@ $(CRT) $(OBJS) $(ERL_OBJS) $(OBJDIR)/beam.o \
	$(OBJDIR)/epmd.o $(LIBDIRS) $(LIBS)  > $(OBJDIR)/$(ERL_TARGET).map

$(LCF):
	$(ECHO) "Create" $@ "from" $(LCFSRC)
	$(CPPTOFILE) $(CPPFLAGS) $(LCFDEFINES) $(LCFSRC) -o $@

lcf:	$(LCF)

erldbg:	$(OBJDIR)/$(ERL_DBG_TARGET).elf
	$(INSERTCONF) lm_conf_file $(OBJDIR)/$(ERL_DBG_TARGET).elf
#	$(GZIP) $(OBJDIR)/$(ERL_DBG_TARGET).elf
	$(ECHO) ""
	$(ECHO) " LM BUILT: " $(OBJDIR)/$(ERL_DBG_TARGET).elf
	$(ECHO) ""

erlopt:	$(OBJDIR)/$(ERL_TARGET).elf
	$(INSERTCONF) lm_conf_file $(OBJDIR)/$(ERL_TARGET).elf
#	$(GZIP) $(OBJDIR)/$(ERL_TARGET).elf
	$(ECHO) ""
	$(ECHO) " LM BUILT: " $(OBJDIR)/$(ERL_TARGET).elf
	$(ECHO) ""

#############################################################################
# CREATE AN ERLANG USER LIB FILE
#############################################################################

$(OBJDIR)/erl_user_pgm_reg.o:	$(ERL_UTILS_SRC)/erl_user_pgm_reg.c
	$(CC) $(CFLAGS) $(DEFS) $(INCLS) $(CCOUT) $<

$(OBJDIR)/erl_user_dbg.o:	$(ERL_UTILS_SRC)/erl_user_dbg.c
	$(CC) $(CFLAGS) $(DEFS) $(INCLS) $(CCOUT) $<

$(USER_LIBDIR)/liberl_user.a:	$(OBJDIR)/erl_user_pgm_reg.o $(OBJDIR)/erl_user_dbg.o
	$(RM) $@
	$(AR) $@ $(OBJDIR)/erl_user_pgm_reg.o $(OBJDIR)/erl_user_dbg.o $(OBJDIR)/dbgprintf.o

erllib: $(USER_LIBDIR)/liberl_user.a


#############################################################################
# MAKE CLEAN
#############################################################################
# This section deletes the $(OBJDIR) directory and all its files.
clean:
	$(RMDIR) $(OBJDIR)
	$(MKDIR) $(OBJDIR)

cleandbg:
	$(RM) $(OBJDIR)/$(ERL_DBG_TARGET).elf
	$(RM) $(LCF)

cleanopt:
	$(RM) $(OBJDIR)/$(ERL_TARGET).elf
	$(RM) $(LCF)


#############################################################################
# END OF MAKEFILE
#############################################################################

