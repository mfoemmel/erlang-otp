#############################################################################
# Makefile for building OSE (without Erlang)
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
TARGET		= ose_ppc$(KRN)

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

EXAMPLES		= $(OSEROOT)/examples
EXAMPLES_COMMON 	= $(OSEROOT)/examples/common
EXAMPLES_COMMON_INCLUDE = $(EXAMPLES_COMMON)/include
EXAMPLES_COMMON_SRC 	= $(EXAMPLES_COMMON)/src
EXAMPLES_LNH_SRC	= $(EXAMPLES)/lnh/src

RELDIR		= /tmp/erl_ose_release
BUILD_DIR	= $(RELDIR)/build_erl_ose


# Output directory.
MAKEROOT	= $(BUILD_DIR)/lm
MAKE_OSE_ROOT	= $(MAKEROOT)/ose
OBJDIR		= $(MAKE_OSE_ROOT)/obj
ERL_UTILS_SRC	= $(BUILD_DIR)/erl_utils

#############################################################################
# HOST TOOLS
#############################################################################
CP              = cp
RM              = rm -f
MKDIR           = mkdir
RMDIR           = rm -rf
ECHO		= @echo

#############################################################################
# BSP CONFIGURATION
#############################################################################

BSPROOT         = $(PLATFORMROOT)/bsp-powercore680
BSPSRC		= $(BSPROOT)/src
BSPINC		= $(BSPROOT)/include
BSPCONF		= $(BSPROOT)/conf
BSPDBG		= $(BSPSRC)/debug

# Board support package.
BSP		= FORCE_POWERCORE680-G3

OBJECTS		+= $(OBJDIR)/board.o $(OBJDIR)/mpc107.o $(OBJDIR)/sentinel.o \
		$(OBJDIR)/ppc_cache.o $(OBJDIR)/pci.o 
OBJECTS         += $(OBJDIR)/PCI_devices.o $(OBJDIR)/w83c553.o \
		$(OBJDIR)/pcore_asm.o $(OBJDIR)/nvram.o 

BSPDEFINE	+= -DBSP_CACHE
BSPDEFINE	+= -DFORCE_POWERCORE680
INCLUDES	+= -I$(BSPINC) -I$(BSPSRC) -I$(BSPCONF)

LCFSRC		= $(EXAMPLES_COMMON)/conf/diab_monolith.lcf
LCF		= $(OBJDIR)/powercore680.lcf

# BSPDEFINE   	+= -DTPRINTF_ENABLE -DERR_PRINTF_ENABLE -DTPRINTF_LVL=4
# OBJECTS     	+= $(OBJDIR)/tprintf.o

# The internal decrementer for the 75X runs at 1/4
# the board frequency.  INTERNAL_TIMER value is loaded
# to decrementer for calculated timer exception of 1msec.
#
# (ie) 100Mhz board speed --> 25Mhz decrementer frequency 
# 
# For calculated 1msec , INTERNAL_TIMER = 25000
#
# NOTE: Actual system time exception is derived by
# INTERNAL_TIMER*SYSTEM_TIMER(4 typ.) for actual 4ms
# system clock

INTERNAL_TIMER  =  24750         # Bus Clock 100Mhz (99 act.) PCORE-680

BSPDEFINE	+= -DBSP_INTERNAL_TIMER=$(INTERNAL_TIMER)

# Ethernet
BSPDEFINE	+= -DETHUNIT=0
BSPDEFINE	+= -DETHNAME=\"i82559_0\"
BSPDEFINE	+= -DI82559_UNIT0
BSPDEFINE	+= -DBSP_ETHERNET_ADDRESS=$(ETH_ADDR)
BSPDEFINE	+= -DETH_IP_ADDRESS=\"$(ETH_IP_ADDR)\"
BSPDEFINE	+= -DBSP_ETHERNET
OBJECTS	        += $(OBJDIR)/i82559.o

# UD16550 Serial 
OBJECTS			+= $(OBJDIR)/ud16550.o
OSEDEF_H_DEFINES	+= -DBSP_SERIAL
OSEDEF_H_DEFINES	+= -DUD16550_UNIT0
OSEDEF_H_DEFINES 	+= -DUD16550_UNIT1
COM1_UNIT	=0
COM2_UNIT	=1
COM1_NAME	=\"UD16550\"
COM2_NAME	=\"UD16550\"

# Flash driver
#OBJECTS 	+= $(OBJDIR)/init_flash.o 
#BSPDEFINE 	+= -DFLASH_START=0xff000000
#BSPDEFINE	+= -DBSP_FLASH 
#OBJECTS   	+= $(OBJDIR)/amd_flash.o
#BSPDEFINE 	+= -DFLASH_END=0xffffffff
#BSPDEFINE 	+= -DFLASH_DRIVER_NAME=\"amd_29f032B\"
#BSPDEFINE 	+= -DFLASH_DRIVER_BIOS_ENTRY=amd_29F0XXB_biosentry
#OSEDEF_H_DEFINES += -DFLASH_ENABLED


# Ramdisk
# we don't necessarily need to compile ramdisk.c (already part of libefs.a)
# OBJECTS 	+= $(OBJDIR)/ramdisk.o

BSPDEFINE	+= -DUSE_RAMDISK 
#BSPDEFINE	+= -DRAMDISK_STATIC
BSPDEFINE 	+= -DRAMDISK_SIZE=65536
#BSPDEFINE 	+= -DRAMDISK_SIZE=32768
#BSPDEFINE 	+= -DRAMDISK_TRAPINT

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
# LDFLAGS         += -d2 -m6 $(COMPTARGET) $(LCF)
LDFLAGS         += -m6 $(COMPTARGET) $(LCF)

LCD		= $(LCF)
LCFDEFINES	+= -DPOWERPC -DIMAGE_START=0x00400000 -DIMAGE_MAX_LENGTH=32768000

LDOUT		= -o $@			# name of the output-file

# ELF to SREC converter
ELF2SREC	= DIABLIB=$(DIABROOT) PATH=$(DIABBIN) ddump -R -v
ELF2SRECFLAGS	+= -y1000000		# maximum gap-size permitted
ELF2SRECOUT	= -o $@

# ELF to binary converter
ELF2BIN		= DIABLIB=$(DIABROOT) PATH=$(DIABBIN) ddump -R -u
ELF2BINFLAGS	+= -y1000000		# maximum gap-size permitted
ELF2BINOUT	= -o $@

### Libraries ###
# general(?)
#LIBDIRS	+= -L$(OSEDIABLIB)/PPC/ose
#LIBDIRS	+= -L$(OSEDIABLIB)/PPCE/ose
# no float
#LIBDIRS	+= -L$(OSEDIABLIB)/PPCEN/ose
# HW float
#LIBDIRS	+= -L$(OSEDIABLIB)/PPCEH/ose
# SW float
LIBDIRS		+= -L$(OSEDIABLIB)/PPCES/ose

INCLUDES	+= -I$(OSEROOT)/powerpc/include/diab
LIBRARIES	+= -limpfp
LIBRARIES	+= -lc
LIBRARIES	+= -lm


#############################################################################
# DBGPRINTF
#############################################################################

OBJECTS	+= $(OBJDIR)/dbgprintf.o

# use serial
OSEDEF_H_DEFINES	+= -DDBGPRINTF_SERUNIT=$(COM1_UNIT)
OSEDEF_H_DEFINES	+= -DDBGPRINTF_SERNAME=$(COM1_NAME)
OBJECTS			+= $(OBJDIR)/serprintf.o

#DBGPRINTF_UDPPORT	= 7777

# use eth
#OSEDEF_H_DEFINES	+= -DUDPPORT=$(DBGPRINTF_UDPPORT)
#OSEDEF_H_DEFINES	+= -DETH_ADDR=$(ETH_ADDR)
#OBJECTS			+= $(OBJDIR)/ethprintf.o

# use udp
#OSEDEF_H_DEFINES	+= -DUDPPRINTF_PORT=$(DBGPRINTF_UDPPORT)
#OBJECTS			+= $(OBJDIR)/udpprintf.o

OSEDEF_H_DEFINES	+= -DUSE_DEBUG_PRINTF

#############################################################################
# CRT
#############################################################################

OSEDEF_H_DEFINES	+= -DINFMT_FLOATING_POINT
OBJECTS			+= $(OBJDIR)/infmt.o
OBJECTS			+= $(OBJDIR)/outfmt.o
LIBRARIES		+= -lcrt
CRT         		= $(OSEDIABLIB)/PPCE/ose/crt0.o

#############################################################################
# DEBUG
#############################################################################

LIBRARIES  += -ldbgserver 

#############################################################################
# EFS
#############################################################################

SIGFILES 	+= $(PLATFORMROOT)/include/ddb.sig
SIGFILES 	+= $(PLATFORMROOT)/include/ddc.sig
SIGFILES 	+= $(PLATFORMROOT)/include/efs.sig
SIGFILES 	+= $(PLATFORMROOT)/include/fm.sig
LIBRARIES  	+= -lefs
OBJECTS		+= $(OBJDIR)/startefs.o

#############################################################################
# HEAP
#############################################################################

SIGFILES  	+= $(PLATFORMROOT)/include/heap.sig
OBJECTS 	+= $(OBJDIR)/heapcon.o
LIBRARIES  	+= -lheap

#############################################################################
# INET
#############################################################################

OSEDEF_H_DEFINES += -DDEFAULT_GATEWAY=\"$(DEFAULT_GATEWAY)\" \
		    -DETH_SUBNETMASK=\"$(ETH_SUBNETMASK)\" 
SIGFILES 	+= $(PLATFORMROOT)/include/inet.sig
SIGFILES 	+= $(PLATFORMROOT)/include/inetmib.sig
SIGFILES 	+= $(PLATFORMROOT)/include/inetlink.sig
OBJECTS 	+= $(OBJDIR)/startinet.o $(OBJDIR)/udppdr.o
LIBRARIES 	+= -linett -linetutil

# lnh
DEFINES		+= -DUSEINET -DUDPPDR -DMASTER
OBJECTS		+= $(OBJDIR)/startlnh.o
SIGFILES 	+= $(PLATFORMROOT)/include/udppdr.sig
OSEDEF_H_DEFINES += -DLNH_ETH_INTERFACE_NAME=\"hme0\"
LIBRARIES	+= -llnh

#############################################################################
# KERNEL
#############################################################################

INCLUDES	+= -I$(KRNROOT)/include
DEFINES		+= -DOSE_DELTA -D_OSE_ 
DEFINES  	+= -DBIG_ENDIAN
OBJECTS 	+= $(OBJDIR)/osemain.o
OBJECTS 	+= $(OBJDIR)/osppcon.o
LIBDIRS		+= -L$(KRNROOT)/lib 
LIBRARIES	+= -lkrnflib 
# opt for speed
# LIBRARIES	+= -lkrn0xxx
# opt for safety 
LIBRARIES	+= -lkrn1xxx

#############################################################################
# PRH
#############################################################################

# for LMs
#  OBJECTS	+= $(OBJDIR)/lmcon.o
OBJECTS 	+= $(OBJDIR)/start_prh.o $(OBJDIR)/prhfuncs.o
LIBRARIES	+= -lprh

#############################################################################
# MMH
#############################################################################

OBJECTS		+= $(OBJDIR)/mmhcon.o
LIBRARIES	+= -lmmh

#############################################################################
# PTHREADS
#############################################################################

LIBRARIES  += -losepthread

#############################################################################
# RTC
#############################################################################

SIGFILES 	+= $(PLATFORMROOT)/include/rtc.sig
LIBRARIES 	+= -lrtc

#############################################################################
# SHELL
#############################################################################

OSEDEF_H_DEFINES	+= -DSHELL_SERUNIT=$(COM2_UNIT)
OSEDEF_H_DEFINES	+= -DSHELL_SERNAME=$(COM2_NAME)
SIGFILES 		+= $(PLATFORMROOT)/include/fss.sig
SIGFILES 		+= $(PLATFORMROOT)/include/login.sig
LIBRARIES		+= -lshell
LIBRARIES		+= -lfss
OBJECTS 		+= $(OBJDIR)/startshell.o 

#############################################################################
# TOSV
#############################################################################

LIBRARIES  += -ltosv

#############################################################################
# ZLIB
#############################################################################

LIBRARIES  += -lzlib

#############################################################################
# PMD
#############################################################################

#############################################################################
# USER
#############################################################################

OBJECTS 	+= $(OBJDIR)/erl_ose_init.o
DEFINES  	+= -DUSE_MY_TIME_SRV

#############################################################################
# COMPILATION FLAGS
#############################################################################

STDINCLUDES	= -I$(PLATFORMROOT)/std-include

INCLS		= -I. -I$(MAKEROOT) -I$(MAKE_OSE_ROOT) -I$(PLATFORMROOT)/include \
	           $(STDINCLUDES) $(INCLUDES) \
		-I$(EXAMPLES_COMMON_INCLUDE) $(BSPINC) $(SIGFILES)

DEFS		= $(DEFINES) $(BSPDEFINE) $(OSEDEF_H_DEFINES) $(LCFDEFINES)

LIBDIRS		+=

LIBS		= -L$(PLATFORMLIB) $(LIBRARIES)

OBJS		= $(OBJDIR)/err_hnd.o $(OBJECTS) $(TEST_OBJS)

#############################################################################
# MAKE USAGE
#############################################################################
# This section contains the default tag evaluated by make if no command line
# parameters are entered.
usage:
	$(ECHO) "Available make targets are:"
	$(ECHO) "clean   - delete the directory $(OBJDIR) and all its files."
	$(ECHO) "ose     - compile OSE kernel system."
	$(ECHO) ""


#############################################################################
# COMPILE RULES
#############################################################################
# Generic compile directives.

$(OBJDIR)/%.o: 		%.c
			$(CC) $(CFLAGS) $(DEFS) $(INCLS) $(CCOUT) $<

$(OBJDIR)/%.o: 		$(MAKEROOT)/%.c
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

$(OBJDIR)/%.o: 		$(EXAMPLES_COMMON_SRC)/%.c
			$(CC) $(CFLAGS) $(DEFS) $(INCLS) $(CCOUT) $<

$(OBJDIR)/%.o: 		$(EXAMPLES_LNH_SRC)/%.c
			$(CC) $(CFLAGS) $(DEFS) $(INCLS) $(CCOUT) $<

$(OBJDIR)/osemain.o:	$(KRNSRC)/osemain.c osemain.con
			$(CC) $(CFLAGS) $(DEFS) $(INCLS) $(CCOUT) $<

$(OBJDIR)/osppcon.o:	$(KRNSRC)/osppcon.c ospp.con
			$(CC) $(CFLAGS) $(DEFS) $(INCLS) $(CCOUT) $<

$(OBJDIR)/pmdcon.o:	$(PLATFORMSRC)/pmdcon.c pmd.con
			$(CC) $(CFLAGS) $(DEFS) $(INCLS) $(CCOUT) $<

# Generic assembly directives.
$(OBJDIR)/%.o: 		$(KRNSRC)/%.s
			$(ASM) $(ASMFLAGS) $(CCOUT) $<

$(OBJDIR)/%.o: 		$(BSPSRC)/%.s
			$(ASM) $(ASMFLAGS) $(CCOUT) $<

$(OBJDIR)/%.o: 		$(TARGETROOT)/lib/%.s
			$(ASM) $(ASMFLAGS) $(CCOUT) $<


#############################################################################
# COMPILATION
#############################################################################

$(OBJDIR)/erl_ose_init.o:	$(ERL_UTILS_SRC)/erl_ose_init.c
	$(CC) $(CFLAGS) $(DEFS) $(INCLS) $(CCOUT) $<

#############################################################################
# LINKING AND GENERATING BINARY
#############################################################################

$(OBJDIR)/$(TARGET).elf:	$(OBJS) $(LCF)
	$(LD) $(LDFLAGS) -o $@ $(CRT) $(OBJS) $(LIBDIRS) $(LIBS)  \
		> $(OBJDIR)/$(TARGET).map

$(OBJDIR)/$(TARGET).bin:	$(OBJDIR)/$(TARGET).elf
	$(ELF2BIN) $(ELF2BINOUT) $(ELF2BINFLAGS) $(OBJDIR)/$(TARGET).elf

$(LCF):
	$(ECHO) "Create" $@
	$(CPPTOFILE) $(CPPFLAGS) $(LCFDEFINES) $(LCFSRC) -o $@

ose:	$(LCF) $(OBJDIR)/$(TARGET).bin
	$(ECHO) ""
	$(ECHO) " OSE BUILT: " $(OBJDIR)/$(TARGET).bin
	$(ECHO) ""

lcf:	$(LCF)


#############################################################################
# MAKE CLEAN
#############################################################################
# This section deletes the $(OBJDIR) directory and all its files.
clean:
	$(RMDIR) $(OBJDIR)
	$(MKDIR) $(OBJDIR)

cleanelf:
	$(RM) $(OBJDIR)/$(ERL_TARGET).elf $(OBJDIR)/$(ERL_TARGET).bin


#############################################################################
# END OF MAKEFILE
#############################################################################
