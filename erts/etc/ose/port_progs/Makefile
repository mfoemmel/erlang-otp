#############################################################################
# Makefile for building example port programs
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
#TARGET		= ose_ppc$(KRN)

TARGET		= 

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
MAKEROOT	= $(BUILD_DIR)/port_progs
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
LCFDEFINES	+= -DPOWERPC -DIMAGE_START=0x00400000 -DIMAGE_MAX_LENGTH=2048000

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
# HEAP
#############################################################################

SIGFILES  	+= $(PLATFORMROOT)/include/heap.sig
OBJECTS 	+= $(OBJDIR)/heapcon.o
LIBRARIES  	+= -lheap

#############################################################################
# CRT
#############################################################################

OSEDEF_H_DEFINES	+= -DINFMT_FLOATING_POINT
OBJECTS			+= $(OBJDIR)/infmt.o
OBJECTS			+= $(OBJDIR)/outfmt.o
LIBRARIES		+= -lcrt

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
# PRH
#############################################################################

# for LMs
OBJECTS		+= $(OBJDIR)/lmcon.o
LIBRARIES	+= -lprh

#############################################################################
# USER
#############################################################################
LIBRARIES	+= -lshell -lerl_user

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

LIBS		= -L$(PLATFORMLIB) -L$(USERLIB) $(LIBRARIES)

OBJS		= $(OBJECTS)


#############################################################################
# MAKE USAGE
#############################################################################
# This section contains the default tag evaluated by make if no command line
# parameters are entered.
usage:
	$(ECHO) "Available make targets are:"
	$(ECHO) "clean   - delete the directory $(OBJDIR) and all its files."
	$(ECHO) "ex      - example port program LM."
	$(ECHO) ""


#############################################################################
# COMPILE RULES
#############################################################################
# Create output directory if there is none.
$(OBJDIR):
		$(MKDIR) $(OBJDIR)

# Generic compile directives.

$(OBJDIR)/erl_port_ex.o:	./erl_port_ex.c
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

$(OBJDIR)/%.o: 		%.c
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
# COMPILATION
#############################################################################

$(OBJDIR)/erl_port_prog_ex.elf:	$(OBJS) $(OBJDIR)/erl_port_ex.o $(LCF)
	$(LD) $(LDFLAGS) -o $@ $(CRT) $(OBJS) $(OBJDIR)/erl_port_ex.o $(LIBDIRS) $(LIBS)  \
		> $(OBJDIR)/erl_port_prog_ex.map

conex:		
	$(CP) osemain.con.ex osemain.con

$(LCF):	
	$(ECHO) "Create" $@ "from" $(LCFSRC)
	$(CPPTOFILE) $(CPPFLAGS) $(LCFDEFINES) $(LCFSRC) -o $@

ex:	conex $(LCF) $(OBJDIR)/erl_port_prog_ex.elf
	$(INSERTCONF) lm_ex_cfg $(OBJDIR)/erl_port_prog_ex.elf
	$(ECHO) ""
	$(ECHO) " LM BUILT: " $(OBJDIR)/erl_port_prog_ex.elf
	$(ECHO) ""

lcf:	$(LCF)


#############################################################################
# LINKING AND GENERATING BINARY
#############################################################################

#############################################################################
# MAKE CLEAN
#############################################################################

elfclean:
	$(RM) $(OBJDIR)/$(TARGET).elf
	$(RM) $(LCF)

# This section deletes the $(OBJDIR) directory and all its files.
clean:
	$(RMDIR) $(OBJDIR)
	mkdir $(OBJDIR)


#############################################################################
# END OF MAKEFILE
#############################################################################
