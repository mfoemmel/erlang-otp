# ``The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved via the world wide web at http://www.erlang.org/.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
# 
# The Initial Developer of the Original Code is Ericsson Utvecklings AB.
# Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
# AB. All Rights Reserved.''
# 
#     $Id$
#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

#
# Macros
#
ifeq ($(findstring vxworks,$(TARGET)),vxworks)
ERTS_SUB_DIRECTORIES = stdlib snmp sasl kernel compiler
OTHER_SUB_DIRECTORIES =	\
  appmon erl_interface os_mon tools runtime_tools
ifdef BUILD_ALL
OTHER_SUB_DIRECTORIES += mnesia jinterface ic mnemosyne asn1 debugger \
  eva inets mnesia_session orber pman tv observer cosTransactions cosEvent \
  cosTime cosNotification cosProperty cosFileTransfer cosEventDomain
endif
else

ERTS_SUB_DIRECTORIES = stdlib snmp sasl kernel compiler

ifdef BUILD_ALL
ifeq ($(findstring win32,$(TARGET)),win32)
OTHER_SUB_DIRECTORIES = 			\
  appmon asn1 erl_interface jinterface eva gs inets ic \
  mnesia mnemosyne mnesia_session $(CRYPTO_APP) orber os_mon parsetools pman \
  $(SSL_APP) toolbar tools tv observer debugger odbc runtime_tools \
  cosTransactions cosEvent cosTime cosNotification cosProperty \
  cosFileTransfer cosEventDomain et megaco webtool
else
OTHER_SUB_DIRECTORIES = \
  appmon asn1 erl_interface jinterface eva debugger gs inets \
  ic mnesia  mnemosyne mnesia_session $(CRYPTO_APP) orber os_mon parsetools \
  pman $(SSL_APP) toolbar tools tv observer \
  runtime_tools cosTransactions cosEvent cosTime cosNotification \
  cosProperty cosFileTransfer cosEventDomain et megaco webtool
ifeq ($(findstring sparc-sun-solaris,$(TARGET)),sparc-sun-solaris)
  OTHER_SUB_DIRECTORIES += odbc
endif
OTHER_SUB_DIRECTORIES += $(HIPE_APP) $(TSP_APP)
endif
endif
endif

ifdef BOOTSTRAP
SUB_DIRECTORIES = \
  stdlib compiler jinterface orber/include eva/include
else
ifdef SECONDARY_BOOTSTRAP
SUB_DIRECTORIES = kernel parsetools asn1 $(HIPE_APP)
else
ifdef TERTIARY_BOOTSTRAP
SUB_DIRECTORIES = snmp_dummy snmp
else
ifdef FOURTH_BOOTSTRAP
SUB_DIRECTORIES = sasl jinterface ic
else
SUB_DIRECTORIES = $(ERTS_SUB_DIRECTORIES) $(OTHER_SUB_DIRECTORIES)
endif
endif
endif
endif


# ----------------------------------------------------------------------
include $(ERL_TOP)/make/otp_subdir.mk
