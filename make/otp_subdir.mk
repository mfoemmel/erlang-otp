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
#
# Make include file for otp

.PHONY: debug opt instr release docs release_docs tests release_tests \
	clean depend

#
# Targets that don't affect documentation directories
#
debug opt instr release docs release_docs tests release_tests clean depend:
	@set -e ;				\
	case "$(MAKE)" in *clearmake*) tflag="-T";; *) tflag="";; esac ; \
	for d in $(SUB_DIRECTORIES); do		\
		if test -f $$d/SKIP ; then	\
			echo "=== Skipping subdir $$d, reason:" ; \
			cat $$d/SKIP ;		\
			echo "===" ;		\
		else				\
			if test ! -d $$d ; then	\
				echo "=== Skipping subdir $$d" ; \
			else			\
				xflag="" ;	\
				if test -f $$d/ignore_config_record.inf; then \
					xflag=$$tflag ;	\
				fi ;			\
				(cd $$d && $(MAKE) $$xflag $@) ; \
			fi ;			\
		fi ;				\
	done
