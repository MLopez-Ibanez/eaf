#------------------------------------------------------*- Makefile-gmake -*-
#  Makefile for eaf

VERSION = 0.1svn$(SVN_REV)

# ---------------------------------------------------------------------

#                       Copyright (c) 2007, 2008
#                  Carlos Fonseca <cmfonsec@ualg.pt>
#             Manuel Lopez-Ibanez <manuel.lopez-ibanez@manchester.ac.uk>

# This program is free software (software libre); you can redistribute
# it and/or modify it under the terms of the GNU General Public
# License as published by the Free Software Foundation; either 
# version 2 of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, you can obtain a copy of the GNU
# General Public License at:
#                  http://www.gnu.org/copyleft/gpl.html
# or by writing to:
#            Free Software Foundation, Inc., 59 Temple Place,
#                  Suite 330, Boston, MA 02111-1307 USA
#
#-----------------------------------------------------------------------
# IMPORTANT NOTE: Please be aware that the fact that this program is
# released as Free Software does not excuse you from scientific
# propriety, which obligates you to give appropriate credit! If you
# write a scientific paper describing research that made substantive
# use of this program, it is your obligation as a scientist to
# acknowledge its use.  Moreover, as a personal note, I would
# appreciate it if you would email <manuel.lopez-ibanez@manchester.ac.uk>
# with citations of papers referencing this work so I can mention them
# to my funding agent and tenure committee.
#-----------------------------------------------------------------------
DEBUG=0
BINDIR := .

OBJS =  eaf.o io.o eaf_main.o eaf3d.o avl.o
HEADERS = eaf.h io_priv.h io.h cvector.h avl.h common.h gcc_attribs.h

EAFEXE=eaf$(EXE)

EXE_LDFLAGS = -lm
ECHO = @echo "$(1)"

-include svnversion.mk
-include gcc.mk

EAF_CFLAGS += $(OPT_CFLAGS) $(WARN_FLAGS) \
	-DDEBUG=$(DEBUG) -DVERSION='"$(VERSION)"' -DMARCH='"$(gcc-guess-march)"'

.PHONY : default all clean distclean test dist

default: $(BINDIR)/$(EAFEXE) 

$(BINDIR)/$(EAFEXE) : $(OBJS)
	@mkdir -p $(BINDIR)
	$(call ECHO,--> Building $@ version $(VERSION) <---)
	$(CC) -o $@ $(OBJS) $(EAF_CFLAGS) $(CFLAGS) $(EXE_LDFLAGS)

$(OBJS) : eaf.h io.h
io.o : io_priv.h

%.o : %.c
	$(CC) $(EAF_CFLAGS) $(CFLAGS) -c -o $@ $<

test: DEBUG=1
test: clean
	$(MAKE) $(BINDIR)/$(EAFEXE)
	$(call ECHO,---> Testing... <--- )
	@perl -w dotest.pl $(BINDIR)/$(EAFEXE) ./testsuite/

clean :
	$(call ECHO,---> Removing $(BINDIR)/$(EAFEXE) <---)
	@$(RM) $(BINDIR)/$(EAFEXE)
	$(call ECHO,---> Removing object files $(OBJS) <---)
	@$(RM) $(OBJS)

all : clean
	$(MAKE) default

DIST_SRC_FILES =  gcc.mk TODO GNUmakefile README LICENSE svn_version \
		  $(OBJS:.o=.c) $(HEADERS)
DIST_SRC := eaf-$(VERSION)-src

dist : DEBUG=0
dist : CDEBUG=
dist :
	@(mkdir -p ../$(DIST_SRC) \
	&& rsync -rlpC --copy-unsafe-links --exclude=.svn $(DIST_SRC_FILES) ../$(DIST_SRC)/ \
	&& cd .. \
	&& tar cf - $(DIST_SRC) | gzip -f9 > $(DIST_SRC).tar.gz \
	&& rm -rf ./$(DIST_SRC)/* && rmdir ./$(DIST_SRC)/ \
	&& echo "$(DIST_SRC).tar.gz created." && cd $(PWD) )


