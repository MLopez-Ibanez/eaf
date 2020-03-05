# -*- Makefile-gmake -*-
WARN_CFLAGS = -pedantic -Wall -Wextra

ifeq ($(DEBUG), 0)
  OPT_CFLAGS ?= -O3 -DNDEBUG
# Options -funroll-loops -ffast-math -msse -mfpmath=sse improve performance but are not portable.
# Options -fstandard-precision=fast -ftree-vectorize are not well
# supported in some versions/architectures.
else
  OPT_CFLAGS += -g3 -O0 -fsanitize=undefined -fsanitize=addres
endif

ifdef march
MARCH=$(march)
endif
ifndef MARCH
 MARCH=native
endif
gcc-guess-march = $(strip $(shell $(CC) -march=$(MARCH) $(CFLAGS)  -x c -S -\#\#\# - < /dev/null 2>&1 | \
	 	            grep -m 1 -e cc1 | grep -o -e "march=[^'\"]*" | head -n 1 | sed 's,march=,,'))
ifeq ($(gcc-guess-march),)
gcc-guess-march=unknown
endif
ifneq ($(MARCH),none)
  OPT_CFLAGS += -march=$(MARCH)
endif

