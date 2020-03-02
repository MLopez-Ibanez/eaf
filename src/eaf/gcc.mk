# -*- Makefile-gmake -*-
gcc-guess-march = $(strip $(shell $(CC) $(CFLAGS) -march=$(MARCH) -x c -S -\#\#\# - < /dev/null 2>&1 | \
                grep -m 1 -o -e "march=[^'\"]*" | \
                sed 's,march=,,'))

WARN_CFLAGS = -pedantic -Wall -Wextra
EAF_CFLAGS += $(WARN_CFLAGS)

ifeq ($(DEBUG), 0)
  OPT_CFLAGS := -O3 -funroll-loops -DNDEBUG
# Options -ffast-math -msse -mfpmath=sse improve performance but are not portable.
# Options -fstandard-precision=fast -ftree-vectorize are not well
# supported in some versions/architectures.
else
  EAF_CFLAGS += -g3 -O0
endif

ifneq ($(MARCH),none)
  ifndef MARCH
    MARCH = native
  endif
  ARCH := $(gcc-guess-march)
  OPT_CFLAGS += -march=$(ARCH)
endif
