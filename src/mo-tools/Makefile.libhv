# -*- Makefile-gmake -*-
VARIANT    ?= 4

HV_SRCS    = hv.c
HV_HDRS    = hv.h
HV_OBJS    = $(HV_SRCS:.c=.o)

## Augment CFLAGS for hv.[co] objects
hv.o: CPPFLAGS += -DVARIANT=$(VARIANT)

## Dependencies:
$(HV_OBJS): $(HV_HDRS)
