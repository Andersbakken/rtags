# File: sbroot_test/make/module.mk
# Abstract:
#   Designed to be included by a module in an project
#   where the project consists of a 'main' module and several
#   shared library (object) modules.
#
#   MODULE_TYPE = Executable || SharedLibrary
#   MODULE_NAME = name   (optional, defaults to current directory name)
#   MODULE_LIBS = name1 ... nameN
#   include ../module.mk
#

OS     := $(shell uname)
OBJDIR := ../obj/$(OS)/$(notdir $(CURDIR))
BINDIR := $(abspath ../bin/$(OS))

#
# PROG - what we are building
#
MOD_EXT = not-set
ifeq ($(MODULE_TYPE),Executable)
    MOD_EXT    =
    MOD_PREFIX =
    LD_SHARED  =
endif
ifeq ($(MODULE_TYPE),Shared_Library)
    MOD_EXT    = .so
    MOD_PREFIX = lib
    LD_SHARED  = -shared
endif
ifeq ($(MOD_EXT),not-set)
    $(error MODULE_TYPE must be specified as Executable or Shared_Library)
endif

ifeq ($(MODULE_NAME),)
    MODULE_NAME = $(notdir $(CURDIR))
endif

PROG=$(BINDIR)/$(MOD_PREFIX)$(MODULE_NAME)$(MOD_EXT)

#
# Sources, objects, and build flags
#

SOURCES 	= $(wildcard *.cpp)
OBJS    	= $(foreach S,$(SOURCES),$(OBJDIR)/$(S:.cpp=.o))
CXX_DEBUG       =
CXX_DEPEND      = -MMD -MF $(@D)/$(*F).d -MP -MT $@
CXX_FLAGS       = -Wall -Werror -std=c++11
ifeq ($(OS),Linux)
  CXX_FLAGS    += -fPIC
endif
CXX_FLAGS      += -I$(dir $(CURDIR))include
CXX_FLAGS      += -DBUILDING_$(shell echo $(MODULE_NAME) | tr a-z A-Z)
LD_MODULE_LIBS  = $(foreach L,$(MODULE_LIBS),-l$(L))
LD_FLAGS        = -L $(BINDIR) -Wl,-rpath,$(BINDIR)
ifeq ($(OS),Linux)
  LD_FLAGS     += -Wl,--no-undefined
endif

$(PROG): $(OBJS) | $(BINDIR)
	$(CXX) $(LD_FLAGS) $(LD_MODULE_LIBS) $(LD_SHARED) -o $@ $^
	@echo SUCCESS: $(abspath $(PROG))

MAKEFILE_LIST_NO_D = $(filter-out %.d,$(MAKEFILE_LIST))

$(OBJS): $(OBJDIR)/%.o: %.cpp $(MAKEFILE_LIST_NO_D) | $(OBJDIR)
	$(CXX) -c $(CXX_DEPEND) $(CXX_FLAGS) $(CXX_DEBUG) -o $@ $<

$(BINDIR) $(OBJDIR):
	mkdir -p $@

clean:
	$(RM) -r $(OBJDIR) $(PROG)

-include $(OBJS:.o=.d)

# LocalWords:  sbroot OBJDIR notdir CURDIR BINDIR ifeq CXX OBJS Wl rpath MMD
# LocalWords:  MF Werror
