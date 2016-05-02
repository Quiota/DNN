##*****************************************************************************
## Copyright(C) 2001-2013 Intel Corporation. All Rights Reserved.
##
## The source code, information  and  material ("Material") contained herein is
## owned  by Intel Corporation or its suppliers or licensors, and title to such
## Material remains  with Intel Corporation  or its suppliers or licensors. The
## Material  contains proprietary information  of  Intel or  its  suppliers and
## licensors. The  Material is protected by worldwide copyright laws and treaty
## provisions. No  part  of  the  Material  may  be  used,  copied, reproduced,
## modified, published, uploaded, posted, transmitted, distributed or disclosed
## in any way  without Intel's  prior  express written  permission. No  license
## under  any patent, copyright  or  other intellectual property rights  in the
## Material  is  granted  to  or  conferred  upon  you,  either  expressly,  by
## implication, inducement,  estoppel or  otherwise.  Any  license  under  such
## intellectual  property  rights must  be express  and  approved  by  Intel in
## writing.
##
## *Third Party trademarks are the property of their respective owners.
##
## Unless otherwise  agreed  by Intel  in writing, you may not remove  or alter
## this  notice or  any other notice embedded  in Materials by Intel or Intel's
## suppliers or licensors in any way.
##
##*****************************************************************************
##  Content:
##      Building and running Intel(R) Math Kernel Library example for 
##      C-style MIC Compiler Assisted Offload TR
##******************************************************************************

help:
	@echo "To build and run TR examples:"
	@echo "  make {libintel64|sointel64}"
	@echo "       [interface=<name>] [threading=<name>] [function=<name>]"
	@echo
	@echo "To get help just run make or:"
	@echo "  make help"
	@echo
	@echo "To clean results:"
	@echo "  make clean"
	@echo
	@echo "Main options:"
	@echo "  targets lib%   use static linkage"
	@echo "  targets so%    use dynamic linkage"
	@echo
	@echo "  interface=<name> selects kind of MKL_INT type for %intel64 targets:"
	@echo "      lp64  - 32-bit integers (DEFAULT)"
	@echo "      ilp64 - 64-bit integers"
	@echo
	@echo "  threading=<name> selects threading of MKL:"
	@echo "      parallel   - multithreaded version (DEFAULT)"
	@echo "      sequential - sequential version"
	@echo
	@echo "  function=<name> selects examples to execute"
	@echo "      Default value: all examples listed in file trsolverc.lst"
	@echo
	@echo "Additional options:"
	@echo "  RES_DIR=<path> defines where to place the results"
	@echo "      Default value: ./_results"
	@echo
	@echo "  MKLROOT=<path> defines alternative MKL root directory"
	@echo "      Default value: ../../.."

# Validate command line parameters and set default values
include trsolverc.lst
ifndef function
	# If example is not specified execute all available
	function = $(EXAMPLES)
endif

RES = $(addsuffix .res ,$(function))

# Check that interface is lp64 or ilp64 for Intel64
ifeq (,$(filter lp64 ilp64,$(interface)))
  override interface = lp64
endif

# Only one interface on IA-32
ifeq (,$(findstring 32,$(target)))
  iface = $(interface)_
else
  override interface =
endif

# Check that threading is sequential or parallel
ifeq (,$(filter parallel sequential,$(threading)))
  override threading = parallel
endif

# Initialize MKLROOT value
ifndef MKLROOT
	MKLROOT = ../../..
endif

ifndef RES_DIR
	RES_DIR=./_results
endif

# Set compiler flags and options
CC=icc
LD=icc

COPTS=-O3 -openmp -std=c99
LDOPTS=-openmp

ifeq ($(interface),ilp64)
	IFACE_LIB=mkl_intel_ilp64
	COPTS += -DMKL_ILP64
else
	IFACE_LIB=mkl_intel_lp64
endif

ifeq ($(threading),sequential)
	THREADING_LIB=mkl_sequential
else
	THREADING_LIB=mkl_intel_thread
endif

CORE_LIB=mkl_core

ifeq ($(linking),static)
	MKL_LIBS=-Wl,--start-group $(MKLROOT)/lib/$(arch)/lib$(IFACE_LIB).a $(MKLROOT)/lib/$(arch)/lib$(THREADING_LIB).a $(MKLROOT)/lib/$(arch)/lib$(CORE_LIB).a -Wl,--end-group -lpthread -lm
	MKL_MIC_LIBS=-Wl,--start-group $(MKLROOT)/lib/mic/lib$(IFACE_LIB).a $(MKLROOT)/lib/mic/lib$(THREADING_LIB).a $(MKLROOT)/lib/mic/lib$(CORE_LIB).a -Wl,--end-group
else
	MKL_LIBS=-L$(MKLROOT)/lib/$(arch) -l$(IFACE_LIB) -l$(THREADING_LIB) -l$(CORE_LIB) -lpthread -lm
	MKL_MIC_LIBS=-L$(MKLROOT)/lib/mic -l$(IFACE_LIB) -l$(THREADING_LIB) -l$(CORE_LIB)
endif

ifneq ($(target),)
	res_dir=$(RES_DIR)/intel_$(interface)_$(threading)_$(target)
else
	res_dir=$(RES_DIR)
endif
src_dir=./source
	
# Rules
.PHONY: libintel64 sointel64 help run clean
.SUFFIXES:
.SUFFIXES: .c .o .res
vpath %.c $(src_dir)

run: mkdir $(RES)

libintel64:
	$(MAKE) run interface=$(interface) threading=$(threading) arch=intel64 linking=static target=libintel64

sointel64:
	$(MAKE) run interface=$(interface) threading=$(threading) arch=intel64 linking=dynamic target=sointel64

mkdir:
	mkdir -p $(res_dir)
	
clean:
	rm -rf $(res_dir)

%.res: %.c
	# Compiler options are passed to MIC compiler automatically, use -offload-option,mic,compiler to pass additional options to compiler
	$(CC) -c -offload-attribute-target=mic $(COPTS) -I$(MKLROOT)/include $< -o $(res_dir)/$*.o
	# Path to MIC libraries should be passed with -offload-option,mic,compiler and not -offload-option,mic,ld, as the latter one will be overriden by host options
	$(LD) $(res_dir)/$*.o $(LDOPTS) $(MKL_LIBS) -offload-option,mic,compiler,"$(MKL_MIC_LIBS)" -o $(res_dir)/$*.out
	export LD_LIBRARY_PATH=$(MKLROOT)/lib/$(arch):$(LD_LIBRARY_PATH); \
	export MIC_LD_LIBRARY_PATH=$(MKLROOT)/lib/mic:$(MIC_LD_LIBRARY_PATH); \
	$(res_dir)/$*.out >$(res_dir)/$*.res
