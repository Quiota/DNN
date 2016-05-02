##******************************************************************************
##  Copyright(C) 2001-2015 Intel Corporation. All Rights Reserved.
##  
##  The source code, information  and  material ("Material") contained herein is
##  owned  by Intel Corporation or its suppliers or licensors, and title to such
##  Material remains  with Intel Corporation  or its suppliers or licensors. The
##  Material  contains proprietary information  of  Intel or  its  suppliers and
##  licensors. The  Material is protected by worldwide copyright laws and treaty
##  provisions. No  part  of  the  Material  may  be  used,  copied, reproduced,
##  modified, published, uploaded, posted, transmitted, distributed or disclosed
##  in any way  without Intel's  prior  express written  permission. No  license
##  under  any patent, copyright  or  other intellectual property rights  in the
##  Material  is  granted  to  or  conferred  upon  you,  either  expressly,  by
##  implication, inducement,  estoppel or  otherwise.  Any  license  under  such
##  intellectual  property  rights must  be express  and  approved  by  Intel in
##  writing.
##  
##  *Third Party trademarks are the property of their respective owners.
##  
##  Unless otherwise  agreed  by Intel  in writing, you may not remove  or alter
##  this  notice or  any other notice embedded  in Materials by Intel or Intel's
##  suppliers or licensors in any way.
##
##******************************************************************************
##  Content:
##      Intel(R) Math Kernel Library CBLAS tests creation and run
##******************************************************************************

help:
	@echo "Usage: make {libia32|soia32|libintel64|sointel64} [test=name]"
	@echo "[compiler=compiler_name] [interface=interface_name] [threading=threading_name]"
	@echo
	@echo "name     - test name. Please see cblas.lst file"
	@echo
	@echo "compiler_name     - can be gnu or intel. Default value is intel."
	@echo "                    Intel (R) Fortran Compiler as default."
	@echo "                    If compiler=gnu then GNU gfortran compiler will be used."
	@echo "                    To use g77 add FC=g77. FC=g77 cannot be used with ilp64 interface."
	@echo
	@echo "interface_name    - can be lp64 or ilp64 for intel64. Default value is lp64."
	@echo
	@echo "threading_name    - can be parallel or sequential. Default value is parallel."

##------------------------------------------------------------------------------
## examples of using:
##
## make libia32 test=dblat3 - build  by  Intel(R) Fortran Compiler  (as default)
##                               and  run  DBLAT3 test for 32-bit  applications,
##                               static linking
##
## make soia32 compiler=gnu - build by GNU Fortran compiler and run all tests
##                               of MKL for 32-bit applications, dynamic linking
##
## make sointel64           - build  by  Intel(R) Fortran Compiler  (as default)
##                               and  run  all  tests  of  MKL   for  Intel(R) 64
##                               processor family applications, dynamic linking
##------------------------------------------------------------------------------

include cblas.lst

ifndef test
test = $(CBLAS)
endif

ifndef compiler
compiler=intel
endif

ifndef interface
interface=lp64
endif

ifndef threading
threading=parallel
endif

RES = $(addsuffix .res,$(test))

ifndef MKLROOT
MKLROOT = ../..
endif
MKL_PATH = "$(MKLROOT)/lib/$(_IA)"
CMPLR_PATH = "$(MKLROOT)/../compiler/lib/$(_IA)"
FOPTS =
ifeq ($(compiler),gnu)
FC=gfortran
FOPTS += $(if $(filter ia32, $(_IA)), -m32, -m64)
ifeq ($(FC),gfortran)
IFACE_COMP_PART=gf
else
IFACE_COMP_PART=intel
endif
IFACE_THREADING_PART=intel
else
override FC=ifort
IFACE_COMP_PART=intel
IFACE_THREADING_PART=intel
endif

ifeq ($(interface),ilp64)
IFACE_LIB = mkl_$(IFACE_COMP_PART)_ilp64
ifeq ($(compiler),intel)
FOPTS += -i8
endif
ifeq ($(compiler),gnu)
override FC=gfortran
FOPTS += -fdefault-integer-8
endif
else
IFACE_LIB = mkl_$(IFACE_COMP_PART)_lp64
endif

ifeq ($(_IA),ia32)
   ifeq ($(compiler),intel)
       SPEC_OPT=-xK
#This option is required by Intel(R) 11.0 compiler to produce workable binaries for Pentium(R) III.
#If you don't need it, you can remove this option.
   endif
IFACE_LIB = mkl_$(IFACE_COMP_PART)
endif

ifeq ($(threading),sequential)
THREADING_LIB = mkl_sequential
OMP_LIB =
else
THREADING_LIB = mkl_$(IFACE_THREADING_PART)_thread
OMP_LIB = -L$(CMPLR_PATH) -liomp5
endif

CORE_LIB = mkl_core

ifeq ($(EXT),a)
    MKL_LIBS = -Wl,--start-group $(MKL_PATH)/$(RES_EXT)$(IFACE_LIB).$(EXT) $(MKL_PATH)/$(RES_EXT)$(THREADING_LIB).$(EXT) $(MKL_PATH)/$(RES_EXT)$(CORE_LIB).$(EXT) -Wl,--end-group
else
    MKL_LIBS = -L$(MKL_PATH) -l$(IFACE_LIB) -l$(THREADING_LIB) -l$(CORE_LIB)
endif

ifeq ($(_IA),ia32)
RES_DIR=_results/$(compiler)_$(threading)_$(_IA)_$(RES_EXT)$Z
else
RES_DIR=_results/$(compiler)_$(interface)_$(threading)_$(_IA)_$(RES_EXT)$Z
endif

libia32 lib32:
	$(MAKE) $(RES) _IA=ia32    EXT=a  RES_EXT=lib PLATF=ia32
soia32 so32:
	$(MAKE) $(RES) _IA=ia32    EXT=so RES_EXT=so  PLATF=ia32
libintel64 libem64t:
	$(MAKE) $(RES) _IA=intel64 EXT=a  RES_EXT=lib PLATF=intel64
sointel64 soem64t:
	$(MAKE) $(RES) _IA=intel64 EXT=so RES_EXT=so  PLATF=intel64

#-------------------------------------------------------------------------------

vpath %.f ../blas_common/source

$(RES): %.res: %.f
	mkdir -p ./$(RES_DIR)
	$(FC) $(SPEC_OPT) $(FOPTS) -w -O0 $< ../blas_common/source/auxspars.f ../blas_common/source/libcwrap_$(PLATF).a $(MKL_LIBS) $(OMP_LIB) -lpthread -ldl -o $(RES_DIR)/$*.out
	export LD_LIBRARY_PATH=$(MKL_PATH):$(LD_LIBRARY_PATH):$(CMPLR_PATH); \
	if [ -e ../blas_common/in/$*.in ]; \
	then $(RES_DIR)/$*.out <../blas_common/in/$*.in >$(RES_DIR)/$@; \
	else $(RES_DIR)/$*.out >$(RES_DIR)/$@; fi
	if [ -e $*.sum ]; then cp -f $*.sum $(RES_DIR)/$@; fi
	rm -f $*.s* $(RES_DIR)/*.o
#-------------------------------------------------------------------------------
ifeq ($(compiler),gnu)
$(addsuffix .res,$(GNU_NO_F2C)): FOPTS += -fno-f2c
endif
