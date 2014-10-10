# Copyright 2005-2012 Intel Corporation.  All Rights Reserved.
# The source code contained or described herein and all documents related
# to the source code ("Material") are owned by Intel Corporation or its
# suppliers or licensors.  Title to the Material remains with Intel
# Corporation or its suppliers and licensors.  The Material is protected
# by worldwide copyright laws and treaty provisions.  No part of the
# Material may be used, copied, reproduced, modified, published, uploaded,
# posted, transmitted, distributed, or disclosed in any way without
# Intel's prior express written permission.

# The original source for this example is
# Copyright (c) 1994-2008 John E. Stone
# All rights reserved.

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
# 3. The name of the author may not be used to endorse or promote products
#    derived from this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
# OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
# OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
# SUCH DAMAGE.

# No license under any patent, copyright, trade secret or other
# intellectual property right is granted to or conferred upon you by
# disclosure or delivery of the Materials, either expressly, by
# implication, inducement, estoppel or otherwise.  Any license under such
# intellectual property rights must be express and approved by Intel in
# writing.

# GNU Makefile that builds and runs example.
NAME:=tachyon

# The C++ compiler
#CXX = g++
CXX = icpc

# The default dataset
export DATASET = balls
# The default runtime arguments
export ARGS =

# define name suffix
SUFFIX = .$(VERSION)

LIBS ?= -lm

include ./common/gui/Makefile.gmake

ifeq ($(UI),x)
CXXFLAGS += -DX_FULLSYNC 
ifneq (,$(findstring -lXext,$(LIBS)))
CXXFLAGS += -DX_NOSHMPIX
endif
endif # X

MYCXXFLAGS = $(CXXFLAGS)

all: build run

build_opt: build
build_with_debug: build_debug
test: run

build: build_serial build_openmp build_tbb build_cilk
build_debug: build_serial_debug build_openmp_debug build_tbb_debug build_cilk_debug
run: run_serial run_openmp run_tbb run_cilk


serial: build_serial run_serial
serial_debug: build_serial_debug run_serial
openmp: build_openmp run_openmp
openmp_debug: build_openmp_debug run_openmp
tbb: build_tbb run_tbb
tbb_debug: build_tbb_debug run_tbb

cilk: build_cilk run_cilk
cilk_debug: build_cilk_debug run_cilk

######################SERIAL
build_serial:
	$(MAKE) VERSION=serial build_one
build_serial_debug:
	$(MAKE) VERSION=serial ADD_DEBUG=1 build_one
run_serial:
	$(MAKE) VERSION=serial run_one

######################OpenMP
build_openmp:
	$(MAKE) VERSION=with_openmp ADD_OPENMP=1 build_one
build_openmp_debug:
	$(MAKE) VERSION=with_openmp ADD_OPENMP=1 ADD_DEBUG=1 build_one
run_openmp:
	$(MAKE) VERSION=with_openmp run_one

######################TBB
build_tbb:
	$(MAKE) VERSION=with_tbb ADD_TBB=1 build_one
build_tbb_debug:
	$(MAKE) VERSION=with_tbb ADD_TBB=1 ADD_DEBUG=1 build_one
run_tbb:
	$(MAKE) VERSION=with_tbb run_one

######################cilk
build_cilk:
	$(MAKE) VERSION=with_cilk build_one
build_cilk_debug:
	$(MAKE) VERSION=with_cilk ADD_DEBUG=1 build_one
run_cilk:
	$(MAKE) VERSION=with_cilk run_one


#
# Per-build Makefile rules (for recursive $(MAKE) calls from above)
#

SVERSION = $(VERSION)

ifeq ($(ADD_DEBUG),1)
MYCXXFLAGS += -O0 -g -D_DEBUG
else
MYCXXFLAGS += -O2
endif

ifeq ($(ADD_OPENMP),1)
MYCXXFLAGS += -qopenmp
else
MYCXXFLAGS +=
endif


ifeq ($(ADD_TBB),1)
MYCXXFLAGS +=
ifeq ($(ADD_DEBUG),1)
MYCXXFLAGS += -DTBB_USE_DEBUG
LIBS += -ltbb_debug
else
LIBS += -ltbb
endif
endif

GUISOURCE = ./common/gui/$(UI)video.cpp
SOURCE = src/build_$(SVERSION)/build_$(SVERSION).cpp src/Windows/pthread.cpp src/video.cpp src/api.cpp src/apigeom.cpp src/apitrigeom.cpp src/bndbox.cpp src/box.cpp src/camera.cpp src/coordsys.cpp src/cylinder.cpp src/extvol.cpp src/getargs.cpp src/global.cpp src/grid.cpp src/imageio.cpp src/imap.cpp src/intersect.cpp src/jpeg.cpp src/light.cpp src/objbound.cpp src/parse.cpp src/plane.cpp src/ppm.cpp src/quadric.cpp src/render.cpp src/ring.cpp src/shade.cpp src/sphere.cpp src/texture.cpp src/tgafile.cpp src/trace_rest.cpp src/triangle.cpp src/ui.cpp src/util.cpp src/vector.cpp src/vol.cpp

build_one:	$(EXE)

run_one:
ifeq ($(strip $(have_libX11_3)),)
ifeq ($(strip $(have_libX11_2)),)
ifeq ($(strip $(have_libX11_1)),)
	@ echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"	
	@ echo "Warning: You  don't have library libX11.so in directories  /usr/X11R6/lib$(x64) or /usr/lib$(x64). If you want to see rendered picture:"
	@ echo "Please, install the X11 dev tools. Or if it is installed then make symlink  in /usr/X11R6/lib$(x64)/ or /usr/lib$(x64)/for libX11.so"
	@ echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"		
	@ echo ""
endif
endif
endif
	
ifeq ($(strip $(have_libXext_3)),)
ifeq ($(strip $(have_libXext_2)),)
ifeq ($(strip $(have_libXext_1)),)
	@ echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"	
	@ echo "Warning: You  don't have library libXext.so in directories  /usr/X11R6/lib$(x64) or /usr/lib$(x64). If you want to see rendered picture:"	
	@ echo "Please, install the X11 dev tools. Or if it is installed then make symlink  in /usr/X11R6/lib$(x64)/ or /usr/lib$(x64)/for libXext.so"	
	@ echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"		
	@ echo ""
endif
endif
endif
				
ifeq ($(UI),mac)
	export DYLD_LIBRARY_PATH="$(DYLD_LIBRARY_PATH):$(TBBLIBSPATH)"; ./$(EXE) $(ARGS) dat/$(DATASET).dat
else
	./$(EXE) $(ARGS) dat/$(DATASET).dat
endif

$(EXE): $(SOURCE) $(GUISOURCE)
ifeq ($(UI),mac)
	mkdir -p $(APPRES)/English.lproj $(NAME).$(VERSION).app/Contents/MacOS
	$(PBXCP) xcode/English.lproj/main.nib $(APPRES)/English.lproj
	$(PBXCP) xcode/Info.plist $(APPRES)
endif
	$(CXX) $(MYCXXFLAGS) -o $@ $(SOURCE) $(GUISOURCE) $(LIBS)
	$(RM) *.o

clean: VERSION = *
clean:
	$(RM) $(EXE) *.o *.d
ifeq ($(UI),mac)
	rm -rf $(NAME).*
endif
