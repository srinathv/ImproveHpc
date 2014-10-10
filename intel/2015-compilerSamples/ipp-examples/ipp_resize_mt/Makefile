################################################################################
## Copyright(C) 2014 Intel Corporation. All Rights Reserved.
##                                                                             
## The source code, information and material ("Material") contained herein is 
## owned by Intel Corporation or its suppliers or licensors, and title 
## to such Material remains with Intel Corporation or its suppliers or 
## licensors. The Material contains proprietary information of Intel or 
## its suppliers and licensors. The Material is protected by worldwide 
## copyright laws and treaty provisions. No part of the Material may be used, 
## copied, reproduced, modified, published, uploaded, posted, transmitted, 
## distributed or disclosed in any way without Intel's prior express written 
## permission. No license under any patent, copyright or other intellectual 
## property rights in the Material is granted to or conferred upon you, 
## either expressly, by implication, inducement, estoppel or otherwise. 
## Any license under such intellectual property rights must be express and 
## approved by Intel in writing. Unless otherwise agreed by Intel in writing, 
## you may not remove or alter this notice or any other notice embedded in 
## Materials by Intel or Intel's suppliers or licensors in any way.

APP            := ipp_resize_mt
CXX            := g++
CXXFLAGS       := -c -O2

DIR_SRC        := src
DIR_BUILD      := build
DIR_INC        := ../common/include $(IPPROOT)/include
DIR_COMMON     := ../common
DIR_LIB.x86_64 := $(IPPROOT)/lib/intel64
DIR_LIB.i686   := $(IPPROOT)/lib/ia32

CPPOBJ         := $(patsubst $(DIR_SRC)/%.cpp,$(DIR_BUILD)/%.o,$(wildcard $(DIR_SRC)/*.cpp))
INCLUDES       := $(foreach d, $(DIR_INC), $(addprefix -I,$d))
LIBS           := -L$(DIR_LIB.$(shell uname -m)) -lippcore -lippi -lipps -lippvm $(DIR_COMMON)/build/common.a -lGL

# TARGETS
all: start common app

app: $(CPPOBJ)
	$(CXX) $< -o $(DIR_BUILD)/$(APP) -Xlinker --start-group $(LIBS) -Xlinker --end-group

$(CPPOBJ): $(DIR_BUILD)/%.o : $(DIR_SRC)/%.cpp
	$(CXX) $(CXXFLAGS) $(INCLUDES) $< -o $@

common:
	make -C $(DIR_COMMON)

start:
	mkdir -p $(DIR_BUILD)

clean:
	rm -rf $(DIR_BUILD)
	@make -C $(DIR_COMMON) clean
