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

CC        := gcc
CXX       := g++
CFLAGS    := -c -O2 -DUNIX -DENABLE_RENDERING
CXXFLAGS  := -c -O2 -DUNIX -DENABLE_RENDERING

DIR_SRC   := src
DIR_INC   := include
DIR_BUILD := build

CPPOBJ    := $(patsubst $(DIR_SRC)/%.cpp,$(DIR_BUILD)/%.o,$(wildcard $(DIR_SRC)/*.cpp))
COBJ      := $(patsubst $(DIR_SRC)/%.c,$(DIR_BUILD)/%.o,$(wildcard $(DIR_SRC)/*.c))
INCLUDES  := $(addprefix -I ,$(DIR_INC))

# TARGETS
all: start common

common: $(COBJ) $(CPPOBJ)
	ar -rcs $(DIR_BUILD)/common.a $(COBJ) $(CPPOBJ)

$(CPPOBJ) : $(DIR_BUILD)/%.o : $(DIR_SRC)/%.cpp
	$(CXX) $(CXXFLAGS) $(INCLUDES) $< -o $@

$(COBJ) : $(DIR_BUILD)/%.o : $(DIR_SRC)/%.c
	$(CC) $(CFLAGS) $(INCLUDES) $< -o $@

start:
	mkdir -p $(DIR_BUILD)

clean:
	rm -rf $(DIR_BUILD)
