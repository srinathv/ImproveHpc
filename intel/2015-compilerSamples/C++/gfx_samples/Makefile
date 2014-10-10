#
#    Copyright 2005-2014 Intel Corporation.  All Rights Reserved.
#
# The source code contained or described herein and all
# documents related to the source code ("Material") are owned by 
# Intel Corporation or its suppliers or licensors. Title to the
# Material remains with Intel Corporation or its suppliers and
# licensors. The Material is protected by worldwide copyright
# laws and treaty provisions.  No part of the Material may be
# used, copied, reproduced, modified, published, uploaded,
# posted, transmitted, distributed,  or disclosed in any way
# except as expressly provided in the license provided with the
# Materials.  No license under any patent, copyright, trade
# secret or other intellectual property right is granted to or
# conferred upon you by disclosure or delivery of the Materials,
# either expressly, by implication, inducement, estoppel or
# otherwise, except as expressly provided in the license
# provided with the Materials. 
#
# This is Makefile for Linux
#

OUT_DIR := out
SRC_DIR := src

ICC := icc
CFLAGS = -MMD
LDFLAGS = -lrt

RM := rm -f

SAMPLES := $(notdir $(wildcard $(SRC_DIR)/Workloads/*))
SAMPLES_OUTS := $(addprefix $(OUT_DIR)/,$(SAMPLES))
SAMPLES_RUNS := $(SAMPLES:%=%-run)

SAMPLES_OBJS := $(wildcard $(SRC_DIR)/Workloads/*/*.cpp)
SAMPLES_OBJS := $(patsubst $(SRC_DIR)/%,$(OUT_DIR)/%,$(SAMPLES_OBJS))
SAMPLES_OBJS := $(patsubst %.cpp,%.o,$(SAMPLES_OBJS))

SAMPLES_LOGS := $(wildcard $(SRC_DIR)/Workloads/*/*.prop)
SAMPLES_LOGS := $(patsubst $(SRC_DIR)/Workloads/%.prop,$(OUT_DIR)/tmp/%-run.log,$(SAMPLES_LOGS))
SAMPLES_LOGS := $(patsubst $(SRC_DIR)/%,$(OUT_DIR)/%,$(SAMPLES_LOGS))

SHARED_OBJS := Util.o Properties.o main.o PerfTimer.o
SHARED_OBJS := $(addprefix $(OUT_DIR)/,$(SHARED_OBJS))

.PHONY: all clean run report $(SAMPLES_RUNS) help
all: $(SAMPLES_OUTS)
run: $(SAMPLES_RUNS)
report: $(SAMPLES_LOGS)
	@for log in `ls $(OUT_DIR)/*.log`; do echo `basename $$log -run.log` - `tail -n 1 $$log`; done

.SECONDEXPANSION:
$(SAMPLES_LOGS): $(OUT_DIR)/tmp/%-run.log: $(OUT_DIR)/$$(dir $$*)
$(SAMPLES_LOGS): $(OUT_DIR)/tmp/%-run.log: $(SRC_DIR)/Workloads/%.prop
	$(eval _PRP:=$(patsubst %-run.log,%,$(notdir $@)))
	$(eval _SMP:=$(notdir $(patsubst %/,%,$(dir $@))))
	@$(call run_smp,$(_SMP),$(_PRP))

$(SHARED_OBJS): $(OUT_DIR)/%.o: $(SRC_DIR)/%.cpp

#%.o: %.cpp
$(OUT_DIR)/%.o: $(SRC_DIR)/%.cpp
	mkdir -p $(dir $@)
	$(ICC) -c $(CFLAGS) -o $@ -I./$(SRC_DIR) $<

define make-target 
$(OUT_DIR)/$1: $(patsubst $(SRC_DIR)/%.cpp,$(OUT_DIR)/%.o,$(wildcard $(SRC_DIR)/Workloads/$1/*.cpp))
$(OUT_DIR)/$1: $(SHARED_OBJS)
	$(ICC) -o $$@ $$(SAMPLE_OBJS) $$^ $(LDFLAGS)
endef

$(foreach sample,$(SAMPLES),$(eval $(call make-target,$(sample))))

$(OUT_DIR):
	mkdir -p $(OUT_DIR)

run_smp = echo "Running $(1)-$(2)..."; mkdir -p $(OUT_DIR)/tmp/$(1); cp $(SRC_DIR)/Workloads/$(1)/$(2).prop $(OUT_DIR)/; $(OUT_DIR)/$(1) $(OUT_DIR)/$(2).prop 1> $(OUT_DIR)/tmp/$(1)/$(2)-run.log 2> $(OUT_DIR)/tmp/$(1)/$(2)-run.err; cp $(OUT_DIR)/tmp/$(1)/$(2)-run.log $(OUT_DIR)/tmp/$(1)/$(2)-run.err $(OUT_DIR)/

$(SAMPLES_RUNS): %-run: $(OUT_DIR)/%
	$(eval PROPS:=$(wildcard $(SRC_DIR)/Workloads/$*/*.prop))
	@for prop in $(PROPS); do base=`basename $$prop .prop`; $(call run_smp,$*,$$base) || true; done

SAMPLES_OBJ := $(wildcard $(OUT_DIR)/Workloads/*/*.o)
clean:
	$(RM) $(SHARED_OBJS) $(SHARED_OBJS:.o=.d)
	$(if $(SAMPLES_OBJ),$(RM) $(SAMPLES_OBJ) $(SAMPLES_OBJ:.o=.d))
	$(RM) -r $(OUT_DIR)

-include $(SHARED_OBJS:.o=.d) $(SAMPLES_OBJ:.o=.d)
