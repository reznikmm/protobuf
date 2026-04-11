# This Makefile is a demo about how to build many proto files taking into
# account their dependencies (i.e. rebuild only affected files after a change).

# Unless you put the `protoc-gen-ada` in your PATH, you should not run `make`
# directly, but instead run `alr build`, which will invoke `make` accordingly.

PROTO_DIR:=proto
OUT_DIR:=src
PARENT_PKG:=my_format-

PROTO_FILES:=$(wildcard $(PROTO_DIR)/*.proto)
ADA_FILES:=$(patsubst $(PROTO_DIR)/%.proto,$(OUT_DIR)/$(PARENT_PKG)%.adb,$(PROTO_FILES))
ADA_FILES+=$(patsubst $(PROTO_DIR)/%.proto,$(OUT_DIR)/$(PARENT_PKG)%.ads,$(PROTO_FILES))
DEP_FILES:=$(patsubst $(PROTO_DIR)/%.proto,$(OUT_DIR)/%.d,$(PROTO_FILES))

all: $(ADA_FILES)
.PHONY: all

-include $(DEP_FILES)

$(OUT_DIR)/$(PARENT_PKG)%.adb $(OUT_DIR)/%.ads : $(PROTO_DIR)/%.proto
	protoc --ada_out=$(OUT_DIR) --proto_path=$(PROTO_DIR) --dependency_out=$(OUT_DIR)/$*.d $<

clean:
	rm -f $(ADA_FILES) $(DEP_FILES)
.PHONY: clean
