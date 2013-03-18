include config.mk

SYSROOT		= ${NDK_PATH}/platforms/android-$(API_LEVEL)/arch-$(ARCHITECTURE)
FULL_PATH	= ${NDK_PATH}/toolchains/$(TOOLCHAIN)/prebuilt/$(HOST)/bin/$(PREFIX)

CC		= $(FULL_PATH)gcc --sysroot=$(SYSROOT)
ADB		= $(SDK_PATH)/platform-tools/adb
EMULATOR	= $(SDK_PATH)/tools/emulator
ANDROID		= $(SDK_PATH)/tools/android

EMULATOR_NAME		= ARM-Simulator
EMULATOR_VERSION	= Google Inc.:Google APIs:8
API_LEVEL			= 8

ASM_SRC		= $(wildcard *.s)
ASM_OBJS	= $(ASM_SRC:.s=.o)
ASMTARGET	= a.out

TARGET		= main
OPTS		= -pp camlp4o
TAGS		= annot,debug
LIBS		= str
EXTENSION	= byte

CFLAGS		= -fno-short-enums -fstrict-aliasing -O2 -Wall -g

all:
	rm -f *.o
	ocamlbuild $(OPTS) -tags $(TAGS) -libs $(LIBS) $(TARGET).$(EXTENSION)

create:
	$(ANDROID) create avd -n "$(EMULATOR_NAME)" -t "${EMULATOR_VERSION}"

emulator:
ifneq ($(shell $(ADB) get-state),device)
	$(EMULATOR) -avd "$(EMULATOR_NAME)"&
endif


lib:
	$(CC) -S $(CFLAGS) perl.c

obj: $(ASM_OBJS)
	$(CC) -o $(ASMTARGET) $^ $(CFLAGS)

try:
	$(ADB) wait-for-device push $(ASMTARGET) /data/local/
	$(ADB) wait-for-device shell /data/local/$(ASMTARGET)

%.o: %.s
	$(CC) -c $(CFLAGS) $< -o $@

clean:
	rm *.o $(TARGET)
	ocamlbuild -clean
