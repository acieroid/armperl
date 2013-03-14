include config.mk

SYSROOT		= ${NDK_PATH}/platforms/android-$(API_LEVEL)/arch-$(ARCHITECTURE)
FULL_PATH	= ${NDK_PATH}/toolchains/$(TOOLCHAIN)/prebuilt/$(HOST)/bin/$(PREFIX)

CC		= $(FULL_PATH)gcc --sysroot=$(SYSROOT)
ADB		= $(SDK_PATH)/platform-tools/adb

ASM_SRC		= $(wildcard *.s)
ASM_OBJS	= $(ASM_SRC:.s=.o)
ASMTARGET	= a.out

TARGET		= main
OPTS		= -pp camlp4o
TAGS		= annot,debug
LIBS		= str
EXTENSION	= byte

CFLAGS		= -fno-short-enums -fstrict-aliasing -O2

all:
	rm -f *.o
	ocamlbuild $(OPTS) -tags $(TAGS) -libs $(LIBS) $(TARGET).$(EXTENSION)

lib:
	$(CC) -S $(CFLAGS) perl.c

obj: $(ASM_OBJS)
	$(CC) -o $(ASMTARGET) $^ $(CFLAGS)

try:
	$(ADB) push $(ASMTARGET) /data/local/
	$(ADB) shell /data/local/$(ASMTARGET)

%.o: %.s
	$(CC) -c $(CFLAGS) $< -o $@

clean:
	rm *.o $(TARGET)
	ocamlbuild -clean
