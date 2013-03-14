# Uncomment the two first lines and use your values
# OS can be linux, windows, macosx but windows is not yet defined
# ANDROID_DK_PATH is the path where the shell can find the Android NDK and SDK

OS						= linux
ANDROID_DK_PATH	= /opt



JAVA					= $(shell which java)
JAVAC					= $(shell which javac)

JFLEX					= ../jflex-1.4.3/bin/jflex
CUP					= ../java-cup-11a.jar
LEXER_NAME			= Lexer
PARSER_NAME			= Parser
SYMBOL_NAME			= Symbols

EMULATOR_NAME		= ARM-Simulator
EMULATOR_VERSION	= Google Inc.:Google APIs:8
API_LEVEL			= 8

TOOLCHAIN			= arm-linux-androideabi-4.6
PREFIX				= arm-linux-androideabi-

LIBS					= 
CFLAGS				= -Wall 


# Auto definition
SDK_PATH				= $(ANDROID_DK_PATH)/android-sdk
NDK_PATH				= $(ANDROID_DK_PATH)/android-ndk
ARCHITECTURE		= arm

ifeq ($(OS),linux)
	HOST					= linux-x86
else ifeq ($(OS),macosx)
	HOST					= darwin-x86
else
	echo "Not yet implemented"
endif

