PREFIX                 ?= /usr
GPRDIR                 ?= $(PREFIX)/share/gpr
LIBDIR                 ?= $(PREFIX)/lib
INSTALL_PROJECT_DIR    ?= $(DESTDIR)$(GPRDIR)
INSTALL_INCLUDE_DIR    ?= $(DESTDIR)$(PREFIX)/include/spawn
INSTALL_LIBRARY_DIR    ?= $(DESTDIR)$(LIBDIR)
INSTALL_ALI_DIR        ?= ${INSTALL_LIBRARY_DIR}/spawn

LIBRARY_TYPE           ?= static
BUILD_MODE             ?= prod
SPAWN_WARN_ERRORS      ?= true

TARGET       := $(shell gcc -dumpmachine)
ifeq ($(strip $(findstring linux, $(TARGET))),linux)
   OS=unix
else
ifeq ($(strip $(findstring mingw32, $(TARGET))),mingw32)
   OS=Windows_NT
else
ifeq ($(strip $(findstring cygwin, $(TARGET))),cygwin)
   OS=Windows_NT
else
ifeq ($(strip $(findstring darwin, $(TARGET))),darwin)
   OS=osx
else
   OS=unix
endif
endif
endif
endif

GPRBUILD_FLAGS = -p -j0 -XBUILD_MODE=$(BUILD_MODE) -XSPAWN_WARN_ERRORS=$(SPAWN_WARN_ERRORS) -XLIBRARY_TYPE=$(LIBRARY_TYPE) -XOS=$(OS)

GPRINSTALL_FLAGS = --prefix=$(PREFIX) --sources-subdir=$(INSTALL_INCLUDE_DIR)\
 --lib-subdir=$(INSTALL_ALI_DIR) --project-subdir=$(INSTALL_PROJECT_DIR)\
 --link-lib-subdir=$(INSTALL_LIBRARY_DIR)

SPAWN_TESTS=.obj/spawn_test/spawn_test .obj/spawn_test/spawn_unexpected

ifneq ($(OS),Windows_NT)
   SPAWN_TESTS += .obj/spawn_test/spawn_kill
endif

all:
	gprbuild $(GPRBUILD_FLAGS) -P gnat/spawn.gpr
	
spawn_glib:
	gprbuild $(GPRBUILD_FLAGS) -P gnat/spawn_glib.gpr

test:
	gprbuild $(GPRBUILD_FLAGS) -P gnat/spawn_tests.gpr

check:
	export LD_LIBRARY_PATH=.libs/spawn/relocatable; \
	for TEST in ${SPAWN_TESTS}; do \
	  echo $$TEST; $$TEST; \
	done

test_spawn_glib:
	gprbuild $(GPRBUILD_FLAGS) -P gnat/spawn_glib_tests.gpr

install:
	gprinstall $(GPRINSTALL_FLAGS) -p -P gnat/spawn.gpr

install_spawn_glib:
	gprinstall $(GPRINSTALL_FLAGS) -p -P gnat/spawn_glib.gpr

clean:
	gprclean -q -P gnat/spawn.gpr

clean_spawn_glib:
	gprclean -q -P gnat/spawn_glib.gpr
