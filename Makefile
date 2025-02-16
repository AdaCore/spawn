GPRBUILD_FLAGS = -p -j0
PREFIX                 ?= /usr
GPRDIR                 ?= $(PREFIX)/share/gpr
LIBDIR                 ?= $(PREFIX)/lib
INSTALL_PROJECT_DIR    ?= $(DESTDIR)$(GPRDIR)
INSTALL_INCLUDE_DIR    ?= $(DESTDIR)$(PREFIX)/include/spawn
INSTALL_LIBRARY_DIR    ?= $(DESTDIR)$(LIBDIR)
INSTALL_ALI_DIR        ?= ${INSTALL_LIBRARY_DIR}/spawn

GPRINSTALL_FLAGS = --prefix=$(PREFIX) --sources-subdir=$(INSTALL_INCLUDE_DIR)\
 --lib-subdir=$(INSTALL_ALI_DIR) --project-subdir=$(INSTALL_PROJECT_DIR)\
 --link-lib-subdir=$(INSTALL_LIBRARY_DIR)

SPAWN_TESTS=spawn_test spawn_unexpected wait_all spawn_bad_exe check_cmd check_die

ifneq ($(OS),Windows_NT)
   SPAWN_TESTS += spawn_kill spawn_stty
endif

all:
	gprbuild $(GPRBUILD_FLAGS) -P gnat/spawn.gpr
	gprbuild $(GPRBUILD_FLAGS) -aP gnat -P gnat/tests/spawn_tests.gpr -XSPAWN_LIBRARY_TYPE=static

check:
	export LD_LIBRARY_PATH=.libs/spawn/relocatable; \
	for TEST in ${SPAWN_TESTS}; do \
	  echo $$TEST; .obj/spawn_test/$$TEST; \
	done

install:
	gprinstall $(GPRINSTALL_FLAGS) -p -P gnat/spawn.gpr
clean:
	gprclean -q -P gnat/spawn.gpr

