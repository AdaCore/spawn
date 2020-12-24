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

SPAWN_TESTS=.obj/spawn_test/spawn_test .obj/spawn_test/spawn_unexpected

all:
	gprbuild $(GPRBUILD_FLAGS) -P gnat/spawn.gpr
	gprbuild $(GPRBUILD_FLAGS) -P gnat/spawn_tests.gpr
check:
	export LD_LIBRARY_PATH=.libs; \
	for TEST in ${SPAWN_TESTS}; do \
	  echo $$TEST; $$TEST; \
	done

install:
	gprinstall $(GPRINSTALL_FLAGS) -p -P gnat/spawn.gpr
clean:
	gprclean -q -P gnat/spawn.gpr

