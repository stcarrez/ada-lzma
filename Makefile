NAME=lzmada
VERSION=1.1.5

DIST_DIR=ada-lzma-$(VERSION)
DIST_FILE=ada-lzma-$(VERSION).tar.gz

MAKE_ARGS += -XLZMA_BUILD=$(BUILD)

-include Makefile.conf

STATIC_MAKE_ARGS = $(MAKE_ARGS) -XLZMA_LIBRARY_TYPE=static
SHARED_MAKE_ARGS = $(MAKE_ARGS) -XLZMA_LIBRARY_TYPE=relocatable

include Makefile.defaults

# Build executables for all mains defined by the project.
build::
ifeq ($(HAVE_ALIRE),yes)
	$(BUILD_COMMAND) $(GPRFLAGS) $(MAKE_ARGS)
endif

build-samples:: build
	cd samples && $(BUILD_COMMAND) $(GPRFLAGS) $(MAKE_ARGS)

# Build and run the unit tests
test:	build-samples
	tar --exclude=test.tar -cf test.tar . && \
	bin/compress_easy test.tar test.tar.xz && \
	bin/decompress test.tar.xz test-res.tar && \
	cmp test.tar test-res.tar && echo "Compression and decompression is OK"
	@-rm -f test.tar test.tar.xz

install_samples:
	$(MKDIR) -p $(DESTDIR)$(samplesdir)/samples
	cp -rp $(srcdir)/samples/*.ad[sb] $(DESTDIR)$(samplesdir)/samples/
	cp -p $(srcdir)/samples.gpr $(DESTDIR)$(samplesdir)
	cp -p $(srcdir)/config.gpr $(DESTDIR)$(samplesdir)

$(eval $(call ada_library,lzmada,.))
$(eval $(call alire_publish,.,lz/lzmada,lzmada-$(VERSION).toml))
