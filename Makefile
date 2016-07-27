IDRIS := idris
PKG   := anatomy

build: .PHONY
	$(IDRIS) --build ${PKG}.ipkg

clean: .PHONY
	$(IDRIS) --clean ${PKG}.ipkg

rebuild: clean build

.PHONY:
