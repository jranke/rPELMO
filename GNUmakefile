PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
TGZ     := $(PKGNAME)_$(PKGVERS).tar.gz
WINBIN  := $(PKGNAME)_$(PKGVERS).zip
R_HOME  ?= $(shell R RHOME)
DATE    := $(shell date +%Y-%m-%d)

.PHONEY: usage check clean

pkgfiles = DESCRIPTION \
	.Rbuildignore \
	docs/* \
	docs/reference/* \
	inst/testdata/* \
	README.html \
	R/* \
	tests/testthat.R \
	tests/testthat/*

clean:
	@echo "Cleaning up..."
	rm -fR rPELMO.Rcheck
	@echo "DONE."

roxygen: 
	@echo "Roxygenizing package..."
	"$(R_HOME)/bin/Rscript" -e 'devtools::document()'
	@echo "DONE."

README.html: README.md
	"$(R_HOME)/bin/Rscript" -e "rmarkdown::render('README.md', output_format = 'html_document', output_options = list(mathjax = NULL))"

pd: roxygen
	@echo "Building static documentation..."
	"$(R_HOME)/bin/Rscript" -e 'pkgdown::build_site()'
	@echo "DONE."

$(TGZ): $(pkgfiles)
	sed -i -e "s/Date:.*/Date: $(DATE)/" DESCRIPTION
	@echo "Roxygenizing package..."
	"$(R_HOME)/bin/Rscript" -e 'devtools::document()'
	@echo "Building package..."
	git log --no-merges -M --date=iso > ChangeLog
	"$(R_HOME)/bin/R" CMD build . > build.log 2>&1
	@echo "DONE."

build: $(TGZ)

$(WINBIN): build
	@echo "Building windows binary package..."
	"$(R_HOME)/bin/R" CMD INSTALL $(TGZ) --build
	@echo "DONE."

winbin: $(WINBIN)

test: build
	@echo "Running testthat tests..."
	NOT_CRAN=true "$(R_HOME)/bin/Rscript" -e 'devtools::test()' 2>&1 | tee test.log
	@echo "DONE."

quickcheck: build
	@echo "Running check..."
	"$(R_HOME)/bin/R" CMD check $(TGZ)
	@echo "DONE."

check: build
	@echo "Running CRAN check..."
	"$(R_HOME)/bin/R" CMD check --as-cran $(TGZ)
	@echo "DONE."

install: build
	@echo "Installing package..."
	"$(R_HOME)/bin/R" CMD INSTALL --no-multiarch $(TGZ)
	@echo "DONE."

drat: build
	"$(R_HOME)/bin/Rscript" -e "drat::insertPackage('$(TGZ)', commit = TRUE)"

dratwin: winbin
	"$(R_HOME)/bin/Rscript" -e "drat::insertPackage('$(WINBIN)', '~/git/drat/', commit = TRUE)"

winbuilder: build
	date
	@echo "Uploading to R-release on win-builder"
	curl -T $(TGZ) ftp://anonymous@win-builder.r-project.org/R-release/
	@echo "Uploading to R-devel on win-builder"
	curl -T $(TGZ) ftp://anonymous@win-builder.r-project.org/R-devel/
