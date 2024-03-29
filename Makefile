PACKAGEVERSION=$(shell sh -c 'grep -F "Version: " DESCRIPTION | cut -f2 -d" "')
PACKAGE=$(shell sh -c 'grep -F "Package: " DESCRIPTION | cut -f2 -d" "')

# FIXME: This Makefile only works with this BINDIR!
EAFTOOLS_DIR=~/work/perfassess/eaf/eaftools/
MOTOOLS_DIR=~/work/perfassess/
BINDIR=$(CURDIR)/..
RNODE=iridiacluster
RDIR=~/
INSTALL_FLAGS="--with-keep.source"
REALVERSION=$(PACKAGEVERSION)$(SVN_REV)
PACKAGEDIR=$(CURDIR)
RHUB_COMMON_ARGS= path='$(BINDIR)/$(PACKAGE)_$(PACKAGEVERSION).tar.gz', env_vars = c('_R_CHECK_FORCE_SUGGESTS_'='false', R_DEFAULT_SAVE_VERSION='2', R_DEFAULT_SERIALIZE_VERSION='2')
Reval=R --slave -e

define Rsed
	R --slave --vanilla -e 'f <- "$(1)"; txt <- sub($(2), $(3), perl=TRUE, readLines(f)); writeLines(txt, f)'
endef

## Do we have git?
ifeq ($(shell sh -c 'which git 1> /dev/null 2>&1 && echo y'),y)
  ## Is this a working copy?
  ifeq ($(shell sh -c 'LC_ALL=C  git describe --first-parent --always | grep -q "[0-9a-z]\+$$"  && echo y'),y)
    $(shell sh -c 'LC_ALL=C  git describe --dirty --first-parent --always --exclude "*" > git_version')
  endif
endif
## Set version information:
SVN_REV = $(shell sh -c 'cat git_version 2> /dev/null')
REVNUM = $(shell sh -c 'cat git_version 2> /dev/null')

.PHONY: build check clean install pdf rsync scripts submit cran winbuild help gendoc pkgdown figures

help :
	@echo 'This makefile has the following targets                  '
	@echo '   build     build $(PACKAGE) at $(BINDIR)		'
	@echo '   cran      check with --as-cran			'
	@echo '   check     check with --timings			'
	@echo '   check TEST=x  run test called test-x.R                '
	@echo '   pdf       build $(PACKAGE).pdf			'
	@echo '   winbuild  send to http\://win-builder.r-project.org/	'
	@echo '   submit    submit to CRAN (see DEVEL-README first!)    '
	@echo '   clean     cleanup    '

install: build
	cd $(BINDIR) && R CMD INSTALL $(INSTALL_FLAGS) $(PACKAGE)_$(PACKAGEVERSION).tar.gz

setup:
	Rscript -e 'if(!require(devtools)) install.packages("devtools"); devtools::install_deps(upgrade="never")'

configure: configure.ac src/Makevars.in
	autoreconf configure.ac

gendoc: $(PACKAGEDIR)/man/$(PACKAGE)-package.Rd configure

NAMESPACE $(PACKAGEDIR)/man/$(PACKAGE)-package.Rd: $(PACKAGEDIR)/R/*.R
	$(Reval) 'pkgbuild::compile_dll();devtools::document()'

pkgdown: gendoc
	$(Reval) 'pkgdown::build_site(run_dont_run = TRUE)'

figures:
	Rscript tools/create-figures.R	

build:
	@$(MAKE) scripts
	@$(MAKE) gendoc
	cd $(BINDIR) && R CMD build $(PACKAGEDIR)

closeversion:
	git push origin :refs/tags/v$(PACKAGEVERSION) # Remove any existing tag
	git tag -f -a v$(PACKAGEVERSION) -m "Version $(PACKAGEVERSION)"
	git push --tags
	r --interactive -e 'usethis::use_version()'

releasebuild: clean
	@$(MAKE) scripts
	@$(MAKE) gendoc
	@$(MAKE) figures
	cd $(BINDIR) &&	R CMD build $(PACKAGEDIR) && tar -atvf $(PACKAGE)_$(PACKAGEVERSION).tar.gz

releasecheck: cran
	$(Reval) 'devtools::spell_check()'
	$(Reval) 'urlchecker::url_check()'
	$(MAKE) winbuild #$(MAKE) macbuild

cran : build
	cd $(BINDIR) && _R_CHECK_FORCE_SUGGESTS_=false _R_OPTIONS_STRINGS_AS_FACTORS_=false R CMD check --as-cran $(PACKAGE)_$(PACKAGEVERSION).tar.gz

check: build
ifdef TEST
	_R_CHECK_FORCE_SUGGESTS_=false NOT_CRAN=true $(Reval) 'devtools::test(filter="$(TEST)")'
else
	cd $(BINDIR) && (_R_CHECK_FORCE_SUGGESTS_=false NOT_CRAN=true R CMD check --run-donttest --run-dontrun --timings $(PACKAGE)_$(PACKAGEVERSION).tar.gz; cat $(PACKAGE).Rcheck/$(PACKAGE)-Ex.timings)
endif

clean:
	cd $(PACKAGEDIR) && (./cleanup; find . -name '*.orig' -o -name '.Rhistory' | xargs $(RM))
	make -C $(PACKAGEDIR)/src/eaf clean
	make -C $(PACKAGEDIR)/src/mo-tools clean

pdf:
	$(RM) $(BINDIR)/$(PACKAGE).pdf
	cd $(BINDIR) && R CMD Rd2pdf --no-preview --batch --output=$(PACKAGE).pdf $(PACKAGEDIR)

scripts:
	@if [ -d $(EAFTOOLS_DIR) ]; then \
	(cp -f $(EAFTOOLS_DIR)/eafplot/eafplot.pl $(EAFTOOLS_DIR)/eafplot/README \
		$(PACKAGEDIR)/inst/scripts/eafplot/ \
	&& chmod a-w $(PACKAGEDIR)/inst/scripts/eafplot/eafplot.pl $(PACKAGEDIR)/inst/scripts/eafplot/README \
	&& cp -f $(EAFTOOLS_DIR)/eafdiff/eafdiff.pl $(EAFTOOLS_DIR)/eafdiff/README \
		$(PACKAGEDIR)/inst/scripts/eafdiff/ \
	&& chmod a-w $(PACKAGEDIR)/inst/scripts/eafdiff/eafdiff.pl $(PACKAGEDIR)/inst/scripts/eafdiff/README \
	&& rm -f $(MOTOOLS_DIR)/mo-tools-*-src.tar.gz \
	&& make -C $(MOTOOLS_DIR)/mo-tools dist \
	&& cd $(PACKAGEDIR)/src/mo-tools \
	&& tar xvzf $(MOTOOLS_DIR)/mo-tools-*-src.tar.gz --strip-components=1 )\
	|| exit 1; \
	(rm -f $(EAFTOOLS_DIR)/eaf-*-src.tar.gz \
	&& make -C $(EAFTOOLS_DIR)/eaf dist \
	&& cd $(PACKAGEDIR)/src/eaf \
	&& tar xvzf $(EAFTOOLS_DIR)/eaf-*-src.tar.gz --strip-components=1 )\
	|| exit 1; \
	else \
	echo "WARNING: $(EAFTOOLS_DIR) not found!"; \
	fi

rsync :
ifndef RDIR
	@echo "ERROR: You must specify a remote dir (e.g., RDIR=~/)"
endif
ifdef RNODE
	rsync -rlp -CIzc -L --delete --copy-unsafe-links --exclude=.git --exclude=/examples/ --progress --relative  \
	.     \
	$(RNODE):$(RDIR)/eaf/
	ssh $(RNODE) "cd $(RDIR)/eaf && make install"
else
	@echo "ERROR: You must specify a remote node (e.g., RNODE=majorana)"
	@exit 1
endif

submit:
	@echo 'Edit cran-comments.md and run devtools::submit_cran() in R'


remotecran: releasebuild
	$(Reval) "rhub::check_for_cran($(RHUB_COMMON_ARGS), show_status = TRUE)"

# Stopped working
# macbuild: releasebuild
#	$(Reval) "rhub::check(platform='macos-highsierra-release-cran', $(RHUB_COMMON_ARGS))"

winbuild: releasebuild
	$(Reval) "devtools::check_win_release()"
	$(Reval) "devtools::check_win_devel()"
	$(Reval) "rhub::check_on_windows($(RHUB_COMMON_ARGS))"

covr: build
	$(Reval) "Sys.setenv(NOT_CRAN='true');covr::report(covr::package_coverage(type='all'))"

