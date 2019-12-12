PACKAGEVERSION=1.9
PACKAGE=$(shell sh -c 'grep -F "Package: " DESCRIPTION | cut -f2 -d" "')

# FIXME: This Makefile only works with this BINDIR!
EAFTOOLS_DIR=~/work/perfassess/eaf/eaftools/
MOTOOLS_DIR=~/work/perfassess/
BINDIR=$(CURDIR)/..
RNODE=iridiacluster
RDIR=~/
INSTALL_FLAGS=
REALVERSION=$(PACKAGEVERSION)$(SVN_REV)
PACKAGEDIR=$(CURDIR)
# This could be replaced by devtools::build_win(version = "R-devel")
FTP_COMMANDS="user anonymous anonymous\nbinary\ncd incoming\nput $(PACKAGE)_$(PACKAGEVERSION).tar.gz\nquit\n"
WINBUILD_FTP_COMMANDS="user anonymous anonymous\nbinary\ncd R-devel\nput $(PACKAGE)_$(PACKAGEVERSION).tar.gz\nquit\n"

## Do we have git?
ifeq ($(shell sh -c 'which git 1> /dev/null 2>&1 && echo y'),y)
  ## Is this a working copy?
  ifeq ($(shell sh -c 'LC_ALL=C  git describe --first-parent --always | grep -q "[0-9a-z]\+$$" && echo y'),y)
    $(shell sh -c 'LC_ALL=C  git describe --first-parent --always | grep -o "[0-9a-z]\+$$" > git_version')
  endif
endif
## Set version information:
SVN_REV = $(shell sh -c 'cat git_version 2> /dev/null')
REVNUM = $(shell sh -c 'cat git_version 2> /dev/null')

.PHONY: build check clean install pdf rsync scripts submit version cran winbuild help gendoc pkgdown

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

gendoc: $(PACKAGEDIR)/man/$(PACKAGE)-package.Rd
$(PACKAGEDIR)/man/$(PACKAGE)-package.Rd: $(PACKAGEDIR)/R/*.R
	R --slave -e 'devtools::document()'

pkgdown: gendoc
	R --slave -e 'pkgdown::build_site(run_dont_run = TRUE)'

build: clean scripts gendoc
	cd $(BINDIR) && R CMD build $(PACKAGEDIR)

closeversion:
	git ci NEWS -m " * NEWS: Close version $(PACKAGEVERSION)"
	git push origin :refs/tags/v$(PACKAGEVERSION) # Remove any existing tag
	git tag -f -a v$(PACKAGEVERSION) -m "Version $(PACKAGEVERSION)"
	git push --tags

releasebuild: clean scripts gendoc
	cd $(BINDIR) &&	R CMD build $(PACKAGEDIR) && tar -atvf $(PACKAGE)_$(PACKAGEVERSION).tar.gz

cran : build pkgdown
	cd $(BINDIR) && _R_CHECK_FORCE_SUGGESTS_=false R CMD check --as-cran $(PACKAGE)_$(PACKAGEVERSION).tar.gz

check: build
ifdef TEST
	_R_CHECK_FORCE_SUGGESTS_=false NOT_CRAN=true R --slave -e 'devtools::test(filter="$(TEST)")'
else
	cd $(BINDIR) && (_R_CHECK_FORCE_SUGGESTS_=false NOT_CRAN=true R CMD check --run-donttest --run-dontrun --timings $(PACKAGE)_$(PACKAGEVERSION).tar.gz; cat $(PACKAGE).Rcheck/$(PACKAGE)-Ex.timings)
endif

clean:
	cd $(PACKAGEDIR) && ($(RM) ./$(PACKAGE)-Ex.R ./src/*.o ./src/*.so; \
		find . -name '*.orig' -o -name '.Rhistory' | xargs $(RM) )
	make -C $(PACKAGEDIR)/src/eaf clean
	make -C $(PACKAGEDIR)/src/mo-tools clean

pdf:
	$(RM) $(BINDIR)/$(PACKAGE).pdf
	cd $(BINDIR) && R CMD Rd2pdf --no-preview --batch --output=$(PACKAGE).pdf $(PACKAGEDIR)

scripts:
	@if [ -d $(EAFTOOLS_DIR) ]; then \
	cp -f $(EAFTOOLS_DIR)/eafplot/eafplot.pl $(EAFTOOLS_DIR)/eafplot/README \
		$(PACKAGEDIR)/inst/scripts/eafplot/ && \
	chmod a-w $(PACKAGEDIR)/inst/scripts/eafplot/eafplot.pl $(PACKAGEDIR)/inst/scripts/eafplot/README && \
	cp -f $(EAFTOOLS_DIR)/eafdiff/eafdiff.pl $(EAFTOOLS_DIR)/eafdiff/README \
		$(PACKAGEDIR)/inst/scripts/eafdiff/ && \
	chmod a-w $(PACKAGEDIR)/inst/scripts/eafdiff/eafdiff.pl $(PACKAGEDIR)/inst/scripts/eafdiff/README && \
	rm -f $(MOTOOLS_DIR)/mo-tools-*-src.tar.gz && \
	make -C $(MOTOOLS_DIR)/mo-tools dist && \
	cd $(PACKAGEDIR)/src/mo-tools \
	&& tar xvzf $(MOTOOLS_DIR)/mo-tools-*-src.tar.gz --strip-components=1 \
	&& make -C $(PACKAGEDIR)/src/mo-tools clean; \
	rm -f $(EAFTOOLS_DIR)/eaf-*-src.tar.gz && \
	make -C $(EAFTOOLS_DIR)/eaf dist && \
	cd $(PACKAGEDIR)/src/eaf \
	&& tar xvzf $(EAFTOOLS_DIR)/eaf-*-src.tar.gz --strip-components=1 \
	&& make -C $(PACKAGEDIR)/src/eaf clean; \
	else \
	echo "WARNING: $(EAFTOOLS_DIR) not found!"; \
	fi

version :
	@sed -i 's/Version:.*$$/Version: $(PACKAGEVERSION)/' $(PACKAGEDIR)/DESCRIPTION

rsync : version
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
	@echo "Read http://cran.r-project.org/web/packages/policies.html"
	cd $(BINDIR) && echo $(FTP_COMMANDS) | ftp -v -e -g -i -n cran.r-project.org
	@echo "Don't forget to send email to cran@r-project.org !"

macbuild: build
	R --slave -e 'rhub::check(platform="macos-elcapitan-release")'

winbuild: build
	@echo "Winbuild: http://win-builder.r-project.org/"
	cd $(BINDIR) && echo $(WINBUILD_FTP_COMMANDS) | ftp -v -p -e -g -i -n win-builder.r-project.org


