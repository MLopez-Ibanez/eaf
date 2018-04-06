PACKAGEVERSION=1.9
PACKAGE=$(shell sh -c 'grep -F "Package: " DESCRIPTION | cut -f2 -d" "')

# FIXME: This Makefile only works with this BINDIR!
SCRIPTSDIR=~/work/perfassess/eaf/eaftools/
BINDIR=$(CURDIR)/..
RNODE=iridiacluster
RDIR=~/
INSTALL_FLAGS=
REALVERSION=$(PACKAGEVERSION)$(SVN_REV)
DATE=$(shell date +%F)
PACKAGEDIR=$(CURDIR)
# This could be replaced by devtools::build_win(version = "R-devel")
FTP_COMMANDS="user anonymous anonymous\nbinary\ncd incoming\nput $(PACKAGE)_$(PACKAGEVERSION).tar.gz\nquit\n"
WINBUILD_FTP_COMMANDS="user anonymous anonymous\nbinary\ncd R-devel\nput $(PACKAGE)_$(PACKAGEVERSION).tar.gz\nquit\n"


## Do we have git?
ifeq ($(shell sh -c 'which git 1> /dev/null 2>&1 && echo y'),y)
  ## Is this a working copy?
  ifeq ($(shell sh -c 'LC_ALL=C  git describe --first-parent --always | sed "s/v[0-9]\.[0-9]//" | grep -q ^[0-9] && echo y'),y)
    $(shell sh -c 'git describe --first-parent --always | sed "s/v[0-9]\.[0-9]//" > git_version')
  endif
endif
## Set version information:
SVN_REV = $(shell sh -c 'cat git_version 2> /dev/null')
REVNUM = $(shell sh -c 'cat git_version 2> /dev/null')

.PHONY: build check clean install pdf rsync scripts submit bumpdate version cran winbuild help

help :
	@echo 'This makefile has the following targets                  '
	@echo '   build     build $(PACKAGE) at $(BINDIR)		'
	@echo '   cran      check with --as-cran			'
	@echo '   check     check with --timings			'
	@echo '   pdf       build $(PACKAGE).pdf			'
	@echo '   winbuild  send to http\://win-builder.r-project.org/	'
	@echo '   submit    submit to CRAN (see DEVEL-README first!)    '


install: clean scripts
	cd $(BINDIR) && R CMD INSTALL $(INSTALL_FLAGS) $(PACKAGEDIR)

gendoc: 
	R --slave -e 'library(devtools);document()'

build: clean scripts bumpdate gendoc
	cd $(BINDIR) && R CMD build $(PACKAGEDIR)

closeversion:
	git ci NEWS -m " * NEWS: Close version $(PACKAGEVERSION)"
	git push origin :refs/tags/v$(PACKAGEVERSION) # Remove any existing tag
	git tag -f -a v$(PACKAGEVERSION) -m "Version $(PACKAGEVERSION)"
	git push --tags

releasebuild:
	cd $(BINDIR) &&	R CMD build $(PACKAGEDIR) && tar -atvf $(PACKAGE)_$(PACKAGEVERSION).tar.gz

cran : build
	cd $(BINDIR) && _R_CHECK_FORCE_SUGGESTS_=false R CMD check --as-cran $(PACKAGE)_$(PACKAGEVERSION).tar.gz

check: build
	cd $(BINDIR) && (_R_CHECK_FORCE_SUGGESTS_=false R CMD check --run-donttest --run-dontrun --timings $(PACKAGE)_$(PACKAGEVERSION).tar.gz; cat $(PACKAGE).Rcheck/$(PACKAGE)-Ex.timings)

clean:
	cd $(PACKAGEDIR) && ($(RM) ./$(PACKAGE)-Ex.R ./src/*.o ./src/*.so; \
		find . -name '*.orig' | xargs $(RM) )

pdf:
	$(RM) $(BINDIR)/$(PACKAGE).pdf
	cd $(BINDIR) && R CMD Rd2pdf --no-preview --batch --output=$(PACKAGE).pdf $(PACKAGEDIR)

scripts:
	@if [ -d $(SCRIPTSDIR) ]; then \
	cp -f ~/work/perfassess/mo-tools/*.[ch] $(PACKAGEDIR)/inst/scripts/mo-tools/ && \
	cp -f ~/work/perfassess/hyperv/trunk/hv.[ch] $(PACKAGEDIR)/src/ && \
	cp -f $(SCRIPTSDIR)/eafplot/eafplot.pl $(SCRIPTSDIR)/eafplot/README \
		$(PACKAGEDIR)/inst/scripts/eafplot/ && \
	chmod a-w $(PACKAGEDIR)/inst/scripts/eafplot/eafplot.pl $(PACKAGEDIR)/inst/scripts/eafplot/README && \
	cp -f $(SCRIPTSDIR)/eafdiff/eafdiff.pl $(SCRIPTSDIR)/eafdiff/README \
		$(PACKAGEDIR)/inst/scripts/eafdiff/ && \
	chmod a-w $(PACKAGEDIR)/inst/scripts/eafdiff/eafdiff.pl $(PACKAGEDIR)/inst/scripts/eafdiff/README && \
	rm -f $(SCRIPTSDIR)/eaf-*-src.tar.gz && \
	make -C $(SCRIPTSDIR)/eaf dist && \
	cd $(PACKAGEDIR)/inst/scripts/eaf \
	&& tar xvzf $(SCRIPTSDIR)/eaf-*-src.tar.gz --strip-components=1 \
	&& make -C $(PACKAGEDIR)/inst/scripts/eaf clean; \
	else \
	echo "WARNING: $(SCRIPTSDIR) not found!"; \
	fi

bumpdate: version
	@sed -i 's/Date: .*/Date: $(DATE)/' $(PACKAGEDIR)/DESCRIPTION
	@sed -i 's/Date: .*$$/Date: \\tab $(DATE) \\cr/' $(PACKAGEDIR)/man/eaf-package.Rd

version :
	@sed -i 's/Version:.*$$/Version: $(PACKAGEVERSION)/' $(PACKAGEDIR)/DESCRIPTION
	@sed -i 's/Version:.*$$/Version: \\tab $(PACKAGEVERSION) \\cr/' $(PACKAGEDIR)/man/$(PACKAGE)-package.Rd
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

winbuild:
	@echo "Winbuild: http://win-builder.r-project.org/"
	cd $(BINDIR) && echo $(WINBUILD_FTP_COMMANDS) | ftp -v -p -e -g -i -n win-builder.r-project.org

web: ./README.md web/pandoc-simple.css
	pandoc -s   --css web/pandoc-simple.css  ./README.md > web/README.html

