BASEDIR=$(CURDIR)
CACHEDIR=$(BASEDIR)/_cache
OUTPUTDIR=$(BASEDIR)/_site

GITHUB_PAGES_BRANCH=master


help:
	@echo 'Usage:                                                          '
	@echo '   make build                (re)generate the web site          '
	@echo '   make html                 (re)generate the web site          '
	@echo '   make clean                remove the generated files         '
	@echo '   make github               upload the web site via gh-pages   '
	@echo '                                                                '

html:
	./site.hs rebuild

clean:
	[ ! -d $(CACHEDIR) ] || rm -rf $(CACHEDIR)
	[ ! -d $(OUTPUTDIR) ] || rm -rf $(OUTPUTDIR)

github: html
	ghp-import -m "Publish." -b $(GITHUB_PAGES_BRANCH) $(OUTPUTDIR)
	git push origin $(GITHUB_PAGES_BRANCH)

.PHONY: build html help clean github
