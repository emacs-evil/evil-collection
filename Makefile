EASK ?= eask

compile:
	$(EASK) compile

install:
	$(EASK) install-deps --dev
	$(EASK) package
	$(EASK) install

lint:
	$(EASK) lint package

test: install
	$(EASK) test ert ./test/evil-collection-test.el

magit-test: install
	$(EASK) test ert ./test/evil-collection-magit-tests.el

.PHONY: compile lint test

# Local Variables:
# tab-width: 8
# End:
