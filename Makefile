NAME = $(notdir $(CURDIR))

FIND = /usr/bin/find

# Python
SYSTEM_PYTHON = /usr/bin/python2
PYTHON_VERSION = 2.7.8
PIP = pip
PEP8_OPTIONS = --max-line-length=120

ONLINE = true

build_directory = build
distribution_directory = dist

.PHONY: all
all: build

.PHONY: test-dependencies
test-dependencies: virtualenv
	if $(ONLINE); then \
		. virtualenv/bin/activate && $(PIP) install --requirement python-test-requirements.txt || exit $$?; \
	fi

.PHONY: test
test: test-dependencies
	. virtualenv/bin/activate && \
		make METHOD=git python-pep8 && \
		nosetests */test_*.py

.PHONY: clean
clean: clean-build clean-dist clean-test

.PHONY: clean-build
clean-build: clean-build-third-party clean-build-local

.PHONY: clean-build-third-party
clean-build-third-party:
	-$(RM) -r $(build_directory)

.PHONY: clean-build-local
clean-build-local:
	-$(RM) -r $(NAME).egg-info
	-$(FIND) . -type d -name '__pycache__' -delete
	-$(FIND) . -type f -name '*.pyc' -delete

.PHONY: clean-test
clean-test:
	-$(RM) .coverage
	-$(RM) virtualenv

include make-includes/python.mk
include make-includes/variables.mk
