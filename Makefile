ATTIC_CACHE ?= lambdamechanic-main
ATTIC_ENDPOINT ?= https://attic.app.lambdamechanic.com/
ATTIC_CMD = nix run github:zhaofengli/attic\#default --
ATTIC_TOKEN ?= $(shell cat .nix-attic-token 2>/dev/null)
NIX_SYSTEM != nix eval --raw --impure --expr 'builtins.currentSystem'

.PHONY: pull push build checks test _attic-login _test-only _test-with-cache

pull: _attic-login
	$(ATTIC_CMD) use $(ATTIC_CACHE)
	nix build .#default .#checks.$(NIX_SYSTEM).tests

push: _attic-login
	@default_path=$$(nix path-info .#default); \
	check_path=$$(nix path-info .#checks.$(NIX_SYSTEM).tests); \
	$(ATTIC_CMD) push --jobs 16 $(ATTIC_CACHE) $$default_path $$check_path

build:
	nix build .#default

checks:
	nix build .#checks.$(NIX_SYSTEM).tests

_test-only:
	nix build .#checks.$(NIX_SYSTEM).tests

_test-with-cache:
	$(MAKE) pull
	$(MAKE) _test-only
	$(MAKE) push

test: _test-with-cache

_attic-login:
	@if [ -z "$(ATTIC_TOKEN)" ]; then \
		echo "ATTIC_TOKEN must be set" >&2; \
		exit 1; \
	fi
	$(ATTIC_CMD) login cli "$(ATTIC_ENDPOINT)" "$(ATTIC_TOKEN)"
