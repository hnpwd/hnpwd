# Essential Targets Relied Upon by CI/CD
# --------------------------------------

test-all: lint test gen tidy

test:
	sbcl --noinform --eval "(defvar *quit* t)" --load test.lisp --quit

lint:
	sbcl --noinform --load lint.lisp --quit

gen:
	sbcl --noinform --load gen.lisp --quit

tidy:
	tidy -q -e web/index.html

ci-deps:
	sudo apt-get install sbcl tidy


# Additional Essential Target Used by CD
# --------------------------------------

# NOTE: We can also run 'make ghpg' in our local development
# environment.  If GitHub Workflow ever breaks in future, say, due to
# deprecation of GitHub Workflow features or change of GitHub terms
# and conditions, then we can run 'make ghpg' manually future (due to
# bugs in our code, deprecation of features by GitHub or change of
# terms), then we can run 'make ghpg' manually in our local
# development environment.

# Publish website to <https://hnpwd.github.io>.  This is typically
# called from the continuous deployment (CD) workflow under the
# .github/ directory.
ghpg:
	sbcl --script gen.lisp
	cd web/ && git init
	cd web/ && git add .
	cd web/ && git config user.name 'Continuous Deployment'
	cd web/ && git config user.email 'cd@localhost'
	cd web/ && git commit -m 'Generate website'
	cd web/ && git branch -M main
	cd web/ && git branch -a
	cd web/ && git remote add origin git@github.com:hnpwd/hnpwd.github.io.git
	cd web/ && git push -f origin main


# Targets to Publish Mirrors
# --------------------------

# Publish website to Codeberg Pages.
cbpg:
	rm -rf /tmp/pages/
	cd /tmp/ && git clone git@github.com:hnpwd/hnpwd.github.io.git pages
	cd /tmp/pages && git remote add cbpg git@codeberg.org:hnpwd/pages.git
	cd /tmp/pages && git push -f cbpg main

# Publish source code to Codeberg.
cb:
	git remote remove cb || :
	git remote add cb git@codeberg.org:hnpwd/hnpwd.git
	git push cb main
	git push cb --tags


# Targets Useful for Local Development Activities
# -----------------------------------------------

loop:
	while true; do make gen; sleep 5; done

pr:
	(git show-ref pr && git branch -d pr) || :
	@echo; echo 'Enter remote URL <space> branch to fetch:'
	@read answer && git fetch $$answer:pr; echo
	git log -n 2 pr
	git diff pr^!


# Less Used Targets for GH Workflow Integration etc.
# --------------------------------------------------

sshkey:
	ssh-keygen -t ed25519 -f ghcd
	mv ghcd ghcd.key
