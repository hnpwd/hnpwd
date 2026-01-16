all: test gen tidy

test:
	sbcl --noinform --eval "(defvar *quit* t)" --load test.lisp --quit

gen:
	sbcl --noinform --load gen.lisp --quit

tidy:
	tidy -q -e index.html

loop:
	while true; do make gen; sleep 5; done

pub: gen co gh cb

pr:
	(git show-ref pr && git branch -d pr) || :
	@echo; echo 'Enter remote URL <space> branch to fetch:'
	@read answer && git fetch $$answer:pr; echo
	git log -n 2 pr
	git diff pr^!

co:
	git add -p
	@echo 'Type Enter to commit, Ctrl + C to cancel.'; read
	git commit

gh:
	git remote remove gh || :
	git remote add gh git@github.com:hnpwd/hnpwd.github.io.git
	git push gh main
	git push gh --tags

cb:
	git remote remove cb || :
	git remote add cb git@codeberg.org:hnpwd/pages.git
	git push cb main
	git push cb --tags
