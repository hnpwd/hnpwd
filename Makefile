all: gen lint

gen:
	sbcl --noinform --load gen.lisp --quit

loop:
	while true; do make gen; sleep 5; done

pub: gen co push

pr:
	(git show-ref pr && git branch -d pr) || :
	@echo; echo 'Enter remote URL <space> branch to fetch:'
	@read answer && git fetch $$answer:pr; echo
	git log -n 2 pr

co:
	git add -p
	@echo 'Type Enter to commit, Ctrl + C to cancel.'; read
	git commit

push:
	git remote remove gh || :
	git remote remove cb || :
	git remote add gh git@github.com:hnpwd/hnpwd.github.io.git
	git remote add cb git@codeberg.org:hnpwd/pages.git
	git push gh main
	git push gh --tags
	git push cb main
	git push cb --tags

lint:
	tidy -q -e index.html
