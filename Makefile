all:
	echo "hosc"

install:
	cabal v1-install --allow-newer

clean:
	rm -Rf dist dist-newstyle
	(cd contrib/tests ; make clean)

mk-cmd:
	echo "hosc - NIL"

push-all:
	r.gitlab-push.sh hosc
	r.github-push.sh hosc

push-tags:
	r.gitlab-push.sh hosc --tags
	r.github-push.sh hosc --tags

indent:
	fourmolu -i Sound

doctest:
	doctest -Wno-x-partial -Wno-incomplete-uni-patterns Sound
