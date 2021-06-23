all:
	echo "hosc"

mk-cmd:
	echo "hosc - NIL"

clean:
	rm -Rf dist dist-newstyle
	(cd contrib/tests ; make clean)

push-all:
	r.gitlab-push.sh hosc
	r.github-push.sh hosc

push-tags:
	r.gitlab-push.sh hosc --tags
	r.github-push.sh hosc --tags
