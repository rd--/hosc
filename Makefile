GH=https://github.com/rd--/hosc

GL_GIT=git@gitlab.com:rd--/hosc.git
GL_HTTP=https://gitlab.com/rd--/hosc.git

all:
	echo "hosc"

mk-cmd:
	echo "hosc - NIL"

clean:
	rm -Rf dist
	(cd contrib/tests ; make clean)

push-gl:
	git push $(GL_GIT)

pull-gl:
	git pull $(GL_HTTP)

update-rd:
	ssh rd@rohandrape.net "(cd sw/hosc;git pull $(GL_HTTP))"

push-rd:
	make push-gl update-rd

push-gh:
	git push $(GH)

push-tags-gh:
	git push $(GH) --tag

pull-gh:
	git pull $(GH)

