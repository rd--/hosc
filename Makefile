GL_GIT=git@gitlab.com:rd--/hosc.git
GL_HTTP=https://gitlab.com/rd--/hosc.git

GH_GIT=git@github.com:rd--/hosc.git
GH_HTTP=https://github.com/rd--/hosc.git

all:
	echo "hosc"

mk-cmd:
	echo "hosc - NIL"

clean:
	rm -Rf dist dist-newstyle
	(cd contrib/tests ; make clean)

push-gl:
	git push $(GL_GIT)

pull-gl:
	git pull $(GL_HTTP)

push-gh:
	git push $(GH_GIT)

pull-gh:
	git pull $(GH_HTTP)

push-tags:
	git push $(GL_GIT) --tags
	git push $(GH_GIT) --tags

update-rd:
	ssh rd@rohandrape.net "(cd sw/hosc;git pull $(GL_HTTP))"

push-all:
	make push-gl push-gh update-rd
