GH=https://github.com/rd--/hosc
GL=https://gitlab.com/rd--/hosc.git

all:
	echo "hosc"

mk-cmd:
	echo "hosc - NIL"

clean:
	rm -Rf dist
	(cd contrib/tests ; make clean)

push-gl:
	git push $(GL)

update-rd:
	ssh rd@rohandrape.net "(cd sw/hosc;git pull $(GL))"

push-rd:
	make push-gl update-rd

push-gh:
	git push $(GH)

push-tags-gh:
	git push $(GH) --tag

pull-gh:
	git pull $(GH)

