R=https://github.com/rd--/hosc

clean:
	rm -Rf dist
	(cd contrib/tests ; make clean)

push-gh:
	git push $(R)

pull-gh:
	git pull $(R)

update-rd:
	ssh rd@rohandrape.net "(cd sw/hosc;git pull $(R))"
