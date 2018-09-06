clean:
	rm -Rf dist

push-gh:
	git push https://github.com/rd--/hosc

pull-gh:
	git pull https://github.com/rd--/hosc

update-rd:
	ssh rd@rohandrape.net "(cd sw/hosc;make pull-gh)"
