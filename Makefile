clean:
	rm -Rf dist

push-rd:
	darcs push -a rd@rohandrape.net:sw/hosc

pull-rd:
	darcs pull -a http://rohandrape.net/sw/hosc
