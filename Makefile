all: slides

slides:
	@ pandoc --self-contained -V revealjs-url:"https://raw.githubusercontent.com/hakimel/reveal.js/master" -s README.md -t revealjs -o README.html

slideshow: slides
	@ chromereload README.html

devel:
	commando -p cat -j -q | grep --line-buffered 'README.md$$' | uniqhash | conscript make slideshow
