GHPAGE=../../130-gh-pages/

full: site lectures

site:
	stack build
	stack exec -- homepage rebuild

lectures:
	cd lectures && make && cd ..
	cp css/syntax-rj.css _site/css/syntax.css
	cp -p -r lectures _site/

upload: site lectures
	cp -r _site/* $(GHPAGE)
	cd $(GHPAGE) && git add . && git commit -a -m "update page" && git push origin gh-pages


clean:
	rm -rf *.hi *.o .*.swp .*.swo website _site/ _cache/
	cd lectures && make clean

server:
	stack exec -- homepage watch
