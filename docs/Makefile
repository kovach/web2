index.html: index.md
	pandoc --template=template.html --toc index.md -o index.html -c docs.css -V date="`date +\"%B %e, %Y\"`"

live.html: live.md
	pandoc -s --template=template.html --toc live.md -o live.html -c docs.css -V date="`date +\"%B %e, %Y\"`"
