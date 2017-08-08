.PHONY: send
send: live.html docs.css
	scp live.html live.md rules2.jpg docs.css \
      scott@cutfree.net:/www/static/

index.html: index.md
	pandoc --template=template.html --toc index.md -o ../index.html -c docs/docs.css -V date="`date +\"%B %e, %Y\"`"

live.html: live.md
	pandoc --template=template.html --toc live.md -o live.html -c docs.css -V date="`date +\"%B %e, %Y\"`"
