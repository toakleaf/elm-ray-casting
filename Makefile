public/index.html: public/elm.js

public/elm.js: $(wildcard src/*.elm)
	elm make src/Main.elm --output=public/elm.js

.PHONY=clean
clean:
	rm public/elm.js
