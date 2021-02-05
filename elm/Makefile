JS = elm.js

live:
	elm-live --port 8001 src/Main.elm -- --output=$(JS)

clean:
	rm -f $(JS)

js: clean
	elm make --output=$(JS) src/Main.elm
