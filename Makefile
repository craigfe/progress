screenshot:
	asciinema rec -c "dune exec examples/main.exe -- Stack" --overwrite /tmp/screenshot.json
	asciicast2gif -h 4 /tmp/screenshot.json .meta/example.gif

.PHONY: docs
docs:
	dune build @doc
	cp -fr docs/{odoc.css,fonts} _build/default/_doc/_html

.PHONY: example
example:
	@dune exec examples/main.exe -- $(name)

.PHONY: examples
examples:
	@for example in $(shell dune exec -- examples/main.exe --list); do \
     echo "*** $$example"; \
     dune exec -- examples/main.exe $$example; \
   done
