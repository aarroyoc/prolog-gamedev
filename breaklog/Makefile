swi:
	swipl-ld -o sdllog sdllog.c -shared -lSDL2
	swilgt loader.lgt

gnu:
	/home/aarroyoc/dev/logtalk3/scripts/embedding/gprolog/gprolog_embed_logtalk.sh -l /home/aarroyoc/dev/breaklog/loader.lgt -- --strip --no-top-level /home/aarroyoc/dev/breaklog/sdl.c /home/aarroyoc/dev/breaklog/init.pl -L -lSDL2