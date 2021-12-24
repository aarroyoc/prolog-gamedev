:- use_module(sdl).

:- object(app).

    :- public(run/0).
    run :-
        sdl:sdl_init,
        sdl:sdl_create_window('SDL+Prolog', 800, 600),
        sdl:sdl_load_bmp('hello.bmp', Image),
        loop(Image).

    loop(Image) :-
        sdl:sdl_poll_event(Event),
        sdl:sdl_blit_surface(Image),
        sdl:sdl_update_window,
        (Event = "quit" -> halt;loop(Image)).


:- end_object.