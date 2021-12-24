:- foreign(sdl_init).
:- foreign(sdl_create_window(+string)).
:- foreign(sdl_load_bmp(+string, -integer)).
:- foreign(sdl_blit(+integer)).
:- foreign(sdl_update_window_surface).
:- foreign(sdl_delay(+integer)).

:- initialization((
   sdl_init,
   sdl_create_window('Hello World'),
   sdl_load_bmp('hello.bmp', Image),
   sdl_blit(Image),
   sdl_update_window_surface,
   sdl_delay(2000)
)).