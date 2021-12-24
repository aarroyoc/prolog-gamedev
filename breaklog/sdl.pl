:- module(sdl, [
    sdl_init/0,
    sdl_create_window/3,
    sdl_load_bmp/2,
    sdl_blit_surface/1,
    sdl_update_window/0,
    sdl_delay/1,
    sdl_poll_event/1
]).
:- use_foreign_library(sdllog).
