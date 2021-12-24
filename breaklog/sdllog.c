#include <SWI-Prolog.h>
#include <SDL2/SDL.h>

SDL_Window* window = NULL;
SDL_Surface* surface = NULL;

static foreign_t
sdl_init()
{
    if(SDL_Init(SDL_INIT_VIDEO) < 0)
    {
        fprintf(stderr, "SDL_Init failed: %s", SDL_GetError());
        return FALSE;
    }
    return TRUE;
}

static foreign_t
sdl_create_window(term_t title, term_t width, term_t height)
{
    char* title_cs;
    PL_get_atom_chars(title, &title_cs);
    int w, h;
    PL_get_integer(width, &w);
    PL_get_integer(height, &h);
    window = SDL_CreateWindow(title_cs, SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, w, h, SDL_WINDOW_SHOWN);
    if(window == NULL)
    {
        fprintf(stderr, "SDL_CreateWindow failed: %s", SDL_GetError());
        return FALSE;
    }
    surface = SDL_GetWindowSurface(window);
    return TRUE;
}

static foreign_t
sdl_load_bmp(term_t path, term_t pointer)
{
    char* path_cs;
    PL_get_atom_chars(path, &path_cs);
    SDL_Surface* loaded_surface = SDL_LoadBMP(path_cs);
    PL_unify_pointer(pointer, loaded_surface);
    return TRUE;
}

static foreign_t
sdl_blit_surface(term_t pointer)
{
    SDL_Surface* surface_ptr;
    PL_get_pointer(pointer, (void**)&surface_ptr);
    SDL_BlitSurface(surface_ptr, NULL, surface, NULL);
    return TRUE;
}

static foreign_t
sdl_update_window()
{
    SDL_UpdateWindowSurface(window);
    return TRUE;
}

static foreign_t
sdl_delay(term_t ms)
{
    int ms_i;
    PL_get_integer(ms, &ms_i);
    SDL_Delay(ms_i);
    return TRUE;
}

static foreign_t
sdl_poll_event(term_t event)
{
    SDL_Event e;
    if(SDL_PollEvent(&e) != 0)
    {
        switch(e.type)
        {
            case SDL_QUIT:
                PL_unify_string_chars(event, "quit");
                break;
            case SDL_KEYDOWN:
                PL_unify_string_chars(event, "keydown");
                break;
            case SDL_KEYUP:
                PL_unify_string_chars(event, "keyup");
                break;
            default:
                PL_unify_string_chars(event, "unknown");
        }
        return TRUE;
    }
    PL_unify_string_chars(event, "none");
    return TRUE;
}

install_t
install()
{
    PL_register_foreign("sdl_init", 0, sdl_init, 0);
    PL_register_foreign("sdl_create_window", 3, sdl_create_window, 0);
    PL_register_foreign("sdl_load_bmp", 2, sdl_load_bmp, 0);
    PL_register_foreign("sdl_blit_surface", 1, sdl_blit_surface, 0);
    PL_register_foreign("sdl_update_window", 0, sdl_update_window, 0);
    PL_register_foreign("sdl_delay", 1, sdl_delay, 0);
    PL_register_foreign("sdl_poll_event", 1, sdl_poll_event, 0);
}