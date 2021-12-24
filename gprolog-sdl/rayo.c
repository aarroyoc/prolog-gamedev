#include <gprolog.h>
#include <SDL2/SDL.h>

SDL_Window *window = NULL;
SDL_Surface* surface = NULL;

PlBool
sdl_init()
{
  if (SDL_Init(SDL_INIT_VIDEO) < 0)
    return PL_FALSE;
  return PL_TRUE;
}

PlBool
sdl_create_window(char* title)
{
    window = SDL_CreateWindow(
        title,
        SDL_WINDOWPOS_UNDEFINED,
        SDL_WINDOWPOS_UNDEFINED,
        640,
        480,
        SDL_WINDOW_SHOWN
    );
    if (window == NULL)
        return PL_FALSE;
    
    surface = SDL_GetWindowSurface(window);

    return PL_TRUE;
}

PlBool
sdl_load_bmp(char* path, PlLong* pointer)
{
    SDL_Surface* loaded_surface = SDL_LoadBMP(path);
    if (loaded_surface == NULL)
        return PL_FALSE;
    
    *pointer = (PlLong)loaded_surface;
    return PL_TRUE;
}

PlBool
sdl_blit(PlLong* pointer)
{
    SDL_BlitSurface((SDL_Surface*)pointer, NULL, surface, NULL);
    return PL_TRUE;
}

PlBool
sdl_update_window_surface()
{
    SDL_UpdateWindowSurface(window);
    return PL_TRUE;
}

PlBool
sdl_delay(PlLong ms)
{
    SDL_Delay(ms);
    return PL_TRUE;
}