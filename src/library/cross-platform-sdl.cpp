/*
 * Example Mickey Scheme C library.
 *
 * Copyright (C) 2012 Christian Stigen Larsen
 * http://csl.sublevel3.org
 *
 * Compile with
 *
 * c++ -shared -Iinclude mickey-uname.cpp
 *
 * To use it,
 *
 * (import (posix dlopen))
 * (define lib (dlopen "libuname.so" 'lazy))
 * (define uname (dlsym lib "proc_uname"))
 * (display (uname))
 *
 */

#include <SDL/SDL.h>
#include "mickey/mickey.h"

template<typename K, typename V>
struct key_value_t {
  const K key;
  const V value;
};

static integer_t intval(cons_t* p)
{
  return p->number.integer;
}

/*
 * Map of scheme symbols to SDL mode flags.
 */
static key_value_t<std::string, uint32_t> sdl_flags[] = {
  {"swsurface",  SDL_SWSURFACE},
  {"hwsurface",  SDL_HWSURFACE},
  {"asyncblit",  SDL_ASYNCBLIT},
  {"anyformat",  SDL_ANYFORMAT},
  {"hwpalette",  SDL_HWPALETTE},
  {"doublebuf",  SDL_DOUBLEBUF},
  {"fullscreen", SDL_FULLSCREEN},
  {"opengl",     SDL_OPENGL},
  {"openglblit", SDL_OPENGLBLIT},
  {"resizable",  SDL_RESIZABLE},
  {"noframe",    SDL_NOFRAME}
};

static size_t num_sdl_flags =
  sizeof(sdl_flags) / sizeof(key_value_t<std::string, uint32_t>);

// to avoid name mangling
extern "C" {

/*
 * (initialize) ==> nothing
 */
cons_t* initialize(cons_t*, environment_t*)
{
  if ( SDL_Init(SDL_INIT_VIDEO) != 0 )
    raise(runtime_exception(SDL_GetError()));

  return unspecified();
}

/*
 * (set-video-mode <width> <height> <bits per pixel>?) or
 * (set-video-mode <width> <height> <bits per pixel> <mode flags>+)
 *
 * where <symbols> are:
 *  swsurface
 *  hwsurface
 *  asyncblit
 *  anyformat
 *  hwpalette
 *  doublebuf
 *  fullscreen
 *  opengl
 *  openglblit
 *  resizable
 *  noframe
 *
 */
cons_t* set_video_mode(cons_t* p, environment_t*)
{
  assert_length_min(p, 2);
  assert_type(INTEGER, car(p));
  assert_type(INTEGER, cadr(p));

  // dimension
  int x = intval(car(p));
  int y = intval(cadr(p));

  // default values
  int bits = 32;
  uint32_t mode = 0;

  // bits per pixel
  if ( length(p) > 2 && integerp(caddr(p)) )
    bits = intval(caddr(p));

  // mode options
  if ( length(p) > 3 ) {
    cons_t *opts = symbolp(caddr(p))? cddr(p) :
                   symbolp(cadddr(p))? cdddr(p) : nil();;

    DPRINT(opts);

    for ( cons_t *s = opts; !nullp(s); s = cdr(s) ) {
      assert_type(SYMBOL, car(s));

      std::string sym = symbol_name(car(s));

      for ( size_t n=0; n < num_sdl_flags; ++n )
        if ( sym == sdl_flags[n].key ) {
          mode |= sdl_flags[n].value;
          goto NEXT_FLAG;
        }

      raise(runtime_exception("Unknown SDL video mode flag: " + sym));

  NEXT_FLAG:
      continue;
    }
  }

  SDL_Surface *screen = SDL_SetVideoMode(x, y, bits, mode);

  if ( screen == NULL )
    raise(runtime_exception(SDL_GetError()));

  return pointer(gc_alloc_pointer("sdl-surface",
                                  reinterpret_cast<void*>(screen)));
}

};
