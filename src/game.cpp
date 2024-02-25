#include <assert.h>
#include <stdint.h>
#include <stdlib.h>

#include "gfx.h"

#include "../data/font.c"
#include "../data/gfx.c"

#include "celeste.h"

#include "vex.h"

#define LOGLOAD(w) printf("loading %s...", w)
#define LOGDONE() printf("done\n")

#define PICO8_W 128
#define PICO8_H 128
vex::brain Brain;
vex::brain::lcd brain_screen = Brain.Screen;
static constexpr int scale = 2;

static constexpr Color base_palette[16] = {
    {0x00, 0x00, 0x00}, {0x1d, 0x2b, 0x53}, {0x7e, 0x25, 0x53},
    {0x00, 0x87, 0x51}, {0xab, 0x52, 0x36}, {0x5f, 0x57, 0x4f},
    {0xc2, 0xc3, 0xc7}, {0xff, 0xf1, 0xe8}, {0xff, 0x00, 0x4d},
    {0xff, 0xa3, 0x00}, {0xff, 0xec, 0x27}, {0x00, 0xe4, 0x36},
    {0x29, 0xad, 0xff}, {0x83, 0x76, 0x9c}, {0xff, 0x77, 0xa8},
    {0xff, 0xcc, 0xaa}};
static Color palette[16];

static inline uint32_t getcolor(char idx) {
  Color c = palette[idx % 16];
  return toUint32(toRGBA(c));
}

static void ResetPalette(void) {
  memcpy(palette, base_palette, sizeof palette);
}

static uint32_t getpixel(Surface *surface, int x, int y) {
  return surface->getpixel(x, y);
}

Surface gfx;
Surface font;

static void loadbmpscale(const char *filename, Surface *s) {
  uint32_t width;
  uint32_t height;
  const uint32_t *data;
  if (strcmp(filename, "font.bmp") == 0) {
    width = FONT_WIDTH;
    height = FONT_HEIGHT;
    data = font_image;
  } else if (strcmp(filename, "gfx.bmp") == 0) {
    width = GFX_WIDTH;
    height = GFX_HEIGHT;
    data = gfx_image;
  } else {
    printf("Unknown File %s\n", filename);
    return;
  }
  s->width = width * scale;
  s->height = height * scale;
  s->data = (uint32_t *)calloc(s->width * s->height, sizeof(uint32_t));
  for (int x = 0; x < width; x++) {
    for (int y = 0; y < height; y++) {
      uint32_t pixraw = data[y * width + x];
      uint8_t r = (pixraw >> 24) & 0xff;
      uint8_t g = (pixraw >> 16) & 0xff;
      uint8_t b = (pixraw >> 8) & 0xff;
      Color pc = {r, g, b};
      uint32_t pix = toUint32(toRGBA(pc));

      for (int sx = 0; sx < scale; sx++) {
        for (int sy = 0; sy < scale; sy++) {
          s->data[(y * scale + sy) * s->width + (x * scale + sx)] = pix;
        }
      }
    }
  }
}

static void LoadData(void) {
  LOGLOAD("gfx.bmp");
  loadbmpscale("gfx.bmp", &gfx);
  LOGDONE();

  LOGLOAD("font.bmp");
  loadbmpscale("font.bmp", &font);
  LOGDONE();
}

#include "tilemap.h"

static uint16_t buttons_state = 0;

static void p8_rectfill(int x0, int y0, int x1, int y1, int col);
static void p8_print(const char *str, int x, int y, int col);

// on-screen display (for info, such as loading a state, toggling screenshake,
// toggling fullscreen, etc)
static char osd_text[200] = "";
static int osd_timer = 0;
static void OSDset(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vsnprintf(osd_text, sizeof osd_text, fmt, ap);
  osd_text[sizeof osd_text - 1] =
      '\0'; // make sure to add NUL terminator in case of truncation
  osd_timer = 30;
  va_end(ap);
}
static void OSDdraw(void) {
  if (osd_timer > 0) {
    --osd_timer;
    const int x = 4;
    const int y =
        120 + (osd_timer < 10 ? 10 - osd_timer
                              : 0); // disappear by going below the screen
    p8_rectfill(x - 2, y - 2, x + 4 * strlen(osd_text), y + 6, 6); // outline
    p8_rectfill(x - 1, y - 1, x + 4 * strlen(osd_text) - 1, y + 5, 0);
    p8_print(osd_text, x, y, 7);
  }
}

static bool enable_screenshake = true;
static bool paused = false;
static bool running = true;
static void *initial_game_state = NULL;
static void *game_state = NULL;
static void mainLoop(void);

int game_main() {
  printf("game main\n");
  ResetPalette();
  printf("game state size %gkb\n", Celeste_P8_get_state_size() / 1024.);

  printf("now loading...\n");

  const unsigned char loading_bmp[] = {
      0x42, 0x4d, 0xca, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x82, 0x00,
      0x00, 0x00, 0x6c, 0x00, 0x00, 0x00, 0x24, 0x00, 0x00, 0x00, 0x09, 0x00,
      0x00, 0x00, 0x01, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x48, 0x00,
      0x00, 0x00, 0x23, 0x2e, 0x00, 0x00, 0x23, 0x2e, 0x00, 0x00, 0x02, 0x00,
      0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x42, 0x47, 0x52, 0x73, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0x00,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff, 0xff, 0xff, 0x00, 0x00, 0x00,
      0x00, 0x00, 0xe0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x00,
      0x00, 0x00, 0x66, 0x3e, 0xf1, 0x24, 0xf0, 0x00, 0x00, 0x00, 0x49, 0x44,
      0x92, 0x24, 0x90, 0x00, 0x00, 0x00, 0x49, 0x3c, 0x92, 0x24, 0x90, 0x00,
      0x00, 0x00, 0x49, 0x04, 0x92, 0x24, 0x90, 0x00, 0x00, 0x00, 0x46, 0x38,
      0xf0, 0x3c, 0xf0, 0x00, 0x00, 0x00, 0x40, 0x00, 0x12, 0x00, 0x00, 0x00,
      0x00, 0x00, 0xc0, 0x00, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00};

  LoadData();

  int pico8emu(CELESTE_P8_CALLBACK_TYPE call, ...);
  Celeste_P8_set_call_func(pico8emu);

  // for reset
  initial_game_state = malloc(Celeste_P8_get_state_size());
  if (initial_game_state)
    Celeste_P8_save_state(initial_game_state);

  Celeste_P8_set_rndseed(vex::timer::system());

  Celeste_P8_init();

  printf("ready\n");

  while (running)
    mainLoop();

  if (game_state)
    free(game_state);
  if (initial_game_state)
    free(initial_game_state);

  return 0;
}

Surface screen = {
    .width = PICO8_W * scale,
    .height = PICO8_H * scale,
    .data = (uint32_t *)calloc((PICO8_W * scale) * (PICO8_H * scale),
                               sizeof(uint32_t)),
};
static void flip_screen(Surface &s);

vex::controller con;
const vex::controller::button &reset_button = con.ButtonR1;
const vex::controller::axis &x_axis = con.Axis4;
const vex::controller::axis &y_axis = con.Axis3;
const vex::controller::button &jump_button = con.ButtonA;
const vex::controller::button &dash_button = con.ButtonB;

const vex::controller::button &save_button = con.ButtonL1;
const vex::controller::button &load_button = con.ButtonL2;

bool save_was_pressing = false;
bool load_was_pressing = false;

static void mainLoop(void) {

  static int reset_input_timer = 0;
  // hold F9 (select+start+y) to reset
  if (initial_game_state != NULL && reset_button.pressing()) {
    printf("resetting\n");
    fflush(stdout);

    reset_input_timer++;
    if (reset_input_timer >= 30) {
      reset_input_timer = 0;
      // reset
      OSDset("reset");
      paused = 0;
      Celeste_P8_load_state(initial_game_state);
      Celeste_P8_set_rndseed(vex::timer::system());
      Celeste_P8_init();
    }
  } else {

    reset_input_timer = 0;
  }

  // Save and load
  if (save_button.pressing() && !save_was_pressing) {
    game_state = game_state ? game_state : malloc(Celeste_P8_get_state_size());
    if (game_state) {
      OSDset("save state");
      Celeste_P8_save_state(game_state);
      Brain.SDcard.savefile("celeste_save.bin", (uint8_t *)game_state,
                            Celeste_P8_get_state_size());
    }
  }
  save_was_pressing = save_button.pressing();

  if (load_button.pressing() && !load_was_pressing) {
    if (Brain.SDcard.exists("celeste_save.bin")) {
      game_state =
          game_state ? game_state : malloc(Celeste_P8_get_state_size());
      Brain.SDcard.loadfile("celeste_save.bin", (uint8_t *)game_state,
                            Celeste_P8_get_state_size());
    }

    if (game_state) {
      OSDset("load state");
      if (paused) {
        paused = 0;
      }

      Celeste_P8_load_state(game_state);
    }
  }
  load_was_pressing = load_button.pressing();

  buttons_state = 0;

  {
    bool jump = jump_button.pressing();
    bool dash = dash_button.pressing();
    bool left = con.ButtonLeft.pressing();
    bool right = con.ButtonRight.pressing();
    bool up = con.ButtonUp.pressing();
    bool down = con.ButtonDown.pressing();

    bool stick_up = false;
    bool stick_down = false;
    bool stick_left = false;
    bool stick_right = false;

    int32_t x = x_axis.position();
    int32_t y = y_axis.position();
    int32_t deadband = 10;

    if (x < -deadband) {
      stick_left = true;
    }
    if (x > deadband) {
      stick_right = true;
    }
    if (y < -deadband) {
      stick_down = true;
    }
    if (y > deadband) {
      stick_up = true;
    }

    if (left || stick_left)
      buttons_state |= (1 << 0);
    if (right || stick_right)
      buttons_state |= (1 << 1);
    if (up || stick_up)
      buttons_state |= (1 << 2);
    if (down || stick_down)
      buttons_state |= (1 << 3);
    if (jump)
      buttons_state |= (1 << 4);
    if (dash)
      buttons_state |= (1 << 5);
  }
  if (paused) {
    const int x0 = PICO8_W / 2 - 3 * 4, y0 = 8;

    p8_rectfill(x0 - 1, y0 - 1, 6 * 4 + x0 + 1, 6 + y0 + 1, 6);
    p8_rectfill(x0, y0, 6 * 4 + x0, 6 + y0, 0);
  } else {
    Celeste_P8_update();
    Celeste_P8_draw();
  }
  OSDdraw();

  flip_screen(screen);

  // Delaying
  static unsigned frame_start = 0;
  unsigned frame_end = vex::timer::system();
  unsigned frame_time = frame_end - frame_start;
  constexpr unsigned target_millis = 33;
  // frame timing for 30fps is 33.333... ms
  // printf("frame time %d\n", frame_time);

  if (frame_time < target_millis) {
    vexDelay(target_millis - frame_time);
  }
  frame_start = vex::timer::system();
}

static int gettileflag(int, int);
static void p8_line(int, int, int, int, unsigned char);

static int FillRect(Surface *dst, const Rect rect, uint32_t color);

static inline void Xblit(Surface *src, Rect *srcrect, Surface *dst,
                         Rect *dstrect, int color, int flipx, int flipy) {
  assert(src && dst);

  Rect fulldst;
  /* If the destination rectangle is NULL, use the entire dest surface */
  if (!dstrect) {
    fulldst = {.x = 0, .y = 0, .width = dst->width, .height = dst->height};
    dstrect = (&fulldst);
  }
  int srcx, srcy, w, h;

  /* clip the source rectangle to the source surface */
  if (srcrect) {
    int maxw, maxh;

    srcx = srcrect->x;
    w = srcrect->width;
    if (srcx < 0) {
      w += srcx;
      dstrect->x -= srcx;
      srcx = 0;
    }
    maxw = src->width - srcx;
    if (maxw < w)
      w = maxw;

    srcy = srcrect->y;
    h = srcrect->height;
    if (srcy < 0) {
      h += srcy;
      dstrect->y -= srcy;
      srcy = 0;
    }
    maxh = src->height - srcy;
    if (maxh < h)
      h = maxh;

  } else {
    srcx = srcy = 0;
    w = src->width;
    h = src->height;
  }

  /* clip the destination rectangle against the clip rectangle */
  {
    Rect clip = dst->clip_rect();
    int dx, dy;

    dx = clip.x - dstrect->x;
    if (dx > 0) {
      w -= dx;
      dstrect->x += dx;
      srcx += dx;
    }
    dx = dstrect->x + w - clip.x - clip.width;
    if (dx > 0)
      w -= dx;

    dy = clip.y - dstrect->y;
    if (dy > 0) {
      h -= dy;
      dstrect->y += dy;
      srcy += dy;
    }
    dy = dstrect->y + h - clip.y - clip.height;
    if (dy > 0)
      h -= dy;
  }

  if (w && h) {
    uint32_t *srcpix = src->data;
    int srcpitch = src->width;
    uint32_t *dstpix = dst->data;
#define _blitter(dp, xflip)                                                    \
  do                                                                           \
    for (int y = 0; y < h; y++)                                                \
      for (int x = 0; x < w; x++) {                                            \
        uint32_t p =                                                           \
            srcpix[!xflip ? srcx + x + (srcy + y) * srcpitch                   \
                          : srcx + (w - x - 1) + (srcy + y) * srcpitch];       \
        if (p)                                                                 \
          dstpix[dstrect->x + x + (dstrect->y + y) * dst->width] = dp;         \
      }                                                                        \
  while (0)
    if (color && flipx)
      _blitter(getcolor(color), 1); // wrong
    else if (!color && flipx)
      _blitter(p, 1);
    else if (color && !flipx)
      _blitter(getcolor(color), 0); // wrong
    else if (!color && !flipx)
      _blitter(p, 0);
#undef _blitter
  }
}

int pico8emu(CELESTE_P8_CALLBACK_TYPE call, ...) {
  static int camera_x = 0, camera_y = 0;
  if (!enable_screenshake) {
    camera_x = camera_y = 0;
  }

  va_list args;
  int ret = 0;
  va_start(args, call);

#define INT_ARG() va_arg(args, int)
#define BOOL_ARG() (Celeste_P8_bool_t) va_arg(args, int)
#define RET_INT(_i)                                                            \
  do {                                                                         \
    ret = (_i);                                                                \
    goto end;                                                                  \
  } while (0)
#define RET_BOOL(_b) RET_INT(!!(_b))

  switch (call) {
  case CELESTE_P8_MUSIC: { // music(idx,fade,mask)
    // no music, sry
  } break;
  case CELESTE_P8_SPR: { // spr(sprite,x,y,cols,rows,flipx,flipy)
    int sprite = INT_ARG();
    int x = INT_ARG();
    int y = INT_ARG();
    int cols = INT_ARG();
    int rows = INT_ARG();
    int flipx = BOOL_ARG();
    int flipy = BOOL_ARG();

    (void)cols;
    (void)rows;

    assert(rows == 1 && cols == 1);

    if (sprite >= 0) {
      Rect srcrc = {8 * (sprite % 16), 8 * (sprite / 16)};
      srcrc.x *= scale;
      srcrc.y *= scale;
      srcrc.width = srcrc.height = scale * 8;
      Rect dstrc = {(x - camera_x) * scale, (y - camera_y) * scale, scale,
                    scale};
      Xblit(&gfx, &srcrc, &screen, &dstrc, 0, flipx, flipy);
    }
  } break;
  case CELESTE_P8_BTN: { // btn(b)
    int b = INT_ARG();
    assert(b >= 0 && b <= 5);
    RET_BOOL(buttons_state & (1 << b));
  } break;
  case CELESTE_P8_SFX: { // sfx(id)
    // no sound sorry
  } break;
  case CELESTE_P8_PAL: { // pal(a,b)
    int a = INT_ARG();
    int b = INT_ARG();
    if (a >= 0 && a < 16 && b >= 0 && b < 16) {
      // swap palette colors
      palette[a] = base_palette[b];
    }
  } break;
  case CELESTE_P8_PAL_RESET: { // pal()
    ResetPalette();
  } break;
  case CELESTE_P8_CIRCFILL: { // circfill(x,y,r,col)
    int cx = INT_ARG() - camera_x;
    int cy = INT_ARG() - camera_y;
    int r = INT_ARG();
    int col = INT_ARG();

    int realcolor = getcolor(col);

    if (r <= 1) {
      FillRect(&screen, Rect{scale * (cx - 1), scale * cy, scale * 3, scale},
               realcolor);
      FillRect(&screen, Rect{scale * cx, scale * (cy - 1), scale, scale * 3},
               realcolor);
    } else if (r <= 2) {
      FillRect(&screen,
               Rect{scale * (cx - 2), scale * (cy - 1), scale * 5, scale * 3},
               realcolor);
      FillRect(&screen,
               Rect{scale * (cx - 1), scale * (cy - 2), scale * 3, scale * 5},
               realcolor);
    } else if (r <= 3) {
      FillRect(&screen,
               Rect{scale * (cx - 3), scale * (cy - 1), scale * 7, scale * 3},
               realcolor);
      FillRect(&screen,
               Rect{scale * (cx - 1), scale * (cy - 3), scale * 3, scale * 7},
               realcolor);
      FillRect(&screen,
               Rect{scale * (cx - 2), scale * (cy - 2), scale * 5, scale * 5},
               realcolor);
    } else {         // i dont think the game uses this
      int f = 1 - r; // used to track the progress of the drawn circle (since
                     // its semi-recursive)
      int ddFx = 1;  // step x
      int ddFy = -2 * r; // step y
      int x = 0;
      int y = r;

      // this algorithm doesn't account for the diameters
      // so we have to set them manually
      p8_line(cx, cy - y, cx, cy + r, col);
      p8_line(cx + r, cy, cx - r, cy, col);

      while (x < y) {
        if (f >= 0) {
          y--;
          ddFy += 2;
          f += ddFy;
        }
        x++;
        ddFx += 2;
        f += ddFx;

        // build our current arc
        p8_line(cx + x, cy + y, cx - x, cy + y, col);
        p8_line(cx + x, cy - y, cx - x, cy - y, col);
        p8_line(cx + y, cy + x, cx - y, cy + x, col);
        p8_line(cx + y, cy - x, cx - y, cy - x, col);
      }
    }
  } break;
  case CELESTE_P8_PRINT: { // print(str,x,y,col)
    const char *str = va_arg(args, const char *);
    int x = INT_ARG() - camera_x;
    int y = INT_ARG() - camera_y;
    int col = INT_ARG() % 16;
    printf("printing %s\n", str);
    p8_print(str, x, y, col);
  } break;
  case CELESTE_P8_RECTFILL: { // rectfill(x0,y0,x1,y1,col)

    int x0 = INT_ARG() - camera_x;
    int y0 = INT_ARG() - camera_y;
    int x1 = INT_ARG() - camera_x;
    int y1 = INT_ARG() - camera_y;
    int col = INT_ARG();

    p8_rectfill(x0, y0, x1, y1, col);
  } break;
  case CELESTE_P8_LINE: { // line(x0,y0,x1,y1,col)
    int x0 = INT_ARG() - camera_x;
    int y0 = INT_ARG() - camera_y;
    int x1 = INT_ARG() - camera_x;
    int y1 = INT_ARG() - camera_y;
    int col = INT_ARG();

    p8_line(x0, y0, x1, y1, col);
  } break;
  case CELESTE_P8_MGET: { // mget(tx,ty)
    int tx = INT_ARG();
    int ty = INT_ARG();

    RET_INT(tilemap_data[tx + ty * 128]);
  } break;
  case CELESTE_P8_CAMERA: { // camera(x,y)
    if (enable_screenshake) {
      camera_x = INT_ARG();
      camera_y = INT_ARG();
    }
  } break;
  case CELESTE_P8_FGET: { // fget(tile,flag)
    int tile = INT_ARG();
    int flag = INT_ARG();

    RET_INT(gettileflag(tile, flag));
  } break;
  case CELESTE_P8_MAP: { // map(mx,my,tx,ty,mw,mh,mask)
    int mx = INT_ARG(), my = INT_ARG();
    int tx = INT_ARG(), ty = INT_ARG();
    int mw = INT_ARG(), mh = INT_ARG();
    int mask = INT_ARG();

    for (int x = 0; x < mw; x++) {
      for (int y = 0; y < mh; y++) {
        int tile = tilemap_data[x + mx + (y + my) * 128];
        // hack
        if (mask == 0 || (mask == 4 && tile_flags[tile] == 4) ||
            gettileflag(tile, mask != 4 ? mask - 1 : mask)) {
          Rect srcrc = {8 * (tile % 16), 8 * (tile / 16)};
          srcrc.x *= scale;
          srcrc.y *= scale;
          srcrc.width = srcrc.height = scale * 8;
          Rect dstrc = {(tx + x * 8 - camera_x) * scale,
                        (ty + y * 8 - camera_y) * scale, scale * 8, scale * 8};

          if (0) {
            srcrc.x = srcrc.y = 0;
            srcrc.width = srcrc.height = 8;
            dstrc.x = x * 8, dstrc.y = y * 8;
            dstrc.width = dstrc.height = 8;
          }

          Xblit(&gfx, &srcrc, &screen, &dstrc, 0, 0, 0);
        }
      }
    }
  } break;
  }

end:
  va_end(args);
  return ret;
}

void flip_screen(Surface &surf) {
  brain_screen.drawImageFromBuffer(surf.data, 0, -10, surf.width, surf.height);
  brain_screen.render();
}
static int gettileflag(int tile, int flag) {
  return tile < sizeof(tile_flags) / sizeof(*tile_flags) &&
         (tile_flags[tile] & (1 << flag)) != 0;
}

// coordinates should NOT be scaled before calling this
static void p8_line(int x0, int y0, int x1, int y1, unsigned char color) {
#define CLAMP(v, min, max) v = v < min ? min : v >= max ? max - 1 : v;
  CLAMP(x0, 0, screen.width);
  CLAMP(y0, 0, screen.height);
  CLAMP(x1, 0, screen.width);
  CLAMP(y1, 0, screen.height);

  uint32_t realcolor = getcolor(color);

#undef CLAMP
#define PLOT(xp, yp)                                                           \
  do {                                                                         \
    Rect r = {                                                                 \
        .x = xp * scale, .y = yp * scale, .width = scale, .height = scale};    \
    FillRect(&screen, r, realcolor);                                           \
  } while (0)
  int sx, sy, dx, dy, err, e2;
  dx = abs(x1 - x0);
  dy = abs(y1 - y0);
  if (!dx && !dy)
    return;

  if (x0 < x1)
    sx = 1;
  else
    sx = -1;
  if (y0 < y1)
    sy = 1;
  else
    sy = -1;
  err = dx - dy;
  if (!dy && !dx)
    return;
  else if (!dx) { // vertical line
    for (int y = y0; y != y1; y += sy)
      PLOT(x0, y);
  } else if (!dy) { // horizontal line
    for (int x = x0; x != x1; x += sx)
      PLOT(x, y0);
  }
  while (x0 != x1 || y0 != y1) {
    PLOT(x0, y0);
    e2 = 2 * err;
    if (e2 > -dy) {
      err -= dy;
      x0 += sx;
    }
    if (e2 < dx) {
      err += dx;
      y0 += sy;
    }
  }
#undef PLOT
}

int min(int a, int b) {
  if (a < b) {
    return a;
  }
  return b;
}
int max(int a, int b) {
  if (a > b) {
    return a;
  }
  return b;
}

static int FillRect(Surface *dst, const Rect rect, uint32_t color) {
  for (int x = max(0, rect.x); x < min(dst->width, rect.x + rect.width); x++) {
    for (int y = max(0, rect.y); y < min(dst->height, rect.y + rect.height);
         y++) {
      dst->setpixel(x, y, color);
    }
  }

  return 0;
}

static void p8_rectfill(int x0, int y0, int x1, int y1, int col) {
  int w = (x1 - x0 + 1) * scale;
  int h = (y1 - y0 + 1) * scale;
  if (w > 0 && h > 0) {
    Rect rc = {x0 * scale, y0 * scale, w, h};
    FillRect(&screen, rc, getcolor(col));
  }
}

static void p8_print(const char *str, int x, int y, int col) {
  for (char c = *str; c; c = *(++str)) {
    c &= 0x7F;
    Rect srcrc = {8 * (c % 16), 8 * (c / 16)};
    srcrc.x *= scale;
    srcrc.y *= scale;
    srcrc.width = srcrc.height = 8 * scale;

    Rect dstrc = {x * scale, y * scale, scale, scale};
    Xblit(&font, &srcrc, &screen, &dstrc, col, 0, 0);
    x += 4;
  }
}