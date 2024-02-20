#include "stdint.h"

struct RGBA {
  uint8_t r;
  uint8_t g;
  uint8_t b;
  uint8_t a;
};

struct Color {
  uint8_t r;
  uint8_t g;
  uint8_t b;
};

RGBA toRGBA(Color c) { return {c.r, c.g, c.b, 0}; }

uint32_t toUint32(RGBA c) {
  uint32_t r = c.r;
  uint32_t g = c.g;
  uint32_t b = c.b;
  return (b << 16) | (g << 8) | (r);
}

uint32_t toUint32(Color c) {
  uint32_t r = c.r;
  uint32_t g = c.g;
  uint32_t b = c.b;
  return (b << 16) | (g << 8) | (r);
}

struct Rect {
  int x;
  int y;
  int width;
  int height;
};

struct Surface {
  int width;
  int height;
  uint32_t *data;

  uint32_t getpixel(int x, int y) { return y * width + x; }
  void setpixel(int x, int y, uint32_t col) { data[y * width + x] = col; }

  Rect clip_rect() {
    return {.x = 0, .y = 0, .width = width, .height = height};
  }
};
