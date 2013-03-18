from rpython.rlib.rarithmetic import r_uint
from rpython.rtyper.lltypesystem import lltype, rffi

from rsdl import RSDL, RSDL_helper


class SDLDisplay(object):
    _attrs_ = ["screen", "width", "height", "depth", "surface", "has_surface"]

    def __init__(self):
        assert RSDL.Init(RSDL.INIT_VIDEO) >= 0
        self.has_surface = False

    def set_video_mode(self, w, h, d):
        assert w > 0 and h > 0
        assert d in [1, 2, 4, 8, 16, 32]
        self.width = w
        self.height = h
        self.depth = d
        self.screen = RSDL.SetVideoMode(w, h, 32, 0)
        assert self.screen
        # self.fillwhite()

    def set_pixelbuffer(self, pixelbuffer):
        if self.has_surface:
            RSDL.FreeSurface(self.surface)
        pitch = 4 * self.width
        rmask, gmask, bmask, amask = r_uint(0x000000FF), r_uint(0x0000FF00), r_uint(0x00FF0000), r_uint(0xFF000000)
        self.surface = RSDL.CreateRGBSurfaceFrom(pixelbuffer, self.width, self.height, 32, pitch,
                                                 rmask, gmask, bmask, amask)
        self.has_surface = True

    def fillwhite(self):
        fmt = self.screen.c_format
        color = RSDL.MapRGB(fmt, 255, 255, 255)
        RSDL.FillRect(self.screen, lltype.nullptr(RSDL.Rect), color)
        RSDL.Flip(self.screen)

    def blit(self):
        RSDL.BlitSurface(self.surface, lltype.nullptr(RSDL.Rect), self.screen, lltype.nullptr(RSDL.Rect))
        RSDL.Flip(self.screen)
