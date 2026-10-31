/* OCaml FFI */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>


/* Relevant GLFW headers */

typedef struct GLFWwindow GLFWwindow;

#define GLFW_PLATFORM_WIN32 0x00060001
#define GLFW_PLATFORM_COCOA 0x00060002
#define GLFW_PLATFORM_WAYLAND 0x00060003
#define GLFW_PLATFORM_X11 0x00060004

extern int glfwGetPlatform(void);
extern GLFWwindow *glfwGetCurrentContext(void);


/* Relevant Windows headers */

#ifdef _WIN32

#include <windows.h>

extern HWND glfwGetWin32Window(GLFWwindow*);

#endif


/* Relevant MacOS headers */

#ifdef __APPLE__

#include <stdlib.h>
#include <objc/objc.h>
#include <objc/runtime.h>
#include <objc/message.h>

extern id glfwGetCocoaWindow(GLFWwindow*);

#define objc_send0(t_ret, obj, sel) \
  ((t_ret (*)(id, SEL))objc_msgSend)((obj), sel_registerName(sel))

#define objc_send1(t_ret, t_arg, obj, sel, arg) \
  ((t_ret (*)(id, SEL, t_arg))objc_msgSend)((obj), sel_registerName(sel), (arg))

#define objc_send2(t_ret, t_arg1, t_arg2, obj, sel, arg1, arg2) \
  ((t_ret (*)(id, SEL, t_arg1, t_arg2))objc_msgSend)((obj), sel_registerName(sel), (arg1), (arg2))

#endif


/* Relevant Linux headers */

#ifdef __linux__

#include <string.h>
#include <dlfcn.h>

typedef int XBool;
typedef unsigned long XTime;
typedef unsigned long XAtom;
typedef unsigned long XWindow;
typedef struct XDisplay XDisplay;
typedef struct XEvent XEvent;

extern XDisplay *glfwGetX11Display(void) __attribute__((weak));
extern XWindow glfwGetX11Window(GLFWwindow*) __attribute__((weak));
extern void *glfwGetWaylandDisplay(void) __attribute__((weak));

typedef enum {LINUX_UNINIT = 0, LINUX_INITING, LINUX_INIT} linux_init;

static struct {
  linux_init init;
  XDisplay *display;
  int (*XFlush)(XDisplay*);
  XWindow (*XDefaultRootWindow)(XDisplay*);
  int (*XMoveResizeWindow)(XDisplay*, XWindow, int, int, unsigned, unsigned);
  int (*XSendEvent)(XDisplay*, XWindow, XBool, long, XEvent*);
  int (*XUngrabPointer)(XDisplay*, XTime);
  XAtom (*XInternAtom)(XDisplay*, const char*, XBool);
} x11;

static int x11_init(void)
{
  if (x11.init) return x11.init == LINUX_INIT;
  x11.init = LINUX_INITING;
  if (!glfwGetX11Display || glfwGetPlatform() != GLFW_PLATFORM_X11 || !glfwGetX11Window)
    return 0;
  x11.display = glfwGetX11Display();
  void *dl = dlopen("libX11.so.6", RTLD_LAZY);
  if (!x11.display || !dl) return 0;
  *(void**)&x11.XFlush = dlsym(dl, "XFlush");
  *(void**)&x11.XDefaultRootWindow = dlsym(dl, "XDefaultRootWindow");
  *(void**)&x11.XMoveResizeWindow = dlsym(dl, "XMoveResizeWindow");
  *(void**)&x11.XSendEvent = dlsym(dl, "XSendEvent");
  *(void**)&x11.XUngrabPointer = dlsym(dl, "XUngrabPointer");
  *(void**)&x11.XInternAtom = dlsym(dl, "XInternAtom");
  if (!x11.XFlush || !x11.XDefaultRootWindow || !x11.XMoveResizeWindow || !x11.XSendEvent || !x11.XUngrabPointer || !x11.XInternAtom) return 0;
  x11.init = LINUX_INIT;
  return 1;
}

static struct {
  linux_init init;
  void *display;
  int (*flush)(void *);
} wl;

static int wayland_init(void)
{
  if (wl.init) return wl.init == LINUX_INIT;
  wl.init = LINUX_INITING;
  if (!glfwGetWaylandDisplay || glfwGetPlatform() != GLFW_PLATFORM_WAYLAND)
    return 0;
  wl.display = glfwGetWaylandDisplay();
  void *dl = dlopen("libwayland-client.so.0", RTLD_LAZY);
  if (!wl.display || !dl) return 0;
  *(void**)&wl.flush = dlsym(dl, "wl_display_flush");
  if (!wl.flush) return 0;
  wl.init = LINUX_INIT;
  return 1;
}

#endif


/* Window accessors */

#define GLFW_ACCESSOR2_API(GlfwName, caml_name, type) \
\
CAMLprim value glfw_##caml_name() \
{ \
  CAMLparam0(); \
  CAMLlocal1(vres); \
\
  GLFWwindow *win = glfwGetCurrentContext(); \
\
  type r1 = 0, r2 = 0; \
  extern void glfw##GlfwName(GLFWwindow*, type*, type*); \
  if (win) glfw##GlfwName(win, &r1, &r2); \
\
  vres = caml_alloc_tuple(2); \
  Store_field(vres, 0, Val_int((int)r1)); \
  Store_field(vres, 1, Val_int((int)r2)); \
  CAMLreturn(vres); \
} \


GLFW_ACCESSOR2_API(GetCursorPos, get_cursor_pos, double)
GLFW_ACCESSOR2_API(GetWindowPos, get_window_pos, int)
GLFW_ACCESSOR2_API(GetWindowSize, get_window_size, int)
GLFW_ACCESSOR2_API(GetFramebufferSize, get_framebuffer_size, int)


/* glfwGetMonitorWorkarea */

CAMLprim value glfw_get_monitor_workarea(value vmon)
{
  CAMLparam1(vmon);
  CAMLlocal1(vres);

  int i = Int_val(vmon);

  int n = 0;
  typedef struct GLFWmonitor GLFWmonitor;
  extern GLFWmonitor **glfwGetMonitors(int*);
  GLFWmonitor **monitors = glfwGetMonitors(&n);
  int x = 0, y = 0, w = 0, h = 0;
  extern void glfwGetMonitorWorkarea(GLFWmonitor*, int*, int*, int*, int*);
  if (i < n) glfwGetMonitorWorkarea(monitors[i], &x, &y, &w, &h);

  vres = caml_alloc_tuple(4);
  Store_field(vres, 0, Val_int(x));
  Store_field(vres, 1, Val_int(y));
  Store_field(vres, 2, Val_int(w));
  Store_field(vres, 3, Val_int(h));
  CAMLreturn(vres);
}


/* glfwSetWindowSizeLimits */

CAMLprim void glfw_set_window_size_limits(value vwmin, value vhmin, value vwmax, value vhmax)
{
  CAMLparam4(vwmin, vhmin, vwmax, vhmax);
  int wmin = Int_val(vwmin);
  int hmin = Int_val(vhmin);
  int wmax = Int_val(vwmax);
  int hmax = Int_val(vhmax);

  GLFWwindow *win = glfwGetCurrentContext();
  if (!win) CAMLreturn0;

  extern void glfwSetWindowSizeLimits(GLFWwindow*, int, int, int, int);
  glfwSetWindowSizeLimits(win, wmin, hmin, wmax, hmax);

  CAMLreturn0;
}


/* glfwSetWindowAspectRatio */

CAMLprim void glfw_set_window_aspect_ratio(value vw, value vh)
{
  CAMLparam2(vw, vh);
  int w = Int_val(vw);
  int h = Int_val(vh);

  GLFWwindow *win = glfwGetCurrentContext();
  if (!win) CAMLreturn0;

  extern void glfwSetWindowAspectRatio(GLFWwindow*, int, int);
  glfwSetWindowAspectRatio(win, w, h);

  CAMLreturn0;
}


/* Emulate glfwSetWindowRect: atomic setting of pos and size */

CAMLprim void glfw_set_window_rect(value vx, value vy, value vw, value vh)
{
  CAMLparam4(vx, vy, vw, vh);
  int x = Int_val(vx);
  int y = Int_val(vy);
  int w = Int_val(vw);
  int h = Int_val(vh);

  GLFWwindow *win = glfwGetCurrentContext();
  if (!win) CAMLreturn0;

#if defined(_WIN32)

  HWND hwnd = glfwGetWin32Window(win);
  if (!hwnd) CAMLreturn0;
  (void)SetWindowPos(hwnd, NULL, x, y, w, h,
    SWP_NOACTIVATE | SWP_NOZORDER | SWP_NOOWNERZORDER);

#elif defined(__APPLE__)

  id owin = glfwGetCocoaWindow(win);
  if (!owin) CAMLreturn0;

  typedef struct { double x, y, w, h; } CGRect;

  extern unsigned int CGMainDisplayID(void);
  extern CGRect CGDisplayBounds(unsigned int);
  double top = CGDisplayBounds(CGMainDisplayID()).h;
  CGRect r = { x, top - y - h, w, h };
  objc_send2(void, CGRect, BOOL, owin, "setFrame:display:", r, 1);

/*
  typedef struct { double x, y; } NSPoint;
  typedef struct { double x, y, w, h; } NSRect;
  extern unsigned int CGMainDisplayID(void);
  extern NSRect CGDisplayBounds(unsigned int);

  id oview = glfwGetCocoaView(win);
  NSRect viewRect = objc_send0(NSRect, oview, "frame");
  double top = CGDisplayBounds(CGMainDisplayID()).h;
  NSRect dummyRect = {x, top - y - viewRect.h, 0, 0};
  NSRect viewFrameRect = objc_send1(NSRect, NSRect, owin, "frameRectForContentRect:", dummyRect);
  NSPoint viewFrameOrigin = {viewFrameRect.x, viewFrameRect.y};
  objc_send1(void, NSPoint, owin, "setFrameOrigin:", viewFrameOrigin);

  NSRect frameRect = objc_send0(NSRect, owin, "frame");
  NSRect contentRect = objc_send1(NSRect, NSRect, owin, "contentRectForFrameRect:", frameRect);
  contentRect.y += contentRect.h - h;
  contentRect.w = w;
  contentRect.h = h;
  NSRect frameRect2 = objc_send1(NSRect, NSRect, owin, "frameRectForContentRect:", contentRect);
  objc_send2(void, NSRect, BOOL, owin, "setFrame:display:", frameRect2, 1);
*/

#elif defined(__linux__)

  if (glfwGetPlatform() == GLFW_PLATFORM_X11)
  {
    if (!x11_init()) CAMLreturn0;
    XWindow xwin = glfwGetX11Window(win);
    if (!xwin) CAMLreturn0;
    x11.XMoveResizeWindow(x11.display, xwin, x, y, (unsigned)w, (unsigned)h);
    x11.XFlush(x11.display);
  }
  else if (glfwGetPlatform() == GLFW_PLATFORM_WAYLAND)
  {
    if (!wayland_init()) CAMLreturn0;
    /* Cannot set position */
    extern void glfwSetWindowSize(GLFWwindow*, int, int);
    glfwSetWindowSize(win, w, h);
  }

#else

  extern void glfwSetWindowPos(GLFWwindow*, int, int);
  extern void glfwSetWindowSize(GLFWwindow*, int, int);
  glfwSetWindowPos(win, x, y);
  glfwSetWindowSize(win, w, h);

#endif

  CAMLreturn0;
}


/* Window callbacks */

#define caml_callback0_exn caml_callback_exn
#define caml_callback1_exn caml_callback_exn

#define PARAMS0
#define PARAMS1 , int a
#define PARAMS2 , int a1, int a2
#define ARGS0
#define ARGS1 , a
#define ARGS2 , a1, a2
#define MLARGS0 , Val_unit
#define MLARGS1 , Val_int(a)
#define MLARGS2 , Val_int(a1), Val_int(a2)

#define GLFW_CALLBACK_API(GlfwName, caml_name, type, N) \
\
typedef void (*GLFW##type##fun)(GLFWwindow* PARAMS##N); \
static GLFW##type##fun old_##type##_callback = NULL; \
static value v##type##_callback = Val_unit; \
static int type##_callback_init = 0; \
static int type##_callback_busy = 0; \
\
static void type##_callback(GLFWwindow *win PARAMS##N) \
{ \
  if (old_##type##_callback) old_##type##_callback(win ARGS##N); \
\
  if (type##_callback_busy || v##type##_callback == Val_unit) return; \
  type##_callback_busy = 1; \
  int need_unblocking = Caml_state == NULL; \
  if (need_unblocking) caml_leave_blocking_section(); \
  (void)caml_callback##N##_exn(v##type##_callback MLARGS##N); \
  if (need_unblocking) caml_enter_blocking_section(); \
  type##_callback_busy = 0; \
} \
\
static void init_##type##_callback() \
{ \
  if (type##_callback_init) return; \
\
  GLFWwindow *win = glfwGetCurrentContext(); \
  if (!win) return; \
\
  extern GLFW##type##fun glfwSet##GlfwName##Callback(GLFWwindow*, GLFW##type##fun); \
  old_##type##_callback = glfwSet##GlfwName##Callback(win, type##_callback); \
  type##_callback_init = 1; \
} \
\
CAMLprim void glfw_set_##caml_name##_callback(value vf) \
{ \
  CAMLparam1(vf); \
\
  init_##type##_callback(); \
\
  caml_register_global_root(&v##type##_callback); \
  v##type##_callback = vf; \
\
  CAMLreturn0; \
} \


GLFW_CALLBACK_API(WindowRefresh, window_refresh, windowrefresh, 0)
GLFW_CALLBACK_API(WindowPos, window_pos, windowpos, 2)
GLFW_CALLBACK_API(WindowSize, window_size, windowsize, 2)
GLFW_CALLBACK_API(FramebufferSize, framebuffer_size, framebuffersize, 2)


/* Emulation of a glfwDragWindow */
/* Inspired by https://github.com/glfw/glfw/pull/2152
 * and https://github.com/Seneral/glfw */

CAMLprim value glfw_drag_window(void)
{
  CAMLparam0();

  init_windowpos_callback();

  GLFWwindow *win = glfwGetCurrentContext();
  if (!win) CAMLreturn(Val_false);

#if defined(_WIN32)

  CAMLreturn(Val_false);

  #if 0  /* this is blocking, and hence useless */
  HWND hwnd = glfwGetWin32Window(win);
  if (!hwnd) CAMLreturn(Val_false);

  ReleaseCapture();
  SendMessage(hwnd, WM_NCLBUTTONDOWN, HTCAPTION, 0);

  #define GLFW_MOUSE_BUTTON_LEFT 0
  #define GLFW_RELEASE 0

  extern void _glfwInputMouseClick(GLFWwindow*, int, int, int);
  _glfwInputMouseClick(win, GLFW_MOUSE_BUTTON_LEFT, GLFW_RELEASE, 0 /*getKeyMods()*/);

  CAMLreturn(Val_true);
  #endif

#elif defined(__APPLE__)

  id owin = glfwGetCocoaWindow(win);
  if (!owin) CAMLreturn(Val_false);

  id app = objc_send0(id, (id)objc_getClass("NSApplication"), "sharedApplication");
  id currentEvent = objc_send0(id, app, "currentEvent");
  objc_send1(void, id, owin, "performWindowDragWithEvent:", currentEvent);

  CAMLreturn(Val_true);

#elif defined(__linux__)

  if (glfwGetPlatform() == GLFW_PLATFORM_X11)
  {
    if (!x11_init()) CAMLreturn(Val_false);

    typedef struct {
      int type;
      unsigned long serial;
      XBool send_event;
      XDisplay *display;
      XWindow window;
      unsigned long message_type;
      int format;
      union {
        char b[20];
        short s[10];
        long l[5];
      } data;
    } XClientMessageEvent;

    #define ClientMessage 33
    #define SubstructureNotifyMask 0x80000
    #define SubstructureRedirectMask 0x100000
    #define _NET_WM_MOVERESIZE_MOVE 8

    x11.XUngrabPointer(x11.display, 0);
    x11.XFlush(x11.display);

    int wx, wy;
    double mx, my;
    extern void glfwGetCursorPos(GLFWwindow*, double*, double*);
    glfwGetCursorPos(win, &mx, &my);
    extern void glfwGetWindowPos(GLFWwindow*, int*, int*);
    glfwGetWindowPos(win, &wx, &wy);

    XClientMessageEvent event;
    memset(&event, 0, sizeof(XClientMessageEvent));
    event.type = ClientMessage;
    event.window = glfwGetX11Window(win);
    event.message_type = x11.XInternAtom(x11.display, "_NET_WM_MOVERESIZE", 0);
    event.format = 32;
    event.data.l[0] = wx + mx;
    event.data.l[1] = wy + my;
    event.data.l[2] = _NET_WM_MOVERESIZE_MOVE;
    event.data.l[3] = 0;
    event.data.l[4] = 0;
    XWindow root = x11.XDefaultRootWindow(x11.display);
    x11.XSendEvent(x11.display, root, 0, SubstructureNotifyMask | SubstructureRedirectMask, (XEvent*)&event);

    CAMLreturn(Val_true);
  }
  else if (glfwGetPlatform() == GLFW_PLATFORM_WAYLAND)
  {
    if (!wayland_init()) CAMLreturn(Val_false);
    /* TODO: would need acces to GLFW/Wayland internals */
  }

  CAMLreturn(Val_false);

#else

  CAMLreturn(Val_false);

#endif
}


/* Emulation of a glfwResizeWindow */
/* Inspired by https://github.com/Seneral/glfw */

#define GLFW_WINDOW_LEFT 1
#define GLFW_WINDOW_TOP 2
#define GLFW_WINDOW_RIGHT 4
#define GLFW_WINDOW_BOTTOM 8
#define GLFW_WINDOW_TOPLEFT (GLFW_WINDOW_TOP|GLFW_WINDOW_LEFT)
#define GLFW_WINDOW_TOPRIGHT (GLFW_WINDOW_TOP|GLFW_WINDOW_RIGHT)
#define GLFW_WINDOW_BOTTOMLEFT (GLFW_WINDOW_BOTTOM|GLFW_WINDOW_LEFT)
#define GLFW_WINDOW_BOTTOMRIGHT (GLFW_WINDOW_BOTTOM|GLFW_WINDOW_RIGHT)

CAMLprim value glfw_resize_window(value vborder)
{
  CAMLparam1(vborder);

  init_windowpos_callback();

  GLFWwindow *win = glfwGetCurrentContext();
  if (!win) CAMLreturn(Val_false);

#if defined(_WIN32)

  CAMLreturn(Val_false);

  #if 0  /* This doesn't appear to work */
  HWND hwnd = glfwGetWin32Window(win);
  if (!hwnd) CAMLreturn(Val_false);

  int border = Int_val(vborder);
  WPARAM wborder;
  switch (border)
  {
    case GLFW_WINDOW_LEFT: wborder = HTLEFT; break;
    case GLFW_WINDOW_TOP: wborder = HTTOP; break;
    case GLFW_WINDOW_RIGHT: wborder = HTRIGHT; break;
    case GLFW_WINDOW_BOTTOM: wborder = HTBOTTOM; break;
    case GLFW_WINDOW_TOPLEFT: wborder = HTTOPLEFT; break;
    case GLFW_WINDOW_TOPRIGHT: wborder = HTTOPRIGHT; break;
    case GLFW_WINDOW_BOTTOMLEFT: wborder = HTBOTTOMLEFT; break;
    case GLFW_WINDOW_BOTTOMRIGHT: wborder = HTBOTTOMRIGHT; break;
    default: CAMLreturn(Val_false);
  }

  ReleaseCapture();
  SendMessage(hwnd, WM_NCLBUTTONDOWN, wborder, 0);

  #define GLFW_MOUSE_BUTTON_LEFT 0
  #define GLFW_RELEASE 0

  extern void _glfwInputMouseClick(GLFWwindow*, int, int, int);
  _glfwInputMouseClick(win, GLFW_MOUSE_BUTTON_LEFT, GLFW_RELEASE, 0 /*getKeyMods()*/);

  CAMLreturn(Val_true);
  #endif

#elif defined(__APPLE__)

  /* There doesn't seem to be an equivalent */
  CAMLreturn(Val_false);

#elif defined(__linux__)

  if (glfwGetPlatform() == GLFW_PLATFORM_X11)
  {
    if (!x11_init()) CAMLreturn(Val_false);

    typedef struct {
      int type;
      unsigned long serial;
      XBool send_event;
      XDisplay *display;
      XWindow window;
      unsigned long message_type;
      int format;
      union {
        char b[20];
        short s[10];
        long l[5];
      } data;
    } XClientMessageEvent;

    #define ClientMessage 33
    #define SubstructureNotifyMask 0x80000
    #define SubstructureRedirectMask 0x100000
    #define _NET_WM_MOVERESIZE_SIZE_TOPLEFT 0
    #define _NET_WM_MOVERESIZE_SIZE_TOP 1
    #define _NET_WM_MOVERESIZE_SIZE_TOPRIGHT 2
    #define _NET_WM_MOVERESIZE_SIZE_RIGHT 3
    #define _NET_WM_MOVERESIZE_SIZE_BOTTOMRIGHT 4
    #define _NET_WM_MOVERESIZE_SIZE_BOTTOM 5
    #define _NET_WM_MOVERESIZE_SIZE_BOTTOMLEFT 6
    #define _NET_WM_MOVERESIZE_SIZE_LEFT 7

    x11.XUngrabPointer(x11.display, 0);
    x11.XFlush(x11.display);

    int wx, wy;
    double mx, my;
    extern void glfwGetCursorPos(GLFWwindow*, double*, double*);
    glfwGetCursorPos(win, &mx, &my);
    extern void glfwGetWindowPos(GLFWwindow*, int*, int*);
    glfwGetWindowPos(win, &wx, &wy);

    int border = Int_val(vborder);
    long xborder;
    switch (border)
    {
      case GLFW_WINDOW_LEFT: xborder = _NET_WM_MOVERESIZE_SIZE_LEFT; break;
      case GLFW_WINDOW_TOP: xborder = _NET_WM_MOVERESIZE_SIZE_TOP; break;
      case GLFW_WINDOW_RIGHT: xborder = _NET_WM_MOVERESIZE_SIZE_RIGHT; break;
      case GLFW_WINDOW_BOTTOM: xborder = _NET_WM_MOVERESIZE_SIZE_BOTTOM; break;
      case GLFW_WINDOW_TOPLEFT: xborder = _NET_WM_MOVERESIZE_SIZE_TOPLEFT; break;
      case GLFW_WINDOW_TOPRIGHT:xborder = _NET_WM_MOVERESIZE_SIZE_TOPRIGHT; break;
      case GLFW_WINDOW_BOTTOMLEFT: xborder = _NET_WM_MOVERESIZE_SIZE_BOTTOMLEFT; break;
      case GLFW_WINDOW_BOTTOMRIGHT: xborder = _NET_WM_MOVERESIZE_SIZE_BOTTOMRIGHT; break;
      default: CAMLreturn(Val_false);
    }

    XClientMessageEvent event;
    memset(&event, 0, sizeof(XClientMessageEvent));
    event.type = ClientMessage;
    event.window = glfwGetX11Window(win);
    event.message_type = x11.XInternAtom(x11.display, "_NET_WM_MOVERESIZE", 0);
    event.format = 32;
    event.data.l[0] = wx + mx;
    event.data.l[1] = wy + my;
    event.data.l[2] = xborder;
    event.data.l[3] = 0;
    event.data.l[4] = 0;
    XWindow root = x11.XDefaultRootWindow(x11.display);
    x11.XSendEvent(x11.display, root, 0, SubstructureNotifyMask | SubstructureRedirectMask, (XEvent*)&event);

    CAMLreturn(Val_true);
  }
  else if (glfwGetPlatform() == GLFW_PLATFORM_WAYLAND)
  {
    if (!wayland_init()) CAMLreturn(Val_false);
    /* TODO: would need acces to GLFW/Wayland internals */
  }

  CAMLreturn(Val_false);

#else

  CAMLreturn(Val_false);

#endif
}
