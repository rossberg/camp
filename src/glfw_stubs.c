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
//extern id glfwGetCocoaView(GLFWwindow*);

#define objc_send0(t_ret, obj, sel) \
  ((t_ret (*)(id, SEL))objc_msgSend)((obj), sel_registerName(sel))

#define objc_send1(t_ret, t_arg, obj, sel, arg) \
  ((t_ret (*)(id, SEL, t_arg))objc_msgSend)((obj), sel_registerName(sel), (arg))

#define objc_send2(t_ret, t_arg1, t_arg2, obj, sel, arg1, arg2) \
  ((t_ret (*)(id, SEL, t_arg1, t_arg2))objc_msgSend)((obj), sel_registerName(sel), (arg1), (arg2))

#endif


/* Relevant Linux headers */

#ifdef __linux__

#include <dlfcn.h>

extern void *glfwGetX11Display(void) __attribute__((weak));
extern unsigned long glfwGetX11Window(GLFWwindow*) __attribute__((weak));
extern void *glfwGetWaylandDisplay(void) __attribute__((weak));

typedef enum {LINUX_UNINIT = 0, LINUX_INITING, LINUX_INIT} linux_init;

static struct {
  linux_init init;
  void *display;
  int (*flush)(void*);
  int (*move_resize)(void*, unsigned long, int, int, unsigned, unsigned);
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
  *(void**)&x11.flush = dlsym(dl, "XFlush");
  *(void**)&x11.move_resize = dlsym(dl, "XMoveResizeWindow");
  if (!x11.flush || !x11.move_resize) return 0;
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


/* Exposed GLFW functionality */

CAMLprim value glfw_get_window_pos()
{
  CAMLparam0();
  CAMLlocal1(vres);

  GLFWwindow *win = glfwGetCurrentContext();

  int x = 0, y = 0;
  extern void glfwGetWindowPos(GLFWwindow*, int*, int*);
  if (win) glfwGetWindowPos(win, &x, &y);

  vres = caml_alloc_tuple(2);
  Store_field(vres, 0, Val_int(x));
  Store_field(vres, 1, Val_int(y));
  CAMLreturn(vres);
}

CAMLprim value glfw_get_window_size()
{
  CAMLparam0();
  CAMLlocal1(vres);

  GLFWwindow *win = glfwGetCurrentContext();

  int w = 0, h = 0;
  extern void glfwGetWindowSize(GLFWwindow*, int*, int*);
  if (win) glfwGetWindowSize(win, &w, &h);

  vres = caml_alloc_tuple(2);
  Store_field(vres, 0, Val_int(w));
  Store_field(vres, 1, Val_int(h));
  CAMLreturn(vres);
}

CAMLprim void glfw_set_window_rect(value vx, value vy, value vw, value vh)
{
  CAMLparam4(vx, vy, vw, vh);
  int x = Int_val(vx);
  int y = Int_val(vy);
  int w = Int_val(vw);
  int h = Int_val(vh);

  GLFWwindow *win = glfwGetCurrentContext();

#if defined(_WIN32)

  HWND hwnd = glfwGetWin32Window(win);
  if (!hwnd) CAMLreturn0;
  (void)SetWindowPos(hwnd, NULL, x, y, w, h,
    SWP_NOACTIVATE | SWP_NOZORDER | SWP_NOOWNERZORDER);

#elif defined(__APPLE__)
  id owin = win ? glfwGetCocoaWindow(win) : NULL;
  unsigned long mask =
    owin ? objc_send0(unsigned long, owin, "styleMask") : 1;
  if (mask & 1) CAMLreturn0;

  typedef struct { double x, y, w, h; } CGRect;

  extern unsigned int CGMainDisplayID(void);
  extern CGRect CGDisplayBounds(unsigned int);
  double top = CGDisplayBounds(CGMainDisplayID()).h;
  CGRect r = { x, top - y - h, w, h };
  objc_send2(void, CGRect, BOOL, owin, "setFrame:display:", r, 1);

  /* Redundantly set position again to force update in Raylib */
  /* (This may be fixed in Raylib 6.2?) */
  extern void glfwGetWindowPos(GLFWwindow*, int*, int*);
  glfwGetWindowPos(win, &x, &y);
  extern void SetWindowPosition(int x, int y);  /* from Raylib */
  SetWindowPosition(x, y);

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
    unsigned long xwin = glfwGetX11Window(win);
    if (!xwin) CAMLreturn0;
    x11.move_resize(x11.display, xwin, x, y, (unsigned)w, (unsigned)h);
    x11.flush(x11.display);
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


static int busy = 0;

static int acquire_runtime(void)
{
  if (Caml_state == NULL) {
    caml_leave_blocking_section();
    return 1;
  }
  return 0;
}

static void release_runtime(int acquired)
{
  if (acquired) caml_enter_blocking_section();
}


typedef void (*GLFWwindowrefreshfun)(GLFWwindow*);
static GLFWwindowrefreshfun old_refresh_callback = NULL;
static value vrefresh_callback = Val_unit;

static void refresh_callback(GLFWwindow *win)
{
  if (old_refresh_callback) old_refresh_callback(win);

  if (busy) return;
  busy = 1;
  int acquired = acquire_runtime();
  (void)caml_callback_exn(vrefresh_callback, Val_unit);
  release_runtime(acquired);
  busy = 0;
}

CAMLprim void glfw_set_window_refresh_callback(value vf)
{
  CAMLparam1(vf);

  GLFWwindow *win = glfwGetCurrentContext();
  if (!win) CAMLreturn0;

  extern GLFWwindowrefreshfun glfwSetWindowRefreshCallback(GLFWwindow*, GLFWwindowrefreshfun);
  old_refresh_callback = glfwSetWindowRefreshCallback(win, refresh_callback);

  caml_register_global_root(&vrefresh_callback);
  vrefresh_callback = vf;

  CAMLreturn0;
}

typedef void (*GLFWwindowsizefun)(GLFWwindow*, int, int);
static GLFWwindowsizefun old_size_callback = NULL;
static value vsize_callback = Val_unit;

static void size_callback(GLFWwindow *win, int w, int h)
{
  if (old_size_callback) old_size_callback(win, w, h);

  if (busy) return;
  busy = 1;
  int acquired = acquire_runtime();
  (void)caml_callback2_exn(vsize_callback, Val_int(w), Val_int(h));
  release_runtime(acquired);
  busy = 0;
}

CAMLprim void glfw_set_window_size_callback(value vf)
{
  CAMLparam1(vf);

  GLFWwindow *win = glfwGetCurrentContext();
  if (!win) CAMLreturn0;

  extern GLFWwindowsizefun glfwSetWindowSizeCallback(GLFWwindow*, GLFWwindowsizefun);
  old_size_callback = glfwSetWindowSizeCallback(win, size_callback);

  caml_register_global_root(&vsize_callback);
  vsize_callback = vf;

  CAMLreturn0;
}
