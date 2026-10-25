#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>


/* Relevant parts of GLFW headers */

typedef struct GLFWmonitor GLFWmonitor;
typedef struct GLFWwindow GLFWwindow;
typedef void (*GLFWwindowsizefun)(GLFWwindow*, int, int);
typedef void (*GLFWwindowrefreshfun)(GLFWwindow*);

extern GLFWwindow *glfwGetCurrentContext(void);
extern GLFWwindowsizefun glfwSetWindowSizeCallback(GLFWwindow*, GLFWwindowsizefun);
extern GLFWwindowrefreshfun glfwSetWindowRefreshCallback(GLFWwindow*, GLFWwindowrefreshfun);
extern GLFWmonitor **glfwGetMonitors(int*);
extern void glfwGetMonitorWorkarea(GLFWmonitor*, int*, int*, int*, int*);


/* Relevant parts of Cocoa headers */

#ifdef __APPLE__
#include <stdlib.h>
#include <objc/objc.h>
#include <objc/runtime.h>
#include <objc/message.h>

extern id glfwGetCocoaWindow(GLFWwindow*);

static id objc_send(id obj, const char *sel)
{
  return ((id (*)(id, SEL))objc_msgSend)(obj, sel_registerName(sel));
}
static void objc_send_id(id obj, const char *sel, id a)
{
  ((void (*)(id, SEL, id))objc_msgSend)(obj, sel_registerName(sel), a);
}
static id objc_string(const char *s)
{
  return ((id (*)(id, SEL, const char *))objc_msgSend)(
    (id)objc_getClass("NSString"), sel_registerName("stringWithUTF8String:"), s);
}
#endif


/* Callbacks */

static GLFWwindow *win = NULL;
static GLFWwindowsizefun old_size_callback = NULL;
//static GLFWwindowrefreshfun old_refresh_callback = NULL;
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

static void size_callback(GLFWwindow *win, int w, int h)
{
  if (old_size_callback) old_size_callback(win, w, h);

  if (busy) return;
  busy = 1;
  int acquired = acquire_runtime();
  const value *f = caml_named_value("glfw_on_size");
  if (f) (void)caml_callback2_exn(*f, Val_int(w), Val_int(h));
  release_runtime(acquired);
  busy = 0;
}
/*
static void refresh_callback(GLFWwindow *win)
{
  if (old_refresh_callback) old_refresh_callback(win);

  if (busy) return;
  busy = 1;
  int acquired = acquire_runtime();
  const value *f = caml_named_value("glfw_on_refresh");
  if (f) (void)caml_callback_exn(*f, Val_unit);
  release_runtime(acquired);
  busy = 0;
}
*/
CAMLprim value glfw_install()
{
  CAMLparam0();

  win = glfwGetCurrentContext();
  if (!win) CAMLreturn(Val_false);
  old_size_callback = glfwSetWindowSizeCallback(win, size_callback);
//  old_refresh_callback = glfwSetWindowRefreshCallback(win, refresh_callback);

#ifdef __APPLE__
  /* For Cocoa, set gravity to top-left to avoid stretching/shrinking */
  id cocoaWin = win ? glfwGetCocoaWindow(win) : NULL;
  id contentView = cocoaWin ? objc_send(cocoaWin, "contentView") : NULL;
  id layer = contentView ? objc_send(contentView, "layer") : NULL;
  if (!layer) CAMLreturn(Val_true);
  objc_send_id(layer, "setContentsGravity:", objc_string("topLeft"));
#endif

  CAMLreturn(Val_true);
}

CAMLprim value glfw_get_monitor_workarea(value v_mon)
{
  CAMLparam1(v_mon);
  CAMLlocal1(v_result);

  int i = Int_val(v_mon);
  int n = 0;
  GLFWmonitor **monitors = glfwGetMonitors(&n);
  int x = 0, y = 0, w = 0, h = 0;
  if (i < n) glfwGetMonitorWorkarea(monitors[i], &x, &y, &w, &h);

  v_result = caml_alloc_tuple(4);
  Store_field(v_result, 0, Val_int(x));
  Store_field(v_result, 1, Val_int(y));
  Store_field(v_result, 2, Val_int(w));
  Store_field(v_result, 3, Val_int(h));
  CAMLreturn(v_result);
}
