// -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 237) (2 outline 259) (3 outline 283) (4 outline 303) (5 outline 375)); -*-
#ifndef CAML_DOMAIN_H
#define CAML_DOMAIN_H

#ifdef CAML_INTERNALS

#include <stdbool.h>

#include "camlatomic.h"
#include "config.h"
CAMLextern bool caml_thread_running_on_expected_domain(uintnat);
/* See caml_c_thread_register_in_domain_index */
#include "mlvalues.h"


#endif

#endif
