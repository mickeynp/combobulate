// -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 454) (2 outline 497) (3 outline 549) (4 outline 592) (5 outline 649) (6 outline 715)); -*-
// Declarations led by a macro tree-sitter cannot expand.  Each of these
// parses into a `declaration' plus a stray `expression_statement'; the
// sibling rules skip the fragment so navigation steps declaration to
// declaration.  Shapes taken from the OCaml runtime.
typedef struct dom_internal dom_internal;

static CAMLthread_local dom_internal* domain_self;

CAMLexport atomic_uintnat

static atomic_intnat domains_exiting = 0; caml_num_domains_running = 0;

CAMLexport void caml_domain_set_name(char *name)
{
    return;
}

static int plain_tail = 1;
