/*
 * libffi interface to Mickey Scheme
 *
 * Copyright (C) 2013 Christian Stigen Larsen
 * Distributed under any of LGPL v2.1, LGPL 3.0, GPL 2.0 or GPL 3.0
 *
 */

#include <mickey.h>
#include <ffi/ffi.h> // libffi

extern "C" {

static const char tag_ffi_cif[] = "libffi call interface";
static const char tag_ffi_retval[] = "libffi return value";

/*
 * ifdefs taken from ffi/x86-ffitarget.h
 */
static struct {
  const char* name;
  ffi_abi value;
} ffi_abi_map[] = {
  {"default-abi", FFI_DEFAULT_ABI},
  {"sysv-abi", FFI_SYSV},

#ifdef X86_WIN32
  {"stdcall-abi", FFI_STDCALL},
#endif

#if !defined(X86_WIN32) && (defined(__i386__) || defined(__x86_64__))
  {"unix64-abi", FFI_UNIX64},
#endif

  {NULL, FFI_LAST_ABI}
};

static struct {
  const char* name;
  ffi_type* value;
} ffi_type_map[] = {
  {"void",       &ffi_type_void},
  {"uint",       &ffi_type_uint},
  {"uint8",      &ffi_type_uint8},
  {"uint16",     &ffi_type_uint16},
  {"uint32",     &ffi_type_uint32},
  {"uint64",     &ffi_type_uint64},
  {"sint",       &ffi_type_sint16},
  {"sint8",      &ffi_type_sint8},
  {"sint16",     &ffi_type_sint16},
  {"sint32",     &ffi_type_sint32},
  {"sint64",     &ffi_type_sint64},
  {"ulong",      &ffi_type_ulong},
  {"slong",      &ffi_type_slong},
  {"float",      &ffi_type_float},
  {"double",     &ffi_type_double},
  {"longdouble", &ffi_type_longdouble},
  {"pointer",    &ffi_type_pointer},
  {"uchar",      &ffi_type_uchar},
  {"schar",      &ffi_type_schar},
  {"ushort",     &ffi_type_ushort},
  {"sshort",     &ffi_type_sshort},
  {NULL,         &ffi_type_void}
};

static void check(const ffi_status& s)
{
  std::string err = "Unknown ffi error";

  switch ( s ) {
  case FFI_OK: return;
  case FFI_BAD_TYPEDEF: err = "FFI_BAD_TYPEDEF"; break;
  case FFI_BAD_ABI: err = "FFI_BAD_ABI"; break;
  }

  raise(runtime_exception(err));
}

static ffi_abi parse_ffi_abi(cons_t* p)
{
  assert_type(SYMBOL, p);

  const std::string name = symbol_name(p);

  for ( size_t n=0; ffi_abi_map[n].name != NULL; ++n)
    if ( name == ffi_abi_map[n].name )
      return ffi_abi_map[n].value;

  raise(runtime_exception(format(
    "Unknown FFI ABI: %s", name.c_str())));

  return FFI_DEFAULT_ABI; // appease compiler
}

static ffi_type* parse_ffi_type(cons_t* p)
{
  assert_type(SYMBOL, p);

  const std::string name = symbol_name(p);

  for ( size_t n=0; ffi_type_map[n].name != NULL; ++n)
    if ( name == ffi_type_map[n].name )
      return ffi_type_map[n].value;

  raise(runtime_exception(format(
    "Unknown FFI type: %s", name.c_str())));

  return &ffi_type_void; // appease compiler
}

/*
 * (prepare-call-interface
 *   <foreign function ABI>
 *   <type of return value>
 *   (<type of input argument> ...))
 *
 *
 * TODO:
 * - Put ABI parameter last.
 * - If there is no input-value list, then ignore it.
 */
cons_t* proc_ffi_prep_cif(cons_t* p, environment_t*)
{
  assert_length(p, 2, 3);

  ffi_abi abi = FFI_DEFAULT_ABI;

  /*
   * ARGUMENT 1: ABI for foreign function
   */
  abi = parse_ffi_abi(car(p));

  /*
   * ARGUMENT 2:
   * Return type for foreign function
   */
  ffi_type* rtype = parse_ffi_type(cadr(p));

  /*
   * ARGUMENT 3:
   * Types for foreign function's input parameters.
   */
  ffi_type** argtypes = NULL;
  unsigned int nargs = 0;

  if ( length(p) >= 3 ) {
    cons_t *args = caddr(p);
    assert_type(PAIR, args);
    nargs = length(args);

    if ( nargs > 0 ) {
      argtypes = static_cast<ffi_type**>(malloc(nargs*sizeof(ffi_type*)));

      for ( unsigned int n=0; n<nargs; ++n ) {
        argtypes[n] = parse_ffi_type(car(args));
        args = cdr(args);
      }
    }
  }

  /*
   * Initialize returned struct
   */
  ffi_cif *cif = new ffi_cif();
  memset(cif, 0, sizeof(ffi_cif));

  check(ffi_prep_cif(cif, abi, nargs, rtype, argtypes));
  return pointer(tag_ffi_cif, cif);

  /*
   * In the future, the malloced argtypes should be added to the
   * pointer-return value here, so that it too can be freed.
   */
}

/*
 * (call <tag_ffi_cif>
 *       <closure w/C function pointer>
 *       <rvalue size in bytes>
 * )
 */
cons_t* proc_ffi_call(cons_t* p, environment_t*)
{
  assert_length(p, 2, 3);
  assert_pointer(tag_ffi_cif, car(p));
  assert_type(CLOSURE, cadr(p));
  assert_type(INTEGER, caddr(p));

  /*
   * libffi description of function.
   */

  ffi_cif *call_interface = static_cast<ffi_cif*>(car(p)->pointer->value);

  /*
   * Pointer to function to call.
   */

  if ( cadr(p)->closure->function == NULL )
    raise(runtime_exception(
      "Can only call foreign C functions; not Scheme procedures"));

  void (*funptr)() =
    reinterpret_cast<void(*)()>(cadr(p)->closure->function);

  /*
   * Size of return value.
   */
  integer_t size = 0;

  if ( length(p)>2 )
    size = caddr(p)->number.integer;

  if ( size < 0 )
    raise(runtime_exception(format(
      "Cannot allocate a negative number of bytes: %d", size)));

  /*
   * Return value.
   */
  void *retval = NULL;

  /*
   * Allocate space for return value and initialize it.
   *
   * It's the caller's responsibility to get this value right with proper
   * alignment!
   */
  if ( size > 0 ) {
    retval = malloc(size);
    memset(retval, 0, size);
  }

  /*
   * Function arguments (currently unsupported).
   */
  void **funargs = NULL;

  ffi_call(call_interface, funptr, retval, funargs);
  return !retval? nil() : pointer(tag_ffi_retval, retval);
}

/*
 * (return-value->string <retval>)
 *
 * USE AT YOUR OWN RISK! :-)
 */
cons_t* proc_retval_to_string(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_pointer(tag_ffi_retval, car(p));

  /*
   * Note that string() duplicates the string, so this is a bit potential
   * risk. We'll assume the authors know they're doing :-)
   */
  char *s = static_cast<char*>(car(p)->pointer->value);
  return string(s);
}

}; // extern "C"
