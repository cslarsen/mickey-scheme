/*
 * libffi interface to Mickey Scheme
 *
 * Copyright (C) 2013 Christian Stigen Larsen
 * Distributed under any of LGPL v2.1, LGPL 3.0, GPL 2.0 or GPL 3.0
 *
 */

#include <mickey.h>
#include <ffi.h>

extern "C" {

static const char tag_ffi_cif[]      = "libffi call interface";
static const char tag_ffi_retval[]   = "libffi return value";
static const char tag_ffi_type[]     = "libffi type";
static const char tag_void_pointer[] = "void*";

/*
 * We need to hold the return value and its size in bytes.
 * (Will also let us free() memory as well, later on)
 *
 * If size==0, then data has not been malloc'ed and needn't be freed.
 */
struct value_t {
  void *data;
  size_t size;

  value_t(const size_t bytes = sizeof(long))
  {
    if ( bytes < sizeof(long) )
      size = sizeof(long);

    data = malloc(size);
    memset(data, 0, size);
  }

  char* string()
  {
    return static_cast<char*>(data);
  }

  integer_t integer()
  {
    return reinterpret_cast<intptr_t>(data);
  }

  character_t character()
  {
    character_t c = '\0';
    memcpy(data, &c, sizeof(character_t));
    return c;
  }

  float real_float()
  {
    float f = 0;
    memcpy(data, &f, sizeof(float));
    return f;
  }

  double real_double()
  {
    double d = 0;
    memcpy(data, &d, sizeof(double));
    return d;
  }

  void free()
  {
    ::free(data);
  }
};

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

static struct {
  const char* name;
  size_t size;
} type_sizes[] = {
  {"uint",       sizeof(unsigned int)},
  {"uint8",      1},
  {"uint16",     2},
  {"uint32",     4},
  {"uint64",     8},
  {"sint",       sizeof(signed int)},
  {"sint8",      1},
  {"sint16",     2},
  {"sint32",     4},
  {"sint64",     8},
  {"ulong",      sizeof(unsigned long)},
  {"slong",      sizeof(signed long)},
  {"float",      sizeof(float)},
  {"double",     sizeof(double)},
  {"longdouble", sizeof(long double)},
  {"pointer",    sizeof(void*)},
  {"uchar",      sizeof(unsigned char)},
  {"schar",      sizeof(signed char)},
  {"ushort",     sizeof(unsigned short)},
  {"sshort",     sizeof(signed short)},
  {NULL,         0}
};

static void check(const ffi_status& s)
{
  std::string err = "Unknown ffi error";

  switch ( s ) {
  case FFI_OK: return;
  case FFI_BAD_TYPEDEF: err = "FFI_BAD_TYPEDEF"; break;
  case FFI_BAD_ABI:     err = "FFI_BAD_ABI"; break;
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
   * Allocate enough memory necessary to hold return data.
   */
  value_t *retval = new value_t(size);

  /*
   * Function arguments (currently unsupported).
   */
  void **funargs = NULL;

  ffi_call(call_interface, funptr, &retval->data, funargs);
  return pointer(tag_ffi_retval, retval);
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
  value_t* value = static_cast<value_t*>(car(p)->pointer->value);
  return string(value->string());
}

cons_t* proc_retval_to_integer(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_pointer(tag_ffi_retval, car(p));
  value_t* value = static_cast<value_t*>(car(p)->pointer->value);
  return integer(value->integer());
}

cons_t* proc_retval_to_pointer(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_pointer(tag_ffi_retval, car(p));
  value_t* value = static_cast<value_t*>(car(p)->pointer->value);
  return pointer(tag_void_pointer, value->data);
}

cons_t* proc_retval_to_uchar(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_pointer(tag_ffi_retval, car(p));
  value_t* value = static_cast<value_t*>(car(p)->pointer->value);
  return character(value->character());
}

cons_t* proc_retval_to_float(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_pointer(tag_ffi_retval, car(p));
  value_t* value = static_cast<value_t*>(car(p)->pointer->value);
  return real(value->real_float());
}

cons_t* proc_retval_to_double(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_pointer(tag_ffi_retval, car(p));
  value_t* value = static_cast<value_t*>(car(p)->pointer->value);
  return real(value->real_double());
}

cons_t* proc_retval_to_u8vector(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_pointer(tag_ffi_retval, car(p));
  value_t* value = static_cast<value_t*>(car(p)->pointer->value);
  return bytevector(value->size, static_cast<uint8_t*>(value->data));
}

cons_t* proc_size_of(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(SYMBOL, car(p));

  const std::string name = symbol_name(car(p));

  for ( size_t n=0; type_sizes[n].name != NULL; ++n)
    if ( name == type_sizes[n].name )
      return integer(type_sizes[n].size);

  raise(runtime_exception(format(
    "Unknown type specifier: %s", name.c_str())));

  return nil(); // appease compiler
}

/*
 * (make-type (<type1> <type2>) size alignment)
 */
cons_t* proc_make_type(cons_t* p, environment_t*)
{
  cons_t *types = car(p),
          *size = cadr(p),
         *align = caddr(p);

  assert_length(p, 3);
  assert_type(PAIR, types);
  assert_type(INTEGER, size);
  assert_type(INTEGER, align);

  const size_t ntypes = length(types);

  if ( ntypes == 0 )
    raise(runtime_exception("No types given"));

  ffi_type *t = new ffi_type();
  t->size      = size->number.integer;
  t->alignment = align->number.integer;
  t->elements  = new ffi_type*[1+ntypes];
  t->elements[ntypes] = NULL;

  p = types;
  for ( size_t n=0; n<ntypes; ++n ) {
    t->elements[n] = parse_ffi_type(car(p));
    p = cdr(p);
  }

  return pointer(tag_ffi_type, t);
}

}; // extern "C"
