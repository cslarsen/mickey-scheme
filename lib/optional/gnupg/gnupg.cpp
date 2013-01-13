/*
 * GPGME (GnuPG Made Easy) bindings for Mickey Scheme
 *
 * Copyright (C) 2013 Christian Stigen Larsen
 * Distributed under any of LGPL v2.1, LGPL 3.0, GPL 2.0 or GPL 3.0
 *
 */

#include <gpgme.h>
#include <mickey.h>

static const char* version = NULL;

/*
 * GPGME authors have bundled their initialization code with the check
 * version function, so we need to call it.
 */
static void assert_gpgme_initialized(const char* required_version = NULL)
{
  if ( !version )
    version = gpgme_check_version(required_version);
}

static gpgme_error_t check(const gpgme_error_t& err)
{
  if ( err != GPG_ERR_NO_ERROR )
    raise(runtime_exception(strdup(gpgme_strerror(err))));

  return err;
}

extern "C" cons_t* proc_version(cons_t* p, environment_t*)
{
  assert_length(p, 0);
  assert_gpgme_initialized();
  return string(version);
}

/*
 * (public-keys <pattern> <secret-only?>) ==> keys with given pattern
 * (public-keys) ==> all keys
 */
extern "C" cons_t* proc_public_keys(cons_t* p, environment_t*)
{
  assert_length(p, 0, 2);
  assert_gpgme_initialized();

  gpgme_ctx_t c;
  check(gpgme_new(&c));

  // list ALL keys
  cons_t *keys = list();

  const char* pattern = NULL;
  int secret_only = 0;

  if ( length(p) > 0 ) {
    assert_type(STRING, car(p));
    pattern = car(p)->string;

    if ( length(p) > 1 )
      secret_only = !boolean_false(cadr(p));
  }

  gpgme_error_t res = gpgme_op_keylist_start(c, pattern, secret_only);

  while ( !res ) {
    gpgme_key_t key;
    res = gpgme_op_keylist_next(c, &key);

    // invalid context pointer
    if ( res == GPG_ERR_INV_VALUE ) {
      check(res);
      break;
    }

    // no more keys
    if ( res == GPG_ERR_EOF )
      break;

    cons_t *k = nil();

    if ( !key ) continue;

    k = cons(list(symbol("revoked"), boolean(key->revoked)), k);
    k = cons(list(symbol("expired"), boolean(key->expired)), k);
    k = cons(list(symbol("disabled"), boolean(key->disabled)), k);
    k = cons(list(symbol("invalid"), boolean(key->invalid)), k);
    k = cons(list(symbol("can-encrypt"), boolean(key->can_encrypt)), k);
    k = cons(list(symbol("can-sign"), boolean(key->can_sign)), k);
    k = cons(list(symbol("can-certify"), boolean(key->can_certify)), k);
    k = cons(list(symbol("secret"), boolean(key->secret)), k);
    k = cons(list(symbol("can-authenticate"), boolean(key->can_authenticate)), k);
    k = cons(list(symbol("german-law-sign-qualified"),
             boolean(key->is_qualified)), k);

    if ( key->uids ) {
      gpgme_user_id_t uids = key->uids;

      while ( uids != NULL ) {
        if ( uids->comment && strlen(uids->comment) )
          k = cons(list(symbol("comment"), string(uids->comment)), k);

        if ( uids->email && strlen(uids->email) )
          k = cons(list(symbol("email"), string(uids->email)), k);

        if ( uids->name && strlen(uids->name) )
          k = cons(list(symbol("name"), string(uids->name)), k);

        uids = uids->next;
      }
    }

    if ( key->subkeys ) {
      k = cons(list(symbol("keyid"), string(key->subkeys->keyid)), k);
    }

    keys = append(keys, list(k));
    gpgme_key_release(key);
  }

  gpgme_release(c);
  return keys;
}
