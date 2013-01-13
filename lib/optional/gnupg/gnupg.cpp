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

    if ( key->subkeys ) {
      gpgme_subkey_t sub = key->subkeys;
      cons_t *skeys = list();
      while ( sub != NULL ) {
        cons_t *skey = nil();
        skey = cons(list(symbol("secret?"), boolean(sub->secret)), skey);
        skey = cons(list(symbol("cardkey?"), boolean(sub->is_cardkey)), skey);
        if ( sub->card_number )
          skey = cons(list(symbol("card-number"), string(sub->card_number)), skey);
        skey = cons(list(symbol("expires"), sub->expires==0? string("never") : integer(sub->expires)), skey);
        skey = cons(list(symbol("timestamp"), integer(sub->timestamp)), skey);
        skey = cons(list(symbol("fingerprint"), string(sub->fpr)), skey);
        skey = cons(list(symbol("length"), integer(sub->length)), skey);
        skey = cons(list(symbol("keyid"), string(sub->keyid)), skey);

        skeys = append(list(skey), skeys);
        sub = sub->next;
      }

      k = cons(list(symbol("subkeys"), skeys), k);
    }

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

      cons_t *us = list();

      while ( uids != NULL ) {
        cons_t *u = nil();

        if ( uids->comment && strlen(uids->comment) )
          u = cons(list(symbol("comment"), string(uids->comment)), u);

        if ( uids->email && strlen(uids->email) )
          u = cons(list(symbol("email"), string(uids->email)), u);

        if ( uids->name && strlen(uids->name) )
          u = cons(list(symbol("name"), string(uids->name)), u);

        us = append(u, us);
        uids = uids->next;
      }

      k = cons(list(symbol("UIDs"), us), k);
    }

    keys = append(keys, list(k));
    gpgme_key_release(key);
  }

  gpgme_release(c);
  return keys;
}
