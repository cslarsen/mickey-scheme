/*
 * Mickey Scheme
 *
 * Copyright (C) 2011-2015 Christian Stigen Larsen <csl@csl.name>
 * http://csl.name
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *
 */

#ifndef INC_MICKEY_H
#define INC_MICKEY_H

#define MICKEY_VERSION_MAJOR 0
#define MICKEY_VERSION_MINOR 1

const char MICKEY_NAME[] = "Mickey Scheme";
const char MICKEY_COPYRIGHT[] = "(C) 2011-2015 Christian Stigen Larsen";

/*
 * The following files are REQUIRED to link to when making
 * dynamic Mickey Scheme libraries.
 *
 * TODO: Reduce this list as much as possible.
 */

#include "mickey/apply.h"
#include "mickey/arguments.h"
#include "mickey/assertions.h"
#include "mickey/backtrace.h"
#include "mickey/circular.h"
#include "mickey/cons.h"
#include "mickey/debug.h"
#include "mickey/eval.h"
#include "mickey/evlis.h"
#include "mickey/exceptions.h"
#include "mickey/file-io.h"
#include "mickey/garbage-collector.h"
#include "mickey/import.h"
#include "mickey/options.h"
#include "mickey/parser.h"
#include "mickey/primitives.h"
#include "mickey/print.h"
#include "mickey/syntax-rules.h"
#include "mickey/tokenizer.h"
#include "mickey/util.h"

#endif
