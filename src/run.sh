function run() {
  $srcdir/mickey \
    -L$srcdir/../lib \
    -I$srcdir/../tests \
    $srcdir/../tests/$1 \
    || exit 1
}
