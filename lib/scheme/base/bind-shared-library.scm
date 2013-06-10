(open-internal-library "libscheme-base.so" 'lazy 'global)

(define * (bind-procedure "proc_mul"))
(define + (bind-procedure "proc_add"))
(define - (bind-procedure "proc_sub"))
(define / (bind-procedure "proc_div"))
(define < (bind-procedure "proc_less"))
(define = (bind-procedure "proc_eqnump"))
(define > (bind-procedure "proc_greater"))
(define abs (bind-procedure "proc_abs"))
(define and (bind-syntax "proc_dummy_placeholder"))
(define append (bind-procedure "proc_append"))
(define apply (bind-syntax "proc_dummy_placeholder"))
(define assoc (bind-procedure "proc_assoc"))
(define assq (bind-procedure "proc_assq"))
(define assv (bind-procedure "proc_assv"))
(define begin (bind-syntax "proc_dummy_placeholder"))
(define binary-port?  (bind-procedure "proc_binary_portp"))
(define boolean->string (bind-procedure "proc_boolean_to_string"))
(define boolean?  (bind-procedure "proc_booleanp"))
(define bytevector (bind-procedure "proc_bytevector"))
(define bytevector-copy (bind-procedure "proc_bytevector_copy"))
(define bytevector-copy!  (bind-procedure "proc_bytevector_copy_bang"))
(define bytevector-copy-partial (bind-procedure "proc_bytevector_copy_partial"))
(define bytevector-copy-partial!  (bind-procedure "proc_bytevector_copy_partial_bang"))
(define bytevector-length (bind-procedure "proc_bytevector_length"))
(define bytevector-u8-ref (bind-procedure "proc_bytevector_u8_ref"))
(define bytevector-u8-set!  (bind-procedure "proc_bytevector_u8_set_bang"))
(define bytevector?  (bind-procedure "proc_bytevectorp"))
(define car (bind-procedure "proc_car"))
(define case (bind-procedure "proc_case"))
(define cdr (bind-procedure "proc_cdr"))
(define char->integer (bind-procedure "proc_char_to_integer"))
(define char<=?  (bind-procedure "proc_char_ltep"))
(define char<?  (bind-procedure "proc_char_ltp"))
(define char=?  (bind-procedure "proc_char_eqp"))
(define char>=?  (bind-procedure "proc_char_gtep"))
(define char>?  (bind-procedure "proc_char_gtp"))
(define char?  (bind-procedure "proc_charp"))
(define close-input-port (bind-procedure "proc_close_input_port"))
(define close-output-port (bind-procedure "proc_close_output_port"))
(define close-port (bind-procedure "proc_close_port"))
(define cond (bind-syntax "proc_dummy_placeholder"))
(define cons (bind-procedure "proc_cons"))
(define current-error-port (bind-procedure "proc_current_error_port"))
(define current-input-port (bind-procedure "proc_current_input_port"))
(define current-output-port (bind-procedure "proc_current_output_port"))
(define define (bind-syntax "proc_dummy_placeholder"))
(define define-syntax (bind-syntax "proc_dummy_placeholder"))
(define do (bind-syntax "proc_dummy_placeholder"))
(define eof-object (bind-procedure "proc_eof_object"))
(define eof-object? (bind-procedure "proc_eof_objectp"))
(define eq?  (bind-procedure "proc_eqp"))
(define equal?  (bind-procedure "proc_equalp"))
(define eqv?  (bind-procedure "proc_eqvp"))
(define error (bind-procedure "proc_error"))
(define even?  (bind-procedure "proc_evenp"))
(define exact (bind-procedure "proc_exact"))
(define exact->inexact (bind-procedure "proc_exact_to_inexact"))
(define exact? (bind-procedure "proc_exactp"))
(define expt (bind-procedure "proc_expt"))
(define finite?  (bind-procedure "proc_finitep"))
(define gcd (bind-procedure "proc_gcd"))
(define if (bind-syntax "proc_dummy_placeholder"))
(define inexact (bind-procedure "proc_inexact"))
(define inexact? (bind-procedure "proc_inexactp"))
(define infinite?  (bind-procedure "proc_infinitep"))
(define input-port?  (bind-procedure "proc_input_portp"))
(define integer->char (bind-procedure "proc_integer_to_char"))
(define integer?  (bind-procedure "proc_integerp"))
(define lambda (bind-syntax "proc_dummy_placeholder"))
(define lcm (bind-procedure "proc_lcm"))
(define length (bind-procedure "proc_length"))
(define let (bind-syntax "proc_dummy_placeholder"))
(define let* (bind-syntax "proc_dummy_placeholder"))
(define letrec (bind-syntax "proc_dummy_placeholder"))
(define letrec* (bind-syntax "proc_dummy_placeholder"))
(define list (bind-procedure "proc_list"))
(define list->string (bind-procedure "proc_list_to_string"))
(define list->vector (bind-procedure "proc_list_to_vector"))
(define list-ref (bind-procedure "proc_list_ref"))
(define list-set! (bind-procedure "proc_list_set"))
(define list-tail (bind-procedure "proc_list_tail"))
(define list?  (bind-procedure "proc_listp"))
(define make-bytevector (bind-procedure "proc_make_bytevector"))
(define make-string (bind-procedure "proc_make_string"))
(define make-vector (bind-procedure "proc_make_vector"))
(define max (bind-procedure "proc_max"))
(define min (bind-procedure "proc_min"))
(define modulo (bind-procedure "proc_modulo"))
(define nan?  (bind-procedure "proc_nanp"))
(define newline (bind-procedure "proc_newline"))
(define not (bind-procedure "proc_not"))
(define null?  (bind-procedure "proc_nullp"))
(define number->string (bind-procedure "proc_number_to_string"))
(define odd?  (bind-procedure "proc_oddp"))
;(define or (bind-syntax "proc_or"))
(define output-port?  (bind-procedure "proc_output_portp"))
(define pair?  (bind-procedure "proc_pairp"))
(define peek-char (bind-procedure "proc_peek_char"))
(define port-open?  (bind-procedure "proc_port_openp"))
(define port?  (bind-procedure "proc_portp"))
(define procedure?  (bind-procedure "proc_procedurep"))
(define quasiquote (bind-syntax "proc_dummy_placeholder"))
(define quote (bind-syntax "proc_dummy_placeholder"))
(define rational?  (bind-procedure "proc_rationalp"))
(define read-line  (bind-procedure "proc_read_line"))
(define real?  (bind-procedure "proc_realp"))
(define reverse (bind-procedure "proc_reverse"))
(define round (bind-procedure "proc_round"))
(define set!  (bind-syntax "proc_dummy_placeholder"))
(define set-car!  (bind-syntax "proc_dummy_placeholder"))
(define set-cdr!  (bind-syntax "proc_dummy_placeholder"))
(define string (bind-procedure "proc_string"))
(define string->list (bind-procedure "proc_string_to_list"))
(define string->number (bind-procedure "proc_string_to_number"))
(define string->symbol (bind-procedure "proc_string_to_symbol"))
(define string->vector (bind-procedure "proc_string_to_vector"))
(define string-append (bind-procedure "proc_strcat"))
(define string-length (bind-procedure "proc_string_length"))
(define string-ref (bind-procedure "proc_string_ref"))
(define string<=?  (bind-procedure "proc_string_ltep"))
(define string<?  (bind-procedure "proc_string_ltp"))
(define string=?  (bind-procedure "proc_string_eqp"))
(define string>=?  (bind-procedure "proc_string_gtep"))
(define string>?  (bind-procedure "proc_string_gtp"))
(define string?  (bind-procedure "proc_stringp"))
(define substring (bind-procedure "proc_substring"))
(define symbol->string (bind-procedure "proc_symbol_to_string"))
(define symbol?  (bind-procedure "proc_symbolp"))
(define textual-port?  (bind-procedure "proc_textual_portp"))
(define truncate (bind-procedure "proc_truncate"))
(define vector (bind-procedure "proc_vector"))
(define vector->list (bind-procedure "proc_vector_to_list"))
(define vector->string (bind-procedure "proc_vector_to_string"))
(define vector-copy (bind-procedure "proc_vector_copy"))
(define vector-fill!  (bind-procedure "proc_vector_fill"))
(define vector-length (bind-procedure "proc_vector_length"))
(define vector-ref (bind-procedure "proc_vector_ref"))
(define vector-set!  (bind-procedure "proc_vector_set"))
(define vector?  (bind-procedure "proc_vectorp"))
(define write (bind-procedure "proc_write"))
(define xor (bind-procedure "proc_xor"))
