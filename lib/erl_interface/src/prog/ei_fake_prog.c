/***************************************************************************
 *
 *  This is a fake program that contains all functions, variables and
 *  defined symbols mentioned in the manual. We compile this file to see
 *  that the header files and created library is complete.
 *
 *  You can't run this program, it is for compiling and linking only.
 *
 ***************************************************************************/

/* This is a link and header file test. Including "ei.h" and linking
   with libei.a should be enough */

/* Use most of
 * CFLAGS="-I../include -g -O2
 *         -ansi -pedantic
 *         -Wall
 *         -Wshadow
 *         -Wstrict-prototypes
 *         -Wmissing-prototypes
 *         -Wmissing-declarations
 *         -Wnested-externs
 *         -Winline
 *         -Werror"
 */

#include "config.h"

#if defined(HAVE_GMP_H) && defined(HAVE_LIBGMP)
#include <gmp.h>
#endif /* HAVE_GMP_H && HAVE_LIBGMP */

#include "ei.h"

int main(void)
{
  ei_cnode xec;
  char* charp = "foo";
  Erl_IpAddr thisipaddr = (Erl_IpAddr)0;
  ei_x_buff eix;
  erlang_msg emsg;
  short creation = 0;
  int fd = 0;
  int bufsize = 0;
  unsigned char * ucharp = (unsigned char *)"foo";
  erlang_pid epid;
  int len = 0;
  int timeout = 0;
  int port = 0;
  ErlConnect conp;
  int index = 0;
  int arity = 0;
  FILE *fp = (FILE *)0;
  long longx = 0;
  int *intp = NULL;
  unsigned long ulongx = 0;
  double doublex = 0.0;
  erlang_fun efun;
  erlang_port eport;
  erlang_ref eref;
  erlang_trace etrace;
  ei_term eterm;
  ei_reg *ei_regp = NULL;
  struct ei_reg_stat *ei_reg_statp = NULL;
  struct ei_reg_tabstat *ei_reg_tabstatp = NULL;
  void *voidp = NULL;

  int intx = erl_errno;

  ei_connect_init(&xec, charp, charp, creation);
  ei_connect_xinit (&xec, charp, charp, charp, thisipaddr, charp, creation);

  ei_connect(&xec, charp);
  ei_xconnect (&xec, thisipaddr, charp);

  ei_receive(fd, ucharp, bufsize);
  ei_receive_msg(fd, &emsg, &eix);
  ei_xreceive_msg(fd, &emsg, &eix);

  ei_send(fd, &epid, charp, len);
  ei_reg_send(&xec, fd, charp, charp, len);

  ei_rpc(&xec, fd, charp, charp, charp, len, &eix);
  ei_rpc_to(&xec, fd, charp, charp, charp, len);

  ei_rpc_from(&xec, fd, timeout, &emsg, &eix);

  ei_publish(&xec, port);
  ei_accept(&xec, fd, &conp);
  ei_unpublish(&xec);

  ei_thisnodename(&xec);
  ei_thishostname(&xec);
  ei_thisalivename(&xec);

  ei_self(&xec);

  ei_encode_version(charp, &index);
  ei_x_encode_version(&eix);
  ei_encode_long(charp, &index, longx);
  ei_x_encode_long(&eix, longx);
  ei_encode_ulong(charp, &index, ulongx);
  ei_x_encode_ulong(&eix, ulongx);
  ei_encode_double(charp, &index, doublex);
  ei_x_encode_double(&eix, doublex);
  ei_encode_boolean(charp, &index, intx);
/* FIXME no ei_x_encode_boolean */
  ei_encode_char(charp, &index, 'a');
/* FIXME no ei_x_encode_char */
  ei_encode_string(charp, &index, charp);
  ei_encode_string_len(charp, &index, charp, len);
  ei_x_encode_string(&eix, charp);
  ei_x_encode_string_len(&eix, charp, len);
  ei_encode_atom(charp, &index, charp);
  ei_encode_atom_len(charp, &index, charp, len);
  ei_x_encode_atom(&eix, charp);
  ei_x_encode_atom_len(&eix, charp, len);
  ei_encode_binary(charp, &index, (void *)0, longx);
  ei_x_encode_binary(&eix, (void*)0, len);
  ei_encode_pid(charp, &index, &epid);
  ei_x_encode_pid(&eix, &epid);
  ei_encode_fun(charp, &index, &efun);
  ei_x_encode_fun(&eix, &efun);
  ei_encode_port(charp, &index, &eport);
/* FIXME no ei_x_encode_port */
  ei_encode_ref(charp, &index, &eref);
/* FIXME no ei_x_encode_ref */
  ei_encode_trace(charp, &index, &etrace);
/* FIXME no ei_x_encode_trace */
  ei_encode_tuple_header(charp, &index, arity);
  ei_x_encode_tuple_header(&eix, longx);
  ei_encode_list_header(charp, &index, arity);
  ei_x_encode_list_header(&eix, longx);
/* #define ei_encode_empty_list(buf,i) ei_encode_list_header(buf,i,0) */
  ei_x_encode_empty_list(&eix);
  ei_get_type(charp, &index, &intx, &intx);
  ei_decode_version(charp, &index, &intx);
  ei_decode_long(charp, &index, &longx);
  ei_decode_ulong(charp, &index, &ulongx);
  ei_decode_double(charp, &index, &doublex);
  ei_decode_boolean(charp, &index, &intx);
  ei_decode_char(charp, &index, charp);
  ei_decode_string(charp, &index, charp);
  ei_decode_atom(charp, &index, charp);
  ei_decode_binary(charp, &index, (void *)0, &longx);
  ei_decode_fun(charp, &index, &efun);
  free_fun(&efun);
  ei_decode_pid(charp, &index, &epid);
  ei_decode_port(charp, &index, &eport);
  ei_decode_ref(charp, &index, &eref);
  ei_decode_trace(charp, &index, &etrace);
  ei_decode_tuple_header(charp, &index, &arity);
  ei_decode_list_header(charp, &index, &arity);
  ei_decode_ei_term(charp, &index, &eterm);
  ei_print_term(fp, charp, &index);
  ei_s_print_term(&charp, charp, &index);
  ei_x_new(&eix);
  ei_x_new_with_version(&eix);
  ei_x_free(&eix);
  ei_x_append(&eix, &eix);
  ei_x_append_buf(&eix, charp, len);
  ei_skip_term(charp, &index);
  ei_x_format(&eix, charp);
  ei_x_format_wo_ver(&eix, charp);

  ei_reg_open(intx);
  ei_reg_resize(ei_regp, intx);
  ei_reg_close(ei_regp);
  ei_reg_setival(ei_regp, charp, longx);
  ei_reg_setfval(ei_regp, charp, doublex);
  ei_reg_setsval(ei_regp, charp, charp);
  ei_reg_setpval(ei_regp, charp, voidp, intx);
  ei_reg_setval(ei_regp, charp, intx);
  ei_reg_getival(ei_regp, charp);
  ei_reg_getfval(ei_regp, charp);
  ei_reg_getsval(ei_regp, charp);
  ei_reg_getpval(ei_regp, charp, intp);
  ei_reg_getval(ei_regp, charp, intx);
  ei_reg_markdirty(ei_regp, charp);
  ei_reg_delete(ei_regp, charp);
  ei_reg_stat(ei_regp, charp, ei_reg_statp);
  ei_reg_tabstat(ei_regp, ei_reg_tabstatp);
  ei_reg_dump(intx, ei_regp, charp, intx);
  ei_reg_restore(intx, ei_regp, charp);
  ei_reg_purge(ei_regp);

#if defined(HAVE_GMP_H) && defined(HAVE_LIBGMP)
  {
      mpz_t obj;
      ei_decode_bignum(charp, &index, obj);
      ei_encode_bignum(charp, &index, obj);
      ei_x_encode_bignum(&eix, obj);
  }
#endif /* HAVE_GMP_H && HAVE_LIBGMP */

  return
      BUFSIZ +
      EAGAIN +
      EHOSTUNREACH +
      EIO +
      EI_BIN +
      EI_DELET +
      EI_DIRTY +
      EI_FLT +
      EI_FORCE +
      EI_INT +
      EI_NOPURGE +
      EI_STR +
      EMSGSIZE +
      ENOMEM +
      ERL_ERROR +
      ERL_EXIT +
      ERL_LINK +
      ERL_MSG +
      ERL_NO_TIMEOUT +
      ERL_REG_SEND +
      ERL_SEND +
      ERL_TICK +
      ERL_TIMEOUT +
      ERL_UNLINK +
      ETIMEDOUT +
      MAXATOMLEN;
}
