/* ``The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved via the world wide web at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * The Initial Developer of the Original Code is Ericsson Utvecklings AB.
 * Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 * 
 *     $Id$
 */
#include <stdlib.h>
#include <stdio.h>
#include "erl_driver.h"


#define ASN1_OK 0
#define ASN1_ERROR -1
#define ASN1_COMPL_ERROR 1
#define ASN1_MEMORY_ERROR 0

#define CEIL(X,Y) ((X-1) / Y + 1)

typedef struct {
  ErlDrvPort port;
} asn1_data;


static ErlDrvData asn1_drv_start(ErlDrvPort, char *);

static void asn1_drv_stop(ErlDrvData);

int asn1_drv_control(ErlDrvData, unsigned int, char *, int, char **, int);

int complete(ErlDrvBinary **,unsigned char *,unsigned char *, int);

int insert_octets(int, unsigned char **, unsigned char **, int *);

int insert_octets_except_unused(int, unsigned char **, unsigned char **,
				int *, int);

int insert_octets_as_bits_exact_len(int, int, unsigned char **,
				    unsigned char **, int *);

int insert_octets_as_bits(int, unsigned char **, unsigned char **,int *);

int pad_bits(int, unsigned char **, int *);

int insert_least_sign_bits(int, unsigned char, unsigned char **, int *);

int insert_most_sign_bits(int, unsigned char, unsigned char **, int *);

int insert_bits_as_bits(int, int, unsigned char **, unsigned char **, int *);

int insert_octets_unaligned(int, unsigned char **, unsigned char **, int);

int realloc_memory(ErlDrvBinary **,int,unsigned char **,unsigned char **);


static ErlDrvEntry asn1_drv_entry = {
  NULL,              /* init, always NULL for dynamic drivers */
  asn1_drv_start,    /* start, called when port is opened */
  asn1_drv_stop,     /* stop, called when port is closed */
  NULL,              /* output, called when erlang has sent */
  NULL,              /* ready_input, called when input descriptor ready */
  NULL,              /* ready_output, called when output descriptor ready */
  "asn1_erl_drv",    /* char *driver_name, the argument to open_port */
  NULL,              /* finish, called when unloaded */
  NULL,              /* void * that is not used (BC) */
  asn1_drv_control,  /* control, port_control callback */
  NULL,              /* timeout, called on timeouts */
  NULL               /* outputv, vector output interface */
};    



DRIVER_INIT(asn1_erl_drv) /* must match name in driver_entry */
{
  return &asn1_drv_entry;
}

static ErlDrvData asn1_drv_start(ErlDrvPort port, char *buff)
{
  asn1_data* d = (asn1_data*)driver_alloc(sizeof(asn1_data));
  set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
  d->port = port;
  return (ErlDrvData)d;
}


static void asn1_drv_stop(ErlDrvData handle)
{
  driver_free((char*)handle);
}



int asn1_drv_control(ErlDrvData   handle, 
		     unsigned int command, 
		     char        *buf,
		     int          buf_len,
		     char       **res_buf,
		     int          res_buf_len) 
{
  char *complete_buf;
  int complete_len;
  ErlDrvBinary *drv_binary;
  asn1_data* a_data;

  if ((drv_binary = driver_alloc_binary(buf_len))==NULL) {
    /* error handling */
/*     printf("error when allocating memory\n"); */
    a_data = (asn1_data *)handle;
    set_port_control_flags(a_data->port, 0);
    return ASN1_MEMORY_ERROR;
  }
  complete_buf = drv_binary->orig_bytes;
  if ((complete_len = complete(&drv_binary,complete_buf,buf,buf_len)) == ASN1_ERROR)
    {
      /* error handling due to failure in complete */
/*       printf("error when running complete\n\r"); */
      driver_free_binary(drv_binary);
      a_data = (asn1_data *)handle;
      set_port_control_flags(a_data->port, 0);
      **res_buf = '1';
      return ASN1_COMPL_ERROR;
    }
  /* now the message is complete packed, return to Erlang */
  if (complete_len < buf_len) {
    ErlDrvBinary *tmp;
    if ((tmp=driver_realloc_binary(drv_binary,complete_len)) == NULL){
      /*error handling due to memory allocation failure */
/*       printf("error when allocating memory\n\r"); */
      driver_free_binary(drv_binary);
      a_data = (asn1_data *)handle;
      set_port_control_flags(a_data->port, 0);
      return ASN1_MEMORY_ERROR;
    }else
      drv_binary=tmp;
  }
  *res_buf = (char *)drv_binary;
  return complete_len;
}


int complete(ErlDrvBinary **drv_binary,unsigned char *complete_buf,
	     unsigned char *in_buf, int in_buf_len)
{
  int counter = in_buf_len;
  /* counter keeps track of number of bytes left in the
     input buffer */

  int buf_space = in_buf_len;
  /* This is the amount of allocated space left of the complete_buf. It
     is possible when padding is applied that more space is needed than
     was originally allocated. */

  int buf_size = in_buf_len;
  /* Size of the buffer. May become reallocated and thus other than 
     in_buf_len */

  unsigned char *in_ptr, *ptr;
  /* in_ptr points att the next byte in in_buf to be moved to
     complete_buf.
     ptr points into the new completed buffer, complete_buf, at the
     position of the next byte that will be set */
  int unused =  8;
  /* unused = [1,...,8] indicates how many of the rigthmost bits of 
     the byte that ptr points at that are unassigned */

  int no_bits,no_bytes,in_unused,desired_len,ret, saved_mem, needed, pad_bits;

  unsigned char val;
  
  in_ptr = in_buf;
  ptr = complete_buf;
  *ptr = 0x00;
  while(counter > 0) {
    counter--;
    switch (*in_ptr) {
    case 0: 
      /* just one zero-bit should be added to the buffer */
      if(unused == 1){
	unused = 8;
	*++ptr = 0x00;
	buf_space--;
      } else 
	unused--;
      break;

    case 1:
      /* one one-bit should be added to the buffer */
      if(unused == 1){
	*ptr = *ptr | 1;
	unused = 8;
	*++ptr = 0x00;
	buf_space--;
      } else {
	*ptr = *ptr | (1 << (unused - 1));
	unused--;
      }
      break;

    case 2:
      /* align buffer to end of byte */
      if (unused != 8) {
	*++ptr = 0x00;
	buf_space--;
	unused = 8;
      }
      break;

    case 10:
      /* next byte in in_buf tells how many bits in the second next byte
	 that will be used */
      /* The leftmost unused bits in the value byte are supposed to be
	 zero bits */
      no_bits =  (int)*(++in_ptr);
      val = *(++in_ptr);
      counter -= 2;
      if ((ret=insert_least_sign_bits(no_bits,val,&ptr,&unused)) == ASN1_ERROR)
	return ASN1_ERROR;
      buf_space -= ret;
      break;

    case 20:
      /* in this case the next value in_ptr points at holds the number
	 of following bytes that holds the value that will be inserted
	 in the completed buffer */
      no_bytes = (int)*(++in_ptr);
      counter -= (no_bytes + 1);
      if ((counter<0) || 
	  (ret=insert_octets(no_bytes,&in_ptr,&ptr,&unused)) == ASN1_ERROR)
	return ASN1_ERROR;
      buf_space -= ret;
      break;

    case 21:
      /* in this case the next two bytes in_ptr points at holds the number
	 of following bytes that holds the value that will be inserted
	 in the completed buffer */
      no_bytes = (int)*(++in_ptr);
      no_bytes = no_bytes << 8;
      no_bytes = no_bytes | (int)*(++in_ptr);
      counter -= (2 + no_bytes);
      if ((counter<0) || 
	  (ret=insert_octets(no_bytes,&in_ptr,&ptr,&unused)) == ASN1_ERROR)
	return ASN1_ERROR;
      buf_space -= ret;
      break;

    case 30:
      /* If we call the following bytes, in the buffer in_ptr points at,
	 By1,By2,Rest then Rest is the value that will be transfered to
	 the completed buffer. By1 tells how many of the rightmost bits in 
	 Rest that should not be used. By2 is the length of Rest in bytes.*/
      in_unused = (int)*(++in_ptr);
      no_bytes = (int)*(++in_ptr);
      counter -= (2 + no_bytes);
/*        printf("%d: case 30: in_unused=%d, no_bytes=%d,counter=%d\n\r",__LINE__,in_unused,no_bytes,counter); */
      ret = -4711;
      if ((counter<0) || 
	  (ret=insert_octets_except_unused(no_bytes,&in_ptr,&ptr,&unused,in_unused)) == ASN1_ERROR)
	return ASN1_ERROR;
/*        printf("%d: ret=%d\n\r",__LINE__, ret); */
      buf_space -= ret;
      break;

    case 31:
      /* If we call the following bytes, in the buffer in_ptr points at,
	 By1,By2,By3,Rest then Rest is the value that will be transfered to
	 the completed buffer. By1 tells how many of the rightmost bits in 
	 Rest that should not be used. By2 and By3 is the length of 
	 Rest in bytes.*/
      in_unused = (int)*(++in_ptr);
      no_bytes = (int)*(++in_ptr);
      no_bytes = no_bytes << 8;
      no_bytes = no_bytes | (int)*(++in_ptr);
      counter -= (3 + no_bytes);
      if ((counter<0) || 
	  (ret=insert_octets_except_unused(no_bytes,&in_ptr,&ptr,&unused,in_unused)) == ASN1_ERROR)
	return ASN1_ERROR;
      buf_space -= ret;
      break;

    case 40:
      /* This case implies that next byte,By1,(..,By1,By2,Bin,...)
	 is the desired length of the completed value, maybe needs 
	 padding zero bits or removal of trailing zero bits from Bin.
	 By2 is the length of Bin and Bin is the value that will be 
	 put into the completed buffer. Each byte in Bin has the value
	 1 or 0.*/
      desired_len = (int)*(++in_ptr);
      no_bytes=(int)*(++in_ptr);
 
      /* This is the algorithm for need of memory reallocation:
	 Only when padding (cases 40 - 43,45 - 47) more memory may be
	 used than allocated. Therefore one has to keep track of how
	 much of the allocated memory that has been saved, i.e. the 
	 difference between the number of parsed bytes of the input buffer
	 and the number of used bytes of the output buffer. 
	 If saved memory is less than needed for the padding then we
	 need more memory. */
      saved_mem = buf_space - counter;
      pad_bits = desired_len - no_bytes - unused;
      needed = (pad_bits > 0) ? CEIL(pad_bits,8) : 0;
      if (saved_mem < needed) {
	/* Have to allocate more memory */
	buf_size += needed;
	buf_space += needed;
	if (realloc_memory(drv_binary,buf_size,&ptr,
			   &complete_buf) == ASN1_ERROR)
	  return ASN1_ERROR;
      }

      counter -= (2 + no_bytes);
     if ((counter<0) || 
	  (ret=insert_octets_as_bits_exact_len(desired_len,no_bytes,&in_ptr,
					       &ptr,&unused)) == ASN1_ERROR)
	return ASN1_ERROR;
      buf_space -= ret;
      break;
      
    case 41:
      /* Same as case 40 apart from By2, the length of Bin, which is in 
	 two bytes*/
      desired_len = (int)*(++in_ptr);
      no_bytes=(int)*(++in_ptr);
      no_bytes = no_bytes << 8;
      no_bytes = no_bytes | (int)*(++in_ptr);

      saved_mem = buf_space - counter;
      needed = CEIL((desired_len-unused),8) - no_bytes;
      if (saved_mem < needed) {
	/* Have to allocate more memory */
	buf_size += needed;
	buf_space += needed;
	if (realloc_memory(drv_binary,buf_size,&ptr,
			   &complete_buf) == ASN1_ERROR)
	  return ASN1_ERROR;
      }

      counter -= (3 + no_bytes);
      if ((counter<0) || 
	  (ret=insert_octets_as_bits_exact_len(desired_len,no_bytes,&in_ptr,
					       &ptr,&unused)) == ASN1_ERROR)
	return ASN1_ERROR;
      buf_space -= ret;
      break;
      
    case 42:
      /* Same as case 40 apart from By1, the desired length, which is in 
	 two bytes*/
      desired_len = (int)*(++in_ptr);
      desired_len = desired_len << 8;
      desired_len = desired_len | (int)*(++in_ptr);
      no_bytes=(int)*(++in_ptr);

      saved_mem = buf_space - counter;
      needed = CEIL((desired_len-unused),8) - no_bytes;
      if (saved_mem < needed) {
	/* Have to allocate more memory */
	buf_size += needed;
	buf_space += needed;
	if (realloc_memory(drv_binary,buf_size,&ptr,
			   &complete_buf) == ASN1_ERROR)
	  return ASN1_ERROR;
      }

      counter -= (3 + no_bytes);
      if ((counter<0) || 
	  (ret=insert_octets_as_bits_exact_len(desired_len,no_bytes,&in_ptr,
					       &ptr,&unused)) == ASN1_ERROR)
	return ASN1_ERROR;
      buf_space -= ret;
      break;
      
    case 43:
      /* Same as case 40 apart from By1 and By2, the desired length and
	 the length of Bin, which are in two bytes each. */
      desired_len = (int)*(++in_ptr);
      desired_len = desired_len << 8;
      desired_len = desired_len | (int)*(++in_ptr);
      no_bytes=(int)*(++in_ptr);
      no_bytes = no_bytes << 8;
      no_bytes = no_bytes | (int)*(++in_ptr);

      saved_mem = buf_space - counter;
      needed = CEIL((desired_len-unused),8) - no_bytes;
      if (saved_mem < needed) {
	/* Have to allocate more memory */
	buf_size += needed;
	buf_space += needed;
	if (realloc_memory(drv_binary,buf_size,&ptr,
			   &complete_buf) == ASN1_ERROR)
	  return ASN1_ERROR;
      }

      counter -= (4 + no_bytes);
      if ((counter<0) || 
	  (ret=insert_octets_as_bits_exact_len(desired_len,no_bytes,&in_ptr,
					       &ptr,&unused)) == ASN1_ERROR)
	return ASN1_ERROR;
      buf_space -= ret;
      break;
      
    case 45:
      /* This case assumes that the following bytes in the incoming buffer
	 (called By1,By2,Bin) is By1, which is the number of bits (n) that 
	 will be inserted in the completed buffer. By2 is the number of
	 bytes in Bin. Each bit in the buffer Bin should be inserted from
	 the leftmost until the nth.*/
      desired_len = (int)*(++in_ptr);
      no_bytes=(int)*(++in_ptr);
 
      saved_mem = buf_space - counter;
      needed = CEIL((desired_len-unused),8) - no_bytes;
      if (saved_mem < needed) {
	/* Have to allocate more memory */
	buf_size += needed;
	buf_space += needed;
	if (realloc_memory(drv_binary,buf_size,&ptr,
			   &complete_buf) == ASN1_ERROR)
	  return ASN1_ERROR;
      }

      counter -= (2 + no_bytes);
      /* printf("calling insert_bits_as_bits: desired_len=%d, no_bytes=%d\n\r",desired_len,no_bytes);*/

      
      if((counter<0) || 
	 (ret=insert_bits_as_bits(desired_len,no_bytes,&in_ptr,
				  &ptr,&unused)) == ASN1_ERROR)
	return ASN1_ERROR;
      buf_space -= ret;
      break;
      
    case 46:
      /* Same as case 45 apart from By1, the desired length, which is
	 in two bytes. */
      desired_len = (int)*(++in_ptr);
      desired_len = desired_len << 8;
      desired_len = desired_len | (int)*(++in_ptr);
      no_bytes=(int)*(++in_ptr);
 
      saved_mem = buf_space - counter;
      needed = CEIL((desired_len-unused),8) - no_bytes;
      if (saved_mem < needed) {
	/* Have to allocate more memory */
	buf_size += needed;
	buf_space += needed;
	if (realloc_memory(drv_binary,buf_size,&ptr,
			   &complete_buf) == ASN1_ERROR)
	  return ASN1_ERROR;
      }

      counter -= (3 + no_bytes);
      if((counter<0) || 
	 (ret=insert_bits_as_bits(desired_len,no_bytes,&in_ptr,
				  &ptr,&unused)) == ASN1_ERROR)
	return ASN1_ERROR;
      buf_space -= ret;
      break;
      
    case 47:
      /* Same as case 45 apart from By1 and By2, the desired length
	 and the length of Bin, which are in two bytes each. */
      desired_len = (int)*(++in_ptr);
      desired_len = desired_len << 8;
      desired_len = desired_len | (int)*(++in_ptr);
      no_bytes=(int)*(++in_ptr);
      no_bytes = no_bytes << 8;
      no_bytes = no_bytes | (int)*(++in_ptr);
 
      saved_mem = buf_space - counter;
      needed = CEIL((desired_len-unused),8) - no_bytes;
      if (saved_mem < needed) {
	/* Have to allocate more memory */
	buf_size += needed;
	buf_space += needed;
	if (realloc_memory(drv_binary,buf_size,&ptr,
			   &complete_buf) == ASN1_ERROR)
	  return ASN1_ERROR;
      }

      counter -= (4 + no_bytes);
      if((counter<0) || 
	 (ret=insert_bits_as_bits(desired_len,no_bytes,&in_ptr,
				  &ptr,&unused)) == ASN1_ERROR)
	return ASN1_ERROR;
      buf_space -= ret;
      break;
      
    default:
      return ASN1_ERROR;
    }
    in_ptr++;
  }
  /* The returned buffer must be at least one byte and
     it must be octet aligned */
  if ((unused == 8) && (ptr != complete_buf)) 
    return (ptr - complete_buf);
  else {
    ptr++; /* octet align buffer */
    return (ptr - complete_buf);
  }
}


int realloc_memory(ErlDrvBinary **drv_binary,
		   int amount,
		   unsigned char **ptr,
		   unsigned char **complete_buf) {

  ErlDrvBinary *tmp_bin;
  int i;

  if ((tmp_bin=driver_realloc_binary(*drv_binary,amount)) == NULL) {
    /*error handling due to memory allocation failure */
/*     printf("error when allocating memory\n"); */
    return ASN1_ERROR;
  }else {
    i = *ptr - *complete_buf;
    *drv_binary=tmp_bin;
    *complete_buf = (*drv_binary)->orig_bytes;
    *ptr = *complete_buf + i;
  }
  return ASN1_OK;
}


int insert_most_sign_bits(int no_bits,
			  unsigned char val,
			  unsigned char **output_ptr,
			  int *unused) {
  unsigned char *ptr = *output_ptr;
  
  if (no_bits < *unused){
    *ptr = *ptr | (val >> (8 - *unused));
    *unused -= no_bits;
  } else if (no_bits == *unused) {
    *ptr = *ptr | (val >> (8 - *unused));
    *unused = 8;
    *++ptr = 0x00;
  } else {
    *ptr = *ptr | (val >> (8 - *unused));
    *++ptr = 0x00;
    *ptr = *ptr | (val << *unused);
    *unused = 8 - (no_bits - *unused);
  }
  *output_ptr = ptr;
  return ASN1_OK;
}


int insert_least_sign_bits(int no_bits,
			   unsigned char val,
			   unsigned char **output_ptr,
			   int *unused) {
  unsigned char *ptr = *output_ptr;
  int ret = 0;

  if (no_bits < *unused){
    *ptr = *ptr | (val << (*unused - no_bits));
    *unused -= no_bits;
  } else if (no_bits == *unused){
    *ptr = *ptr | val;
    *unused = 8;
    *++ptr = 0x00;
    ret++;
  } else {
    /* first in the begun byte in the completed buffer insert 
       so many bits that fit, then insert the rest in next byte.*/
    *ptr = *ptr | (val >> (no_bits - *unused));
    *++ptr = 0x00;
    ret++;
    *ptr = *ptr | (val << (8 - (no_bits - *unused)));
    *unused = 8 - (no_bits - *unused);
  }
  *output_ptr = ptr;
  return ret;
}

/* pad_bits adds no_bits bits in the buffer that output_ptr 
   points at.
 */
int pad_bits(int no_bits, unsigned char **output_ptr, int *unused) 
  {
    unsigned char *ptr = *output_ptr;
    int ret = 0;
    
    while (no_bits > 0) {
      if(*unused == 1){
	*unused = 8;
	*++ptr = 0x00;
	ret++;
      } else 
	(*unused)--;
      no_bits--;
    }
    *output_ptr = ptr;
    return ret;
  }


/* insert_bits_as_bits removes no_bytes bytes from the buffer that in_ptr
   points at and takes the desired_no leftmost bits from those removed
   bytes and inserts them in the buffer(output buffer) that ptr points at.
   The unused parameter tells how many bits that are not set in the 
   actual byte in the output buffer. If desired_no is more bits than the
   input buffer has in no_bytes bytes, then zero bits is padded.*/
int insert_bits_as_bits(int desired_no,
			int no_bytes,
			unsigned char **input_ptr,
			unsigned char **output_ptr,
			int *unused)
{
  unsigned char *in_ptr = *input_ptr;
  unsigned char val;
  int no_bits;

  if (desired_no == (no_bytes * 8)) {
    if(insert_octets_unaligned(no_bytes,&in_ptr,output_ptr,*unused)
       == ASN1_ERROR)
      return ASN1_ERROR;
  }
  else if (desired_no < (no_bytes * 8)) {
/*     printf("insert_bits_as_bits 1\n\r"); */
    if(insert_octets_unaligned(desired_no/8,&in_ptr,output_ptr,*unused)
       == ASN1_ERROR)
      return ASN1_ERROR;
/*     printf("insert_bits_as_bits 2\n\r"); */
    val = *++in_ptr;
/*     printf("val = %d\n\r",(int)val); */
    no_bits = desired_no % 8;
/*     printf("no_bits = %d\n\r",no_bits); */
    insert_most_sign_bits(no_bits,val,output_ptr,unused);
  }
  else {
    if(insert_octets_unaligned(no_bytes,&in_ptr,output_ptr,*unused) 
       == ASN1_ERROR)
      return ASN1_ERROR;
    pad_bits(desired_no - (no_bytes * 8),output_ptr,unused);
  }
/*   printf("*unused = %d\n\r",*unused); */
  *input_ptr = in_ptr;
  return ASN1_OK;
}


/* insert_octets_as_bits_exact_len */
int
insert_octets_as_bits_exact_len(int desired_len,
				int in_buff_len,
				unsigned char **in_ptr,
				unsigned char **ptr,
				int *unused)
{
  int ret = 0;
  int ret2 = 0;

  if (desired_len == in_buff_len) {
    if ((ret = insert_octets_as_bits(in_buff_len,in_ptr,ptr,unused)) == ASN1_ERROR)
      return ASN1_ERROR;
  }
  else if(desired_len > in_buff_len) {
    if((ret = insert_octets_as_bits(in_buff_len,in_ptr,ptr,unused)) == ASN1_ERROR)
      return ASN1_ERROR;
    /* now pad with zero bits */
/*     printf("~npad_bits: called with %d bits padding~n~n~r",desired_len - in_buff_len); */
    if ((ret2=pad_bits(desired_len - in_buff_len,ptr,unused)) == ASN1_ERROR)
      return ASN1_ERROR;
  }
  else {/* desired_len < no_bits */
    if ((ret=insert_octets_as_bits(desired_len,in_ptr,ptr,unused)) == ASN1_ERROR)
      return ASN1_ERROR;
    /* now remove no_bits - desired_len bytes from in buffer */
    *in_ptr += (in_buff_len - desired_len);
  }
  return (ret+ret2);
}



/* insert_octets_as_bits takes no_bytes bytes from the buffer that input_ptr
   points at and inserts the least significant bit of it in the buffer that
   output_ptr points at. Each byte in the input buffer must be 1 or 0
   otherwise the function returns ASN1_ERROR. The output buffer is concatenated
   without alignment.
 */
int insert_octets_as_bits(int no_bytes,
			  unsigned char **input_ptr,
			  unsigned char **output_ptr,
			  int *unused)
{
  unsigned char *in_ptr = *input_ptr;
  unsigned char *ptr = *output_ptr;
  int used_bits = 8 - *unused;

  while (no_bytes > 0) {
    switch (*++in_ptr) {
    case 0:
      if(*unused == 1){
	*unused = 8;
	*++ptr = 0x00;
      } else 
	(*unused)--;
      break;
    case 1:
      if(*unused == 1){
	*ptr = *ptr | 1;
	*unused = 8;
	*++ptr = 0x00;
      } else {
	*ptr = *ptr | (1 << (*unused - 1));
	(*unused)--;
      }
      break;
    default:
      return ASN1_ERROR;
    }
    no_bytes--;
  }
  *input_ptr = in_ptr;
  *output_ptr = ptr;
  return ((used_bits+no_bytes) / 8); /*return number of new bytes
				      in completed buffer */
}

/* insert_octets inserts bytes from the input buffer, *input_ptr,
   into the output buffer, *output_ptr. Before the first byte is
   inserted the input buffer is aligned.
 */
int insert_octets(int no_bytes,
		  unsigned char **input_ptr,
		  unsigned char **output_ptr,
		  int *unused)
{
    unsigned char *in_ptr = *input_ptr;
    unsigned char *ptr = *output_ptr;
    int ret = 0;
      
    if (*unused != 8) {/* must align before octets are added */
      *++ptr = 0x00;
      ret++;
      *unused = 8;
    }
    while(no_bytes > 0) {
      *ptr = *(++in_ptr);
      *++ptr = 0x00;
      /*      *unused = *unused - 1; */
      no_bytes--;
    }
  *input_ptr = in_ptr;
  *output_ptr = ptr;
  return (ret + no_bytes);
}

/* insert_octets_unaligned inserts bytes from the input buffer, *input_ptr,
   into the output buffer, *output_ptr.No alignment is done.
 */
int insert_octets_unaligned(int no_bytes,
			    unsigned char **input_ptr,
			    unsigned char **output_ptr,
			    int unused)
{
  unsigned char *in_ptr = *input_ptr;
  unsigned char *ptr = *output_ptr;
  int n = no_bytes;
  unsigned char val;
  
  while (n > 0) {
    if (unused == 8) {
      *ptr = *++in_ptr;
      *++ptr = 0x00;
    }else {
      val = *++in_ptr;
      *ptr =  *ptr | val >> (8 - unused);
      *++ptr = 0x00;
      *ptr = val << unused;
    }
    n--;
  }
  *input_ptr = in_ptr;
  *output_ptr = ptr;
  return no_bytes;
}


int insert_octets_except_unused(int no_bytes,
				unsigned char **input_ptr,
				unsigned char **output_ptr,
				int *unused,
				int in_unused)
{
  unsigned char *in_ptr = *input_ptr;
  unsigned char *ptr = *output_ptr;
  int val, no_bits;
  int ret = 0;
  
  if (in_unused == 0){
/*      printf("%d: insert_octets_except_unused: if\n\r",__LINE__); */
    if ((ret = insert_octets_unaligned(no_bytes,&in_ptr,&ptr,
				       *unused)) == ASN1_ERROR)
      return ASN1_ERROR;
  }
    else {
/*        printf("%d: insert_octets_except_unused: else\n\r",__LINE__); */
      if ((ret=insert_octets_unaligned(no_bytes - 1,&in_ptr,&ptr,*unused)) != ASN1_ERROR) {
	val = (int) *(++in_ptr);
	no_bits = 8 - in_unused;
	/* no_bits is always less than *unused since the buffer is
	   octet aligned after insert:octets call, so the following
	   if clasuse is obsolete I think */
	if(no_bits < *unused){
	  *ptr = *ptr | (val >> (8 - *unused));
	  *unused = *unused - no_bits;
	} else if (no_bits == *unused) {
	  *ptr = *ptr | (val >> (8 - *unused));
	  *++ptr = 0x00;
	  ret++;
	  *unused = 8;
	} else {
	  *ptr = *ptr | (val >> (8 - *unused));
	  *++ptr = 0x00;
	  ret++;
	  *ptr = *ptr | (val << *unused);
	  *unused = 8 - (no_bits - *unused);
	}
      } else
	return ASN1_ERROR;
    }
  *input_ptr = in_ptr;
  *output_ptr = ptr;
/*    printf("%d: insert_octets_except_unused: ret=%d\n\r",__LINE__,ret); */
  return ret;
}
