#ifndef __ERL_NMGC_H__
#define __ERL_NMGC_H__

#ifdef NOMOVE
/*
 * A non-moving garbage collector for the message area.
 */
#include <stddef.h>      /* offsetof() */

#define NM_NoPAGES  512
#define NM_PAGESIZE 32768
#define NM_ROOTSAVE 16384
#define NM_FULLPAGE (NM_PAGESIZE + offsetof(NM_Page,start) / sizeof(void*))
#define NM_STORAGE_SIZE 1024


#define BOXED_NEED(PTR,HDR)                                             \
  (((HDR) & _HEADER_SUBTAG_MASK) == SUB_BINARY_SUBTAG ?                 \
    header_arity(HDR) + 2 :                                             \
   ((HDR) & _HEADER_SUBTAG_MASK) == FUN_SUBTAG ?                        \
    header_arity(HDR) + ((ErlFunThing*)(PTR))->num_free + 2 :           \
   header_arity(HDR) + 1)


#ifdef INCREMENTAL_GC
#  define NM_MARK_FORWARD(ptr,dst) fwdptrs[(ptr) - inc_n2] = (dst);
#  define NM_IS_FORWARDED(ptr) (fwdptrs[(ptr) - inc_n2] != 0)
#  define NM_FORWARD_VALUE(ptr) fwdptrs[(ptr) - inc_n2]

/* Note for BM_TIMER: Active timer should always be 'system' when IncAlloc
 * is called!
 */
#define IncAlloc(p, sz, objv, nobj)                                     \
    (ASSERT_EXPR((sz) >= 0),                                            \
     (((inc_alloc_limit - global_htop) <= (sz)) ?                       \
      erts_inc_gc((p),(sz),(objv),(nobj)) : 0),                         \
     ASSERT_EXPR(global_hend - global_htop > (sz)),                     \
     global_htop += (sz), global_htop - (sz))

//     (global_hend - global_htop > (sz)) ? 0 : printf("Nu går det åt helvete!\r\n"),                     
// printf("global_hend: 0x%08x   htop: 0x%08x  limit: 0x%08x  sz: %d\r\n",global_hend, global_htop, inc_alloc_limit,sz), 

#else
#  define NM_MARK_FORWARD(ptr) fwdptrs[(ptr) - global_heap] = 1;
#  define NM_IS_FORWARDED(ptr) (fwdptrs[(ptr) - global_heap] != 0)
#  define NM_FORWARD_VALUE(ptr) fwdptrs[(ptr) - global_heap]
#endif


/*
 * These queues sould really be trees that do not store duplicates and
 * sort the stuff in address order (address = key) AAtree ?
 */

#define NM_STORAGE_DECLARATION(ext,name)                                \
    ext NM_Storage *name##head;                                         \
    ext NM_Storage *name##tail;                                         \
    ext NM_Object *name##free;                                          \
    ext NM_Object *name##last_free;                                     \
    ext int name##size;

#define NM_STORAGE_ITERATOR(name)                                       \
    NM_Storage *name##iterator_head = name##tail;                       \
    NM_Object *name##iterator_current = name##last_free;                \
    int name##iterator_left = name##size;

#define NM_STORAGE_INIT(name) do {                                      \
    name##head = (NM_Storage*)malloc(sizeof(NM_Storage));               \
    name##head->next = name##head;                                      \
    name##tail = name##head;                                            \
    name##free = name##head->data;                                      \
    name##last_free = name##free + NM_STORAGE_SIZE - 1;                 \
    name##size = 0;                                                     \
} while(0)

#define NM_STORAGE_SWAP(s1,s2) do {                                     \
    NM_Storage *tmphead = s1##head;                                     \
    NM_Storage *tmptail = s1##tail;                                     \
    NM_Object *tmpfree = s1##free;                                      \
    NM_Object *tmplast = s1##last_free;                                 \
    int tmpsize = s1##size;                                             \
    s1##head = s2##head;                                                \
    s1##tail = s2##tail;                                                \
    s1##free = s2##free;                                                \
    s1##last_free = s2##last_free;                                      \
    s1##size = s2##size;                                                \
    s2##head = tmphead;                                                 \
    s2##tail = tmptail;                                                 \
    s2##free = tmpfree;                                                 \
    s2##last_free = tmplast;                                            \
    s2##size = tmpsize;                                                 \
} while(0)

#define NM_STORAGE_TOP(name) (name##size == 0 ? 0 : name##last_free)

#define NM_STORAGE_POP(name) do {                                       \
    ASSERT(name##size != 0);                                            \
    name##size--;                                                       \
    if (++name##last_free == name##tail->data + NM_STORAGE_SIZE) {      \
        name##tail = name##tail->next;                                  \
        name##last_free = name##tail->data;                             \
    }                                                                   \
} while(0)

#define NM_STORAGE_GET(name) ((name##size == 0 ? 0 : (name##size--,     \
        (++name##last_free != name##tail->data + NM_STORAGE_SIZE) ?     \
        name##last_free : (name##tail = name##tail->next,               \
                           name##last_free = name##tail->data))))

#define NM_STORAGE_STEP(name) ((name##iterator_left == 0 ? 0 :          \
    (name##iterator_left--,                                             \
     (++name##iterator_current != name##iterator_head->data +           \
        NM_STORAGE_SIZE) ? name##iterator_current :                     \
          (name##iterator_head = name##iterator_head->next,             \
             name##iterator_current = name##iterator_head->data))))

#define NM_STORAGE_NEXT(name) do {                                      \
    if (name##free != name##last_free) {                                \
      name##free++;                                                     \
      if (name##free == name##head->data + NM_STORAGE_SIZE) {           \
        name##head = name##head->next;                                  \
        name##free = name##head->data;                                  \
      }                                                                 \
    } else {                                                            \
      name##free++;                                                     \
      name##tail = (NM_Storage*)malloc(sizeof(NM_Storage));             \
      memcpy(name##tail->data,name##head->data,                         \
             NM_STORAGE_SIZE * sizeof(NM_Object));                      \
      name##tail->next = name##head->next;                              \
      name##head->next = name##tail;                                    \
      name##last_free = ((void*)name##tail +                            \
                         ((void*)name##last_free - (void*)name##head)); \
    }                                                                   \
    name##size++;                                                       \
    if (name##free == name##head->data + NM_STORAGE_SIZE) {             \
      name##head = name##head->next;                                    \
      name##free = name##head->data;                                    \
    }                                                                   \
} while(0)

#define NM_STORAGE_HEAD(name) (name##free)

#define NM_STORAGE_EMPTY(name) (name##size == 0)

#define NM_STORE(name,ptr,sz) do {                                      \
    NM_STORAGE_HEAD(name)->this = ptr;                                  \
    NM_STORAGE_HEAD(name)->size = sz;                                   \
    NM_STORAGE_NEXT(name);                                              \
} while(0)

/*
 * Structures used by the non-moving memory manager
 */

typedef struct nm_object
{
  Eterm *this;
  int size;
  //struct nm_object *prev;
  //struct nm_object *next;
} NM_Object;

typedef struct nm_storage {
  struct nm_storage *next;
  NM_Object data[NM_STORAGE_SIZE];
} NM_Storage;

typedef struct nm_mem_block
{
  int size;
  struct nm_mem_block *prev;
  struct nm_mem_block *next;
} NM_MemBlock;

typedef struct nm_page
{
  struct nm_page *prev;  /* Used only in the used lists, not in bibop */
  struct nm_page *next;
  Eterm start[1]; /* Has to be last in struct, this is where the data start */
} NM_Page;


/*
 * Heap pointers for the non-moving memory area.
 */
extern Eterm *nm_heap;
extern Eterm *nm_hend;
extern NM_Page *nm_used_mem;
extern char *blackmap;

#ifdef INCREMENTAL_GC
extern Eterm **fwdptrs;
extern Eterm *inc_n2;
extern Eterm *inc_n2_end;
extern Process *inc_active_proc;
extern Process *inc_active_last;
extern Eterm *inc_alloc_limit;
#else
extern char *fwdptrs;
#endif

NM_STORAGE_DECLARATION(extern,gray);
NM_STORAGE_DECLARATION(extern,blue);
NM_STORAGE_DECLARATION(extern,root);
NM_STORAGE_DECLARATION(extern,build);

#ifdef INCREMENTAL_GC
void erts_init_incgc(void);
void erts_cleanup_incgc(void);
void erts_inc_gc(Process *p, int sz, Eterm* objv, int nobj);
#endif

void erts_init_nmgc(void);
void erts_cleanup_nmgc(void);
void erts_nm_copymark(Process*, Eterm*, int);
Eterm *erts_nm_alloc(Process*, int, Eterm*, int);

#ifdef DEBUG
void print_nm_heap(void);
void print_nm_free_list(void);
#endif

#else
#  define NM_STORE(lst,ptr,sz)  ;
#  define NM_MARK_FORWARD(ptr)  ;
#  define NM_IS_FORWARDED(ptr)  ;
#  define NM_FORWARD_VALUE(ptr) ;
#endif /* NOMOVE */

#endif /* _ERL_NMGC_H_ */
