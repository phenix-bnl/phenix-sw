/* table_header.h */

#ifndef TABLE_HEADER_H
#define TABLE_HEADER_H

typedef struct  {
   char name[20];	/* table name */
   char type[20];	/* table type */
   long maxlen;		/* # rows allocated */
   long nok;		/* # rows filled */
   long rbytes;		/* number of bytes per row */
   long dsl_pointer;	/* swizzled (DS_DATASET_T*) */
   long data_pointer;	/* swizzled (char*) */
}TABLE_HEAD_ST;

typedef TABLE_HEAD_ST table_head_st;
#endif /*TABLE_HEADER_H*/

