#ifndef __DCRKFEM_H__
#define __DCRKFEM_H__


typedef struct {
   unsigned short DAV1;
   unsigned short detid;
   unsigned short evno;
   unsigned short module;
   unsigned short flag;
   unsigned short clock;
   unsigned short tac_cell;
   unsigned short pre_cell;
   unsigned short post_cell;
   unsigned short data[480];
   unsigned short usr[8];
   unsigned short parity;
   unsigned short CAV2;
} DCRKFEM_ST;
#endif /*__DCRKFEM_H__*/
