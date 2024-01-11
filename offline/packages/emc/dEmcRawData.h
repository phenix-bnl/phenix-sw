#ifndef __DEMCRAWDATA_H__
#define __DEMCRAWDATA_H__


typedef struct {
   short id;
   int evno;
   int hwkey;
   int swkey;
   short type;
   short adclopre;
   short adclopost;
   short adchipre;
   short adchipost;
   short tdc;
} DEMCRAWDATA_ST;
#endif /*__DEMCRAWDATA_H__*/
