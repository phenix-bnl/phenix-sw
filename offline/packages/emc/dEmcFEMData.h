#ifndef __DEMCFEMDATA_H__
#define __DEMCFEMDATA_H__


typedef struct {
   short header;
   short detid;
   short evno;
   short module;
   short flag;
   short clock;
   short timecell;
   short precell;
   short postcell;
   short data[720];
   short userword[8];
   short longparity;
   short trailer;
} DEMCFEMDATA_ST;
#endif /*__DEMCFEMDATA_H__*/
