#ifndef _HISTERT_H
#define _HISTERT_H

#include <string>

#include "SubsysReco.h"

class PHCompositeNode;

class QAErt: public SubsysReco //++CINT
{
   static const std::string TRIG_NAME_BBC;
   static const std::string TRIG_NAME_4X4A;
   static const std::string TRIG_NAME_4X4B;
   static const std::string TRIG_NAME_4X4C;
   static const std::string TRIG_NAME_2X2;
   static const std::string TRIG_NAME_E;

 public:
   QAErt(const char *name = "QAErt"): SubsysReco(name) {}
   virtual ~QAErt() {}
   
   int InitRun(PHCompositeNode *topNode);
   int process_event(PHCompositeNode *topNode);
   int GetHalfSectorNum(int arm, int sect, int sm, int trig_mode);
   int GetSectSM(int arm, int sector, int sm, int is_rich);
   int CheckSectSM();

 private:
   void processRICH(PHCompositeNode *topNode);
   void processEmCal(PHCompositeNode *topNode);
   int getIDpmtAssoc(int emc_arm, float cross_phi, float cross_z);
   float xcrk[5120];
   float ycrk[5120];
   float zcrk[5120];

};

#endif /* _HISTERT_H */

//EOF
