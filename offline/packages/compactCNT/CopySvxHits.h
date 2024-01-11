#ifndef __COPYSVXHITS_H__
#define __COPYSVXHITS_H__

#include <SubsysReco.h>

class CopySvxHits: public SubsysReco
{
 public:
   CopySvxHits(const std::string &name = "COPYSVXHITS");
   virtual ~CopySvxHits() {}

   int InitRun(PHCompositeNode *topNode);
   int process_event(PHCompositeNode *topNode);
   int End(PHCompositeNode *topNode);

   void setCheckData(bool flag) { m_checkData=flag; }

 private:
   int checkData(PHCompositeNode *topNode);

 private:
   bool m_checkData;

};

#endif


