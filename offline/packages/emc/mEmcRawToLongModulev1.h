#ifndef __mEmcRawToLongModulev1_h__
#define __mEmcRawToLongModulev1_h__

#include <SubsysReco.h>
#include <string>

class dEmcRawDataWrapper;
class dEmcDCMLongDataWrapper;

/** (STAF) Converts dEmcRawData into dEmcDCMLongData. */

class mEmcRawToLongModulev1 : public SubsysReco
{
public:
  mEmcRawToLongModulev1();
  virtual ~mEmcRawToLongModulev1();
  int process_event(PHCompositeNode *);

private:
  int process_event(PHCompositeNode *root,
		  const char* dEmcRawNodeName,
		  const char* dEmcDCMLongNodeName);

  bool doit(const dEmcRawDataWrapper&, dEmcDCMLongDataWrapper&);

private:
  std::string fEmcRawNodeName;
  std::string fEmcDCMLongNodeName;
};
#endif /*__MEMCRAWTOLONGMODULE_H__*/
