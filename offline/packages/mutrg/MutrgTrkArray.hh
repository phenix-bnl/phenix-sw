#ifndef __MUTRGTRKARRAY__
#define __MUTRGTRKARRAY__

#include "PHObject.h"

#include <vector>

class MutrgTrk;

class MutrgTrkArray : public PHObject{
public:
  MutrgTrkArray(const char *mutrg_trk_class="");
  virtual ~MutrgTrkArray(void);
  virtual void Reset(void);
  virtual void Clear(Option_t* ="") { Reset(); }

  virtual MutrgTrk* Insert(void);
  virtual void Remove(MutrgTrk* trk);
  virtual void Remove(int itrk);
  virtual unsigned int GetSize(void);
  virtual MutrgTrk* Get(int itrk);

  virtual void print(std::ostream &os=std::cout) const;

protected:
  TClonesArray *mutrg_trks;

  ClassDef(MutrgTrkArray,1)
};

#endif /* __MUTRGTRKARRAY__ */
