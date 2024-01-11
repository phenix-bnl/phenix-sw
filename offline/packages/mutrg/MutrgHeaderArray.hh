#ifndef __MUTRGHEADERARRAY__
#define __MUTRGHEADERARRAY__

#include "PHObject.h"

class MutrgHeader;

class MutrgHeaderArray : public PHObject{
public:
  MutrgHeaderArray(const char *mutrg_header_class="");
  virtual ~MutrgHeaderArray(void);
  virtual MutrgHeaderArray* Create(const char *mutrg_header_class="");
  virtual void Reset(void);
  virtual void Clear(Option_t* =0) { Reset(); }

  virtual MutrgHeader* Insert(void);
  virtual void Remove(MutrgHeader *header);
  virtual void Remove(int iheader);
  virtual unsigned int GetSize(void);
  virtual MutrgHeader* Get(int iheader);
  virtual const char* GetMutrgHeaderName(void);

protected:
  TClonesArray *mutrg_headers;

  ClassDef(MutrgHeaderArray,1)
};

#endif /* __MUTRGHEADERARRAY__ */
