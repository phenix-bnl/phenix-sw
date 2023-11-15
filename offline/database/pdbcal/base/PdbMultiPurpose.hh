#ifndef __PDBMULTIPURPOSE_HH__
#define __PDBMULTIPURPOSE_HH__

// Author:Federica Ceretto

#include "PdbParChan1F.hh"

class PdbMultiPurpose : public PdbParChan1F {
private:
  float fValue[5];  
  float fValueError[5];

public:
  enum { a=0, b=1, c=2, d=3, e=4, fNdim=5};

  PdbMultiPurpose();
  PdbMultiPurpose(const unsigned int);
  PdbMultiPurpose(const PdbMultiPurpose &);

  virtual ~PdbMultiPurpose();

  unsigned int GetNdim() const;
  float GetParameter(size_t) const;
  float GetParError(size_t) const;
  const char* GetParName(size_t) const;

  void SetParameter(size_t, float);
  void SetParError(size_t, float);
  virtual void Print() const;
  void Copy(PdbParChan1F *);

};

#endif /* __PDBMULTIPURPOSE_HH__ */
