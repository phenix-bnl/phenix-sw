#ifndef __CORRDATAV1_H__
#define __CORRDATAV1_H__

#include <CorrData.h>
#include <iostream>

class CorrDataV1 : public CorrData
{
public:

  CorrDataV1();
  CorrDataV1(const CorrData&);
  virtual ~CorrDataV1() {}

  CorrData& operator=(const CorrData &rhs);

  Float_t get_bbcq() const { return bbcq; }
  virtual Short_t get_nhits(const int i) const { return nhits[i]; }

  void  set_bbcq(const Float_t q){ bbcq = q; }
  void  set_nhits(const int ch, const Short_t n) { nhits[ch] = n; }

  virtual void print(std::ostream&);

protected:
 
  Float_t bbcq;		//
  Int_t   nchips;
  Short_t *nhits;     //[nchips]

  ClassDef(CorrDataV1,1)
};

#endif /* __CORRDATAV1_H__ */

