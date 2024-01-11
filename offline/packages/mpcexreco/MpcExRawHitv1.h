#ifndef MPCEXRAWHITV1_H
#define MPCEXRAWHITV1_H

#include "MpcExRawHit.h"

#include <vector>

class MpcExRawHitv1: public MpcExRawHit
{
 public:
  MpcExRawHitv1() {}
  virtual ~MpcExRawHitv1();
  void Reset();
  unsigned int fillfromvector(const std::vector<unsigned int> &vec);

  unsigned int getnhits() const {return mpcexrawhits.size();};
  unsigned int gethit(const unsigned int index) const;
  unsigned int getid(const unsigned int index) const;
  unsigned int getadcs(const unsigned int index) const;
  unsigned int getarm(const unsigned int index) const;
  unsigned int getpkt(const unsigned int index) const;
  unsigned int getchipmap(const unsigned int index) const;
  unsigned int getchain(const unsigned int index) const;
  unsigned int getchip(const unsigned int index) const;
  unsigned int getmicromodule(const unsigned int index) const;
  unsigned int getrocbond(const unsigned int index) const;
  unsigned int gethadc(const unsigned int index) const;
  unsigned int getladc(const unsigned int index) const;
  unsigned int getOnlineKey (const unsigned int index) const;
 protected:

  std::vector<unsigned int> mpcexrawhits;

 private:

  ClassDef(MpcExRawHitv1,1)

};

#endif
