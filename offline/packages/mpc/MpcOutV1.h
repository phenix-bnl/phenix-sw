#ifndef __MPCOUTV1_H
#define __MPCOUTV1_H

#include <MpcOut.h>
#include <cmath>

class MpcOutV1: public MpcOut
{
public:


  MpcOutV1();
 
  virtual ~MpcOutV1() {}

  Short_t get_ntow(const short nMpc) const {
    if ( nMpc!=0 && nMpc!=1 ) return -999;
    return mpcntow[nMpc];
  }

  Float_t get_esum(const short nMpc) const {
    if ( nMpc!=0 && nMpc!=1 ) return NAN;
    return mpcesum[nMpc];
  }

  Float_t get_time(const short nMpc) const {
    if ( nMpc!=0 && nMpc!=1 ) return NAN;
    return mpctime[nMpc];
  }

  Float_t get_z() const { return mpcz; }

  Float_t get_dz() const { return mpcdz; }

  Float_t get_t0() const { return mpct0; }

  Float_t get_dt0() const { return mpcdt0; }

  void set_ntow(const Short_t s_ntow, const Short_t n_ntow) {
    mpcntow[0] = s_ntow;
    mpcntow[1] = n_ntow;
  }

  void set_ntow(const Float_t s_esum, const Float_t n_esum) {
    mpcesum[0] = s_esum;
    mpcesum[1] = n_esum;
  }

  void set_time(const Float_t s_time, const Float_t n_time) {
    mpctime[0] = s_time;
    mpctime[1] = n_time;
  }

  void set_vtxt0(const Float_t vtx, const Float_t t0, const Float_t vtxerr, const Float_t t0err) {
    mpcz   = vtx;
    mpct0  = t0;
    mpcdz  = vtxerr;
    mpcdt0 = t0err;
  }

  void Reset();

  void identify(std::ostream& os = std::cout) const;

  int isValid() const;

protected:

  void Clear(Option_t *option = "");
  void Init();

  Short_t mpcntow[2];
  Float_t mpcesum[2];
  Float_t mpctime[2];
  Float_t mpcz;
  Float_t mpcdz;
  Float_t mpct0;
  Float_t mpcdt0;

private:
  ClassDef(MpcOutV1,1)
};

#endif

