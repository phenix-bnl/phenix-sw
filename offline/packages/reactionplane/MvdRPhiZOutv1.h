#ifndef __MVDRPHIZOUTV1_h
#define __MVDRPHIZOUTV1_h

#include "MvdRPhiZOut.h"
#include <iostream>

class TClonesArray;
class MvdSnglRPhiZv1;


class MvdRPhiZOutv1: public MvdRPhiZOut
{
 public:
  MvdRPhiZOutv1();
  virtual ~MvdRPhiZOutv1();

  void Reset();
  void identify(std::ostream& os = std::cout) const;
  int isValid() const {return MvdNRPhiZ;}
  unsigned int get_MvdNRPhiZ() const {return MvdNRPhiZ;}
  short int AddMvdRPhiZ(MvdSnglRPhiZv1* newdndeta, unsigned int thishit);
  void set_MvdNRPhiZ(unsigned int ndndeta) {MvdNRPhiZ = ndndeta; return;}
  unsigned int set_TClonesArraysize(unsigned int nhit);

  // get to the single track data
  unsigned short get_r(unsigned int ihit) const;
  unsigned short get_phi(unsigned int ihit) const;
  unsigned short get_z(unsigned int ihit) const;
  unsigned short get_adc(unsigned int ihit) const;

 protected:

  TClonesArray *GetMvdRPhiZ() const {return MvdRPhiZ;}
  void Clear(Option_t *option = "");

  unsigned int MvdNRPhiZ;
  TClonesArray *MvdRPhiZ;
  

  ClassDef(MvdRPhiZOutv1,1)

}; 

#endif /*__MVDRPHIZOUTV1_h*/



