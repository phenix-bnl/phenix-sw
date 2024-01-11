#include "MvdRPhiZOutv1.h"
#include "MvdSnglRPhiZv1.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(MvdRPhiZOutv1)

static const unsigned int NMVDRPHIZ = 400;

using namespace std;

MvdRPhiZOutv1::MvdRPhiZOutv1()
{
  MvdNRPhiZ = 0;
  MvdRPhiZ = new TClonesArray("MvdSnglRPhiZv1",NMVDRPHIZ);
  return;
}

MvdRPhiZOutv1::~MvdRPhiZOutv1()
{
  if (MvdRPhiZ)
    {
      Clear();
      delete MvdRPhiZ;
    }
  return;
}

void 
MvdRPhiZOutv1::Clear(Option_t *option)
{
  MvdRPhiZ->Clear();
  if (MvdNRPhiZ > NMVDRPHIZ)  // reduce size back to default if it was increased
    {
      MvdRPhiZ->Expand(NMVDRPHIZ);
    }
  MvdNRPhiZ = 0;
  return;
}

void 
MvdRPhiZOutv1::Reset()
{
  Clear();
  return;
}

void 
MvdRPhiZOutv1::identify(ostream& out) const
{
  out << "identify yourself: I am a MvdRPhiZOutv1 object" << endl;
  out << "No of RPhiZ slices: " << MvdNRPhiZ << endl;
  return;
}


short int 
MvdRPhiZOutv1::AddMvdRPhiZ(MvdSnglRPhiZv1* newdndeta, unsigned int thisdndeta)
{
  TClonesArray &mvddndeta = *MvdRPhiZ;
  new(mvddndeta[thisdndeta]) MvdSnglRPhiZv1(newdndeta);
  return 0;
}

unsigned int 
MvdRPhiZOutv1::set_TClonesArraysize(unsigned int dndetaslices)
{
  if (dndetaslices >NMVDRPHIZ )
    {
      MvdRPhiZ->Expand(dndetaslices);
    }
  return dndetaslices;
}  

unsigned short 
MvdRPhiZOutv1::get_r(unsigned int i) const
{
  MvdSnglRPhiZv1 *mvdsnglrphiz = 
      (MvdSnglRPhiZv1*) GetMvdRPhiZ()->UncheckedAt(i);
  return((mvdsnglrphiz) ? mvdsnglrphiz->get_r() : 9999);
}

unsigned short 
MvdRPhiZOutv1::get_phi(unsigned int i) const
{
  MvdSnglRPhiZv1 *mvdsnglrphiz = 
      (MvdSnglRPhiZv1*) GetMvdRPhiZ()->UncheckedAt(i);
  return((mvdsnglrphiz) ? mvdsnglrphiz->get_phi() : 9999);
}

unsigned short 
MvdRPhiZOutv1::get_z(unsigned int i) const
{
  MvdSnglRPhiZv1 *mvdsnglrphiz = 
      (MvdSnglRPhiZv1*) GetMvdRPhiZ()->UncheckedAt(i);
  return((mvdsnglrphiz) ? mvdsnglrphiz->get_z() : 9999);
}

unsigned short 
MvdRPhiZOutv1::get_adc(unsigned int i) const
{
  MvdSnglRPhiZv1 *mvdsnglrphiz = 
      (MvdSnglRPhiZv1*) GetMvdRPhiZ()->UncheckedAt(i);
  return((mvdsnglrphiz) ? mvdsnglrphiz->get_adc() : 9999);
}
