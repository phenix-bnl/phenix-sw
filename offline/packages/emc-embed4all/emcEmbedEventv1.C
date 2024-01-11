#include "emcEmbedEventv1.h"
#include <iomanip>
#include <iostream>
#include <cmath>

ClassImp(emcEmbedEventv1)

//_____________________________________________________________________________
emcEmbedEventv1::emcEmbedEventv1()
  : emcEmbedEvent()
{
  Reset();
}

//_____________________________________________________________________________
emcEmbedEventv1::~emcEmbedEventv1()
{
}

//_____________________________________________________________________________
void
emcEmbedEventv1::identify(std::ostream& os) const
{
  std::cout << "emcEmbedEventv1::identify" << std::endl;
}

//_____________________________________________________________________________
void
emcEmbedEventv1::print(std::ostream& os) const
{
   std::ostream::fmtflags oldflags = os.flags();

   os << "PRIMARY " << std::setw(4) << pid(0)
      << " : Mass = ";
   os.setf(std::ios::fixed);
   os.precision(5);
   os << mass(0) << " Pt = "; 
   os << pt(0);
   os << " zreal = ";
   os.precision(4);
   os << std::setw(8) << zreal(0);
   os.precision(4);
   os << " zsimu = " << std::setw(8) << zsimu(0)
      << std::endl;
   os.setf(oldflags);
}

//_____________________________________________________________________________
void
emcEmbedEventv1::Reset()
{
  double nan = sqrt(-1);

  fEnergy=nan;
  fMass=nan;
  fMomentum=nan;
  fPid=-1;
  fPt=nan;
  fPx=nan;
  fPy=nan;
  fPz=nan;
  fIsValid=-1;
  fSize=0;
  fZreal=nan;
  fZsimu=nan;
}

//_____________________________________________________________________________
void
emcEmbedEventv1::set_primary(unsigned int,
			     int pid, double energy, 
			     double px, double py, double pz,
			     double zreal, double zsimu)
{
  fPid=pid;
  fEnergy=energy;
  fPx=px;
  fPy=py;
  fPz=pz;
  fPt=sqrt(px*px+py*py);
  fMomentum=sqrt(fPt*fPt+pz*pz);
  fMass=sqrt(fEnergy*fEnergy-fMomentum*fMomentum);
  fZsimu=zsimu;
  fZreal=zreal;
  fIsValid=1;
  fSize=1;
}
