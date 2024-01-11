#include "PhPhotonSnglv2.h"

ClassImp(PhPhotonSnglv2)

  using namespace std;

//_____________________________________________________________________________
PhPhotonSnglv2::PhPhotonSnglv2() :
  fTowerid(0),
  fPartesum(0)
{
  Reset();
}

//_____________________________________________________________________________
PhPhotonSnglv2::PhPhotonSnglv2(const PhPhotonSnglv2& to)
  : fTowerid(0),
    fPartesum(0)
{
  to.copy(*this);
}

//_____________________________________________________________________________
PhPhotonSnglv2&
PhPhotonSnglv2::operator=(const PhPhotonSnglv2& to)
{
  if ( this != &to ) 
    {
      Reset();
      to.copy(*this);
    }
  return *this;
}

//_____________________________________________________________________________
PhPhotonSnglv2*
PhPhotonSnglv2::clone(void) const
{
  return new PhPhotonSnglv2(*this);
}

//_____________________________________________________________________________
PhPhotonSnglv2*
PhPhotonSnglv2::create(void) const
{
  return new PhPhotonSnglv2;
}

//_____________________________________________________________________________
PhPhotonSnglv2::~PhPhotonSnglv2()
{
  Clear();
}

//_____________________________________________________________________________
void PhPhotonSnglv2::Clear(Option_t*)
{
  //  cout<<PHWHERE<<" deleting "<<endl;
  delete[] fPartesum;
  fPartesum=0;
  delete[] fTowerid;
  fTowerid=0;
}

//_____________________________________________________________________________
void
PhPhotonSnglv2::copy(PhPhotonSnglv2& to) const
{
  to.fDeadmap = fDeadmap;
  to.fWarnmap = fWarnmap;

  to.fArm = fArm;
  to.fMultiplicity = fMultiplicity;
  to.fSector = fSector;
  to.fIypos = fIypos;
  to.fIzpos = fIzpos;

  to.fX = fX;
  to.fY = fY;
  to.fZ = fZ;
  to.fDispy = fDispy;
  to.fDispz = fDispz;
  to.fE = fE;
  to.fEcent = fEcent;
  to.fRawTDC = fRawTDC;
  to.fEcore = fEcore;
  to.fChi2 = fChi2;
  to.fPadispy = fPadispy;
  to.fPadispz = fPadispz;
  to.fProb_photon = fProb_photon;
  to.fTof = fTof;
  to.fTofcorr = fTofcorr;
  to.fYcg = fYcg;
  to.fZcg = fZcg;
  to.fCorrDispy = fCorrDispy;
  to.fCorrDispz = fCorrDispz;
  to.femcpc3dz       = femcpc3dz     ;
  to.femcpc3dphi     = femcpc3dphi   ;
  to.femctrkdz       = femctrkdz     ;
  to.femctrkdphi     = femctrkdphi   ;


  to.Clear();

  to.fTowerid = new int[to.fMultiplicity];
  to.fPartesum = new float[to.fMultiplicity];
  for ( int i = 0; i < to.fMultiplicity; ++i )
    {
      to.fTowerid[i] = fTowerid[i];
      to.fPartesum[i] = fPartesum[i];
    }
}

//_____________________________________________________________________________
void
PhPhotonSnglv2::identify(std::ostream& os) const
{
  os << "PhPhotonSnglv2:" << endl;
  os << "Id : " << id() 
     << ", Arm : " << arm()
     << ", Sector : " << sector() << endl;
  os << "X : " << x() 
     << ", Y : " << y() 
     << ", Z : " << z() << endl;
  os << "Energy : " << e() 
     << ", TofCorr : " << tofcorr() << endl;
}

//_____________________________________________________________________________
int
PhPhotonSnglv2::isValid() const
{
  return 1;
} 
 
//_____________________________________________________________________________
float
PhPhotonSnglv2::partesum(int index) const
{
  if (index>=0 && index<fMultiplicity)
    {
      return fPartesum[index];
    }
  else
    {
      cerr << "PhPhotonSnglv2::partesum : index out of bounds ("
	   << index << " out of " << fMultiplicity << ")" << endl;
      return 0;
    }
}

//_____________________________________________________________________________
void
PhPhotonSnglv2::print(std::ostream& out) const
{ 
  std::ostream::fmtflags oldflags = out.flags();
  
  out << "ARM " << arm() << " SECT " << sector()
      << " POS(Z,Y) " << izpos() << " " << iypos()
      << " MULT " << multiplicity() 
      << endl;
  out.setf(ios::scientific);
  out.precision(3);
  out << " (X,Y,Z)" << x() << " " << y() << " " << z() 
      << " (CY,CZ)" << ycg() << " " << zcg() 
      << endl
      << " (E,ECENT,ECORE) " << e() << " " << ecent() << " " << ecore() 
      << endl
      << " CHI2 " << chi2() 
      << endl
      << " DISP(Z,Y) " << " " << dispz() << " " << dispy()
      << " PADISP(Z,Y) " << " " << padispz() << " " << padispy() 
      << endl   
      << " CORRDISP(Z,Y) " << " " << corrdispz() << " " << corrdispy()
      << endl;

  out.setf(oldflags);
  out << " Dead " << deadmap() << " Warn " << warnmap()
      << endl;
  out.setf(oldflags);
}

//_____________________________________________________________________________
void
PhPhotonSnglv2::Reset()
{
  fArm=-1;
  fChi2=0;
  fDeadmap=0;
  fDispy=0;
  fDispz=0;
  fE=fEcent=fEcore=0;
  fRawTDC=0;
  fMultiplicity=0;
  fPadispy=fPadispz=0;
  fProb_photon=0;
  fSector=0;
  fTof=0;
  fTofcorr=0;
  fWarnmap=0;
  fX=fY=fZ=0;
  fIypos=fIzpos=0;
  fYcg=fZcg=0;
  fCorrDispy=fCorrDispz=0;
  femcpc3dz       = -100;
  femcpc3dphi     = -100;
  femctrkdz       = -100;
  femctrkdphi     = -100;

  Clear();
}

//_____________________________________________________________________________
int
PhPhotonSnglv2::towerid(int index) const
{
  if (index>=0 && index<fMultiplicity)
    {
      return fTowerid[index];
    }
  else
    {
      cerr << "PhPhotonSnglv2::towerid : index out of bounds ("
	   << index << " out of " << fMultiplicity << ")" << endl;
      return 0;
    }
}

//_____________________________________________________________________________
void
PhPhotonSnglv2::set_multiplicity(int mul)
{
  fMultiplicity=mul;

  Clear();

  fTowerid = new int[fMultiplicity];
  for (int i = 0; i < fMultiplicity; ++i )
    {
      fTowerid[i]=-1;
    }
  fPartesum = new float[fMultiplicity];
  for (int i = 0; i < fMultiplicity; ++i )
    {
      fPartesum[i]=0.0;
    }
}

//_____________________________________________________________________________
void
PhPhotonSnglv2::set_partesum(int index, float value)
{
  if ( !fPartesum ) 
    {
      cerr << "PhPhotonSnglv2::set_partesum : internal array is null."
	   << " Did you forgot to specify the multiplicity ?"
	   << endl;
      return;
    }

  if (index>=0 && index<fMultiplicity)
    {
      fPartesum[index] = value;
    }
  else
    {
      cerr << "PhPhotonSnglv2::set_partesum : index out of bounds ("
	   << index << " out of " << fMultiplicity << ")" << endl;
    }
}

//_____________________________________________________________________________
void
PhPhotonSnglv2::set_towerid(int index, int value)
{ 
  if ( !fTowerid ) 
    {
      cerr << "PhPhotonSnglv2::set_towerid : internal array is null."
	   << " Did you forgot to specify the multiplicity ?"
	   << endl;
      return;
    }

  if (index>=0 && index<fMultiplicity)
    {
      fTowerid[index] = value;
    }
  else
    {
      cerr << "PhPhotonSnglv2::set_towerid : index out of bounds ("
	   << index << " out of " << fMultiplicity << ")" << endl;
    }
}

