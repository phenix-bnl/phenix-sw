#include "emcClusterContentv4.h"

ClassImp(emcClusterContentv4)

  using namespace std;

//_____________________________________________________________________________
emcClusterContentv4::emcClusterContentv4() :
  fTowerid(0),
  fPartesum(0)
{
  Reset();
}

//_____________________________________________________________________________
emcClusterContentv4::emcClusterContentv4(const emcClusterContentv4& to)
  : fTowerid(0),
    fPartesum(0)
{
  to.copy(*this);
}

//_____________________________________________________________________________
emcClusterContentv4&
emcClusterContentv4::operator=(const emcClusterContentv4& to)
{
  if ( this != &to ) 
    {
      Reset();
      to.copy(*this);
    }
  return *this;
}

//_____________________________________________________________________________
emcClusterContentv4*
emcClusterContentv4::clone(void) const
{
  return new emcClusterContentv4(*this);
}

//_____________________________________________________________________________
emcClusterContentv4*
emcClusterContentv4::create(void) const
{
  return new emcClusterContentv4;
}

//_____________________________________________________________________________
emcClusterContentv4::~emcClusterContentv4()
{
  Clear();
}

//_____________________________________________________________________________
void emcClusterContentv4::Clear(Option_t*)
{
  delete[] fPartesum;
  fPartesum=0;
  delete[] fTowerid;
  fTowerid=0;
}

//_____________________________________________________________________________
void
emcClusterContentv4::copy(emcClusterContentv4& to) const
{
  to.fDeadmap = fDeadmap;
  to.fWarnmap = fWarnmap;
  to.fCutword = fCutword;

  to.fArm = fArm;
  to.fId = fId;
  to.fMultiplicity = fMultiplicity;
  to.fPid = fPid;
  to.fSector = fSector;
  to.fIypos = fIypos;
  to.fIzpos = fIzpos;
  to.fType = fType;

  to.fX = fX;
  to.fY = fY;
  to.fZ = fZ;
  to.fDx = fDx;
  to.fDy = fDy;
  to.fDz = fDz;
  to.fDispy = fDispy;
  to.fDispz = fDispz;
  to.fE = fE;
  to.fE9 = fE9;
  to.fEcent = fEcent;
  to.fEcore = fEcore;
  to.fEtofmin = fEtofmin;
  to.fEtofmax = fEtofmax;
  to.fChi2 = fChi2;
  to.fQuality = fQuality;
  to.fPadispy = fPadispy;
  to.fPadispz = fPadispz;
  to.fProb_photon = fProb_photon;
  to.fPhi = fPhi;
  to.fTof = fTof;
  to.fTofHad = fTofHad;
  to.fTofmin = fTofmin;
  to.fTofmax = fTofmax;
  to.fTofcorr = fTofcorr;
  to.fTofcorrmin = fTofcorrmin;
  to.fTofcorrmax = fTofcorrmax;
  to.fTofdisp = fTofdisp; // typo??
  to.fTheta = fTheta;

  to.fYcg = fYcg;
  to.fZcg = fZcg;

  to.fCorrDispy = fCorrDispy;
  to.fCorrDispz = fCorrDispz;

  to.femcpc3         = femcpc3       ;
  to.femcpc3neartrk  = femcpc3neartrk;
  to.femcpc3dz       = femcpc3dz     ;
  to.femcpc3dphi     = femcpc3dphi   ;
  to.femctrk         = femctrk       ;
  to.femctrkdz       = femctrkdz     ;
  to.femctrkdphi     = femctrkdphi   ;
  to.fpemctrk        = fpemctrk      ;
  to.femctrkquality  = femctrkquality;

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
emcClusterContentv4::identify(std::ostream& os) const
{
  os << "emcClusterContentv4:" << endl;
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
emcClusterContentv4::isValid() const
{
  return 1;
} 
 
//_____________________________________________________________________________
float
emcClusterContentv4::partesum(int index) const
{
  if (index>=0 && index<fMultiplicity)
    {
      return fPartesum[index];
    }
  else
    {
      cerr << "emcClusterContentv4::partesum : index out of bounds ("
	   << index << " out of " << fMultiplicity << ")" << endl;
      return 0;
    }
}

//_____________________________________________________________________________
void
emcClusterContentv4::print(std::ostream& out) const
{ 
  std::ostream::fmtflags oldflags = out.flags();
  
  out << "ARM " << arm() << " SECT " << sector()
      << " ID " << id() << " TYPE " << type()
      << " POS(Z,Y) " << izpos() << " " << iypos()
      << " MULT " << multiplicity() 
      << endl;
  out.setf(ios::scientific);
  out.precision(3);
  out << " (X,Y,Z)" << x() << " " << y() << " " << z() 
      << " (CY,CZ)" << ycg() << " " << zcg() 
      << endl
      << " (THETA,PHI)" << theta() << " " << phi() 
      << endl
      << " (E,ECENT,ECORE) " << e() << " " << ecent() << " " << ecore() 
      << endl
      << " (TOF,min,max,disp) " << tof() << " " << tofmin() << " " << tofmax()
      << " " << tofdisp()
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
emcClusterContentv4::Reset()
{
  fArm=-1;
  fChi2=0;
  fDeadmap=0;
  fDispy=0;
  fDispz=0;
  fE=fE9=fEcent=fEcore=fEtofmin=fEtofmax=0;
  fId=0;
  fQuality=0;
  fMultiplicity=0;
  fPadispy=fPadispz=0;
  fProb_photon=0;
  fPhi=0;
  fPid=0;
  fSector=0;
  fTof=fTofHad=fTofmin=fTofmax=0;
  fTofcorr=fTofcorrmin=fTofcorrmax=0;
  fTofdisp=0;
  fTheta=0;
  fType=0;
  fWarnmap=0;
  fX=fY=fZ=0;
  fDx=fDy=fDz=0;
  fIypos=fIzpos=0;
  fYcg=fZcg=0;
  fCutword=0;
  fCorrDispy=fCorrDispz=0;
  femcpc3         = -100;
  femcpc3neartrk  = -100;
  femcpc3dz       = -100;
  femcpc3dphi     = -100;
  femctrk         = -100;
  femctrkdz       = -100;
  femctrkdphi     = -100;
  fpemctrk        = -100;
  femctrkquality  = -100;

  Clear();
}

//_____________________________________________________________________________
int
emcClusterContentv4::towerid(int index) const
{
  if (index>=0 && index<fMultiplicity)
    {
      return fTowerid[index];
    }
  else
    {
      cerr << "emcClusterContentv4::towerid : index out of bounds ("
	   << index << " out of " << fMultiplicity << ")" << endl;
      return 0;
    }
}

//_____________________________________________________________________________
void
emcClusterContentv4::set_multiplicity(int mul)
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
emcClusterContentv4::set_partesum(int index, float value)
{
  if ( !fPartesum ) 
    {
      cerr << "emcClusterContentv4::set_partesum : internal array is null."
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
      cerr << "emcClusterContentv4::set_partesum : index out of bounds ("
	   << index << " out of " << fMultiplicity << ")" << endl;
    }
}

//_____________________________________________________________________________
void
emcClusterContentv4::set_towerid(int index, int value)
{ 
  if ( !fTowerid ) 
    {
      cerr << "emcClusterContentv4::set_towerid : internal array is null."
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
      cerr << "emcClusterContentv4::set_towerid : index out of bounds ("
	   << index << " out of " << fMultiplicity << ")" << endl;
    }
}






