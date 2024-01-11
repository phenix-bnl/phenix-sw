#include <mpcClusterContentV2.h>

ClassImp(mpcClusterContentV2)

  using namespace std;

//_____________________________________________________________________________
mpcClusterContentV2::mpcClusterContentV2() :
  fTowerid(0),
  fPartesum(0),
  fIt(0),
  fID(0),
  fPri(0),
  fEdep(0),
  fFrac(0),
  fKF(0),
  fNode(0),
  fParentKF(0),
  fParentID(0)

{
  Reset();
}

//_____________________________________________________________________________
mpcClusterContentV2::mpcClusterContentV2(const mpcClusterContentV2& to)
  : fTowerid(0),
    fPartesum(0),
    fIt(0),
    fID(0),
    fPri(0),
    fEdep(0),
    fFrac(0),
    fKF(0),
    fNode(0),
    fParentKF(0),
    fParentID(0)
{
  to.copy(*this);
}

//_____________________________________________________________________________
mpcClusterContentV2&
mpcClusterContentV2::operator=(const mpcClusterContentV2& to)
{
  if ( this != &to ) 
    {
      Reset();
      to.copy(*this);
    }
  return *this;
}

//_____________________________________________________________________________
mpcClusterContentV2*
mpcClusterContentV2::clone(void) const
{
  return new mpcClusterContentV2(*this);
}

//_____________________________________________________________________________
mpcClusterContentV2*
mpcClusterContentV2::create(void) const
{
  return new mpcClusterContentV2;
}

//_____________________________________________________________________________
mpcClusterContentV2::~mpcClusterContentV2()
{
  Clear();
  Clear_Sim();
}

//_____________________________________________________________________________
void mpcClusterContentV2::Clear(Option_t*)
{
  if ( fPartesum!=0 ) delete[] fPartesum;
  fPartesum=0;
  if ( fTowerid!=0 ) delete[] fTowerid;
  fTowerid=0;

  Clear_Sim();

}

//_____________________________________________________________________________
void mpcClusterContentV2::Clear_Sim(Option_t*)
{
  if( fIt!=0 ) delete[] fIt;
  fIt = 0;
  if( fID!=0 ) delete[] fID;
  fID = 0;
  if( fPri!=0 ) delete[] fPri;
  fPri = 0;
  if( fEdep!=0 ) delete[] fEdep;
  fEdep = 0;
  if( fFrac!=0 ) delete[] fFrac;
  fFrac = 0;
  if( fKF!=0 ) delete[] fKF;
  fKF = 0;
  if( fNode!=0 ) delete[] fNode;
  fNode = 0;

  if( fParentKF!=0 ) delete[] fParentKF;
  fParentKF = 0;
  if( fParentID!=0 ) delete[] fParentID;
  fParentID = 0;
  
    


}


//_____________________________________________________________________________
void
mpcClusterContentV2::copy(mpcClusterContentV2& to) const
{
  to.fDeadmap = fDeadmap;
  to.fWarnmap = fWarnmap;
  to.fCutword = fCutword;

  to.fArm = fArm;
  to.fId = fId;
  to.fMultiplicity = fMultiplicity;
  to.fPid = fPid;
  //to.fSector = fSector;
  to.fIxpos = fIxpos;
  to.fIypos = fIypos;

  to.fIxpos_hit = fIxpos_hit;
  to.fIypos_hit = fIypos_hit;
  

  to.fX = fX;
  to.fY = fY;
  to.fZ = fZ;
  to.fDx = fDx;
  to.fDy = fDy;
  to.fDz = fDz;
  to.fDispy = fDispy;
  to.fDispx = fDispx;
  to.fLogDispy = fLogDispy;
  to.fLogDispx = fLogDispx;
  to.fE = fE;
  to.fE9 = fE9;
  to.fEcent = fEcent;
  to.fEcore = fEcore;
  to.fEtofmin = fEtofmin;
  to.fEtofmax = fEtofmax;
  to.fChi2 = fChi2;
  to.fQuality = fQuality;
  to.fPadispy = fPadispy;
  to.fPadispx = fPadispx;
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
  to.fXcg = fXcg;

  to.fYcgmin = fYcgmin;
  to.fXcgmin = fXcgmin;
  to.fYmin = fYmin;
  to.fXmin = fXmin;

  to.fLinYcg = fLinYcg;
  to.fLinXcg = fLinXcg;
  to.fLinY = fLinY;
  to.fLinX = fLinX;

  to.fCorrDispy = fCorrDispy;
  to.fCorrDispx = fCorrDispx;

  to.fCorrLogDispy = fCorrLogDispy;
  to.fCorrLogDispx = fCorrLogDispx;
  to.fRawTDC = fRawTDC;

  to.fChi2core = fChi2core;
  to.fNdfcore = fNdfcore;

  to.fIsSplit = fIsSplit;
  to.fX1 = fX1;
  to.fY1 = fY1;
  to.fX2 = fX2;
  to.fY2 = fY2;
  to.fE1 = fE1;
  to.fE2 = fE2;
  to.fChi2_split = fChi2_split;

  to.fNsim = fNsim; 
  to.fType = fType;
/*
  to.femcpc3         = femcpc3       ;
  to.femcpc3neartrk  = femcpc3neartrk;
  to.femcpc3dz       = femcpc3dz     ;
  to.femcpc3dphi     = femcpc3dphi   ;
  to.femctrk         = femctrk       ;
  to.femctrkdz       = femctrkdz     ;
  to.femctrkdphi     = femctrkdphi   ;
  to.fpemctrk        = fpemctrk      ;
  to.femctrkquality  = femctrkquality;
*/

  to.Clear();

  to.fTowerid = new int[to.fMultiplicity];
  to.fPartesum = new float[to.fMultiplicity];
 
  for ( int i = 0; i < to.fMultiplicity; ++i )
    {
      to.fTowerid[i] = fTowerid[i];
      to.fPartesum[i] = fPartesum[i];
    }

  to.Clear_Sim();
  
  to.fIt = new int[to.fNsim];
  to.fPri = new int[to.fNsim];
  to.fID = new int[to.fNsim];
  to.fEdep = new float[to.fNsim];
  to.fFrac = new float[to.fNsim];
  to.fKF = new int[to.fNsim];
  to.fNode = new int[to.fNsim];

  to.fParentKF = new int[to.fNsim];
  to.fParentID = new int[to.fNsim];


  for ( int i = 0; i < to.fNsim; ++i )
    {
      to.fIt[i] = fIt[i];
      to.fID[i] = fID[i];
      to.fPri[i] = fPri[i];
      to.fEdep[i] = fEdep[i];
      to.fFrac[i] = fFrac[i];
      to.fKF[i] = fKF[i];
      to.fNode[i] = fNode[i];

      to.fParentKF[i] = fParentKF[i];
      to.fParentID[i] = fParentID[i];

    }
  
  
}

//_____________________________________________________________________________
void
mpcClusterContentV2::identify(std::ostream& os) const
{
  os << "mpcClusterContentV2:" << endl;
  os << "Id : " << id() 
     << ", Arm : " << arm() << endl;
  os << "X : " << x() 
     << ", Y : " << y() 
     << ", Z : " << z() << endl;
  os << "Energy : " << e() 
     << ", TofCorr : " << tofcorr() << endl;
}

//_____________________________________________________________________________
int
mpcClusterContentV2::isValid() const
{
  return 1;
} 
 
//_____________________________________________________________________________
float
mpcClusterContentV2::partesum(int index) const
{
  if (index>=0 && index<fMultiplicity)
    {
      return fPartesum[index];
    }
  else
    {
      cerr << "mpcClusterContentV2::partesum : index out of bounds ("
	   << index << " out of " << fMultiplicity << ")" << endl;
      return 0;
    }
}

//_____________________________________________________________________________
void
mpcClusterContentV2::print(std::ostream& out) const
{ 
  print_pythia();
  /*
  std::ostream::fmtflags oldflags = out.flags();
  
  out << "ARM " << arm()
      << " ID " << id()
      << " POS(X,Y) " << ixpos() << " " << iypos()
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
      << " (TOF,min,max,disp,raw) " 
      << tof() << " " << tofmin() << " " << tofmax() << " " << tofdisp()
      << " " << rawtdc()
      << endl
      << " CHI2 " << chi2() 
      << endl
      << " DISP(X,Y) " << " " << dispx() << " " << dispy()
      << " PADISP(X,Y) " << " " << padispx() << " " << padispy() 
      << endl   
      << " CORRDISP(X,Y) " << " " << corrdispx() << " " << corrdispy()
      << endl;
      
      out.setf(oldflags);
  out << " Dead " << deadmap() << " Warn " << warnmap()
  << endl;
  out.setf(oldflags);
  */
}

//_____________________________________________________________________________
void
mpcClusterContentV2::print_pythia() const
{ 
  cout << " FEE " << towerid(0)
       << " POS(X,Y) " << ixpos() << " " << iypos() << endl;
  cout.precision(3);
  cout << "ECORE " << ecore() << " (X,Y,Z)" << x() << " " << y() << " " << z() 
       << " ID is: " << fType << endl;
  
  for(int i=0;i<fNsim;i++)
    {
      cout << i << "WHICHNODE " << fNode[i] << endl;
      cout << i << " GEA " << fIt[i] << ", " << fEdep[i] 
	   << ", " << fFrac[i] << endl;
      cout << i << " PYTHIA " << fID[i] << ", " << fKF[i] << ", "
	   << fParentID[i] << ", " << fParentKF[i] << "\n";
    }
     
    
}


//_____________________________________________________________________________
void
mpcClusterContentV2::Reset()
{
  fArm=-1;
  fChi2=0;
  fDeadmap=0;
  fDispy=0;
  fDispx=0;
  fLogDispy=0;
  fLogDispx=0;
  fE=fE9=fEcent=fEcore=fEtofmin=fEtofmax=0;
  fId=0;
  fQuality=0;
  fMultiplicity=0;
  fPadispy=fPadispx=0;
  fProb_photon=0;
  fPhi=0;
  fPid=0;
  //fSector=0;
  fTof=fTofHad=fTofmin=fTofmax=0;
  fTofcorr=fTofcorrmin=fTofcorrmax=0;
  fTofdisp=0;
  fTheta=0;
  
  fWarnmap=0;
  fX=fY=fZ=0;
  fDx=fDy=fDz=0;
  fIxpos=fIypos=0;
  fIxpos_hit=fIypos_hit=0;
  fYcg=fXcg=0;
  fYcgmin=fXcgmin=0;
  fYmin=fXmin=0;
  fLinYcg=fLinXcg=0;
  fLinY=fLinX=0;
  fCutword=0;
  fCorrDispy=fCorrDispx=0;
  fCorrLogDispy=fCorrLogDispx=0;
  fRawTDC=0;

  fChi2core = 0;
  fNdfcore = 0;

  fIsSplit = 0;
  fX1 = 0;
  fY1 = 0;
  fX2 = 0;
  fY2 = 0;
  fE1 = 0;
  fE2 = 0;
  fChi2_split = 0;
  
  fNsim = 0;
  fType=-9999;

/*
  femcpc3         = -100;
  femcpc3neartrk  = -100;
  femcpc3dz       = -100;
  femcpc3dphi     = -100;
  femctrk         = -100;
  femctrkdz       = -100;
  femctrkdphi     = -100;
  fpemctrk        = -100;
  femctrkquality  = -100;
*/
  Clear();
  Clear_Sim();
}

//_____________________________________________________________________________
int
mpcClusterContentV2::towerid(int index) const
{
  if (index>=0 && index<fMultiplicity)
    {
      return fTowerid[index];
    }
  else
    {
      cerr << "mpcClusterContentV2::towerid : index out of bounds ("
	   << index << " out of " << fMultiplicity << ")" << endl;
      return 0;
    }
}

//_____________________________________________________________________________
void
mpcClusterContentV2::set_multiplicity(int mul)
{
  Clear();

  fMultiplicity=mul;

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
mpcClusterContentV2::set_partesum(int index, float value)
{
  if ( !fPartesum ) 
    {
      cerr << "mpcClusterContentV2::set_partesum : internal array is null."
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
      cerr << "mpcClusterContentV2::set_partesum : index out of bounds ("
	   << index << " out of " << fMultiplicity << ")" << endl;
    }
}

//_____________________________________________________________________________
void
mpcClusterContentV2::set_towerid(int index, int value)
{ 
  if ( !fTowerid ) 
    {
      cerr << "mpcClusterContentV2::set_towerid : internal array is null."
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
      cerr << "mpcClusterContentV2::set_towerid : index out of bounds ("
	   << index << " out of " << fMultiplicity << ")" << endl;
    }
}

//_____________________________________________________________________________
void
mpcClusterContentV2::set_sim_size(int size)
{
  Clear_Sim();

  fNsim=size;
    
  fIt = new int[fNsim];
  fPri = new int[fNsim];
  fID = new int[fNsim];
  fEdep = new float[fNsim];
  fFrac = new float[fNsim];
  fKF = new int[fNsim];
  fNode = new int[fNsim];
  
  fParentKF = new int[fNsim];
  fParentID = new int[fNsim];
  
  
  for ( int i = 0; i < fNsim; ++i )
    {
      
      fIt[i] = -1;
      fID[i] = -1;
      fPri[i] = -1;
      fEdep[i] = 0;
      fFrac[i] = 0;
      fKF[i] = -9999;
      fNode[i] = -9999;

      fParentKF[i] = -9999;
      fParentID[i] = -1;

    }

}

void 
mpcClusterContentV2::set_sim(int index, int it, int id, int pri, float edep, float frac,
	     int kf, int node, int parentkf, int parentid){
  
  if(!fIt || !fID || !fPri || !fEdep || !fFrac || !fKF
     || !fNode || !fParentKF || !fParentID)
    {
      cerr << "mpcClusterContentV2::set_sim : internal array is null."
	   << " Did you forgot to specify the fNsim ?"
	   << endl;
      return;
    }
    

  if(index < fNsim && index >=0)
    {
      fIt[index] = it;
      fID[index] = id;
      fPri[index] = pri;
      fEdep[index] = edep;
      fFrac[index] = frac;
      fKF[index] = kf;
      fNode[index] = node;
      fParentKF[index] = parentkf;
      fParentID[index] = parentid;
    }
  else
    {
      cerr << "mpcClusterContentV2::set_sim : index out of bounds ("
	   << index << " out of " << fNsim << ")" << endl;
    }
}


void 
mpcClusterContentV2::get_sim(int index, int& it, int& id, int& pri,
			     float& edep, float& frac, int& kf,
			     int& node, int& parentkf,
			     int& parentid) const
{
  
  if(!fIt || !fID || !fPri || !fEdep || !fFrac || !fKF
     || !fNode || !fParentKF || !fParentID)
    {
      cerr << "mpcClusterContentV2::get_sim : internal array is null."
	   << " Did you forgot to specify the fNsim ?"
	   << endl;
      return;
    }
    

  if(index < fNsim && index >=0)
    {
      it = fIt[index];
      id = fID[index];
      pri = fPri[index];
      edep = fEdep[index];
      frac = fFrac[index];
      kf = fKF[index];
      node = fNode[index];
      parentkf = fParentKF[index];
      parentid = fParentID[index];
    }
  else
    {
      cerr << "mpcClusterContentV2::set_sim : index out of bounds ("
	   << index << " out of " << fNsim << ")" << endl;
    }
}
