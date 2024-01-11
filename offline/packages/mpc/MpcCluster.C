// Name: MpcCluster.cxx
// Author: A. Bazilevsky (RIKEN-BNL)
// Major modifications by M. Volkov (RRC KI) Jan 27 2000
// Adapted to the MPC by M. Chiu and B. Meredith

#include <phool.h>
#include <MpcCluster.h>
#include <MpcSectorRec.h>
#include <MpcCalib.h>

#include <TMath.h>

#include <cassert>
#include <cstdlib>
#include <cstring>
#include <iostream>

using namespace std;

// Define and initialize static members

// Max number of peaks in cluster; used in MpcCluster::GetPeaks(...)
int const MpcCluster::fgMaxNofPeaks=100;

// Used in MpcCluster::GetPeaks(...), it is the number of iterations
// when fitting photons to peak areas
int const MpcCluster::fgPeakIterations=6;

// Emin cuts cluster energy: Ecl >= Epk1+Epk2 +...+Epkn !!!!!!!!!!!!!
float const MpcCluster::fgEmin=0.002;
float const MpcCluster::fgEminV2=0.01;

// chisq=3 devides about 1% of single showers (now it isn't used)
float const MpcCluster::fgChisq=3.;

// define meaningless values for (x,y)
float const MpcCluster::fgXABSURD=-999999.;
float const MpcCluster::fgYABSURD=-999999.;

int MpcCluster::moment_type = 2; 
//this is for which type of moments are used in calculations

MpcModule::MpcModule() 
  : ich(0),
    amp(0),
    tof(0),
    deadmap(0),
    warnmap(0),
    adc(0),
    tac(0)
{
}

//_____________________________________________________________________________
MpcModule::MpcModule(int ich_, float amp_, float tof_,
		     int deadmap_, int warnmap_, float adc_, float tac_) 
  : ich(ich_),
    amp(amp_),
    tof(tof_),
    deadmap(deadmap_),
    warnmap(warnmap_),
    adc(adc_),
    tac(tac_)		     	            
{  
}

// ///////////////////////////////////////////////////////////////////////////
// MpcCluster member functions

void MpcCluster::GetCorrPos(float* px, float* py)
{
  // Returns the cluster corrected position in Sector (SM) frame
  
  float x, y, xx, yy, xy;
  GetMoments(&x, &y, &xx, &xy, &yy);

  // Older style, e-dependent correction
  //float e = GetTotalEnergy();
  //fOwner->CorrectPosition(e, x, y, px, py);
}

// ///////////////////////////////////////////////////////////////////////////

void MpcCluster::GetGlobalPos( float* px, float* py, float* pz )
// Returns the cluster position in PHENIX global coord system
{

   float xc, yc;

   GetCorrPos( &xc, &yc );
// X in Sector coord is Z in Global coord !!
//   fOwner->SectorToGlobal( xc, yc, 0., px, py, pz );

}

// ///////////////////////////////////////////////////////////////////////////

float MpcCluster::GetTowerEnergy( int ich )
// Returns the energy of the ix,iy tower (0 if ich not found in the fHitList)
{
     vector<MpcModule>::iterator ph;
     if( fHitList.empty() ) return 0;
     ph = fHitList.begin();
     while( ph != fHitList.end() ) {
       if( (*ph).ich == ich ) return (*ph).amp;
	ph++;
     }
     return 0;
}

// ///////////////////////////////////////////////////////////////////////////

//Maybe we need a GetTowerEnergy(int ich)??? just convert ich to x,y
float MpcCluster::GetTowerEnergy( int ix, int iy )
// Returns the energy of the tower ix,iy (0 if tower not found in the fHitList)
{
     int ich, ixl, iyl;
     vector<MpcModule>::iterator ph;

     if( fHitList.empty() ) return 0;
     ph = fHitList.begin();
     while( ph != fHitList.end() ) {
       ich = (*ph).ich;
       iyl = ich/18;
       ixl = ich % 18;
       if( ixl == ix && iyl == iy ) return (*ph).amp;
	ph++;
     }
     return 0;
}

// ///////////////////////////////////////////////////////////////////////////
float MpcCluster::GetTowerToF( int ich )
// Returns the ToF of the ich-tower (0 if ich not found in the fHitList)
{
     vector<MpcModule>::iterator ph;
     if( fHitList.empty() ) return 0;
     ph = fHitList.begin();
     while( ph != fHitList.end() ) {
       if( (*ph).ich == ich ) return (*ph).tof;
	ph++;
     }
     return 0;
}

// ///////////////////////////////////////////////////////////////////////////

int MpcCluster::GetTowerDeadMap( int ich )
// Returns the Dead Map of the ich-tower (0 if ich not found in the fHitList)
{
     vector<MpcModule>::iterator ph;
     if( fHitList.empty() ) return 0;
     ph = fHitList.begin();
     while( ph != fHitList.end() ) {
       if( (*ph).ich == ich ) return (*ph).deadmap;
       ph++;
     }
     return 0;
}

// ///////////////////////////////////////////////////////////////////////////

int MpcCluster::GetTowerWarnMap( int ich )
  // Returns the Warning Map of the ich-tower (0 if ich not found in the fHitList)
{
     vector<MpcModule>::iterator ph;
     if( fHitList.empty() ) return 0;
     ph = fHitList.begin();
     while( ph != fHitList.end() ) {
       if( (*ph).ich == ich ) return (*ph).warnmap;
       ph++;
     }
     return 0;
}

// ///////////////////////////////////////////////////////////////////////////

float MpcCluster::GetTowerADC( int ich )
  // Returns ADC of the ich-tower (0 if ich not found in the fHitList)
{
     vector<MpcModule>::iterator ph;
     if( fHitList.empty() ) return 0;
     ph = fHitList.begin();
     while( ph != fHitList.end() ) {
       if( (*ph).ich == ich ) return (*ph).adc;
       ph++;
     }
     return 0.;
}

// ///////////////////////////////////////////////////////////////////////////

float MpcCluster::GetTowerTAC( int ich )
  // Returns ADC of the ich-tower (0 if ich not found in the fHitList)
{
     vector<MpcModule>::iterator ph;
     if ( fHitList.empty() ) {
       cout << "fhitlist was empty" << endl;
       return 0;
     }
     ph = fHitList.begin();
     while( ph != fHitList.end() ) {
       if( (*ph).ich == ich ) return (*ph).tac;
       ph++;
     }

     cout << "didn't find matching tower" << endl;
     return 0.;
}

// ///////////////////////////////////////////////////////////////////////////

// Returns Number of Dead Channels in 3x3 Modules around MaxTower
// see emc-calib/Calib/emcQAs.C for deadmap defn.
int MpcCluster::GetNDead()
{
  MpcModule hmax = GetMaxTower();
  int dead = hmax.deadmap;
  int ndead = 0;

  dead = dead>>4;		// start at bit 4
  for ( int iy=0; iy<3; iy++ )
    {
      for ( int iz=0; iz<3; iz++ )
        {
          ndead += dead&1;
          dead = dead>>1;	// shift to next tower
        }
      dead = dead>>2;		// shift up to next row
    }
  return ndead;
}

// ///////////////////////////////////////////////////////////////////////////

int MpcCluster::GetDeadMap()
{
  // MV 2001/12/06 Returns the deadmap of the dominant tower
  
//  MpcModule hmax = GetMaxTower();
//  return hmax.deadmap;
return 0;

}

// ///////////////////////////////////////////////////////////////////////////

int MpcCluster::GetWarnMap()
{
  // MV 2001/12/06 Returns the warnmap of the dominant tower
  
//  MpcModule hmax = GetMaxTower();
//  return hmax.warnmap;
return 0;

}

// ///////////////////////////////////////////////////////////////////////////

float MpcCluster::GetTotalEnergy()
// Returns the cluster total energy
{
     vector<MpcModule>::iterator ph;
     float et=0;
     if( fHitList.empty() ) return 0;
     ph = fHitList.begin();
     while( ph != fHitList.end() ) { 
	et += (*ph).amp;
	ph++;
     }
     return et;
}

// ///////////////////////////////////////////////////////////////////////////

float MpcCluster::GetECore()
// Returns the energy in 2x2 towers around the cluster Center of Gravity
{
    vector<MpcModule>::iterator ph;
    float xcg, ycg, xx, xy, yy, dx, dy;
    float energy, es, et;
    int ix, iy, ixy;

    GetMoments( &xcg, &ycg, &xx, &xy, &yy );
    xcg /= fOwner->GetModSizex();
    ycg /= fOwner->GetModSizey();
    energy = GetTotalEnergy();
    fOwner->SetProfileParameters(0,energy,xcg,ycg);

    es=0;
    if( fHitList.empty() ) return 0;
    ph = fHitList.begin();
    while( ph != fHitList.end() ) { 
      ixy = (*ph).ich;
      iy = ixy/18;
      ix = ixy - iy*18;
      dx = xcg - ix;
      dy = ycg - iy;
      et = fOwner->PredictEnergy(dx, dy, -1);
      if( et > 0.02 ) es += (*ph).amp;
      ph++;
    }
    return es;
}

// ///////////////////////////////////////////////////////////////////////////

float MpcCluster::GetECoreV2()
// Returns the energy in 2x2 towers around the cluster Center of Gravity
{
    vector<MpcModule>::iterator ph;
    float xcg, ycg, xx, xy, yy, dx, dy;
    float energy, es, et;
    int ix, iy, ixy;

    GetMomentsV2( &xcg, &ycg, &xx, &xy, &yy );
    xcg /= fOwner->GetModSizex();
    ycg /= fOwner->GetModSizey();
    energy = GetTotalEnergy();
    fOwner->SetProfileParametersV2_2(0,energy,xcg,ycg);
    int arm = fOwner->GetArm();
    es=0;

    float ecore_cut = 0.02;
    //    if(energy < 5) ecore_cut = 0.02;
    //if(energy > 5 && energy < 10) ecore_cut = (1-(energy-5.0)/10.0) * 0.02;
    if( fHitList.empty() ) return 0;
    ph = fHitList.begin();
    while( ph != fHitList.end() ) { 
      ixy = (*ph).ich;
      iy = ixy/18;
      ix = ixy - iy*18;
      int ch = mpcmap->getFeeCh(ix,iy,arm);
      float ix_map = mpcmap->getX(ch)/fOwner->GetModSizex();
      float iy_map = mpcmap->getY(ch)/fOwner->GetModSizey();;
      dx = xcg - ix_map;
      dy = ycg - iy_map;
      //      et = fOwner->PredictEnergy(dx, dy, -1);
      et = fOwner->PredictEnergyV2_2(dx, dy, energy);
      if( et > ecore_cut ) es += (*ph).amp;
      ph++;
    }
    return es;
}

// ///////////////////////////////////////////////////////////////////////////

float MpcCluster::GetE4()
// Returns the energy in 2x2 towers around the cluster Center of Gravity
{
    float xcg, ycg, xx, xy, yy;
    float e1, e2, e3, e4;
    int ix0, iy0, isx, isy;

    GetMoments( &xcg, &ycg, &xx, &xy, &yy );
    xcg /= fOwner->GetModSizex();
    ycg /= fOwner->GetModSizey();
    ix0 = int(xcg+0.5);
    iy0 = int(ycg+0.5);

    isx = 1;
    if( xcg-ix0 < 0 ) isx = -1;
    isy = 1;
    if( ycg-iy0 < 0 ) isy = -1;

    e1 = GetTowerEnergy(ix0,     iy0    );
    e2 = GetTowerEnergy(ix0+isx, iy0    );
    e3 = GetTowerEnergy(ix0+isx, iy0+isy);
    e4 = GetTowerEnergy(ix0    , iy0+isy);

    return (e1 + e2 + e3 + e4);
}
// ///////////////////////////////////////////////////////////////////////////

float MpcCluster::GetE9()
// Returns the energy in 3x3 towers around the cluster Center of Gravity
{
     float xcg, ycg, xx, xy, yy;
     int i, ic, ich, ixc, ix0, iy0, ichmax, ichmin, nhit;
     float e;
     MpcModule *hlist, *vv;
     vector<MpcModule>::iterator ph;

     e=0;

     nhit = fHitList.size();

     if( nhit <= 0 ) return e;

     hlist = new MpcModule[nhit];

     ph = fHitList.begin();
     vv = hlist;
     while( ph != fHitList.end() ) *vv++ = *ph++;

     qsort( hlist, nhit, sizeof(MpcModule), fOwner->HitNCompare );

     GetMoments( &xcg, &ycg, &xx, &xy, &yy );
     xcg /= fOwner->GetModSizex();
     ycg /= fOwner->GetModSizey();
     ix0 = int(xcg+0.5);
     iy0 = int(ycg+0.5);
     ich = iy0*18 + ix0;

     ichmax = ich + 18 + 1;
     ichmin = ich - 18 - 1;

     for( i=0; i<nhit; i++ ) 
	if( hlist[i].ich >= ichmin ) break;
     while( (i < nhit) && ( (ic=hlist[i].ich) <= ichmax) ) {
	ixc = ic - ic/18*18;
	if( abs(ixc-ix0) <= 1 ) e += hlist[i].amp;
	i++;
     }
	
     delete [] hlist;
     return e;
}

// ///////////////////////////////////////////////////////////////////////////

float MpcCluster::GetE9( int ich )
// Returns the energy in 3x3 towers around the tower ich
{
     int i, ic, ixc, ix0, ichmax, ichmin, nhit;
     float e;
     MpcModule *hlist, *vv;
     vector<MpcModule>::iterator ph;

     e=0;

     nhit = fHitList.size();

     if( nhit <= 0 ) return e;

     hlist = new MpcModule[nhit];

     ph = fHitList.begin();
     vv = hlist;
     while( ph != fHitList.end() ) *vv++ = *ph++;

     qsort( hlist, nhit, sizeof(MpcModule), fOwner->HitNCompare );

     ichmax = ich + 18 + 1;
     ichmin = ich - 18 - 1;
     ix0 = ich - ich/18*18;

     for( i=0; i<nhit; i++ ) 
	if( hlist[i].ich >= ichmin ) break;
     while( (i < nhit) && ( (ic=hlist[i].ich) <= ichmax) ) {
	ixc = ic - ic/18*18;
	if( abs(ixc-ix0) <= 1 ) e += hlist[i].amp;
	i++;
     }
	
     delete [] hlist;
     return e;
}

// ///////////////////////////////////////////////////////////////////////////

MpcModule MpcCluster::GetImpactTower()
// Returns the MpcModule corresponding to the reconstructed impact tower
{
     float x, y;
     int ix, iy, ich;
     MpcModule ht;

     GetCorrPos( &x, &y );
     ix = lowint(x/fOwner->GetModSizex() + 0.5);
     iy = lowint(y/fOwner->GetModSizey() + 0.5);
     if( ix < 0 || ix > 17 || iy < 0 || iy > 17 ) {
	printf("????? MpcClusterChi2: Something wrong in GetImpactTower: (x,y)=(%f,%f)  (ix,iy)=(%d,%d) \n", x, y, ix, iy);
	memset(&ht, 0, sizeof(MpcModule)); // MV 2002/03/12 bugfix
	ht.ich = -1;
	return ht;
     }
     else {
	ich = iy*18 + ix;
	ht.ich = ich;
	ht.amp = GetTowerEnergy( ich );
	ht.tof = GetTowerToF( ich );
	ht.deadmap = GetTowerDeadMap( ich );
	ht.warnmap = GetTowerWarnMap( ich ); // MV 2002/02/18 bugfix
	ht.adc=GetTowerADC(ich); // MV 2002/03/12 bugfix
	ht.tac=GetTowerTAC(ich); // MV 2002/03/12 bugfix
	return ht;
     }

  cout << "MpcCluster::GetImpactTower(), ERROR, should never get here" << endl;
  return ht;
}

// ///////////////////////////////////////////////////////////////////////////

MpcModule MpcCluster::GetMaxTower()
// Returns the MpcModule with the maximum energy
{
     //cout << "In GetMaxTower " << endl;
     vector<MpcModule>::iterator ph;
     float emax=0;
     MpcModule ht;

     memset(&ht, 0, sizeof(MpcModule)); // MV 2002/03/12 bugfix
     ht.ich = -1;
     if ( fHitList.empty() ) {
       cout << "MpcCluster::GetMaxTower(), fHitList is empty" << endl;
       return ht;
     }

     ph = fHitList.begin();
     while( ph != fHitList.end() ) { 
	if ( (*ph).amp > emax ) {
          emax = (*ph).amp; ht = *ph;
        }

        //cout << "getmax " << (*ph).ich << " " << (*ph).amp << " " << (*ph).adc << " " << (*ph).tac << endl;

	ph++;
     }

     if ( ht.ich == -1 ) cout << "MpcCluster::GetMaxTower(), ERROR didn't find max tower" << endl;

     //cout << "Out of GetMaxTower " << endl;
     return ht;
}

// ///////////////////////////////////////////////////////////////////////////

void MpcCluster::GetHits(MpcModule* phit, int n)
// Returns n MpcModules (sorted) with the maximum energy
{
     int nhit;
     MpcModule *hlist, *vv;
     vector<MpcModule>::iterator ph;
     vector<MpcModule> hl;

     fOwner->ZeroVector(phit, n);
     nhit = fHitList.size();

     if( nhit <= 0 ) return;

     hlist = new MpcModule[nhit];

     ph = fHitList.begin();
     vv = hlist;
     while( ph != fHitList.end() ) *vv++ = *ph++;

     qsort( hlist, nhit, sizeof(MpcModule), fOwner->HitACompare );
     for( int i=0; i<min(nhit,n); i++ ) phit[i]=hlist[i];
     delete [] hlist;
}

// ///////////////////////////////////////////////////////////////////////////

void MpcCluster::GetMoments( float* px, float* py, float* pxx, float* pxy, float* pyy )
//  Returns cluster 1-st (px,py) and 2-d momenta (pxx,pxy,pyy)
{
     vector<MpcModule>::iterator ph;
     float e, x, y, xx, yy, xy;
     int nhit;
     MpcModule *phit, *p;

     *px = fgXABSURD; *py = fgYABSURD;
     *pxx = 0; *pxy = 0; *pyy = 0;
     nhit=fHitList.size();
     if( nhit <= 0 ) return;

     phit = new MpcModule[nhit];
     ph = fHitList.begin();
     p=phit;
     while( ph != fHitList.end() ) { 
	p->ich = (*ph).ich;
	p->amp = (*ph).amp;
	ph++;
	p++;
     }
     fOwner->Momenta( nhit, phit, &e, &x, &y, &xx, &yy, &xy );
     *px = x*fOwner->GetModSizex();
     *py = y*fOwner->GetModSizey();
     *pxx = xx*fOwner->GetModSizex()*fOwner->GetModSizex();
     *pxy = xy*fOwner->GetModSizex()*fOwner->GetModSizey();
     *pyy = yy*fOwner->GetModSizey()*fOwner->GetModSizey();
     delete [] phit;
}

// ///////////////////////////////////////////////////////////////////////////


void MpcCluster::GetMomentsV2( float* px, float* py, float* pxx, float* pxy, float* pyy )
//  Returns cluster 1-st (px,py) and 2-d momenta (pxx,pxy,pyy)
{
  if(moment_type>2 || moment_type < 1)
    {
     vector<MpcModule>::iterator ph;
     float e, x, y, xx, yy, xy;
     int nhit;
     MpcModule *phit, *p;

     *px = fgXABSURD; *py = fgYABSURD;
     *pxx = 0; *pxy = 0; *pyy = 0;
     nhit=fHitList.size();
     if( nhit <= 0 ) return;

     phit = new MpcModule[nhit];
     ph = fHitList.begin();
     p=phit;
     while( ph != fHitList.end() ) { 
	p->ich = (*ph).ich;
	p->amp = (*ph).amp;
	ph++;
	p++;
     }
     fOwner->Momenta( nhit, phit, &e, &x, &y, &xx, &yy, &xy );
     *px = x*fOwner->GetModSizex();
     *py = y*fOwner->GetModSizey();
     *pxx = xx*fOwner->GetModSizex()*fOwner->GetModSizex();
     *pxy = xy*fOwner->GetModSizex()*fOwner->GetModSizey();
     *pyy = yy*fOwner->GetModSizey()*fOwner->GetModSizey();
     delete [] phit;
    }
  else
    {
      float x,y,xx,yy,xy;
      if(moment_type == 1)
	GetLinearMoments(&x,&y,&xx,&xy,&yy);
      else if(moment_type == 2)
	GetLogMoments(&x,&y,&xx,&xy,&yy,4.0);
      else
        assert(true==false && "REACHED IMPOSSIBLE STATE, variables not initialized");
      *px = x;
      *py = y;
      *pxy = xy;
      *pxx = xx;
      *pyy = yy;
    }
  return;
}

// ///////////////////////////////////////////////////////////////////////////

void MpcCluster::GetLinearMoments( float* px, float* py, float* pxx, float* pxy, float* pyy)
//  Returns cluster 1-st (px,py) and 2-d momenta (pxx,pxy,pyy)
{
     vector<MpcModule>::iterator ph;
     float e, x, y, xx, yy, xy;
     int nhit;
     MpcModule *phit, *p;

     *px = fgXABSURD; *py = fgYABSURD;
     *pxx = 0; *pxy = 0; *pyy = 0;
     nhit=fHitList.size();
     if( nhit <= 0 ) return;

     phit = new MpcModule[nhit];
     ph = fHitList.begin();
     p=phit;
     while( ph != fHitList.end() ) { 
	p->ich = (*ph).ich;
	p->amp = (*ph).amp;
	ph++;
	p++;
     }
     fOwner->MomentsLinear( nhit, phit, &e, &x, &y, &xx, &yy, &xy);
     *px = x*fOwner->GetModSizex();
     *py = y*fOwner->GetModSizey();
     *pxx = xx*fOwner->GetModSizex()*fOwner->GetModSizex();
     *pxy = xy*fOwner->GetModSizex()*fOwner->GetModSizey();
     *pyy = yy*fOwner->GetModSizey()*fOwner->GetModSizey();
     delete [] phit;
}

// ///////////////////////////////////////////////////////////////////////////

void MpcCluster::GetLogMoments( float* px, float* py, float* pxx, float* pxy, float* pyy, float weight)
//  Returns cluster 1-st (px,py) and 2-d momenta (pxx,pxy,pyy)
{
     vector<MpcModule>::iterator ph;
     float e, x, y, xx, yy, xy;
     int nhit;
     MpcModule *phit, *p;

     *px = fgXABSURD; *py = fgYABSURD;
     *pxx = 0; *pxy = 0; *pyy = 0;
     nhit=fHitList.size();
     if( nhit <= 0 ) return;

     phit = new MpcModule[nhit];
     ph = fHitList.begin();
     p=phit;
     while( ph != fHitList.end() ) { 
       p->ich = (*ph).ich;
	p->amp = (*ph).amp;
	ph++;
	p++;
     }
     fOwner->MomentsLog( nhit, phit, &e, &x, &y, &xx, &yy, &xy,weight);
     *px = x*fOwner->GetModSizex();
     *py = y*fOwner->GetModSizey();
     *pxx = xx*fOwner->GetModSizex()*fOwner->GetModSizex();
     *pxy = xy*fOwner->GetModSizex()*fOwner->GetModSizey();
     *pyy = yy*fOwner->GetModSizey()*fOwner->GetModSizey();
     delete [] phit;
}

// ///////////////////////////////////////////////////////////////////////////



void MpcCluster::GetErrors( float* pde, float* pdx, float* pdy, float* pdz)
{
  //  Returns the errors for the reconstructed energy and position

    float e, x, y;

    e = GetTotalEnergy();
    GetCorrPos( &x, &y );
    fOwner->CalculateErrors( e, x, y, pde, pdx, pdy, pdz);

}

// ///////////////////////////////////////////////////////////////////////////

void MpcCluster::GetChar( float* pe, 
			  float* pxcg, float* pycg, 
			  float* pxc, float* pyc, 
			  float* pxg, float* pyg, float* pzg,
			  float* pxx, float* pxy, float* pyy,
			  float* pde, float* pdx, float* pdy, float* pdz  )
{
  // This method replaces methods GetTotalEnergy, GetMoments, 
  // GetCorrPos, GetGlobalPos, GetErrors

  *pe = GetTotalEnergy();
  GetMoments( pxcg, pycg, pxx, pxy, pyy );
// chiu
  fOwner->CorrectPosition( *pe, *pxcg, *pycg, pxc, pyc );

//  fOwner->SectorToGlobal( *pxc, *pyc, 0, pxg, pyg, pzg );

  fOwner->CalculateErrors( *pe, *pxc, *pyc, pde, pdx, pdy, pdz);

}

// ///////////////////////////////////////////////////////////////////////////

int MpcCluster::GetPeaks( MpcPeakarea* PkList, MpcModule* ppeaks )
{
//cout << "In MpcCluster::GetPeaks" << endl;
  // Splits the cluster onto peakarea's
  // The number of peakarea's is equal to the number of Local Maxima 
  // in the cluster. Local Maxima can have the energy not less then 
  // defined in fgMinPeakEnergy
  //
  // Output: PkList - the array of peakarea's
  //         ppeaks - the array of peak MpcModules (one for each peakarea)
  //
  // Returns: >= 0 Number of Peaks;
  //	      -1 The number of Peaks is greater then fgMaxNofPeaks;
  //		 (just increase parameter fgMaxNofPeaks)
  
  int npk, ipk, nhit;
  int ixypk, ixpk, iypk, in, nh, ic;
  int ixy, ix, iy, nn;
  int ig, ng, igmpk1[fgMaxNofPeaks], igmpk2[fgMaxNofPeaks];
  int PeakCh[fgMaxNofPeaks];
  float epk[fgMaxNofPeaks*2], xpk[fgMaxNofPeaks*2], ypk[fgMaxNofPeaks*2];
  float ratio, eg, dx, dy, a, chi, chi0;
  float *Energy[fgMaxNofPeaks], *totEnergy, *tmpEnergy;
  MpcModule *ip;
  MpcModule *phit, *hlist, *vv;
  MpcPeakarea peak(fOwner);
  vector<MpcModule>::iterator ph;
  vector<MpcModule> hl;
  
  nhit = fHitList.size();
  
//cout << PHWHERE << "\tfHitList.size() = " << nhit << endl;
  if( nhit <= 0 ) return 0;
  
  hlist = new MpcModule[nhit];
  
  ph = fHitList.begin();
  vv = hlist;
  while( ph != fHitList.end() ) *vv++ = *ph++;
  
  // sort by linear channel number
  qsort( hlist, nhit, sizeof(MpcModule), fOwner->HitNCompare );

  //
  //  Find peak (maximum) position (towers with local maximum amp)
  //
  npk=0;
  for( ic=0; ic<nhit; ic++ ) {
    float amp = hlist[ic].amp;
    if( amp > fOwner->GetPeakThreshold() ) {
      int ich = hlist[ic].ich;
      int ichmax = ich + 18 + 1;
      int ichmin = ich - 18 - 1;
      int ixc = ich - ich/18*18;
      int in = ic + 1;
      // check right, and 3 towers above if there is another max
      while( (in < nhit) && (hlist[in].ich <= ichmax) ) {
	int ix = hlist[in].ich - hlist[in].ich/18*18;
	if( (abs(ixc-ix) <= 1) && (hlist[in].amp >= amp) ) goto new_ic;
	in++;
      }
      in = ic - 1;
      // check left, and 3 towers below if there is another max
      while( (in >= 0) && (hlist[in].ich >= ichmin) ) {
	int ix = hlist[in].ich - hlist[in].ich/18*18;
	if( (abs(ixc-ix) <= 1) && (hlist[in].amp > amp) ) goto new_ic;
	in--;
      }
      //  std::cout << "npeaks is " << npk << "\n";
      if( npk >= fgMaxNofPeaks ) { delete [] hlist; std::cout << "Npeaks > max # of peaks....think about adding more: " << npk << "\n"; return -1;  }

      // ic is a maximum in a 3x3 tower group
      PeakCh[npk]=ic;
      npk++;
    }
  new_ic: continue;
  }
 
  // there was only one peak
  if( npk <= 1 ) {
    hl.erase( hl.begin(), hl.end() );
    for( int ich=0; ich<nhit; ich++ ) hl.push_back(hlist[ich]);
    peak.ReInitialize(hl);
    PkList[0]=peak;

    if( npk == 1 ) *ppeaks = hlist[PeakCh[0]];
    else  *ppeaks = GetMaxTower();

    delete [] hlist;
    return 1;
  }
 
  // there were more than one peak

  for( ipk=0; ipk<npk; ipk++ ) { Energy[ipk] = new float[nhit]; }
  totEnergy = new float[nhit];
  tmpEnergy = new float[nhit];
  for ( int i = 0; i < nhit; ++i ) 
    {
      totEnergy[i]=0.0;
      tmpEnergy[i]=0.0;
    }
  //
  // Divide energy in towers among photons positioned in every peak
  //
  ratio = 1.;
  for( int iter=0; iter<fgPeakIterations; iter++ ) {
    fOwner->ZeroVector( tmpEnergy, nhit );
    for( ipk=0; ipk<npk; ipk++ ) {
      
      ic = PeakCh[ipk];
      if( iter > 0 ) ratio = Energy[ipk][ic]/totEnergy[ic];
      eg = hlist[ic].amp * ratio;
      ixypk = hlist[ic].ich;
      iypk = ixypk/18;
      ixpk = ixypk - iypk*18;
      epk[ipk] = eg;
      xpk[ipk] = eg * ixpk;	// center of energy in x
      ypk[ipk] = eg * iypk;	// center of energy in y
      
      // add up energies of tower to the right and 3 above
      if( ic < nhit-1 ){
	for( in=ic+1; in<nhit; in++ ) {
	  ixy = hlist[in].ich;
	  iy = ixy/18;
	  ix = ixy - iy*18;

	  if( (ixy-ixypk) > 18+1 ) break;
	  if( abs(ix-ixpk) <= 1 ) {
	    if( iter > 0 ) ratio = Energy[ipk][in]/totEnergy[in];
	    eg = hlist[in].amp * ratio;
	    epk[ipk] += eg;
	    xpk[ipk] += eg*ix;
	    ypk[ipk] += eg*iy;
	  }
	}
      } // if ic
      
      // add up energies of tower to the left and 3 below
      if( ic >= 1 ){
	for( in=ic-1; in >= 0; in-- ) {
	  ixy = hlist[in].ich;
	  iy = ixy/18;
	  ix = ixy - iy*18;
	  if( (ixypk-ixy) > 18+1 ) break;
	  if( abs(ix-ixpk) <= 1 ) {
	    if( iter > 0 ) ratio = Energy[ipk][in]/totEnergy[in];
	    eg = hlist[in].amp * ratio;
	    epk[ipk] += eg;
	    xpk[ipk] += eg*ix;
	    ypk[ipk] += eg*iy;
	  }
	}
      } // if ic
      
      if(epk[ipk] < 0.00001) cout << PHWHERE << " Energy too small in sharing, iteration: " << iter << "\n";
      xpk[ipk] = xpk[ipk]/epk[ipk];
      ypk[ipk] = ypk[ipk]/epk[ipk];
      fOwner->SetProfileParameters(0, epk[ipk], xpk[ipk], ypk[ipk]);

      for( in=0; in<nhit; in++ ) {
	ixy = hlist[in].ich;
	iy = ixy/18;
	ix = ixy - iy*18;
	dx = xpk[ipk]-ix;
	dy = ypk[ipk]-iy;
	a = 0;
	
        // predict energy within 2.5 cell square around local peak
	if( ABS(dx) < 2.5 && ABS(dy) < 2.5 )
	  a = epk[ipk]*fOwner->PredictEnergy(dx, dy, -1);
	
	Energy[ipk][in] = a;
	tmpEnergy[in] += a;
      }
      
    } // for ipk
    float flim = 0.000001;
    for ( in = 0; in < nhit; in++ )
      {
	if (tmpEnergy[in] > flim)
	  {
	    totEnergy[in] = tmpEnergy[in];
	  }
	else
	  {
	    totEnergy[in] = flim;
	  }
      }
  } // for iter
  
  phit = new MpcModule[nhit];
  
  ng=0;
  for( ipk=0; ipk<npk; ipk++ ) {
    nh=0;

    // fill phit, the tower distribution for a peak
    for( in=0; in<nhit; in++ ) {
      if( tmpEnergy[in] > 0 ) {
	ixy = hlist[in].ich;
	a = hlist[in].amp * Energy[ipk][in]/tmpEnergy[in];
	if( a > fgEmin ) {
	  phit[nh].ich=ixy;
	  phit[nh].amp=a;
	  phit[nh].tof= hlist[in].tof;  // Not necessary here
	  phit[nh].deadmap= hlist[in].deadmap;  // Not necessary here
	  phit[nh].warnmap= hlist[in].warnmap;  // MV 2002/02/18 bugfix

	  // MV 2002/03/12 bugfix: let adc, tdc=0 for split clusters
	  phit[nh].adc=hlist[in].adc; // MV 2002/03/12 bugfix
	  phit[nh].tac=hlist[in].tac; // MV 2002/03/12 bugfix

	  nh++;
	}
      }
    }

    // if number hits > 0
    if( nh>0 ) {
      chi=fOwner->Chi2Limit(nh)*10;
      int ndf; // just a plug for changed Gamma parameter list MV 28.01.00

      fOwner->Gamma(nh, phit,&chi, &chi0, &epk[ng], &xpk[ng], &ypk[ng],
		    &epk[ng+1], &xpk[ng+1], &ypk[ng+1], ndf);

      igmpk1[ipk]=ng;
      igmpk2[ipk]=ng;
      if( epk[ng+1]>0 ) { ng++; igmpk2[ipk]=ng;}
      ng++;
    }
    else {
      igmpk1[ipk]=-1;
      igmpk2[ipk]=-1;
    }
  }
  
  fOwner->ZeroVector( tmpEnergy, nhit );
  for( ipk=0; ipk<npk; ipk++ ) {
    ig=igmpk1[ipk];
    if( ig >= 0 ) {
      fOwner->SetProfileParameters(0, epk[ig], xpk[ig], ypk[ig]);
      for( in=0; in<nhit; in++ ) {
	Energy[ipk][in]=0;
	for(ig=igmpk1[ipk]; ig<=igmpk2[ipk]; ig++){
	  ixy = hlist[in].ich;
	  iy = ixy/18;
	  ix = ixy - iy*18;
	  dx = xpk[ig]-ix;
	  dy = ypk[ig]-iy;
	  a = epk[ig]*fOwner->PredictEnergy(dx, dy, epk[ig]);
	  Energy[ipk][in] += a;
	  tmpEnergy[in] += a;
	}
      } // for( in
    } // if( ig >= 0
  } // for( ipk
  
  ip = ppeaks;
  nn=0;
  for( ipk=0; ipk<npk; ipk++ ) {
    nh=0;
    for( in=0; in<nhit; in++ ) {
      if( tmpEnergy[in] > 0 ) {
	ixy = hlist[in].ich;
	a = hlist[in].amp * Energy[ipk][in]/tmpEnergy[in];
	if( a > fgEmin ) {
	  phit[nh].ich=ixy;
	  phit[nh].amp=a;
	  phit[nh].tof=hlist[in].tof;
	  phit[nh].deadmap=hlist[in].deadmap;
	  phit[nh].warnmap=hlist[in].warnmap; // MV 2002/02/18 bugfix

	  // MV 2002/03/12 bugfix: let adc, tdc=0 for split clusters
	  phit[nh].adc=hlist[in].adc; // MV 2002/03/12 bugfix
	  phit[nh].tac=hlist[in].tac; // MV 2002/03/12 bugfix

	  nh++;
	}
      }
    }
    if( nh>0 ) {
      *ip++ = hlist[PeakCh[ipk]];
      hl.erase( hl.begin(), hl.end() );
      for( in=0; in<nh; in++ ) hl.push_back(phit[in]);
      peak.ReInitialize(hl);
      PkList[nn++]=peak;
    }
  }
  
  delete [] phit;
  delete [] hlist;
  for( ipk=0; ipk<npk; ipk++ ) delete [] Energy[ipk];
  delete [] totEnergy;
  delete [] tmpEnergy;

  return nn;
}

// ///////////////////////////////////////////////////////////////////////////


int MpcCluster::Get3x3CG(float &x,float &y,int xpk,int ypk,float e[][3])
{
  float tx, ty;
  int ret_val = 0;
  if(moment_type == 2)
    ret_val = Get3x3LogCG(tx,ty,xpk,ypk,e);
  else if(moment_type == 1)
    ret_val = Get3x3LinearCG(tx,ty,xpk,ypk,e);
  else
    ret_val = Get3x3GridCG(tx,ty,xpk,ypk,e);
  x = tx;
  y = ty;
  return ret_val;
}

int MpcCluster::Get3x3LogCG(float &x,float &y,int xpk,int ypk,float e[][3])
{
  x = 0;
  y = 0;
  float w_i = 0;
  float wsum=0;
  float esum=0;
  for(int i=0;i<3;i++){
    for(int j=0;j<3;j++){
      esum+=e[i][j];
    }
  }
  if(esum < 0.000001){ 
    x = -9999.; y=-9999.; 
    cout << PHWHERE << "energy too small in clustering\n";
    return 0;
  }

  int ix,iy,feech;
  //  float x_tow,y_tow;
  for(int i=0;i<3;i++){
    for(int j=0;j<3;j++){
      //cout << i << ", " << j << ": " << e[i][j] << "\n\n";
      if(e[i][j] < 0.0001) 
	continue;
      else {
	w_i = 4 - log(e[i][j]/esum);
	//cout << "weight is: " << w_i << endl;
	if(w_i < 0) continue;
      }
      
      ix = xpk + (i-1);
      iy = ypk + (j-1);
      //  cout << "ix,iy,xpk,ypk: " << ix << ", " << iy << ", " << xpk << ", " << ypk << "\n";
      feech = mpcmap->getFeeCh(ix,iy,fOwner->GetArm());
      //cout << "feech is: " << feech << "\n";
      if(feech < 0 || feech > 575) continue;

     

      wsum+=w_i;
      x+=mpcmap->getX(feech)/fOwner->GetModSizex()*w_i;
      y+=mpcmap->getY(feech)/fOwner->GetModSizey()*w_i;
    }
  }
  x=x/wsum;
  y=y/wsum;

  //  cout << "in cg log funct: x,y: " << x << ", " << y << endl;
  return 1;
  
}


int MpcCluster::Get3x3LinearCG(float &x,float &y,int xpk,int ypk,float e[][3])
{
  x = 0;
  y = 0;
  float w_i = 0;
  float wsum=0;
  
  int ix,iy,feech;

  for(int i=0;i<3;i++){
    for(int j=0;j<3;j++){
      if(e[i][j] < 0.0001) 
	continue;
      w_i = e[i][j];
      
      
      ix = xpk + (i-1);
      iy = ypk + (j-1);
      feech = mpcmap->getFeeCh(ix,iy,fOwner->GetArm());
      if(feech < 0 || feech > 575) continue;
      
      
      wsum+=w_i;
      x+=mpcmap->getX(feech)/fOwner->GetModSizex()*w_i;
      y+=mpcmap->getY(feech)/fOwner->GetModSizey()*w_i;
    }
  }
  
  if(wsum < 0.000001){ 
    x = -9999.; y=-9999.; 
    cout << PHWHERE << "energy too small in clustering\n";
    return 0;
  }
  
  x=x/wsum;
  y=y/wsum;
  return 1;
  
}



int MpcCluster::Get3x3GridCG(float &x,float &y,int xpk,int ypk,float e[][3])
{
  x = 0;
  y = 0;
  float w_i = 0;
  float wsum=0;
  
  int ix,iy,feech;

  for(int i=0;i<3;i++){
    for(int j=0;j<3;j++){
      if(e[i][j] < 0.0001) 
	continue;
      w_i = e[i][j];
      
      
      ix = xpk + (i-1);
      iy = ypk + (j-1);
      feech = mpcmap->getFeeCh(ix,iy,fOwner->GetArm());
      if(feech < 0 || feech > 575) continue;
      
      
      wsum+=w_i;
      x+=(float)(ix)*w_i;
      y+=(float)(iy)*w_i;
    }
  }
  
  if(wsum < 0.000001){ 
    x = -9999.; y=-9999.; 
    cout << PHWHERE << "energy too small in clustering\n";
    return 0;
  }
  
  x=x/wsum;
  y=y/wsum;
  return 1;
  
}


int MpcCluster::GetPeaks_Mpc( MpcPeakarea* PkList, MpcModule* ppeaks )
{
  bool show = false;
  //  if(ncalls%100 == 0) cout << ncalls << endl;

//cout << "In MpcCluster::GetPeaks_Mpc" << endl;

  // Splits the cluster onto peakarea's
  // The number of peakarea's is equal to the number of Local Maxima 
  // in the cluster. Local Maxima can have the energy not less then 
  // defined in fgMinPeakEnergy
  //
  // Output: PkList - the array of peakarea's
  //         ppeaks - the array of peak MpcModules (one for each peakarea)
  //
  // Returns: >= 0 Number of Peaks;
  //	      -1 The number of Peaks is greater then fgMaxNofPeaks;
  //		 (just increase parameter fgMaxNofPeaks)
  
  int npk, ipk, nhit;
  int ixypk, ixpk, iypk, in, nh, ic;

  bool split = 0;
  float chi2_split = 9999;
  float se1,sx1,sy1,se2,sx2,sy2;
  se1=sx1=sy1=se2=sx2=sy2=0;
  
  //  float wsum = 0; float w_i = 0; float esum = 0;
  //float ix_real, iy_real; //new additions
  //int feech;

  int ixy, ix, iy, nn;
  int ng;
  int ig;
    int igmpk1[fgMaxNofPeaks], igmpk2[fgMaxNofPeaks];
  int PeakCh[fgMaxNofPeaks];
  float epk[fgMaxNofPeaks*2], xpk[fgMaxNofPeaks*2], ypk[fgMaxNofPeaks*2];
  float ratio, eg, dx, dy, a, chi, chi0;
  float *Energy[fgMaxNofPeaks], *totEnergy, *tmpEnergy;
  MpcModule *ip;
  MpcModule *phit, *hlist, *vv;
  MpcPeakarea peak(fOwner);
  vector<MpcModule>::iterator ph;
  vector<MpcModule> hl;
  
  nhit = fHitList.size();
  
//cout << PHWHERE << "\tfHitList.size() = " << nhit << endl;
  if( nhit <= 0 ) return 0;
  
  hlist = new MpcModule[nhit];
  
  ph = fHitList.begin();
  vv = hlist;
  while( ph != fHitList.end() ) *vv++ = *ph++;
  
  // sort by linear channel number
  qsort( hlist, nhit, sizeof(MpcModule), fOwner->HitNCompare );

  //
  //  Find peak (maximum) position (towers with local maximum amp)
  //
  //  int arm = fOwner->GetArm();
  npk=0;
  for( ic=0; ic<nhit; ic++ ) {
    float amp = hlist[ic].amp;
    if( amp > fOwner->GetPeakThreshold() ) {
      int ich = hlist[ic].ich;
      int ichmax = ich + 18 + 1;
      int ichmin = ich - 18 - 1;
      int ixc = ich - ich/18*18;
      int in = ic + 1;
      // check right, and 3 towers above if there is another max
      while( (in < nhit) && (hlist[in].ich <= ichmax) ) {
	int ix = hlist[in].ich - hlist[in].ich/18*18;
	if( (abs(ixc-ix) <= 1) && (hlist[in].amp >= amp) ) goto new_ic;
	in++;
      }
      in = ic - 1;
      // check left, and 3 towers below if there is another max
      while( (in >= 0) && (hlist[in].ich >= ichmin) ) {
	int ix = hlist[in].ich - hlist[in].ich/18*18;
	if( (abs(ixc-ix) <= 1) && (hlist[in].amp > amp) ) goto new_ic;
	in--;
      }
      //  std::cout << "npeaks is " << npk << "\n";
      if( npk >= fgMaxNofPeaks ) { delete [] hlist; std::cout << "Npeaks > max # of peaks....think about adding more: " << npk << "\n"; return -1;  }

      // ic is a maximum in a 3x3 tower group
      PeakCh[npk]=ic;
      npk++;
    }
  new_ic: continue;
  }
 
  // there was only one peak
  if( npk <= 1 ) {
    for( int itr=0; itr<nhit; itr++ ) {
      float amp = hlist[itr].amp;
      int ixy = hlist[itr].ich;
      int iy = ixy/18;
      int ix = ixy - iy*18;
      if(show) cout << "ix,iy, energy: " << ix << ", " 
		    << iy << ", " << amp << endl;
    }
    

    hl.erase( hl.begin(), hl.end() );
    for( int ich=0; ich<nhit; ich++ ) hl.push_back(hlist[ich]);
    peak.ReInitialize(hl);
    PkList[0]=peak;



    if( npk == 1 ) *ppeaks = hlist[PeakCh[0]];
    else  *ppeaks = GetMaxTower();

    delete [] hlist;
    return 1;
  }
 
  // there were more than one peak

  for( ipk=0; ipk<npk; ipk++ ) { Energy[ipk] = new float[nhit]; }
  totEnergy = new float[nhit];
  tmpEnergy = new float[nhit];
  for ( int i = 0; i < nhit; ++i ) 
    {
      totEnergy[i]=0.0;
      tmpEnergy[i]=0.0;
    }
  //
  // Divide energy in towers among photons positioned in every peak
  //
  ratio = 1.;
  
  
  
  for( int iter=0; iter<fgPeakIterations; iter++ ) {
    if(show)   cout << "iteration: " << iter << endl;
    fOwner->ZeroVector( tmpEnergy, nhit );
    if(show)    cout << "npeaks: " << npk << endl;
    for( ipk=0; ipk<npk; ipk++ ) {
      if(show)      cout << "npks, ipk: " << npk << ": " << ipk << endl;
      ic = PeakCh[ipk];

      //      if( iter > 0 ) ratio = Energy[ipk][ic]/totEnergy[ic];
      
      //      eg = hlist[ic].amp * ratio;
      ixypk = hlist[ic].ich;
      iypk = ixypk/18;
      ixpk = ixypk - iypk*18;
      
      int ichmax = ixypk + 18 + 1;
      int ichmin = ixypk - 18 - 1;
      float e_3x3[3][3] = {{0}};
      epk[ipk] = 0;
      for( int i=0; i<nhit; i++ ){
	

	int ich = hlist[i].ich;
	in = i;
	if( ich < ichmin ) continue;
	if( ich > ichmax ) continue;
	
	int iy_tow = ich/18;
	int ix_tow = ich - ich/18*18;

	if( abs(ix_tow-ixpk) <= 1 && abs(iy_tow-iypk) <=1){
	  

	  if( iter > 0 ) ratio = Energy[ipk][in]/totEnergy[in];


	  eg = hlist[i].amp*ratio;
	  epk[ipk] += eg;
	  e_3x3[ix_tow-ixpk+1][iy_tow-iypk+1]=eg;
	  if(show)    cout << "nhit,ihit,,ich,ix,iy: " << i << ", " << ich 
	    << ", " << ix_tow << ", " << iy_tow;
	  if(show)cout << ": epk,ratio,etow: " << epk[ipk] << ", " << ratio << ", " << e_3x3[ix_tow-ixpk+1][iy_tow-iypk+1] << endl;
	}
      }
	
      Get3x3LinearCG(xpk[ipk],ypk[ipk],ixpk,iypk,e_3x3);
      //Get3x3LinearCG(xpk[ipk],ypk[ipk],ixpk,iypk,e_3x3);
      if(show) cout << "x,y from cg calc are: " << xpk[ipk] << ", " << ypk[ipk] << " Given that centrla tower is: " << ixpk << ", " << iypk << "\n";
      //      fOwner->SetProfileParameters(0, epk[ipk], xpk[ipk], ypk[ipk]);
      fOwner->SetProfileParameters(0, epk[ipk], xpk[ipk], ypk[ipk]);
      
      for( in=0; in<nhit; in++ ) {
	ixy = hlist[in].ich;
	iy = ixy/18;
	ix = ixy - iy*18;
	int feech = mpcmap->getFeeCh(ix,iy,fOwner->GetArm());
	if(moment_type == 1 || moment_type == 2){
	  dx = xpk[ipk]-mpcmap->getX(feech)/fOwner->GetModSizex();
	  dy = ypk[ipk]-mpcmap->getY(feech)/fOwner->GetModSizey();
	}
	else{
	  dx = xpk[ipk]-ix;
	  dy = ypk[ipk]-iy;
	}
	
	a = 0;
	
        // predict energy within 2.5 cell square around local peak
	if( ABS(dx) < 2.5 && ABS(dy) < 2.5 )
	  a = epk[ipk]*fOwner->PredictEnergy(dx, dy, epk[ipk]);
	  //	  a = epk[ipk]*fOwner->PredictEnergy(dx, dy, -1);
	
	Energy[ipk][in] = a;
	tmpEnergy[in] += a;
      }
      
    } // for ipk
    float flim = 0.000001;
    for ( in = 0; in < nhit; in++ )
      {
	if (tmpEnergy[in] > flim)
	  {
	    totEnergy[in] = tmpEnergy[in];
	  }
	else
	  {
	    totEnergy[in] = flim;
	  }
      }
  } // for iter
  
  phit = new MpcModule[nhit];
  
  ng=0;
  for( ipk=0; ipk<npk; ipk++ ) {
    nh=0;
    if(show) cout << "\npeak #: " << ipk << endl;
    // fill phit, the tower distribution for a peak
    for( in=0; in<nhit; in++ ) {
      if( tmpEnergy[in] > 0 ) {
	ixy = hlist[in].ich;
	a = hlist[in].amp * Energy[ipk][in]/tmpEnergy[in];
	if( a > fgEminV2 ) {


	  phit[nh].ich=ixy;
	  phit[nh].amp=a;
	  phit[nh].tof= hlist[in].tof;  // Not necessary here
	  phit[nh].deadmap= hlist[in].deadmap;  // Not necessary here
	  phit[nh].warnmap= hlist[in].warnmap;  // MV 2002/02/18 bugfix

	  float amp = hlist[in].amp;
	  int ixy = hlist[in].ich;
	  int iy = ixy/18;
	  int ix = ixy - iy*18;
	  if(show) cout << "ix,iy, energy: " << ix << ", " 
			<< iy << ", " << amp << endl;

	  // MV 2002/03/12 bugfix: let adc, tdc=0 for split clusters
	  phit[nh].adc=hlist[in].adc; // MV 2002/03/12 bugfix
	  phit[nh].tac=hlist[in].tac; // MV 2002/03/12 bugfix

	  nh++;
	}
      }
    }

    // if number hits > 0
    if( nh>0 ) {
      //chi=fOwner->Chi2Limit(nh)*1000; //this effectively turns off cluster splitting
      chi = 1000;
      int ndf; // just a plug for changed Gamma parameter list MV 28.01.00

      fOwner->Gammacore(nh, phit, &chi, &chi0, &epk[ng], &xpk[ng], &ypk[ng],
			&epk[ng+1], &xpk[ng+1], &ypk[ng+1], ndf,
			split,chi2_split,se1,sx1,sy1,se2,sx2,sy2);
      
      // float temp_e, x_temp,y_temp;
       
      //        fOwner->Gammacore(nh, phit,&chi, &chi0, &temp_e, &x_temp, &y_temp,
      // 			 &epk[ng+1], &xpk[ng+1], &ypk[ng+1], ndf);
      //        fOwner->Mom1V2(nh,phit,&epk[ng],&xpk[ng],&ypk[ng]);
      //        xpk[ng] = x_temp;
      //        ypk[ng] = y_temp;
      //        epk[ng] = temp_e;
       
       
       //cout << "nh, Energy before/after: " << nh << ": " << epk[ng] << ", " << temp_e << endl;
       //cout << "(x,y) before/after: (" << xpk[ng] << ", " << ypk[ng] << "), (" << x_temp << ", " << y_temp << ")\n";

       igmpk1[ipk]=ng;
       igmpk2[ipk]=ng;
       if( epk[ng+1]>0 ) { ng++; igmpk2[ipk]=ng;}
       ng++;
     }
     else {
       igmpk1[ipk]=-1;
       igmpk2[ipk]=-1;
     }
   }

   fOwner->ZeroVector( tmpEnergy, nhit );
   for( ipk=0; ipk<npk; ipk++ ) {
     if(show) cout << "\npeak #: " << ipk << endl;

     ig=igmpk1[ipk];
     if( ig >= 0 ) {
       if(show) cout << "did we try to split? " << igmpk1[ipk] << ", " << igmpk2[ipk] << endl;
       //      fOwner->SetProfileParameters(0, epk[ig], xpk[ig], ypk[ig]);
       //fOwner->SetProfileParameters(0, epk[ig], xpk[ig], ypk[ig]);
       fOwner->SetProfileParametersV2_2(0, epk[ig], xpk[ig], ypk[ig]);
       //fOwner->SetProfileParametersV2_2(0, 20.0, xpk[ig], ypk[ig]);
       for( in=0; in<nhit; in++ ) {
 	Energy[ipk][in]=0;
 	for(ig=igmpk1[ipk]; ig<=igmpk2[ipk]; ig++){
 	  ixy = hlist[in].ich;
 	  iy = ixy/18;
 	  ix = ixy - iy*18;
 	  int feech = mpcmap->getFeeCh(ix,iy,fOwner->GetArm());
 	  if(moment_type == 1 || moment_type == 2){
 	    dx = xpk[ipk]-mpcmap->getX(feech)/fOwner->GetModSizex();
 	    dy = ypk[ipk]-mpcmap->getY(feech)/fOwner->GetModSizey();
 	  }
 	  else{
 	    dx = xpk[ipk]-ix;
 	    dy = ypk[ipk]-iy;
 	  }
 	  //	  a = epk[ig]*fOwner->PredictEnergy(dx, dy, epk[ig]);
 	  a = 0;
 	  //	  if( ABS(dx) < 2.5 && ABS(dy) < 2.5 )
	  a = epk[ig]*fOwner->PredictEnergyV2_2(dx, dy, epk[ig]);
	  //a = epk[ig]*fOwner->PredictEnergyV2_2(dx, dy, 20.);
	  //a = epk[ig]*fOwner->PredictEnergy(dx, dy, epk[ig]);
 	  Energy[ipk][in] += a;
 	  tmpEnergy[in] += a;
 	}
       } // for( in
     } // if( ig >= 0
   } // for( ipk

   ip = ppeaks;
   nn=0;
   for( ipk=0; ipk<npk; ipk++ ) {
     if(show) cout << "\npeak #: " << ipk << endl;
     nh=0;
     for( in=0; in<nhit; in++ ) {
       if( tmpEnergy[in] > 0 ) {
 	ixy = hlist[in].ich;
 	a = hlist[in].amp * Energy[ipk][in]/tmpEnergy[in];
 	if( a > fgEminV2 ) {
 	  phit[nh].ich=ixy;
 	  phit[nh].amp=a;
 	  phit[nh].tof=hlist[in].tof;
 	  phit[nh].deadmap=hlist[in].deadmap;
 	  phit[nh].warnmap=hlist[in].warnmap; // MV 2002/02/18 bugfix


 	  float amp = hlist[in].amp;
 	  int ixy = hlist[in].ich;
 	  int iy = ixy/18;
 	  int ix = ixy - iy*18;
 	  if(show) cout << "ix,iy, energy: " << ix << ", " 
 			<< iy << ", " << amp << endl;

 	  // MV 2002/03/12 bugfix: let adc, tdc=0 for split clusters
 	  phit[nh].adc=hlist[in].adc; // MV 2002/03/12 bugfix
 	  phit[nh].tac=hlist[in].tac; // MV 2002/03/12 bugfix

 	  nh++;
 	}
       }
     }

    if( nh>0 ) {
      *ip++ = hlist[PeakCh[ipk]];
      hl.erase( hl.begin(), hl.end() );
      for( in=0; in<nh; in++ ) hl.push_back(phit[in]);
      peak.ReInitialize(hl);
      PkList[nn++]=peak;
    }
  }
  
  delete [] phit;
  delete [] hlist;
  for( ipk=0; ipk<npk; ipk++ ) delete [] Energy[ipk];
  delete [] totEnergy;
  delete [] tmpEnergy;

  return nn;
}



// /////////////////////////////////////////////////////////////////
// MpcPeakarea member functions

void MpcPeakarea::GetChar( float* pe, float* pec, 
			   float* pecore, float* pecorec, 
			   float* pe9,
			   float* pxcg, float* pycg, 		// center of gravity
			   float* pxcgmin, float* pycgmin, 	// 
			   float* pxc, float* pyc, 		// Local (Sector) coords
			   float* pxg, float* pyg, float* pzg,	// Global coords
			   float* pxx, float* pxy, float* pyy,	// moments
			   float* pchi,
			   float* pde, float* pdx, float* pdy, float* pdz  )
// This method replaces "cluster" methods GetTotalEnergy, GetMoments,
// GetCorrPos, GetGlobalPos, GetErrors and "MpcPeakarea" methods GetCGmin, GetChi2
{
//cout << "in GetChar" << endl;
    float chi, chi0;
    float e1, x1, y1, e2, x2, y2;
    int nh;
    MpcModule *phit, *vv;
    vector<MpcModule>::iterator ph;
    vector<MpcModule> hl;
    float tmplvalue; // temporary lvalue

    *pe = 0;
    hl = fHitList;
    nh = hl.size();
    if( nh <= 0 ) return;

//cout << "nh " << nh << endl;

    phit = new MpcModule[nh];

    ph = hl.begin();
    vv = phit;
    while( ph != hl.end() ) *vv++ = *ph++;

    chi = fgChisq*1000;
    int ndf; // Gamma parameter list changed MV 28.01.00
    fOwner->Gamma(nh, phit, &chi, &chi0, &e1, &x1, &y1, &e2, &x2, &y2, ndf);
    fNdf=ndf;
    *pchi=chi0;

    // Calculate CL
    tmplvalue=fOwner->Chi2Correct(chi0, ndf)*ndf; // nh->ndf MV 28.01.00
    if(tmplvalue>0.) fCL=TMath::Prob(tmplvalue, ndf);
    else fCL=1.; // nothing to say about this peak area

    // Shower Center of Gravity after shower profile fit
    *pxcgmin = x1*fOwner->GetModSizex();
    *pycgmin = y1*fOwner->GetModSizey();

    *pe = GetTotalEnergy();
    *pecore = GetECore();
    GetMoments( pxcg, pycg, pxx, pxy, pyy );
    fOwner->CorrectPosition(*pe, *pxcgmin, *pycgmin, pxc, pyc);

    if ( fOwner->GetArm()==0 ) *pzg = -220.947;
    else if ( fOwner->GetArm()==1 ) *pzg = 220.947;
    else
      {
        cout << PHWHERE << "impossible arm value " << fOwner->GetArm() << endl;
        *pzg = 0.;
      }

    fOwner->CalculateErrors( *pe, *pxc, *pyc, pde, pdx, pdy, pdz);
    fOwner->CorrectEnergy( *pe, *pxcg, *pycg, pec ); // MM 02.11.2000
    fOwner->CorrectECore( *pecore, *pxc, *pyc, pecorec );

    MpcModule hmax = GetMaxTower();
    *pe9 = GetE9( hmax.ich );

    // Position
    GetLogWeightCG(pxcg,pycg,4.0);

    // Now do impact angle plus 2nd order corrections
    int arm = fOwner->GetArm();
    float x_skin_offset = 0.;	// used to get back onto the rectilinear grid
    float x_p1_angle_corr = 0.;	// for some reason the south has a dependency on module
    if ( arm==0 && fabs(*pycg)>=9.3575 )	// TOP_SMPC
      {
        x_skin_offset = 0.15875;	// 1/16"
        x_p1_angle_corr = 2.070245e-02;
      }
    else if ( arm==0 && fabs(*pycg)<9.3575 )	// MID_SMPC
      {
        x_skin_offset = 0.4642;
        x_p1_angle_corr = 2.070245e-02;
      }
    else if ( arm==1 && fabs(*pycg)>=7.13262 )	// TOP_NMPC
      {
        x_skin_offset = 0.4642;
        x_p1_angle_corr = 3.26463e-02;
      }
    else if ( arm==1 && fabs(*pycg)<7.13262 )	// MID_NMPC
      {
        x_skin_offset = 0.4642;
        x_p1_angle_corr = 3.26463e-02;
      }

    float x_sign = 1.0;
    if ( *pxcg<0. ) x_sign = -1.0;
    float x_angle_corr = x_sign * x_p1_angle_corr * (fabs(*pxcg)-x_skin_offset);

    float y_skin_offset = 2.0*0.15875;	// used to get back onto the rectilinear grid, = 2*1/16"
    float y_p1_angle_corr = 3.58578e-2;
    float y_sign = 1.0;
    if ( *pycg<0. ) y_sign = -1.0;
    float y_angle_corr = y_sign * y_p1_angle_corr * (fabs(*pycg)-y_skin_offset);

    *pxg = *pxcg - x_angle_corr;
    *pyg = *pycg - y_angle_corr;

    // Leakage Correction
    //MpcCalib *mpccalib = MpcCalib::instance();
    //float leak_corr = mpccalib->get_leakage_correction(arm,*pe,*pxg,*pyg);
    //*pe /= leak_corr;
    //*pe9 /= leak_corr;
    //*pecore /= leak_corr;

/*
cout << *pe << "\t" << *pec << "\t" << *pecore << "\t" << *pecorec << "\n"
     << *pxcg << "\t" << *pycg << "\t" << *pxcgmin << "\t" << *pycgmin << "\n" 
     << *pxc << "\t" << *pyc << "\t" << *pxg << "\t" << *pyg << "\t" << *pzg << "\n" 
     << *pxx << "\t" << *pxy << "\t" << *pyy << "\t" << *pchi << "\t" << *pde << "\n" 
     << *pdx << "\t" << *pdy << "\t" << *pdz << "\n" << endl;
*/

    delete [] phit;

}

// ///////////////////////////////////////////////////////////////////////////

void MpcPeakarea::GetCharV2( float* pe, float* pec, 
			     float* pecore, float* pecorec, 
			     float* pe9,
			     float* pxcg, float* pycg, 		// log center of gravity
			     float* pxcgmin, float* pycgmin, 	// chi2 min cg
			     float* pxlcg, float* pylcg, 	// linear cg
			     float* pxg, float* pyg,float* pzg, // log pos, z
			     float* pxmin, float* pymin,	// lin pos
			     float* pxl, float* pyl,	        // chi2 min pos
			     float* pxx, float* pxy, float* pyy,	// moments
			     float* pchi,float *pchicore,int *pndfcore,
			     float* pde, float* pdx, float* pdy, float* pdz,
			     float* plogxx, float* plogxy, float* plogyy,
			     bool &split, float &chi2_split, float &se1, float &sx1, float &sy1,
			     float &se2, float &sx2, float &sy2)
// This method replaces "cluster" methods GetTotalEnergy, GetMoments,
// GetCorrPos, GetGlobalPos, GetErrors and "MpcPeakarea" methods GetCGmin, GetChi2
{
//cout << "in GetChar" << endl;
    float chi, chi0;
    float e1, x1, y1, e2, x2, y2;
    
    se1=sx1=sy1=se2=sx2=sy2=0;
    chi2_split=9999;
    split=0;
    
    int nh;
    MpcModule *phit, *vv;
    vector<MpcModule>::iterator ph;
    vector<MpcModule> hl;
    //    float tmplvalue; // temporary lvalue

    *pe = 0;
    hl = fHitList;
    nh = hl.size();
    if( nh <= 0 ) return;

//cout << "nh " << nh << endl;

    phit = new MpcModule[nh];

    ph = hl.begin();
    vv = phit;
    while( ph != hl.end() ) *vv++ = *ph++;

    //    chi = fgChisq*1000; //this just means no splitting will be tried in this stage
    chi = 2.5;
    int ndf; // Gamma parameter list changed MV 28.01.00
    fOwner->Gammacore(nh, phit, &chi, &chi0, &e1, &x1, &y1, &e2, &x2, &y2, ndf,
		      split,chi2_split,se1,sx1,sy1,se2,sx2,sy2);
//     if(split){
//       cout << "split_ph1: " << se1 << ", " << sx1 << ", " << sy1 << endl;
//       cout << "split_ph2: " << se2 << ", " << sx2 << ", " << sy2 << endl;
//     }  
    if(split){
      sx1= sx1*fOwner->GetModSizex();
      sx2= sx2*fOwner->GetModSizex();

      sy1= sy1*fOwner->GetModSizey();
      sy2= sy2*fOwner->GetModSizey();
    }

    fNdf=ndf;
    *pchi=chi0;


    // Calculate CL
    // tmplvalue=fOwner->Chi2Correct(chi0, ndf)*ndf; // nh->ndf MV 28.01.00
    //if(tmplvalue>0.) fCL=TMath::Prob(tmplvalue, ndf);
    //else fCL=1.; // nothing to say about this peak area

    // Shower Center of Gravity after shower profile fit
    *pxcgmin = x1*fOwner->GetModSizex();
    *pycgmin = y1*fOwner->GetModSizey();

    *pe = GetTotalEnergy();
    
/*
    if ( fabs(*pycgmin)>25.0 || fabs(*pxcgmin)>25.0 )
      {
        cout << PHWHERE << " bad p*cgmin "
             << x1 << "\t" << *pxcgmin << "\t" 
             << y1 << "\t" << *pycgmin << endl;
      }
*/

    float xemc,yemc;
    fOwner->CorrectPosition(*pe, *pxcgmin, *pycgmin, &xemc,&yemc);
    *pecore = GetECoreV2();
    
    GetLinearMoments( pxlcg, pylcg, pxx, pxy, pyy);
    float tx,ty,txx,tyy,txy;
    GetMoments( &tx, &ty, &txx, &txy, &tyy);
    GetLogMoments( pxcg, pycg, plogxx, plogxy, plogyy,4.0);

    int ndf_2 = 1;
    float chicore = fOwner->ClusterChisqcoreV2(nh, phit, *pe, *pxcgmin/(float)fOwner->GetModSizex(),*pycgmin/(float)fOwner->GetModSizey(), ndf_2);
    //    cout << chicore << endl;
    *pndfcore = (ndf_2>0?ndf_2:1);
    *pchicore = chicore/(float)(*pndfcore);

    
    if(*pchicore>0.) fCL=TMath::Prob(chicore, *pndfcore);
    else fCL=1.; // nothing to say about this peak area


    int ndf_1 = 1;
    float chi2_orig = fOwner->ClusterChisqV2(nh, phit, *pe, *pxcgmin/(float)fOwner->GetModSizex(),*pycgmin/(float)fOwner->GetModSizey(), ndf_1);
    //    cout << chicore << endl;
    ndf_1 = (ndf_1>0?ndf_1:1);
    *pchi = chi2_orig/(float)(ndf_1);

    // cout << "linear c.g: ";
    //cout << "x: " << *pxc << ", " << tx;
    //cout << "-- y: " << *pyc << ", " << ty << endl;
    
    if ( fOwner->GetArm()==0 ) *pzg = -220.947;
    else if ( fOwner->GetArm()==1 ) *pzg = 220.947;
    else
      {
        cout << PHWHERE << "impossible arm value " << fOwner->GetArm() << endl;
        *pzg = 0.;
      }

    *pxl = *pyl = 0.;	// in this instance, pxl, pyl are not used
    fOwner->CalculateErrors( *pe, *pxl, *pyl, pde, pdx, pdy, pdz);
    //    fOwner->CorrectEnergy( *pe, *pxcg, *pycg, pec ); // MM 02.11.2000
    *pec = *pe; //rather than correct we just use it as is


    MpcModule hmax = GetMaxTower();
    *pe9 = GetE9( hmax.ich );

    // Position
    //float test_x, test_y;
    //GetLogWeightCG(&test_x,&test_y,4.0);
    //cout << "log c.g: ";
    //cout << "x: " << *pxcg << ", " << test_x;
    //cout << "-- y: " << *pycg << ", " << test_y << endl;
    


    // Now do impact angle plus 2nd order corrections
    int arm = fOwner->GetArm();
    float x_skin_offset = 0.;	// used to get back onto the rectilinear grid
    float x_p1_angle_corr = 0.;	// for some reason the south has a dependency on module
    if ( arm==0 && fabs(*pycg)>=9.3575 )	// TOP_SMPC
      {
        x_skin_offset = 0.15875;	// 1/16"
        x_p1_angle_corr = 2.070245e-02;
      }
    else if ( arm==0 && fabs(*pycg)<9.3575 )	// MID_SMPC
      {
        x_skin_offset = 0.4642;
        x_p1_angle_corr = 2.070245e-02;
      }
    else if ( arm==1 && fabs(*pycg)>=7.13262 )	// TOP_NMPC
      {
        x_skin_offset = 0.4642;
        x_p1_angle_corr = 3.26463e-02;
      }
    else if ( arm==1 && fabs(*pycg)<7.13262 )	// MID_NMPC
      {
        x_skin_offset = 0.4642;
        x_p1_angle_corr = 3.26463e-02;
      }

    float x_sign = 1.0;
    if ( *pxcg<0. ) x_sign = -1.0;
    float x_angle_corr = x_sign * x_p1_angle_corr * (fabs(*pxcg)-x_skin_offset);
    float x_angle_corr_lin = x_sign * x_p1_angle_corr * (fabs(*pxlcg)-x_skin_offset);
    float x_angle_corr_min = x_sign * x_p1_angle_corr * (fabs(*pxcgmin)-x_skin_offset);

    float y_skin_offset = 2.0*0.15875;	// used to get back onto the rectilinear grid, = 2*1/16"
    float y_p1_angle_corr = 3.58578e-2;
    float y_sign = 1.0;
    if ( *pycg<0. ) y_sign = -1.0;
    float y_angle_corr = y_sign * y_p1_angle_corr * (fabs(*pycg)-y_skin_offset);
    float y_angle_corr_lin = y_sign * y_p1_angle_corr * (fabs(*pylcg)-y_skin_offset);
    float y_angle_corr_min = y_sign * y_p1_angle_corr * (fabs(*pycgmin)-y_skin_offset);
    
    if(split){
      float x1_angle_corr = x_sign * x_p1_angle_corr * (fabs(sx1)-x_skin_offset);
      float x2_angle_corr = x_sign * x_p1_angle_corr * (fabs(sx2)-x_skin_offset);
      float y1_angle_corr = y_sign * y_p1_angle_corr * (fabs(sy1)-y_skin_offset);
      float y2_angle_corr = y_sign * y_p1_angle_corr * (fabs(sy2)-y_skin_offset);
    
      sx1 = sx1 - x1_angle_corr;
      sx2 = sx2 - x2_angle_corr;
      sy1 = sy1 - y1_angle_corr;
      sy2 = sy2 - y2_angle_corr;
    }
    
    
    *pxg = *pxcg - x_angle_corr;
    *pyg = *pycg - y_angle_corr;

    *pxmin = *pxcgmin - x_angle_corr_min;
    *pymin = *pycgmin - y_angle_corr_min;

    *pxl = *pxlcg - x_angle_corr_lin;
    *pyl = *pylcg - y_angle_corr_lin;


    fOwner->CorrectECore( *pecore, *pxg, *pyg, pecorec );

    // Leakage Correction
    //MpcCalib *mpccalib = MpcCalib::instance();
    //float leak_corr = mpccalib->get_leakage_correction(arm,*pe,*pxg,*pyg);
    //float leak_corr = 1;
    //*pe /= leak_corr;
    //*pe9 /= leak_corr;
    //*pecore /= leak_corr;
    //*pecorec /= leak_corr;

/*
cout << *pe << "\t" << *pec << "\t" << *pecore << "\t" << *pecorec << "\n"
     << *pxcg << "\t" << *pycg << "\t" << *pxcgmin << "\t" << *pycgmin << "\n" 
     << *pxc << "\t" << *pyc << "\t" << *pxg << "\t" << *pyg << "\t" << *pzg << "\n" 
     << *pxx << "\t" << *pxy << "\t" << *pyy << "\t" << *pchi << "\t" << *pde << "\n" 
     << *pdx << "\t" << *pdy << "\t" << *pdz << "\n" << endl;
*/

  //   if(*pe9 > 10){
//       cout << "e,ec,ecore,ecorec,e9: " << *pe << "\t"  << *pec << "\t"  << *pecore << "\t"  << *pecorec << "\t"  << *pe9 << endl << endl
// 	   << "x,ycg:       " << *pxcg << "\t"  << *pycg << endl  		// log center of gravity
// 	   << "x,ylinearcg: " << *pxlcg << "\t"  << *pylcg << endl  		// lin center of gravity
// 	   << "x,ycgmin:    " << *pxcgmin << "\t"  << *pycgmin << endl  		// chi2 min center of gravity
// 	   << "x,y:         " << *pxg << "\t"  << *pyg << endl  		// log center of gravity
// 	   << "x,ylinear  : " << *pxl << "\t"  << *pyl << endl  		// lin center of gravity
// 	   << "x,ymin:      " << *pxmin << "\t"  << *pymin << endl  		// chi2 min center of gravity
// 	   << "x,yemc_min:  " << xemc << "\t"  << yemc << endl << endl  		// chi2 min center of gravity
// 	   << "dispx,xy,dispy:       "  << *pxx << "\t"  << *pxy << "\t"  << *pyy << endl 	// moments
// 	   << "logdisp x,xy,y:       " << *plogxx << "\t"  << *plogxy << "\t"  << *plogyy << endl << endl
// 	   << "chi:         " << *pchi << endl 
// 	   << "de,dx,dy,dz: " << *pde << "\t"  << *pdx << "\t"  << *pdy << "\t"  << *pdz << endl << endl;

//    }

    delete [] phit;


}

////////////////////////////////////////////////////////////////////

float MpcPeakarea::GetChi2()
// Returns Hi2 after its minimization fluctuating CG position 
// (i.e. after shower profile fit)
{
    float chi, chi0;
    float e1, x1, y1, e2, x2, y2;
    int nh;
    MpcModule *phit, *vv;
    vector<MpcModule>::iterator ph;
    vector<MpcModule> hl;

    hl = fHitList;
    nh = hl.size();
    if( nh <= 0 ) return 0;

    phit = new MpcModule[nh];

    ph = hl.begin();
    vv = phit;
    while( ph != hl.end() ) *vv++ = *ph++;

    chi = fgChisq*1000;
    int ndf; // Gamma parameter list changed MV 28.01.00
    fOwner->Gamma(nh, phit, &chi, &chi0, &e1, &x1, &y1, &e2, &x2, &y2, ndf);
    fNdf=ndf;
    //float chicorr = fOwner->Chi2Correct(chi0, ndf); // nh->ndf MV 28.01.00

    delete [] phit;
    return chi0;
}

void MpcPeakarea::GetLinearCG( float* px, float* py )
{
  // Gets CG coordinates before shower shape fit

  *px = fgXABSURD; 
  *py = fgYABSURD;

  int nh = fHitList.size();
  if ( nh <= 0 ) return; 
    
  int arm = fOwner->GetArm();
  double xsum = 0.;
  double ysum = 0.;
  double esum = 0.;

  vector<MpcModule>::iterator tower = fHitList.begin();
  while ( tower != fHitList.end() )
    {
      int linear_ch = (*tower).ich;
      int gridy = linear_ch/18;
      int gridx = linear_ch%18;
      int ifeech = mpcmap->getFeeCh(gridx,gridy,arm);

      double x_i = mpcmap->getX(ifeech);
      double y_i = mpcmap->getY(ifeech);
      double e_i = (*tower).amp;

      xsum += x_i*e_i;
      ysum += y_i*e_i;
      esum += e_i;

      tower++;
    }

  *px = xsum/esum;
  *py = ysum/esum;

}

// void MpcPeakarea::GetLinearMoments( float* px, float* py, float* pxx, float* pyy, float* pxy  )
// {
//   // Gets CG coordinates before shower shape fit

//   *px = fgXABSURD; 
//   *py = fgYABSURD;
//   *pxx = fgXABSURD; 
//   *pyy = fgYABSURD;
//   *pxy = fgXABSURD; 

//   int nh = fHitList.size();
//   if ( nh <= 0 ) return; 
    
//   int arm = fOwner->GetArm();
//   double xsum = 0.;
//   double ysum = 0.;
//   double xxsum = 0.;
//   double yysum = 0.;
//   double xysum = 0.;

//   double esum = 0.;

//   vector<MpcModule>::iterator tower = fHitList.begin();
//   while ( tower != fHitList.end() )
//     {
//       int linear_ch = (*tower).ich;
//       int gridy = linear_ch/18;
//       int gridx = linear_ch%18;
//       int ifeech = mpcmap->getFeeCh(gridx,gridy,arm);

//       double x_i = mpcmap->getX(ifeech);
//       double y_i = mpcmap->getY(ifeech);
//       double e_i = (*tower).amp;

//       xsum += x_i*e_i;
//       ysum += y_i*e_i;
//       xxsum+= x_i*x_i*e_i;
//       yysum+= y_i*y_i*e_i;
//       xysum+= x_i*y_i*e_i;
//       esum += e_i;

//       tower++;
//     }

//   *px = xsum/esum;
//   *py = ysum/esum;
//   *pxx = xxsum/esum;
//   *pyy = yysum/esum;
//   *pxy = xysum/esum;


// }



void MpcPeakarea::GetLogWeightCG( float* px, float* py, const double w0 )
{
  // Gets CG coordinates before shower shape fit

  *px = fgXABSURD;
  *py = fgYABSURD;

  int nh = fHitList.size();
  if ( nh <= 0 ) return;

  // First get esum
  double esum = 0.;
  vector<MpcModule>::iterator tower;
  for ( tower = fHitList.begin(); tower != fHitList.end(); tower++ )
    {
      esum += (*tower).amp;
    }
//cout << "xxx " << esum << endl;

  int arm = fOwner->GetArm();
  double xsum = 0.;
  double ysum = 0.;
  double wsum = 0.;

  tower = fHitList.begin();
  while ( tower != fHitList.end() )
    {
      int linear_ch = (*tower).ich;
      int gridy = linear_ch/18;
      int gridx = linear_ch%18;
      int ifeech = mpcmap->getFeeCh(gridx,gridy,arm);

      double x_i = mpcmap->getX(ifeech);
      double y_i = mpcmap->getY(ifeech);
      double w_i = w0 + log((*tower).amp/esum);
      if ( w_i<0. )  w_i = 0.;

      xsum += x_i*w_i;
      ysum += y_i*w_i;
      wsum += w_i;

      tower++;
    }

  *px = xsum/wsum;
  *py = ysum/wsum;

}

// // ///////////////////////////////////////////////////////////////////////////

// void MpcPeakarea::GetLogMoments( float* px, float* py, float* pxx, float* pyy, float* pxy, float w0  )
// {
//   // Gets CG coordinates before shower shape fit

//   *px = fgXABSURD; 
//   *py = fgYABSURD;
//   *pxx = fgXABSURD; 
//   *pyy = fgYABSURD;
//   *pxy = fgXABSURD; 

//   int nh = fHitList.size();
//   if ( nh <= 0 ) return; 
    
//   // First get esum
//   double esum = 0.;
//   vector<MpcModule>::iterator tower;
//   for ( tower = fHitList.begin(); tower != fHitList.end(); tower++ )
//     {
//       esum += (*tower).amp;
//     }


//   int arm = fOwner->GetArm();
//   double xsum = 0.;
//   double ysum = 0.;
//   double xxsum = 0.;
//   double yysum = 0.;
//   double xysum = 0.;

//   double wsum = 0.;

//   vector<MpcModule>::iterator tower = fHitList.begin();
//   while ( tower != fHitList.end() )
//     {
//       int linear_ch = (*tower).ich;
//       int gridy = linear_ch/18;
//       int gridx = linear_ch%18;
//       int ifeech = mpcmap->getFeeCh(gridx,gridy,arm);

//       double x_i = mpcmap->getX(ifeech);
//       double y_i = mpcmap->getY(ifeech);
//       double e_i = (*tower).amp;
//       double w_i = w0 + log((*tower).amp/esum);
//       if ( w_i<0. )  w_i = 0.;
//       xsum += x_i*w_i;
//       ysum += y_i*w_i;
//       xxsum+= x_i*x_i*w_i;
//       yysum+= y_i*y_i*w_i;
//       xysum+= x_i*y_i*w_i;
//       wsum += w_i;

//       tower++;
//     }

//   *px = xsum/esum;
//   *py = ysum/esum;
//   *pxx = xxsum/esum;
//   *pyy = yysum/esum;
//   *pxy = xysum/esum;


// }

////////////////////////////////////////////////////////////////////

float MpcPeakarea::GetCLNew()
// Conf. Level based on new chi2 calculation
{
    float xcg, ycg, xx, xy, yy;
    float e1, e2, e3, e4;
    float e1m, e2m, e3m, e4m;
    float e1p, e2p, e3p, e4p;
    float s1, s2, s3, s4;
    float dx, dy, dt, et, etot, sc;
    float chi2, pr;
    int ix0, iy0, isx, isy, ndf;

    etot = GetTotalEnergy();
    GetMoments( &xcg, &ycg, &xx, &xy, &yy );
    xcg /= fOwner->GetModSizex();
    ycg /= fOwner->GetModSizey();
    ix0 = int(xcg+0.5);
    iy0 = int(ycg+0.5);
    dx = ABS(xcg-ix0);
    dy = ABS(ycg-iy0);

    isx = 1;
    if( xcg-ix0 < 0 ) isx = -1;
    isy = 1;
    if( ycg-iy0 < 0 ) isy = -1;

    e1 = GetTowerEnergy(ix0,     iy0    );
    e2 = GetTowerEnergy(ix0+isx, iy0    );
    e3 = GetTowerEnergy(ix0+isx, iy0+isy);
    e4 = GetTowerEnergy(ix0    , iy0+isy);

    if( dy > dx ) {
      et = e2;
      e2 = e4;
      e4 = et;
      dt = dx;
      dx = dy;
      dy = dt;
    }

    e1m = e1 + e2 + e3 + e4;
    e2m = e1 + e2 - e3 - e4;
    e3m = e1 - e2 - e3 + e4;
    e4m = e4 - e3;

    e1p = 0.932;
    e2p = 0.835 - 2*dy*dy/(dy+0.099);
    e3p = 0.835 - 2*dx*dx/(dx+0.099);
    e4p = 0.02;

    sc = sqrt(0.1*0.1/etot + 0.03*0.03)/0.04;
    s1 = sc*0.02;
    sc = sqrt(0.1*0.1/etot + 0.02*0.02)/0.04;
    s2 = sc*(0.056-0.026*e2p);
    s3 = sc*(0.056-0.026*e2p);
    s4 = sc*0.03;

    chi2 = 0.;
    chi2 += (e1p*etot-e1m)*(e1p*etot-e1m)/s1/s1/etot/etot;
    chi2 += (e2p*etot-e2m)*(e2p*etot-e2m)/s2/s2/etot/etot;
    chi2 += (e3p*etot-e3m)*(e3p*etot-e3m)/s3/s3/etot/etot;
    chi2 += (e4p*etot-e4m)*(e4p*etot-e4m)/s4/s4/etot/etot;
    chi2 /= 0.7;

    ndf = 4;
    pr = TMath::Prob(chi2, ndf);
    return pr;
}

// ///////////////////////////////////////////////////////////////////////////

float MpcPeakarea::GetChi2New()
// Conf. Level based on new chi2 calculation
{
    float xcg, ycg, xx, xy, yy;
    float e1, e2, e3, e4;
    float e1m, e2m, e3m, e4m;
    float e1p, e2p, e3p, e4p;
    float s1, s2, s3, s4;
    float dx, dy, dt, et, etot, sc;
    float chi2;
    int ix0, iy0, isx, isy, ndf;

    etot = GetTotalEnergy();
    GetMoments( &xcg, &ycg, &xx, &xy, &yy );
    xcg /= fOwner->GetModSizex();
    ycg /= fOwner->GetModSizey();
    ix0 = int(xcg+0.5);
    iy0 = int(ycg+0.5);
    dx = ABS(xcg-ix0);
    dy = ABS(ycg-iy0);

    isx = 1;
    if( xcg-ix0 < 0 ) isx = -1;
    isy = 1;
    if( ycg-iy0 < 0 ) isy = -1;

    e1 = GetTowerEnergy(ix0,     iy0    );
    e2 = GetTowerEnergy(ix0+isx, iy0    );
    e3 = GetTowerEnergy(ix0+isx, iy0+isy);
    e4 = GetTowerEnergy(ix0    , iy0+isy);

    if( dy > dx ) {
      et = e2;
      e2 = e4;
      e4 = et;
      dt = dx;
      dx = dy;
      dy = dt;
    }

    e1m = e1 + e2 + e3 + e4;
    e2m = e1 + e2 - e3 - e4;
    e3m = e1 - e2 - e3 + e4;
    e4m = e4 - e3;

    e1p = 0.932;
    e2p = 0.835 - 2*dy*dy/(dy+0.099);
    e3p = 0.835 - 2*dx*dx/(dx+0.099);
    e4p = 0.02;

    sc = sqrt(0.1*0.1/etot + 0.03*0.03)/0.04;
    s1 = sc*0.02;
    sc = sqrt(0.1*0.1/etot + 0.02*0.02)/0.04;
    s2 = sc*(0.056-0.026*e2p);
    s3 = sc*(0.056-0.026*e2p);
    s4 = sc*0.03;

    chi2 = 0.;
    chi2 += (e1p*etot-e1m)*(e1p*etot-e1m)/s1/s1/etot/etot;
    chi2 += (e2p*etot-e2m)*(e2p*etot-e2m)/s2/s2/etot/etot;
    chi2 += (e3p*etot-e3m)*(e3p*etot-e3m)/s3/s3/etot/etot;
    chi2 += (e4p*etot-e4m)*(e4p*etot-e4m)/s4/s4/etot/etot;
    chi2 /= 0.7;

    ndf = 4;
    return chi2/ndf;
}

// ///////////////////////////////////////////////////////////////////////////

void MpcPeakarea::GetCGmin( float* px, float* py )
{
  // Gets CG coordinates corresponding to min Hi2 (after shower shape fit)

  float chi, chi0;
  float e1, x1, y1, e2, x2, y2;
  int nh;
  MpcModule *phit, *vv;
  vector<MpcModule>::iterator ph;
  vector<MpcModule> hl;
  
  *px = fgXABSURD;
  *py = fgYABSURD;
  hl = fHitList;
  nh = hl.size();
  if( nh <= 0 ) return;
  
  phit = new MpcModule[nh];
  
  ph = hl.begin();
  vv = phit;
  while( ph != hl.end() ) *vv++ = *ph++;
  
  chi = fgChisq*1000;
  int ndf; // Gamma parameter list changed MV 28.01.00
  fOwner->Gamma(nh, phit, &chi, &chi0, &e1, &x1, &y1, &e2, &x2, &y2, ndf);
  fNdf=ndf;
  *px = x1*fOwner->GetModSizex();
  *py = y1*fOwner->GetModSizey();
  
  delete [] phit;

}

// ///////////////////////////////////////////////////////////////////////////

int MpcPeakarea::GetGammas( MpcEmshower* ShList)
{
  // Splits the peakarea onto 1 or 2 MpcEmshower's
  // Returns Number of Showers (0, 1 or 2)
  //
  // If the Chi2 of the peakarea is less then the value determined 
  // in Chi2Limit(Number_of_Hits) function, 1-photon hypothesis is 
  // accepted, otherwise the 2-shower hypothesis is checked
  
  float chi, chi0, e1, x1, y1, e2, x2, y2;
  int nh, ig;
  MpcModule *phit, *vv;
  MpcEmshower sh1(fOwner), sh2(fOwner);
  vector<MpcModule>::iterator ph;
  vector<MpcModule> hl;
  
  hl = fHitList;
  nh = hl.size();
  if( nh <= 0 ) return 0;
  
  phit = new MpcModule[nh];
  
  ph = hl.begin();
  vv = phit;
  while( ph != hl.end() ) *vv++ = *ph++;
  
  chi=fOwner->Chi2Limit(nh); // potential problem for PbGl -- ndf is not known
                             // beforehand
  int ndf; // Gamma parameter list changed MV 28.01.00
  fOwner->Gamma(nh, phit, &chi, &chi0, &e1, &x1, &y1, &e2, &x2, &y2, ndf);
  //float chicorr = fOwner->Chi2Correct(chi, ndf); // nh->ndf MV 28.01.00
  if(e1>0)
    sh1.ReInitialize(e1, x1*fOwner->GetModSizex(), y1*fOwner->GetModSizey(),
		     chi, ndf);
  else
    sh1.ReInitialize(0, fgXABSURD, fgYABSURD, 0, 0);
  
  if( e2 > 0 )
    sh2.ReInitialize(e2, x2*fOwner->GetModSizex(), y2*fOwner->GetModSizey(),
		     chi, ndf);
  else
    sh2.ReInitialize(0, fgXABSURD, fgYABSURD, 0, ndf);
  
  if( e2 > e1 ) {
    sh1.ReInitialize(e2, x2*fOwner->GetModSizex(), y2*fOwner->GetModSizey(),
		     chi, ndf);
    sh2.ReInitialize(e1, x1*fOwner->GetModSizex(), y1*fOwner->GetModSizey(),
		     chi, ndf);
  }
  
  ig = 0;
  if( e1>0 ) {
    ShList[ig++]=sh1;
    if( e2>0 ) {
      ShList[ig++]=sh2;
    }
  }
  
  delete [] phit;
  return ig;
}

// ///////////////////////////////////////////////////////////////////////////
// MpcEmshower member functions

MpcEmshower::MpcEmshower():
  fNdf(0), 
  fCL(1.),  
  fEnergy(0.),
  fXcg(0.),
  fYcg(0.),
  fChisq(999.999)
{}

// ///////////////////////////////////////////////////////////////////////////

MpcEmshower::MpcEmshower(MpcSectorRec *sector):
  fOwner(sector),  
  fNdf(0), 
  fCL(1.),  
  fEnergy(0.),
  fXcg(0.),
  fYcg(0.),
  fChisq(999.999)
{}

// ///////////////////////////////////////////////////////////////////////////

MpcEmshower::MpcEmshower(float e, float x, float y, float chi, int ndf,
			 MpcSectorRec *sector):
  fOwner(sector),  
  fNdf(ndf), 
  fCL(1.),  // This should be calculated !!!
  fEnergy(e),
  fXcg(x),
  fYcg(y),
  fChisq(chi)
{}

// ///////////////////////////////////////////////////////////////////////////

void MpcEmshower::GetCorrPos( float* px, float* py )
// Returns MpcEmshower corrected position in Sector (SM) frame
{
   fOwner->CorrectPosition(fEnergy, fXcg, fYcg, px, py );
}

// ///////////////////////////////////////////////////////////////////////////

void MpcEmshower::GetGlobalPos( float* px, float* py, float* pz )
// Returns MpcEmshower position in PHENIX global coord system
{
   float xc, yc;

   GetCorrPos( &xc, &yc );
// X in Sector coord is Z in Global coord !!
//mchiu
   //fOwner->SectorToGlobal( xc, yc, 0, px, py, pz );
}

// ///////////////////////////////////////////////////////////////////////////

void MpcEmshower::GetErrors( float* pde, float* pdx, float* pdy, float* pdz)
// Returns the errors for the reconstructed energy and position
{
    float e, x, y;

    e = GetTotalEnergy();
    GetCorrPos( &x, &y );
    fOwner->CalculateErrors( e, x, y, pde, pdx, pdy, pdz);
}

// ///////////////////////////////////////////////////////////////////////////

void MpcEmshower::GetChar( float* pe, 
			   float* pxcg, float* pycg, 
			   float* pxc, float* pyc, 
			   float* pxg, float* pyg, float* pzg, 
			   float* pchi,
			   float* pde, float* pdx, float* pdy, float* pdz  )
{
  // This method replaces methods GetTotalEnergy, GetCG, 
  // GetCorrPos, GetGlobalPos, GetChi2, GetErrors
  
  float tmplvalue; // temporary lvalue

  *pe = GetTotalEnergy();
  GetCG( pxcg, pycg );
  fOwner->CorrectPosition( *pe, *pxcg, *pycg, pxc, pyc );
//  fOwner->SectorToGlobal( *pxc, *pyc, 0, pxg, pyg, pzg );
  fOwner->CalculateErrors( *pe, *pxc, *pyc, pde, pdx, pdy, pdz);
  *pchi=fChisq;
  
  // Calculate CL
  tmplvalue=fOwner->Chi2Correct(fChisq, fNdf)*fNdf; // nh->ndf MV 28.01.00
  if(tmplvalue>0.) fCL=TMath::Prob(tmplvalue, fNdf);
  else fCL=1.; // nothing to say about this peak area

}

// ///////////////////////////////////////////////////////////////////////////
// EOF
