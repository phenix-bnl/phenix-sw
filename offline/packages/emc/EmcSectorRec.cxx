// Name: EmcSectorRec.cxx
// Author: A. Bazilevsky (RIKEN-BNL)
// Major modifications by M. Volkov (RRC KI) Jan 27 2000

// EmcSectorRec -- abstract base class for both PbSc and PbGl

// ///////////////////////////////////////////////////////////////////////////

/* This package consists of the following files:

       EmcCluster.cxx (clusters, peakareas, modules etc.)
       EmcCluster.h
       EmcGlSectorRec.cxx (PbGl specific code)
       EmcGlSectorRec.h
       EmcScSectorRec.cxx (PbSc specific code)
       EmcScSectorRec.h
       EmcSectorRec.cxx (generic interface to calorimeter reconstruction)
       EmcSectorRec.h

For the original description of the algorithm by A. Bazilevsky see
EmcCluster.cxx.
       The package can be used independently of anything else. Here is an
example application code (<...> means "do your stuff here").

// Beginning of example
// Local includes
#include "EmcGlSectorRec.h"

int main(int argc, char **argv)
{

<...>

  float vertex[3]; // assume that the vertex changes from event to event

  SecGeom SectorGeometry; // geometry doesn't change
  SectorGeometry.nx=17;
  SectorGeometry.ny=17;
  SectorGeometry.Tower_xSize=4.1; // cm
  SectorGeometry.Tower_ySize=4.1; // cm
  SectorGeometry.xyz0[0]=0.;
  SectorGeometry.xyz0[1]=-(SectorGeometry.ny/2-((SectorGeometry.ny+1)%2)*0.5)*
    SectorGeometry.Tower_ySize;
  SectorGeometry.xyz0[2]=-(SectorGeometry.nx/2-((SectorGeometry.nx+1)%2)*0.5)*
    SectorGeometry.Tower_xSize;
  SectorGeometry.nxyz[0]=1.;
  SectorGeometry.nxyz[1]=0.;
  SectorGeometry.nxyz[2]=0.;

<...>

  EmcPeakarea pPList[MaxNofPeaks];
  EmcModule peaks[MaxNofPeaks];
  EmcEmshower pShList[2];
  
  list<EmcModule> *moduleList=new list<EmcModule>();
  list<EmcCluster> *clusterList;
  list<EmcCluster>::iterator pc;
  EmcPeakarea* pp;
  EmcEmshower* ps;

  float e, xcg, ycg, xcgm, ycgm, xc, yc, xgl, ygl, zgl,
    xx, xy, yy, chi2, de, dx, dy, dz;
  
  
  // Create sector here. It can be either EmcGlSectorRec (PbGl) or
  // EmcScSectorRec (PbSc).
  EmcSectorRec *sector=new EmcGlSectorRec();
  
  // Sets threshold in each tower (GeV)
  sector->SetThreshold(0.02);

  //====================> Event loop <====================

  int nanal=atoi(argv[2]);
  for (int evtCount=0; evtCount<nanal; evtCount++){

<... Get event here ...>

    sector->SetGeometry(SectorGeometry, vertex);

<... Fill the list of fired modules here ...>

    sector->SetModules(moduleList);
    int ncl=sector->FindClusters();
    clusterList=sector->GetClusters();

<...>

    // Loop over clusters
    for(pc=clusterList->begin(); pc!=clusterList->end(); pc++){

<...>

      e = pc->GetTotalEnergy();
      nh = pc->GetNofHits();

<...>

      // Get PeakAreas - 2-st Clustering Level
      npk=pc->GetPeaks(pPList, peaks);
      pp=pPList;

      // Loop over peak areas
      for(int ipk=0; ipk<npk; ipk++){

        // Get peak area parameters
        pp->GetChar(&e, &xcg, &ycg, &xcgm, &ycgm, &xc, &yc, &xgl, &ygl, &zgl,
                    &xx, &xy, &yy, &chi2, &de, &dx, &dy, &dz);

<...>

        // Gets EMShower - 3-d Clustering Level
        nsh=pp->GetGammas(pShList);
        ps=pShList;
        
        for(int ish=0; ish<nsh; ish++){ // Loop over EMShowers (can be 1 or 2)
          
          e=ps->GetTotalEnergy();
          chi2=ps->GetChi2();
          ps++;
          
        } // Loop over EMShowers

        pp++; // to next peakarea
        
      } // Loop over peak areas
    } // Loop over clusters
  } // Event loop

  //=========> End of event loop <==========

  // Clean up
  if(moduleList){

    delete moduleList;
    moduleList=NULL;

  }

  if(sector){

    delete sector;
    sector=NULL;

  }

  return(0);
  
}
// End of example

MV March 6 2000.
*/

// ///////////////////////////////////////////////////////////////////////////

#include "EmcSectorRec.h"
#include "EmcCluster.h"
#include "emcTowerContent.h"
#include "EmcIndexer.h"

//INCLUDECHECKER: Removed this line: #include "PHFrame.h"
#include "PHGeometry.h"

using namespace std;

// Define and initialize static members

// Minimal shower energy when splitting peakarea onto showers, used in Gamma()
float const EmcSectorRec::fgMinShowerEnergy=0.1;

// Max number of clusters in sector, used in Find_Clusters()
int const EmcSectorRec::fgMaxLen=1000;

// Minimum Chi2 improvement required in each step to continue fitting
float const EmcSectorRec::fgMinIterStep=-0.000001;

// Max number of calls to ClusterChisq() for each cluster (soft limit)
int const EmcSectorRec::fgMaxIter=10000;

// Default level, now for the Conf Level: 1% for GEANT, 2%-5% for TestBeam

float EmcSectorRec::fgChi2Level[50]={
    6.634899, 4.605171, 3.780564, 3.318915, 3.017103,    
    2.801872, 2.639259, 2.511249, 2.407341, 2.320967,    
    2.247720, 2.184744, 2.129863, 2.081515, 2.038526,    
    1.999994, 1.965214, 1.933627, 1.904781, 1.878311,    
    1.853912, 1.831334, 1.810365, 1.790825, 1.772564, 
    1.755449, 1.739367, 1.724222, 1.709926, 1.696406,    
    1.683593, 1.671430, 1.659864, 1.648850, 1.638344,    
    1.628311, 1.618716, 1.609528, 1.600721, 1.592268,    
    1.584148, 1.576338, 1.568822, 1.561579, 1.554596,    
    1.547856, 1.541346, 1.535055, 1.528968, 1.523077 };   

// For the Conf Level: 1% for GEANT, 2%-5% for TestBeam
float EmcSectorRec::fgChi2Level1[50]={
    6.634899, 4.605171, 3.780564, 3.318915, 3.017103,    
    2.801872, 2.639259, 2.511249, 2.407341, 2.320967,    
    2.247720, 2.184744, 2.129863, 2.081515, 2.038526,    
    1.999994, 1.965214, 1.933627, 1.904781, 1.878311,    
    1.853912, 1.831334, 1.810365, 1.790825, 1.772564, 
    1.755449, 1.739367, 1.724222, 1.709926, 1.696406,    
    1.683593, 1.671430, 1.659864, 1.648850, 1.638344,    
    1.628311, 1.618716, 1.609528, 1.600721, 1.592268,    
    1.584148, 1.576338, 1.568822, 1.561579, 1.554596,    
    1.547856, 1.541346, 1.535055, 1.528968, 1.523077 };   

// For the Conf Level: 2% for GEANT, 4%-7% for TestBeam
float EmcSectorRec::fgChi2Level2[50]={
    5.411895, 3.912024, 3.278443, 2.916812, 2.677547,    
    2.505458, 2.374582, 2.271008, 2.186567, 2.116065,    
    2.056169, 2.004491, 1.959343, 1.919481, 1.883964,    
    1.852072, 1.823237, 1.797008, 1.773021, 1.750981,    
    1.730640, 1.711795, 1.694274, 1.677931, 1.662643,    
    1.648301, 1.634814, 1.622101, 1.610093, 1.598727,    
    1.587948, 1.577709, 1.567968, 1.558684, 1.549824,    
    1.541357, 1.533256, 1.525494, 1.518051, 1.510903,    
    1.504033, 1.497424, 1.491059, 1.484924, 1.479006,    
    1.473292, 1.467771, 1.462433, 1.457267, 1.452265 };

// ///////////////////////////////////////////////////////////////////////////
// EmcSectorRec member functions

EmcSectorRec::EmcSectorRec()
{
  fModules=new vector<EmcModule>;
  fClusters=new vector<EmcCluster>;
  SetPeakThreshold(0.08);
  SetChi2Limit(2);
}

// ///////////////////////////////////////////////////////////////////////////

EmcSectorRec::~EmcSectorRec()
{

  if(fModules){

    fModules->clear();
    delete fModules;

  }

  if(fClusters){

    fClusters->clear();
    delete fClusters;

  }
}

// ///////////////////////////////////////////////////////////////////////////

void  EmcSectorRec::SetGeometry(SecGeom const &geom, PHMatrix * rm, PHVector * tr )
{
  // Sets the sector geometry.
  // Should be called before Find_Clusters(...)
  // Note: static data members are initialized => has to be called after
  // a new EmcSectorRec has been created.

  emcrm = *rm; 
  emctr = *tr; 
  PHFrame local;
  PHFrame global=PHGeometry::MatrixAndVector2frames(local,emcrm,emctr);
  PHGeometry::frames2MatrixAndVector(global,local,invemcrm,invemctr);

  // The number of towers in X and Y dir.
  fNx = geom.nx;
  fNy = geom.ny;

  // Tower size in X and Y dir.
  fModSizex = geom.Tower_xSize;
  fModSizey = geom.Tower_ySize;

}

// ///////////////////////////////////////////////////////////////////////////

void EmcSectorRec::SetModules(vector<EmcModule> const *modules)
{

  *fModules=*modules;

#if 0
  // Make sure that each hit(fired module) knows what sector it belogs to
  vector<EmcModule>::iterator listIter;
  for(listIter=fModules->begin(); listIter!=fModules->end(); listIter++)
    listIter->fOwner=this;
#endif // #if 0

}

// ///////////////////////////////////////////////////////////////////////////

int EmcSectorRec::FindClusters()
{
  // Cluster search algorithm based on Lednev's one developed for GAMS.
  // Returns -1 if fgMaxLen parameter is too small (just increase it)
  
  int nhit, nCl;
  int LenCl[fgMaxLen];
  int next, ib, ie, iab, iae, last, LastCl, leng, ich;
  int ia = 0;
  
  EmcModule *vv;
  EmcModule *vhit, *vt;
  EmcCluster Clt(this);
  vector<EmcModule>::iterator ph;
  vector<EmcModule> hl;
  
  (*fClusters).erase(  (*fClusters).begin(),  (*fClusters).end() );
  nhit = (*fModules).size();
  
  if( nhit <= 0 ) return 0;
  if( nhit == 1 ) {
    Clt.ReInitialize( (*fModules) );
    fClusters->push_back(Clt);
    return 1;
  }

  vt = new EmcModule[nhit];
  vhit = new EmcModule[nhit];
  
  ph = (*fModules).begin();
  vv = vhit;
  while( ph != (*fModules).end() ) *vv++ = *ph++;
  
  qsort( vhit, nhit, sizeof(EmcModule), HitNCompare );
  
  nCl=0;
  next = 0;
  for( ich=1; ich<nhit+1; ich++ ){
    if( ich<nhit ) ia=vhit[ich].ich;
    // Protect against gluing together edges of the different rows

    if( (ia-vhit[ich-1].ich > 1) || (ich >= nhit)
	||(ia-ia/fNx*fNx == 0) ){
      ib=next;
      ie=ich-1;
      next=ich;
      if( nCl >= fgMaxLen ) {
	delete [] vt;
	delete [] vhit;
	return -1;
      }
      nCl++;
      LenCl[nCl-1]=next-ib;
      if( nCl > 1 ) {
	// Job to glue the subclusters
	iab=vhit[ib].ich;       // The last subcl begin
	iae=vhit[ie].ich;       // The last subcl end
	last=ib-1;              // The prelast subcl end
	LastCl=nCl-2;
	for( int iCl=LastCl; iCl>=0; iCl-- ){
	  leng=LenCl[iCl];

#ifdef GLUE_CLUSTER_CORNER
	  //*********** Begin: glue clusters with adjacent corner **********
	  //
	  	  if( (iab-vhit[last].ich > fNx+1) || 
	  //	      // This is if iab-channel is on the left edge
	  	      ( (iab-vhit[last].ich == fNx+1) &&
	  		(iab-iab/fNx*fNx == 0) ) ) goto new_ich;
	  	  for( int ichc=last; ichc >= last-leng+1; ichc-- ) {
	  //
	  	    if( (iab-vhit[ichc].ich >  fNx+1) ||  
	  //		// This is if iab-channel is on the left edge
	  		( (iab-vhit[ichc].ich == fNx+1) &&
	  		  (iab-iab/fNx*fNx == 0) ) ) goto new_icl;
	  //
	  	    if( (iae-vhit[ichc].ich >= fNx-1) && 
	  //		// This is if iae-channel is not on the right edge
	  		( (iae-vhit[ichc].ich != fNx-1) || 
	  		  ((iae+1)-(iae+1)/fNx*fNx != 0) ) ) {
	  //*********** End: glue clusters with adjacent corner **********
#else
	  //*********** Begin: glue clusters with adjacent edge **********
	  if( iab-vhit[last].ich > fNx ) goto new_ich;
	  for( int ichc=last; ichc >= last-leng+1; ichc-- ) {
	    if( iab-vhit[ichc].ich > fNx ) goto new_icl;
	    if( iae-vhit[ichc].ich >= fNx ) {
	      //*********** End: glue clusters with adjacent edge **********
#endif
	      CopyVector( &vhit[last+1-leng], vt, leng );
	      CopyVector( &vhit[last+1], &vhit[last+1-leng], ib-1-last )
		;
	      CopyVector( vt, &vhit[ib-leng], leng );
	      for( int i=iCl; i<nCl-2; i++ ) LenCl[i]=LenCl[i+1];
	      ib -= leng;
	      LenCl[nCl-2]=LenCl[nCl-1]+leng;
	      nCl--;
	      goto new_icl;
	    }
	  }
	new_icl:           last=last-leng;
	}
      }
    }
  new_ich:   continue;
  }
  if( nCl > 0 ) {
    ib=0;
    for( int iCl=0; iCl<nCl; iCl++ ) { 
      leng=LenCl[iCl];
      hl.erase( hl.begin(), hl.end() );
      for( ich=0; ich<leng; ich++ ) hl.push_back(vhit[ib+ich]);
      Clt.ReInitialize(hl);
      ib += LenCl[iCl];
      fClusters->push_back(Clt);
    }
  }
  delete [] vhit;
  delete [] vt;

  return nCl;
  
}

// ///////////////////////////////////////////////////////////////////////////
void EmcSectorRec::GetImpactAngle(float x, float y, float *sinT )
  // Get impact angle, (x,y) - position in Sector frame (cm)
{
  float xVert, yVert, zVert;
  float vx, vy, vz;

  GlobalToSector( fVx, fVy, fVz, &xVert, &yVert, &zVert );
  vz = -zVert;
  vy = y - yVert;
  vx = x - xVert;
  // From this point X coord in sector frame is Z coord in Global Coord System !!!
  *sinT = sqrt((vx*vx+vy*vy)/(vx*vx+vy*vy+vz*vz));
}


void EmcSectorRec::GlobalToSector(float xgl, float ygl, float zgl, float* px,
				 float* py, float* pz)
{

  PHPoint phnxHit(xgl, ygl, zgl);
  PHPoint emcHit  = PHGeometry::transformPoint(invemcrm, invemctr, phnxHit);
  *px =  emcHit.getX();
  *py =  emcHit.getY();
  *pz =  emcHit.getZ();

}

// ///////////////////////////////////////////////////////////////////////////

void EmcSectorRec::SectorToGlobal(float xsec, float ysec, float zsec,
				 float* px, float* py, float* pz ) 
{
  PHPoint emcHit(xsec, ysec, zsec);
  PHPoint phnxHit  = PHGeometry::transformPoint(emcrm, emctr, emcHit);
  *px =  phnxHit.getX();
  *py =  phnxHit.getY();
  *pz =  phnxHit.getZ();
  
}


// ///////////////////////////////////////////////////////////////////////////

void EmcSectorRec::SectorToGlobalErr( float dxsec, float dysec, float dzsec,
				     float* pdx, float* pdy, float* pdz ) 
{
  *pdx = 0.;
  *pdy = 0.;
  *pdz = 0.;
}

// ///////////////////////////////////////////////////////////////////////////


void EmcSectorRec::Gamma(int nh, emcTowerContent** phit, float* pchi, float* pchi0,
			float* pe1, float* px1, float* py1, float* pe2,
			float* px2, float* py2, int &ndf)
{
  // Tests for 1 or 2 photon hypothesis by minimizing the chi2.
  // If the energy of one of two showers is less then fgMinShowerEnergy 
  // they are merged
  //
  // Returns two shower parameters (pe1,px1,py1,pe2,px2,py2). 
  // If one shower hypothesis accepted -> *pe2=0
  // *pchi  - chi2 after splitting
  // *pchi0 - chi2 before splitting
  // ndf contains number of degrees of freedom (not equal to number of cluster
  // modules for PbGl).

  float e1, x1, y1, e2, x2, y2;
  float chi, chi0, chi00, chisq0, chisave;
  float chir, chil, chiu, chid;
  int dof;
  float x0, y0, d2, xm2;
  float stepx, stepy, parx, pary;
  const float dxy=0.06;
  const float stepmin=0.01;
  const float zTG=100;
  const float xmcut=0.0015; // (GeV), for overlapping showers separation

  *pe1=0;
  *px1=0;
  *py1=0;
  *pe2=0;
  *px2=0;
  *py2=0;
  if( nh <= 0 ) return;
  Mom1(nh,phit,&e1,&x1,&y1);
  *pe1=e1;     
  if( e1 <= 0 ) return;
  
  SetProfileParameters(0, e1,x1,y1);   // mmmmml1


  int icount = 0;
  chisave = *pchi;
  chi = *pchi;
  // ClusterChisq parameter list changed MV 28.01.00
  chi0 = ClusterChisq(nh, phit, e1, x1, y1, ndf);
  //  icount++;

  chisq0 = chi0;
  dof = ndf; // nh->ndf MV 28.01.00

  // ndf=0 means the cluster's chi2 cannot be found; in this case chi0=0.
  if( dof < 1 ) dof=1;
  chi = chisq0/dof;
  x0 = x1;
  y0 = y1;
  //  for(;;)
    while (icount < 0)
    {

      chir = ClusterChisq(nh, phit, e1, x0+dxy, y0, ndf);
      chil = ClusterChisq(nh, phit, e1, x0-dxy, y0, ndf);
      chiu = ClusterChisq(nh, phit, e1, x0, y0+dxy, ndf);
      chid = ClusterChisq(nh, phit, e1, x0, y0-dxy, ndf);
      icount+=4;
      
      if( (chi0 > chir) || (chi0 > chil) ) {
	stepx = dxy;
	if( chir > chil ) stepx = -stepx;
      }
      else 
	{
	  stepx = 0;
	  parx = chir+chil-2*chi0;
	  if( parx > 0 ) stepx = -dxy*(chir-chil)/2/parx;
	}
      
      if( (chi0 > chiu) || (chi0 > chid) ) 
	{
	  stepy = dxy;
	  if( chiu > chid ) stepy = -stepy;
	}
      else 
	{
	  stepy = 0;
	  pary = chiu+chid-2*chi0;
	  if( pary > 0 ) stepy = -dxy*(chiu-chid)/2/pary;
	}
      if( (EmcCluster::ABS(stepx) < stepmin) && (EmcCluster::ABS(stepy) < stepmin) ) break;


      chi00 = ClusterChisq(nh, phit, e1, x0+stepx, y0+stepy, ndf);
      icount++;

      if( fgMinIterStep < chi00 - chi0 ) break;
      chi0 = chi00;
      x0 += stepx;
      y0 += stepy;

      if( icount > fgMaxIter )
        {
          std::cout << "EmcSectorRec::Gamma: giving up after " << icount << " chisq calls: " << nh << " " << e1 << std::endl;
          break;
        }
    }
  
  //  std::cout << __FILE__ << " " << __LINE__ << " number of chisq calls: " << icount 
  //	    << "  " << x1 << "  " << x0 << "    " << y1 << "  " << y0 << std::endl;
  
  
  if( chi0 < chisq0 ) {
    x1 = x0;
    y1 = y0;
    chi = chi0/dof;
  }
  
  *pchi0 = chi;
  *pchi = chi;
  *px1 = x1;
  *py1 = y1;

  if( e1 <= fgMinShowerEnergy ) return;
  
  if( chi > chisave ) {
    TwoGamma(nh,phit,&chi,&e1,&x1,&y1,&e2,&x2,&y2);
    if( e2 > 0 ) {
      d2 = ((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2))/zTG/zTG;
      xm2 = e1*e2*d2;
      if( xm2 > 0 ) xm2 = sqrt(xm2);
      if( xm2 > xmcut && e1 > fgMinShowerEnergy && e2 > fgMinShowerEnergy) {
	*pe1 = e1;
	*px1 = x1;
	*py1 = y1;
	*pe2 = e2;
	*px2 = x2;
	*py2 = y2;
	*pchi = chi;
      }
    }	
  }
}








void EmcSectorRec::Gamma(int nh, EmcModule* phit, float* pchi, float* pchi0,
			float* pe1, float* px1, float* py1, float* pe2,
			float* px2, float* py2, int &ndf)
{
  // Tests for 1 or 2 photon hypothesis by minimizing the chi2.
  // If the energy of one of two showers is less then fgMinShowerEnergy 
  // they are merged
  //
  // Returns two shower parameters (pe1,px1,py1,pe2,px2,py2). 
  // If one shower hypothesis accepted -> *pe2=0
  // *pchi  - chi2 after splitting
  // *pchi0 - chi2 before splitting
  // ndf contains number of degrees of freedom (not equal to number of cluster
  // modules for PbGl).

  float e1, x1, y1, e2, x2, y2;
  float chi, chi0, chi00, chisq0, chisave;
  float chir, chil, chiu, chid;
  int dof;
  float x0, y0, d2, xm2;
  float stepx, stepy, parx, pary;
  const float dxy=0.06;
  const float stepmin=0.01;
  const float zTG=100;
  const float xmcut=0.0015; // (GeV), for overlapping showers separation

  *pe1=0;
  *px1=0;
  *py1=0;
  *pe2=0;
  *px2=0;
  *py2=0;
  if( nh <= 0 ) return;
  Mom1(nh,phit,&e1,&x1,&y1);
  *pe1=e1;     
  if( e1 <= 0 ) return;
  
  SetProfileParameters(0, e1,x1,y1); // mmmmml2


  int icount = 0;
  chisave = *pchi;
  chi = *pchi;
  // ClusterChisq parameter list changed MV 28.01.00
  chi0 = ClusterChisq(nh, phit, e1, x1, y1, ndf);
  icount++;

  chisq0 = chi0;
  dof = ndf; // nh->ndf MV 28.01.00

  // ndf=0 means the cluster's chi2 cannot be found; in this case chi0=0.
  if( dof < 1 ) dof=1;
  chi = chisq0/dof;
  x0 = x1;
  y0 = y1;
  for(;;)
    //  while (icount < 2)
    {

      chir = ClusterChisq(nh, phit, e1, x0+dxy, y0, ndf);
      chil = ClusterChisq(nh, phit, e1, x0-dxy, y0, ndf);
      chiu = ClusterChisq(nh, phit, e1, x0, y0+dxy, ndf);
      chid = ClusterChisq(nh, phit, e1, x0, y0-dxy, ndf);
      icount+=4;
      
      if( (chi0 > chir) || (chi0 > chil) ) {
	stepx = dxy;
	if( chir > chil ) stepx = -stepx;
      }
      else 
	{
	  stepx = 0;
	  parx = chir+chil-2*chi0;
	  if( parx > 0 ) stepx = -dxy*(chir-chil)/2/parx;
	}
      
      if( (chi0 > chiu) || (chi0 > chid) ) 
	{
	  stepy = dxy;
	  if( chiu > chid ) stepy = -stepy;
	}
      else 
	{
	  stepy = 0;
	  pary = chiu+chid-2*chi0;
	  if( pary > 0 ) stepy = -dxy*(chiu-chid)/2/pary;
	}
      if( (EmcCluster::ABS(stepx) < stepmin) && (EmcCluster::ABS(stepy) < stepmin) ) break;


      chi00 = ClusterChisq(nh, phit, e1, x0+stepx, y0+stepy, ndf);
      icount++;

      if( fgMinIterStep < chi00 - chi0 ) break;
      chi0 = chi00;
      x0 += stepx;
      y0 += stepy;

      if( icount > fgMaxIter )
        {
          std::cout << "EmcSectorRec::Gamma: giving up after " << icount << " chisq calls: " << nh << " " << e1 << std::endl;
          break;
        }
    }
  
  //  std::cout << __FILE__ << " " << __LINE__ << " number of chisq calls: " << icount 
  //	    << "  " << x1 << "  " << x0 << "    " << y1 << "  " << y0 << std::endl;
  
  
  if( chi0 < chisq0 ) {
    x1 = x0;
    y1 = y0;
    chi = chi0/dof;
  }
  
  *pchi0 = chi;
  *pchi = chi;
  *px1 = x1;
  *py1 = y1;

  if( e1 <= fgMinShowerEnergy ) return;
  
  if( chi > chisave ) {
    TwoGamma(nh,phit,&chi,&e1,&x1,&y1,&e2,&x2,&y2);
    if( e2 > 0 ) {
      d2 = ((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2))/zTG/zTG;
      xm2 = e1*e2*d2;
      if( xm2 > 0 ) xm2 = sqrt(xm2);
      if( xm2 > xmcut && e1 > fgMinShowerEnergy && e2 > fgMinShowerEnergy) {
	*pe1 = e1;
	*px1 = x1;
	*py1 = y1;
	*pe2 = e2;
	*px2 = x2;
	*py2 = y2;
	*pchi = chi;
      }
    }	
  }
}

// ///////////////////////////////////////////////////////////////////////////

void EmcSectorRec::Mom1(int nh, emcTowerContent** phit, float* pe, float* px,
		       float* py)
{
  // First momentum calculation

  if( nh <= 0 ) return;

  float a, xw, yw, e;
  int ix, iy;
  int iS,iST;

  emcTowerContent** p;
  
  *pe=0;
  *px=0;
  *py=0;

  p=phit;

  xw=0;
  yw=0;
  e=0;

  for( int i=0; i<nh; i++ ) {
    a = (*p)->Energy();
    EmcIndexer::iPXiSiST((*p)->TowerID(),iS,iST);
    EmcIndexer::iSTxyST(iS,iST,ix,iy);
    e += a;
    xw += ix*a;
    yw += iy*a;
    p++;
  }
  *pe = e;
  if( e <= 0 ) return;
  *px = xw/e;
  *py = yw/e;

}


// ///////////////////////////////////////////////////////////////////////////

void EmcSectorRec::Mom1(int nh, EmcModule* phit, float* pe, float* px,
		       float* py)
{
  // First momentum calculation

  float a, xw, yw, e;
  int ix, iy;
  EmcModule* p;
  
  *pe=0;
  *px=0;
  *py=0;
  if( nh <= 0 ) return;
  p=phit;
  xw=0;
  yw=0;
  e=0;
  for( int i=0; i<nh; i++ ) {
    a = p->amp;
    iy = p->ich / fNx;
    ix = p->ich - iy*fNx;
    e += a;
    xw += ix*a;
    yw += iy*a;
    p++;
  }
  *pe = e;
  if( e <= 0 ) return;
  *px = xw/e;
  *py = yw/e;

}




// ///////////////////////////////////////////////////////////////////////////

void EmcSectorRec::Momenta(int nh, emcTowerContent **phit, float* pe, float* px,
			  float* py, float* pxx, float* pyy, float* pyx )
{
  // First and second momenta calculation
  
  if( nh <= 0 ) return;

  float a, x, y, e, xx, yy, yx;
  int ix, iy, i;
  int iS,iST;

  emcTowerContent** p;
  
  *pe=0;
  *px=0;
  *py=0;
  *pxx=0;
  *pyy=0;
  *pyx=0;
  
  p=phit;
  x=0;
  y=0;
  e=0;
  xx=0;
  yy=0;
  yx=0;

  for( i=0; i<nh; i++ ) 
    {
      a = (*p)->Energy();
      EmcIndexer::iPXiSiST((*p)->TowerID(),iS,iST);
      EmcIndexer::iSTxyST(iS,iST,ix,iy);

      e += a;
      x += ix*a;
      y += iy*a;
      xx += a*ix*ix;
      yy += a*iy*iy;
      yx += a*ix*iy;
      p++;
    }
  *pe = e;
  if( e <= 0 ) return;
  x /= e;
  y /= e;
  xx = xx/e - x*x;
  yy = yy/e - y*y;
  yx = yx/e - y*x;
  
  *px = x;
  *py = y;
  *pxx = xx;
  *pyy = yy;
  *pyx = yx;

}

// ///////////////////////////////////////////////////////////////////////////

void EmcSectorRec::Momenta(int nh, EmcModule* phit, float* pe, float* px,
			  float* py, float* pxx, float* pyy, float* pyx )
{
  // First and second momenta calculation
  
  float a, x, y, e, xx, yy, yx;
  int ix, iy, i;
  EmcModule* p;
  
  *pe=0;
  *px=0;
  *py=0;
  *pxx=0;
  *pyy=0;
  *pyx=0;
  if( nh <= 0 ) return;
  
  p=phit;
  x=0;
  y=0;
  e=0;
  xx=0;
  yy=0;
  yx=0;
  for( i=0; i<nh; i++ ) {
    a = p->amp;
    iy = p->ich / fNx;
    ix = p->ich - iy*fNx;
    e += a;
    x += ix*a;
    y += iy*a;
    xx += a*ix*ix;
    yy += a*iy*iy;
    yx += a*ix*iy;
    p++;
  }
  *pe = e;
  if( e <= 0 ) return;
  x /= e;
  y /= e;
  xx = xx/e - x*x;
  yy = yy/e - y*y;
  yx = yx/e - y*x;
  
  *px = x;
  *py = y;
  *pxx = xx;
  *pyy = yy;
  *pyx = yx;

}
// ///////////////////////////////////////////////////////////////////////////
// Static functions

int EmcSectorRec::HitNCompare(const void* h1, const void* h2)
{

  return ( ((EmcModule*)h1)->ich - ((EmcModule*)h2)->ich );

}

// ///////////////////////////////////////////////////////////////////////////

int EmcSectorRec::HitACompare(const void* h1, const void* h2)
{

  float amp1 = ((EmcModule*)h1)->amp;
  float amp2 = ((EmcModule*)h2)->amp;
  return (amp1<amp2) ? 1: (amp1>amp2) ? -1 : 0;

}


// ///////////////////////////////////////////////////////////////////////////

void EmcSectorRec::ZeroVector(int* v, int N)
{

  int* p = v;
  for(int i=0; i<N; i++) *p++ = 0;

}

// ///////////////////////////////////////////////////////////////////////////

void EmcSectorRec::ZeroVector(float* v, int N)
{

  float* p = v;
  for(int i=0; i<N; i++) *p++ = 0;

}

// ///////////////////////////////////////////////////////////////////////////

void EmcSectorRec::ZeroVector(EmcModule* v, int N)
{
  memset(v, 0, N*sizeof(EmcModule));
}

// ///////////////////////////////////////////////////////////////////////////

void EmcSectorRec::CopyVector(int* from, int* to, int N)
{

  if( N <= 0 ) return;
  for( int i=0; i<N; i++ ) to[i]=from[i];

}

// ///////////////////////////////////////////////////////////////////////////

void EmcSectorRec::CopyVector(EmcModule* from, EmcModule* to, int N)
{

  if( N <= 0 ) return;
  for( int i=0; i<N; i++ ) to[i]=from[i];

}

// ///////////////////////////////////////////////////////////////////////////

void EmcSectorRec::c3to5(float e0, float x0, float y0, float eps,
		      float dx, float dy,
		      float* pe1, float* px1, float* py1,
		      float* pe2, float* px2, float* py2)
{
  // 3 to 5-dim space conversion subroutine.
  // eps=(e1-e2)/e0,  (e0=e1+e2), x0*e0=x1*e1+x2*e2, dx=x1-x2

  *pe1 = e0*(1+eps)/2;
  *pe2 = e0 - *pe1;
  *px1 = x0 + dx*(1-eps)/2;
  *py1 = y0 + dy*(1-eps)/2;
  *px2 = x0 - dx*(1+eps)/2;
  *py2 = y0 - dy*(1+eps)/2;

}

// ///////////////////////////////////////////////////////////////////////////

void EmcSectorRec::SetChi2Limit(int limit)
{
  // Sets the limit for PeakArea splitting onto 2 EMShowers:
  // limit=0 -> For the Conf Level: 0%
  // limit=1 -> For the Conf Level: 1% for GEANT, 2%-5% for TestBeam
  // limit=2 -> For the Conf Level: 2% for GEANT, 4%-7% for TestBeam
  
  int i;
  
  switch ( limit ) {

  case 0:
    for( i=0; i<50; i++ ) fgChi2Level[i]=9999.;
    break;
  case 1:
    for( i=0; i<50; i++ ) fgChi2Level[i]=fgChi2Level1[i];
    break;
  case 2:
    for( i=0; i<50; i++ ) fgChi2Level[i]=fgChi2Level2[i];
    break;
  default:
    for( i=0; i<50; i++ ) fgChi2Level[i]=fgChi2Level1[i];
    break;

  }
}

// ///////////////////////////////////////////////////////////////////////////

/* Future improvements:

1. FindClusters(): to ensure that all EmcModules are above energy threshold 
set by SetThreshold routine (or default one)

*/

// ///////////////////////////////////////////////////////////////////////////
// EOF
