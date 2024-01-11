// Name: MpcSectorRec.cxx

// MpcSectorRec -- abstract base class for both PbSc and PbGl and now MPC

// ///////////////////////////////////////////////////////////////////////////

/* This package consists of the following files:

       EmcCluster.cxx (clusters, peakareas, modules etc.)
       EmcCluster.h
       EmcGlSectorRec.cxx (PbGl specific code)
       EmcGlSectorRec.h
       EmcScSectorRec.cxx (PbSc specific code)
       EmcScSectorRec.h
       MpcSectorRec.cxx (generic interface to calorimeter reconstruction)
       MpcSectorRec.h

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
  MpcSectorRec *sector=new EmcGlSectorRec();
  
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


#include <MpcSectorRec.h>
#include <MpcCluster.h>
#include <mpcTowerContainer.h>
#include <mpcTowerContent.h>
#include <mpcClusterContainer.h>
#include <mpcClusterContent.h>
#include <MpcMap.h>

#include <PHPoint.h>
#include <recoConsts.h>

#include <TH2.h>
#include <TPad.h>

#include <cassert>
#include <cstdlib>
#include <iostream>
#include <string>


using namespace std;

// Define and initialize static members
int MpcSectorRec::moment_type = 2; 

// Minimal shower energy when splitting peakarea onto showers, used in Gamma()
float const MpcSectorRec::fgMinShowerEnergy=0.1;

// Max number of clusters in sector, used in Find_Clusters()
int const MpcSectorRec::fgMaxLen=220;

  // Parameters for sigma in Hi2 calculations (p.36-37 v.3)
float MpcSectorRec::fgEpar00 = 0.005;
float MpcSectorRec::fgEpar0 = 0.0014;
float MpcSectorRec::fgEpar1 = 0.03;
float MpcSectorRec::fgEpar2 = -0.03;
  // This is for PPLO mode !!!
float MpcSectorRec::fgEpar3 = 0.;
float MpcSectorRec::fgEpar4 = 4.0;


// Default level, now for the Conf Level: 1% for GEANT, 2%-5% for TestBeam

float MpcSectorRec::fgChi2Level[50]={
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
float MpcSectorRec::fgChi2Level1[50]={
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
float MpcSectorRec::fgChi2Level2[50]={
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
// MpcSectorRec member functions

MpcSectorRec::MpcSectorRec(const int iarm)
{
  fTowers = 0;
  fClusters = 0;
  fArm = iarm;
  SetPeakThreshold(0.08);
  SetChi2Limit(2);

  fModSizex = 2.26;
  fModSizey = 2.26;

  fHVect = new MpcModule[HVECTSIZE];

  fModules = new vector<MpcModule>;
  fClusters = new vector<MpcCluster>;

  sdisplay = 0;
  ndisplay = 0;

  mpcmap = MpcMap::instance();		// fee->position map
  
  reco_consts = recoConsts::instance(); // variables for behavior
  mpc_event_display = reco_consts->get_IntFlag("MPC_EVENT_DISPLAY",0);
  mpc_verbosity = reco_consts->get_IntFlag("MPC_VERBOSITY",0);
}

// ///////////////////////////////////////////////////////////////////////////

MpcSectorRec::~MpcSectorRec()
{
  delete [] fHVect;
  delete fModules;
  delete fClusters;
}

// ///////////////////////////////////////////////////////////////////////////

void MpcSectorRec::SetVertex(const PHPoint& p)
{
  fVx = p.getX();
  fVy = p.getY();
  fVz = p.getZ();
}

void MpcSectorRec::SetClusterOut(mpcClusterContainer *mclus)
{
  fClusterOut = mclus;
}

void MpcSectorRec::FillHitList(const mpcTowerContainer *towers)
{
  fModules->clear();
  if ( mpc_event_display )
    {
      if ( sdisplay==0&&fArm==0 )
        {
          cout << "CREATING SDISPLAY" << endl;
          //sdisplay = new TH2F("sdisplay","MPC South",18,-0.5,17.5,18,-0.5,17.5);
          Float_t offset = 8.5;
          sdisplay = new TH2F("sdisplay","MPC South",18,(-0.5-offset)*2.26,(17.5-offset)*2.26,18,(-0.5-offset)*2.26,(17.5-offset)*2.26);
          sdisplay->SetXTitle("cm");
          sdisplay->SetYTitle("cm");
        }
      if ( fArm==0 ) sdisplay->Reset();
    }

  for ( size_t itow = 0; itow < towers->size(); ++itow )
    {
      mpcTowerContent* t = towers->getTower(itow);
      int fee576ch = t->get_ch();

      // check that we are in the right arm
      if ( fArm==0 && fee576ch>=288 ) continue;
      if ( fArm==1 && fee576ch<288 ) continue;

      int xpos = mpcmap->getGridX( fee576ch );
      int ypos = mpcmap->getGridY( fee576ch );

      float energy = t->get_energy();

/*
if ( fee576ch==122||fee576ch==123||fee576ch==144 )
{
  cout << PHWHERE << "\t" << fee576ch << "\t" << xpos << "\t" << ypos << "\t" << energy << endl;
}
*/
      // skip faulty channels
      if ( !finite(energy) )
        {
          if ( mpc_verbosity>0 )
            {
              cout << "MpcSectorRec::FillHitList(), ERROR, Tower ";
              t->print(cout);
              cout << endl;
            }
          continue;
        }

      // skip low energy towers
      if ( energy<fgTowerThresh ) continue;

      MpcModule vhit(ypos*18 + xpos,
                     energy,
                     t->get_tof(),
                     0, //t->ErrorNeighbours(),	// no deadmap yet
                     0, //t->WarnNeighbours(),	// no warnmap yet
                     energy,
                     t->get_tof());

      fModules->push_back(vhit);

      if ( mpc_event_display )
        {
          Float_t x = mpcmap->getX( fee576ch );
          Float_t y = mpcmap->getY( fee576ch );

          //if ( fArm==0 ) sdisplay->Fill(xpos,ypos,energy);
          if ( fArm==0 ) sdisplay->Fill(x,y,energy);
        }
    }

  if ( mpc_event_display )
    {
      if ( fArm==0 )
        {
          sdisplay->Draw("colz");
          gPad->Modified();
          gPad->Update();
        }
    }
}

/*
void SetTowers(const mpcTowerContainer *mtow)
{
  fTowers = mtow;

  // clear the grid
  for (int ix=0; ix<fNx; ix++)
    {
      for (int iy=0; iy<fNy; iy++)
        {
          grid_towers[ix][iy] = 0;
        }
    }

  // fill the grid
  int ntowers = fTowers->size();
  for (int itow=0; itow<ntowers; itow++)
    {
      mpcTowerContent *tower = fTowers->getTower(itow);
      int fee576ch = tower->get_ch();

      if ( fArm==0 && fee576ch>=288 ) continue;
      if ( fArm==1 && fee576ch<288 ) continue;

      int xpos = mpcmap->getGridX( fee576ch );
      int ypos = mpcmap->getGridY( fee576ch );

      if ( tower->get_energy()>fgTowerThresh )
        {
          grid_towers[xpos][ypos] = tower;
        }
    }
}
*/

// ///////////////////////////////////////////////////////////////////////////

int MpcSectorRec::FindClusters()
{
  // Cluster search algorithm based on Lednev's one developed for GAMS.
  // Returns -1 if fgMaxLen parameter is too small (just increase it)
  
  int nhit, nCl;
  int LenCl[fgMaxLen];
  int next, ib, ie, iab, iae, last, LastCl, leng, ich;
  int ia = 0;
  
  MpcModule *vv;
  MpcModule *vhit, *vt;
  MpcCluster Clt(this);
  vector<MpcModule>::iterator ph;
  vector<MpcModule> hl;
  
  (*fClusters).erase(  (*fClusters).begin(),  (*fClusters).end() );
  nhit = (*fModules).size();
  
  if( nhit <= 0 ) return 0;
  if( nhit == 1 ) {
    Clt.ReInitialize( (*fModules) );
    fClusters->push_back(Clt);
    return 1;
  }

  vt = new MpcModule[nhit];
  vhit = new MpcModule[nhit];	// sorted list of hits
  
  ph = (*fModules).begin();
  vv = vhit;
  while( ph != (*fModules).end() ) *vv++ = *ph++;
  
  /* // print all of the modules
  for (int ihit=0; ihit<nhit; ihit++)
    {
      cout << "orig modules " << " " << vhit[ihit].ich << " " << vhit[ihit].adc << " " << vhit[ihit].tac << endl;
    }
  */

  // sort by difference in linear id
  qsort( vhit, nhit, sizeof(MpcModule), HitNCompare );
  
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

  if( nCl > 0 )
    {
      ib=0;
      for( int iCl=0; iCl<nCl; iCl++ )
        { 
          leng=LenCl[iCl];
          hl.erase( hl.begin(), hl.end() );
          for ( ich=0; ich<leng; ich++ )
            {
              hl.push_back(vhit[ib+ich]);
            }
          Clt.ReInitialize(hl);
          ib += LenCl[iCl];
          fClusters->push_back(Clt);

      /* // Print all "clustered" modules
      MpcModule ppp[1000];
      int nnn = 1000;
      Clt.GetHits(ppp,nnn);
      int nhhh = Clt.GetNofHits();
      cout << "XXX orig 2 nhits " << nhhh << endl;
      for (int i=0; i<nhhh; i++)
        {
          cout << "orig2 modules " << i << " " << ppp[i].ich << " " << ppp[i].adc << " " << ppp[i].tac << endl;
        }
      */

        }
    }
  delete [] vhit;
  delete [] vt;

  return nCl;
  
}

// ///////////////////////////////////////////////////////////////////////////
mpcClusterContent *MpcSectorRec::FillPeakArea(MpcPeakarea& pp, MpcCluster& cluster)
{
//cout << "In FillPeakArea" << endl;
  MpcModule hmax = pp.GetMaxTower();
  int ndead = pp.GetNDead();
  float qual = ndead ? 1.0 : -ndead;

//   float rmax = (hmax.amp > 0) ? 
//     hmax.amp / cluster.GetTowerEnergy(hmax.ich) : 0;

//  EmcModule himp = pp.GetImpactTower();

//   float rimp = (himp.amp > 0) ? 
//     himp.amp / cluster.GetTowerEnergy(himp.ich) : 0;

  float e,ecorr,ecore,ecorecorr,e9;
  float xcg,ycg,xcgm,ycgm;
  float xc,yc,xgl,ygl,zgl;
  float xx,xy,yy;
  float chi2;
  float de,dx,dy,dz;

  pp.GetChar(&e, &ecorr, &ecore, &ecorecorr, &e9,
              &xcg, &ycg, &xcgm, &ycgm,
              &xc, &yc, &xgl, &ygl, &zgl,
              &xx, &xy, &yy, &chi2, &de, &dx, &dy, &dz);

  //float e9 = pp.GetE9(hmax.ich);

  int nh = pp.GetNofHits();
  assert(nh<=HVECTSIZE);
  pp.GetHits(fHVect, nh);

  float e_max_cl = cluster.GetTowerEnergy(hmax.ich);

  // Principal axis dispersion (eigenvalues).
  float padisp[2];
  float pahelp = (xx + yy) * (xx + yy) - 4.0 * (xx * yy - xy * xy);
  pahelp = sqrt(abs(pahelp));
  padisp[0] = (xx + yy + pahelp) / 2.0;
  padisp[1] = (xx + yy - pahelp) / 2.0;

/*
  float vx = fVx-xgl;
  float vy = fVy-ygl;
  float vz = fVz-zgl;
*/

//  float lactual = sqrt( vx*vx + vy*vy + vz*vz );
//  float lnominal = sqrt(xgl * xgl + ygl * ygl + zgl * zgl);
//  float dd = lactual - lnominal;

//  float etof = -9999.;
  float etofmin = -9999.;
  float etofmax = -9999.;
//  float dtof = -9999.;
  float tof = -9999.;
  float tofcorr = -9999.;
  float tofmin = -9999.;
  float tofmax = -9999.;
  float tofmincorr = -9999.;
  float tofmaxcorr = -9999.;
  float tofdisp = 0;

/*
  ToF_Process(fHVect, nh,
              dd, hmax,
              &tof, &etof, &tofcorr, &dtof,
              &tofmin, &etofmin, &tofmincorr,
              &tofmax, &etofmax, &tofmaxcorr,
              tofdisp);
*/

  size_t id = fClusterOut->size();

  mpcClusterContent* clus = fClusterOut->addCluster(id);

  clus->set_multiplicity(nh);

  clus->set_id(id);
  clus->set_arm(fArm);
  clus->set_xyz(xgl,ygl,zgl);
  clus->set_dxyz(dx,dy,dz);
  clus->set_e(e);
  clus->set_e9(e9);

  clus->set_ecore(ecorecorr);
  clus->set_ecent(e_max_cl);
  clus->set_chi2(chi2);
  clus->set_tof(tof);
  clus->set_tofcorr(tofcorr);
  clus->set_tofdisp(tofdisp);
  //                  clus->set_dtof(nPeakArea, dtof);
  clus->set_quality(qual);
  clus->set_pid(0);
  clus->set_prob_photon(pp.GetCL());
  //                  clus->set_prob_neuhad(nPeakArea, 0);
  float phi = ( xgl == 0.0 && ygl == 0.0 ? 0.0 : atan2(ygl,xgl) );
  float theta = ( xgl == 0.0 && ygl == 0.0 && zgl == 0.0 ? 0.0 :
                  atan2(sqrt(xgl*xgl+ygl*ygl),zgl) );
  clus->set_theta(theta);
  clus->set_phi(phi);
  int iy = hmax.ich / fNx;
  int iz = hmax.ich - iy * fNx;
  clus->set_ipos(iz,iy);	// cent arm (iz,iy)->(ix,iy) mpc
  clus->set_tofmin(tofmin);
  clus->set_etofmin(etofmin);
  clus->set_tofcorrmin(tofmincorr);
  clus->set_tofmax(tofmax);
  clus->set_etofmax(etofmax);
  clus->set_tofcorrmax(tofmaxcorr);
  //                  clus->set_tofmean(0);
  float dispy = yy;
  float dispx = xx;
  clus->set_disp(dispy,dispx);
  clus->set_padisp(padisp[1],padisp[0]);

  if ( clus->has_yz_cg() )
    {
      clus->set_yz_cg(ycg,xcg);

      float corrdispy=-9999;
      float corrdispx=-9999;

//      float zModSize = fSectorGeometries[is]->Tower_xSize;
//      float yModSize = fSectorGeometries[is]->Tower_ySize;

      const float xModSize = 2.27;
      const float yModSize = 2.27;
      
      computeCorrectedDispersion(ycg,xcg, dispy,dispx, yModSize,xModSize, corrdispy,corrdispx);

      clus->set_corrdisp(corrdispy,corrdispx);
    }



  for (int ih = 0; ih < nh; ++ih)
    {
      float esum = 0;
      if (fHVect[ih].amp <= 0)
        {
          clus->set_towerid(ih,-1);
          clus->set_partesum(ih,0);
	  
        }
      else
        {
	  int ach = fHVect[ih].ich;
	  int iy = ach / fNx;
	  int ix = ach - iy * fNx;
          
	  int towerid = mpcmap->getFeeCh(ix,iy,fArm);
          clus->set_towerid(ih, towerid);
          esum += fHVect[ih].amp;
          clus->set_partesum(ih, esum);
        }
    }




/*
  float esum = 0;
  for (int ih = 0; ih < nh; ++ih)
    {
      if (fHVect[ih].amp <= 0)
        {
          clus->set_towerid(ih,-1);
          clus->set_partesum(ih,0);

        }
      else
        {
          int ich = fHVect[ih].ich;
          int iy = ich / Nx[is];
          int iz = ich - iy * Nx[is];
          int swkey = iz + iy * 100 + 10000 * sector
            + 100000 * arm;
          int towerid = EmcIndexer::TowerID(swkey);
          clus->set_towerid(ih, towerid);
          esum += fHVect[ih].amp;
          clus->set_partesum(ih, esum);
        }
    }
*/

  clus->set_maps(hmax.deadmap,hmax.warnmap);
  clus->set_rawtdc(hmax.tac);

  return clus;
}


void  MpcSectorRec::computeCorrectedDispersion(float ycg, float xcg,
                                   float dispy, float dispx,
                                   float yModSize, float xModSize,
                                   float& corrdispy, float& corrdispx)
{

  
  float xpos = xcg/xModSize;
  float ypos = ycg/yModSize;

  corrdispy = dispy/(yModSize*yModSize);
  corrdispx = dispx/(xModSize*xModSize);

  float xposmod = xpos - floor(xpos);
  float yposmod = ypos - floor(ypos);
  corrdispx -= ( xposmod - xposmod*xposmod);
  corrdispy -= ( yposmod - yposmod*yposmod);
  corrdispx*=xModSize*xModSize;
  corrdispy*=yModSize*yModSize;

  if(dispx < 0.00001) corrdispx = 0.00001;
  if(dispy < 0.00001) corrdispy = 0.00001;
}


// ///////////////////////////////////////////////////////////////////////////
void MpcSectorRec::GetImpactAngle(float x, float y, float *sinT )
  // Get impact angle, (x,y) - position in Sector frame (cm)
// Should change x and y not in sector frame, but global frame
// everything should be in global frame!
{
//  float xVert, yVert, zVert;
//  float vx, vy, vz;

//  GlobalToSector takes global coords to sector coordinates
//  GlobalToSector( fVx, fVy, fVz, &xVert, &yVert, &zVert );

  float vz = 0.;
  if ( fArm==0 ) vz = -220.947 - fVz;
  else if ( fArm==1 ) vz = 220.947 - fVz;
  else
    {
      cout << PHWHERE << " ERROR impossible arm " << fArm << endl;
    }

//  vy = y;
//  vx = x;
  // From this point X coord in sector frame is Z coord in Global Coord System !!!
  *sinT = sqrt((x*x+y*y)/(x*x+y*y+vz*vz));
}

// ///////////////////////////////////////////////////////////////////////////

void MpcSectorRec::Gamma(int nh, MpcModule* phit, float* pchi, float* pchi0,
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
  
  SetProfileParameters(0, e1,x1,y1);

  chisave = *pchi;
  chi = *pchi;
  // ClusterChisq parameter list changed MV 28.01.00
  chi0 = ClusterChisq(nh, phit, e1, x1, y1, ndf);

  chisq0 = chi0;
  dof = ndf; // nh->ndf MV 28.01.00

  // ndf=0 means the cluster's chi2 cannot be found; in this case chi0=0.
  if( dof < 1 ) dof=1;
  chi = chisq0/dof;
  x0 = x1;
  y0 = y1;
  for(;;){

    chir = ClusterChisq(nh, phit, e1, x0+dxy, y0, ndf);
    chil = ClusterChisq(nh, phit, e1, x0-dxy, y0, ndf);
    chiu = ClusterChisq(nh, phit, e1, x0, y0+dxy, ndf);
    chid = ClusterChisq(nh, phit, e1, x0, y0-dxy, ndf);
    
    if( (chi0 > chir) || (chi0 > chil) ) {
      stepx = dxy;
      if( chir > chil ) stepx = -stepx;
    }
    else {
      stepx = 0;
      parx = chir+chil-2*chi0;
      if( parx > 0 ) stepx = -dxy*(chir-chil)/2/parx;
    }
    
    if( (chi0 > chiu) || (chi0 > chid) ) {
      stepy = dxy;
      if( chiu > chid ) stepy = -stepy;
    }
    else {
      stepy = 0;
      pary = chiu+chid-2*chi0;
      if( pary > 0 ) stepy = -dxy*(chiu-chid)/2/pary;
    }
    if( (MpcCluster::ABS(stepx) < stepmin) && (MpcCluster::ABS(stepy) < stepmin) ) break;
    chi00 = ClusterChisq(nh, phit, e1, x0+stepx, y0+stepy, ndf);

    if( chi00 >= chi0 ) break;
    chi0 = chi00;
    x0 += stepx;
    y0 += stepy;
  }
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

void MpcSectorRec::Mom1(int nh, MpcModule* phit, float* pe, float* px,
		       float* py)
{
  // First momentum calculation

  float a, xw, yw, e;
  int ix, iy;
  MpcModule* p;
  
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

void MpcSectorRec::Momenta(int nh, MpcModule* phit, float* pe, float* px,
			  float* py, float* pxx, float* pyy, float* pyx )
{
  // First and second momenta calculation
  
  float a, x, y, e, xx, yy, yx;
  int ix, iy, i;
  MpcModule* p;
  
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


void MpcSectorRec::Mom1Log(int nh, MpcModule* phit, float* pe, float* px,
			  float* py, float weight)
{

  float a, x, y, e, wsum;
  int ix, iy, i;
  MpcModule* p;
  
  *pe=0;
  *px=0;
  *py=0;
  if( nh <= 0 ) return;
  
  p=phit;
  x=0;
  y=0;
  e=0;
  wsum = 0;
  
  float esum = 0;
  
  cout << "in mom1Log\n";
  //first we get the total energy
  for( i=0; i<nh; i++ ) {
    a = p->amp;
    esum+=a;
    p++;
  }
  cout << "nhits,etot are: " << nh << ", " << esum << "\n";
  if(esum <= 0) return;
  p=phit;
  
  for( i=0; i<nh; i++ ) {
    
    a = p->amp;
    float w_i = a/esum; //use linear by default
    if(weight > 0){ //use log weighting
      
      w_i = weight + log(a/esum);
      if(w_i < 0) w_i = 0;
    }
    
    iy = p->ich / fNx;
    ix = p->ich - iy*fNx;
    e += a;
    int ifeech = mpcmap->getFeeCh(ix,iy,fArm);
    float x_val = mpcmap->getX(ifeech)/fModSizex;
    float y_val = mpcmap->getY(ifeech)/fModSizey;
    
    x += x_val*w_i;
    y += y_val*w_i;
    wsum+=w_i;
    cout << "x,y, weight: " << x << ", " << y << ", " << w_i << "\n";
    p++;
  }
  *pe = e;
  if( e <= 0 ) return;
  if(wsum <= 0) return;

  *px = x/wsum;
  *py = y/wsum;
  
  cout << "e,x,y: " << *pe << ", " << *px << ", " << *py << "\n";

  return;
}

void MpcSectorRec::Mom1Linear(int nh, MpcModule* phit, float* pe, float* px,
			  float* py)
{

  float a, x, y, e;
  int ix, iy, i;
  MpcModule* p;
  
  *pe=0;
  *px=0;
  *py=0;
  if( nh <= 0 ) return;
  
  p=phit;
  x=0;
  y=0;
  e=0;

  float esum = 0;
  
  //first we get the total energy
  for( i=0; i<nh; i++ ) {
    a = p->amp;
    esum+=a;
    p++;
  }
  if(esum <= 0) return;
  p=phit;
  
  for( i=0; i<nh; i++ ) {
    
    a = p->amp;
    float w_i = a/esum; //use linear by default

    iy = p->ich / fNx;
    ix = p->ich - iy*fNx;
    e += a;
    int ifeech = mpcmap->getFeeCh(ix,iy,fArm);
    float x_val = mpcmap->getX(ifeech)/fModSizex;
    float y_val = mpcmap->getY(ifeech)/fModSizey;
    
    x += x_val*w_i;
    y += y_val*w_i;
    p++;
  }
  *pe = e;
  if( e <= 0 ) return;
  
  *px = x;
  *py = y;


  return;
}

// ///////////////////////////////////////////////////////////////////////////

//call this rather than the poorly named momenta function
void MpcSectorRec::MomentsLog(int nh, MpcModule* phit, float* pe, float* px,
				float* py, float* pxx, float* pyy, 
				float* pyx,float weight)
{
  // First and second moment calculation


  float a, x, y, e, xx, yy, yx;
  int ix, iy, i;
  MpcModule* p;
  
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
  
  float esum = 0;
  float wsum = 0;
  //first we get the total energy
  for( i=0; i<nh; i++ ) {
    a = p->amp;
    esum+=a;
    p++;
  }
  if(esum <= 0) return;
  p=phit;
  
  for( i=0; i<nh; i++ ) {
    
    a = p->amp;
    float w_i = a/esum; //use linear by default
    if(weight > 0){ //use log weighting
      
      w_i = weight + log(a/esum);
      if(w_i < 0) w_i = 0;
    }
    
    iy = p->ich / fNx;
    ix = p->ich - iy*fNx;
    e += a;
    int ifeech = mpcmap->getFeeCh(ix,iy,fArm);
    float x_val = mpcmap->getX(ifeech)/fModSizex;
    float y_val = mpcmap->getY(ifeech)/fModSizey;
    
    x += x_val*w_i;
    y += y_val*w_i;
    xx += w_i*x_val*x_val;
    yy += w_i*y_val*y_val;
    yx += w_i*y_val*x_val;
    wsum+=w_i;
    p++;
  }
  *pe = e;
  if( e <= 0 || wsum <=0) return;

  xx = xx/wsum;
  yy = yy/wsum;
  yx = yx/wsum;
  x = x/wsum;
  y = y/wsum;
  *px = x;
  *py = y;
  *pxx = xx - x*x;
  *pyy = yy - y*y;
  *pyx = yx - x*y;
  

}
// ///////////////////////////////////////////////////////////////////////////

void MpcSectorRec::MomentsLinear(int nh, MpcModule* phit, float* pe, float* px,
				float* py, float* pxx, float* pyy, 
				float* pyx)
{
  // First and second momenta calculation


  float a, x, y, e, xx, yy, yx;
  int ix, iy, i;
  MpcModule* p;
  
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
  
  float esum = 0;
  float wsum = 0;
  //first we get the total energy
  for( i=0; i<nh; i++ ) {
    a = p->amp;
    esum+=a;
    p++;
  }
  if(esum <= 0) return;
  p=phit;
  
  for( i=0; i<nh; i++ ) {
    
    a = p->amp;
    float w_i = a/esum; //use linear by default

    
    iy = p->ich / fNx;
    ix = p->ich - iy*fNx;
    e += a;
    int ifeech = mpcmap->getFeeCh(ix,iy,fArm);
    float x_val = mpcmap->getX(ifeech)/fModSizex;
    float y_val = mpcmap->getY(ifeech)/fModSizey;
    
    x += x_val*w_i;
    y += y_val*w_i;
    xx += w_i*x_val*x_val;
    yy += w_i*y_val*y_val;
    yx += w_i*y_val*x_val;
    wsum+=w_i;
    p++;
  }
  *pe = e;
  if( e <= 0 || wsum < 0) return;

  xx = xx/wsum;
  yy = yy/wsum;
  yx = yx/wsum;
  x = x/wsum;
  y = y/wsum;
  *px = x;
  *py = y;
  *pxx = xx - x*x;
  *pyy = yy - y*y;
  *pyx = yx - x*y;

}
// ///////////////////////////////////////////////////////////////////////////



// Static functions

int MpcSectorRec::HitNCompare(const void* h1, const void* h2)
{

  return ( ((MpcModule*)h1)->ich - ((MpcModule*)h2)->ich );

}

// ///////////////////////////////////////////////////////////////////////////

int MpcSectorRec::HitACompare(const void* h1, const void* h2)
{

  float amp1 = ((MpcModule*)h1)->amp;
  float amp2 = ((MpcModule*)h2)->amp;
  return (amp1<amp2) ? 1: (amp1>amp2) ? -1 : 0;

}

// ///////////////////////////////////////////////////////////////////////////

void MpcSectorRec::ZeroVector(int* v, int N)
{

  int* p = v;
  for(int i=0; i<N; i++) *p++ = 0;

}

// ///////////////////////////////////////////////////////////////////////////

void MpcSectorRec::ZeroVector(float* v, int N)
{

  float* p = v;
  for(int i=0; i<N; i++) *p++ = 0;

}

// ///////////////////////////////////////////////////////////////////////////

void MpcSectorRec::ZeroVector(MpcModule* v, int N)
{
  memset(v, 0, N*sizeof(MpcModule));
}

// ///////////////////////////////////////////////////////////////////////////

void MpcSectorRec::ResizeVector(int* Vector, int OldSize, int NewSize)
{

  int* vsave;

  if( OldSize <= 0 ) { Vector = new int[NewSize]; return; }
  vsave = new int[OldSize];
  CopyVector( Vector, vsave, OldSize );
  delete [] Vector;
  Vector = new int[NewSize];
  if( NewSize > OldSize ) CopyVector( vsave, Vector, OldSize );
  else 			CopyVector( vsave, Vector, NewSize );
  delete [] vsave;
  return;

}

// ///////////////////////////////////////////////////////////////////////////

void MpcSectorRec::CopyVector(int* from, int* to, int N)
{

  if( N <= 0 ) return;
  for( int i=0; i<N; i++ ) to[i]=from[i];

}

// ///////////////////////////////////////////////////////////////////////////

void MpcSectorRec::CopyVector(MpcModule* from, MpcModule* to, int N)
{

  if( N <= 0 ) return;
  for( int i=0; i<N; i++ ) to[i]=from[i];

}

// ///////////////////////////////////////////////////////////////////////////

void MpcSectorRec::c3to5(float e0, float x0, float y0, float eps,
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

void MpcSectorRec::SetChi2Limit(int limit)
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

void MpcSectorRec::CorrectEnergy(float Energy, float x, float y, 
				   float* Ecorr)
{
  // Corrects the EM Shower Energy for attenuation in fibers and 
  // long energy leakage
  //
  // (x,y) - impact position (cm) in Sector frame

  float sinT;
  float att, leak, corr;
  const float leakPar = 0.0033; // parameter from fit
  const float attPar = 120; // Attenuation in module (cm)
  const float X0 = 2; // radiation length (cm)

  *Ecorr = Energy;
  if( Energy < 0.01 ) return;

  GetImpactAngle(x, y, &sinT); // sinT not used so far
  leak = 2-sqrt(1+leakPar*log(1+Energy)*log(1+Energy));
  att = exp(log(Energy)*X0/attPar);
  corr = leak*att;
  *Ecorr = Energy/corr;
}

/////////////////////////////////////////////////////////////////

void MpcSectorRec::CorrectECore(float Ecore, float x, float y, float* Ecorr)
{
  // Corrects the EM Shower Core Energy for attenuation in fibers, 
  // long energy leakage and angle dependance
  //
  // (x,y) - impact position (cm) in Sector frame

  float ec, ec2, corr;
  float sinT;
  const float par1 = 0.918;
  const float par2 = 1.35;
  const float par3 = 0.003;

  *Ecorr = Ecore;
  if( Ecore < 0.01 ) return;

  GetImpactAngle(x, y, &sinT );
  corr = par1 * ( 1 - par2*sinT*sinT*sinT*sinT*(1 - par3*log(Ecore)) );
  ec = Ecore/corr;

  CorrectEnergy( ec, x, y, &ec2);
  *Ecorr = ec2;
}

/////////////////////////////////////////////////////////////////////

void MpcSectorRec::CorrectPosition(float Energy, float x, float y,
				     float* pxc, float* pyc, bool callSetPar)
{
  // Corrects the Shower Center of Gravity for the systematic error due to 
  // the limited tower size and angle shift
  //
  // Everything here is in cm. 
  // (x,y) - CG position, (*pxc,*pyc) - corrected position

  float xShift, yShift, xZero, yZero, bx, by;
  float t, x0, y0;
  int ix0, iy0;
  int signx, signy;

  if ( Energy<0. )
    {
      cout << "MpcSectorRec::CorrectPosition, ERROR, energy is " << Energy << endl;
    }

  SetProfileParameters( 0, Energy, x/GetModSizex(), y/GetModSizey() );
  if( fSinTx > 0 ) signx =  1;
  else 	   signx = -1;
  if( fSinTy > 0 ) signy =  1;
  else 	   signy = -1;
  t = 1.93+0.383*log(Energy);
  xShift = t*fSinTx;
  yShift = t*fSinTy;
  xZero=xShift-(0.417*MpcCluster::ABS(fSinTx)+1.500*fSinTx*fSinTx)*signx;
  yZero=yShift-(0.417*MpcCluster::ABS(fSinTy)+1.500*fSinTy*fSinTy)*signy;
  t = 0.98 + 0.98*sqrt(Energy);
  bx = 0.16 + t*fSinTx*fSinTx;
  by = 0.16 + t*fSinTy*fSinTy;
  
  x0 = x/GetModSizex();
  x0 = x0 - xShift + xZero;
  ix0 = MpcCluster::lowint(x0 + 0.5);
  if( MpcCluster::ABS(x0-ix0) <= 0.5 ) {
    x0 = (ix0-xZero)+bx*asinh( 2.*(x0-ix0)*sinh(0.5/bx) );
    *pxc = x0*GetModSizex();
  }
  else {
    *pxc =  x - xShift*GetModSizex();
    printf("????? Something wrong in CorrectPosition of EMCalClusterChi2: x=%f  dx=%f\n", x, x0-ix0);
  }

  y0 = y/GetModSizey();
  y0 = y0 - yShift + yZero;
  iy0 = MpcCluster::lowint(y0 + 0.5);

  if( MpcCluster::ABS(y0-iy0) <= 0.5 ) {

    y0 = (iy0-yZero)+by*asinh( 2.*(y0-iy0)*sinh(0.5/by) );
    *pyc = y0*GetModSizey();

  }
  else {

    *pyc = y - yShift*GetModSizey();
    printf("????? Something wrong in CorrectPosition of EMCalClusterChi2: y=%f  dy=%f\n", y, y0-iy0);

  }
  
  //cout << PHWHERE << " in CorrectPositionEnd " << x << "\t" << y << "\t" << *pxc << "\t" << *pyc << endl;
}

// ///////////////////////////////////////////////////////////////////////////

void MpcSectorRec::CalculateErrors( float e, float x, float y, float* pde,
				   float* pdx, float* pdy, float* pdz)
{
  // Returns the errors for the reconstructed energy and position 
  // (in the hypothesis of EM shower)
  // Should be called only just after CorrectPosition !!!

  float de, dy, dz;
//float  dxg, dyg, dzg;
  static float ae = 0.076, be = 0.022;  	// de/E = a/sqrt(E)&b
  static float a = 0.57, b = 0.155, d = 1.6;	// dx = a/sqrt(E)+b (cm)
  static float dx = 0.1;  // (cm)
  
  de = sqrt( ae*ae*e + be*be*e*e );
  dz = a/sqrt(e) + b;
  dy = dz;
  dz = sqrt( dz*dz + d*d*fSinTx*fSinTx );
  dy = sqrt( dy*dy + d*d*fSinTy*fSinTy );
  
//  SectorToGlobalErr( dx, dy, dz, &dxg, &dyg, &dzg );
  
  *pde = de;
  *pdx = dx;
  *pdy = dy;
  *pdz = dz;
/*
  *pdx = dxg;
  *pdy = dyg;
  *pdz = dzg;
*/

}

// ///////////////////////////////////////////////////////////////////////////

void MpcSectorRec::SetProfileParameters(int sec, float Energy, float x,
					float y )
{
  // Axis Z here means X in two dim Sector coord system !!! 
  // So one has to supply (x,y) coordinates in cell units (x instead of z)
  // If sec < 0 this routine changes only Energy dependent parameters - 
  // the angle dependent ones are set in the previous call

  float t;
  static float sin2ax, sin2ay, sin2a, lgE;
  float vx, vy, vz;
//  float xVert, yVert, zVert;
  int sign;
  
  if( sec >= 0 ) {
    // Vertex coordinates in Sector coordinate system
//    GlobalToSector( fVx, fVy, fVz, &xVert, &yVert, &zVert );
// chiu
    if ( fArm==0 )
      {
        vz = -220.947 - fVz;
      }
    else if ( fArm==1 ) 
      {
        vz = 220.947 - fVz;
      }
    else 
      {
        cout << PHWHERE << " must have fArm set" << endl;
        return;
      }

    // It is if we use the coordinates of the Sector Center (not zero-tower)
    vy = y*fModSizey - fVy;
    vx = x*fModSizex - fVx;

    if ( mpc_verbosity>0 )
      {
        cout << "vz " << vx << "\t" << vy << "\t" << vz << endl;
      }

    // From this point X coord in sector frame is Z coord in Global Coord System !!!
    fSinTx = vx/sqrt(vx*vx+vz*vz);
    fSinTy = vy/sqrt(vy*vy+vz*vz);
    if( MpcCluster::ABS(fSinTx) > 0.7 || MpcCluster::ABS(fSinTy) > 0.7 ) 
      {
        cout << "!!!!! Something strange in SetProfileParameters of mMpcClusterChi2: fSinTx = "
             << fSinTx << "\tfSinTy = " << fSinTy << endl;
      }
    t = (vx*vx+vy*vy)/(vx*vx+vy*vy+vz*vz);
    sin2a=t;
    fSin4T=t*t;
    sin2ax=fSinTx*fSinTx;
    sin2ay=fSinTy*fSinTy;
  }
  
  if( Energy <= 1.e-10 ) lgE=0;
  else lgE=log(Energy);
  
  fPpar1=0.59-(1.45+0.13*lgE)*sin2a;
  fPpar2=0.265+(0.80+0.32*lgE)*sin2a;
  fPpar3=0.25+(0.45-0.036*lgE)*sin2a;
  fPpar4=0.42;
  
  if( fSinTx > 0 ) sign = 1;
  else sign = -1;
  fPshiftx = (1.05+0.12*lgE) * sin2ax * sign;
  
  if( fSinTy > 0 ) sign = 1;
  else sign = -1;
  fPshifty = (1.05+0.12*lgE) * sin2ay * sign;
}

// ///////////////////////////////////////////////////////////////////////////

float MpcSectorRec::PredictEnergy(float xc, float yc, float en)
{
  // Calculates the energy deposited in the tower, the distance between 
  // its center and shower Center of Gravity being (xc,yc)
  // en - shower energy
  // If en<0 -> no Shower Profile parameters change is needed

  float dx, dy, r1, r2, r3, e;
  
  if( en > 0 ) SetProfileParameters(-1,en,xc,yc);
  dx=fabs(xc-fPshiftx);
  dy=MpcCluster::ABS(yc-fPshifty);
  e=0;
  r2=dx*dx+dy*dy;
  r1=sqrt(r2);
  r3=r2*r1;
  e=fPpar1*exp(-r3/fPpar2)+fPpar3*exp(-r1/fPpar4);
  
  return e;

}

// ///////////////////////////////////////////////////////////////////////////

void MpcSectorRec::TwoGamma(int nh, MpcModule* phit, float* pchi, float* pe1,
			   float* px1, float* py1, float* pe2, float* px2,
			   float* py2)
{

  float e0, x0, y0, xx, yy, yx;
  float dxy, rsg2, rsq;
  float dxc, dyc, r, epsc;
  int ix, iy, ixy, in, iter, dof;
  double step;
  float cosi, chisq2, u;
  //float step, cosi, chisq2, u;
  float e1c, x1c, y1c, e2c, x2c, y2c;
  float eps0 = 0.0;
  float eps1, eps2, chisqc, ex;
  float dx1, dy1, dx2, dy2, a0, d;
  float dchi, dchi0, dd, dchida, a1, a2;
  float gr = 0.0;
  float grec, grxc, gryc, grc, gx1, gx2, gy1, gy2;
  float gre = 0.0;
  float grx = 0.0;
  float gry = 0.0;
  float scal;
  float dx0 = 0.0;
  float dy0 = 0.0;
  
  const float epsmax=0.9999;
  const float stpmin=0.025;
  const float delch=2;
  
  Momenta(nh,phit,&e0,&x0,&y0,&xx,&yy,&yx);
  *pe2 = 0;
  *px2 = 0;
  *py2 = 0;
  if( nh <= 0 ) return;
  //  choosing of the starting point
  dxy = xx-yy;
  rsg2 = dxy*dxy + 4*yx*yx;
  if( rsg2 < 1e-20 ) rsg2 = 1e-20;
  rsq = sqrt(rsg2);
  dxc = -sqrt((rsq+dxy)*2);
  dyc =  sqrt((rsq-dxy)*2);
  if( yx >= 0 ) dyc = -dyc;
  r = sqrt(dxc*dxc + dyc*dyc);
  epsc = 0;
  for( in=0; in<nh; in++ ) {
    ixy = phit[in].ich;
    iy = ixy/fNx;
    ix = ixy - iy*fNx;
    u = (ix-x0)*dxc/r + (iy-y0)*dyc/r;
    epsc -= phit[in].amp * u * MpcCluster::ABS(u);
  }
  epsc /= (e0*rsq);
  if( epsc >  0.8 ) epsc = 0.8;
  if( epsc < -0.8 ) epsc =-0.8;
  dxc /= sqrt(1-epsc*epsc);
  dyc /= sqrt(1-epsc*epsc);
  //  Start of iterations
  step = 0.1;
  cosi = 0;
  chisq2 = 1.e35;
  for( iter=0; iter<100; iter++)
    {
      c3to5(e0,x0,y0,epsc,dxc,dyc,&e1c,&x1c,&y1c,&e2c,&x2c,&y2c);
      eps1 = (1+epsc)/2;
      eps2 = (1-epsc)/2;
      chisqc = 0;
      for( in=0; in<nh; in++ ) {
	ex = phit[in].amp;
	ixy = phit[in].ich;
	iy = ixy/fNx;
	ix = ixy - iy*fNx;
	dx1 = x1c - ix;
	dy1 = y1c - iy;
	dx2 = x2c - ix;
	dy2 = y2c - iy;
	a0 = e1c*PredictEnergy(dx1, dy1, e1c) + e2c*PredictEnergy(dx2, dy2, e2c);
	d = fgEpar00*fgEpar00 + e0*( fgEpar1*a0/e0 + fgEpar2*a0*a0/e0/e0 +fgEpar3*a0*a0*a0/e0/e0/e0 ) + e0*sqrt(e0)*fgEpar4*a0/e0*(1-a0/e0)*fSin4T + e0*e0*fgEpar0*fgEpar0;
	chisqc += (a0-ex)*(a0-ex)/d;
      }
      if( chisqc >= chisq2 ) {
	if( iter > 0 ) {
	  dchi = chisqc-chisq2;
	  dchi0 = gr*step;
	  step /= (2*sqrt(1+dchi/dchi0));
	}
	step /= 2;
      }
      else {
	// Calculation of gradient
	grec = 0;
	grxc = 0;
	gryc = 0;
	for( in=0; in<nh; in++ ) {
	  ex = phit[in].amp;
	  ixy = phit[in].ich;
	  iy = ixy/fNx;
	  ix = ixy - iy*fNx;
	  dx1 = x1c - ix;
	  dy1 = y1c - iy;
	  dx2 = x2c - ix;
	  dy2 = y2c - iy;
	  a1 = e1c*PredictEnergy(dx1,dy1,e1c);
	  a2 = e2c*PredictEnergy(dx2,dy2,e2c);
	  a0 = a1 + a2;
	  d = fgEpar00*fgEpar00 + e0*( fgEpar1*a0/e0 + fgEpar2*a0*a0/e0/e0 +fgEpar3*a0*a0*a0/e0/e0/e0 ) + e0*sqrt(e0)*fgEpar4*a0/e0*(1-a0/e0)*fSin4T + e0*e0*fgEpar0*fgEpar0;
	  dd = (a0-ex)/d;
	  dchida = dd*( 2 - dd*(fgEpar1 + 2*fgEpar2*a0/e0 + 3*fgEpar3*a0*a0/e0/e0 + e0*sqrt(e0)*fgEpar4*fSin4T*(1-2*a0/e0) + 2*fgEpar0*fgEpar0*a0) );
	  gx1 = ( e1c*PredictEnergy(x1c+0.05-ix,dy1,e1c) - a1 )*20;
	  gx2 = ( e2c*PredictEnergy(x2c+0.05-ix,dy2,e2c) - a2 )*20;
	  gy1 = ( e1c*PredictEnergy(dx1, y1c+0.05-iy,e1c) - a1 )*20;
	  gy2 = ( e2c*PredictEnergy(dx2, y2c+0.05-iy,e2c) - a2 )*20;
	  grec += (dchida*((a1/e1c-a2/e2c)*e0 - (gx1+gx2)*dxc -(gy1+gy2)*dyc)/2);
	  grxc += (dchida*(gx1*eps2-gx2*eps1));
	  gryc += (dchida*(gy1*eps2-gy2*eps1));
	}
	grc = sqrt(grec*grec + grxc*grxc + gryc*gryc);
	if( grc < 1e-10 ) grc = 1e-10;
	if( iter > 0 ) {
	  cosi = (gre*grec + grx*grxc + gry*gryc ) / (gr*grc);
	  scal = MpcCluster::ABS(gr/grc - cosi);
	  if( scal < 0.1 ) scal = 0.1;
	  step /= scal;
	}
	chisq2 = chisqc;
	eps0 = epsc;
	dx0 = dxc;
	dy0 = dyc;
	gre = grec;
	grx = grxc;
	gry = gryc;
	gr = grc;
      }
      epsc = eps0 - step*gre/gr;
      while( MpcCluster::ABS(epsc) >= epsmax ) {
	step /= 2;
	epsc = eps0 - step*gre/gr;

        // break if step becomes too small
        if ( step<1e-29 ) break;
      }
      dxc = dx0 - step*grx/gr;
      dyc = dy0 - step*gry/gr;
      if( step*gr < stpmin ) break;
    }
  if( (*pchi)*nh-chisq2 < delch ) return;
  dof = nh;
  if( dof < 1 ) dof = 1;
  *pchi = chisq2/dof;
  c3to5(e0,x0,y0,eps0,dx0,dy0,pe1,px1,py1,pe2,px2,py2);

}

// ///////////////////////////////////////////////////////////////////////////

float MpcSectorRec::ClusterChisq(int nh, MpcModule* phit, float e, float x,
				float y, int &ndf)
{

  float chi=0;
  int ixy, ix, iy;
  float et, a, d;
  
  for( int in=0; in<nh; in++ ) {
    ixy = phit[in].ich;
    iy = ixy/fNx;
    ix = ixy - iy*fNx;
    et = phit[in].amp;
    a = PredictEnergy(x-ix, y-iy, -1);
    d = fgEpar00*fgEpar00 + e*(fgEpar1*a + fgEpar2*a*a + fgEpar3*a*a*a) + 
      e*sqrt(e)*fgEpar4*a*(1-a)*fSin4T + e*e*fgEpar0*fgEpar0;
    a *= e;
    chi += (et-a)*(et-a)/d;
  }

  ndf=nh; // change needed for PbGl MV 28.01.00
  return chi;

}

// ///////////////////////////////////////////////////////////////////////////


float MpcSectorRec::Chi2Limit(int ND)
{
  //  Here the reverse Chi2Correct function is used
  
  float rn, a, b, chi2;
  
  if( ND < 1 ) return 9999.;  // Should we put 0. here?
  
  chi2 = fgChi2Level[MpcCluster::min(ND,50)-1];
  if( chi2 > 100 ) return 9999.; // Why should chi2 ever be >100?
  
  rn = ND;
  b = 0.072*sqrt(rn);
  a = 6.21/(sqrt(rn)+4.7);
  
  return chi2*a/(1.-chi2*b);

}

// ///////////////////////////////////////////////////////////////////////////

float MpcSectorRec::Chi2Correct(float Chi2, int ND)
{
  // Chi2 - is reduced Chi2: Chi2/ND !!
  // MV 02.22.2000: Actually the above is not true. The supplied value of Chi2
  // has been already divided by ND. So Chi2 here is only corrected.

  float rn, a, b, c;
  
  if( ND < 1 ) return 9999.; // Should we put 0. here?
  
  rn = ND;
  b = 0.072*sqrt(rn);
  a = 6.21/(sqrt(rn)+4.7);
  c = a + b*Chi2;
  if( c < 1 ) c=1;
  
  return Chi2/c;

}

// ///////////////////////////////////////////////////////////////////////////

void MpcSectorRec::SetTowerThreshold(float Thresh)
{
  fgTowerThresh = Thresh;
  fgEpar0 = Thresh*0.07;
  fgEpar00 = MpcCluster::max( (double)Thresh/3, 0.005 );
}

// **********************************************************************

void MpcSectorRec::getTowerPos(int ix, int iy, float &x, float & y){
  x = 2.859+5.562*ix+int(ix/12)*0.256;
  y = 2.859+5.562*iy+int(iy/12)*0.156;
}

// **********************************************************************


/// Converts coordinates in units of towers into cm's (Local coord. system)
/*
void   MpcSectorRec::TowersToSector(float xT, float yT, float & xS, float & yS){
  int   x  = int(xT);
  float dx = xT - x;
  int   y  = int(yT);
  float dy = yT - y;
  xS = fModSizex*(x+0.5) + int(xT/12)*0.256 + fModSizex*dx;
  yS = fModSizey*(y+0.5) + int(yT/12)*0.156 + fModSizey*dy;
}
*/

// **********************************************************************
/// Returns  coordinates of the tower centers in cm's (Local coord. system)
/*
void   MpcSectorRec::TowersToSector(int xT,   int yT,   float & xS, float & yS){
    xS = fModSizex*(xT+0.5) + int(xT/12)*0.256;
    yS = fModSizey*(yT+0.5) + int(yT/12)*0.156;
}
*/

// **********************************************************************
/// Converts Local Sector coordinates in cm into integer tower numbers
/*
void   MpcSectorRec::SectorToTowers(float xS, float yS, int & xT,   int & yT){
  // PbSc
  xT = int(((xS-int(xS/67.0)*67.0)-0.078)/fModSizex) + 12*int(xS/67.0);
  yT = int(((yS-int(yS/66.9)*66.9)-0.078)/fModSizey) + 12*int(yS/66.9);

}
*/

// ///////////////////////////////////////////////////////////////////////////
// EOF
