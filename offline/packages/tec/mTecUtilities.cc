
//#include "gsl/gsl_rng.h"

#include "mTecUtilities.h"
#include "TecCalibrationObject.hh"
#include "TecGeometryObject.hh"
#include "PHCompositeNode.h"
#include "TecOutV1.hh"
#include "TecOutV2.hh"
#include "TecClusterContainer.hh"
#include "TecProj.hh"
#include "TClonesArray.h"
#include "PHGeometry.h"

#include <cstdlib>
#include <vector>
using namespace std;
using namespace PHGeometry;

typedef PHIODataNode < TecOutV2 > TecOutNodeS_t;
typedef PHIODataNode < TecOut > TecOutNodeShort_t;
typedef PHIODataNode < TecOutV1 > TecOutNode_t;
typedef PHIODataNode < PHObject > PHObjectNode_t;

//static gsl_rng *rng;
//static int init_a2c_done = 0;

namespace TecUtilities {

//---------------------------------------------------------------------------
	
float Ampl2Charge(int ampl1) {
  //  if(!init_a2c_done) {
  //    init_a2c_done=1;
  //    rng = gsl_rng_alloc(gsl_rng_mt19937);
  //  }
  //  unsigned long int lw, lh, ll;
  float myampl = (float)ampl1;
  float charge = 0.;
  //  lw = gsl_rng_get(rng) & 0xFFFFFFFF;
  //  ll = lw & 0x0000FFFF;
  //  lh = lw >> 16;
  //  float myrandom = ((float) ll) / ((float) 0x0000FFFF);
    if(myampl<10.0) {
      //       charge= myampl*2.0 + 2.0*myrandom;
      charge= myampl*2.0;
    }
    else {
      if(myampl>=10.0 && myampl<28.0) {
	//        charge = 20. + (myampl-10.0)*4.0 + 4.0*myrandom;
        charge = 20. + (myampl-10.0)*4.0;
      }
      else {
	//        charge = 92.0 + (myampl-28)*100. + 100.*myrandom;
        charge = 92.0 + (myampl-28)*100.;
      }
    }
    
  return charge;
}

//---------------------------------------------------------------------------

int Charge2Ampl(float charge0) {

  float cdk1=2., cdk2=4., cdk3=100.;
  float ch1=10., ch2=28.;
  int ampl0;

  if(charge0 < cdk1*ch1) {
    ampl0 = int(charge0/cdk1);
  }
    else if(charge0 < (cdk1*ch1 + cdk2*(ch2-ch1)) ) {
      ampl0=int((charge0 - cdk1*ch1)/cdk2) + int(ch1);
    }
      else {
        ampl0 = int(ch2) + int((charge0-(cdk1*ch1 + cdk2*(ch2-ch1)))/cdk3);
      }

  if(ampl0>31) {ampl0=31;}
  return ampl0;
}
	
//---------------------------------------------------------------------------
	
int GetMinMax(int MinBin[TECMAXINDEX], int MaxBin[TECMAXINDEX],
              TecCalibrationObject * TCO, int Debug) {

// Find first and last time bins from Calibration Object
  for (int i = 0; i < TECMAXINDEX; i++) {
      MinBin[i] = TCO->getFirstTimeBin (i);
      MaxBin[i] = TCO->getLastTimeBin (i);
  }

  return 0;
}


//---------------------------------------------------------------------------

int CalcXYfromBin(float relativebin, float Xwire, float Ywire, 
                  float Sinus, float &Xpos, float &Ypos, int Debug) {

  float Cosinus = sqrt (1.0 - Sinus * Sinus);

// East Arm only !!!
  Xpos = Xwire + (TecGeometryObject::get_ywidth () - 0.3) * Cosinus * relativebin;
  Ypos = Ywire + (TecGeometryObject::get_ywidth () - 0.3) * Sinus * relativebin;

  return 0;
}

//-----------------------------------------------------------------------------

int GetMidPhi(float MidPhi[TECMAXINDEX], 
	      float midphi[TECMAXSECT*TECMAXSIDE],
	      float SinAlpha[TECMAXINDEX],
              TecGeometryObject* TGO, int Debug) {

// Calculate phi for the center of each Tec chamber. EAST arm only
  for (int i = 0; i < TECMAXINDEX; i++) {
      MidPhi[i] = 0.0;
      SinAlpha[i] = 0.0;
  }
  for (int i = 0; i < TECMAXSECT*TECMAXSIDE; i++) {
      midphi[i] = 0.0;
  }

  for (int isect = 0; isect < TECMAXSECT; isect++) {
      for (int iplane = 0; iplane < TECMAXPLANE; iplane++) {
          for (int iside = 0; iside < TECMAXSIDE; iside++) {
              int index = iside + iplane*TECMAXSIDE + isect*TECMAXSIDE*TECMAXPLANE;
              PHPanel tecpanel = TGO->getTecPanel (index);
              PHVector normal = tecpanel.getNormal ();
              float x = normal.getX ();
              float y = normal.getY ();
              if (x != 0) {
                  MidPhi[index] = M_PI + atan (y / x);
              }
              else {
                  MidPhi[index] = M_PI;
              }
              SinAlpha[index] = -sin(MidPhi[index]);
          }
      }
  }

  // Calculate phi for the center of each Tec sector/side. Take
  // average for all 12 planes. North and South midphi are taken to be 
  // the same, so that onlt first 4 numbers are used.
  for (int isect = 0; isect < TECMAXSECT; isect++) {
      midphi[isect] = 0.0;

      for (int iside = 0; iside < TECMAXSIDE; iside++)
        {
          for (int iplane = 0; iplane < TECMAXPLANE; iplane++)
            {
              int index = iside + iplane * TECMAXSIDE + isect * TECMAXSIDE * TECMAXPLANE;
              midphi[isect] += MidPhi[index];
            }
        }
      midphi[isect] = midphi[isect] / (TECMAXPLANE * TECMAXSIDE);
      midphi[isect] = M_PI - midphi[isect];

    }

  return 0;

}

//---------------------------------------------------------------------------

void copyTecTrackOut(PHCompositeNode* root) {

// Find TecOutV1 node (input).

  TecOutV1* tecout = 0;
  PHTypedNodeIterator<TecOutV1> teciter(root);
  TecOutNode_t *TecOutNode = teciter.find("TecOutV1");

  if(TecOutNode) {
    tecout = (TecOutV1*)TecOutNode->getData();
  }
  else {
    cerr << PHWHERE << "ERROR: TecOutV1 node does not exist." << endl;
    exit(-1);
  }

// Try to find TecOut node (output), exit if it is not found.

  PHTypedNodeIterator<TecOut> tecitershort(root);
  TecOutNodeShort_t *TecOutNodeShort = tecitershort.find("TecOut");
  TecOut* tecoutshort = 0;

  if(TecOutNodeShort) {
    tecoutshort = TecOutNodeShort->getData();
  }
  else {        
    cerr << PHWHERE << "ERROR: TecOut node does not exist." << endl;
    exit(-2);
  }

// Copy tracks 

  int runnumber = 0;
  runnumber = tecout->getRunNumber();
  tecoutshort->setRunNumber(runnumber);

  //cout << "Copying tracks to TecOut... " << tecout->getNTracks() << endl;
  for (int i = 0; i < tecout->getNTracks(); i++) {
    TecTrack* tectrack = (TecTrack*)((tecout->GetTecTracks())->UncheckedAt(i));
    tecoutshort->AddTecTrack(*tectrack);
  }
  
  return;
}

//---------------------------------------------------------------------------

void copyTecHitOut(PHCompositeNode* root, int StripOrphanHits) {

// Find TecOutV1 node (input).

  TecOutV1* tecout = 0;
  PHTypedNodeIterator<TecOutV1> teciter(root);
  TecOutNode_t *TecOutNode = teciter.find("TecOutV1");

  if(TecOutNode) {
    tecout = (TecOutV1*)TecOutNode->getData();
  }
  else {
    cerr << PHWHERE << "ERROR: TecOutV1 node does not exist." << endl;
    exit(-1);
  }

// Try to find TecHitOut node (output), exit if it is not found.

  PHTypedNodeIterator<TecOut> tecitershort(root);
  TecOutNodeShort_t *TecOutNodeShort = tecitershort.find("TecHitOut");
  TecOut* tecoutshort = 0;

  if(TecOutNodeShort) {
    tecoutshort = TecOutNodeShort->getData();
  }
  else {      
      cerr << PHWHERE << "ERROR: TecHitOut node does not exist." << endl;
      exit(-2);
  }

// Copy hits 

  int runnumber = 0;
  runnumber = tecout->getRunNumber();
  tecoutshort->setRunNumber(runnumber);

  float charge = 0.0;
  float xyz[2];
  xyz[0] = 0.0;
  xyz[1] = 0.0;

    for (int i = 0; i < tecout->getNHits(); i++) {
      int index = tecout->getHitIndex(i);
      int wire = tecout->getHitWire(i);
      int bin = tecout->getHitTimeBin(i);
      int adc = tecout->getHitADC(i);
      int trackid = tecout->getHitTrackID(i);
      if(StripOrphanHits == 0 || trackid > -1) {
        tecoutshort->AddTecHit(index, wire, bin, adc, charge, xyz, trackid);
      }
    }

  return;
}

//---------------------------------------------------------------------------

void copyTecOut(PHCompositeNode* root, 
                int eventNumber, 
                int LastEventWithHits, 
                int StripOrphanHits) {

// Find TecOutV1 node (input).
	
  TecOutV1* tecout = 0;
  PHTypedNodeIterator<TecOutV1> teciter(root);
  TecOutNode_t *TecOutNode = teciter.find("TecOutV1");

  if(TecOutNode) {
    tecout = (TecOutV1*)TecOutNode->getData();
  }
  else {
    cerr << PHWHERE << "ERROR: TecOutV1 node does not exist." << endl;
    exit(-1);
  }

// Try to find TecOut node (output), exit if it is not found.
  
  PHTypedNodeIterator<TecOut> tecitershort(root);
  TecOutNodeShort_t *TecOutNodeShort = tecitershort.find("TecOut");
  TecOut* tecoutshort;

  if(TecOutNodeShort) {
    tecoutshort = TecOutNodeShort->getData();
  }
  else {        
      cerr << PHWHERE << "ERROR: TecOut node does not exist." << endl;
      exit(-2);
  }

// Copy TecOutV1 to TecOutV2
  
  int runnumber = 0;
  runnumber = tecout->getRunNumber();
  tecoutshort->setRunNumber(runnumber);

  float charge = 0.0;
  float xyz[2];
  xyz[0] = 0.0;
  xyz[1] = 0.0;

// Copy hits
  if(eventNumber<=LastEventWithHits) {
    for (int i = 0; i < tecout->getNHits(); i++) {
      int index = tecout->getHitIndex(i);
      int wire = tecout->getHitWire(i);
      int bin = tecout->getHitTimeBin(i);
      int adc = tecout->getHitADC(i);
      int trackid = tecout->getHitTrackID(i);
      if(StripOrphanHits == 0 || trackid > -1) {
        tecoutshort->AddTecHit(index, wire, bin, adc, charge, xyz, trackid);
      }
    }
  }

// Copy tracks
  for (int i = 0; i < tecout->getNTracks(); i++) {
      TecTrack* tectrack = (TecTrack*)((tecout->GetTecTracks())->UncheckedAt(i));
      tecoutshort->AddTecTrack(*tectrack);
  }

  return;
}

//----------------------------------------------------------------------------

void copyTecOutV1(PHCompositeNode* root,
                  TecGeometryObject* TGO,
                  TecCalibrationObject* TCO) {

// Find TecOut

  TecOut* tecoutshort;
  PHTypedNodeIterator<TecOut> tecitershort(root);
  TecOutNodeShort_t *TecOutNodeShort = tecitershort.find("TecOut");

  if(TecOutNodeShort) {
    tecoutshort = (TecOut*)TecOutNodeShort->getData();
  }
  else {
    cerr << PHWHERE << "ERROR: Can not find TecOut." << endl; 
    return ;
  }

// Try to find TecOutV1 and if it does not exist, create it in TEC node.

  TecOutV1* tecout = 0;
  PHCompositeNode* tecNode = 0;
  PHObjectNode_t *TecOutNodeNew = 0;
  PHTypedNodeIterator<TecOutV1> teciter(root);
  TecOutNode_t *TecOutNode = teciter.find("TecOutV1");

  if(TecOutNode) {
    tecout = TecOutNode->getData();
  }
  else {                // TecOut Does not exist. Add it to node tree (TEC node).
    tecNode = static_cast<PHCompositeNode*>(teciter.findFirst("PHCompositeNode", "TEC"));
    if(!tecNode) {
      cerr << PHWHERE << "ERROR: TEC node does not exist." << endl;
      return ;
    }
      // Add TecOutV1 to tecNode
      tecout = new TecOutV1();
      TecOutNodeNew = new PHObjectNode_t (tecout, "TecOutV1", "PHObject");
      tecNode->addNode(TecOutNodeNew);
  }

  float SinAlpha[TECMAXINDEX];
  float MinBin[TECMAXINDEX];
  float MaxBin[TECMAXINDEX];
  float MidPhi[TECMAXINDEX];
  float midphi[TECMAXSECT*TECMAXSIDE];
  for (int i = 0; i < TECMAXINDEX; i++) {
      MinBin[i] = TCO->getFirstTimeBin (i);
      MaxBin[i] = TCO->getLastTimeBin (i);
  }

  GetMidPhi(MidPhi, midphi, SinAlpha, TGO, 0);
// Copy TecOut to TecOutV1
  int runnumber = tecoutshort->getRunNumber();
  tecout->setRunNumber(runnumber);

  float xyz[3]; xyz[0] = 0.0; xyz[1] = 0.0; xyz[2] = 0.0;
  float Xpos=0., Ypos=0.;

  int hotdeadlist[TECMAXNUMHOT];
  int nhotdead=0;
  nhotdead=TCO->getHotDeadList(hotdeadlist);
       
  for (int i = 0; i < tecoutshort->getNHits(); i++) {
    int index = tecoutshort->getHitIndex(i);
    int wire = tecoutshort->getHitWire(i);
    int bin = tecoutshort->getHitTimeBin(i);
    int adc = tecoutshort->getHitADC(i);
    int trackid = tecoutshort->getHitTrackID(i);
    // calculate coordinates
       int trkindex = index/1000;
       float Xwire = TGO->getGlobalX(trkindex, wire);
       float Ywire = TGO->getGlobalY(trkindex, wire);
       float difference1 = (MaxBin[trkindex] - MinBin[trkindex]);
       float difference2 = (((float) bin) + 0.5) - ((float) (MinBin[trkindex]));
       float relativebin = difference2 / difference1;
       CalcXYfromBin(relativebin, Xwire, Ywire, SinAlpha[trkindex], Xpos, Ypos, 0);
       xyz[0] = Xpos;
       xyz[1] = Ypos;
       xyz[2] = 1.;
       if(trkindex%2==0) xyz[2] = -1.;
    // calculate charge
         float gainA = TCO->getAbsoluteGain(index)*TCO->getRelativeGain(index,wire);
         // Check if this wire is hot or dead
         int glindex = index*1000+wire;
         for(int j2=0; j2<nhotdead; j2++) {
           if(hotdeadlist[j2]==glindex) { gainA=0.; }
         }
           float rawcharge = Ampl2Charge(adc);
           float charge = rawcharge*gainA;
	     int tmp = i;
             if(gainA==0.) {tecoutshort->setHitADC(tmp,0);}

    tecout->AddTecHit(index, wire, bin, adc, charge, xyz, trackid);

  }

  for (int i = 0; i < tecoutshort->getNTracks(); i++) {
    TecTrack* tectrack = (TecTrack*)((tecoutshort->GetTecTracks())->UncheckedAt(i));
    tecout->AddTecTrack(*tectrack);
  }

  return;
}

PHPoint
get_projection(PHLine dcline, int ichamber, TecGeometryObject* TGO)
{
  PHPoint projPoint;
  if (!TGO->isActivePlane(ichamber))
    {
      projPoint.setX(-9999.9);
      projPoint.setY(-9999.9);
      projPoint.setZ(-9999.9);
      return projPoint;
    }
  PHPanel tecpanel = TGO->getTecPanel(ichamber);
  if (intersectionLinePanel(dcline,tecpanel,projPoint)==0)
    {
      projPoint.setX(-9999.9);
      projPoint.setY(-9999.9);
      projPoint.setZ(-9999.9);
    }
  return projPoint;
}

bool
Associate(int icgl, PHLine dcline, TecClusterContainer* tec, TecCalibrationObject* TCO, TecGeometryObject* TGO, TecProj* tecproj, bool is_swapped, float min_distance_cut)
{
  // if track is in West arm return
  if (dcline.getBasepoint().getX() > 0 ) return false;

  unsigned int iproj = tecproj->get_TecNProj();

  int clusid[6] = {-1,-1,-1,-1,-1,-1};
  bool match=false;
  PHPoint projPointKeep;

  int isidetrack = -1;
  int isectortrack = -1;

  // Cache access to the clusters so we don't have to make the round trip
  // through the TClonesArray in TecClusterContainer all the time ...
  const size_t nclusters = tec->getNClusters();
  std::vector<TecCluster*> tecccv(nclusters);
  for (size_t i=0; i<nclusters; ++i)
    tecccv[i] = tec->getTecCluster(i);

  for (int ichamber=0; ichamber<TECMAXINDEX; ichamber++)
    {
      int isector = ichamber/12;
      int iside = ichamber%2;
      int iplane = (ichamber%12)/2;
      
      // iside and isector only have to be determined once per track, after that we can skip all others
      if (isidetrack != -1 && iside != isidetrack) continue;
      if (isectortrack != -1 && isector != isectortrack) continue;
      
      // this guy is slow
      PHPoint projPoint = get_projection(dcline,ichamber,TGO);
      if (projPoint.getX() == -9999.9) continue;
      
      // OK, this is a valid side and sector for this track, capture side and sector
      if(isidetrack == -1)
	isidetrack = iside;
      if(isectortrack == -1)
	isectortrack = isector;

      float projPointZ =  projPoint.getZ(); // tec has no Z information, using the projection from DCH
      if (is_swapped)
	projPointZ *= (-1.0);
      
      float min_dist = 9999.9;
      int id_min_dist = -1;
      for (size_t ihit=0; ihit<nclusters; ihit++)
	{
          TecCluster* teccc = tecccv[ihit];
	  if (teccc->get_index() != ichamber)
	    continue;
	  PHPoint tecpoint(teccc->get_xyz_global(TCO,TGO,0),
			   teccc->get_xyz_global(TCO,TGO,1),
			   projPointZ); 
	  double dist = distanceLinePoint(dcline,tecpoint);

	  if (dist < min_dist)
	    {
	      min_dist = dist;
	      id_min_dist = ihit;
	      projPointKeep = projPoint;
	    }
	}
      if (min_dist > min_distance_cut)
	continue;
      
      // If we made it to here, we found a matched TEC hit in at least one plane
      
      match = true;
      clusid[iplane] = id_min_dist;
    
      /*
      cout << "    Found match with icgl " << icgl << " is_swapped " << is_swapped
	   << " iproj " << iproj << " iplane " << iplane << " id_min_dist "
	   << id_min_dist << " min_dist " << min_dist
	   << " index " << tec->getTecCluster(id_min_dist)->get_index() << endl;
      cout << "     X " << tec->getTecCluster(id_min_dist)->get_xyz_global(TCO,TGO,0)
	   << " Y " << tec->getTecCluster(id_min_dist)->get_xyz_global(TCO,TGO,1)
	   << " projPointZ " << projPointZ
	   << endl;
      cout << "    id_min_dist " << id_min_dist
	   << " index " << tec->getTecCluster(id_min_dist)->get_index()
	   << " wire " << tec->getTecCluster(id_min_dist)->get_wire()
	   << " ntimebins " <<  tec->getTecCluster(id_min_dist)->get_ntimebins()
	   << " avgtime " <<  tec->getTecCluster(id_min_dist)->get_avgtime()
	   << " charge " <<  tec->getTecCluster(id_min_dist)->get_charge()
	   << " X " << tec->getTecCluster(id_min_dist)->get_xyz_global(TCO,TGO,0)
	   << " Y " << tec->getTecCluster(id_min_dist)->get_xyz_global(TCO,TGO,1)
	   << endl;
      */

    }
  
  if(match)
    {
      tecproj->set_TClonesArraySize(iproj+1);
      tecproj->set_TecNProj(iproj+1);
      tecproj->AddTecProj(iproj);

      tecproj->set_cgltrackid(iproj,icgl);
      for(int iplane=0;iplane<TECMAXPLANE;iplane++)
        {
          tecproj->set_tecclusterid(iproj,iplane,clusid[iplane]);
        }
      
      // Note that we are not guaranteed that startpoint or endpoint was found, so
      // we store the projection as the input dcline

      tecproj->set_pstartx(iproj, dcline.getBasepoint().getX());
      tecproj->set_pstarty(iproj, dcline.getBasepoint().getY());
      tecproj->set_pstartz(iproj, dcline.getBasepoint().getZ());

      // The endpoint is taken as projPoint, note this is the unswapped projection

      tecproj->set_pendx(iproj, projPointKeep.getX());
      tecproj->set_pendy(iproj, projPointKeep.getY());
      tecproj->set_pendz(iproj, projPointKeep.getZ());
      return true;
    }

  return false;
}

} // namespace end


