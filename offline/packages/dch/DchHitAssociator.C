//
//  TKH--Updated UV hit association logic.
//       Code now respects the mirror hits
//       for UV wires which are sensitive 
//       on both sides.
//                       11-25-2001
//
#include <iostream>
#include <cmath>
#include "DchHitAssociator.hh"
#include "PHGeometry.h"
#include "recoConsts.h"

static float e_max_sag = 0.;
static float w_max_sag = 0.;
static float e_mean_sag = 1./3.;
static float w_mean_sag = 1./3.;

static int firsttime = 1;

using namespace std;
using namespace PHGeometry;

DchHitAssociator::DchHitAssociator(PHDchGeometryObject* geo)
{
  geometry = geo;
  hitList = 0;
  candidates = 0;
  if (firsttime) 
    { 
      firsttime = 0;
      recoConsts *rc = recoConsts::instance();
      e_max_sag =rc->get_FloatFlag("EASTMAXSAG",0.0);
      w_max_sag =rc->get_FloatFlag("WESTMAXSAG",0.0);
      e_mean_sag =rc->get_FloatFlag("EASTMEANSAG",0.33333);
      w_mean_sag =rc->get_FloatFlag("WESTMEANSAG",0.33333);
      cout<<"/---------------/ Maximum Sags: East "<<e_max_sag<<" cm,  West "<<w_max_sag<<" ,cm"<<endl;
    }     
}

DchHitAssociator::DchHitAssociator(PHDchGeometryObject* geo,
				   DchHitLineLists* list,
				   PHPointerList<DchTrackCandidate>* cList)
{
  geometry = geo;
  hitList = list;
  candidates = cList;
  if (firsttime) 
    { 
      firsttime = 0; 
      recoConsts *rc = recoConsts::instance();
      e_max_sag =rc->get_FloatFlag("EASTMAXSAG",0.0);
      w_max_sag =rc->get_FloatFlag("WESTMAXSAG",0.0);
      e_mean_sag =rc->get_FloatFlag("EASTMEANSAG",0.33333);
      w_mean_sag =rc->get_FloatFlag("WESTMEANSAG",0.33333);
      cout<<"/---------------/ Maximum Sags: East "<<e_max_sag<<" cm,  West "<<w_max_sag<<" ,cm"<<endl;
    }
}

DchHitAssociator::~DchHitAssociator()
{}

void
DchHitAssociator::associateXHits(int opt, int share)
{
  // associate hits to candidates
  // opt =  0   ->  X1&X2
  // opt =  1   ->  X1
  // opt =  2   ->  X2
  unsigned int i;
  long id;
  short plane;
  float tmpdistance;
  double alpha,cosal;
  int myhit;
  PHPoint inter;
  DchHitLine* hit;
  DchHitLine* oldhit;
  int prev_hit;
  unsigned int oldcandidate;
  DchTrackCandidate* mycandidate;
  DchTrackCandidate* tmpcandidate;

  for (i = 0;i < candidates->length();i++)
    {
      mycandidate = (*candidates)[i];
      mycandidate->X->clear();
      mycandidate->closestX->clear();
      PHLine trackLine(mycandidate->getLocalPoint(),
		       mycandidate->getLocalVector());
      alpha = mycandidate->getLocalAlpha();
      cosal =  fabs(cos(alpha));
      int numberOfX1Hits = mycandidate->x1->length();
      int numberOfX2Hits = mycandidate->x2->length();
      
      if (opt == 0 || opt == 1)
        {
          for (int h1 = 0; h1 < numberOfX1Hits; h1++)
            {
              hit = (*(mycandidate->x1))[h1];
              id = hit->getId();
	      plane = hit->getPlane();

              PHVector zax(0., 0., 1.);
              PHLine hitLineX(hit->getLocalPoint(), zax);
              double distance = 
		distanceLineLine(trackLine, hitLineX) / cosal;

              // hit in road arround candidate
              if (distance < pMaxXDistance)
                {
                  (mycandidate->X)->append(hit);

                  // search for closest hits to candidate
                  // hits are unique  one hit <-> one candidate
                  myhit = 0;
                  // 1st case  no hit on this plane yet

                  prev_hit = mycandidate->getAssociatedHit(plane);
		  
		  if (share)
		    {
		      if (prev_hit < 0)
			{
			  myhit = 1;
			}
		      else if (distance < mycandidate->getAssociatedChi2(plane))
			{
			  myhit = 1;
			  oldhit = (*(hitList->getList()))[prev_hit];
			  oldcandidate = oldhit->getAssociatedCandidate();
			  if (oldcandidate == i) oldhit->setAssociatedCandidate(-1);
			}
		      // ok, best hit for this candidate now check other candidates
		      if (myhit)
			{
			  (mycandidate->closestX)->append(hit);      
			  hit->setAssociatedCandidate(i);
			  mycandidate->setAssociatedChi2(plane,distance);
			  mycandidate->setAssociatedHit(plane,id);
			}
		    }
		  else
		    {
		      if (prev_hit < 0) 
			{
			  myhit = 1;
			} 
		      else 
			if (distance < mycandidate->getAssociatedChi2(plane)) 
			  {
			    myhit = 1;
			    oldhit =  (*(hitList->getList()))[prev_hit];
			    oldcandidate = oldhit->getAssociatedCandidate();
			    if (oldcandidate == i) oldhit->setAssociatedCandidate(-1);
			  }
		      // ok, best hit for this candidate now check other candidates
		      if (myhit) 
			{
			  short wasusedbyme = hit->getAssociatedCandidate();
			  tmpcandidate = 0;
			  tmpdistance = 10000.;
			  if (wasusedbyme >= 0) 
			    {
			      tmpcandidate = (*candidates)[wasusedbyme];
			      tmpdistance = tmpcandidate->getAssociatedChi2(plane);
			    }
			  // no other candidate
			  if ( (wasusedbyme == -1) ||               // or there is another candidate must check distance
			       (wasusedbyme >= 0 && distance < tmpdistance)) 
			    { 
			      (mycandidate->closestX)->append(hit);      
			      hit->setAssociatedCandidate(i);
			      mycandidate->setAssociatedChi2(plane,distance);
			      mycandidate->setAssociatedHit(plane,id);  
			    }
			}
		    }
		}
	    }
	}
      
      if (opt == 0 || opt == 2)
        {
          for (int h2 = 0; h2 < numberOfX2Hits; h2++)
            {
              hit = (*(mycandidate->x2))[h2];
              id = hit->getId();
              plane = hit->getPlane();
	      
              PHVector zax(0., 0., 1.);
              PHLine hitLineX(hit->getLocalPoint(), zax);
              double distance = (distanceLineLine(trackLine, hitLineX)) / cosal;
	      
              if (distance < pMaxXDistance)
                {
                  (mycandidate->X)->append(hit);
                  // search for closest hits to candidate
                  // hits are unique  one hit <-> one candidate
                  myhit = 0;
                  // 1st case  no hit on this plane yet
		  
                  prev_hit = mycandidate->getAssociatedHit(plane);
		  
		  if (share)
		    {
		      if (prev_hit < 0)
			{
			  myhit = 1;
			}
		      else if (distance < mycandidate->getAssociatedChi2(plane))
			{
			  myhit = 1;
			  oldhit = (*(hitList->getList()))[prev_hit];
			  oldcandidate = oldhit->getAssociatedCandidate();
			  if (oldcandidate == i) oldhit->setAssociatedCandidate(-1);
			}
		      // ok, best hit for this candidate now check other candidates
		      if (myhit)
			{
			  (mycandidate->closestX)->append(hit);      
			  hit->setAssociatedCandidate(i);
			  mycandidate->setAssociatedChi2(plane,distance);
			  mycandidate->setAssociatedHit(plane,id);
			}
		    }
		  else
		    {
		      if (prev_hit < 0) 
			{
			  myhit = 1;
			} 
		      else 
			if (distance < mycandidate->getAssociatedChi2(plane)) 
			  {
			    myhit = 1;
			    oldhit =  (*(hitList->getList()))[prev_hit];
			    oldcandidate = oldhit->getAssociatedCandidate();
			    if (oldcandidate == i) oldhit->setAssociatedCandidate(-1);
			  }
		      // ok, best hit for this candidate now check other candidates
		      if (myhit) 
			{
			  short wasusedbyme = hit->getAssociatedCandidate();
			  tmpcandidate = 0;
			  tmpdistance = 10000.;
			  if (wasusedbyme >= 0) 
			    {
			      tmpcandidate = (*candidates)[wasusedbyme];
			      tmpdistance = tmpcandidate->getAssociatedChi2(plane);
			    }
			  // no other candidate
			  if ( (wasusedbyme == -1) ||               // or there is another candidate must check distance
			       (wasusedbyme >= 0 && distance < tmpdistance)) 
			    { 
			      (mycandidate->closestX)->append(hit);      
			      hit->setAssociatedCandidate(i);
			      mycandidate->setAssociatedChi2(plane,distance);
			      mycandidate->setAssociatedHit(plane,id);  
			    }
			}
		    }
		}
            }
        }
    }

  // second pass to clean list of closest hits
  for (i = 0;i < candidates->length();i++)
    {
      mycandidate = (*candidates)[i];
      
      int numberOfClosest = mycandidate->closestX->length();
      for (int h = numberOfClosest - 1; h >= 0; h--)
	{
	  hit = (*(mycandidate->closestX))[h];
	  unsigned int wasusedbyme = hit->getAssociatedCandidate();
	  short idh = hit->getId();
	  short idc = mycandidate->getAssociatedHit(hit->getPlane());
	  
	  if (wasusedbyme != i && (idc == idh) && !share)
	    {
	      (mycandidate->closestX)->removeAt(h);
	      plane = hit->getPlane();
	      mycandidate->setAssociatedChi2(plane, 10000.);
	      mycandidate->setAssociatedHit(plane, -1);
	    }
	  else if (idc != idh)
	    {
	      (mycandidate->closestX)->removeAt(h);
	    }
	}
    }
}

void
DchHitAssociator::clearAssociation()
{
  unsigned int i;
  DchHitLine* hit;
  DchTrackCandidate* mycandidate;

  int numberOfHits = hitList->lengthOfList();

  for (int k = 0; k < numberOfHits; k++)
    {
      hit = (*(hitList->getList()))[k];
      hit->setAssociatedCandidate(-1);
    }

  for (i = 0;i < candidates->length();i++)
    {
      mycandidate = (*candidates)[i];
      for (int n = 0; n < numberOfPlanes; n++)
        {
          mycandidate->resetAssociated(n);
        }
      mycandidate->closestX->clear();
      mycandidate->closestUV->clear();
    }
}

void
DchHitAssociator::markAssociatedHitsAsUsed()
{
  DchHitLine* hit;
  DchTrackCandidate* mycandidate;

  for (unsigned int i = 0;i < candidates->length();i++)
    {
      mycandidate = (*candidates)[i];
      int numberOfXHits = mycandidate->X->length();
      int numberOfUVHits = mycandidate->UV->length();
      for (int n = 0; n < numberOfXHits; n++)
        {
          hit = (*(mycandidate->X))[n];
          hit->used = 1;
        }
      for (int n = 0; n < numberOfUVHits; n++)
        {
          hit = (*(mycandidate->UV))[n];
          hit->used = 1;
        }
    }
}

void
DchHitAssociator::markAssociatedHitsAsUnused()
{
  DchHitLine* hit;
  DchTrackCandidate* mycandidate;

  for (unsigned int i = 0;i < candidates->length();i++)
    {
      mycandidate = (*candidates)[i];
      int numberOfXHits = mycandidate->X->length();
      int numberOfUVHits = mycandidate->UV->length();
      for (int n = 0; n < numberOfXHits; n++)
        {
          hit = (*(mycandidate->X))[n];
          hit->used = 0;
        }
      for (int n = 0; n < numberOfUVHits; n++)
        {
          hit = (*(mycandidate->UV))[n];
          hit->used = 0;
        }
    }
}

void
DchHitAssociator::associateUVHits()
{
  // Changed to use "projective" criterion to select the best UV hits.
  // TKH-- 11-30-2001

  unsigned int i;
  long id;
  short plane;
  float alpha;
  float tmpdistance;
  PHPoint inter;
  DchHitLine* hit;
  DchHitLine* oldhit;
  int prev_hit;
  unsigned int oldcandidate;
  DchTrackCandidate* mycandidate;
  DchTrackCandidate* tmpcandidate;

  float u11r=214.85; //This is the ideal radius of the u11 wire.

  for (i = 0;i < candidates->length();i++) {
    mycandidate = (*candidates)[i];
    alpha = mycandidate->getLocalAlpha();
    mycandidate->UV->clear();
    if (!(mycandidate->getQuality() & 0x10)) continue; // continue if no PC1 found...

    DchPc1Hit *PC=0;
    int final = mycandidate->getFinalPc1Id();
    for (unsigned int pc=0; pc<mycandidate->pc1->length(); pc++) {
      if ((*mycandidate->pc1)[pc]->getId()==final) {
	PC=(*mycandidate->pc1)[pc];
      }
    }

    float px =PC->getX();
    float py =PC->getY();
    float pz =PC->getZ();
    float pr =sqrt(px*px+py*py);
    float vz =vertex.getZ();

    int numberOfUV1Hits = mycandidate->uv1->length();
    int numberOfUV2Hits = mycandidate->uv2->length();
    
    for (int h1 = numberOfUV1Hits - 1; h1 >= 0; h1--) {
      hit = (*(mycandidate->uv1))[h1];
      plane = hit->getPlane();
      id = hit->getId();
      
      if (mycandidate->intersectUVwithXPlane(hit, inter)) {
	float hix =  inter.getX();
	float hiy =  inter.getY();
	float hiz =  inter.getZ();
	float hr  =  sqrt(hix*hix+hiy*hiy);
	float prediction = (pz-vz)/pr * hr + vz;
	// Parameterized error due to residual bend: TKH
	prediction = prediction +(0.25*pz*alpha*alpha*(pr-hr)/(pr-u11r));
	float distance = abs( hiz - prediction );

	if (distance < pMaxUVDistance) {
	  (mycandidate->UV)->append(hit);
	  // search for closest hits to candidate
	  // hits are unique  one hit <-> one candidate
	  int myhit = 0;
	  // 1st case  no hit on this plane yet
	  
	  prev_hit = mycandidate->getAssociatedHit(plane);
	  
	  if (prev_hit < 0) {
	    myhit = 1;
	  }
	  else {
	    if (distance < mycandidate->getAssociatedChi2(plane)) {
	      myhit = 1;
	      oldhit = (*(hitList->getList()))[prev_hit];
	      oldcandidate = oldhit->getAssociatedCandidate();
	      if (oldcandidate == i) {
		oldhit->setAssociatedCandidate(-1);
	      }
	    }
	  }
	  // ok, best hit for this candidate now check other candidates
	  if (myhit) {
	    short wasusedbyme = hit->getAssociatedCandidate();
	    tmpcandidate = 0;
	    tmpdistance = 10000.;
	    if (wasusedbyme >= 0) {
	      tmpcandidate = (*candidates)[wasusedbyme];
	      tmpdistance = tmpcandidate->getAssociatedChi2(plane);
	    }
	    // no other candidate
	    if ((wasusedbyme == -1) ||
		// or there is another candidate must check distance
		(wasusedbyme >= 0 && distance < tmpdistance)) { /// das geht so nicht!!!!
	      (mycandidate->closestUV)->append(hit);
	      hit->setAssociatedCandidate(i);
	      mycandidate->setAssociatedChi2(plane, distance);
	      mycandidate->setAssociatedHit(plane, id);
	    }
	  }
	}
      }
    }

    for (int h2 = numberOfUV2Hits - 1; h2 >= 0; h2--) {
      hit = (*(mycandidate->uv2))[h2];
      plane = hit->getPlane();
      id = hit->getId();
      if (mycandidate->intersectUVwithXPlane(hit, inter)) {
	float hix =  inter.getX();
	float hiy =  inter.getY();
	float hiz =  inter.getZ();
	float hr  =  sqrt(hix*hix+hiy*hiy);
	float prediction = (pz-vz)/pr * hr + vz;
	// Parameterized error due to residual bend: TKH
	// There is no a fundamental justification for the form.
	prediction = prediction +(0.25*pz*alpha*alpha*(pr-hr)/(pr-u11r));
	float distance = abs( hiz - prediction );

	if (distance < pMaxUVDistance) {
	  (mycandidate->UV)->append(hit);
	  // search for closest hits to candidate
	  // hits are unique  one hit <-> one candidate
	  int myhit = 0;
	  
	  prev_hit = mycandidate->getAssociatedHit(plane);
	  
	  // 1st case  no hit on this plane yet
	  if (prev_hit < 0) {
	    myhit = 1;
	  }
	  else {
	    if (distance < mycandidate->getAssociatedChi2(plane)) {
	      myhit = 1;
	      oldhit = (*(hitList->getList()))[prev_hit];
	      oldcandidate = oldhit->getAssociatedCandidate();
	      if (oldcandidate == i) {
		oldhit->setAssociatedCandidate(-1);
	      }
	    }
	  }

	  // ok, best hit for this candidate now check other candidates
	  if (myhit) {
	    short wasusedbyme = hit->getAssociatedCandidate();
	    tmpcandidate = 0;
	    tmpdistance = 10000.;
	    if (wasusedbyme >= 0) {
	      tmpcandidate = (*candidates)[wasusedbyme];
	      tmpdistance = tmpcandidate->getAssociatedChi2(plane);
	    }
	    // no other candidate
	    if ((wasusedbyme == -1) ||
		// or there is another candidate must check distance
		(wasusedbyme >= 0 && distance < tmpdistance)) { /// das geht so nicht!!!!
	      (mycandidate->closestUV)->append(hit);
	      hit->setAssociatedCandidate(i);
	      mycandidate->setAssociatedChi2(plane, distance);
	      mycandidate->setAssociatedHit(plane, id);
	    }
	  }
	}
      }
    }
  }

  // second pass to clean list of closest hits
  for (i = 0;i < candidates->length();i++) {
    mycandidate = (*candidates)[i];
    
    int numberOfClosest = mycandidate->closestUV->length();
    for (int h = numberOfClosest - 1; h >= 0; h--) {
      hit = (*(mycandidate->closestUV))[h];
      unsigned int wasusedbyme = hit->getAssociatedCandidate();
      short idh = hit->getId();
      short idc = mycandidate->getAssociatedHit(hit->getPlane());
      
      if (wasusedbyme != i && (idc == idh)) {
	(mycandidate->closestUV)->removeAt(h);
	plane = hit->getPlane();
	mycandidate->setAssociatedChi2(plane, 10000.);
	mycandidate->setAssociatedHit(plane, -1);
      }
      else {
	if (idc != idh) {
	  (mycandidate->closestUV)->removeAt(h);
	}
      }
    }
  }
}
  
void
DchHitAssociator::associateUVHitsWithUVPlanes()
{
  // collect UV hits that intersect alpha-plane
  // The "Both-sides" drift config requires that
  // we choose between mirror hits depending upon alpha.
  // TKH -- 10-23-2001.
  short arm;
  unsigned int side;
  short choice[numberOfArms][numberOfPlanes] = 
  { {0,0,0,0,0,0,0,0,0,0,0,0/* X1 east*/, -1, 1,-1, 1/* U1 east*/, 1,-1, 1,-1/* V1 east*/,
     0,0,0,0,0,0,0,0,0,0,0,0/* X2 east*/, -1, 1,-1, 1/* U2 east*/, 1,-1, 1,-1/* V2 east*/},  
    {0,0,0,0,0,0,0,0,0,0,0,0/* X1 west*/, -1, 1,-1, 1/* U1 west*/,-1, 1,-1, 1/* V1 west*/,
     0,0,0,0,0,0,0,0,0,0,0,0/* X2 west*/, -1, 1,-1, 1/* U2 west*/,-1, 1,-1, 1/* V2 west*/}};
  short slope;

  DchTrackCandidate *mycandidate;

  PHPoint inter;
  PHAngle candidatePhi, hitPhi;
  DchHitLine *hit;
  PHPointerList<DchHitLine> *uv1candi;
  PHPointerList<DchHitLine> *uv2candi;
  PHPointerList<DchHitLine>* list1;
  PHPointerList<DchHitLine>* list2;
  
  for (unsigned int i=0; i<candidates->length();i++) {
    mycandidate = (*candidates)[i];
    arm = mycandidate->getArm();
    side = mycandidate->getSide();
    
    list1 = hitList->getList(UV1Wire,arm,side);
    list2 = hitList->getList(UV2Wire,arm,side);
    
    int lengthList1 = list1->length();
    int lengthList2 = list2->length();
    
    candidatePhi = mycandidate->getLocalPhi();
    
    uv1candi = mycandidate->uv1; // get the pointer to the list
    uv2candi = mycandidate->uv2; // get the pointer to the list
    
    for(int h=0; h<lengthList1; h++) {
      hit = (*list1)[h];
      hitPhi = hit->getLocalPhi();
      
      // Check the mirror ID and only use non-mirror
      // or correct slope mirror hits!!!
      slope = choice[arm][hit->getPlane()];
      if (mycandidate->getLocalAlpha()<0) slope=-1*slope;
      if (!(hit->getIdmirror()==0||hit->getIdmirror()==slope)) continue;
      
      if ( fabs(candidatePhi-hitPhi) > (pCellDifferenceCut+4)*0.07854 ) continue;   
      if (mycandidate->intersectUVwithXPlane(hit,inter)) {
	// select z range depending on side (side = -1 no side selection)
	if (inter.getZ()<pZMin || inter.getZ()>pZMax) continue;
	if (side == NORTH && (inter.getZ() < -pZAvg)) continue; 
	if (side == SOUTH && (inter.getZ() >  pZAvg)) continue; 
      
	PHPoint transInter = mycandidate->rotateAndTranslate(inter);
	hit->setLocalIntersectionPoint(transInter);
	hit->setGlobalIntersectionPoint(inter);
	hit->setAssociatedCandidate(i);
	uv1candi->append(hit); // append the uv hit to the list of the candidate
      }
    }

    for(int h=0; h<lengthList2; h++) {
      hit = (*list2)[h];
      hitPhi = hit->getLocalPhi();
      
      // Check the mirror ID and only use non-mirror
      // or correct slope mirror hits!!!
      slope = choice[arm][hit->getPlane()];
      if (mycandidate->getLocalAlpha()<0) slope=-1*slope;
      if (!(hit->getIdmirror()==0||hit->getIdmirror()==slope)) continue;

      if ( fabs(candidatePhi-hitPhi) > (pCellDifferenceCut+4)*0.07854 ) continue;   
      if (mycandidate->intersectUVwithXPlane(hit,inter)) {
      // select z range depending on side (side = -1 no side selection)
	if (inter.getZ()<pZMin || inter.getZ()>pZMax) continue;
	if (side == NORTH && (inter.getZ() < -pZAvg)) continue; 
	if (side == SOUTH && (inter.getZ() >  pZAvg)) continue; 
      
	PHPoint transInter = mycandidate->rotateAndTranslate(inter);
	hit->setLocalIntersectionPoint(transInter);
	hit->setGlobalIntersectionPoint(inter);
	hit->setAssociatedCandidate(i);
	uv2candi->append(hit); // append the uv hit to the list of the candidate
      }
    }
  }
}

void
DchHitAssociator::associateX1AndX2HitListForCandidates(int opt)
{
  short side, arm, plane, cell;
  int numberOfHitsPerCell;
  int cellOfCandidate, minCell, maxCell;
  DchTrackCandidate *mycandidate;
  DchHitLine* hit;
  PHPointerList<DchHitLine>* x1candi;
  PHPointerList<DchHitLine>* x2candi;

  for (unsigned int i = 0; i < candidates->length();i++)
    {
      mycandidate = (*candidates)[i];
      arm = mycandidate->getArm();
      side = mycandidate->getSide();
      x1candi = mycandidate->x1;
      x2candi = mycandidate->x2;
      x1candi->clear();
      x2candi->clear();

      float localPhiOfCandidate = mycandidate->getLocalPhi();
      cellOfCandidate = geometry->getCellOfPoint(localPhiOfCandidate);
      minCell = cellOfCandidate - 4;
      maxCell = cellOfCandidate + 4;

      if (opt == 0 || opt == 1)
        {
          for (plane = 0; plane < 12; plane++)
            {
              for (cell = minCell; cell <= maxCell; cell++)
                {
                  if (cell < 0 || cell > 79)
                    {
		      continue;
		    }
                  numberOfHitsPerCell = hitList->lengthOfList(arm, side, plane, cell);
                  for (int h = 0; h < numberOfHitsPerCell; h++)
                    {
                      hit = (*(hitList->getList(arm, side, plane, cell)))[h];
                      x1candi->append(hit);
                    }
                }
            }
        }
      if (opt == 0 || opt == 2)
        {
          for (plane = 20; plane < 32; plane++)
            {
              for (cell = minCell; cell <= maxCell; cell++)
                {
                  if (cell < 0 || cell > 79)
                    {
		      continue;
		    }
                  numberOfHitsPerCell = hitList->lengthOfList(arm, side, plane, cell);
                  for (int h = 0; h < numberOfHitsPerCell; h++)
                    {
                      hit = (*hitList->getList(arm, side, plane, cell))[h];
                      x2candi->append(hit);
                    }
                }
            }
        }
    }
}

void
DchHitAssociator::associatePc1Hits()
{
  int arm;
  unsigned int side;
  PHPoint inter;
  PHPlane planeUV;

  // DC candidate
  DchTrackCandidate* mycandidate;
  int lengthCandidates = candidates->length();
  // PC1 hits
  PHPointerList<DchPc1Hit>* list = pc1List->getList();
  int lengthList = list->length();

  // loop over all candidates
  for (int i = 0; i < lengthCandidates; i++)
    {
      mycandidate = (*candidates)[i];
      arm = mycandidate->getArm();
      side = mycandidate->getSide();
      planeUV = mycandidate->getGlobalPlane(UV1Wire);

      // loop over Pc1Hits
      DchPc1Hit *hit;
      for (int h = 0; h < lengthList; h++)
        {
          hit = (*list)[h];
          if (arm != hit->getArm())
            {
	      continue;
	    }
          if (side == SOUTH && hit->getZ() > 10)
            {
	      continue;
	    }
          if (side == NORTH && hit->getZ() < -10)
            {
	      continue;
	    }
          if (hit->used)
            {
	      continue;
	    }

          PHLine tmp(*hit, planeUV.getNormal());
          intersectionLinePlane(tmp, planeUV, inter);
          float dist = distancePointToPoint(inter, *hit);

          if (dist < 2)
            {
              (mycandidate->pc1)->append(hit);
            }
        }
    }
}

void
DchHitAssociator::moveHitsToCandidates(TABLE_HEAD_ST *dDchHit_h,
				       DDCHHIT_ST *dDchHit,
				       DchHitLineLists* hitList,
				       DchHitLineTable *hitLineTable)
{
  // This routine is changed so that the UV hits are not moved to the
  // candidate line, but are instead moved to the X-plane intersection
  // location.  The latter is good for code development (you see what the
  // code sees), the former is better for publicity since the hits are moved 
  // closer to an ideal straight line.  Neither choice makes a physics
  // difference given the present algorithm.
  unsigned int i;
  DchHitLine* hit;
  DchTrackCandidate* mycandidate;
  float L = 180.;
  float Shift,x_tmp,y_tmp,z_tmp,dist;
  float max_sag,mean_sag;

  for (i = 0;i < candidates->length();i++)
    {
      mycandidate = (*candidates)[i];
      int numberOfUVHits = mycandidate->closestUV->length();
      int numberOfXHits = mycandidate->closestX->length();
      int arm=mycandidate->getArm();

      if (numberOfUVHits >= 0)
        {
          for (int n = 0; n < numberOfXHits; n++)
            {
              hit = (*(mycandidate->closestX))[n];
              int id = hit->getId();
	      int plane = (hit->getPlane()%2);

	      if (plane == 0) plane = -1;
	      if (hit->getArm() == 0)
		{
		  max_sag  = e_max_sag;
		  mean_sag = e_mean_sag;
		}
	      else
		{
		  max_sag  = w_max_sag;
		  mean_sag = w_mean_sag;
		}

              PHLine hitLine(hit->getBasepoint(), hit->getDirection());
              PHLine candidateLine(mycandidate->getGlobalPoint() ,
				   mycandidate->getGlobalVector());
              PHPoint tmp = closestApproachLineLine(candidateLine, hitLine);
	      x_tmp = tmp.getX();
	      y_tmp = tmp.getY();
	      z_tmp = tmp.getZ();
	      
	      Shift = ((4./L*fabs(z_tmp)-1.)*(4./L*fabs(z_tmp)-1.)-mean_sag);
	      dist = sqrt(x_tmp*x_tmp+y_tmp*y_tmp);
	      
	      tmp.setX(x_tmp + Shift*max_sag*plane*y_tmp/dist);
	      tmp.setY(y_tmp - Shift*max_sag*plane*x_tmp/dist);
	      //cout<<"arm = "<<hit->getArm()<<" plane ="<<plane<<" shift = "<<Shift<<"  x_tmp = "<<x_tmp<<" x_after = "<<tmp.getX()<<" max_sag = "<<max_sag<<" dist = "<<dist<<endl;
              dDchHit[id].xyz[0] = tmp.getX();
              dDchHit[id].xyz[1] = tmp.getY();
              dDchHit[id].xyz[2] = tmp.getZ();

	      
	      hit->setBasepoint(tmp);
	      PHPoint localPoint = geometry->rotateAndTranslateInverse(arm,tmp);
	      hit->setLocalPoint( (PHCylPoint)localPoint );

              if (hitLineTable->Entries()>=id) 
 		{
		  hitLineTable->setXYZ(id, tmp);
		}
	      else
		{
		  cout << PHWHERE << "ERROR hit id " << id 
		       << " outside of no of hits: " << hitLineTable->Entries()
		       << endl;
		}
            }
        }

      if (numberOfUVHits > 0)
        {
          for (int n = 0; n < numberOfUVHits; n++)
            {
              hit = (*(mycandidate->closestUV))[n];
              int id = hit->getId();

              dDchHit[id].xyz[0] = hit->getGlobalIntersectionPoint().getX();
              dDchHit[id].xyz[1] = hit->getGlobalIntersectionPoint().getY();
              dDchHit[id].xyz[2] = hit->getGlobalIntersectionPoint().getZ();

	      PHPoint tmp = hit->getGlobalIntersectionPoint();
	      hit->setBasepoint(tmp);
	      PHPoint localPoint = geometry->rotateAndTranslateInverse(arm,tmp);
	      hit->setLocalPoint( (PHCylPoint)localPoint );

              if (hitLineTable->Entries()>=id) 
		{
		  hitLineTable->setXYZ(id, hit->getGlobalIntersectionPoint());
		}
	      else
		{
		  cout << PHWHERE << "ERROR hit id " << id 
		       << " outside of no of hits: " << hitLineTable->Entries()
		       << endl;
		}
	      
            }
        }
    }
}
