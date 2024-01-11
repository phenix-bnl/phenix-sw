#include "TMuiRoadMapO.h"
#include "TMuiClusterMapO.h"
#include "TMuiRoadOGroup.h"

#include <PHGeometry.h>

#include <gsl/gsl_sort_double.h>
#include <gsl/gsl_statistics.h>

#include <iostream>

using namespace std;

// CONSTRUCTOR/DESTRUCTOR METHODS ===================================
// Constructor.
TMuiRoadOGroup::TMuiRoadOGroup(const short& arm, const float& z,
  const float& MutrWin, const float& MuidWin)
  : fZPlane(z),fMutrWin(MutrWin),fMuidWin(MuidWin),
    fGroup(-1),fGoldenIndex(0),fChi2DOF(100000.0),
    fArm(arm),fTotalRoads(0),f_pRoads(0)
{}

// Copy constructor.
TMuiRoadOGroup::TMuiRoadOGroup(const TMuiRoadOGroup& source)
  : fZPlane(source.fZPlane),fMutrWin(source.fMutrWin),
    fMuidWin(source.fMuidWin),
    fGroup(source.fGroup),fGoldenIndex(source.fGoldenIndex),
    fChi2DOF(source.fChi2DOF),fArm(source.fArm), 
    fTotalRoads(source.fTotalRoads),f_pRoads(source.f_pRoads)
{}

// Destructor.
TMuiRoadOGroup::~TMuiRoadOGroup() {}

// MUTATOR METHODS ==================================================

// TMuiRoadOGroup::SetGroup
// Set the group index for this group.
void
TMuiRoadOGroup::SetGroup(const short& Group)
{
  if (Group < 0) {
   return;
  }
  fGroup = Group;
  vector<TMuiRoadMapO::pointer>::iterator i;
  for (i=f_pRoads.begin(); i != f_pRoads.end(); ++i){
    (*i)->get()->set_group(fGroup);
  }
  MarkGolden();
}

// TMuiRoadOGroup::GoldenIndex
// the "golden" mark for this road in the current event.
size_t
TMuiRoadOGroup::GoldenIndex() const
{
  return fGoldenIndex;
}

// TMuiRoadOGroup::Group
// the group index for this road in the current event.
short
TMuiRoadOGroup::Group() const
{
  return fGroup;
}

// TMuiRoadOGroup::AttachRoad()
// Attach the given cluster to this road.
// Assume that this cluster has been verified to be consistent with the
// road.
void
TMuiRoadOGroup::AttachRoad(TMuiRoadMapO::pointer road)
{
  // Attach the road to the group.
  f_pRoads.push_back(road);
  fTotalRoads = f_pRoads.size();
}

// TMuiRoadOGroup::MergeGroup(TMuiRoadOGroup*)
// Merge roads from another group to this group
void
TMuiRoadOGroup::MergeGroup(TMuiRoadOGroup* sourceGroup)
{
  if(sourceGroup==NULL)
  {
    return;
  }
  int roadIndex = 0;
  while(roadIndex < sourceGroup->TotalRoads())
  {
    AttachRoad(sourceGroup->Road(roadIndex));
    roadIndex++;
  }
}

// TMuiRoadOGroup::Road(int)
// Get the Nth road from this group
TMuiRoadMapO::pointer TMuiRoadOGroup::Road(int roadIndex) const
{
  if((unsigned int)roadIndex < f_pRoads.size())
  {
    return f_pRoads[roadIndex];
  }
  return NULL;
}

// TMuiRoadOGroup::Clear()
void TMuiRoadOGroup::Clear()
{
  f_pRoads.clear();
  fTotalRoads = f_pRoads.size();
}

// TMuiRoadOGroup::RemoveRoad()
// Remove the given road from this group.
void
TMuiRoadOGroup::RemoveRoad(TMuiRoadMapO::pointer road)
{
  // Remove the road from the group.
  vector<TMuiRoadMapO::pointer>::iterator i;
  for (i=f_pRoads.begin(); i != f_pRoads.end(); ++i){
    if((*i) == road){
      f_pRoads.erase(i);
      break;
    }
  }
  fTotalRoads = f_pRoads.size();
}

// TMuiRoadOGroup::MarkGolden()
// mark the "golden" road (least square road) from this group.
void
TMuiRoadOGroup::MarkGolden()
{
  // Mark the "golden" road in the group.
  // Lets sort the roads by their projections to station 3 of the MuTr
  // and choose the median.  We may want to weight by Chi2.
  if(f_pRoads.size()>=1500)
  {
    cout<<"Road Group reached maximum entries: 1500\n";
    return;
  }
  double st3x, st3y;
  double st3xlist[1500];
  double st3ylist[1500];
  double Rst3xlist[1500];
  double Rst3ylist[1500];
  TMuiRoadMapO::pointer Rlist[1500];
  int xycount=0;

  short Golden = 0;
  vector<TMuiRoadMapO::pointer>::iterator i;
  TMuiRoadMapO::pointer road = NULL;
  PHPoint st3p;
  for (i=f_pRoads.begin(); i != f_pRoads.end(); ++i){
    road=(*i);
    st3p = ProjectToZ(road->get()->get_fit_par(),fZPlane);
    st3xlist[xycount] = st3p.getX();
    st3ylist[xycount] = st3p.getY();
    Rst3xlist[xycount] = st3p.getX();
    Rst3ylist[xycount] = st3p.getY();
    Rlist[xycount] = road;
    xycount++;
  }
  // Now lets calculate the median
  gsl_sort(st3xlist,1,xycount);
  gsl_sort(st3ylist,1,xycount);
  st3x = gsl_stats_median_from_sorted_data(st3xlist,1,xycount);
  st3y = gsl_stats_median_from_sorted_data(st3ylist,1,xycount);
  // Lets find the road the minimum distance from this
  double MinDist = 1E10;
  for (int rid = 0; rid < xycount; rid++)
  { 
    double ThisDist = sqrt((st3x-Rst3xlist[rid])*(st3x-Rst3xlist[rid])+(st3y-Rst3ylist[rid])*(st3y-Rst3ylist[rid]));
		//Sanghoon
		//There're are case of showing differnet comparion results between same float-point numbers in SL6 and SL7 becase of +/-1e-19 flucutation
		//This fluctuation cause different results of selecting good muid roads (Not good in high occupancy cases)
    //if(ThisDist < MinDist)
    if((ThisDist-MinDist)<1e-4)
    {
      MinDist = ThisDist;
      road = Rlist[rid];
    }
  }
  //Now lets set this road as golden
  for (i=f_pRoads.begin(); i != f_pRoads.end(); ++i){
    if( (*i) == road){
      Golden = 1;
      (*i)->get()->set_golden(Golden);
    }else{
      (*i)->get()->set_golden(0);
    }
  }
}



// TMuiRoadOGroup::TotalRoads
// the total number roads contained in this group?
short
TMuiRoadOGroup::TotalRoads() const {return fTotalRoads;}

// TMuiRoadOGroup::IsGroup
// Does the road belong to this group?
PHPoint TMuiRoadOGroup::ProjectToZ(TMutFitPar fit_par, double fZPlane)
{
  PHPoint z_intersect;
	
	// here we don't care about
  PHGeometry::intersectionLinePlane(
		PHLine(fit_par.get_point(),fit_par.get_tangent( fZPlane>0 ? MUIOO::North:MUIOO::South )),
		PHPlane(PHPoint(0,0,fZPlane),PHVector(0,0,1)),
		z_intersect  );
  return z_intersect;
}

// TMuiRoadOGroup::IsGroup
// Does the road belong to this group?
bool
TMuiRoadOGroup::IsGroup(TMuiRoadMapO::pointer road) 
{
  vector<TMuiRoadMapO::pointer>::iterator i;
  short arm = road->get()->get_arm();

  // Is the given road in the same arm as the group?
  if ( arm != fArm  ) {
    return false;
  }

  //
  if(f_pRoads.size()>=1500)
  {
    return false;
  }
    
  // Grouping roads with following criteria:
  // 1. Project roads to MuTR Station 3, if they are in a certain window
  // 2. If pass 1, if all associated hits are in a certain window in each gap
    
  // compare the given road with each road within this group
  bool St3IsGroup = false;
  PHPoint roadX = ProjectToZ(road->get()->get_fit_par(),fZPlane);
  for (i = f_pRoads.begin(); i != f_pRoads.end(); ++i) {
    // Does the road already exist in the group?
    if( (*i)->get()->get_key() == road->get()->get_key() ){
      return false;
    }
    PHPoint iX = ProjectToZ((*i)->get()->get_fit_par(),fZPlane);
    if ( (fabs(iX.getX()- roadX.getX()) < fMutrWin) && (fabs(iX.getY()- roadX.getY()) < fMutrWin)){
      St3IsGroup=true;
      break;
    }
  } 
  if(St3IsGroup == false)
  {
    return false;
  }
  // check associated clusters in each MuID gap
  int MatchingVHits = 0;
  int MatchingHHits = 0;
  int MinMatchHits = 1;

  for (i = f_pRoads.begin(); i != f_pRoads.end(); ++i)
  {
    //Loop over clusters and check for matches within window
    TMuiClusterMapO::const_key_iterator iclust1
      =road->get()->get_associated<TMuiClusterO>();
    TMuiClusterMapO::const_key_iterator iclust2
      =(*i)->get()->get_associated<TMuiClusterO>();

    while(TMuiClusterMapO::const_pointer clust1 = iclust1.next())
    {
      iclust2 = (*i)->get()->get_associated<TMuiClusterO>();
      while(TMuiClusterMapO::const_pointer clust2 = iclust2.next())
      {
        if(clust1->get()->get_key()==clust2->get()->get_key())
        {
          if(clust1->get()->get_orientation()==0)
          {
            MatchingHHits++;
          }else{
            MatchingVHits++;
          }
        }
      }
    }
    
    
  }

  if(MatchingVHits>=MinMatchHits && MatchingHHits>=MinMatchHits && St3IsGroup)
  {
    return true;
  }

  return false;

}

