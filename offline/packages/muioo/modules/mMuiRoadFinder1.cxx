// $Id: mMuiRoadFinder1.cxx,v 1.7 2008/09/04 14:50:21 shoji Exp $

/*!
   \file mMuiRoadFinder1.cxx
   \brief find road from clusters
   \author S. Kelly D. Silvermyr
   \version $Revision: 1.7 $
   \date $Date: 2008/09/04 14:50:21 $
*/

#include <iostream>
#include <cmath>
#include <TMuiRoadOGroup.h>
#include <MuiGeomClasses.hh>

#include <gsl/gsl_fit.h>
#include <list>

#include "mMuiRoadFinder1.h"

using namespace std;

//_________________________________________________________________
struct muioo_cluster_distance
{
  TMuiClusterMapO::pointer pCluster;
  float distance;
};

//_________________________________________________________________
class muioo_cluster_distance_sort
{	
  //sort in order of distance
  public:
  bool operator() (const muioo_cluster_distance&, const muioo_cluster_distance&) const;
};

//_________________________________________________________________
bool muioo_cluster_distance_sort::operator()(const muioo_cluster_distance& a, const muioo_cluster_distance& b) const
{ return a.distance < b.distance; }


//_________________________________________________________________
// Default constructor and destructor to pacify CINT
mMuiRoadFinder1::mMuiRoadFinder1() :
   _road_map(0),
   _road1d_map(0),
   _cluster_map(0),
   _timer( PHTimeServer::get()->insert_new("mMuiRoadFinder1") )

{}

//_________________________________________________________________
PHBoolean mMuiRoadFinder1::event(PHCompositeNode *top_node) 
{

  _timer.get()->restart();
  
  try {
    
    set_interface_ptrs(top_node);
    _road1d_map->clear();
    _road_map->clear();
    
    if( _mod_par->get_verbosity() >= MUIOO::MAX )
    MUIOO::PRINT( cout, "mMuiRoadFinder1::event" );

    find_1droads();
    cut_1droads();
    find_2droads();
    flag_golden();

    if( _mod_par->get_verbosity() >= MUIOO::MAX ) {
      
      MUIOO::PRINT( cout, "mMuiRoadFinder1::event" );
      TMuiRoadMapO::iterator road_iter = _road_map->range();
      while( TMuiRoadMapO::pointer road_ptr = road_iter.next() )
      { 
        cout << road_ptr->get()->get_key().get_obj_key() << endl;
        road_ptr->get()->dump_associations<TMuiClusterO>(); 
         cout << endl;
       
        // retrieve associated 1D road
        TMui1DRoadMapO::key_iterator road_1d_iter = road_ptr->get()->get_associated<TMui1DRoadO>();
        while( TMui1DRoadMapO::pointer road_1d_ptr = road_1d_iter.next() )
        { 
          cout << "1D road: " << road_1d_ptr->get()->get_key().get_obj_key() << endl;
          road_1d_ptr->get()->dump_associations<TMuiClusterO>(); 
          cout << endl;
        }
        
        cout << endl;
      }
      MUIOO::PRINT( cout, "**" );
      
    }
      
    if( _mod_par->get_verbosity() >= MUIOO::MAX ) {
      
      TMui1DRoadMapO::iterator iroad = _road1d_map->get(0);
      cout<<" Finished road finding with "<< iroad.count() << " 1d roads.\n";	
      
      while(TMui1DRoadMapO::pointer roadptr = iroad.next()) 
      roadptr->get()->print();
  
      TMuiRoadMapO::iterator iroad2 = _road_map->get(0);
      cout<<" Finished road finding with "<< iroad2.count() << " 2d roads.\n";
      
      while(TMuiRoadMapO::pointer roadptr = iroad2.next())
      roadptr->get()->print();
      MUIOO::PRINT( cout, "**" );
    }
    
  } catch( exception& e ) { cout << e.what() << endl; }


  _timer.get()->stop();
  return 0;
}

//_________________________________________________________________
PHBoolean mMuiRoadFinder1::cut_1droads()
{
  for(int iArm = 0; iArm < TMuiChannelId::kArmsTotal; iArm++)
  {
  //Now we have a list of roads tracked all the way through the
  //detector. Before we add them to the 1D Road Container
  //perform all the 1DRoad Cuts except the Duplicate cut.


    TMui1DRoadMapO::iterator iRoad1 = _road1d_map->get(iArm);

    while(TMui1DRoadMapO::pointer pRoad1 = iRoad1.next()) {

      // Ok we found a road already, we need to know
      // whether we should save it or not
      // check road quality...
      //
      if( _mod_par->get_verbosity() >= MUIOO::MAX ) {
        cout<<"Checking Road: "<<endl;
        pRoad1->get()->print();
      }
      
      // 1. last plane of the road
      bool LastPlaneOK = false;
      if ( pRoad1->get()->get_depth() >= _mod_par->get_min_last_gap_1d() ) LastPlaneOK = true;

      // 2. positon at reference plane
      bool VertexCutOK = false;

      TMutFitPar roadfit = pRoad1->get()->get_fit_par();
      if(pRoad1->get()->get_orientation()==0) {
        if ( fabs(roadfit.get_y()) <= _mod_par->get_max_yref_1d() ) VertexCutOK = true;
      } else {
        if ( fabs(roadfit.get_x()) <= _mod_par->get_max_xref_1d() )
        VertexCutOK = true;
      }

      // 3. number of gaps contain hits
      bool FiredGapsOK = false;
      
      if( _mod_par->get_verbosity() >= MUIOO::MAX ) 
      cout
        <<"Testing FiredGaps: G " << pRoad1->get()->get_gapbit() 
        << " N " << pRoad1->get()->get_numfired() << endl;
      
      if ( pRoad1->get()->get_numfired() > _mod_par->get_min_fired_gaps()) FiredGapsOK = true;

      // Here is our criteria for a candidate road
      // at this stage.
      // i.e. 1) There are at least two gaps containing hits
      //			2) LastPlane of the road passes last plane cut
      //			3) Reference position passes vertex cut


      // At this point we are only cutting exact duplicates
      // resulting from the multiple seed loops

      if ( LastPlaneOK && VertexCutOK&& FiredGapsOK ){
        int NumHits1 = 0;
        int NumHits2 = 0;
        TMui1DRoadMapO::iterator iRoad2 = iRoad1;
        while(TMui1DRoadMapO::pointer pRoad2 = iRoad2.next()) {
          if(pRoad1->get()->get_orientation() != pRoad2->get()->get_orientation()) continue;

          TMuiClusterMapO::const_key_iterator iclust1 =pRoad1->get()->get_associated<TMuiClusterO>();
          TMuiClusterMapO::const_key_iterator iclust2 =pRoad2->get()->get_associated<TMuiClusterO>();
    
          NumHits1 = iclust1.count();
          NumHits2 = iclust2.count();
    
          if(NumHits1!=NumHits2) continue;

          short SharedHits1D = 0;	
          while(TMuiClusterMapO::const_pointer clust1 = iclust1.next()) {
            
            if( _mod_par->get_verbosity() >= MUIOO::MAX )
            clust1->get()->get_key().print();
            
            iclust2.reset(); 
            while(TMuiClusterMapO::const_pointer clust2 = iclust2.next()) {

              if( _mod_par->get_verbosity() >= MUIOO::MAX )
              clust2->get()->get_key().print();
              
              if(clust1->get()->get_key()==clust2->get()->get_key()) SharedHits1D++;
            }
          }
          
          if( _mod_par->get_verbosity() >= MUIOO::MAX ) {
            if(pRoad1->get()->get_panel()==pRoad1->get()->get_panel()){
              cout
                <<"Checked for duplicate: SharedHits("<<SharedHits1D<<") "
                <<NumHits1<<"/"<<NumHits2<<endl;
              pRoad1->get()->print();
              pRoad2->get()->print();			
            }
          }
              
          if(SharedHits1D==NumHits1){
          
            if( _mod_par->get_verbosity() >= MUIOO::MAX ) {
              cout<<"Duplicate road found with "<<SharedHits1D<<" shared hits"<<endl;
              pRoad1->get()->print();
              pRoad2->get()->print();
            }
          
            // Instead of erasing now.	Lets flagit by
            // setting its depth to 0.
            // Later we'll come back and erase all.
            pRoad2->get()->set_depth(0);
            
          } // if SharedHits1D==NumHits1
        }	 //while(pRoad2
        
      } else {

        if( _mod_par->get_verbosity() >= MUIOO::MAX )
        cout
          <<"Cutting 1d road: " << "LastPlaneOK "<<LastPlaneOK
          <<" VertexCutOK " << VertexCutOK
          <<" FiredGapsOK " <<FiredGapsOK
          <<endl;
      
        // Instead of erasing now.	Lets flagit by
        // setting its depth to 0.
        // Later we'll come back and erase all.
        pRoad1->get()->set_depth(0);
      }
      
    }//while(pRoad1
    iRoad1 = _road1d_map->get(iArm);
    while(TMui1DRoadMapO::pointer pRoad1 = iRoad1.next())
    if(pRoad1->get()->get_depth()==0) _road1d_map->erase(pRoad1->get()->get_key());

  }//for(iArm..

  return 0;
}

//_________________________________________________________________
PHBoolean mMuiRoadFinder1::find_1droads() 
{
  
  short Count0,iPlane0, iPlane1;
  EOrient_t orient;

  for (short iArm=0; iArm<TMuiChannelId::kArmsTotal; iArm++) 
  {

    // check number of clusters
    unsigned int counts = _cluster_map->get(iArm).count();
    if( counts > _mod_par->get_max_occupancy_per_arm() ) 
    {
      cout << "mMuiRoadFinder1::find_1droads - Skipping analysis of arm " 
        << iArm 
        << " with too many hits " 
        << counts << endl;
      continue;
    }
    
    for (short iorient=0; iorient<TMuiChannelId::kOrientations; iorient++) 
    {

      orient = (iorient == 0) ? kHORIZ : kVERT;

      for( short iLoop=0; iLoop<_mod_par->get_num_seed_loops(); iLoop++) 
      {

        // checkout the seed gap(s) from search order
        iPlane0 = _mod_par->get_search_order(iLoop,0);
        iPlane1 = _mod_par->get_search_order(iLoop,1);

        //Create a temporary list of roads for this search order.			
        // one-seed/two-seed-gap method, by pass iPlane0
        short TotalPanel0 = (iPlane0 == -1) ? 1:TMuiChannelId::kPanelsPerPlane; // two-seed-gap method

        for (short iPanel0=0; iPanel0<TotalPanel0; iPanel0++) 
        {

          TMuiClusterMapO::iterator iclust0;
          if (iPlane0 != -1) iclust0 = _cluster_map->get(iArm,iPlane0,iPanel0,orient);
          Count0 = (iPlane0 == -1)? (1) : iclust0.count();
          
          for( short idClust0=0; idClust0<Count0; idClust0++ ) 
          {
            
            TMuiClusterMapO::pointer clust0ptr = iclust0.next();
            for (short iPanel1=0; iPanel1<TMuiChannelId::kPanelsPerPlane; iPanel1++) 
            {

              TMuiClusterMapO::iterator iclust1 = _cluster_map->get(iArm,iPlane1,iPanel1,orient);;
              while(TMuiClusterMapO::pointer pC1 = iclust1.next()) 
              {
                      
                // Found seed cluster(s), create a new 1D road
                // object, and attach cluster pC1 (and pC0, if 2 seed gaps)
  
                TMui1DRoadMapO::pointer road1D = (_road1d_map->insert_new(iArm,iPanel1,orient)).current();
  
                if (iPlane0 != -1 )
                {
                  
                  //If Two Gap seed set vertex weight to 0
                  PHKey::associate(road1D, clust0ptr);
                  road1D->get()->set_fitweight(0, 0.0);
                  
                  if( _mod_par->get_verbosity() >= MUIOO::MAX )
                  {
                    cout 
                      << "mMuiRoadFinder1::find_1droads - associate 1DRoad=" << road1D->get()->get_key().get_obj_key() 
                      << " cluster " <<	clust0ptr->get()->get_key().get_obj_key() << endl;
                    road1D->get()->dump_associations<TMuiClusterO>(); 
                    cout << endl;

                    }
                } else {
                  //If One Seed gap method use vertex in fits.
                  road1D->get()->set_fitweight(0, 1.0);
                }
  
                PHKey::associate(road1D, pC1);
                
                if( _mod_par->get_verbosity() >= MUIOO::MAX ) 
                {
                  cout 
                    << "mMuiRoadFinder1::find_1droads - associate 1DRoad=" << road1D->get()->get_key().get_obj_key() 
                    << " cluster " <<	pC1->get()->get_key().get_obj_key() << endl;
                  road1D->get()->dump_associations<TMuiClusterO>(); 
                  cout << endl;
                  //cout<<"Begin tracking seed: iLoop("<< iLoop	<< ") Panel ("<<iPanel1 <<")"<<endl;
                  //road1D->get()->print();
                  //pC1->get()->print();
                }
                
                track_seed(road1D,iLoop,2);

                fit1d(road1D);									

              }//for (iClust1)
            }//for(iPanel1)
          }//for(iClust0)
        }//for(iPanel0)				
      } //short (short iLoop ...)			
    } // for (short iorient ...)
  }	// for(iArm ...)
  return True;
}

//__________________________________________________________________
PHBoolean mMuiRoadFinder1::find_2droads() 
{

  // found all possible horizontal and vertical roads in both arms
  // construct 2D roads
  for (short iArm=0; iArm<TMuiChannelId::kArmsTotal; iArm++) {
    
    TMui1DRoadMapO::iterator iRoadH = _road1d_map->get(iArm);
    while(TMui1DRoadMapO::pointer pRoadH = iRoadH.next()) {
    
      if(pRoadH->get()->get_orientation()==1) continue;
      
      TMui1DRoadMapO::iterator iRoadV = _road1d_map->get(iArm);
      while(TMui1DRoadMapO::pointer pRoadV = iRoadV.next()) {
        if(pRoadV->get()->get_orientation()==0) continue;


        // What are our criteria for accepting a road
        // as valid?	Check them here.
        // 1. difference of total number clusters in vRoad and in hRoad
        // 2. difference of last gaps in vRoad and hRoad
        // 3. InterSection OK or not (if X Y clusters in each gap are
        //														in same panel or overlap region)
        // 4. Reference position check
        // 5. Chisquare check
        // 6. Last Plane check
        // 7. Duplicate road check

        // JLN - bug fix - added fabs around the depth differenece 07/25/03
        bool LastGapDeltaOK = false;
        
        if (abs(pRoadH->get()->get_depth() - pRoadV->get()->get_depth()) <= _mod_par->get_max_del_last_gap() )
        LastGapDeltaOK = true;

        // JLN - bug fix - added fabs around the hits differenece 07/25/03
        bool TotalHitsDeltaOK = false;
        if(abs(pRoadH->get()->get_nhit() - pRoadV->get()->get_nhit()) <= _mod_par->get_max_del_total_hits() )
        TotalHitsDeltaOK = true;

        bool IntersectionOK = true;
        TMuiClusterMapO::const_key_iterator iClustH =pRoadH->get()->get_associated<TMuiClusterO>();
          
        while(TMuiClusterMapO::const_pointer pClustH = iClustH.next()) {
        
          TMuiClusterMapO::const_key_iterator iClustV =pRoadV->get()->get_associated<TMuiClusterO>();
          while(TMuiClusterMapO::const_pointer pClustV = iClustV.next()) {
          
            if(pClustH->get()->get_plane()!=pClustV->get()->get_plane()) continue;
            if( !(intersectionOK(pClustH,pClustV)) ){
              IntersectionOK = false;
              break;
            }
           
          }
        }

        bool VertexCutOK = false;
        TMutFitPar fitH = pRoadH->get()->get_fit_par();
        TMutFitPar fitV = pRoadV->get()->get_fit_par();
        
        if ( fabs(fitH.get_y()) <= _mod_par->get_max_yref_2d() &&
             fabs(fitV.get_x()) <= _mod_par->get_max_xref_2d() ) VertexCutOK = true;

        bool ChiSquareCutOK = false;
        if ( fitV.get_chi_square() <= _mod_par->get_max_xchisq()
             &&	fitH.get_chi_square() <= _mod_par->get_max_ychisq() ) ChiSquareCutOK = true;
        
        bool minLastGapOK = false;
        if (pRoadH->get()->get_depth() >= _mod_par->get_min_last_gap_2d()
            || pRoadV->get()->get_depth() >= _mod_par->get_min_last_gap_2d()) minLastGapOK = true;

        if ( LastGapDeltaOK &&
             TotalHitsDeltaOK &&
             IntersectionOK &&
             VertexCutOK &&
             ChiSquareCutOK &&
             minLastGapOK
             ) {
          TMuiRoadMapO::iterator iRoad = _road_map->insert_new(iArm);
          make2d(iRoad.current(),pRoadH,pRoadV);
        }// if(pass cuts ..
      } // for ( vRoad ..
    } // for ( hRoad ..
  }//for(iArm ...
  return 0;
}

//_________________________________________________________________________________
int mMuiRoadFinder1::track_seed(TMui1DRoadMapO::pointer road1dptr, int iLoop, int iSearch)
{

  double Win = _mod_par->get_mui_window();
  if(road1dptr->get()->get_associated<TMuiClusterO>().count() == 1) Win = 2.0 * Win;

  // fit the road
  fit1d(road1dptr);
  
  //Invalid iLoop
  if(iLoop>=_mod_par->get_num_seed_loops()) {
    if( _mod_par->get_verbosity() >= MUIOO::MAX )
    cout << "mMuiRoadFinder1::track_seed - [iloop] aborted for 1DRoad " << road1dptr->get()->get_key().get_obj_key() << endl;
    return 0;
  }
  
  // End this recursion loop over search planes
  if(iSearch>=_mod_par->get_search_length(iLoop)){
    if( _mod_par->get_verbosity() >= MUIOO::MAX )
    cout << "mMuiRoadFinder1::track_seed - [isearch] aborted for 1DRoad " << road1dptr->get()->get_key().get_obj_key() << endl;
    return 0;
  }
  
  if( _mod_par->get_verbosity() >= MUIOO::MAX )
  cout<< "mMuiRoadFinder1::track_seed - Tracking seed: iLoop("<<iLoop<<") iSearch("<<iSearch<<")\n";
  
  //First get list of clusters in panel
  // and order them by their distance from seed projection
  TMuiClusterMapO::iterator iclusts =
    _cluster_map->get(road1dptr->get()->get_arm(),
      _mod_par->get_search_order(iLoop, iSearch),
      road1dptr->get()->get_panel(),
      road1dptr->get()->get_orientation()
      );
  
  // Loop over all clusters in the panel
  // and project the road to the current panel;
  // if in the window append to local copy
  // then sort by distance
  std::vector<muioo_cluster_distance> clusterList;
  muioo_cluster_distance tmp_cd;
  double clust_dist = 0.0;
  bool clustInPanel = false;

  while(TMuiClusterMapO::pointer clustptr = iclusts.next()) {
  
    clust_dist = ClusterDistance(road1dptr,clustptr);
    /* 
      JLN - this was MuidWindow, but changed to use Win 
      (as defined above to double in case of first hit) - 
      07/25/03
    */
    if(clust_dist < Win) {
      tmp_cd.distance = clust_dist;
      tmp_cd.pCluster = clustptr;
      clusterList.push_back(tmp_cd);
    }
  }
  
  /* 
    If there's no cluster found
    lets look for clusters in the adjacent panels, but
    continue to consider a candidate with no hit in the
    current panel, ie Hardware Inefficiency
  */
  
  if( _mod_par->get_verbosity() >= MUIOO::MAX )
  cout<<"Found "<<clusterList.size()<<" in the current panel "<<endl;

  if(clusterList.size()>0) clustInPanel=true;

  short curPanel = road1dptr->get()->get_panel();
  
  if(!clustInPanel)
  for (short iPanel=0; iPanel<TMuiChannelId::kPanelsPerPlane; iPanel++) {

    //Skip non-adjacent panels
    if(!(
	 ( iPanel == (curPanel + 1)% TMuiChannelId::kPanelsPerPlane ) ||
	 ( iPanel == (curPanel + TMuiChannelId::kPanelsPerPlane - 1)% TMuiChannelId::kPanelsPerPlane )
      )) continue; 
    
    TMuiClusterMapO::iterator iclustsNA = _cluster_map->get(
      road1dptr->get()->get_arm(),
      _mod_par->get_search_order(iLoop, iSearch),
      iPanel,
      road1dptr->get()->get_orientation()
      );

    while(TMuiClusterMapO::pointer clustptr = iclustsNA.next()) {

      if(!clustptr) break;
      
      // Get the displacement of cluster pc to road1D
      clust_dist	= ClusterDistance(road1dptr,clustptr);

      // Consider all of the clusters within the window
      // JLN - again MuidWindow -> Win - 07/25/03
      if ( clust_dist < Win ) {
        tmp_cd.pCluster=clustptr;
        tmp_cd.distance = clust_dist;
        clusterList.push_back(tmp_cd);
      }
      
    }	 //while(iClust)
  }			 //if(!clustInPanel)
  
  //Let's sort the list by the distance from projection
  sort(clusterList.begin(), clusterList.end(), muioo_cluster_distance_sort());

  // First case: No clusters in any panel so go to next search gap
  if((!clustInPanel&& (clusterList.size()==0))) track_seed(road1dptr , iLoop, iSearch+1);
  else if(!clustInPanel) 
  {
  
    // Second case: no clust in current panel but in adjacent
    // track a copy without clusters
    TMui1DRoadMapO::pointer new1droadptr = bifurcate( road1dptr );
    
    //(_road1d_map->insert_new(road1dptr)).current();
    
    if( _mod_par->get_verbosity() >= MUIOO::MAX ) 
    {
      cout << "mMuiRoadFinder1::track_seed - iSearch: " << iSearch 
        << " original: " << road1dptr->get()->get_key().get_obj_key() 
        << " copy: " << new1droadptr->get()->get_key().get_obj_key() 
        << endl;
    }

    track_seed(new1droadptr, iLoop, iSearch+1);
  
  }

  // Loop over all clusters up to maxClustersPerGapSearch
  // Create a copy (if not last)
  // Add cluster to copy and track_seed
  int newRoadCount = 0;
  vector<muioo_cluster_distance>::iterator lastEntry = clusterList.end();
  lastEntry--;
  
  for(vector<muioo_cluster_distance>::iterator iClustCand = clusterList.begin(); iClustCand != clusterList.end(); iClustCand++)
  {
    if(iClustCand == lastEntry || (newRoadCount == _mod_par->get_max_clusters_per_gap_search()-1))
    {

      PHKey::associate(road1dptr,(*iClustCand).pCluster);

      if( _mod_par->get_verbosity() >= MUIOO::MAX ) 
      {
        cout 
          << "mMuiRoadFinder1::track_seed - iSearch: " << iSearch << " associate 1DRoad=" << road1dptr->get()->get_key().get_obj_key() 
          << " cluster " <<	(*iClustCand).pCluster->get()->get_key().get_obj_key() << endl;
        road1dptr->get()->dump_associations<TMuiClusterO>(); 
        cout << endl;
      }
      track_seed(road1dptr, iLoop, iSearch+1);
      break;
      
    } else {
      
      // clone road
      TMui1DRoadMapO::pointer new1droadptr = bifurcate(road1dptr);
      
      if( _mod_par->get_verbosity() >= MUIOO::MAX ) 
      {
        
        cout << "mMuiRoadFinder1::track_seed - iSearch: " << iSearch 
          << " original: " << road1dptr->get()->get_key().get_obj_key() 
          << " copy: " << new1droadptr->get()->get_key().get_obj_key() 
          << endl;
        
      }
      
      PHKey::associate(new1droadptr,(*iClustCand).pCluster);
      
      if( _mod_par->get_verbosity() >= MUIOO::MAX ) 
      {
        cout 
          << "mMuiRoadFinder1::track_seed - iSearch: " << iSearch << " associate 1DRoad=" << new1droadptr->get()->get_key().get_obj_key() 
          << " cluster " <<	(*iClustCand).pCluster->get()->get_key().get_obj_key() << endl;
        new1droadptr->get()->dump_associations<TMuiClusterO>(); 
        cout << endl;
      }
      // this looks wrong. Should not we call it for the new road (?)
      track_seed(road1dptr, iLoop, iSearch+1);
      newRoadCount++;
    
    }
  
  }
  clusterList.clear();
  return 0;
}

//_________________________________________________________________________________
int mMuiRoadFinder1::make2d(TMuiRoadMapO::pointer cRoad,
         TMui1DRoadMapO::pointer pRoadH,
         TMui1DRoadMapO::pointer pRoadV)
{

  TMutFitPar fitH = pRoadH->get()->get_fit_par();
  TMutFitPar fitV = pRoadV->get()->get_fit_par();

  // associate horizontal road clusters to road
  TMuiClusterMapO::key_iterator iClustH =pRoadH->get()->get_associated<TMuiClusterO>();
  while(TMuiClusterMapO::pointer pClust = iClustH.next()) PHKey::associate(pClust, cRoad);
  
  if( _mod_par->get_verbosity() >= MUIOO::MAX )
  cout << "mMuiRoadFinder1::make2d - road=" << cRoad->get()->get_key().get_obj_key()
    << " 1DRoad=" << pRoadH->get()->get_key().get_obj_key()
    << " clusters=" << iClustH.count() 
    << endl;
  
  // associate vertical road clusters to road
  TMuiClusterMapO::key_iterator iClustV =pRoadV->get()->get_associated<TMuiClusterO>();
  while(TMuiClusterMapO::pointer pClust = iClustV.next()) PHKey::associate(pClust,cRoad);

  if( _mod_par->get_verbosity() >= MUIOO::MAX )
  cout << "mMuiRoadFinder1::make2d - road=" << cRoad->get()->get_key().get_obj_key()
    << " 1DRoad=" << pRoadV->get()->get_key().get_obj_key()
    << " clusters=" << iClustV.count() 
    << endl;

  cRoad->get()->set_nhit(pRoadH->get()->get_nhit() + pRoadV->get()->get_nhit());
  
  //Combine the FitPar of the 1D roads
  TMutFitPar fit2d;
  fit2d.set_x(fitV.get_x());
  fit2d.set_y(fitH.get_y());
  fit2d.set_z(fitH.get_z());
  fit2d.set_dxdz(fitV.get_dxdz());
  fit2d.set_dydz(fitH.get_dydz());
  if(pRoadV->get()->get_depth()>pRoadH->get()->get_depth()) 
  {
    cRoad->get()->set_depth(pRoadV->get()->get_depth());
    fit2d.set_z_end(fitV.get_z_end());
  } else {
    cRoad->get()->set_depth(pRoadH->get()->get_depth());
    fit2d.set_z_end(fitH.get_z_end());
  }
  
  fit2d.set_z_begin( (abs(fitH.get_z_begin())<abs(fitV.get_z_begin()) ) 
    ? fitH.get_z_begin() 
    : fitV.get_z_begin() );

  int vDOF = pRoadV->get()->get_nhit() + (int)(pRoadV->get()->get_fitweight(0)) - 2;
  int hDOF = pRoadH->get()->get_nhit() + (int)(pRoadH->get()->get_fitweight(0)) - 2 ;

  cRoad->get()->set_freedom(vDOF+hDOF);

  // JLN - fix bug - should be OR of H and V
  cRoad->get()->set_gapbit(pRoadH->get()->get_gapbit() | (pRoadV->get()->get_gapbit() << 5));

  // DS - the vertical and horizontal fits have chi-square values, not divided
  // by ndof. For consistency with other chi-square values,
  // the 2d road values are reduced chi-squares though, i.e. divided by ndof
  double chisq = fitV.get_chi_square() + fitH.get_chi_square();
  double reduced_chisq( ((vDOF + hDOF) >0 ) ? chisq/(vDOF + hDOF) : 0.0 );
  fit2d.set_chi_square( reduced_chisq );

  /* 
    set covariance matrix. Is the sum of the two 1D covariance matrices
    since all 'unused' elements there have been set to 0
  */
  for( unsigned int i=0; i<4; i++ )
  for( unsigned int j=0; j<4; j++ )
  fit2d.set_covar( i, j, fitV.get_covar( i, j ) + fitH.get_covar( i, j ) ); 

  cRoad->get()->set_fit_par(fit2d);

  // previously not set parameters
  cRoad->get()->set_road_quality(chisq);
  UShort_t gapmatch = pRoadH->get()->get_gapbit() & pRoadV->get()->get_gapbit();
  if (gapmatch != 0) cRoad->get()->set_max_hit_plane(2);
  else	cRoad->get()->set_max_hit_plane(1);

  // associate the 1D roads to the road
  PHKey::associate( cRoad, pRoadH );
  PHKey::associate( cRoad, pRoadV );

  if( _mod_par->get_verbosity() >= MUIOO::MAX )
  cout << "mMuiRoadFinder1::make2d - depth=" << cRoad->get()->get_depth() << " gapbit=" << int( cRoad->get()->get_gapbit() ) << endl;

  return 0;
}

//_________________________________________________________________________________
int mMuiRoadFinder1::fit1d(TMui1DRoadMapO::pointer road1dptr)
{
  double z[mMuiRoadFinder1Par::NUMFITPOINTS+1]={0.0};
  double x[mMuiRoadFinder1Par::NUMFITPOINTS+1]={0.0};
  double w[mMuiRoadFinder1Par::NUMFITPOINTS+1]={0.0};
  double cov00=0, cov11=0, cov22=0;
  int npoints = 0;
  double slope = 0.0,
    intercept = 0.0,
    chisq = 0.0,
    hit_zmin = 1E10,
    hit_zmax = 0.0;
  double dx = 0;
  double dz = 0;

  z[npoints] = _mod_par->get_zvert();
  x[npoints] = (road1dptr->get()->get_orientation()==0)
                 ? _mod_par->get_yvert() : _mod_par->get_xvert();
  w[npoints] = _mod_par->get_weight_par_1d(0);
  npoints++;
  
  TMuiClusterMapO::const_key_iterator iclust =road1dptr->get()->get_associated<TMuiClusterO>();
  
  if( _mod_par->get_verbosity() >= MUIOO::MAX ) {
    cout << "mMuiRoadFinder1::fit1d" <<endl;
    road1dptr->get()->print();
    cout << "Associated cluster count(" << iclust.count() <<")"<<endl;
  }
  
  road1dptr->get()->set_nhit(iclust.count());
  
  while(TMuiClusterMapO::const_pointer clustptr = iclust.next()) 
  {
  
    PHPoint centroid = clustptr->get()->get_centroidpos();
    PHPoint sigma = clustptr->get()->get_centroidsigma();
    road1dptr->get()->set_gapbit( road1dptr->get()->get_gapbit() | (0x1<<clustptr->get()->get_plane()));

    if( _mod_par->get_verbosity() >= MUIOO::MAX )
    cout
      << "mMuiRoadFinder1::fit1d - road=" << road1dptr->get()->get_key().get_obj_key()
      << " cluster plane=" << clustptr->get()->get_plane() 
      << " key=" << clustptr->get()->get_key().get_obj_key() << endl;

    if(road1dptr->get()->get_depth() < clustptr->get()->get_plane()) 
    {
      road1dptr->get()->set_depth(clustptr->get()->get_plane());
      
      if( _mod_par->get_verbosity() >= MUIOO::MAX )
      cout 
        << "mMuiRoadFinder1::fit1d - 1DRoad=" << road1dptr->get()->get_key().get_obj_key()
        << " depth=" << clustptr->get()->get_plane() << endl;
    
    }
      
    z[npoints]	= centroid.getZ();
    if(abs(z[npoints])<abs(hit_zmin)) hit_zmin = z[npoints];
    if(abs(z[npoints])>abs(hit_zmax)) hit_zmax = z[npoints];

    if (road1dptr->get()->get_orientation() == 0) 
    {
      x[npoints] = centroid.getY();
      w[npoints] = 1.0/(sigma.getY()*sigma.getY());
    } else {
      x[npoints] = centroid.getX();
      w[npoints] = 1.0/(sigma.getX()*sigma.getX());
    }
    /* 
       check that a point at this exact location was not already added
       - this can happen if we have the split cluster mode - neighboring fired tubes
       would be added as separate clusters, but have the same exact coordinates in one
       orientation (but not both)
    */
    bool too_close = false;
    int ipoint = npoints - 1; 
    while (!too_close && ipoint>=0) 
    {
      dx = fabs(x[npoints] - x[ipoint]);
      dz = fabs(z[npoints] - z[ipoint]);

      if (dx<0.001 && dz<0.001) { too_close = true; }
      ipoint--;
    }

    if (!too_close) npoints++;
    if(npoints>6) break;
  }

  // remove vertex point from fit if we have enough points to make a line without it
  if (npoints > 2) {
  
    // move all points one index backward
    for (int ipoint = 1; ipoint<npoints; ipoint++) 
    {
      x[ipoint-1] = x[ipoint];
      z[ipoint-1] = z[ipoint];
      w[ipoint-1] = w[ipoint];
    }
    
    // decrement number of points
    npoints--;
    
    // set the first fitweight to 0
    road1dptr->get()->set_fitweight( 0, 0 );
  }
  
  // end kludge

  // replace utiLineFit with gsl; converted fit variables from floats to doubles 
  int status = gsl_fit_wlinear(z,1,w,1,x,1,npoints,
             &intercept,&slope,&cov00,&cov11,&cov22,&chisq);
  if(status!= 0 ) {
    cout<<PHWHERE<<" Unable to fit MuID road." << endl;
    return -1;
  }
  
  double dydz, dxdz, xint, yint;
  double sig_x2, sig_y2, sig_dxdz2, sig_dydz2;
  if (road1dptr->get()->get_orientation() == 0) 
  {

    dxdz = 0.0;
    dydz = slope;
    xint = 0.0;
    yint = intercept;
    
    sig_x2 = 0;
    sig_y2 = cov00;
    sig_dxdz2 = 0;
    sig_dydz2 = cov22;
    
  } else {

    dxdz = slope;
    dydz = 0.0;
    xint = intercept;
    yint = 0.0;
    
    sig_x2 = cov00;
    sig_y2 = 0;
    sig_dxdz2 = cov22;
    sig_dydz2 = 0;

  }
  
  TMutFitPar fitpar1d(xint,yint,0.0,dxdz,dydz,chisq);
  fitpar1d.set_z_begin(hit_zmin);
  fitpar1d.set_z_end(hit_zmax);
  
  // set the covariance matrix
  fitpar1d.set_covar( 0, 0, sig_x2 );
  fitpar1d.set_covar( 1, 1, sig_y2 );
  fitpar1d.set_covar( 2, 2, sig_dxdz2 );
  fitpar1d.set_covar( 3, 3, sig_dydz2 );
  
  road1dptr->get()->set_fit_par(fitpar1d); 
  return 0;
}

//_________________________________________________________________________________
double mMuiRoadFinder1::ClusterDistance(TMui1DRoadMapO::pointer road1dptr, TMuiClusterMapO::pointer clustptr)
{
  PHVector dirV = road1dptr->get()->get_fit_par().get_tangent( road1dptr->get()->get_arm() );
  PHPoint panelX = TMuiGeometry::Geom()->FindIntersection(
    road1dptr->get()->get_arm(),
    clustptr->get()->get_plane(),
    clustptr->get()->get_panel(),
    road1dptr->get()->get_fit_par().get_point(),
    dirV
  );

  double distance = (road1dptr->get()->get_orientation()==0)
    ? abs(panelX.getY() - clustptr->get()->get_centroidpos().getY())
    : abs(panelX.getX() - clustptr->get()->get_centroidpos().getX());

  return distance;
}

//_________________________________________________________________________________
void mMuiRoadFinder1::set_interface_ptrs(PHCompositeNode* top_node)
{
  // module runtime parameters
  _mod_par = TMutNode<mMuiRoadFinder1Par>::find_node(top_node,"mMuiRoadFinder1Par");

  // IOC
  _road_map = TMutNode<TMuiRoadMapO>::find_node(top_node,"TMuiRoadMapO");
  _road1d_map = TMutNode<TMui1DRoadMapO>::find_node(top_node,"TMui1DRoadMapO");
  _cluster_map = TMutNode<TMuiClusterMapO>::find_node(top_node,"TMuiClusterMapO");
  
}

//_________________________________________________________________________________
bool mMuiRoadFinder1::intersectionOK(TMuiClusterMapO::const_pointer clustH,
             TMuiClusterMapO::const_pointer clustV) const
{
  TMuiGeometry* geom = TMuiGeometry::Geom();
  // Is the intersection point inside the panel(s) containing the H and
  // V clusters?
  TMuiPanelGeo* ph = geom->getPanel(clustH->get()->get_arm(),
    clustH->get()->get_plane(),
    clustH->get()->get_panel());
  TMuiPanelGeo* pv = geom->getPanel(clustV->get()->get_arm(),
    clustV->get()->get_plane(),
    clustV->get()->get_panel());

  PHPoint hpoint = clustH->get()->get_centroidpos();
  PHPoint vpoint = clustV->get()->get_centroidpos();

  PHPoint h1_local = ph->TransformToPanel(hpoint);
  PHPoint v1_local = ph->TransformToPanel(vpoint);

  PHPoint h2_local = pv->TransformToPanel(hpoint);
  PHPoint v2_local = pv->TransformToPanel(vpoint);

  return (ph->IsInPanel(v1_local.getX(), h1_local.getY(), 0.0) &&
      pv->IsInPanel(v2_local.getX(), h2_local.getY(), 0.0)	 );
  
}

//_________________________________________________________________________________
int mMuiRoadFinder1::flag_golden()
{
  //Loop over arms
  for (short iArm=0; iArm<TMuiChannelId::kArmsTotal; iArm++) 
  {
  
    // Reject ghost by grouping the roads
    vector<TMuiRoadOGroup*> pGroup;
    vector<TMuiRoadOGroup*>::iterator ig;
    
    TMuiRoadOGroup* iGroup;
    TMuiRoadMapO::iterator iRoad = _road_map->get(iArm);
    while(TMuiRoadMapO::pointer road2D = iRoad.next()) 
    {
      
      bool InGroup = false;
      iGroup=NULL;
      for(ig=pGroup.begin();ig!=pGroup.end();ig++)
      if ((*ig)->IsGroup(road2D)) 
      {
        
        if(!InGroup) {
        
          //This is the first group associated with the road
          (*ig)->AttachRoad(road2D);
          iGroup = (*ig);
          
        } else {
        
          // This road has already been associated with a road
          // Instead of adding it to two groups lets merge this into the first
          // then remove the first group
          iGroup->MergeGroup(*ig);
          TMuiRoadOGroup* removedGroup = (*ig);
          delete removedGroup;
          pGroup.erase(ig);	
          ig--;
        
        }
        InGroup = true;
      }
      
      if( !InGroup)
      {
        
        // This road doesn't belong to any group, create a new group
        float MutrZ;
        if(iArm == 0) MutrZ = _mod_par->get_mut_z_south();
        else MutrZ = _mod_par->get_mut_z_north();

        iGroup = new TMuiRoadOGroup(iArm,MutrZ,_mod_par->get_mut_window(),_mod_par->get_mui_window());
        iGroup->AttachRoad(road2D);
        pGroup.push_back(iGroup);
      
      }
      
    }
    
    if( _mod_par->get_verbosity() >= MUIOO::MAX )
    cout<<PHWHERE<<" Mark Golden Roads"<< endl;

    // set group index and golden mark for each road object
    // The golden flags are saved in the TMui2DRoads
    short ngroup = pGroup.size();
    for(short i=0; i<ngroup; i++)
    {
      iGroup = pGroup[i];
      iGroup->SetGroup(i);
    }

    if( _mod_par->get_verbosity() >= MUIOO::MAX ) 
    cout<<PHWHERE<<" Destroy Group Objects"<< endl;

    // destroy the all group objects
    for(short i=0; i<ngroup; i++) delete pGroup[i];

  }
  
  return 0;
}

//_________________________________________________________________________________
TMui1DRoadMapO::pointer mMuiRoadFinder1::bifurcate( TMui1DRoadMapO::pointer source_ptr )
{
  
  // insert new road
  TMui1DRoadMapO::pointer dest_ptr = _road1d_map->insert_new(
    source_ptr->get()->get_arm(),
    source_ptr->get()->get_panel(),
    source_ptr->get()->get_orientation()
    ).current();
  
  // retrieve clusters associated to source 
  TMuiClusterMapO::key_iterator cluster_iter =  source_ptr->get()->get_associated<TMuiClusterO>();
  while( TMuiClusterMapO::pointer cluster_ptr = cluster_iter.next())
  { PHKey::associate( dest_ptr , cluster_ptr); }
  
  return dest_ptr;
  
}

  
