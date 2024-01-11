#ifndef __MMPCEXMINICLUSTERS__
#define __MMPCEXMINICLUSTERS__

#include <stdio.h>
#include <string.h>
#include <iostream>
#include <vector>
#include "TMpcExMiniCluster.h"
#include "MpcExGeomToKey.h"

#define WHERE __LINE__

class TMpcExShower;
class TMpcExHitContainer;

//define a base class for minicluster filter
class MiniClusterFilter{
  public:
    MiniClusterFilter(){_g2k = MpcExGeomToKey::instance();}
    virtual ~MiniClusterFilter(){}
    //for select the main mini-cluster
    virtual bool operator()(TMpcExMiniCluster*);
    //select the sub mini-clusters for Pi0 reconstruction
    virtual bool operator()(TMpcExMiniCluster*,TMpcExMiniCluster*,float);
  private:
    MpcExGeomToKey* _g2k;
};


class mMpcExMiniClusters{
  public:
     mMpcExMiniClusters();
     mMpcExMiniClusters(TMpcExShower*,TMpcExHitContainer* );
    
     virtual void Reset();
     virtual ~mMpcExMiniClusters();

     unsigned int GetNMiniCluster();
     TMpcExMiniCluster* GetMiniCluster(unsigned int i);
     
     //construct MiniClusters
     void ConstructMiniClusters(TMpcExShower*,TMpcExHitContainer*);

     void SetDebug(bool val=true){
       _debug = val;
     }

     std::vector<TMpcExMiniCluster*> GoodMiniClusters(float mindist = -9999.0);
     
     void SetThresholdRMS(double val) {_th_rms = val;}
     void SetThresholdRMSAsy(double val) {_th_rms_asy = val;}
     void SetThresholdPKMeanDST(double val) {_th_pk_mean_dist = val;}
     void SetMaxIterate(int n) {_max_iterate = n;}

     void SetSqCuts(bool val) {_sq_cuts = val;}

     void SetFilter(MiniClusterFilter* filter){
       if(!filter) return;
       if(_filter) delete _filter;
       _filter=filter;
     }


     void VisualMiniClusters(TMpcExShower* ex_shower,const char* data_set="",double pi0_mass=-9999);

  private:
     int _arm;
     //the filter to select the main mini-cluster
     //it will not be reset when call Reset()
     MiniClusterFilter* _filter;

     std::vector<TMpcExMiniCluster*> _mini_cluster_list;
     //make a cut on Square, these Square will not
     //used in miniCluster process
     std::vector<Square*> _delete_sq_list;

     //flag for sq cuts 
     bool _sq_cuts;
     
     //flage for debug, the initial value will be true
     bool _debug;
     //thershold for clustering
     //the default value is 1
     double _th_rms;
     //max number iteration for miniclusters
     //the defualt is 100; which is the size 
     //of the MpcEx
     //ratio of (rms_x-rms_y)/(rms_x+rms_y)
     //the idea case is 0 when rms_x ~ rms_y
     //the default value will be 0.5 , when rms_x/rms_y~3
     double _th_rms_asy;
     
     //the distance between pk square and mean x y of cluster
     //we use the rms as a reference
     double _th_pk_mean_dist;
     int _max_iterate;
     
     void print_square_2d(std::vector<Square*> sq_list,TMpcExShower* ex_shower,TMpcExHitContainer* hits);

     bool IsGoodSq(Square* sq);

};


#endif
