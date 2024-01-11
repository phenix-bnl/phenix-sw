#include "MutrgDataProcessUtil.hh"

#include "MutrgPar.hh"
#include "MutrgKey.hh"
#include "MutrgHit.hh"
#include "MutrgHitArray.hh"

using namespace std;

//////////////////////////////////////////////////////////////////

void MutrgDataProcessUtil::
CountMultiplicity(const MutrgHitArray *mutrg_hits,
		  map<unsigned int,vector<int> > &nhit){
  nhit.clear();

  vector<int> nhit_oct[MutrgPar::NARM][MutrgPar::NSTATION][MutrgPar::NOCTANT];
  for(int iarm=0; iarm<MutrgPar::NARM; iarm++){
    for(int ist=0; ist<MutrgPar::NSTATION; ist++){
      for(int ioct=0; ioct<MutrgPar::NOCTANT; ioct++){
	nhit_oct[iarm][ist][ioct].resize(MutrgPar::MAX_NHITCLOCK,0);
      }
    }
  }

  MutrgHitArray::const_private_itr_pair itr_pair=mutrg_hits->Range();
  MutrgHitArray::const_private_itr itr_beg=itr_pair.first;
  MutrgHitArray::const_private_itr itr_end=itr_pair.second;
  for(MutrgHitArray::const_private_itr itr=itr_beg; itr!=itr_end; itr++){
    MutrgHit *hit=itr->second;

    int arm,st,oct,hoct,strip; // hoct and strip aree not used
    hit->GetLocation(arm,st,oct,hoct,strip);

    unsigned short hit_clk=hit->GetHitClock();
    for(int iclk=0; iclk<MutrgPar::MAX_NHITCLOCK; iclk++){
      if((hit_clk&(0x1<<iclk))==0){continue;}
      nhit_oct[arm][st][oct][iclk]++;
    }
  }

  for(int iarm=0; iarm<MutrgPar::NARM; iarm++){
    for(int ist=0; ist<MutrgPar::NSTATION; ist++){
      for(int ioct=0; ioct<MutrgPar::NOCTANT; ioct++){
	unsigned int key=MutrgKey::LocToKey(iarm,ist,ioct,0,0);
	nhit.insert(pair<unsigned int,vector<int> >
		    (key,nhit_oct[iarm][ist][ioct]));
      }
    }
  }

  return;
}

/////////////////////////////////////////////////////////////////

void MutrgDataProcessUtil::Clustering(const MutrgHitArray *mutrg_hits1,
				      unsigned int mask,
				      vector<vector<MutrgHit*> > &clusters){
  vector<MutrgHit*> cluster(0);

  int arm_pre=-1;
  int st_pre=-1;
  int oct_pre=-1;
  int stripo_pre=-1;
  unsigned int hit_clk_pre=0;

  MutrgHitArray::const_private_itr_pair itr_pair=mutrg_hits1->Range();
  MutrgHitArray::const_private_itr itr_beg=itr_pair.first;
  MutrgHitArray::const_private_itr itr_end=itr_pair.second;
  for(MutrgHitArray::const_private_itr itr=itr_beg; itr!=itr_end; itr++){
    unsigned int key=itr->first;
    MutrgHit *hit=itr->second;

    unsigned int hit_clk=hit->GetHitClock();
    if((hit_clk&mask)==0){continue;}

    int arm,st,oct,hoct,strip;
    MutrgKey::KeyToLoc(key,arm,st,oct,hoct,strip);

    int nstp_h0=MutrgPar::NSTRIP_IN_HALFOCTANT(arm,st,oct,0);
    int stripo=strip+hoct*nstp_h0;

    if(arm_pre>=0 &&
       (arm!=arm_pre || st!=st_pre ||
	oct!=oct_pre || stripo!=stripo_pre+1 ||
	(hit_clk&mask)==0 || (hit_clk_pre&mask)==0)){
      clusters.push_back(cluster);
      cluster.clear();
    }

    cluster.push_back(hit);
    arm_pre=arm;
    st_pre=st;
    oct_pre=oct;
    stripo_pre=stripo;
    hit_clk_pre=hit_clk;
  }

  // add last cluster
  if(cluster.size()>0){clusters.push_back(cluster);}

  return;
}

///////////////////////////////////////////////////////////////////

int MutrgDataProcessUtil::Clustering(MutrgHitArray *mutrg_hits,
				     int max_cluster_size){
  if(max_cluster_size<1){
    printf("Error - MutrgDataprocessUtill::Clustering : ");
    printf("max_cluster_size must be >=1.\n");
    return -1;
  }

  for(int iclk=0; iclk<MutrgPar::MAX_NHITCLOCK; iclk++){
    vector<vector<MutrgHit*> > clusters;

    MutrgDataProcessUtil::Clustering(mutrg_hits,(0x1<<iclk),clusters);

    // split cluster with cluster_size>max_cluster_size
    vector<vector<MutrgHit*> > clusters_split;
    for(unsigned int icl=0; icl<clusters.size(); icl++){
      vector<MutrgHit*> &cl=clusters[icl];

      //if(cl.size()==1 && !cl[0]->GetHitClock(iclk)){continue;}

      while(1){
	if((int)cl.size()<=max_cluster_size){
	  clusters_split.push_back(cl);
	  break;
	}

	vector<MutrgHit*> cl_sub(cl.begin(),cl.begin()+max_cluster_size);
	clusters_split.push_back(cl_sub);

	cl.erase(cl.begin(),cl.begin()+max_cluster_size);
      }
    }

    // pick up center strip
    for(unsigned int icl=0; icl<clusters_split.size(); icl++){
      vector<MutrgHit*> &cl=clusters_split[icl];

      unsigned int nhit=cl.size();
      for(unsigned int ihit=0; ihit<nhit; ihit++){
	cl[ihit]->SetHitClock(iclk,false);
      }

      //int hit_cent=(int)(nhit/2);
      int hit_cent=(int)((nhit-1)/2); // original
      cl[hit_cent]->SetHitClock(iclk,true);
    }
  }

  return 0;
}

/////////////////////////////////////////////////////////////

void MutrgDataProcessUtil::WarnDefFuncCall(const char *class_name,
					   const char *func_name){
  printf("Warning - %s : Default function was called.(%s)\n",
         class_name,func_name);
  return;
}

//////////////////////////////////////////////////////////////////
