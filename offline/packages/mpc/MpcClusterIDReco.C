#include <getClass.h>
#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>
#include <recoConsts.h>


#include <mpcClusterContainer.h>
#include <mpcClusterContent.h>
#include <mpcGeaClusterContainer.h>
#include <mpcGeaClusterContent.h>
#include <MpcMap.h>
#include <MpcClusterIDReco.h>



#include <TString.h>

#include <map>
#include <algorithm>
#include <iostream>

using namespace std;
using namespace findNode;




struct TR
{
  int node;
  float edep; //energy track deposited in cluster
};

struct geaTR
{
  int node;
  mpcGeaClusterContent* geaclus;
};



bool comp_tr(TR a, TR b){  return (a.edep > b.edep); }
bool comp_geatr(geaTR a, geaTR b){  
  return (a.geaclus->get_edep() > b.geaclus->get_edep()); }
bool comp_geaclus(mpcGeaClusterContent* a, mpcGeaClusterContent* b){  
  return ( a->get_edep() > b->get_edep() ); 
}


MpcClusterIDReco::MpcClusterIDReco(const std::string &name, int nnodes): SubsysReco(name)
{
  n_nodes = nnodes;
}


MpcClusterIDReco::~MpcClusterIDReco()
{
}

int MpcClusterIDReco::EndRun(const int runnumber)
{
  return EVENT_OK;
}

int MpcClusterIDReco::Init(PHCompositeNode *topNode)
{
  return EVENT_OK;
}



int MpcClusterIDReco::InitRun(PHCompositeNode *topNode)
{
  mpcmap = findNode::getClass<MpcMap>(topNode,"MpcMap");
  if (!mpcmap)
    {
      cout << PHWHERE << "Unable to get MpcMap, is Node missing?" << endl;
      return False;
    }
  
  return EVENT_OK;
}

void MpcClusterIDReco::Print(const std::string&) const
{
  cout << "MpcClusterIDReco::Print():" << endl;
  if(!mpcclus) cout << "mpcclus is null...returning\n";
  else{
    for(unsigned int i=0;i<mpcclus->size();i++){
      mpcClusterContent* clus = mpcclus->getCluster(i);
      if(clus->ecore() > 0.5){
	clus->print();
      }
    }
  }
}

int MpcClusterIDReco::process_event(PHCompositeNode *topNode)
{

  //  Fun4AllServer* se = Fun4AllServer::instance();
  

  mpcclus = findNode::getClass<mpcClusterContainer>(topNode,"mpcClusterContainer");
  if (!mpcclus)
    {
      cout << PHWHERE << "Unable to get mpcClusterContainer, is Node missing?" << endl;
      return False;
    }


  
  for(int inode=0;inode<n_nodes;inode++){
    TString name = "mpcGeaClusterContainer";
    if(inode > 0) name+=inode;
    
    mpcgeaclus = findNode::getClass<mpcGeaClusterContainer>(topNode,name.Data());
    
    if (!mpcgeaclus)
      {
	cout << PHWHERE 
	     << "Unable to get mpcGeaClusterContainer, is Node missing?: iteration " 
	     << inode << endl;
	return -2;
      }
    mpcgeaclus_vector.push_back(mpcgeaclus);
  }


  //now we have all mpcgeaclus objects
  //we track what the cluster composition is of two biggest contributors to cluster energy

  FillClusters();
  
  if(verbosity >1) Print();

  mpcgeaclus_vector.clear();

  return EVENT_OK;  

}


void MpcClusterIDReco::FillClusters()
{

  for(unsigned int iclus=0;iclus<mpcclus->size();iclus++)
    {
      mpcClusterContent* clus = mpcclus->getCluster(iclus);
      if(clus->ecore() < 0.5) continue;
      int clusfee = clus->towerid(0);
      

      vector<geaTR> tracks;
      //      vector<mpcGeaClusterContent*> geacontent_vector;

      
      int nn = mpcgeaclus_vector.size();
      if(verbosity > 1) cout << "nnodes is: " << nn << endl;
      for(int inode = 0; inode < nn;inode++){
	mpcgeaclus = mpcgeaclus_vector[inode];
	
	for(unsigned int itow=0;itow<mpcgeaclus->size();itow++){
	  mpcGeaClusterContent* geaclus = mpcgeaclus->getCluster(itow);
	  if(geaclus->get_edep() < 0.2 || geaclus->get_fraction() < 0.02) continue;
	  int feech = geaclus->get_ch();
	  if(feech != clusfee) continue;
	  //	  geacontent_vector.push_back(geaclus);
	  geaTR geatr = {inode,geaclus};
	  tracks.push_back(geatr);
	}
      }
      //      sort(geacontent_vector.begin(),geacontent_vector.end(),comp_geaclus);
      sort(tracks.begin(),tracks.end(),comp_geatr);
      //      int npart = min(4,(int)geacontent_vector.size()); //fill at most 4 particles
      int npart = min(4,(int)tracks.size()); //fill at most 4 particles

      if(verbosity > 1) cout << "npart: " << npart << endl;
      
      clus->set_sim_size( npart );

      //next we fill MpcClusterContent w/ values
      for(int igea=0;igea<npart;igea++){
	
	mpcGeaClusterContent* mg = tracks[igea].geaclus;
	//geacontent_vector[igea];
	int whichnode = tracks[igea].node;
	
	clus->set_sim(igea,
		      mg->get_itorigin(),
		      mg->get_id(),
		      0,
		      mg->get_edep(),
		      mg->get_fraction_tr(),
		      mg->get_type(),
		      whichnode,
		      mg->get_parent_type(),
		      mg->get_parent_id());
      }
      clus->set_sim_type( GetType2(clus) );
    }
  
}


int MpcClusterIDReco::GetType(mpcClusterContent* clus){
  //only consider 1st 2 elements
  //0 gamma pi0
  //1 eta gamma
  //2 merged pi0
  //3 "direct photon" = non eta/pi0 photon
  //4 e+/-
  //5 p+/-
  //6 n
  //7 pi+/-
  //8 something merged
  //9 other

  int size = clus->get_sim_size();
  if(size <=0) return 9; //basically unknown


  int it1=-9999,id1=-9999,pri1 = -9999;
  float edep1=0,frac1 = 0;
  int typ1=-9999,node1=-9999,partyp1=-9999,parid1 = -9999;
  
  clus->get_sim(0,it1,id1,pri1,edep1,frac1,
		typ1,node1,partyp1,parid1);


  
  int it2=-9999,id2=-9999,pri2 = -9999;
  float edep2=0,frac2 = 0;
  int typ2=-9999,node2=-9999,partyp2=-9999,parid2 = -9999;
  if(size >=2){
    clus->get_sim(1,it2,id2,pri2,edep2,frac2,
		  typ2,node2,partyp2,parid2);
  }



  if(typ1 == 22 && partyp1 == 111){ //pi0
    

    if(parid1 == parid2 && node1 == node2 && edep2 > 0.5 && frac2 > 0.5){ //requires photon2 to deposit 1 GeV and > 50% of its energy into the cluster
      return 2; //merged pi0
    }
    else if(edep2 > 0.5 && frac2 > 0.5){ //requires photon2 to deposit 1 GeV and > 50% of its energy into the cluster
      return 1; //merged gamma from pi0
    }
    else 
      return 0; //pi0 without merging
  }
  else if(typ1 == 22 && partyp1 == 221){//eta
    return 3;
  }
  else if(edep2 > 1. && frac2 > 0.5){ //requires particle2 to deposit 1 GeV and > 50% of its energy into the cluster
    return 8; //merged something
  }
  else if(typ1 == 22){//"direct photon"
    return 4;
  }
  else if(typ1 == 2212 || typ1 == -2212){
    return 5;
  }
  else if(typ1 == 2112){
    return 6;
  }
  else if(typ1 == 211 || typ1 == -211){
    return 7;
  }
  return 9;
}

int MpcClusterIDReco::GetType2(mpcClusterContent* clus){
  //only consider 1st 2 elements
  //0 gamma pi0
  //1 eta gamma
  //2 merged pi0
  //3 "direct photon" = non eta/pi0 photon
  //4 e+/-
  //5 p+/-
  //6 n
  //7 pi+/-
  //8 something merged
  //9 other

  int size = clus->get_sim_size();
  if(size <=0) return 9; //basically unknown


  /*  vector<int> vit;
  vector<int> vid;
  vector<int> vpri;
  vector<float> vedep;
  vector<float> vfrac;
  vector<int> vtyp;
  vector<int> vnode;
  vector<int> vpartyp;
  vector<int> vparid;
  */
  


  int it1=-9999,id1=-9999,pri1 = -9999;
  float edep1=0,frac1 = 0;
  int typ1=-9999,node1=-9999,partyp1=-9999,parid1 = -9999;
  /*  for(int i=0;i<size;i++){
    clus->get_sim(i,it1,id1,pri1,edep1,frac1,
		  typ1,node1,partyp1,parid1);
    
    vit.push_back(it1);
    vid.push_back(id1);
    vpri.push_back(pri1);
    vedep.push_back(edep1);
    vfrac.push_back(frac1);
    vtyp.push_back(typ1);
    vnode.push_back(node1);
    vpartyp.push_back(partyp1);
    vparid.push_back(parid1);
    
  }
  */
  clus->get_sim(0,it1,id1,pri1,edep1,frac1,
		typ1,node1,partyp1,parid1);


  
  int it2=-9999,id2=-9999,pri2 = -9999;
  float edep2=0,frac2 = 0;
  int typ2=-9999,node2=-9999,partyp2=-9999,parid2 = -9999;
  if(size >=2){
    clus->get_sim(1,it2,id2,pri2,edep2,frac2,
		  typ2,node2,partyp2,parid2);
  }

  float ecore = clus->ecore();

  float efrac_tot1 = edep1/ecore;
  /*
  float efrac_tot2 = 0;
  if(size >=2) efrac_tot2 = edep2/ecore;
  */

  if(typ1 == 22 && partyp1 == 111){ //pi0
    
    
    

    if(parid1 == parid2 && node1 == node2 && edep2 > 1 && frac2 > 0.5){ //requires photon2 to deposit 1 GeV and > 50% of its energy into the cluster
      return 6; //merged pi0
    }
    else if(efrac_tot1 > 0.9) return 0;
    else if(efrac_tot1 > 0.8) return 1;
    else if(efrac_tot1 > 0.6) return 2;
    else if(efrac_tot1 > 0.5) return 3;
    else if(efrac_tot1 > 0.3) return 4;
    else return 5;
    
  }
  else if(typ1 == 22 && partyp1 == 221){//eta
    return 7;
  }
  else if(typ1 == 22){//"direct photon"
    return 8;
  }
  else if(typ1 == 2212 || typ1 == -2212){
    return 8;
  }
  else if(typ1 == 2112){
    return 8;
  }
  else if(typ1 == 211 || typ1 == -211){
    return 8;
  }
  return 9;
}

