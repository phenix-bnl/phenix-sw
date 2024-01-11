#include <iostream>
#include <fstream>


#include <MpcGeaClusterReco.h>
#include <mpcClusterContainer.h>
#include <mpcClusterContent.h>
#include <mpcTowerContainer.h>
#include <mpcTowerContent.h>
#include <mpcGeaTowerContainer.h>
#include <mpcGeaTowerContent.h>
#include <mpcGeaClusterContainer.h>
#include <mpcGeaClusterContent.h>
#include <mpcGeaClusterContainerV1.h>
#include <mpcGeaClusterContentV1.h>

#include <PHPythiaHeader.h>
#include <PHPythiaContainer.h>
#include <TMCParticle.h>

#include <getClass.h>
#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>
#include <MpcMap.h>
#include <algorithm>
//#include <recoConsts.h>
#include <map>
#include <vector>

#include <iostream>

using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;
typedef map<int, map<int, map<int,float> > > map3d;
typedef map<int, map<int,float> > map2d;
typedef map<int,float> map1d;


//taken from MpcSectorRecV2.C

MpcGeaClusterReco::MpcGeaClusterReco(const std::string &name, const std::string gname,std::string pname, int node_index,int alg): SubsysReco(name)
{
  whichnode=node_index;
  fAlgorithm = alg; 
  gea_name = gname;
  pythia_name = pname;
  //1 means itorigin, sharing
  //2 means itincoming, sharing
  //3 means itincoming, 1 cluster per track (not the converse)
  
  
  fShowPar2[0][0]=0.109076;  
  fShowPar2[0][1]=-0.0020524;  
  fShowPar2[0][2]=4.83446e-05;  
  fShowPar2[0][3]=-3.58957e-07;  
  
  fShowPar2[1][0]=1.6985;  
  fShowPar2[1][1]=-0.00639118;  
  fShowPar2[1][2]=0.000153674;  
  fShowPar2[1][3]=-1.17523e-06;  
  
  fShowPar2[2][0]=0.190027;  
  fShowPar2[2][1]=-0.00752877;  
  fShowPar2[2][2]=0.000176111;  
  fShowPar2[2][3]=-1.28594e-06;  
  
  fShowPar2[3][0]=2.42983;  
  fShowPar2[3][1]=-0.0417858;  
  fShowPar2[3][2]=0.000893255;  
  fShowPar2[3][3]=-6.32293e-06;  
  
  fShowPar2[4][0]=0.516242;  
  fShowPar2[4][1]=0.010746;  
  fShowPar2[4][2]=-0.000251936;  
  fShowPar2[4][3]=1.84155e-06;  
  
  fShowPar2[5][0]=15.0943;  
  fShowPar2[5][1]=-0.0981804;  
  fShowPar2[5][2]=0.00261719;  
  fShowPar2[5][3]=-1.90878e-05;  
  
}


MpcGeaClusterReco::~MpcGeaClusterReco()
{
}

int MpcGeaClusterReco::EndRun(const int runnumber)
{
  return EVENT_OK;
}

int MpcGeaClusterReco::Init(PHCompositeNode *topNode)
{
  int iret = CreateNodeTree(topNode);
  return iret;
}

int MpcGeaClusterReco::InitRun(PHCompositeNode *topNode)
{
  mpcmap = findNode::getClass<MpcMap>(topNode,"MpcMap");
  if (!mpcmap)
    {
      cout << PHWHERE << "Unable to get MpcMap, is Node missing?" << endl;
      return False;
    }
  
  return EVENT_OK;
}

int MpcGeaClusterReco::process_event(PHCompositeNode *topNode)
{



  Fun4AllServer *se = Fun4AllServer::instance();
  geaNode = 0;
  if(gea_name == "DEFAULT"){
    geaNode = topNode;
  }
  else{
    geaNode= se->topNode(gea_name.c_str());
  }


  if(!geaNode){
    cout << PHWHERE << "Unable to node for mpcGeaTowerContent, did i use the wrong node?" << endl;
    return 0;
  }
  
  
  pythiaNode = 0;
  if(pythia_name != "NONE"){
    pythiaNode = se->topNode(pythia_name.c_str());
    
    if(pythiaNode == 0){
      static int npywarn = 0;
      if(npywarn < 5){
	cout << "not using pythia nodes b/c files do not exist"
	     <<" ...setting node name from " << pythia_name << ", to NONE\n";
	npywarn++;
      }
      pythia_name = "NONE";
    }
    

    
  }
  
  
  if(pythia_name != "NONE" && pythiaNode != 0){
    
    primary = findNode::getClass<primaryWrapper>(geaNode, "primary");
    phpythia = findNode::getClass<PHPythiaContainer>(pythiaNode,"PHPythia");
    if(phpythia == 0) phpythia = findNode::getClass<PHPythiaContainer>(pythiaNode,"PHHijing");
    if(!primary || !phpythia){
      cout << "phpythia, primary not found: " 
	   << phpythia << "\t"
	   << primary << "\n";
      return -2;
    }

  }
  
    
  
  mpcclus = findNode::getClass<mpcClusterContainer>(topNode,"mpcClusterContainer");
  if (!mpcclus)
    {
      cout << PHWHERE << "Unable to get mpcClusterContainer, is Node missing?" << endl;
      return False;
    }

  mpctow = findNode::getClass<mpcTowerContainer>(topNode,"mpcTowerContainer");
  if (!mpctow)
    {
      cout << PHWHERE << "Unable to get mpcTowerContainer, is Node missing?" << endl;
      return False;
    }

  mpcgeatow = findNode::getClass<mpcGeaTowerContainer>(geaNode,"mpcGeaTowerContainer");
  if (!mpcgeatow)
    {
      cout << PHWHERE << "Unable to get mpcGeaTowerContainer, is Node missing?" << endl;
      return False;
    }

  TString nodename = "mpcGeaClusterContainer";
  if(whichnode > 0) nodename+=whichnode;
  mpcgeaclus = findNode::getClass<mpcGeaClusterContainer>(topNode,nodename.Data());
  if (!mpcgeaclus)
    {
      cout << PHWHERE << "Unable to get mpcGeaClusterContainer, is Node missing?" << endl;
      return False;
    }

  
  int retval = 0;
  if(fAlgorithm <=3){
    retval = process_event_bam(topNode,fAlgorithm);
  }
  else if(fAlgorithm > 3){
    retval = process_event_mc(topNode,fAlgorithm);
  }

  if(pythiaNode != 0 && pythia_name != "NONE"){
    process_event_pythia();
  }

  if(verbosity >1) Print();

  return retval;
}


//this is optional
int MpcGeaClusterReco::process_event_pythia()
{

  for(unsigned int itow=0;itow<mpcgeaclus->size();itow++){
    mpcGeaClusterContent* geaclus = mpcgeaclus->getCluster(itow);
    if(geaclus->get_edep() < 0.2 || geaclus->get_fraction() < 0.05) continue;
    int itor = geaclus->get_itorigin();
    
    TMCParticle* part = 0;
    int primarynum = -1;
    int npr = primary->RowCount();
    for(int ipr=0;ipr<npr;ipr++){
      int tt = primary->operator[](ipr).true_track;
      if(tt == itor){
	if(verbosity > 2) {
	  cout << "found a match for primary true_track info: " << tt << " primary row is: " << ipr << endl;
	}
	primarynum = ipr;
	break;
      }
    }

    
    if(primarynum >= 0){
      part = phpythia->getPrimaryParticle( primarynum );
    } 
    
    if(part != 0){
      geaclus->set_type( part->GetKF() );
      geaclus->set_id( (int)(phpythia->getLineNumber(part)-1) );
      geaclus->set_py_energy( part->GetEnergy() );
      TMCParticle* parent = phpythia->getParent( part );
      if(parent != 0){
	geaclus->set_parent_type( parent->GetKF() );
	geaclus->set_parent_id( (int)(part->GetParent()-1) );
      }
    }
    
  } //end for

  return 1;
}
  

int MpcGeaClusterReco::process_event_bam(PHCompositeNode* topNode, int alg){
  
  //The maps may seem confusing
  //for 2D maps or map2d, there are two integer keys (tow,clus or track) and the value is the energy in the tower
  //inv just means switch the index that comes first in the key, e.g. invtowtr means trtow

  
  map2d towtrmap; //for each tower, track, this holds the gea energy
  map2d invtowtrmap; //for each track, tow, this holds the gea energy
  
  map1d trackmap; //for each track, this holds the gea energy

  map<int,int> itr_map; //tracks itr to specific geatowercontent object
  map<int,int> itin_map; //tracks itincoming to specific geatowercontent object
  map<int,int> itorigin_map; //tracks itorigin to specific geatowercontent object
  
  //note: itr stands for itrack
  //alg 1 uses itr = itorigin
  //algs 2,3 use itr = itincoming
  
  //filling towtrmap
  int nmpcgea = mpcgeatow->size();
  static bool good_itincoming = 0;
  if(!good_itincoming && fAlgorithm >=2 && nmpcgea > 0){
    //just check this once and cross our fingers after that
    int itin = mpcgeatow->getTower(0)->get_itincoming();
    int oldalg = fAlgorithm;
    if(itin < 0){
      fAlgorithm = 1;
      
      cout << "MpcGeaClusterReco: Switching from algorigthm # " << oldalg << " to algorigthm 1 b/c itincoming is not good for this simDST\n";
      
    }
    else{
      cout << "MpcGeaClusterReco: Good itincoming...using algorithm " << fAlgorithm << endl;
      good_itincoming = 1;
    }

  }
  
  for(int igea=0;igea<nmpcgea;igea++)
    {
      float edep  = mpcgeatow->getTower(igea)->get_edep();
      //    if(edep < 0.01) continue; //ignore towers that where the deposited energy is < 0.01 
      int twr = mpcgeatow->getTower(igea)->get_ch();
      int itincoming = mpcgeatow->getTower(igea)->get_itincoming();  
      
      //since itincoming doesn't always work, we presently don't have much of a choice with this; unless sims are rerun
      int itorigin = mpcgeatow->getTower(igea)->get_itorigin();
      int itr = itincoming;
      if(fAlgorithm == 1) itr = itorigin; //we do this b/c itincoming doesn't exist
      mpcGeaTowerContent* gea = mpcgeatow->getTower(igea);
      if(verbosity > 3) gea->print();
      
      towtrmap[twr][itr] = edep*1.05/0.918;
      invtowtrmap[itr][twr] = edep*1.05/0.918;
      trackmap[itr]+=edep; //don't use ecore for this
      itr_map[itr] = igea; //overwriting this is ok.
      //      itin_map[itr] = igea; //overwriting this is ok.
      //itorigin_map[it] = igea; //overwriting this is ok.
    }
  if(verbosity > 2) cout << "num tracks is: " << itr_map.size() << endl;

  
  map2d towclusmap; //for each tower, cluster, this holds the energy
  map2d invtowclusmap; //for each tower, cluster, this holds the energy

  int nclus = mpcclus->size();
  if(verbosity >2) cout << "\n" << "BeginData\n";
  for (int iclus=0; iclus<nclus; iclus++)
    {
      mpcClusterContent *clus = mpcclus->getCluster( iclus );

      float ecore = clus->ecore();
      //      if(ecore < 0.5) continue;   //here only consider channels w/ e > 0.3 GeV...can get rid of problems w/ duplicate clusters for same tower
      if(verbosity > 2) cout << clus->towerid(0) << "\t" << ecore << "\t chi2: " << clus->chi2core() << "\t disp: " << max(clus->dispx(),clus->dispy()) << endl;
      SetProfileParametersV2_2(clus->e());
      //      int feech = clus->towerid(0);
      int ntowers = clus->multiplicity();
      int ncoretwrs = 0;
      float myecore = 0;
      for (int it=0; it<ntowers; it++)
        {
	  int twrid = clus->towerid(it);
	  bool status = GetEcoreStatus(clus,twrid);
	  if( status == 0) continue;

	  ncoretwrs++;
	  towclusmap[twrid][iclus] = clus->partesum(it)/0.918;
	  invtowclusmap[iclus][twrid] = clus->partesum(it)/0.918;
	  myecore+=clus->partesum(it)/0.918;
	}
      if(verbosity > 2)
	cout << "my ecore is: " << myecore << endl;
      //      if(verbosity > 2) cout << "ncoretwrs is: " << ncoretwrs << endl;
    }
  
  if(verbosity >2) cout << "\n" << "BeginSimulation\n";
  
  //for each cluster, sum up energy deposited by each track
  //best way to do this: go through each tower of cluster, look
  

  map2d clustrmap; //for each cluster,track, this holds the energy
  map3d esubmap; //for each cluster,track, this holds the energy
  map1d etottrmap; //for each track, this holds the energy
  map2d new_clustrmap; //for each cluster,track, this holds the energy
  map1d new_etottrmap; //for each track, this holds the energy
  map1d old_etottrmap; //for each track, this holds the energy


//   for(map2d::iterator iter1 = invtowtrmap.begin(); iter1 != invtowtrmap.end(); ++iter1){
//     int itr = iter1->first;
//     for( map1d::iterator iter2 = invtowtrmap[itr].begin(); iter2 != invtowtrmap[itr].end(); ++iter2){
//       int itwr = iter2->first; float itr_e = iter2->second;
//       if(towclusmap[itwr].size() > 0)  etottrmap[itr]+=itr_e;
//     }
//   }
  
  
  for(map2d::iterator iter1 = invtowclusmap.begin(); iter1 != invtowclusmap.end(); ++iter1){
    int iclus = iter1->first;
    for( map1d::iterator iter2 = invtowclusmap[iclus].begin(); iter2 != invtowclusmap[iclus].end(); ++iter2){
      int itwr = iter2->first; //float tow_e = iter2->second;
      //here we assume that each tower that has energy also has a track in it
      for( map1d::iterator iter3 = towtrmap[itwr].begin(); iter3 != towtrmap[itwr].end(); ++iter3){
	int itr = iter3->first; float itr_e = iter3->second;
	
	//here's the tricky part...have to decide how much energy to associate w/ each cluster
	//if particles don't overlap this is easy
	//how about we try to make is each track is uniquely associated w/ a cluster...pretty simple idea in practice.
	//another idea      if(rawtr == tr[itr]) {tracks[itr][iclus].e_dep+=minimum(e,rawe); break;}
	etottrmap[itr]+=itr_e; //presently an overestimate...but we also overestimate clustrmap as well...corrected later
	clustrmap[itr][iclus]+=itr_e;
	if(towclusmap[itwr].size() > 1){
	  esubmap[itr][iclus][itwr] = itr_e;
	}
	//first pass we store all energy from track into clusters...this overestimates the total energy
	//next we do n iterations
      }
    }
  }

  old_etottrmap = etottrmap;
  //next visit only towers that have multiple clusters
  //need fraction of energy from each track cluster in these towers
  //specify tower
  //speify clusters
  //specify tracks
  int n_itr = 15;
  for(int itr = 0;itr<n_itr;itr++){
    for(map2d::iterator iter1 = towclusmap.begin(); iter1 != towclusmap.end(); ++iter1){
      int itwr = iter1->first;
      if(towclusmap[itwr].size() <=1) continue;   //everything is okay if there is only 1 cluster with this tower
      for( map1d::iterator iter2 = towclusmap[itwr].begin(); iter2 != towclusmap[itwr].end(); ++iter2){
	int iclus = iter2->first; //float tow_e = iter2->second;
	//here we assume that each tower that has energy also has a track in it
	map1d fracmap;
	for( map1d::iterator iter3 = towtrmap[itwr].begin(); iter3 != towtrmap[itwr].end(); ++iter3){
	  int itr = iter3->first; float itr_e = iter3->second;
	  float esub = esubmap[itr][iclus][itwr]; //amount we put in there last time
	  float eclus = clustrmap[itr][iclus]; //nominal amount of energy deposited in cluster by track
	  float etottr = etottrmap[itr]; //total amount of energy deposited into ecore towers;

	  if(etottr <= 0){ cout << "something wrong w/ total energy...continuing\n"; continue;}

	  float efrac = eclus/etottr; //fraction of track energy deposited into this cluster
	  float edep = efrac*itr_e;  //we put this fraction of energy into track
	  esubmap[itr][iclus][itwr] = edep;  //record this for the next iteration
	  float new_e_clus = eclus-esub+edep;
	  clustrmap[itr][iclus] = new_e_clus;
	  new_etottrmap[itr] = etottr - esub + edep;

	  if(efrac < -0.02) { //change this soon
	    clustrmap[itr][iclus] = eclus - itr_e;
	    new_etottrmap[itr] = etottrmap[itr] - itr_e + edep;
	  }

	}
      }
    }
    old_etottrmap = etottrmap; //slightly wasteful assignemnt statements...should change it if need to save time...could do some pointer magic
    etottrmap = new_etottrmap;
  }
  
  
 
  
  map2d clustrmap_sngl; //for each cluster,track, this holds the energy
  map2d clustrmap_dbl; //for each cluster,track, this holds the energy

  map1d clusemap_sngl; //for each cluster, this holds the total energy from deposited tracks
  map1d clusemap; //for each cluster, this holds the total energy from deposited tracks

  //1 cluster per track...now we find the maximum
  for(map2d::iterator iter1 = clustrmap.begin(); iter1 != clustrmap.end(); ++iter1){
    int itr = iter1->first;
    float max_e=0;    int max_clus = -1;
    for( map1d::iterator iter2 = clustrmap[itr].begin(); iter2 != clustrmap[itr].end(); ++iter2){
      //if(verbosity > 3) cout << "iterator2.first: " << iter2->first << "iterator2.second: " << iter2->second << endl;
      int iclus = iter2->first; float itr_e = iter2->second;
      //      float etot_tr = etottrmap[itr];
      //      cout << "etot_tr: " << etot_tr << endl;
//       if(etot_tr <= 0) {cout << "something wrong with etottrmap...continuing\n"; cout << etot_tr << endl;continue;}
//       float frac_tot = itr_e/etot_tr;
//       if(frac_tot > 1){ cout << "something wrong...tower has more than 100% of tracks energyn\n"; continue;}

//       if(itr_e < 0.02 || frac_tot < 0.02){
// 	//	etottrmap[itr] = etot_tr - itr_e; //subtract it off if it contributes very small percentage
// 	continue;
//       }

      clusemap[iclus]+=itr_e;
      clustrmap_dbl[iclus][itr] = itr_e;

      if(itr_e > max_e){
	max_clus = iclus;
	max_e = itr_e;
      }
    }
    if(max_clus >=0){
      clustrmap_sngl[max_clus][itr] = max_e;//this is what we need to do b/c of ecore/leakage
      clusemap_sngl[max_clus]+=max_e;//this is what we need to do b/c of ecore/leakage
    }
  }


  map2d *final_clustrmap;
  final_clustrmap = &clustrmap_dbl;
  if(fAlgorithm == 0 || fAlgorithm == 2) final_clustrmap = &clustrmap_sngl;

  mpcGeaClusterContentV1 geaclus;
  for(map2d::iterator iter1 = final_clustrmap->begin(); iter1 != final_clustrmap->end(); ++iter1){
    int iclus = iter1->first;
    float edep = clusemap[iclus];
    if(edep <=0){
      if(verbosity > 1) cout << "problem with edeposit <=0..continuing\n"; continue;
    }
    map1d* final_trmap;
    final_trmap = &clustrmap_dbl[iclus];
    if(fAlgorithm == 0 || fAlgorithm == 2) final_trmap = &clustrmap_sngl[iclus];
    //    final_trmap = 
    for( map1d::iterator iter2 = final_trmap->begin(); iter2 != final_trmap->end(); ++iter2){
      int itr = iter2->first; float itr_e = iter2->second;
      
     

      int igea = itr_map[itr];
      mpcGeaTowerContent* gea = mpcgeatow->getTower(igea);
      mpcClusterContent* clus = mpcclus->getCluster(iclus);
      
      //      int itorigin = gea->get_itorigin();
      //int itincoming = gea->get_itincoming();
      // int idorigin = gea->get_idorigin();
      // int idincoming = gea->get_idincoming();
      
      float etottr = trackmap[itr]*1.05;
      float efrac_tr = 0;
      if(etottr <=0) efrac_tr = 0;
      else efrac_tr = itr_e/etottr;

      //      cout << "etr,etottr, efrac_tr: " << itr_e << ", " << etottr << ", " << efrac_tr << endl;
      

      float efrac = itr_e/edep;
      

      geaclus.set_itorigin(gea->get_itorigin());
      geaclus.set_idorigin(gea->get_idorigin());

      geaclus.set_itincoming(gea->get_itincoming());
      geaclus.set_idincoming(gea->get_idincoming());
      
      geaclus.set_ch(clus->towerid(0));
      geaclus.set_edep(itr_e);
      geaclus.set_fraction(efrac);
      geaclus.set_fraction_tr(efrac_tr);
      
      //      if(itr_e > 0.05 && efrac > 0.01) 
      mpcgeaclus->addCluster(geaclus);
      if(verbosity > 2) geaclus.print(std::cout);
    }
    if(verbosity > 2) cout << endl;

    
  }
  
  //is that really it?
  return 0;
}

int MpcGeaClusterReco::process_event_mc(PHCompositeNode* topNode, int alg){
  int ngeatow = mpcgeatow->size();
  
  int nclus = mpcclus->size();
  for (int iclus=0; iclus<nclus; iclus++)
    {
      mpcClusterContent *clus = mpcclus->getCluster( iclus );
      //float ecore = clus->ecore();
      int ixpos = clus->ixpos();
      int iypos = clus->iypos();
      int iarm = clus->arm();
      if ( ixpos<0 ) continue;
      int feech = mpcmap->getFeeCh( ixpos, iypos, iarm );
      mpcGeaTowerContent *gea_primary = mpcgeatow->findPrimary( feech );
      //int itincoming = gea_primary->get_itincoming();
      int itorigin = gea_primary->get_itorigin();

      int ntowers = clus->multiplicity();
      //cout << "iclus " << iclus << "\t" << ntowers << "\t" << ecore << endl;
      float esum = 0.;
      for (int it=0; it<ntowers; it++)
        {
          //float partesum = clus->partesum(it);
          int twrid = clus->towerid(it);	// same as feech

          //int itower = mpctow->findTower( twrid );
          //mpcTowerContent *tower = mpctow->getTower( itower );
          //float tower_energy = tower->get_energy();
          //cout << "  " << it << "\t" << twrid << "\t" << partesum << "\t" << tower_energy << endl;

          // Now look for the mpcGeaTower for this tower
          for (int igeatow=0; igeatow<ngeatow; igeatow++)
            {
              mpcGeaTowerContent *geatow = mpcgeatow->getTower( igeatow );
              int this_feech = geatow->get_ch();
              //int this_itincoming = geatow->get_itincoming();
              int this_itorigin = geatow->get_itorigin();
              //if ( this_itincoming == itincoming )
              if ( this_feech == twrid && this_itorigin == itorigin )
                {
                  esum += geatow->get_edep();
/*
                  cout << "zzzz    " << igeatow << " " << geatow->get_edep() << " "
                       << this_itincoming << " " << itincoming << " "
                       << this_itorigin << " " << itorigin << endl;
*/
                }
            }
//          cout << " esum  " << esum << endl;
        }
//      cout << " total esum  " << esum << endl;
    }

  return 0;
}

void MpcGeaClusterReco::Print(const std::string &) const {
  cout << "MpcGeaClusterReco::Print() for module:" << Name() << endl;
  if(!mpcgeaclus) cout << "mpcgeaclus is null...returning\n";
  else{
    for(int unsigned i=0;i<mpcgeaclus->size();i++){
      mpcGeaClusterContent* geaclus = mpcgeaclus->getCluster(i);
      if(geaclus->get_edep() > 0.2){
	geaclus->print();
      }
    }
  }
}


float MpcGeaClusterReco::GetGeaEnergy(int track){
   int nmpcgea = mpcgeatow->size();
  
  float etot = 0;
  for(int i=0;i< nmpcgea;i++)
    {
      mpcGeaTowerContent* gea = mpcgeatow->getTower(i);
      if(gea->get_itorigin() == track) etot+=gea->get_edep();
    }
  if(verbosity > 2) cout << "Sim e_dep in calorimeter for Track: " << track << ", Energy: " << etot << endl;
  
  return etot;
}


//copied over from MpcSectorRecV2.C

void MpcGeaClusterReco::SetProfileParametersV2_2(float Energy)
{
  //shower shape parameters
  for(int ipar=0;ipar<6;ipar++){
    fShow2[ipar] = EvalShowPar2(ipar,Energy); //used for energy sharing in clusters
  }
  return;
}


float MpcGeaClusterReco::EvalShowPar2(int ipar, float e){
  float par = 0;
  if(e > 60) e = 60;
  par = fShowPar2[ipar][0]+fShowPar2[ipar][1]*e+fShowPar2[ipar][2]*e*e+fShowPar2[ipar][3]*e*e*e;
  return par;
}

float MpcGeaClusterReco::PredictEnergyV2_2(float x, float y, float Energy)
{
  float ret = 0;
  float r = sqrt(x*x+y*y);
  ret = fShow2[0]*exp(-fShow2[1]*r)+fShow2[2]*exp(-fShow2[3]*r*r*r)+fShow2[4]*exp(-fShow2[5]*r*r*r*r*r);
  
  return ret;
}

float MpcGeaClusterReco::PredictEnergyV2_2(float r,float Energy)
{
  float ret = 0;
  ret = fShow2[0]*exp(-fShow2[1]*r)+fShow2[2]*exp(-fShow2[3]*r*r*r)+fShow2[4]*exp(-fShow2[5]*r*r*r*r*r);
  
  return ret;
}
 
 
 bool MpcGeaClusterReco::GetEcoreStatus(mpcClusterContent* clus, int feech)
 {

   //use logxcg() b/c x() has angle dependent correction
   float logxcg = clus->logxcg();
   float logycg = clus->logycg();
   float tow_x  = mpcmap->getX(feech);
   float tow_y  = mpcmap->getY(feech);

   //have to convert sizes to module units
   float dr = sqrt( pow( (logxcg-tow_x) ,2) + pow( (logycg-tow_y) ,2) )/2.26;
   

   if(verbosity > 3){
     int ix = mpcmap->getGridX(feech);
     int iy = mpcmap->getGridY(feech);

     int icx = mpcmap->getGridX(clus->towerid(0));
     int icy = mpcmap->getGridY(clus->towerid(0));

     cout << "dr, icx, icy: ix,iy" << dr << ", " << icx << ", " << icy << " : " << ix << ", " << iy << endl;
   }

   //   if(dr > 1.4) return 0;
   // else return 1;

     
   //use e() for  predict energy func b/c this is what is used in code
   //if efrac < 0.02, we ignore it
   //assume ProfileParameters has already been called for this cluster
   float efrac = PredictEnergyV2_2(dr,clus->e());
   if(efrac < 0.02) return 0;
   else return 1;
 }


//adds mpcGeaClusterContainer to node tree
int MpcGeaClusterReco::CreateNodeTree(PHCompositeNode *topNode)
{
  PHCompositeNode *dstNode;
  PHNodeIterator iter(topNode);
  dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode)
    {
      cout << PHWHERE << "DST Node missing doing nothing" << endl;
      return -1;
    }

  mpcGeaClusterContainer *mpcgeacluster = new mpcGeaClusterContainerV1();
  TString nodename = "mpcGeaClusterContainer";
  if(whichnode > 0) nodename+=whichnode;
  PHObjectNode_t *MpcGeaClusterNode = new PHObjectNode_t(mpcgeacluster, nodename.Data(), "PHObject");
  dstNode->addNode(MpcGeaClusterNode);
  return 0;
}


