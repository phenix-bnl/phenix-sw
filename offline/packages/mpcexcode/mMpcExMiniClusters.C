#include "mMpcExMiniClusters.h"
#include "TMpcExMiniClusterV1.h"
#include <TMpcExShower.h>
#include <TMpcExHitContainer.h>
#include <TFile.h>
#include <TH1D.h>
#include <TH2D.h>
#include <TSystem.h>
#include <TCanvas.h>
#include <Exogram.h>
#include <MpcExMapper.h>
#include <algorithm>
#include <TEllipse.h>
#include <TLine.h>
#include "MpcExGeomToKey.h"
#include "MpcExConstants.h"
#include <MpcMap.h>

using namespace std;

bool cmp_mini_cluster(TMpcExMiniCluster* clus0,TMpcExMiniCluster* clus1){
  if(!clus0 || !clus1) return false;

  return clus0->GetPkE() > clus1->GetPkE();
}

bool cmp_mini_cluster2(TMpcExMiniCluster* clus0,TMpcExMiniCluster* clus1){
  if(!clus0 || !clus1) return false;

  return clus0->GetExE() > clus1->GetExE();
}

bool MiniClusterFilter::operator()(TMpcExMiniCluster* mclus){
  if(!mclus->IsSeedPk()) return false;
  int last_layer = mclus->GetLastLayer();
  int nfired = mclus->GetNFired();
  if(mclus->GetRMS()<1.0e-4) return false;
  if(mclus->GetPkE()/nfired<150) return false;
  if(mclus->GetNSq()<2) return false;
  if(last_layer!=7) return false;
  return true;
}

//mclus0 is the main cluster
bool MiniClusterFilter::operator()(TMpcExMiniCluster* mclus0,TMpcExMiniCluster* mclus1,float mindist){
  
  int nfired0 = mclus0->GetNFired();
  int nfired1 = mclus1->GetNFired();
  if(nfired1<3) return false;
    
  //ex_e_ratio_cut ratio cut
  //can add in the future
  double ex_ratio = (mclus0->GetExE()/nfired0)/(mclus1->GetExE()/nfired1);
  if(ex_ratio>1) ex_ratio = 1./ex_ratio;
//    if(pk_ratio>0.3) continue;
  
  //check if there is a tower behind can added in the futre
  //if(mclus0->GetTowerE()<=0 && mclus1->GetTowerE()<=0) continue;

  double dx = mclus1->GetX()-mclus0->GetX();
  double dy = mclus1->GetY()-mclus0->GetY();
  double dr = sqrt(pow(dx,2)+pow(dy,2));
    
  //two cluster is two close
  if(dr<fabs(mclus0->GetRadius()-mclus1->GetRadius())) return false;    
  if(dr<mindist) return false; 
   
  dx = mclus1->GetPkX()-mclus0->GetX();
  dy = mclus1->GetPkY()-mclus0->GetY();

  dr = sqrt(pow(dx,2)+pow(dy,2));
      
  //make sure they are not in the same minipads
  //of x type and y type

  int key0 = _g2k->get_key(mclus0->GetArm(),
                              0,
			      mclus0->GetPkX(),
			      mclus0->GetPkY()
			      );
      
  int key1 = _g2k->get_key(mclus1->GetArm(),
                              0,
			      mclus1->GetPkX(),
			      mclus1->GetPkY()
			      );
  if(key0==key1 && fabs(dx)<1*mclus0->GetRMSX()) return false;

    
  key0 = _g2k->get_key(mclus0->GetArm(),
                          1,
			  mclus0->GetPkX(),
			  mclus0->GetPkY()
			  );
      
  key1 = _g2k->get_key(mclus1->GetArm(),
                          1,
			  mclus1->GetPkX(),
			  mclus1->GetPkY()
			  );
   
  if(key0==key1 && fabs(dy)<1*mclus0->GetRMSY()) return false;
   
  return true;
}

mMpcExMiniClusters::mMpcExMiniClusters(){
  _arm = 0;
  _debug = false;
  _th_rms = 2.2;
  _th_rms_asy = 0.5;
  _th_pk_mean_dist = 1;
  _max_iterate = 100;
  _sq_cuts = false;
  _filter = new MiniClusterFilter();
  _mini_cluster_list.clear();
  _delete_sq_list.clear();
}


mMpcExMiniClusters::mMpcExMiniClusters(TMpcExShower* shower,TMpcExHitContainer* hits){ 
  _mini_cluster_list.clear();
  _delete_sq_list.clear();
  _arm=shower->get_arm();
  _debug = false;
  _th_rms = 2.2;
  _th_rms_asy = 0.5;
  _th_pk_mean_dist = 1;
  _max_iterate = 100;
  _sq_cuts = false;
  _filter = new MiniClusterFilter();
  ConstructMiniClusters(shower,hits);
}


mMpcExMiniClusters::~mMpcExMiniClusters(){
  Reset();
  if(_filter) delete _filter;
}

void mMpcExMiniClusters::Reset(){
  for(unsigned int i=0;i<_mini_cluster_list.size();i++){
    if(_mini_cluster_list[i]) delete _mini_cluster_list[i];
    _mini_cluster_list[i] = NULL;
  }
  _mini_cluster_list.clear();

  for(unsigned int i=0;i<_delete_sq_list.size();i++){
    if(_delete_sq_list[i]) delete _delete_sq_list[i];
    _delete_sq_list[i] = NULL;
  }
  _delete_sq_list.clear();
  _arm = 0;
}

unsigned int mMpcExMiniClusters::GetNMiniCluster(){
  return _mini_cluster_list.size();
}

TMpcExMiniCluster* mMpcExMiniClusters::GetMiniCluster(unsigned int i){
  if(i>=_mini_cluster_list.size()) return NULL;
  return _mini_cluster_list[i];
}

//make compare function for square

bool square_cmp(Square* sq0,Square* sq1){
  double e0 = sq0->GetE();
  double e1 = sq1->GetE();
  return e0 > e1; 
}


void mMpcExMiniClusters::ConstructMiniClusters(TMpcExShower* ex_shower,TMpcExHitContainer* hits){
  if(!ex_shower) return;
  Reset();
  _arm = ex_shower->get_arm();

  //create square
  //we will divide 40cmx40cm into 214x214 square
  const int nsquare = 214;
  double dspace = 40./nsquare;
  double space_min = -20;

  const double ex_front_z[2] = {-203.66,203.66};
  const double ex_layer_z[2][8] ={
                                 {-203.982,-204.636,-205.29,-205.944,-206.598,-207.252,-207.906,-208.56},
                                 {203.982,204.636,205.29,205.944,206.598,207.252,207.906,208.56}
                                 };
//  double space_max =  20;

  //make a square array
  //this is map, we can use the index
  //to fast access the Square;
  Square* sq_array[nsquare][nsquare];
  memset(sq_array,0,nsquare*nsquare*sizeof(Square*));
  //Square list for delete
  std::vector<Square*> sq_list;

  MpcExGeomToKey* g2k = MpcExGeomToKey::instance();
  //MpcMap
  MpcMap* mpc_map = MpcMap::instance();
 
  // //make hits map for hit container for look up
  // //for whole hits
  // map<int,TMpcExHit*> hits_map;
  // for(unsigned int ihit=0;ihit<hits->size();ihit++){
  //   TMpcExHit* hit = hits->getHit(ihit);
  //   int key = hit->key();
  //   hits_map[key] = hit;
  // }

  //for fast look up for shower only
  TMpcExHit* hit_array[49152];
  memset(hit_array,0,49152*sizeof(TMpcExHit*));
  //make map for look up

  for(unsigned int ihit=0;ihit<ex_shower->sizeHits();ihit++){
    // int key = ex_shower->getHit(ihit);
    // if(hits_map.find(key)==hits_map.end()){
    //   cout<<WHERE<<" bad news hits not exist !"<<endl;
    //   exit(0);
    // }
    TMpcExHit* hit = ex_shower->getHit(ihit,hits);\
    int key = hit->key(); 
    hit_array[key] = hit;
  }

  //fill each square
  for(unsigned int i=0;i<ex_shower->sizeHits();i++){
    //int key = ex_shower->getHit(i);
    //TMpcExHit* hit = hit_array[key];
    TMpcExHit* hit = ex_shower->getHit(i,hits); 
    int layer = hit->layer();
    int arm = hit->arm();
    double x = hit->x();
    double y = hit->y();
    double q = hit->combined();

    //correction for hsx and hsy
    double z = hit->z();
    double vertex = ex_shower->get_vertex();

    //search overlap region
    //the ratio of the lenght /width is 8
    //we search following layer first and then
    //previous layer
    for(int s=0;s<2;s++){
      int t_layer = layer-2*s+1;
      if(t_layer>7 || t_layer<0) continue;
      //record overlap position
      std::vector<double> pos_list;
      std::vector<double> q_list;
      //make sure each overlap region is search for only once
      std::set<int> used_set;
      double q_sum=0;
     
      double cor_factor=(ex_layer_z[arm][t_layer]-vertex)/(z-vertex);
      
      for(int t=0;t<8;t++){
        double t_x = x; 
	double t_y = y+0.1876*t-0.65625;
	if(layer%2==1){
	  t_x = x+0.1876*t-0.65625;
	  t_y = y;
	}

        //search the overlap region
	int t_key = g2k->get_key(arm,t_layer,t_x*cor_factor,t_y*cor_factor);
	if(t_key<0) continue;
	if(!hit_array[t_key]) continue;
	
	//this is important, during the search for overlap region
	//some key may appears again
	if(used_set.find(t_key)!=used_set.end()) continue;
	used_set.insert(t_key);
        TMpcExHit* t_hit = hit_array[t_key];
	double t_q = t_hit->combined();
        t_x = t_hit->x();
        t_y = t_hit->y();
	double pos = t_y;
	if(layer%2==1) pos = t_x;
	else pos = t_y;
	pos = pos/cor_factor;
	pos_list.push_back(pos);
	q_list.push_back(t_q);
	q_sum += t_q;
      }
      
      //isolated minipads
      if(q_sum<=0) continue;

      //not isolated minipads
      for(unsigned int k=0;k<pos_list.size();k++){
        
	double t_sq_x = x;
	double t_sq_y = pos_list[k];

	if(layer%2==1){
	  t_sq_x = pos_list[k];
	  t_sq_y = y;
	}
        
        //correction for first layer
	t_sq_x = t_sq_x*(ex_front_z[arm]-vertex)/(z-vertex);
	t_sq_y = t_sq_y*(ex_front_z[arm]-vertex)/(z-vertex);

	int gix = (t_sq_x-space_min)/dspace;
	int giy = (t_sq_y-space_min)/dspace;

	if(sq_array[gix][giy]){
	  Square* sq = sq_array[gix][giy];
	  //change t_layer to layer, we are interested in current layer
	  //not the adjacent layer
	  double cur_layer_e = sq->GetLayerE(layer);
	  //weighted the E by the adjacent minipads
          sq->SetLayerE(layer,cur_layer_e+q*q_list[k]/q_sum);
	}
	else{
	  Square* sq = new Square();
	  sq->SetX(t_sq_x);
	  sq->SetY(t_sq_y);
	  sq->SetGridX(gix);
	  sq->SetGridY(giy);
	  sq->SetLayerE(layer,q*q_list[k]/q_sum);
	  sq_array[gix][giy] = sq;
	  sq_list.push_back(sq);
	}
      }
      break;//find adjacent layers
    }
  }
  
  if(_debug) print_square_2d(sq_list,ex_shower,hits);
  
    //make miniclusters
  //start from largest energy
  //if the distance is less then 1 rms 
  //then include the square

  //search all peaks
  //if one square is larger than its surrondings
  //these peak will used as peak
  
  std::sort(sq_list.begin(),sq_list.end(),square_cmp);

  //check if we get the right value
  if(_debug){
    for(unsigned int i=0;i<sq_list.size();i++){
      cout<<"ordered square:  "<<i<<"  "
          <<"E "<<sq_list[i]->GetE()<<endl;
    }
  }

  //loop pk first , then non pk list
  //0 is pk and 1 is non peak
  std::vector<Square*> sq_pk_non_pk_list[2];
  for(unsigned int i=0;i<sq_list.size();i++){
    int sq_ix = sq_list[i]->GetGridX();
    int sq_iy = sq_list[i]->GetGridY();
    double sq_e = sq_list[i]->GetE();
    
    //ignore this square
    if(_sq_cuts && !IsGoodSq(sq_list[i])){
      _delete_sq_list.push_back(sq_list[i]);
      sq_array[sq_ix][sq_iy] = 0;
      continue;
    }

    bool is_peak=true;
    for(int idx=-1;idx<=1;idx++){
      for(int idy=-1;idy<=1;idy++){
        
	if(idx==0 && idy==0) continue;
         //rule out index which are out of range
        int tmp_sq_ix = sq_ix+idx;
	int tmp_sq_iy = sq_iy+idy;
	if(tmp_sq_ix<0 || tmp_sq_ix>=nsquare) continue;
	if(tmp_sq_iy<0 || tmp_sq_iy>=nsquare) continue;

	//not exists
	if(!sq_array[tmp_sq_ix][tmp_sq_iy]) continue;
	double tmp_sq_e = sq_array[tmp_sq_ix][tmp_sq_iy]->GetE();
	//not peak
	if(tmp_sq_e>sq_e){ 
          is_peak = false;
	  break;
	}
      }//idy
      if(!is_peak) break;
    }//idx
    
    if(is_peak) sq_pk_non_pk_list[0].push_back(sq_list[i]);
    else sq_pk_non_pk_list[1].push_back(sq_list[i]);
  }//i

  if(_debug){
    for(unsigned i_d=0;i_d<sq_pk_non_pk_list[0].size();i_d++){
      cout<<"pk list: "<<i_d<<" E "<<sq_pk_non_pk_list[0][i_d]->GetE()<<endl;
    }
  }

  //use these peak as seed to clusters
  //then non the left none peak values;
  bool used_square[nsquare][nsquare];
  //1 is used and 0 is not used
  memset(used_square,0,nsquare*nsquare*sizeof(bool));
  
  //first peak, then second none peak
  for(int i_is_pk=0;i_is_pk<2;i_is_pk++){
    for(unsigned int i=0;i<sq_pk_non_pk_list[i_is_pk].size();i++){
      int sq_ix = sq_pk_non_pk_list[i_is_pk][i]->GetGridX();
      int sq_iy = sq_pk_non_pk_list[i_is_pk][i]->GetGridY();
      double sq_x = sq_pk_non_pk_list[i_is_pk][i]->GetX();
      double sq_y = sq_pk_non_pk_list[i_is_pk][i]->GetY();
      
      if(_debug){
        cout<<"pk non pk set: "<<i_is_pk<<endl;
	cout<<"sq i "<<i<<" E "<<sq_pk_non_pk_list[i_is_pk][i]->GetE()<<endl;
      }

      if(used_square[sq_ix][sq_iy]){
        if(_debug) cout<<"sq E "<<sq_pk_non_pk_list[i_is_pk][i]->GetE()<<" used "<<endl;
	continue;
      }
      
      //use this as seed, first,3x3 array,
      double sum_x = 0;
      double sum_x2 = 0;
      double sum_y = 0;
      double sum_y2 = 0;
      double norm = 0;
      
      //define the outline of the mxn box
      int left = 0;
      int right = 0;
      int up = 0;
      int down = 0;

      if(_debug){
        cout<<"create mini cluster "<<endl;
        sq_pk_non_pk_list[i_is_pk][i]->Print();
      } 
            
      TMpcExMiniClusterV1* mini_cluster = new TMpcExMiniClusterV1();
      mini_cluster->SetArm(ex_shower->get_arm());
      mini_cluster->SetShowerE(ex_shower->get_roughTotE());
      mini_cluster->SetVertex(ex_shower->get_vertex());
      double zdist = ex_layer_z[ex_shower->get_arm()][0] - ex_shower->get_vertex();

      mini_cluster->SetShowerX(ex_shower->get_hsx()*zdist);
      mini_cluster->SetShowerY(ex_shower->get_hsy()*zdist);
      
      _mini_cluster_list.push_back((TMpcExMiniCluster*)mini_cluster);
      
      if(i_is_pk==0) mini_cluster->SetIsSeedPk(true);
      else mini_cluster->SetIsSeedPk(false);

      mini_cluster->SetPkX(sq_pk_non_pk_list[i_is_pk][i]->GetX());
      mini_cluster->SetPkY(sq_pk_non_pk_list[i_is_pk][i]->GetY());
     
      mini_cluster->SetPkGridX(sq_pk_non_pk_list[i_is_pk][i]->GetGridX());
      mini_cluster->SetPkGridY(sq_pk_non_pk_list[i_is_pk][i]->GetGridY());

      //set layer E
      for(int ilayer=0;ilayer<8;ilayer++){ 
        mini_cluster->SetLayerPkE(ilayer,sq_pk_non_pk_list[i_is_pk][i]->GetLayerE(ilayer));
      }

      int niter = 0;
      while(niter<_max_iterate){ 
        //try nxm array square when enlarge 
	//each time we only iter out range
	//for example
	//1 2 3
	//4 5 6
	//7 8 9
	//we iter all except 5
	//the algo is performed as following
	//when iud (iter y) is located on bottom or top
	//we set the iter step to 1
	//if iud is betwen center (like 4 5 6)
	//set the step to 2, then we will skip 5
        
	//make sure we update for each iteration
        bool is_update = false;

	for(int iud=sq_iy-down;iud<=sq_iy+up;iud++){
	  if(iud>=nsquare || iud<0) continue;
	  //iterate step
	  int it_step=1;
	  if(iud>sq_iy-down && iud<sq_iy+up) it_step = left+right;
	  //for begining when left, right are the same
	  //set it_step to 1 to avoid infinit loop
	  if(it_step==0) it_step=1;
	  for(int ilr=sq_ix-left;ilr<=sq_ix+right;ilr+=it_step){
	    if(ilr>=nsquare || ilr<0) continue;
            if(used_square[ilr][iud]) continue;
	    //if the square exist or not
            if(sq_array[ilr][iud]){
              used_square[ilr][iud]=true;
              double tmp_x = sq_array[ilr][iud]->GetX();
              double tmp_y = sq_array[ilr][iud]->GetY();
              double tmp_e = sq_array[ilr][iud]->GetE();
              if(_debug) cout<<"square E: "<<tmp_e<<endl;
              sum_x += tmp_e*tmp_x;
              sum_x2 += tmp_e*tmp_x*tmp_x;
              sum_y += tmp_e*tmp_y;
              sum_y2 += tmp_e*tmp_y*tmp_y;
              norm += tmp_e;
              mini_cluster->InsertSq(sq_array[ilr][iud]);
              for(int ilayer=0;ilayer<8;ilayer++){ 
                double add_e = sq_array[ilr][iud]->GetLayerE(ilayer);
                double cur_e = mini_cluster->GetLayerE(ilayer);
                mini_cluster->SetLayerE(ilayer,add_e+cur_e);
              }//ilayer
              is_update = true;
            }//if
	  }//ilr
	}//iud

        //isolated
	if(!is_update){
	  if(_debug) cout<<"no new update !"<<endl;
	  break;
	}
        
	if(norm==0){
	  cout<<WHERE<<" norm is zero should not happen"<<endl;
	  return;
	}

	double tmp_mean_x = sum_x/norm;
        double tmp_mean_y = sum_y/norm;
        double tmp_mean_x2 = sum_x2/norm;
        double tmp_mean_y2 = sum_y2/norm;

        double tmp_rms_x = sqrt(tmp_mean_x2-tmp_mean_x*tmp_mean_x+1.0e-10);
	if(tmp_rms_x<=1.0e-4) tmp_rms_x=0.2/sqrt(12.);
        if(_debug) cout<<"tmp_rms_x "<<tmp_rms_x<<endl;

        double tmp_rms_y = sqrt(tmp_mean_y2-tmp_mean_y*tmp_mean_y+1.0e-10);
	if(tmp_rms_y<=1.0e-4) tmp_rms_y=0.2/sqrt(12.);
	if(_debug) cout<<"tmp_rms_y "<<tmp_rms_y<<endl;

        double tmp_rms_r = sqrt(tmp_rms_x*tmp_rms_x+tmp_rms_y*tmp_rms_y+1.0e-10);
        
	if(niter==0){
	//first iteration
	//enlarge all
	  left+=1;
	  right+=1;
	  up+=1;
	  down+=1;
	}
	else{
	  //the distance to mean center
	  double dleft = (left+1)*dspace+(tmp_mean_x-sq_x);
	  if(dleft<_th_rms*(tmp_rms_r)) left++;
	  double dright = (right+1)*dspace+(sq_x-tmp_mean_x);
	  if(dright<_th_rms*(tmp_rms_r)) right++;
	  double dup = (up+1)*dspace+(sq_y-tmp_mean_y);
	  if(dup<_th_rms*(tmp_rms_r)) up++;
	  double ddown = (down+1)*dspace+(tmp_mean_y-sq_y);
	  if(ddown<_th_rms*(tmp_rms_r)) ddown++;
	}

	if(tmp_rms_x+tmp_rms_y>0.00001){
	  
	  double tmp_rms_asy = fabs(tmp_rms_x-tmp_rms_y)/(tmp_rms_x+tmp_rms_y);
	  if(tmp_rms_asy>0.5){
	    if(_debug) cout<<"large rms asymmetry !"<<endl;
	    break;
	  }

	  //pk distance to mean x and y
	  double pk_m_dx = fabs(sq_x - tmp_mean_x);
	  double pk_m_dy = fabs(sq_y - tmp_mean_y);
	  double pk_m_dr = sqrt(pow(pk_m_dx,2)+pow(pk_m_dy,2));
	  if(pk_m_dx>_th_pk_mean_dist*tmp_rms_x) break;
	  if(pk_m_dy>_th_pk_mean_dist*tmp_rms_y) break;
	  if(pk_m_dr>_th_pk_mean_dist*tmp_rms_r) break;

	}
        niter++;
      }//while
      
      if(_debug) cout<<"iterator number: "<<niter<<endl;
      
      double mean_x = sum_x/norm;
      double mean_y = sum_y/norm;
      double mean_x2 = sum_x2/norm;
      double mean_y2 = sum_y2/norm;

      double rms_x = sqrt(mean_x2-mean_x*mean_x+1.0e-10);
      if(rms_x<=1.0e-4) rms_x=0;
      double rms_y = sqrt(mean_y2-mean_y*mean_y+1.0e-10);
      if(rms_y<=1.0e-4) rms_y=0;

      mini_cluster->SetX(mean_x);
      mini_cluster->SetY(mean_y);
      mini_cluster->SetRMSX(rms_x);
      mini_cluster->SetRMSY(rms_y);
      mini_cluster->SetRadius(sqrt(pow((left+right)/2.,2)/2.+pow((up+down)/2.,2)/2.)*dspace+0.5*dspace);
      
      //get closet Mpc Tower
      //use the 5x5 array of shower
      int ct_ix = ex_shower->get_mpcPeakix();
      int ct_iy = ex_shower->get_mpcPeakiy();
      double best_dr = 9999;
      int best_ch = -9999;
      double best_tower_e = -9999;
      double mpc_z[2] = {-MpcExConstants::MPC_REFERENCE_Z,MpcExConstants::MPC_REFERENCE_Z};
      double vertex = ex_shower->get_vertex();
      int arm = ex_shower->get_arm();
      double tower_cor = (mpc_z[arm]-vertex)/(ex_front_z[arm]-vertex); 
      for(int itx=-2;itx<=2;itx++){
	for(int ity=-2;ity<=2;ity++){
          int tmp_ch = mpc_map->getFeeCh(ct_ix+itx,ct_iy+ity,arm);
	  if(tmp_ch<0) continue;
	  double tmp_dx = mean_x*tower_cor-mpc_map->getX(tmp_ch);
	  double tmp_dy = mean_y*tower_cor-mpc_map->getY(tmp_ch);
	  double tmp_dr = sqrt(tmp_dx*tmp_dx+tmp_dy*tmp_dy);
          
	  if(fabs(tmp_dx)<1.2 && fabs(tmp_dy)<1.2 && tmp_dr<best_dr){
	    best_dr = tmp_dr;
	    best_ch = tmp_ch;
	    best_tower_e = ex_shower->get_mpcTwrE(2+itx,2+ity,true);
	  }
	}
      }

      mini_cluster->SetTowerCh(best_ch);
      mini_cluster->SetTowerE(best_tower_e);

    }//i
  }


  //remove the pointer for the debug test;
//  for(unsigned int i=0;i<sq_list.size();i++){
//    if(sq_list[i]){ 
//      delete sq_list[i];
//      sq_list[i] = NULL;
//    }
//  }
//  sq_list.clear();


  return;
}

std::vector<TMpcExMiniCluster*> mMpcExMiniClusters::GoodMiniClusters(float mindist){
  std::vector<TMpcExMiniCluster*> pk_mclus_list;
  int Nclus = GetNMiniCluster();
  
  for(int i=0;i<Nclus;i++){
    TMpcExMiniCluster* mclus = GetMiniCluster(i);
     if(_debug){
      mclus->Print();
    }
   
    if(!(*_filter)(mclus)) continue;

    pk_mclus_list.push_back(mclus);
  }
  
  std::sort(pk_mclus_list.begin(),pk_mclus_list.end(),cmp_mini_cluster);
  std::vector<TMpcExMiniCluster*> good_mini_clus_list;
  if(pk_mclus_list.size()<2) return good_mini_clus_list;

  std::sort(pk_mclus_list.begin()+1,pk_mclus_list.end(),cmp_mini_cluster2);

  good_mini_clus_list.push_back(pk_mclus_list[0]);

  for(unsigned int i=1;i<pk_mclus_list.size();i++){
    TMpcExMiniCluster* mclus0 = pk_mclus_list[0];
    TMpcExMiniCluster* mclus1 = pk_mclus_list[i];
    
    if(!(*_filter)(mclus0,mclus1,mindist)) continue;

    good_mini_clus_list.push_back(mclus1);
  }
  return good_mini_clus_list;
}



void mMpcExMiniClusters::print_square_2d(vector<Square*> sq_list,TMpcExShower* ex_shower,TMpcExHitContainer*hits){
  static int ct = 0;
  if(!ex_shower || sq_list.size()==0) return;
  
  std::vector<TH1*> delete_list;   
 
  TH2D* hsquare_2d = new TH2D(Form("hsquare_2d_%d",ct),"Square 2D",214,-20,20,214,-20,20);
  hsquare_2d->GetXaxis()->SetTitle("X/cm");
  hsquare_2d->GetYaxis()->SetTitle("Y/cm");
  delete_list.push_back(hsquare_2d);

  Exogram* hexo = new Exogram(Form("hexo_%d",ct),"Exogram",600,-20,20,600,-20,20,8,-0.5,7.5,1);
  delete_list.push_back(hexo);

  for(unsigned int i=0;i<sq_list.size();i++){
    hsquare_2d->Fill(sq_list[i]->GetX(),
                    sq_list[i]->GetY(),
		    sq_list[i]->GetE());
  }
  
  int nhits = ex_shower->sizeHits(); 
  for(int i=0;i<nhits;i++){
    //int key = ex_shower->getHit(i);
    //TMpcExHit* hit = hits->get_hit_by_key(key);
    TMpcExHit* hit = ex_shower->getHit(i,hits);
    double q = hit->combined();
    hexo->FillEx(hit->key(),q);
  }

  TH2D* htemp_2d = (TH2D*)(hexo->Project3D("yx"));
  delete_list.push_back(htemp_2d);

  double mean_x = htemp_2d->GetMean(1);
  double mean_y = htemp_2d->GetMean(2);
  double rms_x = htemp_2d->GetRMS(1);
  double rms_y = htemp_2d->GetRMS(2);

  htemp_2d->SetAxisRange(mean_x-5*rms_x,mean_x+5*rms_x,"x");
  htemp_2d->SetAxisRange(mean_y-5*rms_y,mean_y+5*rms_y,"y");

  hsquare_2d->SetAxisRange(mean_x-5*rms_x,mean_x+5*rms_x,"x");
  hsquare_2d->SetAxisRange(mean_y-5*rms_y,mean_y+5*rms_y,"y");

  TCanvas* c = new TCanvas(Form("c_%d",ct),"c",1600,800);
  c->Divide(3,2);
  c->cd(1);
  htemp_2d->Draw("colz");
  c->cd(4);
  hsquare_2d->Draw("colz");
  c->cd(2);
  TH1D* htemp_1d = htemp_2d->ProjectionX();
  delete_list.push_back(htemp_1d);
  htemp_1d->Draw();
  c->cd(5);
  htemp_1d = hsquare_2d->ProjectionX();
  htemp_1d->Draw();
  c->cd(3);
  htemp_1d = htemp_2d->ProjectionY();
  delete_list.push_back(htemp_1d);
  htemp_1d->Draw();
  c->cd(6);
  htemp_1d = hsquare_2d->ProjectionY();
  delete_list.push_back(htemp_1d);
  htemp_1d->Draw();
  
  TFile* ofile = NULL;
  if(ct>0) ofile = new TFile("Debug_ExMiniClusters_shower_square_2d.root","UPDATE");
  else{
    gSystem->Exec("rm -f Debug_ExMiniClusters_shower_square_2d.root");
    ofile = new TFile("Debug_ExMiniClusters_shower_square_2d.root","RECREATE"); 
  }

  c->Write();
  ofile->Close();

  for(unsigned int i=0;i<delete_list.size();i++){
    if(delete_list[i]) delete delete_list[i];
    delete_list[i] = NULL;
  }

  delete_list.clear();
  ct++;
}

void mMpcExMiniClusters::VisualMiniClusters(TMpcExShower* ex_shower,const char* dataset,double pi0_mass){
  if(_mini_cluster_list.size()<=0) return;
  static int ct = 0;

  vector<TObject*> _delete_list;

  double reco_e = -9999;
  double mpc_e3x3 = -9999;
  double mpcex_e = -9999;
  if(ex_shower){
    reco_e= ex_shower->get_roughTotE();
    mpc_e3x3 = ex_shower->get_mpcE3x3();
    mpcex_e = ex_shower->get_esum();
  }

  TH2D* hsquare_2d = new TH2D(Form("hsquare_2d_%d",ct),
                             Form("Reco E %.3f MpcE3x3: %.3f MpcExE: %.3f Pi0 Mass:%.3f",
			         reco_e,mpc_e3x3,mpcex_e,pi0_mass),
			     214,-20,20,214,-20,20);
  hsquare_2d->GetXaxis()->SetTitle("X/cm");
  hsquare_2d->GetYaxis()->SetTitle("Y/cm");
  _delete_list.push_back(hsquare_2d);
 
  TH2D* hsquare_2d_w_e = new TH2D(Form("hsquare_2d_%d_w_e",ct),"Square 2D",214,-20,20,214,-20,20);
  hsquare_2d_w_e->GetXaxis()->SetTitle("X/cm");
  hsquare_2d_w_e->GetYaxis()->SetTitle("Y/cm");
  _delete_list.push_back(hsquare_2d_w_e);

  if(_debug) cout<<"Number of MiniCluster: "<<_mini_cluster_list.size()<<endl;
  for(unsigned int i=0;i<_mini_cluster_list.size();i++){
    int Nsq = _mini_cluster_list[i]->GetNSq();
    for(int j=0;j<Nsq;j++){
      Square* sq = _mini_cluster_list[i]->GetSq(j);
      hsquare_2d->Fill(sq->GetX(),
                       sq->GetY(),
		       sq->GetE());
      hsquare_2d_w_e->Fill(sq->GetX(),
                       sq->GetY(),
		       _mini_cluster_list[i]->GetExE());
    }
    hsquare_2d_w_e->Fill(_mini_cluster_list[i]->GetPkX(),
                         _mini_cluster_list[i]->GetPkY(),
		         _mini_cluster_list[i]->GetExE());

  }

  double mean_x = hsquare_2d->GetMean(1);
  double mean_y = hsquare_2d->GetMean(2);
  double rms_x = hsquare_2d->GetRMS(1);
  double rms_y = hsquare_2d->GetRMS(2);

  hsquare_2d->SetAxisRange(mean_x-6*rms_x,mean_x+6*rms_x,"x");
  hsquare_2d->SetAxisRange(mean_y-6*rms_y,mean_y+6*rms_y,"y");

  hsquare_2d_w_e->SetAxisRange(mean_x-6*rms_x,mean_x+6*rms_x,"x");
  hsquare_2d_w_e->SetAxisRange(mean_y-6*rms_y,mean_y+6*rms_y,"y");

  TCanvas* c = new TCanvas(Form("c_visual_mini_clusters_%d",ct),"c",1600,800);
  c->Divide(2,1);
  _delete_list.push_back(c);
  
  c->cd(1);
  hsquare_2d->Draw("colz");

  for(unsigned int i=0;i<_mini_cluster_list.size();i++){
    double pk_x = _mini_cluster_list[i]->GetX();
    double pk_y = _mini_cluster_list[i]->GetY();
    double r = _mini_cluster_list[i]->GetRadius();
    double r_rms = _mini_cluster_list[i]->GetRMS();
    
    TLine* t_l = new TLine(pk_x,std::max(-20.,pk_y-r/2.),pk_x,std::min(pk_y+r/2.,20.));
    _delete_list.push_back(t_l);
    t_l->SetLineWidth(2);
    t_l->Draw("same");

    t_l = new TLine(std::max(-20.,pk_x-r/2.),pk_y,std::min(20.,pk_x+r/2.),pk_y);
    _delete_list.push_back(t_l);
    t_l->SetLineWidth(2);
    t_l->Draw("same");

    TEllipse* elp = new TEllipse(pk_x,pk_y,r,r);
    elp->SetFillStyle(0);
    if(_mini_cluster_list[i]->IsSeedPk()){
      elp->SetLineColor(kRed);
    }

    _delete_list.push_back(elp);
    
    elp->Draw("same");
    
    TEllipse* elp_rms = new TEllipse(pk_x,pk_y,r_rms,r_rms);
    elp_rms->SetFillStyle(0);
    elp_rms->SetLineStyle(9);
    if(_mini_cluster_list[i]->IsSeedPk()){
      elp_rms->SetLineColor(kRed);
    }
    _delete_list.push_back(elp_rms);

    elp_rms->Draw("same");

  }

  c->cd(2);
  hsquare_2d_w_e->Draw("colz");
  
  TFile* ofile = NULL;
  string ofname = string("Visual_MiniClusters_")+string(dataset)+string(".root");
  if(ct>0) ofile = new TFile(ofname.c_str(),"UPDATE");
  else{
    gSystem->Exec(Form("rm -f %s",ofname.c_str()));
    ofile = new TFile(ofname.c_str(),"RECREATE"); 
  }

  c->Write();
  ofile->Close();

  for(unsigned i=0;i<_delete_list.size();i++){
    delete _delete_list[i];
    _delete_list[i] = NULL;
  }
  _delete_list.clear();

  ct++;
}

bool mMpcExMiniClusters::IsGoodSq(Square* sq){
  if(!sq) return false;
  //continues cut
  bool e_start = false;
  for(int i=0;i<8;i++){
    if(e_start && sq->GetLayerE(i)<=0.0000001){
      return false;
    } 
    if(sq->GetLayerE(i)>0) e_start = true;
  }
  return true;
}
