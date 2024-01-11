#include "TMpcExMiniClusterV1.h"
#include <iostream>

TMpcExMiniClusterV1::TMpcExMiniClusterV1(){
  Reset();
}

TMpcExMiniClusterV1::~TMpcExMiniClusterV1(){
  Reset();
}

void TMpcExMiniClusterV1::Reset(){
  _arm = -9999;
  _is_seed_pk = false;
  _rms_x = -9999;
  _rms_y = -9999;
  _pk_x = -9999;
  _pk_y = -9999;
  _x = -9999;
  _y = -9999;
  _pk_ix = -9999;
  _pk_iy = -9999;
  _radius = -9999;
  _tower_e = -9999;
  _tower_ch = -9999;

  memset(_layer_pk_e,0,sizeof(_layer_pk_e));
  memset(_layer_ex_e,0,sizeof(_layer_ex_e));

  for(unsigned int i=0;i<_sq_list.size();i++){
    if(_sq_list[i]){
      delete _sq_list[i];
      _sq_list[i] = NULL;
    }
  }
  _sq_list.clear();
}

double TMpcExMiniClusterV1::GetPkE(){
  double sum=0;
  for(int i=0;i<8;i++){
    sum+=_layer_pk_e[i];
  }
  return sum;

}

double TMpcExMiniClusterV1::GetExE(){
  double sum=0;
  for(int i=0;i<8;i++){
    sum+=_layer_ex_e[i];
  }
  return sum;
}

void TMpcExMiniClusterV1::Print(){
  std::cout<<"ExE: "<<GetExE()
           <<" PkE: "<<GetPkE()
           <<" x: "<<GetX()
	   <<" y: "<<GetY()
	   <<" RMS X: "<<GetRMSX()
	   <<" RMS Y: "<<GetRMSY()
	   <<" RMS R: "<<GetRMS()
	   <<" NSquare: "<<GetNSq()
	   <<std::endl;

}

int TMpcExMiniClusterV1::GetNFired(){
   int nfired=0;
   for(int i=0;i<8;i++){
    double layer_e = GetLayerE(i);
    if(layer_e>1.0e-6) nfired++;
  
   }
  return nfired; 
}

bool TMpcExMiniClusterV1::IsContinues(){
  int first = GetFirstLayer();
  for(int i=first;i<8;i++){
    double layer_e = GetLayerE(i);
    if(layer_e<=1.0e-6) return false;
  
  }
  return true;
}

int TMpcExMiniClusterV1::GetFirstLayer(){
  int layer=-1;
  for(int i=0;i<8;i++){
    double layer_e = GetLayerE(i);
    if(layer_e>1.0e-6){
      layer = i;
      return layer;
    }
  }
  return layer;
}

int TMpcExMiniClusterV1::GetLastLayer(){
  int layer=-1;
  for(int i=0;i<8;i++){
    double layer_e = GetLayerE(i);
    if(layer_e>1.0e-6){
      layer = i;
    }
  }
  return layer;
}

double TMpcExMiniClusterV1::GetMaxLayerE(){
  double max_layer_ex_e = -9999;
  for(int i=0;i<8;i++){
    double e = GetLayerE(i);
    if(e>max_layer_ex_e) max_layer_ex_e = e;
  }
  return max_layer_ex_e;
}

double TMpcExMiniClusterV1::GetMaxLayerPkE(){
  double max_layer_pk_e = -9999;
  for(int i=0;i<8;i++){
    double e = GetLayerPkE(i);
    if(e>max_layer_pk_e) max_layer_pk_e = e;
  }
  return max_layer_pk_e;
}


TMpcExMiniCluster* TMpcExMiniClusterV1::Clone(){
  TMpcExMiniClusterV1* mclus = new TMpcExMiniClusterV1();
  mclus->SetArm(_arm);
  mclus->SetRMSX(_rms_x);
  mclus->SetRMSY(_rms_y);
  mclus->SetPkX(_pk_x);
  mclus->SetPkY(_pk_y);
  mclus->SetPkGridX(_pk_ix);
  mclus->SetPkGridY(_pk_iy);
  mclus->SetX(_x);
  mclus->SetY(_y);
  mclus->SetRadius(_radius);
  mclus->SetIsSeedPk(_is_seed_pk);
  mclus->SetShowerE(_shower_e);
  mclus->SetVertex(_vertex);
  mclus->SetShowerX(_shower_x);
  mclus->SetShowerY(_shower_y);
  mclus->SetTowerE(_tower_e);
  mclus->SetTowerCh(_tower_ch);

  for(int i=0;i<8;i++){
    mclus->SetLayerE(i,_layer_ex_e[i]);
    mclus->SetLayerPkE(i,_layer_pk_e[i]);
  }
  

  for(unsigned int i=0;i<_sq_list.size();i++){
    mclus->InsertSq(_sq_list[i]->Clone());
  }
  
  return (TMpcExMiniCluster*)mclus;
}
