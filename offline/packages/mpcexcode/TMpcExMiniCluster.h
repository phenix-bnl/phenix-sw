#ifndef __TMPCEXMINICLUSTER__
#define __TMPCEXMINICLUSTER__

#include <iostream>
#include <stdio.h>
#include <string.h>

#define WHERE __LINE__

class Square{
  private:
    double _x;
    double _y;
    //grid index
    int _ix;
    int _iy;
    double _layer_e[8];
  public:
    
    Square(){_x=0;_y=0;memset(_layer_e,0,sizeof(_layer_e));_ix=0;_iy=0;}
    Square(double x,double y,double layer_e[8]){
      _x = x;
      _y = y;
      for(int i=0;i<8;i++){
        _layer_e[i] = layer_e[i]; 
      }
      _ix = 0;
      _iy = 0;
    }

    double GetX() {return _x;}
    double GetY() {return _y;}
    int GetGridX() {return _ix;}
    int GetGridY() {return _iy;}
    double GetE(){
      double sum=0;
      for(int i=0;i<8;i++){
        sum += _layer_e[i];
      }
      return sum;
    }

    double GetLayerE(unsigned int i) {if(i<8) return _layer_e[i];return 0;}

    void Print();

    void SetX(double val) {_x = val;}
    void SetY(double val) {_y = val;}
    void SetGridX(int val) {_ix = val;}
    void SetGridY(int val) {_iy = val;}
    void SetLayerE(unsigned int i,double val){if(i<8) _layer_e[i] = val;}

    Square* Clone();
};


class TMpcExMiniCluster
{
  public:
    
    
    TMpcExMiniCluster(){}
    virtual ~TMpcExMiniCluster(){}
    virtual int GetArm() {std::cout<<WHERE<<" GetArm  "<<std::endl;return -9999;};
    
    virtual double GetRMSX() {std::cout<<WHERE<<" GetRMSX "<<std::endl;return -9999;}
    virtual double GetRMSY() {std::cout<<WHERE<<" GetRMSY "<<std::endl;return -9999;}
    virtual double GetRMS() {std::cout<<WHERE<<" GetRMS "<<std::endl;return -9999;}
    virtual double GetRadius() {std::cout<<WHERE<<" GetRadius "<<std::endl;return -9999;}
    virtual double GetPkE() {std::cout<<WHERE<<" GetPKE "<<std::endl;return -9999;}
    virtual double GetExE() {std::cout<<WHERE<<" GetExE "<<std::endl;return -9999;}
    
    //get closest Mpc Tower Energy for this miniCluster
    virtual double GetTowerE() {std::cout<<WHERE<<" GetTowerE  "<<std::endl;return -9999;}
    
    //get closest Mpc Tower Channel
    virtual int GetTowerCh() {std::cout<<WHERE<<" GetMpcPkCh   "<<std::endl;return -9999;}
        
    //get the total shower energy of which the minicluster contains
    virtual double GetShowerE() {std::cout<<WHERE<<" GetShowerE "<<std::endl;return -9999;}
    //vertex
    virtual double GetVertex() {std::cout<<WHERE<<" GetVertex "<<std::endl;return -9999;}

    virtual double GetShowerX() {std::cout<<WHERE<<" GetShowerX "<<std::endl;return -9999;}

    virtual double GetShowerY() {std::cout<<WHERE<<" GetShowerY "<<std::endl;return -9999;}

    //get Layer Peak E
    virtual double GetLayerPkE(unsigned int layer) {std::cout<<WHERE<<" GetLayerPkE "<<std::endl;return -9999;}
    virtual double GetLayerE(unsigned int layer) {std::cout<<WHERE<<" GetLayerE "<<std::endl;return -9999;}
    //return position of the Peak X
    virtual double GetPkX() {std::cout<<WHERE<<" GetPkX "<<std::endl;return -9999;}
    virtual double GetPkY() {std::cout<<WHERE<<" GetPkY "<<std::endl;return -9999;}

    virtual int GetPkGridX() {std::cout<<WHERE<<" GetPkGridX "<<std::endl;return -9999;}
    virtual int GetPkGridY() {std::cout<<WHERE<<" GetPkGridY "<<std::endl;return -9999;}


    virtual double GetX() {std::cout<<WHERE<<" GetX "<<std::endl;return -9999;}
    virtual double GetY() {std::cout<<WHERE<<" GetY "<<std::endl;return -9999;}

    virtual bool IsSeedPk() {std::cout<<WHERE<<" IsSeedPk "<<std::endl;return false;}
    
    //number of fired layer for this mini cluster
    virtual int GetNFired() {std::cout<<WHERE<<" GetNfired "<<std::endl;return -9999;}
    virtual bool IsContinues() {std::cout<<WHERE<<" IsContinues "<<std::endl;return false;}
    virtual int GetFirstLayer() {std::cout<<WHERE<<" GetFirstLayer "<<std::endl;return -9999;}
    //last fired layer,some minicluster may not penetrate all layers
    virtual int GetLastLayer() {std::cout<<WHERE<<" GetLastLayer  "<<std::endl;return -9999;}
    virtual double GetMaxLayerE() {std::cout<<WHERE<<" GetMaxLayerE  "<<std::endl;return -9999;}
    virtual double GetMaxLayerPkE() {std::cout<<WHERE<<" GetMaxLayerPkE  "<<std::endl;return -9999;}

    
    //cluster is represent by a 2d array nxn square box
    virtual unsigned int GetNSq(){std::cout<<WHERE<<" GetNSq  "<<std::endl;return 0;}
    virtual Square* GetSq(unsigned int) {std::cout<<WHERE<<" GetSq   "<<std::endl;return NULL;}

    virtual void Reset(){std::cout<<WHERE<<" Reset    "<<std::endl;}

    virtual void Print() {std::cout<<WHERE<<" Print     "<<std::endl;}
    virtual TMpcExMiniCluster* Clone() {std::cout<<WHERE<<" Clone     "<<std::endl;return NULL;}

  private:
    /*this is a base class*/
    
};

#endif
