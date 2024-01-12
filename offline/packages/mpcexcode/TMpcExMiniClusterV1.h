#ifndef __TMPCEXMINICLUSTERV1__
#define __TMPCEXMINICLUSTERV1__

#include "mMpcExMiniClusters.h"
#include "math.h"


class TMpcExMiniClusterV1:TMpcExMiniCluster
{
  public:
     TMpcExMiniClusterV1();
     virtual ~TMpcExMiniClusterV1();

     int GetArm() {return _arm;}

     double GetRMS(){
       double rms = sqrt(_rms_x*_rms_x+_rms_y*_rms_y+1.0e-10);
       if(rms<=1.0e-4) rms=0;
       return rms;
     }
     double GetRMSX() {return _rms_x;}
     double GetRMSY() {return _rms_y;}
     double GetRadius() {return _radius;}
     double GetPkE();
     double GetExE();
     double GetShowerE() {return _shower_e;}
     double GetVertex() {return _vertex;}

     double GetShowerX() {return _shower_x;}
     double GetShowerY() {return _shower_y;}
   
     double GetTowerE() {return _tower_e;}
     int GetTowerCh() {return _tower_ch;}

     double GetLayerPkE(unsigned int i) {if(i<8) return _layer_pk_e[i];return -9999;}
     double GetLayerE(unsigned int i) {if(i<8) return _layer_ex_e[i];return -9999;}
     unsigned int GetNSq(){return _sq_list.size();}

     Square* GetSq(unsigned int i) {if(i<_sq_list.size()) return _sq_list[i];return NULL;} 
   
     //Get E weighted X
     double GetX() {return _x;}
     //Get E weighted Y
     double GetY() {return _y;}

     double GetPkX() {return _pk_x;}
     double GetPkY() {return _pk_y;}

     int GetPkGridX() {return _pk_ix;}
     int GetPkGridY() {return _pk_iy;}

     bool IsSeedPk() {return _is_seed_pk;}


    int GetNFired();
    bool IsContinues();
    int GetFirstLayer();
    //last fired layer,some minicluster may not penetrate all layers
    int GetLastLayer();
    double GetMaxLayerE();
    double GetMaxLayerPkE();

    void SetArm(int val){_arm= val;}
    void SetRMSX(double value) {_rms_x = value;}
    void SetRMSY(double value) {_rms_y = value;}
    void SetRadius(double val) {_radius = val;}
    void SetLayerPkE(unsigned int i,double val) {if(i<8) _layer_pk_e[i] = val;}
    void SetLayerE(unsigned int i,double val) {if(i<8) _layer_ex_e[i] = val;}

    void SetX(double val) {_x = val;}
    void SetY(double val) {_y = val;}
    void SetPkX(double val) {_pk_x = val;}
    void SetPkY(double val) {_pk_y = val;}

    void SetPkGridX(int val) {_pk_ix = val;}
    void SetPkGridY(int val) {_pk_iy = val;}
    void SetShowerE(double val) {_shower_e = val;}
    void SetVertex(double val) {_vertex = val;}
    void SetShowerX(double val) {_shower_x = val;}
    void SetShowerY(double val) {_shower_y = val;}

    void SetTowerE(double val) {_tower_e = val;}
    void SetTowerCh(int ch) {_tower_ch = ch;}

    void SetIsSeedPk(bool val) {_is_seed_pk = val;}

    void InsertSq(Square* sq ){_sq_list.push_back(sq);}

    virtual void Reset();
    void Print();

    virtual TMpcExMiniCluster* Clone();


  private:
    int _arm;
    double _rms_x;
    double _rms_y;
    double _pk_x;//pk may not exist
    double _pk_y;//
    int _pk_ix; //grid pk ix
    int _pk_iy; //grid pk iy

    int _tower_ch;
    double _tower_e;
    double _x;//mean value
    double _y;
    double _radius;
    double _layer_pk_e[8];
    double _layer_ex_e[8];
    bool _is_seed_pk;

    //shower E
    double _shower_e;
    double _vertex;
    double _shower_x;
    double _shower_y;
    std::vector<Square*> _sq_list;

};

#endif
