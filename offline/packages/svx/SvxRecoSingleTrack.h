#ifndef __SVXRECOSINGLETRACK__
#define __SVXRECOSINGLETRACK__

class SvxRecoSingleTrack
{
  public:
    SvxRecoSingleTrack()
    {
      isprimary=true;
      for(short l=0;l<4;l++)
      {
        for(short h=0;h<2;h++)
        {
          for(short c=0;c<3;c++)
          {
            position[l][h][c]=0.;
          }
        }
        nhits[l]=0;
      }
      quality=-9999.;
      momentum=-9999.;
    }
    ~SvxRecoSingleTrack(){}
    
    void setHitPosition(int layer, float x, float y, float z, short hit=0);
    float getHitPosition(int layer, int coor, short hit=0);  //coor 0,1,2 == x,y,z

    void setMomentum(float p);
    float getMomentum();

    void set3Momentum(int coor, float pp);
    float get3Momentum(int coor);

    void setPrimary(bool prim);
    bool getPrimary();
    
    void setNhits(int layer, short n);
    short getNhits(int layer);
    
    void setQuality(float q);
    float getQuality();
    
    void setClusterID(int layer, int hit, int index);
    int getClusterID(int layer, int hit);
    
    
    void setHelicity(bool hel);
    bool getHelicity();
    
    void setScatter(int layer, float sct);
    float getScatter(int layer);
    
  private:
    //position[layer][hit][xyz] -- could be 2 hits where layers overlap
    float position[4][2][3];
    short nhits[4];
    float momentum;
    float mom3[3];
    bool helicity;
    bool isprimary;
    float quality;
    int cluster_index[4][2];
    float scatter[3];

};


inline void SvxRecoSingleTrack::setQuality(float q){quality=q;}


inline float SvxRecoSingleTrack::getQuality(){return quality;}


inline void SvxRecoSingleTrack::setNhits(int layer, short n){nhits[layer-1]=n;}


inline short SvxRecoSingleTrack::getNhits(int layer){return nhits[layer-1];}


inline void SvxRecoSingleTrack::setPrimary(bool prim){isprimary=prim;}


inline bool SvxRecoSingleTrack::getPrimary(){return isprimary;}


inline float SvxRecoSingleTrack::getHitPosition(int layer, int coor, short hit){return position[layer-1][hit][coor];}


inline void SvxRecoSingleTrack::setHitPosition(int layer, float x, float y, float z, short hit)
{
  position[layer-1][hit][0]=x;
  position[layer-1][hit][1]=y;
  position[layer-1][hit][2]=z;
}


inline void SvxRecoSingleTrack::setMomentum(float p){momentum=p;}


inline float SvxRecoSingleTrack::getMomentum(){return momentum;}


inline void SvxRecoSingleTrack::set3Momentum(int coor, float pp){mom3[coor]=pp;}

inline float SvxRecoSingleTrack::get3Momentum(int coor){return mom3[coor];}



inline void SvxRecoSingleTrack::setClusterID(int layer, int hit, int index)
{
  cluster_index[layer-1][hit]=index;

}
inline int SvxRecoSingleTrack::getClusterID(int layer, int hit)
{
  return cluster_index[layer-1][hit];
}


inline void SvxRecoSingleTrack::setHelicity(bool hel){helicity=hel;}
inline bool SvxRecoSingleTrack::getHelicity(){return helicity;}

inline void SvxRecoSingleTrack::setScatter(int layer, float sct){scatter[layer-1]=sct;}

inline float SvxRecoSingleTrack::getScatter(int layer){return scatter[layer-1];}






#endif
