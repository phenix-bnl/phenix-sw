// ==================
// FILE: SvxClusterInfov2.h
// ==================

#ifndef __SVXCLUSTERINFOV2_H_
#define __SVXCLUSTERINFOV2_H_

#include <SvxClusterInfo.h>

/**
 * @brief  The implementation v2 of SvxCentralTrack.
 *
 * Created on 3/29/2012 by Takashi Hachiya.
 */

class SvxClusterInfov2 : public SvxClusterInfo { // ver v2
  public:
    SvxClusterInfov2(){ Reset(); /*std::cout<<"Ctorv2"<<std::endl;*/};
    SvxClusterInfov2(const SvxClusterInfov2& info){ copy(info); /*std::cout<<"cpCtorv2 : "<<m_id<<std::endl;*/}
    virtual ~SvxClusterInfov2(){};

    void copy(const SvxClusterInfo& info);


    // access fuction
    void setClusterId(const int id)      { m_id = id;}
    void setLayer(const char layer)      { m_layer = layer;}
    void setLadder(const char ladder)    { m_ladder = ladder;}
    void setSensor(const char sensor)    { m_sensor = sensor;}
    void setPosition(const float x, const float y, const float z)
                                         { m_global_pos[0]=x; m_global_pos[1]=y; m_global_pos[2]=z;}
    void setSize(const short size)       { m_size = size;}
    void setXZSize(const short xsize, const short zsize)
                                         { m_xz_size[0]=xsize, m_xz_size[1]=zsize;}
    void setEdgeFlag(const int flag)     { m_edgeflag=flag;}
    void setAdc(const int adc1, const int adc2) { m_adc[0]=adc1; m_adc[1]=adc2;}
    void setCircumference(const short c) { m_circumference=c;}
    void setAmbiguous(const short a)     { m_ambiguous=a;}

    void setdproj(const float dproj)     { m_dproj = dproj;}
    void setbend(const float bend)       { m_bend  = bend ;}
    void setzproj(const float zproj)     { m_zproj = zproj;}


    int   getClusterId()const            { return m_id;}
    char  getLayer()    const            { return m_layer;}
    char  getLadder()   const            { return m_ladder;}
    char  getSensor()   const            { return m_sensor;}
    float getPosition(const int idx)const{ return (0<=idx&&idx<3) ? m_global_pos[idx]:-9999.;} // idx: 0,1,2=x,y,z
    short getSize()                 const{ return m_size;}
    short getXZSize(const int idx)  const{ return (0<=idx&&idx<2) ? m_xz_size[idx]:-9999;} // idx: 0,1=x,z
    short getEdgeFlag()             const{ return m_edgeflag;}
    int   getAdc(const int idx)     const{ return (0<=idx&&idx<2) ? m_adc[idx]:-9999.;} // idx: 0,1=x,u
    short getCircumference()        const{ return m_circumference;}
    short getAmbiguous()            const{ return m_ambiguous;}

    float getdphi()                 const { return m_dproj+m_bend;}
    float getdz()                   const { return m_zproj-m_global_pos[2];}

    float getdproj()                const { return m_dproj;}
    float getbend()                 const { return m_bend; }
    float getzproj()                const { return m_zproj;}



    void Reset();
    void print();

  public:
    short m_id;
    char  m_layer;
    char  m_ladder;
    char  m_sensor;
    float m_global_pos[3];
    short m_size;
    short m_xz_size[2];
    short m_edgeflag;
    int   m_adc[2];  ///< adc sum for each readout (x & u)
    short m_circumference;
    short m_ambiguous; 

    float m_dproj;
    float m_bend;
    float m_zproj;

  ClassDef(SvxClusterInfov2,1)
};
#endif
