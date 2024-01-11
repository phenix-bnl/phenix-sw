// ==================
// FILE: SvxResidualInfov1.h
// ==================

#ifndef __SVXRESIDUALINFOV1_H_
#define __SVXRESIDUALINFOV1_H_

#include <SvxResidualInfo.h>

/**
 * @brief  The implementation v1 of SvxResidualInfo.
 *
 * Created on 3/29/2012 by Takashi Hachiya.
 */

class SvxResidualInfov1 : public SvxResidualInfo { // ver v1
  public:
    SvxResidualInfov1(){ Reset(); /*std::cout<<"Ctorv1"<<std::endl;*/};
    SvxResidualInfov1(const SvxResidualInfov1& info){ copy(info); /*std::cout<<"cpCtorv1 : "<<m_id<<std::endl;*/}
    virtual ~SvxResidualInfov1(){};

    void copy(const SvxResidualInfo& info);


    // access fuction
    void setClusterId(const int id)  { m_id = id;}
    void setdphi(const float dphi)   { m_dphi = dphi;}
    void setdz  (const float dz)     { m_dz   = dz;}

    int   getClusterId()       const { return m_id;}
    float getdphi()            const { return m_dphi;}
    float getdz()              const { return m_dz;}

    void Reset();
    void print();

  public:
    short m_id;

    float m_dphi;
    float m_dz;

  ClassDef(SvxResidualInfov1,1)
};
#endif
