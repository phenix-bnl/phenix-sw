#ifndef __RPSNGLSUMXYV1_H
#define __RPSNGLSUMXYV1_H

#include "RpSnglSumXY.h"
#include "TString.h"

#include <iostream>

/**
 * @brief  The implement v1 class for a storage of reaction plane element 
 *
 * Created on 11/16/2011 by Takashi Hachiya.
 */


class RpSnglSumXYv1 : public RpSnglSumXY
{
  public:
  
    RpSnglSumXYv1();
    RpSnglSumXYv1(const char *name, const unsigned int id, const float qx, const float qy, const float w);
    virtual ~RpSnglSumXYv1(){}

    virtual void QVector(const short index, const float qvec){ if(index==0||index==1) m_qvector[index] = qvec; // index=0:Qx, 1:Qy
                                                               else {std::cerr<<"out of range. index="<<index<<std::endl;}
                                                             }
    virtual float QVector(const short index) const           { if(index==0||index==1) return m_qvector[index]; 
                                                               else { std::cerr<<"out of range. index:"<<index<<std::endl; return -9999.; }
                                                             }
    virtual void Weight(const float weight) { m_weight = weight; }
    virtual float Weight() const            { return m_weight;};
    
    virtual void Name(const char *name)     { m_name = name; }
    virtual const char *Name() const        { return m_name.Data(); }
    virtual void IdCode(const int idcode) { m_idcode = idcode; }
    virtual int IdCode() const            { return m_idcode; }

  private:
    float   m_qvector[2]; // 0:x, 1:y
    float   m_weight;
    int     m_idcode;
    TString m_name;
    
    ClassDef(RpSnglSumXYv1,1)
};

#endif /* __RPSNGLSUMXYV1_H */
