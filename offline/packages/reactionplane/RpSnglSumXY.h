#ifndef __RPSNGLSUMXY_H
#define __RPSNGLSUMXY_H

#include <iostream>
#include <string>
#include "PHObject.h"

/**
 * @brief  The abstract class for a storage of reaction plane element
 *
 * Created on 11/16/2011 by Takashi Hachiya.
 */

class RpSnglSumXY : public PHObject
{
  public:
  
    virtual ~RpSnglSumXY() {}
    virtual RpSnglSumXY *clone() const;
    virtual RpSnglSumXY& operator=(const RpSnglSumXY& source);

    virtual void QVector(const short index, const float qvec){} 
    virtual float QVector(const short index) const { return -9999.;}
    virtual void Weight(const float weight) {;}
    virtual float Weight() const { return -9999.;};
    
    virtual void Name(const char *name) {}
    virtual const char *Name() const {return "DUMMY";}
    virtual void IdCode(const int idcode) {}
    virtual int IdCode() const {return -9999;}

  protected:
    void virtual_warning(const char *label){
      std::cout << "RpSnglSumXY::"<<label<<" not implemented by daughter class" << std::endl;
    }
    
    ClassDef(RpSnglSumXY,1)
};

#endif /* __RPSNGLSUMXY_H */
