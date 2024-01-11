// ==================
// FILE: SvxResidualInfo.h
// ==================

#ifndef __SVXRESIDUALINFO_H_
#define __SVXRESIDUALINFO_H_

#include <iostream>
#include <PHObject.h>
#include <phool.h>

/**
 * @brief  The abstract of SvxResidualInfo.
 *
 * Created on 4/14/2012 by Takashi Hachiya.
 */

class SvxResidualInfo : public PHObject { 
  public:
    SvxResidualInfo(){};
    SvxResidualInfo(const SvxResidualInfo& info){ /*std::cout<<"cpCtor"<<std::endl;*/}

    virtual ~SvxResidualInfo(){};

    SvxResidualInfo& operator=(const SvxResidualInfo& info){
      copy(info);
      return *this;
    }

    virtual void copy(const SvxResidualInfo& info){PHOOL_VIRTUAL_WARN("copy");}

    // access fuction
    virtual void setClusterId(const int id)      { PHOOL_VIRTUAL_WARN("setClusterId");}
    virtual void setdphi(const float dphi)       { PHOOL_VIRTUAL_WARN("setdphi");}
    virtual void setdz(const float dz)           { PHOOL_VIRTUAL_WARN("setdz");}

    virtual int   getClusterId()             const { PHOOL_VIRTUAL_WARN("getClusterId"); return -9999;}
    virtual float getdphi()                  const { PHOOL_VIRTUAL_WARN("setdphi");  return -9999.;}
    virtual float getdz()                    const { PHOOL_VIRTUAL_WARN("setdz");    return -9999.;}

    virtual void Reset() {
      std::cout << PHWHERE << "ERROR: Reset() not implemented by daughter function" << std::endl;
    }

    virtual void print(){ PHOOL_VIRTUAL_WARN("print"); }

    ClassDef(SvxResidualInfo, 1);
};

#endif
