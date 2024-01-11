// ==================
// FILE: SvxClusterInfo.h
// ==================

#ifndef __SVXCLUSTERINFO_H_
#define __SVXCLUSTERINFO_H_

#include <iostream>
#include <PHObject.h>
#include <phool.h>

/**
 * @brief  The implementation v1 of SvxClusterInfo.
 *
 * Created on 11/27/2011 by Takashi Hachiya.
 */

class SvxClusterInfo : public PHObject { 
  public:
    SvxClusterInfo(){};
    SvxClusterInfo(const SvxClusterInfo& info){ /*std::cout<<"cpCtor"<<std::endl;*/}

    virtual ~SvxClusterInfo(){};

    SvxClusterInfo& operator=(const SvxClusterInfo& info){
      copy(info);
      return *this;
    }

    virtual void copy(const SvxClusterInfo& info){PHOOL_VIRTUAL_WARN("copy");}

    // access fuction
    virtual void setClusterId(const int id)      { PHOOL_VIRTUAL_WARN("setClusterId");}
    virtual void setLayer(const char layer)      { PHOOL_VIRTUAL_WARN("setLayer"); }
    virtual void setLadder(const char ladder)    { PHOOL_VIRTUAL_WARN("setLadder");}
    virtual void setSensor(const char sensor)    { PHOOL_VIRTUAL_WARN("setSensor");}
    virtual void setPosition(const float x, const float y, const float z)
                                                 { PHOOL_VIRTUAL_WARN("setPosition");}
    virtual void setSize(const short size)       { PHOOL_VIRTUAL_WARN("setSize");}
    virtual void setXZSize(const short xsize, const short zsize)
                                                 { PHOOL_VIRTUAL_WARN("setXZSize");}
    virtual void setEdgeFlag(const int flag)     { PHOOL_VIRTUAL_WARN("setEdgeFlag");}
    virtual void setAdc(const int adc1, const int adc2)
                                                 { PHOOL_VIRTUAL_WARN("setAdc");}
    virtual void setCircumference(const short c) { PHOOL_VIRTUAL_WARN("setCircumference");}
    virtual void setAmbiguous(const short a)     { PHOOL_VIRTUAL_WARN("setAmbiguous");}

    virtual void setdphi(const float dphi)       { PHOOL_VIRTUAL_WARN("setdphi");}
    virtual void setdz(const float dz)           { PHOOL_VIRTUAL_WARN("setdz");}

    virtual void setdproj(const float dproj)     { PHOOL_VIRTUAL_WARN("setdproj");}
    virtual void setbend(const float bend)       { PHOOL_VIRTUAL_WARN("setbend");}
    virtual void setzproj(const float zproj)     { PHOOL_VIRTUAL_WARN("setzproj");}
    
    virtual void setfitdphi(const float dphi)    { PHOOL_VIRTUAL_WARN("setfitdphi");}
    virtual void setfitdz(const float dz)        { PHOOL_VIRTUAL_WARN("setfitdz");}

    virtual void setscatter(const float scat)    { PHOOL_VIRTUAL_WARN("setscatter");}
    virtual void setscatterXY(const float scat)  { PHOOL_VIRTUAL_WARN("setscatterXY");}
    virtual void setscatterRZ(const float scat)  { PHOOL_VIRTUAL_WARN("setscatterRZ");}
    virtual void setsscatter(const float sscat)  { PHOOL_VIRTUAL_WARN("setsscatter");}

    virtual void setNcold(const int n)           { PHOOL_VIRTUAL_WARN("setNcold");}
    virtual void setNhot(const int n)            { PHOOL_VIRTUAL_WARN("setNhot");}


    virtual int   getClusterId()             const { PHOOL_VIRTUAL_WARN("getClusterId"); return -9999;}
    virtual char  getLayer()                 const { PHOOL_VIRTUAL_WARN("getLayer");     return -127;}
    virtual char  getLadder()                const { PHOOL_VIRTUAL_WARN("getLadder");    return -127;}
    virtual char  getSensor()                const { PHOOL_VIRTUAL_WARN("getSensor");    return -127;}
    virtual float getPosition(const int idx) const { PHOOL_VIRTUAL_WARN("getPosition");  return -9999.;} // idx: 0,1,2=x,y,z
    virtual short getSize()                  const { PHOOL_VIRTUAL_WARN("getSize");      return -9999;}
    virtual short getXZSize(const int idx)   const { PHOOL_VIRTUAL_WARN("getXZSize");    return -9999;} // idx: 0,1=x,z
    virtual short getEdgeFlag()              const { PHOOL_VIRTUAL_WARN("getEdgeFlag");  return -9999;}
    virtual int   getAdc(const int idx)      const { PHOOL_VIRTUAL_WARN("getAdc");       return -9999;} // idx: 0,1=x,u
    virtual short getCircumference()         const { PHOOL_VIRTUAL_WARN("getCircumference"); return -9999;}
    virtual short getAmbiguous()             const { PHOOL_VIRTUAL_WARN("getAmbiguous"); return -9999;}

    virtual float getdphi()                  const { PHOOL_VIRTUAL_WARN("setdphi");  return -9999.;}
    virtual float getdz()                    const { PHOOL_VIRTUAL_WARN("setdz");    return -9999.;}

    virtual float getdproj()                 const { PHOOL_VIRTUAL_WARN("setdproj"); return -9999.;}
    virtual float getbend()                  const { PHOOL_VIRTUAL_WARN("setbend");  return -9999.;}
    virtual float getzproj()                 const { PHOOL_VIRTUAL_WARN("setzproj"); return -9999.;}

    virtual float getfitdphi()               const { PHOOL_VIRTUAL_WARN("getfitdphi"); return -9999.;}
    virtual float getfitdz()                 const { PHOOL_VIRTUAL_WARN("getfitdz"); return -9999.;}

    virtual float getscatter()               const { PHOOL_VIRTUAL_WARN("getscatter"); return -9999.;}
    virtual float getscatterXY()             const { PHOOL_VIRTUAL_WARN("getscatterXY"); return -9999.;}
    virtual float getscatterRZ()             const { PHOOL_VIRTUAL_WARN("getscatterRZ"); return -9999.;}
    virtual float getsscatter()              const { PHOOL_VIRTUAL_WARN("getsscatter"); return -9999.;}

    virtual short getNcold()                 const { PHOOL_VIRTUAL_WARN("getNcold"); return -9999;}
    virtual short getNhot()                  const { PHOOL_VIRTUAL_WARN("getNhot"); return -9999;}


    virtual void Reset() {
      std::cout << PHWHERE << "ERROR: Reset() not implemented by daughter function" << std::endl;
    }

    virtual void print(){ PHOOL_VIRTUAL_WARN("print"); }

    virtual int  get_sublayer();

    ClassDef(SvxClusterInfo, 1);
};

#endif
