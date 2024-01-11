
#ifndef __RpcStrip_v1_H__
#define __RpcStrip_v1_H__

#include <RPCFULLGEOM.h>

#include <RpcStrip.h>
#include <RpcDBInfo.h>
#include <PHPoint.h>
#include <TObject.h>

class PHPoint;

class RpcStrip;

class RpcStrip_v1 : public RpcStrip
{

 public: 

  //constructor
  RpcStrip_v1(){InitVal();}

  //destructor
  virtual ~RpcStrip_v1(){;}
 
  // data members
  int arm;
  int station;
  int octant;
  int halfoctant;
  int Rseg;
  int strip;

  // set strip # and get the global position of the strip
  //  assumption none;
  //  result   RpcStrip object;
  virtual void SetStrip(int _arm,int _station,int _octant,int _halfoctant,int _Rseg,int _strip);//done

  // convert point to RpcStrip object
  // assumption none;
  // result     RpcStrip objects

  virtual void SetStrip(double x, double y, int arm_index, int station_index, int R_segmentation);
  virtual void SetStrip(double x, double y, int arm_index, int station_index, int R_segmentation, int excludestrip);
  virtual double GetPointStripDCA(double x, double y, int arm_index, int station_index, int octant, int halfoct, int R_segmentation, int strip);

  virtual int GetNumStrips(int _arm,int _station,int _octant,int _halfoctant,int _Rseg) { std::cout << "Depreciated Function \"GetNumStrips\" for RpcStrip_v1" << std::endl; return -999;}
  
  // Init
  virtual const void Init(){InitVal();}

  virtual void print(std::ostream& os = std::cout) const{
    os << "RpcStrip_v1::print " << std::endl
       << "arm " << arm << "\tsta " << station
       << "\toct " << octant << "\thalfoct " << halfoctant 
       << "\tRseg " << Rseg << "\tstr " << strip << std::endl;}

  // Get's
  virtual PHPoint GetBegin();//done
  virtual PHPoint GetEnd();//done
  virtual PHPoint GetMid();//done

  virtual int GetArm() const { return arm; }
  virtual int GetStation()const {return station;}
  virtual int GetOctant() const { return octant; }
  virtual int GetHalfOctant() const { return halfoctant; }
  virtual int GetRSeg()const {return Rseg;}
  virtual int GetStripId() const {return strip;}

  virtual double GetStripWidth() const;//done
  virtual double GetStripLength() const;//done

  virtual int IsEmpty() const;

  virtual int IsDead();

 protected:

  virtual const void InitVal();

  Float_t begin_x,begin_y,begin_z;
  Float_t end_x,end_y,end_z;
  Float_t mid_x,mid_y,mid_z;

  //This is REALLY bad:
  virtual void CheckStrip(int _arm,int _station,int _octant,int _halfoctant,int _Rseg,int _strip);

  virtual double GetStripWidth(int _station, int _Rseg) const;  // done

  RpcDBInfo *db;
  
 public:

  ClassDef(RpcStrip_v1,1)
};

#endif /* __RpcStrip_H__*/
