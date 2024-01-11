
#ifndef __RpcStrip_H__
#define __RpcStrip_H__

#include <RPCGEOM.h>

#include <PHPoint.h>
#include <TObject.h>

class PHPoint;

class RpcStrip : public TObject
{

 public: 

  //constructor
  RpcStrip(){InitVal();}

  //destructor
  virtual ~RpcStrip(){;}
 
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
  virtual void SetStrip(int _arm,int _station,int _octant,int _halfoctant,int _Rseg,int _strip);

  // convert point to RpcStrip object
  // assumption none;
  // result     RpcStrip objects

  virtual void SetStrip(double x, double y, int arm_index, int station_index, int R_segmentation);
  virtual void SetStrip(double x, double y, int arm_index, int station_index, int R_segmentation, int excludestrip) { std::cout << "Function not enabled" << std::endl; return; }

  virtual double GetPointStripDCA(double x, double y, int arm_index, int station_index, int octant, int halfoct, int R_segmentation, int strip) { std::cout << "GetPointStripDCA function not available" << std::endl; return -999.; }
  virtual int GetNumStrips(int _arm,int _station,int _octant,int _halfoctant,int _Rseg);

  // Init
  virtual const void Init(){InitVal();}

  virtual void print(std::ostream& os = std::cout) const{
    os << "RpcStrip::print " << std::endl
       << "arm " << arm << "\tsta " << station
       << "\toct " << octant << "\thalfoct " << halfoctant 
       << "\tRseg " << Rseg << "\tstr " << strip << std::endl;
  }

  // Get's
  virtual PHPoint GetBegin();
  virtual PHPoint GetEnd();
  virtual PHPoint GetMid();

  virtual int GetArm() const { return arm; }
  virtual int GetStation()const {return station;}
  virtual int GetOctant() const { return octant; }
  virtual int GetHalfOctant() const { return halfoctant; }
  virtual int GetRSeg()const {return Rseg;}
  virtual int GetStripId() const {return strip;}

  virtual double GetStripWidth() const {
    if(station<0 || station>2 || Rseg<0 || Rseg>2) {
      std::cout << "RpcStrip::GetStripWidth() ... station/Rseg out of range" << std::endl; return 0; }
    return RPCGEOM::strip_size[station][Rseg];}
  virtual double GetStripLength() const { return 0; }
  
  virtual int IsEmpty() const;

  virtual int IsDead() { return -1; }

  virtual int IsInSameArm(RpcStrip& str)const {
    if ( arm == str.arm ) return 1;
    return 0;
  }
  virtual int IsInSameStation(RpcStrip &str)const {
    if ( IsInSameArm(str) && station == str.station ) return 1;
    return 0;
  }
  virtual int IsInSameOctant(RpcStrip &str)const {
    if ( IsInSameArm(str) && IsInSameStation(str)
	 && octant == str.octant) return 1;
    return 0;
  }
  virtual int IsInSameRSeg(RpcStrip &str)const {
    if( IsInSameArm(str) && IsInSameStation(str)
	&& IsInSameOctant(str) && Rseg == str.Rseg ) return 1;
    return 0;
  }


 protected:

  virtual const void InitVal();

  Float_t begin_x,begin_y,begin_z;
  Float_t end_x,end_y,end_z;
  Float_t mid_x,mid_y,mid_z;

  virtual void CheckStrip(int _arm,int _station,int _octant,int _halfoctant,int _Rseg,int _strip);

  virtual double GetStripWidth(int _station,int _Rseg) const {return RPCGEOM::strip_size[_station][_Rseg];}

  virtual int GetNumStripsInner(int _arm,int _station,int _octant,int _halfoctant,int _Rseg);
  virtual int GetNumStripsOuter(int _arm,int _station,int _octant,int _halfoctant,int _Rseg);

 public:

  ClassDef(RpcStrip,1)
};

#endif /* __RpcStrip_H__*/
