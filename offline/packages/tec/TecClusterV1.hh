#ifndef TECCLUSTERV1_H
#define TECCLUSTERV1_H 

#include <TecCluster.hh>

class TecGeometryObject;
class TecCalibrationObject;
class PHCompositeNode;

/** Represents one fired Tec time bin */
class TecClusterV1 : public TecCluster {
 
 public:
 
/// Default constructor
  TecClusterV1(); 
  TecClusterV1(const TecCluster &source); 
/// Constructors
  TecClusterV1(short index_,
	     short wire_, short avgtime_, short ntimebins_,
	     float charge_);

  TecClusterV1(short side_, short sector_, short plane_,
	     short wire_, short avgtime_, short ntimebins_,
	     float charge_);

/// Destructor
  virtual ~TecClusterV1() { }

  short get_index()  const    {return index;}
  short get_side()   const    {return index%2;}
  short get_sector() const    {return index/12;}
  short get_plane()  const    {return (index%12)/2;}
  short get_wire()    const   {return wire;}
  short get_ntimebins() const  {return ntimebins;}
  short get_avgtime() const   {return avgtime;}
  float get_charge() const;// const    {return charge;}
  float get_xyz_global(TecCalibrationObject* TCO, TecGeometryObject* TGO, unsigned short i);
  float get_xyz_global(PHCompositeNode *topNode, unsigned short i);


  void set_index(const short a) {index = a;}
  void set_index(const short sector_, const short side_, const short plane_) {index = side_ + plane_*2 + sector_*2*6;}
  void set_wire(const short a) {wire = a;}
  void set_ntimebins(const short a) {ntimebins = a;}
  void set_avgtime(const short a)   {avgtime = a;}
  void set_charge(const float a);//  {charge = a;}

///
  void identify(std::ostream& os = std::cout) const;

 private:
  void calc_position(TecGeometryObject *TGO, TecCalibrationObject *TCO);

 protected:

  short index;
  short wire;
  short ntimebins;
  short avgtime;
  short charge;
  float xyz[3];//!
  bool xyz_calculated;//!

  ClassDef(TecClusterV1,1)

};

#endif // TECCLUSTERV1_H
