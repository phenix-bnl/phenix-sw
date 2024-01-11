#ifndef TECCLUSTER_H
#define TECCLUSTER_H 

#include <PHObject.h>
#include <PHCompositeNode.h>
#include <TecCalibrationObject.hh>
#include <TecGeometryObject.hh>
#include <cmath>


class TecCluster : public PHObject {
 
 public:
 
/// Destructor
  virtual ~TecCluster() { }

  virtual short get_side()   const   {return -1;}
  virtual short get_sector()  const  {return -1;}
  virtual short get_plane() const    {return -1;}
  virtual short get_index() const    {return -1;}
  virtual short get_wire()  const    {return -1;}
  virtual short get_avgtime() const  {return -1;}
  virtual short get_ntimebins() const {return -1;}
  virtual float get_charge() const   {return NAN;}
  virtual float get_xyz_global(TecCalibrationObject* TCO, TecGeometryObject* TGO, unsigned short i) {return NAN;}
  virtual float get_xyz_global(PHCompositeNode *topNode, unsigned short i) {return NAN;}

  virtual void set_index(const short a) {return;}
  virtual void set_index(const short sector_, const short side_, const short plane_) {return;}
  virtual void set_wire(const short a) {return;}
  virtual void set_avgtime(const short a) {return;}
  virtual void set_ntimebins(const short a) {return;}
  virtual void set_charge(const float a)  {return;}

///
  virtual void identify(std::ostream& os = std::cout) const;
  TecCluster(const TecCluster &source) {}

 protected:
/// Default constructor
  TecCluster() {}

  ClassDef(TecCluster,1)

};

#endif // TECCLUSTER_H
