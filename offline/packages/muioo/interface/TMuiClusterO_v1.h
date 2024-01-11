// $Id: TMuiClusterO_v1.h,v 1.1 2006/04/22 01:58:27 hpereira Exp $

#ifndef _TMUICLUSTERO_V1_H_
#define _TMUICLUSTERO_V1_H_

#include "TMuiClusterO.h"
#include<MUIOO.h>


/*! Version v1 */
class TMuiClusterO_v1 : public TMuiClusterO {

public:

  TMuiClusterO_v1();  

  virtual ~TMuiClusterO_v1(){;}

  TMuiClusterO_v1(const Key&,
		  UShort_t arm,
		  UShort_t plane,
		  UShort_t panel,
		  UShort_t orientation,
		  UShort_t index);  

  TMuiClusterO_v1(const TMuiClusterO*);  
  TMuiClusterO_v1(const TMuiClusterO&);  

  void set_arm( UShort_t arm) { _arm=arm;}
  void set_plane( UShort_t plane) { _plane=plane;}
  void set_panel( UShort_t panel) { _panel=panel;}
  void set_orientation( UShort_t orientation) { _orientation=orientation;}
  void set_index( UShort_t index) { _index=index;}

  UShort_t get_arm() const {return _arm;}
  UShort_t get_plane() const {return _plane;}
  UShort_t get_panel() const {return _panel;}
  UShort_t get_orientation() const {return _orientation;}
  UShort_t get_index() const {return _index;}

  void set_size(UShort_t size) {_size=size;}
  void set_centroidpos(const PHPoint& point);
  void set_centroidsigma(const PHPoint& point);
  
  UShort_t get_size() const {return 0;}
  PHPoint get_centroidpos() const;
  PHPoint get_centroidsigma() const;
  
  void print(std::ostream& os = std::cout) const;
  
private:
  
  UShort_t _arm;
  UShort_t _plane;
  UShort_t _panel;
  UShort_t _orientation;
  UShort_t _size;
  UShort_t _index;
  
  enum {POINT_SIZE=3};
  double _centroidpos[POINT_SIZE]; 
  double _centroidsigma[POINT_SIZE]; 

  ClassDef(TMuiClusterO_v1,1)
};

#endif
