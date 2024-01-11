// $Id: TMuiHitO_v1.h,v 1.1 2006/04/22 01:58:28 hpereira Exp $

#ifndef _TMuiHitO_v1_h_
#define _TMuiHitO_v1_h_

/*!
	\file TMuiHitO_v1.h
	\brief Interface Object Class : TMuiHitO
	\author Jason Newby
  \version $Revision: 1.1 $
  \date    $Date: 2006/04/22 01:58:28 $
*/

#include "TMuiHitO.h"
#include<MUIOO.h>

//! Interface Object Class : TMuiHitO
class TMuiHitO_v1 : public TMuiHitO 
{

public:

  TMuiHitO_v1();  

  virtual ~TMuiHitO_v1()
  {;}

  TMuiHitO_v1(const Key&,
  UShort_t arm,
  UShort_t plane,
  UShort_t panel,
  UShort_t orientation,
  UShort_t twopack,
  UShort_t index);  


  TMuiHitO_v1(const TMuiHitO*);  

  TMuiHitO_v1(const TMuiHitO&);  

  void set_arm( UShort_t arm) 
  { _arm=arm;}

  void set_plane( UShort_t plane) 
  { _plane=plane;}

  void set_panel( UShort_t panel) 
  { _panel=panel;}

  void set_orientation( UShort_t orientation) 
  { _orientation=orientation;}

  void set_twopack( UShort_t twopack) 
  { _twopack=twopack;}

  void set_index( UShort_t index) 
  { _index=index;}

  UShort_t get_arm() const 
  {return _arm;}

  UShort_t get_plane() const 
  {return _plane;}

  UShort_t get_panel() const 
  {return _panel;}

  UShort_t get_orientation() const 
  {return _orientation;}

  UShort_t get_twopack() const 
  {return _twopack;}

  UShort_t get_index() const 
  {return _index;}

  void print(std::ostream& os = std::cout) const;

private:

  UShort_t _arm;
  UShort_t _plane;
  UShort_t _panel;
  UShort_t _orientation;
  UShort_t _twopack;
  UShort_t _index;

  ClassDef(TMuiHitO_v1,1)
};

#endif
