// Interface Object Class : TFvtxCompactCoord
// Author: Cesar da Silva
// Date: 03/31/2016
// Description: Class for Forward Silicon Compact Coordinate.

#ifndef __TFvtxCompactCOORD_v1H__
#define __TFvtxCompactCOORD_v1H__

#include<TFvtxCompactCoord.h>
#include<PHKey.hh>
#include<PHLine.h>
#include<FVTXOO.h>
#include<map>


class TFvtxCompactCoord_v1 : public TFvtxCompactCoord
{

 public:

  TFvtxCompactCoord_v1();

  TFvtxCompactCoord_v1(const Key& key,
	       unsigned short arm,
               unsigned short cage,
	       unsigned short station,
	       unsigned short sector,
	       unsigned short column,
	       unsigned short index);

  TFvtxCompactCoord_v1(const TFvtxCompactCoord& base_ref);

  TFvtxCompactCoord_v1(const TFvtxCompactCoord* base_ptr);

  virtual ~TFvtxCompactCoord_v1(){;}

  PHLine get_coord() const;
  PHPoint get_coord_end() const;
  PHPoint get_coord_begin() const;
  virtual double get_mean_z() const;

  void set_coord(const PHLine& coord);

  unsigned short  get_arm() const 
  {return (_peak_strip >> TFvtxCompactCoord::ARM) & 0x01;}
  unsigned short  get_cage() const 
  {return (_peak_strip >> TFvtxCompactCoord::CAGE) & 0x01;}
  unsigned short  get_station() const 
  {return (_peak_strip >> TFvtxCompactCoord::STATION) & 0x03;}
  unsigned short  get_sector() const 
  {return (_peak_strip >> TFvtxCompactCoord::SECTOR) & 0x3f;}
  unsigned short  get_column() const 
  {return (_peak_strip >> TFvtxCompactCoord::COLUMN) & 0x01;}
  unsigned short  get_index() const 
  {return (_peak_strip >> TFvtxCompactCoord::STRIP) & 0x7ff;}
  unsigned int get_peak_strip() const {return _peak_strip;}

  void set_arm(unsigned short arm)
  { _peak_strip |= (arm&0x01)<<TFvtxCompactCoord::ARM; }
  void set_cage(unsigned short cage)
  { _peak_strip |= (cage&0x01)<<TFvtxCompactCoord::CAGE; }
  void set_station(unsigned short station)
  { _peak_strip |= (station&0x03)<<TFvtxCompactCoord::STATION; }
  void set_sector(unsigned short sector)
  { _peak_strip |= (sector&0x3f)<<TFvtxCompactCoord::SECTOR; }
  void set_column(unsigned short column)
  { _peak_strip |= (column&0x01)<<TFvtxCompactCoord::COLUMN; }
  void set_index(unsigned short index) 
  { _peak_strip |= (index&0x7ff)<<TFvtxCompactCoord::STRIP;}

  void set_usedintrack()
  {_peak_strip |= 1<<TFvtxCompactCoord::USEDINTRACK;}

  bool get_usedintrack() const
  { return (_peak_strip & 1<<TFvtxCompactCoord::USEDINTRACK) != 0; }

  void print(std::ostream& os = std::cout) const;

 private:

  unsigned int _peak_strip;

  enum {POINT_SIZE=3};
  float _point_1[POINT_SIZE];
  float _point_2[POINT_SIZE];

  ClassDef(TFvtxCompactCoord_v1,1)
};


#endif /* __TFvtxCompactCOORD_v1H__*/










