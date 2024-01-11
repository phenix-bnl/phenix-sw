// Interface Object Class : TFvtxCoord
// Author: M. Brooks
// Date: 07/26/2006
// Description: Class for Forward Silicon Coordinate.

#ifndef __TFvtxCOORD_v1H__
#define __TFvtxCOORD_v1H__

#include<TFvtxCoord.h>
#include<PHKey.hh>
#include<PHLine.h>
#include<FVTXOO.h>
#include<map>


class TFvtxCoord_v1 : public TFvtxCoord
{

 public:

  TFvtxCoord_v1();

  TFvtxCoord_v1(const Key& key,
	       unsigned short arm,
               unsigned short cage,
	       unsigned short station,
	       unsigned short sector,
	       unsigned short column,
	       unsigned short index);

  TFvtxCoord_v1(const TFvtxCoord& base_ref);

  TFvtxCoord_v1(const TFvtxCoord* base_ptr);

  virtual ~TFvtxCoord_v1(){;}

  PHLine get_coord() const;
  PHPoint get_coord_end() const;
  PHPoint get_coord_begin() const;
  virtual double get_mean_z() const;

  Float_t get_q_peak() const {return _q_peak;}
  Float_t get_q_tot() const {return _q_tot;}
  double get_q_error() const {return _q_error;}
  unsigned short get_peak_strip() const {return _peak_strip;}
  Float_t get_w() const {return _w;}
  virtual Float_t get_error() const {return _error;}
  void set_coord(const PHLine& coord);
  void set_q_peak(Float_t q_peak) { _q_peak = q_peak;}
  void set_q_tot(Float_t q_tot) { _q_tot = q_tot;}
  void set_q_error(double q_error) { _q_error = q_error;}
  void set_peak_strip(unsigned short peak_strip) { _peak_strip = peak_strip;}
  void set_w(Float_t w) { _w = w;}
  void set_error(double error){_error = error;}

  unsigned short  get_arm() const {return _arm;}
  unsigned short  get_cage() const {return _cage;}
  unsigned short  get_station() const {return _station;}
  unsigned short  get_sector() const {return _sector;}
  unsigned short  get_column() const {return _column;}
  unsigned short  get_index() const {return _index;}
  void set_arm(unsigned short arm){ _arm = arm; }
  void set_cage(unsigned short cage){ _cage = cage; }
  void set_station(unsigned short station){ _station = station; }
  void set_sector(unsigned short sector){ _sector = sector; }
  void set_column(unsigned short column){ _column = column; }
  void set_index(unsigned short index) {_index = index;}

  void set_usedintrack()
  {_status |= 1<<TFvtxCoord::USEDINTRACK;}

  bool get_usedintrack() const
  { return (_status & 1<<TFvtxCoord::USEDINTRACK) != 0; }

  void set_status(unsigned long status) { _status = status; }
  unsigned long get_status() const {return _status;}
  void clear_status() { _status=0;}

  void print(std::ostream& os = std::cout) const;

#ifndef __CINT__
  void push_chi_sqr_inc(unsigned long track_key, double chi_square) {
    _chi_inc.insert(std::make_pair(track_key,chi_square));
  }
  double get_chi_sqr_inc(unsigned long track_key) {
    std::map<unsigned long, double>::iterator iter = _chi_inc.find(track_key);
    if(iter == _chi_inc.end()) {
      return 0;
    } else {
      return iter->second;
    }
  }
#endif

 private:

  unsigned short _arm;
  unsigned short _cage;
  unsigned short _station;
  unsigned short _sector;
  unsigned short _column;
  unsigned short _index;
  double _q_peak;
  double _q_tot;
  unsigned short _peak_strip;
  double _error;
  double _w;
  unsigned long _status;
  double _q_error;

  enum {POINT_SIZE=3};
  double _point_1[POINT_SIZE];
  double _point_2[POINT_SIZE];

#ifndef __CINT__
  std::map<unsigned long, double> _chi_inc;
#endif

  ClassDef(TFvtxCoord_v1,1)
};


#endif /* __TFvtxCOORD_v1H__*/










