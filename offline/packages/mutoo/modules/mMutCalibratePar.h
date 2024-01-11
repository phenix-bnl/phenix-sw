#ifndef __MMUTCALIBPAR_HH__
#define __MMUTCALIBPAR_HH__

#include<PHObject.h>
#include<MUTOO_FEM.h>
#include<PHException.h>
#include<TDataType.h>
#include<TMutParBase.h>
#include<boost/array.hpp>

//!Runtime parameter object for mMutUnpack analysis module
/*!
*/
class mMutCalibratePar : public TMutParBase
{
 public:

  //  enum Mode {DATA,SIMULATIONS};

  /*! default constructor */

  mMutCalibratePar() :
    _fit_type(AVERAGE),
    _n_sample(MUTOO_FEM::NSAMPLES),
    _t_rise(500),
    _t_fall(1500),
    _min_gain(0),
    _fit_rise(0),
    _fit_fall(0)
    {
      _t_sample[0] = 100;
      _t_sample[1] = 600;
      _t_sample[2] = 700;
      _t_sample[3] = 800;
    }

  /*! destructor */
  virtual ~mMutCalibratePar(){;}

  /*! Enumeration for the sample fitting scheme */
  enum FitType {AVERAGE,EXP};

  std::string get_fit_type_name( void ) const
  {
    switch( _fit_type )
    {
      case AVERAGE: return "AVERAGE";
      case EXP: return "EXP";
      default: return "Unknown";
    }
  }

  void set_min_gain(Float_t min_gain){_min_gain = min_gain;}
  void set_fit_type(FitType fit_type){_fit_type = fit_type;}
  void set_t_rise(Float_t t_rise){_t_rise = t_rise;}
  void set_t_fall(Float_t t_fall){_t_fall = t_fall;}
  void set_fit_rise(Int_t fit_rise){_fit_rise = fit_rise;}
  void set_fit_fall(Int_t fit_fall){_fit_fall = fit_fall;}
  void set_n_sample(unsigned short n_sample){_n_sample = n_sample;}
  void set_t_sample(unsigned short index, Float_t t){
    BOUNDS_CHECK(index,TSAMPLE_SIZE); _t_sample[index] = t;
  }

  Float_t get_min_gain() const {return _min_gain;}
  FitType get_fit_type() const {return _fit_type;}
  Int_t get_fit_rise()  const {return _fit_rise;}
  Int_t get_fit_fall()  const {return _fit_fall;}
  Float_t get_t_rise()  const {return _t_rise;}
  Float_t get_t_fall()  const {return _t_fall;}
  unsigned short get_n_sample() const {return _n_sample;}
  Float_t get_t_sample(unsigned short index) const {
    BOUNDS_CHECK(index,TSAMPLE_SIZE); return _t_sample[index];
  }

  void print(std::ostream& os = std::cout)
  {
    MUTOO::PRINT( os, "mMutCalibratePar" );
    os << "fit_type: " << get_fit_type_name() << std::endl;
    os << "n samples: " << _n_sample << std::endl;
    os << "sample times: "
       << _t_sample[0] << " "
       << _t_sample[1] << " "
       << _t_sample[2] << " "
       << _t_sample[3] << std::endl;
    os << "t rise: " << _t_rise << std::endl;
    os << "t fall: " << _t_fall << std::endl;
    os << "min gain: " << _min_gain << std::endl;
    os << "fit rise: " << _fit_rise << std::endl;
    os << "fit fall: " << _fit_fall << std::endl;
    MUTOO::PRINT( os, "**" );
  }

 private:

  enum {TSAMPLE_SIZE=4};
  boost::array<Float_t, TSAMPLE_SIZE>  _t_sample;
  FitType _fit_type;
  unsigned short _n_sample;
  Float_t _t_rise;
  Float_t _t_fall;
  Float_t _min_gain;
  Int_t _fit_rise;
  Int_t _fit_fall;
};

#endif /* __MMUTUNPACK_HH__ */







