// $Id: mMutZeroSupPar.h,v 1.10 2013/04/25 15:33:38 brooks Exp $
#ifndef __MMUTZEROSUPPAR_HH__
#define __MMUTZEROSUPPAR_HH__

#include <PHException.h>
#include <PHObject.h>
#include <TMutParBase.h>
#include <boost/array.hpp>

//!Runtime parameter object for mMutZeroSup module
class mMutZeroSupPar : public TMutParBase
{

 public: 

  /*! default constructor */
  mMutZeroSupPar() : 
    _mode( SHAPE | DCM | BADCHAN ),
    //_mode(  DCM | BADCHAN ),
    //_mode(  TEST | BADCHAN ),
    _thresh_1( 3.0 ),
    _thresh_2(0.0),
    _adc13_lo(0.0),
    _adc13_hi(1.1),
    _adc23_lo(-2.0),
    _adc23_hi(0.9), 
    _adc43_lo(-2.0),
    _adc43_hi(0.9)
    {
      
      // initialize thresholds [v1]
      _mc_thresh_1.assign(-10);
      _mc_thresh_1[0] = 3.0;
      _mc_thresh_1[1] = 3.0;
      _mc_thresh_1[2] = 3.0; 
      _mc_thresh_1[3] = 3.0; 
      _mc_thresh_1[4] = 3.0;
      _mc_thresh_1[5] = 3.0;
  
      // initialize thresholds [v2]
      _mc_thresh_2.assign(0.0);    
    
    }
  
  /*! destructor */
  ~mMutZeroSupPar()
  {;}

  /*! Zero suppression mode */
  enum Mode {
    NONE = 0,                     //!< no suppression
    MC_IDMUTC_DCM1 = (1<<0),      //!< first monte carlo zero suppression mode
    MC_IDMUTC_FPGA0SUP = (1<<1),  //!< second monte carlo zero suppression mode
    DCM = (1<<2),                 //!< first RD suppression mode
    SHAPE = (1<<3),                //!< second RD suppression mode
    BADCHAN = (1<<4),             //!< suppress bad channels?
    TEST = (1<<5)                 //!< zero suppression tests
  };

  /*! Zero suppression mode enumerated in Mode */
  ULong_t get_mode() const 
  {return _mode;} 

  /*! Zero suppression threshold */
  double get_thresh_1() const 
  {return _thresh_1;} 
  
  /*! Zero suppression threshold */
  double get_thresh_2() const 
  {return _thresh_2;} 
  
  /*! MC Zero suppression threshold */
  double get_mc_thresh_1(unsigned int arm, unsigned int station) const 
  {
    unsigned int index = 2*station+arm;
    BOUNDS_CHECK(index,THRESH_SIZE);
    return _mc_thresh_1[index];
  } 
  
  /*! MC Zero suppression threshold */
  double get_mc_thresh_2(unsigned int arm, unsigned int station) const 
  {
    unsigned int index = 2*station+arm;
    BOUNDS_CHECK(index,THRESH_SIZE);
    return _mc_thresh_2[index];
  } 

  /*! Zero suppression threshold*/
  void  set_mc_thresh_1(unsigned int arm, unsigned int station, double val)
  {
    unsigned int index = 2*station+arm;
    BOUNDS_CHECK(index,THRESH_SIZE);
    _mc_thresh_1[index] = val;
  } 

  /*! Zero suppression threshold*/
  void  set_mc_thresh_2(unsigned int arm, unsigned int station, double val)
  {
    unsigned int index = 2*station+arm;
    BOUNDS_CHECK(index,THRESH_SIZE);
    _mc_thresh_2[index] = val;
  } 

  /*! Zero suppression mode enumerated in Mode */
  void  set_mode(ULong_t mode)
  {_mode = mode;} 

  /*! Zero suppression threshold*/
  void  set_thresh_1(double val)
  {_thresh_1 = val;} 

  /*! Zero suppression threshold*/
  void  set_thresh_2(double val)
  {_thresh_2 = val;} 

  /*! Parameters for pulse-shape zero suppression */
  double get_adc13_lo() const 
  {return _adc13_lo;}
  
  double get_adc13_hi() const 
  {return _adc13_hi;}
  
  double get_adc23_lo() const 
  {return _adc23_lo;}
  
  double get_adc23_hi() const 
  {return _adc23_hi;}
  
  double get_adc43_lo() const 
  {return _adc43_lo;}
  
  double get_adc43_hi() const 
  {return _adc43_hi;}

  void  set_adc13_lo(double val)
  {_adc13_lo = val;}
  
  void  set_adc13_hi(double val)
  {_adc13_hi = val;}
  
  void  set_adc23_lo(double val)
  {_adc23_lo = val;}
  
  void  set_adc23_hi(double val)
  {_adc23_hi = val;}
  
  void  set_adc43_lo(double val)
  {_adc43_lo = val;}
  
  void  set_adc43_hi(double val)
  {_adc43_hi = val;}

  //! dump all parameters	
  void print( std::ostream& out = std::cout ) const
  {
    MUTOO::PRINT( out, "mMutZeroSupPar" );
    out << "_verbosity = " << _verbosity << std::endl;;
    out << "_mode = " << _mode << std::endl;;
    out << "_thresh_1 = " << _thresh_1 << " adc" << std::endl;;
    out << "_thresh_2 = " << _thresh_2 << " adc" << std::endl;;

    out << "_mc_thresh_1 = [ ";
    for( unsigned int i=0; i<THRESH_SIZE; i++ ) 
    { out << _mc_thresh_1[i] << " "; }
    out << "]" << std::endl;
  
    out << "_mc_thresh_2 = [ ";
    for( unsigned int i=0; i<THRESH_SIZE; i++ ) 
    { out << _mc_thresh_2[i] << " "; }
    out << "]" << std::endl;

    out << "_adc13_lo = " << _adc13_lo << " adc" << std::endl;
    out << "_adc13_hi = " << _adc13_hi << " adc" << std::endl;
    out << "_adc23_lo = " << _adc23_lo << " adc" << std::endl;
    out << "_adc23_hi = " << _adc23_hi << " adc" << std::endl;
    out << "_adc43_lo = " << _adc43_lo << " adc" << std::endl;
    out << "_adc43_hi = " << _adc43_hi << " adc" << std::endl;
    MUTOO::PRINT( out, "**" );
  }
  
 private:  

  enum {THRESH_SIZE=6};
  
  //! zero suppression threshold [v1]
  boost::array<double,THRESH_SIZE> _mc_thresh_1;

  //! zero suppression threshold [v2]
  boost::array<double,THRESH_SIZE> _mc_thresh_2;

  ULong_t _mode;
  double _thresh_1;
  double _thresh_2;
  double _adc13_lo;
  double _adc13_hi;
  double _adc23_lo;
  double _adc23_hi;
  double _adc43_lo;
  double _adc43_hi;

};

#endif /* __MMUTUNPACK_HH__ */







