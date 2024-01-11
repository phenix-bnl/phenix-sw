#ifndef __TMPCEXHIT_H__
#define __TMPCEXHIT_H__

/**
 * @class  TMpcExHit
 * @author ngrau@augie.edu
 * @date   July 2015
 * @brief  The in-memory hit that knows if it is raw, calibrated, etc.
 */

class TMpcExHit {

 public:

  enum State {
    UNKNOWN = 0,
    ADC,
    PEDESTAL_SUBTRACTED,
    PEDESTAL_AND_CMN_SUBTRACTED,
    GAIN_CALIBRATED
  }; 

  enum Status {
    ADC_ZERO_SUPPRESSED = (1u << 0),
    BAD_PEDESTAL_CALIBRATION  = (1u << 1),
    DEAD_HOT = (1u << 2),
    LOW_RANGE_LARGE_ADC_ONLY = (1u << 3),
    BAD_GAIN_CALIBRATION = (1u << 4),
    FAILS_SIGMA_CUT = (1u << 5)
  };

  // All BAD values first (below VALID), 
  // All OK values above VALID
  enum CombinedState {
    CS_UNKNOWN = 0,
    INVALID = 1,
    VALID_LOW_EXPECTED_BOTH = 2,
    VALID_HIGH_EXPECTED_BOTH = 3, 
    VALID = 4,
    VALID_LOW_ONLY = 5,
    VALID_HIGH_ONLY = 6
  }; 

  //! Construct with the key
  TMpcExHit(unsigned int key);

  //! Destructor
  virtual ~TMpcExHit() {}

  //! Reset the hit to its default values at construction
  void Reset();

  //! set the low gain value
  void set_low(const float low) { _low = low; }

  //! set the high gain value
  void set_high(const float high) { _high = high; }

  //! set the combined value
  void set_combined(const float combined) { _combined = combined; }

  //! set the status of the hit
  void set_status_low(unsigned int status) { _status_low |= status; }
  void set_status_high(unsigned int status) { _status_high |= status; }

  //! set the state of the hit
  void set_state_low(const State state) { _state_low = state; }
  void set_state_high(const State state) { _state_high = state; }
  void set_state_combined(const CombinedState state) { _state_combined = state; }

  //! create a clone of this hit, caller owns the pointer.
  TMpcExHit* clone() const {
    TMpcExHit *hit = new TMpcExHit(_key);
    hit->set_low(_low);
    hit->set_high(_high);
    hit->set_combined(_combined);
    hit->set_status_low(_status_low);
    hit->set_status_high(_status_high);
    hit->set_state_low(_state_low);
    hit->set_state_high(_state_high);
    hit->set_state_combined(_state_combined);
    return hit;
  }

  //! the key
  unsigned int key() const { return _key; }

  //! the low gain value
  float low() const { return _low; }

  //! the high gain value
  float high() const { return _high; }

  //! the combined energy value
  float combined() const { return _combined; }

  //! the status of the hit, flags indicating various issues
  unsigned int status_low() const { return _status_low; }
  unsigned int status_high() const { return _status_high; }

  //! the state of the hit, i.e. raw adc, pedestal subtracted, etc.
  unsigned int state_low() const { return _state_low; }
  unsigned int state_high() const { return _state_high; }
  unsigned int state_combined() const { return _state_combined; }

  //! is this a good hit?		       
  bool isGoodLowHit() const { return (_status_low==0); } 
  bool isGoodHighHit() const { return (_status_high==0); } 

  //! is this a good *gain calibrated* hit?		       
  bool isGoodGCLowHit() const { return ((_state_low==TMpcExHit::GAIN_CALIBRATED)&&(_status_low==0)); } 
  bool isGoodGCLowHitHighRange() const { return ((_state_low==TMpcExHit::GAIN_CALIBRATED)&&
						 (_status_low==TMpcExHit::LOW_RANGE_LARGE_ADC_ONLY)); } 
  bool isGoodGCHighHit() const { return ((_state_high==TMpcExHit::GAIN_CALIBRATED)&&(_status_high==0)); } 
  bool isGoodCombinedHit() const {return (_state_combined>=TMpcExHit::VALID); }

  //! the arm this hit is in
  unsigned short arm() const;

  //! the packet index within the arm of this hit
  unsigned short packet() const;

  //! the chipmap index of this hit
  unsigned short chipmap() const;

  //! the chain within the packet of this hit
  unsigned short chain() const;

  //! the chip within the chain of this hit
  unsigned short chip() const;

  //! the minipad rocbond within the chip of this hit
  unsigned short rocbond() const;

  //! the global x position of the hit
  float x() const;

  //! the global y position of the hit
  float y() const;

  //! the global z position of the hit
  float z() const;

  //! the hough slope in x for a hit given collision vertex
  float hsx(float z_vertex) const;

  //! the hough slope in y for a hit given collision vertex
  float hsy(float z_vertex) const;

  //! the global layer position of the hit
  unsigned short layer() const;

  //! the quadrant (0-3) of the hit
  unsigned short quadrant() const;

  //! the sensor (0-5) in the quadrant position of the hit
  unsigned short sensor_in_quadrant() const;

  //! the local x index (0-31) of the hit
  unsigned short lx() const;

  //! the local y index (0-3) of the hit
  unsigned short ly() const;

  //! get the minipad width in the x direction 
  float minipad_x_width() const;

  //! get the minipad width in the y direction 
  float minipad_y_width() const;

 private:

  //! key of the minipad
  unsigned int _key;

  //! low-gain value
  float _low;

  //! high-gain value
  float _high;

  //! combined energy value
  float _combined; 

  //! the status: flags indicating various issues
  unsigned int _status_low;
  unsigned int _status_high;

  //! the state: raw adc, pedestal subtracted, gain calibrated, etc.
  State _state_low;
  State _state_high;
  CombinedState _state_combined; 

  //! local LUT information from the MpcExMapper
  float _x;
  float _y;
  float _z;
  unsigned short _layer;
  float _x_width; 
  float _y_width; 
};

#endif /* __TMPCEXHIT_H__ */
