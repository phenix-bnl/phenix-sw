#ifndef __PDBMPCEDONLINEPEDESTALS_HH__
#define __PDBMPCEDONLINEPEDESTALS_HH__

#include <PdbCalChan.hh>
#include <phool.h>
#include <iostream>

class PdbMpcExOnlinePedestals : public PdbCalChan {

public:

  //! Internal indices for the 98304 channels in terms of the online coordinates
  enum { 
    NARMS = 2,
    NPACKETS = 8,
    NCHANNELS = 6144
  };

  //! Constructor
  PdbMpcExOnlinePedestals();

  //! Destructor
  virtual ~PdbMpcExOnlinePedestals() {}

  //! Override the print function
  void print() const;

  void Reset();

  //! set the pedestal of the given channel
  void set_pedestal(unsigned int arm, unsigned int packet, unsigned int channel, float pedestal){
    if(arm<NARMS && packet<NPACKETS && channel<NCHANNELS){
      _pedestal[arm][packet][channel] = pedestal;
      return;
    }
    std::cout<<PHWHERE<<" PdbMpcExOnlinePedestals::set_pedestal - array out of bounds, no updates"<<std::endl;
  }

  //! set the pedestal width of the given channel
  void set_pedestal_width(unsigned int arm, unsigned int packet, unsigned int channel, float width){
    if(arm<NARMS && packet<NPACKETS && channel<NCHANNELS){
      _pedestal_width[arm][packet][channel] = width;
      return;
    }
    std::cout<<PHWHERE<<" PdbMpcExOnlinePedestals::set_pedestal_width - array out of bounds, no updates"<<std::endl;
  }

  //! set the pedestal fit chi2 of the given channel
  void set_pedestal_chi2(unsigned int arm, unsigned int packet, unsigned int channel, float chi2){
    if(arm<NARMS && packet<NPACKETS && channel<NCHANNELS){
      _pedestal_chi2[arm][packet][channel] = chi2;
      return;
    }
    std::cout<<PHWHERE<<" PdbMpcExOnlinePedestals::set_pedestal_chi2 - array out of bounds, no updates"<<std::endl;
  }

  //! set the threshold (pedestal + n*width) of the given channel
  void set_threshold(unsigned int arm, unsigned int packet, unsigned int channel, short threshold){
    if(arm<NARMS && packet<NPACKETS && channel<NCHANNELS){
      _threshold[arm][packet][channel] = threshold;
      return;
    }
    std::cout<<PHWHERE<<" PdbMpcExOnlinePedestals::set_threshold - array out of bounds, no updates"<<std::endl;
  }

  //! set the flag indicating if these thresholds were applied in the DCM
  void set_applied_thresholds(short applied){
    _applied_thresholds = applied;
  }

  //! get the pedestal of a channel
  float get_pedestal(unsigned int arm, unsigned int packet, unsigned int channel){
    if(arm<NARMS && packet<NPACKETS && channel<NCHANNELS){
      return _pedestal[arm][packet][channel];
    }
    std::cout<<PHWHERE<<" PdbMpcExOnlinePedestals::get_pedestal - array out of bounds, returning junk"<<std::endl;
    return -9999.;
  }

  //! get the pedestal width of a channel
  float get_pedestal_width(unsigned int arm, unsigned int packet, unsigned int channel){
    if(arm<NARMS && packet<NPACKETS && channel<NCHANNELS){
      return _pedestal_width[arm][packet][channel];
    }
    std::cout<<PHWHERE<<" PdbMpcExOnlinePedestals::get_pedestal_width - array out of bounds, returning junk"<<std::endl;
    return -9999.;
  }

  //! get the pedestal fit chi2 of a channel
  float get_pedestal_chi2(unsigned int arm, unsigned int packet, unsigned int channel){
    if(arm<NARMS && packet<NPACKETS && channel<NCHANNELS){
      return _pedestal_chi2[arm][packet][channel];
    }
    std::cout<<PHWHERE<<" PdbMpcExOnlinePedestals::get_pedestal_chi2 - array out of bounds, returning junk"<<std::endl;
    return -9999.;
  }

  //! get the threshold (pedestal + n*width) of a channel
  short get_threshold(unsigned int arm, unsigned int packet, unsigned int channel){
    if(arm<NARMS && packet<NPACKETS && channel<NCHANNELS){
      return _threshold[arm][packet][channel];
    }
    std::cout<<PHWHERE<<" PdbMpcExOnlinePedestals::get_threshold - array out of bounds, returning junk"<<std::endl;
    return -9999;
  }

  //! get the status if the thresholds were applied in the DCM
  short get_applied_thresholds() {
    return _applied_thresholds;
  }

private:

  //! The pedestal of the given channel
  float _pedestal[NARMS][NPACKETS][NCHANNELS];
  //! The pedestal width of the given channel
  float _pedestal_width[NARMS][NPACKETS][NCHANNELS];
  //! The pedestal fit chi2 of the given channel
  float _pedestal_chi2[NARMS][NPACKETS][NCHANNELS];
  //! The zero suppression threshold for the channel
  short _threshold[NARMS][NPACKETS][NCHANNELS];
  //! Single number indicating whether these thresholds were used in the DCM
  short _applied_thresholds;

  ClassDef(PdbMpcExOnlinePedestals,1)

};

#endif /* __PDBMPCEDONLINEPEDESTALS_HH__ */
