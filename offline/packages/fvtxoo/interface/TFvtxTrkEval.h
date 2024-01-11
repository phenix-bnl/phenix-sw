#ifndef __TFvtxTrkEval_H__
#define __TFvtxTrkEval_H__

#include <iostream>
#include <TDataType.h>
#include <PHException.h>

//! The Muon tracker track finding evaluation object
/*! */

class TFvtxTrkEval : public TObject
{
public:

  //! @name Constructors/Destructors
  //@{
  /*! Default constructor */
  TFvtxTrkEval();

  /*! Destructor */
  virtual ~TFvtxTrkEval(){;}

  //@}

  //! @name Global Evaluation
  //@{
  // getter
  //
  /*! Number of MC hits of the MC track */
  
  virtual unsigned short get_n_total_true_hits() const { 

    // count number of bits set to get total number of true hits	  
    //
    unsigned short nhits = 0;
    for (int i = 0; i<16; i++){
      nhits += (_n_total_true_hits & (1<<i)) >> i; 
    }
    return nhits;
  }

  virtual unsigned short get_station_true_hits(int istation) const { 

    // count number of bits set to get total number of true hits	  
    //
    unsigned short nhits = 0;
    for (int i = 0; i<6; i++){
      nhits += (_n_total_true_hits & (1<<(i + istation*6))) >> (i + istation*6); 
    }
    return nhits;
  }

  virtual unsigned short get_station_true_gaps(int istation) const {

    // count number of bits set to get total number of true hits
    //
    unsigned short ngaps = 0;
    for (int j = 0; j<3; j++){
      unsigned short nhits = 0;
      for (int i = 0; i<2; i++){
        nhits += (_n_total_true_hits & (1<<(i + 2*j + istation*6))) >>
          (i + 2*j + istation*6);
      }
      if (nhits==2) ngaps++;
    }
    return ngaps;
  }


  /*! Number of masked hits of the MC track */  
  virtual unsigned short get_n_masked_true_hits() const { return _n_masked_true_hits;}
  /*! The Ratio of masked hits out of true hits */
  virtual Float_t get_ratio_masked() const { 
    return difference(_n_total_true_hits,_n_masked_true_hits);
  }

  /*! Number of hits from the primary contributor of the reco track */
  virtual unsigned short get_n_reco_true_hits() const { 
    // count number of bits set to get total number of true hits
    unsigned short nhits=0;
    for (int i = 0; i<16; i++){
      nhits += (_n_reco_true_hits & (1<<i)) >> i;
    }	
    return nhits;	
  }

  virtual unsigned short get_station_reco_true_hits(int istation) const {

    // count number of bits set to get total number of true hits
    //
    unsigned short nhits = 0;
    for (int i = 0; i<6; i++){
      nhits += (_n_reco_true_hits & (1<<(i + istation*6))) >> (i + istation*6);
    }
    return nhits;
  }
		  

  /*! Number of ghost hits of the reco track, ghost means that the track id 
    of this hit is different from the primary contributor of the reco track */
  virtual unsigned short get_n_reco_ghost_hits() const { return _n_reco_ghost_hits;}

  /*! Percentage of how many hits are ghosts in the reco track */
  virtual Float_t get_ratio_ghost() const { 
    return difference(_n_reco_true_hits,_n_reco_ghost_hits);
  }

  /*! Number of ghost hits of the reco track, ghost means that the track id 
    of this hit is different from the primary contributor of the reco track */
  virtual unsigned short get_best_track() const { return _best_track;}

  /*! True charge */
  virtual void set_charge_true(Float_t charge_true) { _charge_true = charge_true;}

  /*! True charge */
  virtual Float_t get_charge_true() const { 
    return _charge_true;
  }

  /*! Reconstructed charge */
  virtual void set_charge_reco(Float_t charge_reco) { _charge_reco = charge_reco;}

  /*! Reconstructed charge */
  virtual Float_t get_charge_reco() const { 
    return _charge_reco;
  }

  //setter
  //
  /*! Number of MC hits of the MC track */
  virtual void set_n_total_true_hits(unsigned short n_total_true_hits) { _n_total_true_hits = n_total_true_hits;}
  /*! Number of masked hits of the MC track */  
  virtual void set_n_masked_true_hits(unsigned short n_masked_true_hits) { _n_masked_true_hits = n_masked_true_hits;}
  /*! Number of hits from the primary contributor of the reco track */
  virtual void set_n_reco_true_hits(unsigned short n_reco_true_hits) { _n_reco_true_hits = n_reco_true_hits;}
  /*! Number of ghost hits of the reco track, ghost means that the track id 
    of this hit is different from the primary contributor of the reco track */
  virtual void set_n_reco_ghost_hits(unsigned short n_reco_ghost_hits) { _n_reco_ghost_hits = n_reco_ghost_hits;}

  /*! Track which has the best match to the MC track */
  virtual void set_best_track(unsigned short best_track) { _best_track = best_track;}

  //@}

  //! @name Primary Vertex
  //@{
  //getter
  //
  /*! Momentum in x-direction of MC track */
  virtual Float_t get_px_true_vx() const { return _px_true_vx;}
  /*! Momentum in y-direction of MC track */
  virtual Float_t get_py_true_vx() const { return _py_true_vx;}
  /*! Momentum in z-direction of MC track */
  virtual Float_t get_pz_true_vx() const { return _pz_true_vx;}
  /*! Total momentum of MC track */
  virtual Float_t get_ptot_true_vx() const { return _ptot_true_vx;}
  /*! Momentum in x-direction of reconstructed track */
  virtual Float_t get_px_reco_vx() const { return _px_reco_vx;}
  /*! Momentum in y-direction of reconstructed track */
  virtual Float_t get_py_reco_vx() const  { return _py_reco_vx;}
  /*! Momentum in z-direction of reconstructed track */
  virtual Float_t get_pz_reco_vx() const { return _pz_reco_vx;}
  /*! Total momentum of reconstructed track */
  virtual Float_t get_ptot_reco_vx() const { return _ptot_reco_vx;}
  /*! dp/p */
  virtual Float_t get_delta_px_vx() const { return difference(_px_true_vx,_px_reco_vx);}
  /*! dp/p */
  virtual Float_t get_delta_py_vx() const { return difference(_py_true_vx,_py_reco_vx);}
  /*! dp/p */
  virtual Float_t get_delta_pz_vx() const { return difference(_pz_true_vx,_pz_reco_vx);}
  /*! dp/p */
  virtual Float_t get_delta_ptot_vx() const { return difference(_ptot_true_vx,_ptot_reco_vx);}

  //setter
  //
  /*! Momentum in x-direction of MC track */
  virtual void set_px_true_vx(Float_t px_true) { _px_true_vx = px_true;}
  /*! Momentum in y-direction of MC track */
  virtual void set_py_true_vx(Float_t py_true) { _py_true_vx = py_true;}
  /*! Momentum in z-direction of MC track */
  virtual void set_pz_true_vx(Float_t pz_true) { _pz_true_vx = pz_true;}
  /*! Total momentum of MC track */
  virtual void set_ptot_true_vx(Float_t ptot_true) { _ptot_true_vx = ptot_true;}
  /*! Momentum in x-direction of reconstructed track */
  virtual void set_px_reco_vx(Float_t px_reco) { _px_reco_vx = px_reco;}
  /*! Momentum in y-direction of reconstructed track */
  virtual void set_py_reco_vx(Float_t py_reco) { _py_reco_vx = py_reco;}
  /*! Momentum in z-direction of reconstructed track */
  virtual void set_pz_reco_vx(Float_t pz_reco) { _pz_reco_vx = pz_reco;}
  /*! Total momentum of reconstructed track */
  virtual void set_ptot_reco_vx(Float_t ptot_reco) { _ptot_reco_vx = ptot_reco;}

  //@}

  //! @name Upstream Gap
  //@{
  // getter
  //
  /*! Momentum in x-direction of MC track */
  virtual Float_t get_px_true_us() const { return _px_true_us;}
  /*! Momentum in y-direction of MC track */
  virtual Float_t get_py_true_us() const { return _py_true_us;}
  /*! Momentum in z-direction of MC track */
  virtual Float_t get_pz_true_us() const { return _pz_true_us;}
  /*! Total momentum of MC track */
  virtual Float_t get_ptot_true_us() const { return _ptot_true_us;}
  /*! Momentum in x-direction of reconstructed track */
  virtual Float_t get_px_reco_us() const { return _px_reco_us;}
  /*! Momentum in y-direction of reconstructed track */
  virtual Float_t get_py_reco_us()const { return _py_reco_us;}
  /*! Momentum in z-direction of reconstructed track */
  virtual Float_t get_pz_reco_us() const { return _pz_reco_us;}
  /*! Total momentum of reconstructed track */
  virtual Float_t get_ptot_reco_us() const { return _ptot_reco_us;}
  /*! dp/p */
  virtual Float_t get_delta_px_us() const { return difference(_px_true_us,_px_reco_us);}
  /*! dp/p */
  virtual Float_t get_delta_py_us() const { return difference(_py_true_us,_py_reco_us);}
  /*! dp/p */
  virtual Float_t get_delta_pz_us() const { return difference(_pz_true_us,_pz_reco_us);}
  /*! dp/p */
  virtual Float_t get_delta_ptot_us() const { return difference(_ptot_true_us,_ptot_reco_us);}

  // setter
  //
  /*! Momentum in x-direction of MC track */
  virtual void set_px_true_us(Float_t px_true) { _px_true_us = px_true;}
  /*! Momentum in y-direction of MC track */
  virtual void set_py_true_us(Float_t py_true) { _py_true_us = py_true;}
  /*! Momentum in z-direction of MC track */
  virtual void set_pz_true_us(Float_t pz_true) { _pz_true_us = pz_true;}
  /*! Total momentum of MC track */
  virtual void set_ptot_true_us(Float_t ptot_true) { _ptot_true_us = ptot_true;}
  /*! Momentum in x-direction of reconstructed track */
  virtual void set_px_reco_us(Float_t px_reco) { _px_reco_us = px_reco;}
  /*! Momentum in y-direction of reconstructed track */
  virtual void set_py_reco_us(Float_t py_reco) { _py_reco_us = py_reco;}
  /*! Momentum in z-direction of reconstructed track */
  virtual void set_pz_reco_us(Float_t pz_reco) { _pz_reco_us = pz_reco;}
  /*! Total momentum of reconstructed track */
  virtual void set_ptot_reco_us(Float_t ptot_reco) { _ptot_reco_us = ptot_reco;}  
  //@}

  //! @name Downstream Gap
  //@{
  //getter
  //
  /*! Momentum in x-direction of MC track */
  virtual Float_t get_px_true_ds() const { return _px_true_ds;}
  /*! Momentum in y-direction of MC track */
  virtual Float_t get_py_true_ds() const { return _py_true_ds;}
  /*! Momentum in z-direction of MC track */
  virtual Float_t get_pz_true_ds() const { return _pz_true_ds;}
  /*! Total momentum of MC track */
  virtual Float_t get_ptot_true_ds() const { return _ptot_true_ds;}
  /*! Momentum in x-direction of reconstructed track */
  virtual Float_t get_px_reco_ds() const { return _px_reco_ds;}
  /*! Momentum in y-direction of reconstructed track */
  virtual Float_t get_py_reco_ds() const { return _py_reco_ds;}
  /*! Momentum in z-direction of reconstructed track */
  virtual Float_t get_pz_reco_ds() const { return _pz_reco_ds;}
  /*! Total momentum of reconstructed track */
  virtual Float_t get_ptot_reco_ds() const { return _ptot_reco_ds;}
  /*! dp/p */
  virtual Float_t get_delta_px_ds() const { return difference(_px_true_ds,_px_reco_ds);}
  /*! dp/p */
  virtual Float_t get_delta_py_ds() const { return difference(_py_true_ds,_py_reco_ds);}
  /*! dp/p */
  virtual Float_t get_delta_pz_ds() const { return difference(_pz_true_ds,_pz_reco_ds);}
  /*! dp/p */
  virtual Float_t get_delta_ptot_ds() const { return difference(_ptot_true_ds,_ptot_reco_ds);}
  
  //setter
  //
  /*! Momentum in x-direction of MC track */
  virtual void set_px_true_ds(Float_t px_true) { _px_true_ds = px_true;}
  /*! Momentum in y-direction of MC track */
  virtual void set_py_true_ds(Float_t py_true) { _py_true_ds = py_true;}
  /*! Momentum in z-direction of MC track */
  virtual void set_pz_true_ds(Float_t pz_true) { _pz_true_ds = pz_true;}
  /*! Total momentum of MC track */
  virtual void set_ptot_true_ds(Float_t ptot_true) { _ptot_true_ds = ptot_true;}
  /*! Momentum in x-direction of reconstructed track */
  virtual void set_px_reco_ds(Float_t px_reco) { _px_reco_ds = px_reco;}
  /*! Momentum in y-direction of reconstructed track */
  virtual void set_py_reco_ds(Float_t py_reco) { _py_reco_ds = py_reco;}
  /*! Momentum in z-direction of reconstructed track */
  virtual void set_pz_reco_ds(Float_t pz_reco) { _pz_reco_ds = pz_reco;}
  /*! Total momentum of reconstructed track */
  virtual void set_ptot_reco_ds(Float_t ptot_reco) { _ptot_reco_ds = ptot_reco;}
  //@}

  //! @name Dumpers
  //@{    
  virtual void print(std::ostream& os = std::cout) const;
  //@}

private:

  virtual Float_t difference(Float_t a, Float_t b) const {
    if(a!=0) {
      return (b-a)/a;
    } else {
      return -999.0;
    }
  }

  virtual Float_t difference(unsigned short a, unsigned short b) const {
    if(a!=0) {
      return (1.0*b)/(1.0*a);
    } else {
      return -999.0;
    }
  }

  // Global evalution varibles.
  //
  unsigned short _n_total_true_hits;
  unsigned short _n_masked_true_hits;
  unsigned short _n_reco_true_hits;
  unsigned short _n_reco_ghost_hits;
  unsigned short _best_track;   // flag to indicate this is the best match to the true track
  Float_t _charge_true;
  Float_t _charge_reco;

  // Momentum evaluation varibles at vertex.
  //
  Float_t _px_true_vx;
  Float_t _py_true_vx;
  Float_t _pz_true_vx;
  Float_t _ptot_true_vx;
  Float_t _px_reco_vx;
  Float_t _py_reco_vx;
  Float_t _pz_reco_vx;
  Float_t _ptot_reco_vx;

  // Momentum evaluation variables for up stream point.
  // 
  Float_t _px_true_us;
  Float_t _py_true_us;
  Float_t _pz_true_us;
  Float_t _ptot_true_us;
  Float_t _px_reco_us;
  Float_t _py_reco_us;
  Float_t _pz_reco_us;
  Float_t _ptot_reco_us;

  // Momentum evaluation variable at down stream point.
  //
  Float_t _px_true_ds;
  Float_t _py_true_ds;
  Float_t _pz_true_ds;
  Float_t _ptot_true_ds;
  Float_t _px_reco_ds;
  Float_t _py_reco_ds;
  Float_t _pz_reco_ds;
  Float_t _ptot_reco_ds;

  ClassDef(TFvtxTrkEval,1)
};

#endif /* __TFvtxTrkEval_H__ */













