#ifndef __TMUIROADEVAL_H__
#define __TMUIROADEVAL_H__

#include<TDataType.h>
#include<PHException.h>
#include<MUIOO.h>

//! The Muon id road finding evaluation object
/*! */

class TMuiRoadEval : public TObject
{
public:

  //! @name Constructors/Destructors
  //@{
  /*! Default constructor */
  TMuiRoadEval();

  /*! Destructor */
  virtual ~TMuiRoadEval(){;}

  //@}

  //! @name Global Evaluation
  //@{
  // getter
  //
  /*! Number of MC hits of the MC track */
  
  virtual UShort_t get_n_true_hits() const { 
    // count number of bits set to get total number of true hits	     
    //
    UShort_t nhits = 0;
    int limit = MUIOO::MAX_PLANE * MUIOO::MAX_ORIENTATION;
    for (int i = 0; i<limit; i++){
      nhits += (_n_true_hits & (1<<i)) >> i; 
    }
    return nhits;
  }

  virtual bool get_is_plane_true_hit(UShort_t iplane) const {
    return (_n_true_hits & 1 << iplane) != 0; 
  }

  virtual bool get_is_plane_masked_hit(UShort_t iplane) const {
    return (_n_masked_hits & 1 << iplane) != 0; 
  }

  virtual bool get_is_plane_reco_true_hit(UShort_t iplane) const {
    return (_n_reco_true_hits & 1 << iplane) != 0; 
  }

  virtual bool get_is_plane_reco_ghost_hit(UShort_t iplane) const {
    return (_n_reco_ghost_hits & 1 << iplane) != 0; 
  }

  virtual UShort_t get_panel_true_hits(int ipanel) const { 
    // count number of bits set to get total number of true hits	  
    //
    UShort_t nhits = 0;
    int limit = MUIOO::MAX_ORIENTATION;
    for (int i = 0; i<limit; i++){
      nhits += (_n_true_hits & (1<<(i + ipanel*limit))) >> (i + ipanel*limit); 
    }
    return nhits;
  }

  /*! Number of masked hits of the MC track */  
  virtual UShort_t get_n_masked_hits() const { 
    // count number of bits set to get total number of true hits	     
    //
    UShort_t nhits = 0;
    int limit = MUIOO::MAX_PLANE * MUIOO::MAX_ORIENTATION;
    for (int i = 0; i<limit; i++){
      nhits += (_n_masked_hits & (1<<i)) >> i; 
    }
    return nhits;
  }

  /*! The Ratio of masked hits out of true hits */
  virtual Float_t get_ratio_masked() const { 
    return difference(get_n_true_hits(),get_n_masked_hits());
  }

  /*! Number of hits from the primary contributor of the reco track */
  virtual UShort_t get_n_reco_true_hits() const { 
    // count number of bits set to get total number of true hits
    UShort_t nhits=0;
    int limit = MUIOO::MAX_PLANE * MUIOO::MAX_ORIENTATION;
    for (int i = 0; i<limit; i++){
      nhits += (_n_reco_true_hits & (1<<i)) >> i;
    }	
    return nhits;	
  }

  virtual UShort_t get_panel_reco_true_hits(int ipanel) const {
    // count number of bits set to get total number of true hits
    //
    UShort_t nhits = 0;
    int limit = MUIOO::MAX_ORIENTATION;
    for (int i = 0; i<limit; i++){
      nhits += (_n_reco_true_hits & (1<<(i + ipanel*limit))) >> (i + ipanel*limit);
    }
    return nhits;
  }
		  
  /*! Number of ghost hits of the reco track, ghost means that the track id 
    of this hit is different from the primary contributor of the reco track */
  virtual UShort_t get_n_reco_ghost_hits() const {
    // count number of bits set to get total number of true hits
    UShort_t nhits=0;
    int limit = MUIOO::MAX_PLANE * MUIOO::MAX_ORIENTATION;
    for (int i = 0; i<limit; i++){
      nhits += (_n_reco_ghost_hits & (1<<i)) >> i;
    }	
    return nhits;	
  }

  /*! Percentage of how many hits are ghosts in the reco track */
  virtual Float_t get_ratio_ghost() const { 
    return difference(get_n_reco_true_hits(), get_n_reco_ghost_hits());
  }

  //setter
  //
  /*! Number of MC hits of the MC track */
  virtual void set_n_true_hits(UShort_t n_true_hits) { _n_true_hits = n_true_hits;}
  /*! Number of masked hits of the MC track */  
  virtual void set_n_masked_hits(UShort_t n_masked_hits) { _n_masked_hits = n_masked_hits;}
  /*! Number of hits from the primary contributor of the reco track */
  virtual void set_n_reco_true_hits(UShort_t n_reco_true_hits) { _n_reco_true_hits = n_reco_true_hits;}
  /*! Number of ghost hits of the reco track, ghost means that the track id 
      of this hit is different from the primary contributor of the reco track */
  virtual void set_n_reco_ghost_hits(UShort_t n_reco_ghost_hits) { _n_reco_ghost_hits = n_reco_ghost_hits;}

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

  virtual Float_t difference(UShort_t a, UShort_t b) const {
    if(a!=0) {
      return (1.0*b)/(1.0*a);
    } else {
      return -999.0;
    }
  }

  // Global evalution varibles.
  //
  UShort_t _n_true_hits;
  UShort_t _n_masked_hits;
  UShort_t _n_reco_true_hits;
  UShort_t _n_reco_ghost_hits;

  // Momentum evaluation varibles at vertex.
  //
  Float_t _px_true_vx;
  Float_t _py_true_vx;
  Float_t _pz_true_vx;
  Float_t _ptot_true_vx;

  ClassDef(TMuiRoadEval,1)
};

#endif /* __TMUIROADEVAL_H__ */
