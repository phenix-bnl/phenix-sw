#ifndef __TMUTEVALRES_H__
#define __TMUTEVALRES_H__

#include<TDataType.h>
#include<PHException.h>

//! TMutEvalRes object stores evauation data for each hit in a track.

/*! 
  For each chamber, you have a set of reco values, such as 
  momentum and the coordinate of cross point ( the intersection 
  point of track with the chamber plane. You definitely want to 
  compare all the values with your MC input.
  On the other hand each chamber has its own geometry, such as the 
  angle between anode wire and cathode strip as well as the angle 
  between anode wires and beam direction. They are relavent to the 
  resolution.
  There are two pairs of residules much more important to look at.
  Here we call the ( w_true, w_track) and (w_digit, w_meas). First 
  pair is the pair of distances from where MC track/reco track 
  intersection with detector plane to anode wire, abs(w_true-w_track)
  is a good evaluator of Track Fit code. 
*/

class TMutEvalRes : public TObject 
{
public:
  
  //! @name Constructors/Destructors
  //@{

  /*! Default constructor */
  TMutEvalRes();

  /*! Virtual destructor */
  virtual ~TMutEvalRes(){;}

  //@}

  //! @name Truth Data
  //@{
  /*! x coordinate of where MC track intersects with detector plane */
  virtual Float_t get_x_true() const { return _x_true;}
  /*! y coordinate of where MC track intersects with detector plane */
  virtual Float_t get_y_true() const { return _y_true;}
  /*! z coordinate of where MC track intersects with detector plane */
  virtual Float_t get_z_true() const { return _z_true;}
  /*! theta coordinate of where MC track intersects with detector plane */
  virtual Float_t get_theta_true() const { return _theta_true;}
  /*! phi coordinate of where MC track intersects with detector plane */
  virtual Float_t get_phi_true() const { return _phi_true;}
  /*! px of MC track when it intersects with detector plane */
  virtual Float_t get_px_true() const { return _px_true;}
  /*! py of MC track when it intersects with detector plane */
  virtual Float_t get_py_true() const { return _py_true;}
  /*! pz of MC track when it intersects with detector plane */
  virtual Float_t get_pz_true() const { return _pz_true;}

  /*! x coordinate of where MC track intersects with detector plane */
  virtual void set_x_true(Float_t x_true) { _x_true=x_true;}
  /*! y coordinate of where MC track intersects with detector plane */
  virtual void set_y_true(Float_t y_true) { _y_true=y_true;}
  /*! z coordinate of where MC track intersects with detector plane */
  virtual void set_z_true(Float_t z_true) { _z_true=z_true;}
  /*! theta coordinate of where MC track intersects with detector plane */
  virtual void set_theta_true(Float_t theta_true) { _theta_true=theta_true;}
  /*! phi coordinate of where MC track intersects with detector plane */
  virtual void set_phi_true(Float_t phi_true) { _phi_true=phi_true;}
  /*! px of MC track when it intersects with detector plane */
  virtual void set_px_true(Float_t px_true) { _px_true=px_true;}
  /*! py of MC track when it intersects with detector plane */
  virtual void set_py_true(Float_t py_true) { _py_true=py_true;}
  /*! pz of MC track when it intersects with detector plane */
  virtual void set_pz_true(Float_t pz_true) { _pz_true=pz_true;}
  //@}

  //! @name Reconstructed Data
  //@{
  /*! x coordinate of where reco track intersects with detector plane */
  virtual Float_t get_x_reco() const { return _x_reco;}
  /*! y coordinate of where reco track intersects with detector plane */
  virtual Float_t get_y_reco() const { return _y_reco;}
  /*! z coordinate of where reco track intersects with detector plane */
  virtual Float_t get_z_reco() const { return _z_reco;}
  /*! theta coordinate of where reco track intersects with detector plane */
  virtual Float_t get_theta_reco() const { return _theta_reco;}
  /*! phi coordinate of where reco track intersects with detector plane */
  virtual Float_t get_phi_reco() const { return _phi_reco;}
  /* px of reco track when it intersects with detector plane */
  virtual Float_t get_px_reco() const { return _px_reco;}
  /* py of reco track when it intersects with detector plane */
  virtual Float_t get_py_reco() const { return _py_reco;}
  /* pz of reco track when it intersects with detector plane */
  virtual Float_t get_pz_reco() const { return _pz_reco;}
  /*! Difference between x_true and x_reco. */
  virtual Float_t get_delta_x() const { return difference(_x_true,_x_reco);}
  /*! Difference between y_true and y_reco. */
  virtual Float_t get_delta_y() const { return difference(_y_true,_y_reco);}
  /*! Difference between z_true and z_reco. */
  virtual Float_t get_delta_z() const { return difference(_z_true,_z_reco);}
  /*! x coordinate of where reco track intersects with detector plane */
  virtual void set_x_reco(Float_t x_reco) { _x_reco=x_reco;}
  /*! y coordinate of where reco track intersects with detector plane */
  virtual void set_y_reco(Float_t y_reco) { _y_reco=y_reco;}
  /*! z coordinate of where reco track intersects with detector plane */
  virtual void set_z_reco(Float_t z_reco) { _z_reco=z_reco;}
  /*! theta coordinate of where reco track intersects with detector plane */
  virtual void set_theta_reco(Float_t theta_reco) { _theta_reco=theta_reco;}
  /*! phi coordinate of where reco track intersects with detector plane */
  virtual void set_phi_reco(Float_t phi_reco) { _phi_reco=phi_reco;}
  /*! px of reco track when it intersects with detector plane */
  virtual void set_px_reco(Float_t px_reco) { _px_reco=px_reco;}
  /*! py of reco track when it intersects with detector plane */
  virtual void set_py_reco(Float_t py_reco) { _py_reco=py_reco;}
  /*! pz of reco track when it intersects with detector plane */
  virtual void set_pz_reco(Float_t pz_reco) { _pz_reco=pz_reco;}

  //@}

  //! @name Cathode Local Data
  //@{

  virtual Float_t get_w_true() const { return _w_true;}
  virtual Float_t get_w_track() const { return _w_track;}
  virtual Float_t get_w_digit() const { return _w_digit;}
  virtual Float_t get_w_meas() const { return _w_meas;}
  virtual Float_t get_theta_ac() const { return _theta_ac;}
  virtual Float_t get_theta_wz() const { return _theta_wz;}
  virtual void set_w_true(Float_t w_true) { _w_true=w_true;}
  virtual void set_w_track(Float_t w_track) { _w_track=w_track;}
  virtual void set_w_digit(Float_t w_digit) { _w_digit=w_digit;}
  virtual void set_w_meas(Float_t w_meas) { _w_meas=w_meas;}
  virtual void set_theta_ac(Float_t theta_ac) { _theta_ac=theta_ac;}
  virtual void set_theta_wz(Float_t theta_wz) { _theta_wz=theta_wz;}

  //@}

  //! @name Locators
  //@{
  virtual UShort_t get_arm() const { return _arm;}
  virtual UShort_t get_station() const { return _station;}
  virtual UShort_t get_octant() const { return _octant;}
  virtual UShort_t get_half_octant() const { return _half_octant;}
  virtual UShort_t get_gap() const { return _gap;}
  virtual UShort_t get_cathode() const { return _cathode;}
  virtual void set_arm(UShort_t arm) { _arm=arm;}
  virtual void set_station(UShort_t station) { _station=station;}
  virtual void set_octant(UShort_t octant) { _octant=octant;}
  virtual void set_half_octant(UShort_t half_octant) { _half_octant=half_octant;}
  virtual void set_gap(UShort_t gap) { _gap=gap;}
  virtual void set_cathode(UShort_t cathode) { _cathode=cathode;}

  //@}

private:

  // A method to calculate the difference of two varibles. 
  //
  virtual Float_t difference(Float_t a, Float_t b) const {
    if(a!=0) {
      return (b-a)/a;
    } else {
      return -999.0;
    }
  }
  
  Float_t _x_true;
  Float_t _y_true;
  Float_t _z_true;
  Float_t _theta_true;
  Float_t _phi_true;

  Float_t _x_reco;
  Float_t _y_reco;
  Float_t _z_reco;
  Float_t _theta_reco;
  Float_t _phi_reco;

  Float_t _px_true;
  Float_t _py_true;
  Float_t _pz_true;

  Float_t _px_reco;
  Float_t _py_reco;
  Float_t _pz_reco;

  Float_t _w_track;
  Float_t _w_meas;
  Float_t _w_true;
  Float_t _w_digit;
  Float_t _theta_ac;
  Float_t _theta_wz;

  UShort_t _arm;
  UShort_t _station;
  UShort_t _octant;
  UShort_t _half_octant;
  UShort_t _gap;
  UShort_t _cathode; 

  ClassDef(TMutEvalRes,1) 
};


#endif /* __TMUTEVALRES_H__ */













