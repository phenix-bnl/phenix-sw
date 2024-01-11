#ifndef __TMUIEVALRES_H__
#define __TMUIEVALRES_H__

#include<TDataType.h>
#include<PHException.h>

//! TMuiEvalRes object stores evauation data for each hit in a track.

/*! 
	For each chamber, you have a set of reco values, such as 
	momentum and the coordinate of cross point ( the intersection 
	point of track with the chamber plane. You definitely want to 
	compare all the values with your MC input.
*/

class TMuiEvalRes : public TObject 
{
public:
	
	//! @name Constructors/Destructors

	//@{

	/*! Default constructor */
	TMuiEvalRes();

	/*! Virtual destructor */
	virtual ~TMuiEvalRes(){;}

	//@}

	//! @name Truth Data
	//@{
	/*! x coordinate of where MC track intersects with detector plane */
	virtual Float_t get_x_true() const 
	{ return _x_true;}
	
	/*! y coordinate of where MC track intersects with detector plane */
	virtual Float_t get_y_true() const 
	{ return _y_true;}
	
	/*! z coordinate of where MC track intersects with detector plane */
	virtual Float_t get_z_true() const 
	{ return _z_true;}

	/*! theta coordinate of where MC track intersects with detector plane */
	virtual Float_t get_theta_true() const 
	{ return _theta_true;}
	
	/*! phi coordinate of where MC track intersects with detector plane */
	virtual Float_t get_phi_true() const 
	{ return _phi_true;}
	
	/*! px of MC track when it intersects with detector plane */
	virtual Float_t get_px_true() const 
	{ return _px_true;}
	
	/*! py of MC track when it intersects with detector plane */
	virtual Float_t get_py_true() const 
	{ return _py_true;}
	
	/*! pz of MC track when it intersects with detector plane */
	virtual Float_t get_pz_true() const 
	{ return _pz_true;}

	/*! x coordinate of where MC track intersects with detector plane */
	virtual void set_x_true(Float_t x_true) 
	{ _x_true=x_true;}
	
	/*! y coordinate of where MC track intersects with detector plane */
	virtual void set_y_true(Float_t y_true) 
	{ _y_true=y_true;}
	
	/*! z coordinate of where MC track intersects with detector plane */
	virtual void set_z_true(Float_t z_true) 
	{ _z_true=z_true;}
	
	/*! theta coordinate of where MC track intersects with detector plane */
	virtual void set_theta_true(Float_t theta_true) 
	{ _theta_true=theta_true;}
	
	/*! phi coordinate of where MC track intersects with detector plane */
	virtual void set_phi_true(Float_t phi_true) 
	{ _phi_true=phi_true;}
	
	/*! px of MC track when it intersects with detector plane */
	virtual void set_px_true(Float_t px_true) 
	{ _px_true=px_true;}
	
	/*! py of MC track when it intersects with detector plane */
	virtual void set_py_true(Float_t py_true) 
	{ _py_true=py_true;}
	
	/*! pz of MC track when it intersects with detector plane */
	virtual void set_pz_true(Float_t pz_true) 
	{ _pz_true=pz_true;}
	
	//@}

	//! @name Reconstructed Data
	//@{
	/*! x coordinate of where reco track intersects with detector plane */
	virtual Float_t get_x_reco() const 
	{ return _x_reco;}
	
	/*! y coordinate of where reco track intersects with detector plane */
	virtual Float_t get_y_reco() const 
	{ return _y_reco;}
	
	/*! z coordinate of where reco track intersects with detector plane */
	virtual Float_t get_z_reco() const 
	{ return _z_reco;}
	
	/*! theta coordinate of where reco track intersects with detector plane */
	virtual Float_t get_theta_reco() const 
	{ return _theta_reco;}
	
	/*! phi coordinate of where reco track intersects with detector plane */
	virtual Float_t get_phi_reco() const 
	{ return _phi_reco;}

	/*! Difference between x_true and x_reco. */
	virtual Float_t get_delta_x() const 
	{ return difference(_x_true,_x_reco);}
	
	/*! Difference between y_true and y_reco. */
	virtual Float_t get_delta_y() const 
	{ return difference(_y_true,_y_reco);}
	
	/*! Difference between z_true and z_reco. */
	virtual Float_t get_delta_z() const 
	{ return difference(_z_true,_z_reco);}

	/*! x coordinate of where reco track intersects with detector plane */
	virtual void set_x_reco(Float_t x_reco) 
	{ _x_reco=x_reco;}
	
	/*! y coordinate of where reco track intersects with detector plane */
	virtual void set_y_reco(Float_t y_reco) 
	{ _y_reco=y_reco;}
	
	/*! z coordinate of where reco track intersects with detector plane */
	virtual void set_z_reco(Float_t z_reco)
	{ _z_reco=z_reco;}
	
	/*! theta coordinate of where reco track intersects with detector plane */
	virtual void set_theta_reco(Float_t theta_reco) 
	{ _theta_reco=theta_reco;}
	
	/*! phi coordinate of where reco track intersects with detector plane */
	virtual void set_phi_reco(Float_t phi_reco) 
	{ _phi_reco=phi_reco;}

	//@}

	//! @name Locators
	//@{
	virtual UShort_t get_arm() const 
	{ return _arm;}
	
	virtual UShort_t get_plane() const 
	{ return _plane;}
	
	virtual UShort_t get_panel() const 
	{ return _panel;}
	
	virtual UShort_t get_orientation() const 
	{ return _orientation;}

	virtual void set_arm(UShort_t arm) 
	{ _arm=arm;}
	
	virtual void set_plane(UShort_t plane) 
	{ _plane=plane;}
	
	virtual void set_panel(UShort_t panel) 
	{ _panel=panel;}
	
	virtual void set_orientation(UShort_t orientation) 
	{ _orientation=orientation;}

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

	UShort_t _arm;
	UShort_t _plane;
	UShort_t _panel;
	UShort_t _orientation;

	ClassDef(TMuiEvalRes,1) 
};


#endif /* __TMUIEVALRES_H__ */
