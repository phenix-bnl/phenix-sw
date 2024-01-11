// $Id: TMutBPPar.hh,v 1.3 2009/08/26 02:29:48 hpereira Exp $

/*!
	\file TMutBPPar.h
	\brief Bend plane track parameters
	\author S. Kelly
  \version $Revision: 1.3 $
  \date    $Date: 2009/08/26 02:29:48 $
*/

#ifndef __TMUTBPPAR_H__
#define __TMUTBPPAR_H__

#include<PHException.h>
#include<TDataType.h>
#include<algorithm>
#include<TObject.h>
#include<PHVector.h>
#include<PHPoint.h>
#include<MUTOO.h>
#include<vector>

/*! @ingroup classes */
//!  Bend Plane track parameters 
/*!  
Bend Plane Track Parameters
<ul>
<li> List of parameters here
</ul>
*/
class TMutBPPar : public TObject
{
public:

  enum Status {NO_FIT, SINGLE_BP, DOUBLE_BP};

  //! @name Constructors/Destructors
  //@{    
  /*! Construct with parameter set */
  TMutBPPar();
  /*! Destructor */
  virtual ~TMutBPPar(){;}
  //@}

  //! @name  Bend Plane Parameters
  //@{    
  /*! Status */
  virtual Status get_status() const 
  {return (Status) _status;}
	
  /*! Status */
  virtual void set_status(Status value) 
  { _status = value;}
	
  /*! z of bend plane between stations 1 and 2 */
  virtual double get_zbp_12() const 
  {return _zbp_12;}
	
  /*! z of bend plane between stations 1 and 2 */
  virtual void set_zbp_12(double value) 
  { _zbp_12 = value;}
	
  
  /*! z of bend plane between stations 2 and 3 */
  virtual double get_zbp_23() const 
  {return _zbp_23;}
	
  /*! z of bend plane between stations 2 and 3 */
  virtual void set_zbp_23(double value) 
  { _zbp_23 = value;}
  
  /*! pt kick of bend plane between stations 1 and 2 */
  virtual double get_pt_kick_12() const 
  {return _pt_kick_12;}
	
  /*! pt kick of bend plane between stations 1 and 2 */
  virtual void set_pt_kick_12(double value) 
  { _pt_kick_12 = value;}
	
  
  /*! pt kick of bend plane between stations 2 and 3 */
  virtual double get_pt_kick_23() const 
  {return _pt_kick_23;}
	
  /*! pt kick of bend plane between stations 2 and 3 */
  virtual void set_pt_kick_23(double value)
  { _pt_kick_23 = value;}

  //@}

  //! @name Global Attributes
  //@{    
  /*! chi square */
  virtual double get_chi_sq() const 
  {return _chi_sq;}
	
  /*! chi square */
  virtual void set_chi_sq(double value) 
  { _chi_sq = value;}  
	
  /*! charge */
  virtual double get_charge() const 
  {return _charge;}
	
  /*! charge */
  virtual void set_charge(double value) 
  { _charge = value;}  
	
  //@}


  //! @name BP Parameters at Vertex
  //@{    
  //@}
  virtual double get_x_vtx() const 
  {return _x_vtx;}
	
  virtual void set_x_vtx(double value) 
  {_x_vtx = value;}
	
  virtual double get_y_vtx() const 
  {return _y_vtx;}
	
  virtual void set_y_vtx(double value) 
  {_y_vtx = value;}
	
  virtual double get_z_vtx() const 
  {return _z_vtx;}
	
  virtual void set_z_vtx(double value) 
  {_z_vtx = value;}
	
  virtual double get_px_vtx() const 
  {return _px_vtx;}
	
  virtual void set_px_vtx(double value) 
  {_px_vtx = value;}
	
  virtual double get_py_vtx() const 
  {return _py_vtx;}
	
  virtual void set_py_vtx(double value) 
  {_py_vtx = value;}
	
  virtual double get_pz_vtx() const 
  {return _pz_vtx;}
	
  virtual void set_pz_vtx(double value) 
  {_pz_vtx = value;}
	
  //@}

  //! @name BP Parameters in MUTR
  //@{    
  virtual double get_x_st1() const 
  {return _x_st1;}
	
  virtual void set_x_st1(double value) 
  {_x_st1 = value;}
	
  virtual double get_y_st1() const 
  {return _y_st1;}
	
  virtual void set_y_st1(double value) 
  {_y_st1 = value;}
	
  virtual double get_z_st1() const 
  {return _z_st1;}
	
  virtual void set_z_st1(double value) 
  {_z_st1 = value;}
	
  virtual double get_px_st1() const 
  {return _px_st1;}
	
  virtual void set_px_st1(double value) 
  {_px_st1 = value;}
	
  virtual double get_py_st1() const 
  {return _py_st1;}
	
  virtual void set_py_st1(double value) 
  {_py_st1 = value;}
	
  virtual double get_pz_st1() const 
  {return _pz_st1;}
	
  virtual void set_pz_st1(double value) 
  {_pz_st1 = value;}

  virtual double get_x_st2() const 
  {return _x_st2;}
	
  virtual void set_x_st2(double value) 
  {_x_st2 = value;}
	
  virtual double get_y_st2() const 
  {return _y_st2;}
	
  virtual void set_y_st2(double value) 
  {_y_st2 = value;}
	
  virtual double get_z_st2() const 
  {return _z_st2;}
	
  virtual void set_z_st2(double value) 
  {_z_st2 = value;}
	
  virtual double get_px_st2() const 
  {return _px_st2;}
	
  virtual void set_px_st2(double value) 
  {_px_st2 = value;}
	
  virtual double get_py_st2() const 
  {return _py_st2;}
	
  virtual void set_py_st2(double value) 
  {_py_st2 = value;}
	
  virtual double get_pz_st2() const 
  {return _pz_st2;}
	
  virtual void set_pz_st2(double value) 
  {_pz_st2 = value;}
	
  virtual double get_x_st3() const 
  {return _x_st3;}
	
  virtual void set_x_st3(double value) 
  {_x_st3 = value;}
	
  virtual double get_y_st3() const 
  {return _y_st3;}
	
  virtual void set_y_st3(double value) 
  {_y_st3 = value;}
	
  virtual double get_z_st3() const 
  {return _z_st3;}
	
  virtual void set_z_st3(double value) 
  {_z_st3 = value;}
	
  virtual double get_px_st3() const 
  {return _px_st3;}
	
  virtual void set_px_st3(double value)
  {_px_st3 = value;}
	
  virtual double get_py_st3() const 
  {return _py_st3;}
	
  virtual void set_py_st3(double value) 
  {_py_st3 = value;}
	
  virtual double get_pz_st3() const 
  {return _pz_st3;}
	
  virtual void set_pz_st3(double value) 
  {_pz_st3 = value;}
	
  //@}

  //! @name BP Parameters Errors
  //@{    
  virtual double get_err_x_vtx() const 
  {return _err_x_vtx;}
	
  virtual void set_err_x_vtx(double value) 
  {_err_x_vtx = value;}
	
  virtual double get_err_y_vtx() const 
  {return _err_y_vtx;}
	
  virtual void set_err_y_vtx(double value) 
  {_err_y_vtx = value;}
	
  virtual double get_err_z_vtx() const 
  {return _err_z_vtx;}
	
  virtual void set_err_z_vtx(double value) 
  {_err_z_vtx = value;}
	
  virtual double get_err_px_vtx() const 
  {return _err_px_vtx;}
	
  virtual void set_err_px_vtx(double value) 
  {_err_px_vtx = value;}
	
  virtual double get_err_py_vtx() const 
  {return _err_py_vtx;}
	
  virtual void set_err_py_vtx(double value) 
  {_err_py_vtx = value;}
	
  virtual double get_err_pz_vtx() const 
  {return _err_pz_vtx;}
	
  virtual void set_err_pz_vtx(double value) 
  {_err_pz_vtx = value;}
	
  virtual double get_err_x_st1() const 
  {return _err_x_st1;}
	
  virtual void set_err_x_st1(double value) 
  {_err_x_st1 = value;}
	
  virtual double get_err_y_st1() const 
  {return _err_y_st1;}
	
  virtual void set_err_y_st1(double value) 
  {_err_y_st1 = value;}
	
  virtual double get_err_z_st1() const 
  {return _err_z_st1;}
	
  virtual void set_err_z_st1(double value) 
  {_err_z_st1 = value;}
	
  virtual double get_err_px_st1() const 
  {return _err_px_st1;}
	
  virtual void set_err_px_st1(double value) 
  {_err_px_st1 = value;}
	
  virtual double get_err_py_st1() const 
  {return _err_py_st1;}
	
  virtual void set_err_py_st1(double value) 
  {_err_py_st1 = value;}
	
  virtual double get_err_pz_st1() const 
  {return _err_pz_st1;}
	
  virtual void set_err_pz_st1(double value) 
  {_err_pz_st1 = value;}
	
  virtual double get_err_x_st2() const 
  {return _err_x_st2;}
	
  virtual void set_err_x_st2(double value) 
  {_err_x_st2 = value;}
	
  virtual double get_err_y_st2() const 
  {return _err_y_st2;}
	
  virtual void set_err_y_st2(double value) 
  {_err_y_st2 = value;}
	
  virtual double get_err_z_st2() const 
  {return _err_z_st2;}
	
  virtual void set_err_z_st2(double value)
  {_err_z_st2 = value;}
	
  virtual double get_err_px_st2() const 
  {return _err_px_st2;}
	
  virtual void set_err_px_st2(double value) 
  {_err_px_st2 = value;}
	
  virtual double get_err_py_st2() const 
  {return _err_py_st2;}
	
  virtual void set_err_py_st2(double value) 
  {_err_py_st2 = value;}
	
  virtual double get_err_pz_st2() const 
  {return _err_pz_st2;}
	
  virtual void set_err_pz_st2(double value) 
  {_err_pz_st2 = value;}
	
  virtual double get_err_x_st3() const 
  {return _err_x_st3;}
	
  virtual void set_err_x_st3(double value) 
  {_err_x_st3 = value;}
	
  virtual double get_err_y_st3() const 
  {return _err_y_st3;}
	
  virtual void set_err_y_st3(double value) 
  {_err_y_st3 = value;}
	
  virtual double get_err_z_st3() const 
  {return _err_z_st3;}
	
  virtual void set_err_z_st3(double value) 
  {_err_z_st3 = value;}
	
  virtual double get_err_px_st3() const 
  {return _err_px_st3;}
	
  virtual void set_err_px_st3(double value) 
  {_err_px_st3 = value;}
	
  virtual double get_err_py_st3() const 
  {return _err_py_st3;}
	
  virtual void set_err_py_st3(double value) 
  {_err_py_st3 = value;}
	
  virtual double get_err_pz_st3() const 
  {return _err_pz_st3;}
	
  virtual void set_err_pz_st3(double value) 
  {_err_pz_st3 = value;}
	
  //@}

private:

  //Status _status;
  int _status;
  double _zbp_12;
  double _zbp_23;
  double _pt_kick_12;
  double _pt_kick_23;
  double _chi_sq;
  double _charge;
  double _x_vtx;
  double _y_vtx;
  double _z_vtx;
  double _px_vtx;
  double _py_vtx;
  double _pz_vtx;
  double _x_st1;
  double _y_st1;
  double _z_st1;
  double _px_st1;
  double _py_st1;
  double _pz_st1;
  double _x_st2;
  double _y_st2;
  double _z_st2;
  double _px_st2;
  double _py_st2;
  double _pz_st2;
  double _x_st3;
  double _y_st3;
  double _z_st3;
  double _px_st3;
  double _py_st3;
  double _pz_st3;
  double _err_x_vtx;
  double _err_y_vtx;
  double _err_z_vtx;
  double _err_px_vtx;
  double _err_py_vtx;
  double _err_pz_vtx;
  double _err_x_st1;
  double _err_y_st1;
  double _err_z_st1;
  double _err_px_st1;
  double _err_py_st1;
  double _err_pz_st1;
  double _err_x_st2;
  double _err_y_st2;
  double _err_z_st2;
  double _err_px_st2;
  double _err_py_st2;
  double _err_pz_st2;
  double _err_x_st3;
  double _err_y_st3;
  double _err_z_st3;
  double _err_px_st3;
  double _err_py_st3;
  double _err_pz_st3;

  //  residual_list _bp_residuals;
  ClassDef(TMutBPPar,1)
};

#endif





