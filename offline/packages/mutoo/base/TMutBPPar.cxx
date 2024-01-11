// $Id: TMutBPPar.cxx,v 1.1 2006/04/22 01:53:01 hpereira Exp $

/*!
	\file TMutBPPar.cxx
	\brief Bend plane track parameters
	\author S. Kelly
  \version $Revision: 1.1 $
  \date    $Date: 2006/04/22 01:53:01 $
*/

#include<TMutBPPar.hh>
ClassImp(TMutBPPar)

TMutBPPar::TMutBPPar() :  
  _status(NO_FIT),
  _zbp_12(0),
  _zbp_23(0),
  _pt_kick_12(0),
  _pt_kick_23(0),
  _chi_sq(0),
  _charge(0),  
  _x_vtx(0),
  _y_vtx(0),
  _z_vtx(0),
  _px_vtx(0),
  _py_vtx(0),
  _pz_vtx(0),
  _x_st1(0),
  _y_st1(0),
  _z_st1(0),
  _px_st1(0),
  _py_st1(0),
  _pz_st1(0),
  _x_st2(0),
  _y_st2(0),
  _z_st2(0),
  _px_st2(0),
  _py_st2(0),
  _pz_st2(0),
  _x_st3(0),
  _y_st3(0),
  _z_st3(0),
  _px_st3(0),
  _py_st3(0),
  _pz_st3(0),  
  _err_x_vtx(0),
  _err_y_vtx(0),
  _err_z_vtx(0),
  _err_px_vtx(0),
  _err_py_vtx(0),
  _err_pz_vtx(0),
  _err_x_st1(0),
  _err_y_st1(0),
  _err_z_st1(0),
  _err_px_st1(0),
  _err_py_st1(0),
  _err_pz_st1(0),
  _err_x_st2(0),
  _err_y_st2(0),
  _err_z_st2(0),
  _err_px_st2(0),
  _err_py_st2(0),
  _err_pz_st2(0),
  _err_x_st3(0),
  _err_y_st3(0),
  _err_z_st3(0),
  _err_px_st3(0),
  _err_py_st3(0),
  _err_pz_st3(0)
{;}






