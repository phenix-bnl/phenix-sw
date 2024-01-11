//////////////////////////////////////////////////////////////////
//
// Utility class: TMutMeasureModel.h
// Author: S.Kelly 
// Date: 4/16/02
// Description: Measurement model for MUTR 
//              
//              
//////////////////////////////////////////////////////////////////

#ifndef __TMUTMEASUREMODEL_H__
#define __TMUTMEASUREMODEL_H__

#include<PHPoint.h>
#include<PHLine.h>
#include<boost/tuple/tuple.hpp>
#include<TMutCoordMap.h>
#include<TMutGapCoordMap.h>

class TMutFitPar;
class TMutMeasureModel
{
 public:
  
  /*! 
    MUTR cathode measurement model.  Calculates the distance
    of closest approach between the projection of the
    track onto the anode and the given TMutCoord.    
  */
  static double cathode_measure(const PHPoint&, const TMutCoord*, bool anode_project=true);

  /*! 
    Propagate given point with errors through MUTR measurement model. 
  */
  static double get_w_error(const  PHPoint& point, const PHPoint& error, const TMutCoord* coord);
  
  /*! 
    MUTR anode measurement model.  Calculates the distance of 
    closest approach between the anode wire and given TMutGapCoord.
  */
  static double anode_measure(const PHPoint&, const TMutGapCoord*);

  /*! 
    Measurement model jacobian.  The derivative is with respect to the x coordinate of the
    given PHPoint.
  */
  static double d_cathode_measure_dx(const PHPoint&, const TMutCoord*, bool anode_project=true);  

  /*! 
    Measurement model jacobian.  The derivative is with respect to the y coordinate of the
    given PHPoint.
  */
  static double d_cathode_measure_dy(const PHPoint&, const TMutCoord*, bool anode_projecct=true);  
  
  /*! 
    Measurement model jacobian.  The derivative is with respect to the x coordinate of the
    given PHPoint. 
  */
  static double d_anode_measure_dx(const PHPoint&, const TMutGapCoord*);  

  /*! 
    Measurement model jacobian.  The derivative is with respect to the y coordinate of the
    given PHPoint. 
  */
  static double d_anode_measure_dy(const PHPoint&, const TMutGapCoord*);  
  
 private:
  
  /*!
    Projects input point onto nearest anode and returns projected PHPoint. 
  */
  static PHPoint get_anode_projection(unsigned short arm, const PHPoint& point);
  
  // We use gsl routines to numerically evaluate derivatives
  // of the measurment model. This necessiates exposing the
  // "cathode_measure (anode_measure)" method as a function of 
  // a single variable while the other variables held constant 
  // during the differentiation are passed in as parameters via 
  // a void pointer to a cathode_function_data (anode_function_data)
  // structure.
  
  //  Parameter structure for passing in non-variable arguments
  //  to cathode_measure_*
  //
  struct cathode_function_data {
    PHPoint point;
    const TMutCoord* coord;
    bool use_anodes;
  };  
  
  //  Parameter structure for passing in non-variable arguments
  //  to anode_measure_*
  //
  struct anode_function_data {
    PHPoint point;
    const TMutGapCoord* gap;
  };  
  
  // "cathode_measure" exposed as a function of a single variable
  //
  static double cathode_measure_x(double x, void* params);
  static double cathode_measure_y(double y, void* params);
  
  // "anode_measure" exposed as a function of a single variable
  //
  static double anode_measure_x(double x, void* params);
  static double anode_measure_y(double y, void* params);
  
};

#endif



