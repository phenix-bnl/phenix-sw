// $Id: FVTXOO.h,v 1.14 2012/12/06 22:09:18 jinhuang Exp $
#ifndef __FVTXOO_H__
#define __FVTXOO_H__

/*!
 \file FVTXOO.h
 \brief widely used utility functions and enumerations
 \author H. Pereira Da Costa
 \version $Revision: 1.14 $
 \date $Date: 2012/12/06 22:09:18 $
 */

#include <cmath>
#include<string>
#include<iostream>
#include<sstream>
#include <algorithm>
#include<TDataType.h>

#include <stdexcept>

#ifndef __CINT__
#include "gsl/gsl_math.h"
#endif

//! widely used utility functions and enumerations
namespace FVTXOO
{

  //! Exception classes
  struct Unimplmented : public std::runtime_error
  {
    Unimplmented(std::string msg = "Unimplmented functionality") :
        std::runtime_error(msg)
    {
    }
  };

  // encapsulate constants used in class invariant tests and
  // the trace functions.

  //! module verbosity level
  enum Verbosity
  {
    NONE = 0, SOME = 1, ALOT = 2, MAX = 3
  };

  //! number of arms
  const int MAX_ARM = 2;

  //! number of cages
  const int MAX_CAGE = 2;

  //! number of stations
  const int MAX_STATION = 4;

  //! number of sectors
  const int MAX_SECTOR = 24;

  //! number of columns per plane
  const int MAX_COLUMN = 2;

  //! number of strips
  const int MAX_STRIP = 1664;

  //! FPHX threshold values (in electrons):
  const float FPHX_THRESH[8] =
    { 2000, 3200, 4600, 6300, 8100, 10000, 15000, 30000 };
  const float FPHX_MAXCHARGE = 50000;

#ifndef __CINT__
  //! convert rads into degrees
  const double RAD_TO_DEG = 180.0 / M_PI;

  //! convert degrees into radians
  const double DEG_TO_RAD = M_PI / 180.0;
#endif

  //! muon mass
  const double MASS_MUON = 0.105658388214;

  //! muon mass square
  const double MASS_MUON_SQUARE = 0.011163695;

  //! arm enumeration
  enum ArmNumber
  {
    South, North
  };

  //! station enumeration
  enum StationNumber
  {
    Station1, Station2, Station3, Station4
  };

  //! gap enumeration
  enum PlaneNumber
  {
    Plane1, Plane2
  };

  //! print a message to cout stream
  inline void
  TRACE(const std::string& message)
  {
    std::cout << "TRACE: " << message << std::endl;
  }

  //! print a message and a value to cout stream
  inline void
  TRACE(const std::string& message, const float& val)
  {
    std::cout << "TRACE: " << message << "\t" << val << std::endl;
  }

  //! print a message (formated) to a stream
  void
  PRINT(std::ostream& os = std::cout, const std::string& message = "");

  //! square a number of a given type
  template<typename T>
    T
    SQUARE(const T& x)
    {
      return x * x;
    }

  //! return sign of a number: 0; +1 or -1
  template<typename T>
    int
    SIGN(const T& value)
    {
      if (!value)
        return 0;
      return (value > 0) ? 1 : -1;
    }

  // Normalizes angle to (pi,-pi].  Shamelessly stolen from YLai's 
  // analysis module (jet reconstruction).
  template<typename T>
    T
    angle_normalize(const T x)
    {
      T ret;
      if (x > -3 * M_PI && x <= 3 * M_PI)
        {
          ret = x;
          if (ret > M_PI)
            ret -= 2 * M_PI;
          else if (ret <= -M_PI)
            ret += 2 * M_PI;
        }
      else
        {
          const T x_pi_mod = std::fmod(x + M_PI, 2 * M_PI);
          ret = x_pi_mod > 0 ? x_pi_mod - M_PI : x_pi_mod + M_PI;
        }
      return ret;
    }

  // A predicate-like functor that returns true if the two ranges 
  // given are overlapping.  Includes the end values.
  template<typename T>
    struct AngleOverlap
    {
      bool
      operator()(const T x1_, const T x2_, const T y1_, const T y2_) const
      {
        T x1 = x1_;
        T x2 = x2_;
        T y1 = y1_;
        T y2 = y2_;

        // Make this calculation robust against the possibility that
        // the geometry of the 1 and 2 are the opposite of what we have assumed
        if (angle_normalize(x1 - x2) > 0.0)
          std::swap(x1, x2);
        if (angle_normalize(y1 - y2) > 0.0)
          std::swap(y1, y2);

        //if ( angle_normalize(x2-x1) < 0 ) throw std::runtime_error("Invalid args to AngleOverlap");
        //if ( angle_normalize(y2-y1) < 0 ) throw std::runtime_error("Invalid args to AngleOverlap");

        T dX1Y1 = 0.0;
        T dX1Y2 = 0.0;
        T dX2Y1 = 0.0;
        T dX2Y2 = 0.0;

        if (x2 >= x1 && y2 >= y1)
          {
            // In this case, neither range crosses the branch cut
            dX1Y1 = (x1 - y1);
            dX1Y2 = (x1 - y2);
            dX2Y1 = (x2 - y1);
            dX2Y2 = (x2 - y2);
          }
        else
          {
            dX1Y1 = FVTXOO::angle_normalize(x1 - y1);
            dX1Y2 = FVTXOO::angle_normalize(x1 - y2);
            dX2Y1 = FVTXOO::angle_normalize(x2 - y1);
            dX2Y2 = FVTXOO::angle_normalize(x2 - y2);
          }

        // If x1 (start of range) is between y1 & y2, return true
        if (dX1Y1 >= 0 && dX1Y2 <= 0)
          return true;

        // If x2 (end of range) is between y1 & y2, return true
        else if (dX2Y1 >= 0 && dX2Y2 <= 0)
          return true;

        // If x1 < y1 and x2 > y2 (y is contained within x), return true
        else if (dX1Y1 <= 0 && dX2Y2 >= 0)
          return true;

        // if x1 > y1 and x2 < y2 (x is contained within y), return true
        else if (dX1Y1 >= 0 && dX2Y2 <= 0)
          return true;

        // otherwise x1-x2 does not overlap y1-y2, return false
        return false;
      }

    };


  //! return true if the event number is special, i.e. 1,2...5,10,20...50,100,200,...
  bool special_event_num(const int event_num);

}

#endif
