
/*!
   \file    TMutVtxPar_v2.hh
   \brief   
   parameter class for muon two track vertex. 
   this class is obselete. It is only used internaly for the TMutVtx_v1 interface and is kept for 
   backward compatibility.
   \author  Sean KELLY
   \version $Revision: 1.3 $
   \date    $Date: 2011/12/24 04:48:20 $
*/

#ifndef __TMUTVTXPAR_V2_H__
#define __TMUTVTXPAR_V2_H__

#include <TDataType.h>
#include <algorithm>
#include <TObject.h>

#include "MUTOO.h"
#include "PHException.h"
#include "TMutTrkPar.hh"

/*!
   \class TMutVtxPar_v2
   \brief   
   parameter class for muon two track vertex. 
   this class is obselete. It is only used internaly for the TMutVtx_v1 interface and is kept for 
   backward compatibility.
*/
class TMutVtxPar_v2 : public TObject
{
public:

  //! @name Constructors/Destructors
  //@{    
  /*! Construct with parameter set */
  TMutVtxPar_v2( 
    double x=0, 
    double y=0, 
    double z=0, 
	  double px1=0,
	  double py1=0,
	  double pz1=0,
	  double charge1=0,
	  double px2=0,
	  double py2=0,
	  double pz2=0,
	  double charge2=0) : 
    
  _x(x),
  _y(y),
  _z(z),
    
  _x_bp(0),
  _y_bp(0),
  _z_bp(0),
  _dca_bp(0),
  
  _px1(px1),
  _py1(py1),
  _pz1(pz1),
  _charge1(charge1),
  _px2(px2),
  _py2(py2),
  _pz2(pz2),
  _charge2(charge2),
  _chi_square(0),
  _ndf(0)
  {
    // initialize covariance matrix to 0
    std::fill(_covar,_covar+COVAR_SIZE,0);
  }
  
  /*! Destructor */
  virtual ~TMutVtxPar_v2()
  {}
  
  //@}

  //! @name Vertex Parameters Physics Units
  //@{    
  /*! Read only reference to track one parameters */
  const TMutTrkPar& get_trk1_par() const 
  {
    
    static TMutTrkPar trk_par;
    trk_par.set_x(_x);
    trk_par.set_y(_y);
    trk_par.set_z(_z);
    trk_par.set_px(_px1);
    trk_par.set_py(_py1);
    trk_par.set_pz(_pz1);
    // return read only reference to function scope static
    return trk_par;
    
  }
  
  /*! Read only reference to track two parameters */
  const TMutTrkPar& get_trk2_par() const 
  {
    static TMutTrkPar trk_par;
    trk_par.set_x(_x);
    trk_par.set_y(_y);
    trk_par.set_z(_z);
    trk_par.set_px(_px2);
    trk_par.set_py(_py2);
    trk_par.set_pz(_pz2);
    // return read only reference to function scope static
    return trk_par;
  }

  /*! x vertex */
  virtual double get_x() const 
  { return _x;}

  /*! y vertex */
  virtual double get_y() const 
  { return _y;}

  /*! z vertex */
  virtual double get_z() const 
  { return _z;}

  /*! bend plane x vertex */
  virtual double get_x_bp() const 
  { return _x_bp;}

  /*! bend plane y vertex */
  virtual double get_y_bp() const 
  { return _y_bp;}

  /*! bend plane z vertex */
  virtual double get_z_bp() const 
  { return _z_bp;}

  /*! bend plane distance of closest approach */
  virtual double get_dca_bp() const 
  { return _dca_bp;}
  
  /*! px track 1 */
  virtual double get_px1() const 
  { return _px1;}
  
  /*! py track 1 */
  virtual double get_py1() const 
  { return _py1;}
  
  /*! pz track 1 */
  virtual double get_pz1() const 
  { return _pz1;}
  
  /*! total momentum track 1*/
  virtual double get_ptot1() const 
  { 
    return sqrt(
      MUTOO::SQUARE(_px1) +
      MUTOO::SQUARE(_py1) +
      MUTOO::SQUARE(_pz1));
  }
  /*! charge track 1 */
  virtual double get_charge1() const 
  { return _charge1;}
  
  /*! px track 2 */
  virtual double get_px2() const 
  { return _px2;}
  
  /*! py track 1 */
  virtual double get_py2() const 
  { return _py2;}
  
  /*! pz track 2 */
  virtual double get_pz2() const 
  { return _pz2;}  
  
  /*! charge track 2 */
  virtual double get_charge2() const 
  { return _charge2;}
  
  /*! total momentum track 1*/
  virtual double get_ptot2() const 
  { 
    return sqrt(
      MUTOO::SQUARE(_px2) +
      MUTOO::SQUARE(_py2) +
      MUTOO::SQUARE(_pz2));
  }

  /*! x vertex */
  virtual void set_x(double x) 
  { _x = x;}  

  /*! y vertex */
  virtual void set_y(double y) 
  { _y = y;}  

  /*! z vertex */
  virtual void set_z(double z) 
  { _z = z;}  

  /*! bend plane x vertex */
  virtual void set_x_bp(double x) 
  { _x_bp = x;}  

  /*! bend plane y vertex */
  virtual void set_y_bp(double y) 
  { _y_bp = y;}  

  /*! bend plane z vertex */
  virtual void set_z_bp(double z) 
  { _z_bp = z;}  

  /*! bend plane distance of closest approach */
  virtual void set_dca_bp(double dca) 
  { _dca_bp = dca;}  
  
  /*! px track 1 */
  virtual void set_px1(double px1) 
  { _px1 = px1;}
  
  /*! py track 1 */
  virtual void set_py1(double py1) 
  { _py1 = py1;}
  
  /*! pz track 1 */
  virtual void set_pz1(double pz1) 
  { _pz1 = pz1;}
  
  /*! charge track 1 */
  virtual void set_charge1(double charge1) 
  { _charge1 = charge1;}
  
  /*! px track 1 */
  virtual void set_px2(double px2) 
  { _px2 = px2;}
  
  /*! py track 2 */
  virtual void set_py2(double py2) 
  { _py2 = py2;}
  
  /*! pz track 2 */
  virtual void set_pz2(double pz2) 
  { _pz2 = pz2;}
  
  /*! charge track 2 */
  virtual void set_charge2(double charge2) 
  { _charge2 = charge2;}

  //@}

  //! @name Vertex Parameters Reco Units
  //@{    

  /*! phi track 1 */
  virtual double get_phi1() const 
  { return atan2(_py1,_px1); }
  
  /*! dzds track 1 */
  virtual double get_dzds1() const 
  { return _pz1/get_ptot1(); }
  
  /*! q/p track 1 */
  virtual double get_pinv1() const 
  { return _charge1/get_ptot1(); }
  
  /*! phi track 2 */
  virtual double get_phi2() const 
  { return atan2(_py2,_px2); }
  
  /*! dzds track 2 */
  virtual double get_dzds2() const 
  { return _pz2/get_ptot2(); }
  
  /*! q/p track 2 */
  virtual double get_pinv2() const 
  { return _charge2/get_ptot2(); }
  
  //@}

  //! @name Merit statistics and Covariance
  //@{    

  /*! full chisquare */ 
  double get_chi_square() const 
  { return _chi_square;}

  /*! number of degrees of freedom */ 
  size_t get_ndf() const 
  { return _ndf;}

  /*! reduced chisquare */ 
  double get_chi_square_pdf() const 
  { return (_ndf>0) ? _chi_square/_ndf:_chi_square;}

  /*! covariance matrix */ 
  double get_covar(unsigned short i, unsigned short j) const 
  {  
    unsigned short index = i*COVAR_ROW+j;
    BOUNDS_CHECK(index,COVAR_SIZE);
    return _covar[index];
  }  

  /*! full chisquare */ 
  void set_chi_square(double chi_square) 
  { _chi_square = chi_square;}

  /*! degrees of freedom */ 
  void set_ndf(size_t ndf) 
  {_ndf = ndf; }

  /*! covariance matrix */ 
  void set_covar(unsigned short i, unsigned short j, double val)
  {
    unsigned short index = i*COVAR_ROW+j;
    BOUNDS_CHECK(index,COVAR_SIZE);
    _covar[index] = val;
  }
  //@}

  //! @name Dumpers
  //@{    
  virtual void print(std::ostream& os = std::cout) const;

  //@}

private:				    

  //! vertex x position
  double _x;
  
  //! vertex y position
  double _y;
  
  //! vertex z position
  double _z;

  //! vertex bend plane x position
  double _x_bp;
  
  //! vertex bend plane y position
  double _y_bp;
  
  //! vertex bend plane z position
  double _z_bp;
 
  //! vertex bend plane distance of closest approach
  double _dca_bp;
  
  //! first track momentum
  double _px1;

  //! first track momentum
  double _py1;

  //! first track momentum
  double _pz1;

  //! first track charge
  double _charge1;
  
  //! second track momentum
  double _px2;

  //! second track momentum
  double _py2;

  //! second track momentum
  double _pz2;

  //! second track charge
  double _charge2;
  
  //! vertex full chisquare
  double _chi_square;
  
  //! vertex number of degrees of freedom
  size_t _ndf;
  
  //! covariance matrix size
  enum { COVAR_ROW = 7, COVAR_SIZE = 49 };
  
  //! covariance matrix
  double _covar[COVAR_SIZE];
  
  //!
  ClassDef(TMutVtxPar_v2,1)

};

#endif

