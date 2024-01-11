
/*!
   \file    TMutVtxPar.hh
   \brief   
   parameter class for muon two track vertex. 
   this class is obselete. It is only used internaly for the TMutVtx_v1 interface and is kept for 
   backward compatibility.
   \author  Sean KELLY
   \version $Revision: 1.3 $
   \date    $Date: 2011/12/24 04:48:20 $
*/

#ifndef __TMUTVTXPAR_H__
#define __TMUTVTXPAR_H__

#include <TDataType.h>
#include <algorithm>
#include <TObject.h>

#include "MUTOO.h"
#include "PHException.h"
#include "TMutTrkPar.hh"

/*!
   \class TMutVtxPar
   \brief   
   parameter class for muon two track vertex. 
   this class is obselete. It is only used internaly for the TMutVtx_v1 interface and is kept for 
   backward compatibility.
*/
class TMutVtxPar : public TObject
{
public:

  //! @name Constructors/Destructors
  //@{    
  /*! Construct with parameter set */
  TMutVtxPar(
    double z=0, 
    double px1=0,
    double py1=0,
    double pz1=0,
    double charge1=0,
    double px2=0,
    double py2=0,
    double pz2=0,
    double charge2=0) :    
    _z(z),
    _px1(px1),
    _py1(py1),
    _pz1(pz1),
    _charge1(charge1),
    _px2(px2),
    _py2(py2),
    _pz2(pz2),
    _charge2(charge2),
    _chi_square(0)
  {
    // initialize covariance matrix to 0
    std::fill(_covar,_covar+COVAR_SIZE,0);
  }
  
  /*! Destructor */
  virtual ~TMutVtxPar()
  {}
  
  //@}

  //! @name Vertex Parameters Physics Units
  //@{    
  /*! Read only reference to track one parameters */
  const TMutTrkPar& get_trk1_par() const 
  {
    
    static TMutTrkPar trk_par;
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
    trk_par.set_z(_z);
    trk_par.set_px(_px2);
    trk_par.set_py(_py2);
    trk_par.set_pz(_pz2);
    // return read only reference to function scope static
    return trk_par;
    
  }

  /*! z vertex */
  virtual double get_z() const 
  { return _z;}
  
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
    return std::sqrt(
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
    return std::sqrt(
      MUTOO::SQUARE(_px2) +
      MUTOO::SQUARE(_py2) +
      MUTOO::SQUARE(_pz2));
  }

  /*! z vertex */
  virtual void set_z(double z) 
  { _z = z;}  
  
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
  { return std::atan2(_py1,_px1); }
  
  /*! dzds track 1 */
  virtual double get_dzds1() const 
  { return _pz1/get_ptot1(); }
  
  /*! q/p track 1 */
  virtual double get_pinv1() const 
  { return _charge1/get_ptot1(); }
  
  /*! phi track 2 */
  virtual double get_phi2() const
  { return std::atan2(_py2,_px2); }
  
  /*! dzds track 2 */
  virtual double get_dzds2() const
  { return _pz2/get_ptot2(); }
  
  /*! q/p track 2 */
  virtual double get_pinv2() const 
  { return _charge2/get_ptot2(); }
  
  //@}

  //! @name Merit statistics and Covariance
  //@{    

  /*! chi square statistic */ 
  double get_chi_square() const 
  { return _chi_square;}

  /*! covariance matrix */ 
  double get_covar(unsigned short i, unsigned short j) const 
  {  
    unsigned short index = i*COVAR_ROW+j;
    BOUNDS_CHECK(index,COVAR_SIZE);
    return _covar[index];
  }  

  /*! chi square statistic */ 
  void set_chi_square(double chi_square) 
  { _chi_square = chi_square;}

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

  double _z;
  double _px1;
  double _py1;
  double _pz1;
  double _charge1;
  double _px2;
  double _py2;
  double _pz2;
  double _charge2;
  double _chi_square;
  enum { COVAR_ROW=7, COVAR_SIZE=49 };
  double _covar[COVAR_SIZE];
  ClassDef(TMutVtxPar,1)

};

#endif

