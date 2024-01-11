
#ifndef _TMUTVTX_H_
#define _TMUTVTX_H_

// $Id: TMutVtx.hh,v 1.7 2011/12/29 20:19:31 slash Exp $

/*!
   \file    TMutVtx.hh
   \brief   Class for muon two track vertex
   \author  sean KELLY
   \version $Revision: 1.7 $
   \date    $Date: 2011/12/29 20:19:31 $
*/

// CINT compatible headers
#include<TDataType.h>
#include<PHKey.hh>
#include<PHException.h>
#include<MUTOO.h>
#include<TMutTrkPar.hh>

// forward declarations
class TMutVtxMap;

/*! @ingroup interface */

/*!
   \class TMutVtx
   \brief virtual class for muon two track vertex
*/
class TMutVtx : public PHKey
{    
  public:  

  /*! Enumeration for the charge combination of muon pairs. */
  enum Sign{POSNEG,POSPOS,NEGNEG};

  //! @name Constructors/Destructors
  //@{    

  /*! Default constructor */
  TMutVtx(){;}

  /*! Construct with key and location */
  TMutVtx(const Key& key) : PHKey(key) {;}

  /*! Virtual destructor */
  virtual ~TMutVtx(){;}
  
  //@}
  
  //! @name Functional Interface
  //@{    
  //@}
  
  //! returns track parameters associated to first track
  virtual const TMutTrkPar& get_trk1_par() const 
  {
    static TMutTrkPar trk_par;
    return trk_par;
  }    
  
  //! returns track parameters associated to first track
  virtual const TMutTrkPar& get_trk2_par() const 
  {
    static TMutTrkPar trk_par;
    return trk_par;
  }    

  /*! px sum */
  virtual double get_px() const 
  { return get_px1() + get_px2(); }

  /*! py sum */
  virtual double get_py() const 
  { return get_py1() + get_py2(); }

  /*! pz sum */
  virtual double get_pz() const 
  { return get_pz1() + get_pz2(); }

  /*! x vertex */
  virtual double get_x() const 
  { return 0; }    

  /*! y vertex */
  virtual double get_y() const 
  { return 0; }    

  /*! z vertex */
  virtual double get_z() const 
  { return 0; }

  /*! bend plane x vertex */
  virtual double get_x_bp() const 
  { return 0; }    

  /*! bend plane y vertex */
  virtual double get_y_bp() const 
  { return 0; }    

  /*! bend plane z vertex */
  virtual double get_z_bp() const 
  { return 0; }

  /*! bend plane distance of closest approch */
  virtual double get_dca_bp() const 
  { return 0; }
  
  /*! px track 1 */
  virtual double get_px1() const
  { return 0; }

  /*! py track 1 */
  virtual double get_py1() const
  { return 0; }

  /*! pz track 1 */
  virtual double get_pz1() const
  { return 0; }

  /*! total momentum track 1*/
  virtual double get_ptot1() const 
  { return 0; }

  /*! charge track 1 */
  virtual double get_charge1() const
  { return 0; }

  /*! px track 2 */
  virtual double get_px2() const
  {return 0;}
  
  /*! py track 1 */
  virtual double get_py2() const
  {return 0;}
  
  /*! pz track 2 */
  virtual double get_pz2() const
  {return 0;}
  
  /*! total momentum track 1*/
  virtual double get_ptot2() const 
  {return 0;}

  /*! charge track 2 */
  virtual double get_charge2() const 
  {return 0;}
  
  /*! Calculate the invariant mass */
  virtual double get_mass() const;
  
  /*! Set and Get the charge combination of muon pairs. */
  virtual UShort_t get_sign() const;

  /*! total momentum */
  virtual double get_ptot() const;

  /*! transverse momentum */
  virtual double get_pt() const; 
  
  /*! Get the rapidity (assume two muons) */
  virtual double get_rapidity() const; 

  /*! x vertex */
  virtual void set_x(double x) 
  {}  

  /*! y vertex */
  virtual void set_y(double y) 
  {}  
  
  /*! z vertex */
  virtual void set_z(double z) 
  {}  

  /*! bend plane x vertex */
  virtual void set_x_bp(double ) 
  {}  

  /*! bend plane y vertex */
  virtual void set_y_bp(double y) 
  {}  
  
  /*! bend plane z vertex */
  virtual void set_z_bp(double z) 
  {}  
  
  /*! bend plane distance of closest approach */
  virtual void set_dca_bp( double dca )
  {}
  /*! px track 1 */
  virtual void set_px1(double px1)
  {}  
 
  /*! py track 1 */
  virtual void set_py1(double py1)
  {}  
  
  /*! pz track 1 */
  virtual void set_pz1(double pz1)
  {}  
  
  /*! charge track 1 */
  virtual void set_charge1(double charge1)
  {}  
  
  /*! px track 2 */
  virtual void set_px2(double px2) 
  {}  

  /*! py track 2 */
  virtual void set_py2(double py2)
  {}  
  
  /*! pz track 2 */
  virtual void set_pz2(double pz2)
  {}  
  
  /*! charge track 2 */
  virtual void set_charge2(double charge2)
  {}  

  //! @name Vertex Parameters Reco Units
  //@{    

  /*! phi track 1 */
  virtual double get_phi1() const 
  { return 0; }
  
  /*! dzds track 1 */
  virtual double get_dzds1() const 
  { return 0; }
  
  /*! q/p track 1 */
  virtual double get_pinv1() const 
  { return 0; }
  
  /*! phi track 2 */
  virtual double get_phi2() const 
  { return 0; }
  
  /*! dzds track 2 */
  virtual double get_dzds2() const 
  { return 0; }
  
  /*! q/p track 2 */
  virtual double get_pinv2() const 
  { return 0; }
  
  //@}


  //! @name Merit statistics and Covariance
  //@{    
  /*! chi square statistic */ 
  virtual double get_chi_square() const 
  { return 0;}

  /*! chi square/degrees of freedom statistic */ 
  virtual double get_chi_square_pdf() const 
  { return 0;}

  /*! degrees of freedom */ 
  virtual size_t get_ndf() const 
  { return 0;}

  /*! covariance matrix */ 
  virtual double get_covar(UShort_t i, UShort_t j) const 
  { return 0; }  

  /*! chi square statistic */ 
  virtual void set_chi_square(double chi_square) 
  {}

  /*! degrees of freedom */
  virtual void set_ndf( size_t value )
  {}

  /*! covariance matrix */ 
  virtual void set_covar(UShort_t i, UShort_t j, double val)
  {}
  
  //@}
  
  //! @name Dumpers
  //@{    
  virtual void print(std::ostream& os = std::cout) const 
  {}

  //@}
  
  //! @name Locators
  //@{ 
    
  #ifndef __CINT__
  //! return class ID, mapped from class name
  virtual PHClassId::id_type get_class_id( void ) const
  { return (_class_id) ? _class_id : (_class_id = PHClassId::get( GetName() ) ); }
  #endif

  /*! Arm [0,1] */
  virtual UShort_t  get_arm() const 
  {return 0;}
  
  /*! Arm [0,1] */
  virtual void set_arm(UShort_t arm)
  {}
  
  /*! Index [0,1024] */
  virtual UShort_t  get_index() const 
  {return 0;}
  
  /*! Index [0,1024] */
  virtual void set_index(UShort_t index)
  {}
  //@}
  
  protected:
  
  //! covariance matrix side size and full size
  enum { COVAR_ROW=7, COVAR_SIZE=49 };

  private:
  
#ifndef __CINT__
  
  //! static class ID 
  static PHClassId::id_type _class_id;

#endif
  
  ClassDef(TMutVtx,1)
};

#endif
