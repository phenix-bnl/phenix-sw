#ifndef _TMUTVTX_V1H_
#define _TMUTVTX_V1H_

// $Id: TMutVtx_v1.hh,v 1.13 2011/12/29 20:19:32 slash Exp $

/*!
   \file    TMutVtx_v1.hh
   \brief
   Class for muon two track vertex.
   Use an internal TMutVtxPar object to store the parameters. The object cannot
   be accessed externaly. Use direct access method instead.
   Later versions of the TMutVtx interface have no TMutVtxPar any more.
   Makes evolution easier.
   \author  Sean KELLY, Hugo PEREIRA
   \version $Revision: 1.13 $
   \date    $Date: 2011/12/29 20:19:32 $
*/

#include "TMutVtx.hh"
#include "TMutVtxPar.hh"

////////////////////////////////////////////////////////////////////////////
/*!
   \class TMutVtx_v1
   \brief Class for muon two track vertex
*/
////////////////////////////////////////////////////////////////////////////
class TMutVtx_v1 :  public TMutVtx
{
  public:

  //! empty constructor
  TMutVtx_v1();

  //! filled constructor
  TMutVtx_v1(const Key&,
	     UShort_t arm,
	     UShort_t index);

  //! copy constructor from base pointer
  TMutVtx_v1(const TMutVtx* base_ptr);

  //! copy constructor from base reference
  TMutVtx_v1(const TMutVtx& base_ref);

  //! destructor
  virtual ~TMutVtx_v1()
  {}

  //! returns track parameters associated to first track
  const TMutTrkPar& get_trk1_par() const
  { return _vtx_par.get_trk1_par(); }

  //! returns track parameters associated to first track
  const TMutTrkPar& get_trk2_par() const
  { return _vtx_par.get_trk2_par(); }

  /*! z vertex */
  virtual double get_z() const
  { return _vtx_par.get_z(); }

  /*! px track 1 */
  virtual double get_px1() const
  { return _vtx_par.get_px1(); }

  /*! py track 1 */
  virtual double get_py1() const
  { return _vtx_par.get_py1(); }

  /*! pz track 1 */
  virtual double get_pz1() const
  { return _vtx_par.get_pz1(); }

  /*! total momentum track 1*/
  virtual double get_ptot1() const
  { return _vtx_par.get_ptot1(); }

  /*! charge track 1 */
  virtual double get_charge1() const
  { return _vtx_par.get_charge1(); }

  /*! px track 2 */
  virtual double get_px2() const
  { return _vtx_par.get_px2(); }

  /*! py track 1 */
  virtual double get_py2() const
  { return _vtx_par.get_py2(); }

  /*! pz track 2 */
  virtual double get_pz2() const
  { return _vtx_par.get_pz2(); }

  /*! total momentum track 1*/
  virtual double get_ptot2() const
  { return _vtx_par.get_ptot2(); }

  /*! charge track 2 */
  virtual double get_charge2() const
  { return _vtx_par.get_charge2(); }

  /*! z vertex */
  virtual void set_z(double z)
  { _vtx_par.set_z( z ); }

  /*! px track 1 */
  virtual void set_px1(double px1)
  { _vtx_par.set_px1( px1 ); }

  /*! py track 1 */
  virtual void set_py1(double py1)
  { _vtx_par.set_py1( py1 ); }

  /*! pz track 1 */
  virtual void set_pz1(double pz1)
  { _vtx_par.set_pz1( pz1 ); }

  /*! charge track 1 */
  virtual void set_charge1(double charge1)
  { _vtx_par.set_charge1( charge1 ); }

  /*! px track 2 */
  virtual void set_px2(double px2)
  { _vtx_par.set_px2( px2 ); }

  /*! py track 2 */
  virtual void set_py2(double py2)
  { _vtx_par.set_py2( py2 ); }

  /*! pz track 2 */
  virtual void set_pz2(double pz2)
  { _vtx_par.set_pz2( pz2 ); }

  /*! charge track 2 */
  virtual void set_charge2(double charge2)
  { _vtx_par.set_charge2( charge2 ); }

  //! @name Vertex Parameters Reco Units
  //@{

  /*! phi track 1 */
  virtual double get_phi1() const
  { return _vtx_par.get_phi1(); }

  /*! dzds track 1 */
  virtual double get_dzds1() const
  { return _vtx_par.get_dzds1(); }

  /*! q/p track 1 */
  virtual double get_pinv1() const
  { return _vtx_par.get_pinv1(); }

  /*! phi track 2 */
  virtual double get_phi2() const
  { return _vtx_par.get_phi2(); }

  /*! dzds track 2 */
  virtual double get_dzds2() const
  { return _vtx_par.get_dzds2(); }

  /*! q/p track 2 */
  virtual double get_pinv2() const
  { return _vtx_par.get_pinv2(); }
  //@}


  //! @name Merit statistics and Covariance
  //@{
  /*! chi square */
  double get_chi_square() const
  { return _vtx_par.get_chi_square();}

  /*! \brief
    chi square/degrees of freedom
    number of degrees of freedom is 1 by default
    (no external information)
  */
  virtual double get_chi_square_pdf() const
  { return _vtx_par.get_chi_square();}

  /*! covariance matrix */
  double get_covar(UShort_t i, UShort_t j) const
  { return _vtx_par.get_covar( i, j); }

  /*! chi square statistic */
  void set_chi_square(double chi_square)
  { _vtx_par.set_chi_square( chi_square ); }

  /*! covariance matrix */
  void set_covar(UShort_t i, UShort_t j, double val)
  { _vtx_par.set_covar( i, j, val ); }
  //@}

  //! @name Dumpers
  //@{
  //! print vertex
  void print(std::ostream& os = std::cout) const;
  //@}

  //! @name Locators
  //@{
  //! vertex arm
  UShort_t  get_arm() const
  { return _arm; }

  //! vertex arm
  void set_arm(UShort_t arm)
  { _arm = arm; }

  //! vertex index
  UShort_t  get_index() const
  {return _index;}

  //! vertex index
  void set_index(UShort_t index)
  { _index = index; }
  //@}

private:

  //! arm index
  UShort_t _arm;

  //! vertex index
  UShort_t _index;

  /*!
    this variable is obselete
    but cannot be removed for readback compatibility
    the get_sigh method recalculates the sign at each call
    using the track charges
  */
  UShort_t _sign;

  //! vertex parameters
  TMutVtxPar _vtx_par;

  ClassDef(TMutVtx_v1,1)

};

#endif



