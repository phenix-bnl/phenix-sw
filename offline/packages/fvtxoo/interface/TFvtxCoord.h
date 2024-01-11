// Interface Object Class : TFvtxCoord
// Author: M. Brooks
// Date: 07/11/06
// Description: Class for Forward Silicon (FVTX) Coordinate.

#ifndef __TFVTXCOORD_H__
#define __TFVTXCOORD_H__

#include<PHKey.hh>
#include<PHLine.h>
#include<FVTXOO.h>
#include<map>

/*! @ingroup interface */

//!  The Forward Silicon (FVTX) coordinate object
/*!  The Forward Silicon (FVTX) coordinate object */

class TFvtxCoord : public PHKey
{

 public:

  /*! Enumeration for coordinate status bits */
  enum Status {USEDINTRACK};

  //! @name Constructors/Destructors
  //@{
  /*! Default Constructor */
  TFvtxCoord(){;}

  /*!Construct with arm, station, octant, half_octant, gap */
  TFvtxCoord(const Key& key) : PHKey(key) {;}

  /*! Default Destructor */
  virtual ~TFvtxCoord(){;}
  //@}

  //! @name Functional Interface
  //@{
  /*! Return coordinate */
  virtual PHLine get_coord() const{return PHLine();}
  /*! Return end point */
  virtual PHPoint get_coord_end() const { return PHPoint();}
  /*! Return starting point */
  virtual PHPoint get_coord_begin() const { return PHPoint();}

  /*! Return mid-point */
  virtual PHPoint get_coord_midpoint() const;

  /*! Return mean z coord */
  virtual double get_mean_z() const { return 0;}
  /*! Charge associated with peak strip */
  virtual Float_t get_q_peak() const {return 0;}
  /*! Total charge associated with centroid fit*/
  virtual Float_t get_q_tot() const {return 0;}
  /*! Peak strip */
  virtual unsigned short get_peak_strip() const {return 0;}
  /*! Offset from peak strip (cm) */
  virtual Float_t get_w() const {return 0;}

  /*! Offset from the 0th strip (cm) */
  virtual double get_w_absolute() const;

  /*! Coordinate error  */
  virtual Float_t get_error() const {return 0;}
  /*! Error on fitted total charge */
  virtual double get_q_error()const {return 0;}

  /*! Coordinate */
  virtual void set_coord(const PHLine& coord){;}
  /*! Charge associated with peak strip */
  virtual void set_q_peak(Float_t q_peak) {;}
  /*! Total charge associated with this centroid */
  virtual void set_q_tot(Float_t q_tot) {;}
  /*! Peak strip */
  virtual void set_peak_strip(unsigned short peak_strip) {;}
  /*! Offset from peak strip (cm) */
  virtual void set_w(Float_t w) {;}
  /*! Coordinate error */
  virtual void set_error(double error){;}
  /*! Error on fitted total charge */
  virtual void set_q_error(double q_error){;}
  //@}

  //! @name Locators
  //@{
  /*! Arm [0,1] */
  virtual unsigned short  get_arm() const {return 0;}
  /*! Cage [0,1] */
  virtual unsigned short  get_cage() const {return 0;}
  /*! Station [0,2] */
  virtual unsigned short  get_station() const {return 0;}
  /*! Sector[0,23] */
  virtual unsigned short  get_sector() const {return 0;}
  /*! Plane[0,1] */
  virtual unsigned short  get_column() const {return 0;}
  /*! Index [0,1023] */
  virtual unsigned short  get_index() const {return 0;}
  /*! Arm [0,1] */
  virtual void set_arm(unsigned short arm){;}
  /*! Cage [0,1] */
  virtual void set_cage(unsigned short cage){;}
  /*! Station [0,2] */
  virtual void set_station(unsigned short station){;}
  /*! Sector [0,24] */
  virtual void set_sector(unsigned short sector){;}
  /*! radius[0,1] */
  virtual void set_column(unsigned short column){;}
  /*! Index [0,1023] */
  virtual void set_index(unsigned short index) {;}

	//@}
  //! @name TFvtxCoord Status
  //@{
  /*! Set the usedintrack bit */
  virtual void set_usedintrack() {}

  /*! Get the usedintrack bit */
  virtual bool get_usedintrack() const { return 0; }

  /*! Set the status word */
  virtual void set_status(unsigned long status) {;}

  /*! Get the status word */
  virtual unsigned long get_status() const { return 0;}

  /*! Clear the status word */
  virtual void clear_status() {}
  //@}

  //! @name Dumpers
  //@{
  /*! Print object to stream, default stream is std::cout */
  virtual void print(std::ostream& os = std::cout) const{;}
  //@}

  //! @name Interface to non persistent data members
  //
#ifndef __CINT__
  /*! Store chi square increment associated with track */
  virtual void push_chi_sqr_inc(unsigned long track_key, double chi_square){;}
  /*! Retrieve chi square increment associated with track */
  virtual double get_chi_sqr_inc(unsigned long track_key){return 0;}
  //@}
#endif
  ClassDef(TFvtxCoord,1)
};

#endif










