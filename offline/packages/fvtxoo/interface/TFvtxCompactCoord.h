// Interface Object Class : TFvtxCompactCoord
// Author: Cesar da Silva
// Date: 03/31/16
// Description: Compact Class for Forward Silicon (FVTX) Coordinate.

#ifndef __TFVTXCOMPACTCOORD_H__
#define __TFVTXCOMPACTCOORD_H__

#include<PHKey.hh>
#include<PHLine.h>
#include<FVTXOO.h>
#include<map>

/*! @ingroup interface */

//!  The Forward Silicon (FVTX) compact coordinate object

class TFvtxCompactCoord : public PHKey
{

 public:

  /*! Enumeration for coordinate status bits */
  enum Status {USEDINTRACK=23};

  enum STRIPBIT {STRIP=0, COLUMN=12, SECTOR=13, STATION=19,
		 CAGE=21, ARM=22};
  //! @name Constructors/Destructors
  //@{
  /*! Default Constructor */
  TFvtxCompactCoord(){;}

  /*!Construct with arm, station, octant, half_octant, gap */
  TFvtxCompactCoord(const Key& key) : PHKey(key) {;}

  /*! Default Destructor */
  virtual ~TFvtxCompactCoord(){;}
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

  /*! Coordinate */
  virtual void set_coord(const PHLine& coord){;}

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
  /*! Index [0,1664] */
  virtual unsigned short  get_index() const {return 0;}

  virtual unsigned int get_peak_strip() const {return 0;}

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
  /*! Index [0,1664] */
  virtual void set_index(unsigned short index) {;}

	//@}
  //! @name TFvtxCompactCoord Status
  //@{
  /*! Set the usedintrack bit */
  virtual void set_usedintrack() {}

  /*! Get the usedintrack bit */
  virtual bool get_usedintrack() const { return 0; }

  //! @name Dumpers
  //@{
  /*! Print object to stream, default stream is std::cout */
  virtual void print(std::ostream& os = std::cout) const{;}
  //@}

  //! @name Interface to non persistent data members
  //
#ifndef __CINT__
  /*! Store chi square increment associated with track */
  //@}
#endif
  ClassDef(TFvtxCompactCoord,1)
};

#endif










