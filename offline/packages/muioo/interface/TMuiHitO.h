// $Id: TMuiHitO.h,v 1.1 2006/04/22 01:58:28 hpereira Exp $

#ifndef _TMUIHITO_H_
#define _TMUIHITO_H_

/*!
	\file TMuiHitO.h
	\brief Interface Object Class : TMuiHitO
	\author Jason Newby
  \version $Revision: 1.1 $
  \date    $Date: 2006/04/22 01:58:28 $
*/

// CINT compatible headers//
//
#include<TDataType.h>
#include<PHKey.hh>
#include<PHException.h>
#include<PHPoint.h>

/*! @ingroup interface */
//!  The MUID Hit Interface Object 

/*! 
  <b>The MUID Hit Interface Object </b><br>

  The MUID hit object presents an interface to
  some data members of old framework mui road object.

  The purpose for having this interface object is the translate old frame 
  work Hit road into a new framework object.
  <p>
*/

class TMuiHitO : public PHKey {

 public:

  //! @name Constructors/Destructors
  //@{  
  
  /*! Default constructor */
  TMuiHitO()
  {;}  
  
  /*! Default destructor */  
  virtual ~TMuiHitO()
  {;}
  
  /*! Default constructor */  
  TMuiHitO(const Key& key) :
    PHKey(key)
    {;}
  
  /*! Construct with key and location */
  TMuiHitO(const Key&,
	   UShort_t arm,
	   UShort_t plane,
	   UShort_t panel,
	   UShort_t orientation,
	   UShort_t twopack,
	   UShort_t index)
     {;}
  
  //! @name Functional Interface
  //@{
  //@}

  //! @name Locators
  //@{
  /*! Arm [0,1] */
  virtual void set_arm( UShort_t arm) 
  {}

  /*! Plane [0,4] */
  virtual void set_plane( UShort_t plane) 
  {}

  /*! Panel [0,5] */
  virtual void set_panel( UShort_t panel) 
  {}

  /*! Orientation[0,1] */
  virtual void set_orientation( UShort_t orientation) 
  {}

  /*! TwoPack[0,64] */
  virtual void set_twopack( UShort_t twopack) 
  {}

  /*! Arm [0,1] */
  virtual UShort_t get_arm() const 
  {return 0;}

  /*! Plane [0,4] */
  virtual UShort_t get_plane() const 
  {return 0;}

  /*! Panel [0,5] */
  virtual UShort_t get_panel() const 
  {return 0;}

  /*! Orientation[0,1] */
  virtual UShort_t get_orientation() const 
  {return 0;}

  /*! TwoPack[0,64] */
  virtual UShort_t get_twopack() const 
  {return 0;}

  /*! Index */
  virtual UShort_t get_index() const 
  {return 0;}

  //@}

  //! @name Dumpers
  //@{  

  virtual void print(std::ostream& os = std::cout) const 
  {}

  //@}

  ClassDef(TMuiHitO,1)

    };

#endif /* _TMUIHITO_H_ */
	      








	
