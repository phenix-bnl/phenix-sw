// $Id: MutGeom.h,v 1.11 2010/05/07 18:24:38 hpereira Exp $
#ifndef MutGeom_h
#define MutGeom_h

/*!
   \file    MutGeom.h
   \brief   Muon tracker Geometry
   \author  Hugo Pereira
   \version $Revision: 1.11 $
   \date    $Date: 2010/05/07 18:24:38 $
*/

#include "MutDCMChannelMap.h"
#include "MutArm.h"
#include "MUTGEOM.h"

#include <cstdio>

//! forward declaration
class MutPISAPara;

//! pointer to south arm singleton
/*! the arm geometry is initialized at first call */
MutArm* SouthArm();

//! pointer to north arm
/*! the arm geometry is initialized at first call */
MutArm* NorthArm();

//! pointer to south arm DCM channels
/*! the map is initialized at first call */
MutDCMChannelMap* SouthArmChannelMap();

//! pointer to north arm DCM channels
/*! the map is initialized at first call */
MutDCMChannelMap* NorthArmChannelMap();

//! stores pointers to the muon geometry
class MutGeom
{

  public:

  //! retrieve singleton
  static MutGeom& get( void )
  {
    static MutGeom geom;
    return geom;
  }

  //! set mutr PISA parameter
  /*!
    must be called before any of the other method if to be taken
    into account at arm initialization
  */
  void set_pisa_parameters( MutPISAPara* in )
  { _mut_pisa_parameters = in; }

  //! reset geometry
  void reset( void );

  //! pointer to given arm singleton
  MutArm* get_arm( int arm_number )
  {
    if( arm_number == MUTGEOM::South ) return south_arm();
    else if( arm_number == MUTGEOM::North ) return north_arm();
    else
    {
      std::cout << "MutGeom::arm - invalid index " << arm_number << std::endl;
      return 0;
    }
  }

  //! pointer to south arm singleton
  /*! the arm geometry is initialized at first call */
  MutArm* south_arm();

  //! pointer to north arm
  /*! the arm geometry is initialized at first call */
  MutArm* north_arm();

  //! pointer to south arm DCM channels
  /*! the map is initialized at first call */
  MutDCMChannelMap* south_dcm_map();

  //! pointer to north arm DCM channels
  /*! the map is initialized at first call */
  MutDCMChannelMap* north_dcm_map();

  //! destructor
  ~MutGeom( void );

  private:

  //! constructor
  MutGeom( void );

  //! initialize arm
  void initialize_arm( MutArm*, PHTimeStamp, PdbBankID ) const;

  //! south arm
  MutArm* _south_arm;

  //! north arm
  MutArm* _north_arm;

  //! south arm DCM map
  MutDCMChannelMap* _south_dcm_map;

  //! north arm DCM map
  MutDCMChannelMap* _north_dcm_map;

  //! Mutr PISA parameter (for MC Geometry initialization)
  MutPISAPara* _mut_pisa_parameters;

};

#endif /*__MUTGEOM_H__*/
