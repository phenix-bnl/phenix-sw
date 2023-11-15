// $Id: PdbFvtxAlignment.hh,v 1.2 2013/11/05 00:02:32 jinhuang Exp $                                                                                             

/*!
 * \file PdbFvtxAlignment.hh
 * \brief 
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $Revision: 1.2 $
 * \date $Date: 2013/11/05 00:02:32 $
 */

#ifndef PDBFVTXALIGNMENT_HH_
#define PDBFVTXALIGNMENT_HH_

class TGeoManager;

#include "PdbCalChan.hh"

/*!
 * \brief PdbFvtxAlignment
 *
 * Save FVTX geometry to database. Note that the geometry object is not owned by this class,
 * since it will register itself as gGeoManager and removed automatically, e.g. when a new TGeoManager
 * is loaded.
 *
 * This class is now obsolete since stream a complex ROOT object to PHENIX database is not encouraged
 */
class PdbFvtxAlignment : public PdbCalChan
{
public:
  PdbFvtxAlignment();
  virtual
  ~PdbFvtxAlignment();

  //! WARNING : the new pointer do not belong to this class,
  //! the TGeoManager should be only deleted after PdbFvtxAlignment is deleted or set to another Geometry object
  void
  SetGeometry(TGeoManager * g);

  TGeoManager *
  GetGeometry();

  virtual void
  print() const;

private:

  //! save the full geometry in database
  TGeoManager * _fGeom;

ClassDef(PdbFvtxAlignment,1)

};

#endif /* PDBFVTXALIGNMENT_HH_ */
