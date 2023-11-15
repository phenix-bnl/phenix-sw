// $Id: PdbMuiTubeEff.hh,v 1.3 2007/04/05 07:59:53 hpereira Exp $

#ifndef PdbMuiTubeEff_h
#define PdbMuiTubeEff_h

/*!
  \file    PdbMuiTubeEff.hh
  \brief   database container for muid tube efficiencies
  \author  J. Newby, H. Pereira
  \version $Revision: 1.3 $
  \date    $Date: 2007/04/05 07:59:53 $
*/

#include "PdbCalChan.hh"

//! database container for muid tube efficiencies
class PdbMuiTubeEff : public PdbCalChan 
{
  
  public:
  
  //! constructor
  /*! 
    note that the default value for entries is 1, this to make the
    the calculation of an average efficiency possible, even if the 
    entries was not set when writting to the DB
  */
  PdbMuiTubeEff():	
    _arm(0),
    _plane(0),
    _panel(0),
    _orientation(0),
    _twopack(0),
    _eff(0.0),
    _entries(1)
  {}
  
  //! destructor
  virtual ~PdbMuiTubeEff()
  {}
  
  //! print object
  virtual void print() const;
  
  //!@name accessors
  //@{
  
  //! arm
  const short& arm() const 
  { return _arm; }
  
  //! plane
  const short& plane() const 
  { return _plane; }
  
  //! panel
  const short& panel() const 
  { return _panel; }
  
  //! orientation (0: horizonta, 1: vertical)
  const short& orientation() const 
  { return _orientation; }
  
  //! twopack
  const short& twopack() const 
  { return _twopack; }
  
  //! efficiency
  const float& eff() const 
  { return _eff; }
  
  /*! entries */
  /*! 
    the number of entries is used to quantify the accuracy of the 
    measurement and calculate an error on the estimated efficiency
  */
  const unsigned int& entries( void ) const
  { return _entries; }
  
  //@}
  
  //!@name modifiers
  //@{
  
  //! arm
  void set_arm(const short& newVal) 
  { _arm = newVal; }
  
  //! plane
  void set_plane(const short& newVal) 
  { _plane = newVal; }
  
  //! panel
  void set_panel(const short& newVal) 
  { _panel = newVal; }
  
  //! orientation (0: horizonta, 1: vertical)
  void set_orientation(const short& newVal) 
  { _orientation = newVal; }
  
  //! twopack
  void set_twopack(const short& newVal) 
  { _twopack = newVal; }
  
  //! efficiency
  void set_eff(const float& newVal)
  { _eff = newVal; }
  
  /*! entries */
  /*! 
    the number of entries is used to quantify the accuracy of the 
    measurement and calculate an error on the estimated efficiency
  */
  void set_entries( const unsigned int& entries )
  { _entries = entries; }
  
  //@}
  
  private:

  /*! arm */
  short _arm;
  
  /*! plane */
  short _plane;
  
  /*! panel */
  short _panel;
  
  /*! orientation */
  short _orientation;
  
  /*! two pack */
  short _twopack;
  
  /*! efficiency */
  float _eff;
  
  /*! entries */
  /*! 
    the number of entries is used to quantify the accuracy of the 
    measurement and calculate an error on the estimated efficiency
  */
  unsigned int _entries;
  
  ClassDef(PdbMuiTubeEff,2);
  
};

#endif /* __PDBMUITUBEEFF_HH__ */
