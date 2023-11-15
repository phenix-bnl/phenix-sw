#ifndef _EMCPISAPARA_
#define _EMCPISAPARA_

// $Id: EmcPISAPara.h,v 1.3 2007/11/13 22:27:50 hpereira Exp $

/*!
  \file  EmcPISAPara.h
  \brief container for EMC pisa parameters
  \author  T. K. Ghosh
  \version $Revision: 1.3 $
  \date    $Date: 2007/11/13 22:27:50 $
*/


#include "TObject.h"
#include "TClonesArray.h"


const Int_t EMC_PAR_ROWS = 80;  

#include <vector>

class EmcPISAPara : public TObject 
{

  private:
  Float_t udetpar[80];
  
  //! static interface
  static std::vector<EmcPISAPara> _hits;

  public:
  
  //! empty constructor
  EmcPISAPara ();
  
  //! constructor
  EmcPISAPara(const Float_t upar[]);
  
  void GetEmcPar(Float_t upar[]);

  //! destructor
  virtual ~EmcPISAPara() 
  {}
  
  //!@name static interface
  //@{
  static Int_t GetEmcParaCount() 
  { return _hits.size(); }
  
  static EmcPISAPara* GetEmcParaEvt() 
  {return _hits.empty() ? 0:&_hits[0]; }
  
  static void AddHit( const EmcPISAPara& hit )
  { _hits.push_back( hit ); }
  
  static void EmcParaClear()
  { _hits.clear(); }
  
  //@}

  ClassDef(EmcPISAPara,1)  // A EMC parameter instance

};

#endif








