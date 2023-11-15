#ifndef _AerPISAHit_
#define _AerPISAHit_

// $Id: AerPISAHit.h,v 1.4 2007/11/13 22:27:43 hpereira Exp $

/*!
  \file  AerPISAHit.h
  \brief container for aerogel pisa hits
  \author  T. K. Ghosh
  \version $Revision: 1.4 $
  \date    $Date: 2007/11/13 22:27:43 $
*/

#include <TObject.h>
#include <vector>

//! container for aerogel pisa hits
class AerPISAHit : public TObject 
{

  private:
  Int_t mctrack; 
  Int_t idPart;  
  Float_t xyzglobal[3] ;
  Float_t dele; 
  Float_t pmomxyz[3];
  Int_t track;   
  
  //! AER layer (*not* counting from 0!!)
  Int_t layer;   
  
  Float_t pathLength;
  Float_t tof;
  Float_t stepLength;
  Float_t etot;
  Float_t charge;
  Float_t momentum;
  Float_t vertxyz[3];
  Int_t isubevent; 
  Int_t nfile;
    
  //! static interface
  static std::vector<AerPISAHit> _hits;
  
  public:
  
  //! default constructor
  AerPISAHit();
  
  //! constructor
  AerPISAHit(
    Float_t xyzglobal[], Float_t pmomxyz[],
    Float_t dele, Int_t track, Int_t layer, Int_t aerID,
    Float_t pathLength, Float_t tof, Float_t stepLength, Float_t etot,
    Float_t charge, Float_t momentum, Float_t vertxyz[], Int_t isubevent,
    Int_t mctrack, Int_t nfile); 
  
  virtual ~AerPISAHit() 
  {}
 
  //!@name static interface 
  //@{
  
  static Int_t GetAerCount() 
  {return _hits.size(); }
  
  //! push hit
  static void AddHit( const AerPISAHit& hit ) 
  { _hits.push_back( hit ); }
  
  static AerPISAHit *GetAerHitEvt() 
  { return _hits.empty() ? 0: &_hits[0]; }
    
  static void AerClear()
  { _hits.clear(); }
  
  //@}
  
  Int_t GetIsubevent() const 
  { return isubevent; }
  
  Int_t GetNtrack() const 
  { return track; }
  
  void SetIdPart(const Int_t aerID) 
  { idPart = aerID; }
  
  Int_t GetIdPart() const
  { return idPart; }
  
  void SetMctrack(const Int_t ntrack) 
  { mctrack = ntrack; }
  
  Int_t GetMctrack() const 
  { return mctrack; }
  
  Int_t GetLayer() const 
  { return layer; }
  
  Float_t GetX() const 
  { return xyzglobal[0]; }
  
  Float_t GetY() const 
  { return xyzglobal[1]; }
  
  Float_t GetZ() const 
  { return xyzglobal[2]; }
  
  Float_t GetDele() const
  { return dele; }
  
  Float_t GetPathLength() const 
  { return pathLength; }
  
  Float_t GetTOF() const 
  { return tof; }
  
  Float_t GetStepLength() const 
  { return stepLength; }
  
  Float_t GetEtot() const 
  { return etot; }
  
  Float_t GetCharge() const 
  { return charge; }
  
  Float_t GetPtot() const 
  { return momentum; }
  
  Float_t GetVertX() const 
  { return vertxyz[0]; }
  
  Float_t GetVertY() const
  { return vertxyz[1]; }
  
  Float_t GetVertZ() const 
  { return vertxyz[2]; }
  
  Float_t GetPx() const
  { return pmomxyz[0]; }
  
  Float_t GetPy() const 
  { return pmomxyz[1]; }
  
  Float_t GetPz() const 
  { return pmomxyz[2]; }
  
  Int_t GetNfile() const 
  { return nfile; }
  
  void SetNfile(const Int_t file) 
  { nfile = file; }
  
  ClassDef(AerPISAHit,2)
};

#endif
