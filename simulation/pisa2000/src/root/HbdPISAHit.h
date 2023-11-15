#ifndef _HbdPISAHit_
#define _HbdPISAHit_

// $Id: HbdPISAHit.h,v 1.4 2007/11/13 22:27:44 hpereira Exp $

/*!
\file  HbdPISAHit.h
\brief container for hadron blind detector pisa hits
\author  H. Pereira
\version $Revision: 1.4 $
\date    $Date: 2007/11/13 22:27:44 $
*/

#include <TObject.h>
#include <vector>

//! container for hadron blind detector pisa hits
class HbdPISAHit : public TObject 
{
  
  private:
  
  //! Track number filled after read of PISA hits files
  Int_t   mctrack; 
  Float_t xyzin[3] ;
  Float_t pxyz[3];
  Float_t tof;
  
  //! GEANT particle ID assigned from hbdID
  Int_t idPart;  
  
  //! track in subevent
  Int_t track;   
  Float_t xyzout[3] ;
  Float_t dele; 
  Float_t pathLength;
  Int_t detector;
  Int_t sector;
  Int_t padrow;
  Int_t detflag;
  Int_t isubevent; 
  Int_t nfile;
  
  //! static interface
  static std::vector<HbdPISAHit> _hits;
  
  
  public:
  
  //! default constructor
  HbdPISAHit();
  
  //! constructor
  HbdPISAHit(
    Float_t xyzin[], Float_t pxyz[],
    Float_t tof, Int_t hbdID, Int_t track,
    Float_t xyzout[], Float_t dele,  
    Float_t pathLength, 
    Int_t detector, Int_t sector, Int_t padrow,
    Int_t detflag,
    Int_t isubevent,  
    Int_t mctrack, Int_t nfile);
  
  //! destructor
  virtual ~HbdPISAHit() 
  {}
  
  //!@name static interface 
  //@{
  
  static Int_t GetHbdCount() 
  {return _hits.size(); }
  
  //! push hit
  static void AddHit( const HbdPISAHit& hit ) 
  { _hits.push_back( hit ); }
  
  static HbdPISAHit *GetHbdHitEvt() 
  { return _hits.empty() ? 0: &_hits[0]; }
  
  static void HbdClear()
  { _hits.clear(); }
  
  //@} 
  
  Int_t GetIsubevent() const 
  { return isubevent; }
  
  Int_t GetNtrack() const 
  { return track; }
  
  void SetIdPart(const Int_t hbdID) 
  { idPart = hbdID; }
  
  Int_t GetIdPart() const 
  { return idPart; }
  
  void SetMctrack(const Int_t ntrack) 
  { mctrack = ntrack; }
  
  Int_t GetMctrack() const 
  { return mctrack; }
  
  Float_t GetXin() const 
  { return xyzin[0]; }
  
  Float_t GetYin() const 
  { return xyzin[1]; }
  
  Float_t GetZin() const 
  { return xyzin[2]; }
  
  Float_t GetXout() const 
  { return xyzout[0]; }
  
  Float_t GetYout() const 
  { return xyzout[1]; }
  
  Float_t GetZout() const
  { return xyzout[2]; }
  
  Float_t GetDele() const
  { return dele; }
  
  Float_t GetPathLength() const
  { return pathLength; }
  
  Float_t GetTOF() const 
  { return tof; }
  
  Float_t GetPx() const 
  { return pxyz[0]; }
  
  Float_t GetPy() const
  { return pxyz[1]; }
  
  Float_t GetPz() const 
  { return pxyz[2]; }
  
  Int_t GetNfile() const 
  { return nfile; }
  
  void SetNfile(const Int_t file) 
  { nfile = file; }
  
  Int_t GetDet() const 
  { return detector; }
  
  Int_t GetSect() const
  { return sector; }
  
  Int_t GetPR() const
  { return padrow; }
  
  Int_t GetDetFlag() const 
  { return detflag; }
  
  ClassDef(HbdPISAHit,1)
};

#endif
