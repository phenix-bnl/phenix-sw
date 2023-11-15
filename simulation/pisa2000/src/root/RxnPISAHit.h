#ifndef _RxnPISAHit_
#define _RxnPISAHit_

// $Id: RxnPISAHit.h,v 1.4 2007/11/13 22:27:48 hpereira Exp $

/*!
\file  RxnPISAHit.h
\brief container for reaction plane detector pisa hits
\author  C. F. Maguire
\version $Revision: 1.4 $
\date    $Date: 2007/11/13 22:27:48 $
*/

#include <TObject.h>
#include <vector>

//! container for reaction plane detector pisa hits
class RxnPISAHit : public TObject
{
  
  private:
  Int_t   id;
  Int_t   mctrack;
  Float_t xyzinloc[3] ;
  Float_t xyzoutloc[3];
  Float_t tof ;
  Float_t dedx; 
  Float_t xyzinglo[3];
  Float_t pathLength;
  Float_t pmomxyz[3];
  
  //! track in subevent
  Int_t   track;   
  Int_t   arm;
  Int_t   isubevent; 
  Int_t   nfile;
  
  //! static interface
  static std::vector<RxnPISAHit> _hits;
  
  public:
  
  //! default constructor
  RxnPISAHit();
  
  //! constructor
  RxnPISAHit(
    Float_t xyzinloc[], Float_t xyzoutloc[], Float_t xyzinglo[], Float_t pmomxyz[],
    Float_t tof, Float_t dedx, Float_t pathLength, Int_t track,
    Int_t arm, Int_t id,
    Int_t isubevent,  Int_t mctrack, Int_t nfile); 
  
  //! destructor
  virtual ~RxnPISAHit() 
  {}
  
  //!@name static interface 
  //@{
  
  static Int_t GetRxnCount() 
  {return _hits.size(); }
  
  //! push hit
  static void AddHit( const RxnPISAHit& hit ) 
  { _hits.push_back( hit ); }
  
  static RxnPISAHit *GetRxnHitEvt() 
  { return _hits.empty() ? 0: &_hits[0]; }
  
  static void RxnClear()
  { _hits.clear(); }
  
  //@}  
  
  void SetArm(const Int_t iarm) 
  { arm = iarm; }
  
  Int_t GetArm() const 
  { return arm; }
  
  Int_t GetIsubevent() const 
  { return isubevent; }
  
  Int_t GetNtrack() const 
  { return track; }
  
  void SetId(const Int_t pcID) 
  { id = pcID; }
  
  Int_t GetId() const 
  { return id; }
  
  void SetMctrack(const Int_t ntrack)
  { mctrack = ntrack; }
  
  Int_t GetMctrack() const
  { return mctrack; }
  
  Float_t get_xyzinloc(const int i) const 
  {return xyzinloc[i];}
  
  Float_t GetXin() const 
  { return xyzinloc[0]; }
  
  Float_t GetYin() const 
  { return xyzinloc[1]; }
  
  Float_t GetZin() const 
  { return xyzinloc[2]; }
  
  Float_t get_xyzoutloc(const int i) const 
  {return xyzoutloc[i];}
  
  Float_t GetXout() const 
  { return xyzoutloc[0]; }
  
  Float_t GetYout() const 
  { return xyzoutloc[1]; }
  
  Float_t GetZout() const 
  { return xyzoutloc[2]; }
  
  Float_t GetTof() const 
  { return tof; }
  
  Float_t GetDedx() const
  { return dedx; }
  
  Float_t get_xyzinglo(const int i) const 
  {return xyzinglo[i];}
  
  Float_t GetXing() const
  { return xyzinglo[0]; }
  
  Float_t GetYing() const 
  { return xyzinglo[1]; }
  
  Float_t GetZing() const 
  { return xyzinglo[2]; }
  
  Float_t GetPathLength() const 
  { return pathLength; }
  
  Int_t GetNfile() const
  { return nfile; }
  
  void SetNfile(const Int_t file) 
  { nfile = file; }
  
  Float_t GetPx() const 
  { return pmomxyz[0]; }
  
  Float_t GetPy() const 
  { return pmomxyz[1]; }
  
  Float_t GetPz() const 
  { return pmomxyz[2]; }
  
  ClassDef(RxnPISAHit,1)
};

#endif
