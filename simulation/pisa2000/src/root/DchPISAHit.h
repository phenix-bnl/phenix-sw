#ifndef _DchPISAHit_
#define _DchPISAHit_

// $Id: DchPISAHit.h,v 1.5 2007/11/13 22:27:44 hpereira Exp $

/*!
\file  DchPISAHit.h
\brief container for drift chambers pisa hits
\author  T. K. Ghosh
\version $Revision: 1.5 $
\date    $Date: 2007/11/13 22:27:44 $
*/

#include <TObject.h>
#include <vector>

//! container for drift chambers pisa hits
class DchPISAHit : public TObject 
{
  
  private:
  
  Int_t iArm;
  Int_t id;
  Float_t xyzinloc[3];
  Float_t tof;
  Float_t xyzoutloc[3];
  Int_t plane;
  Int_t mctrack;
  Int_t cell;
  Float_t xyzinglo[3];
  Float_t pathLength;
  Int_t track;
  Int_t isubevent;
  Int_t nfile;
  
  //! static interface
  static std::vector<DchPISAHit> _hits;
  
  public:
  
  //! default constructor
  DchPISAHit();
  
  //! constructor
  DchPISAHit(
    Float_t xyzinloc[], Float_t tof,  Float_t xyzoutloc [], 
    Int_t plane, Int_t cell, Float_t xyzinglo[],
    Float_t pathLength, Int_t track, Int_t isubevent,
    Int_t iArm, Int_t id, Int_t mctrack, Int_t nfile); 
  
  //! destructor
  virtual ~DchPISAHit() 
  {}
  
  //!@name static interface 
  //@{
  
  static Int_t GetDchCount() 
  {return _hits.size(); }
  
  //! push hit
  static void AddHit( const DchPISAHit& hit ) 
  { _hits.push_back( hit ); }
  
  static DchPISAHit *GetDchHitEvt() 
  { return _hits.empty() ? 0: &_hits[0]; }
  
  static void DchClear()
  { _hits.clear(); }
  
  //@}
  
  void SetId(const Int_t seqnum) 
  { id = seqnum; }
  
  Int_t GetId() const 
  { return id; }
  
  Float_t get_xyzinloc(const int i) const 
  {return xyzinloc[i];}
  
  Float_t GetXin() const 
  { return xyzinloc[0]; }
  
  Float_t GetYin() const 
  { return xyzinloc[1]; }
  
  Float_t GetZin() const 
  { return xyzinloc[2]; }
  
  Float_t GetTof() const 
  { return tof; }
  
  Float_t get_xyzoutloc(const int i) const 
  {return xyzoutloc[i];}
  
  Float_t GetXout() const 
  { return xyzoutloc[0]; }
  
  Float_t GetYout() const 
  { return xyzoutloc[1]; }
  
  Float_t GetZout() const 
  { return xyzoutloc[2]; }
  Int_t GetIarm() const 
  { return iArm; }
  
  Int_t GetPlane() const 
  { return plane; }
  
  Int_t GetNtrack() const
  { return track; }
  
  Int_t GetIsubevent() const 
  { return isubevent; }
  
  void SetMctrack(const Int_t ntrack) 
  { mctrack = ntrack; }
  
  Int_t GetMctrack() const
  { return mctrack; }
  
  Int_t GetCell() const 
  { return cell; }
  
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
  
  ClassDef(DchPISAHit,1) 
};

#endif


