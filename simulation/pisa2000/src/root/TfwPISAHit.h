#ifndef _TfwPISAHit_
#define _TfwPISAHit_

// $Id: TfwPISAHit.h,v 1.3 2007/11/13 22:27:49 hpereira Exp $

/*!
\file  TfwPISAHit.h
\brief container for time of flight west pisa hits
\author  C. F. Maguire
\version $Revision: 1.3 $
\date    $Date: 2007/11/13 22:27:49 $
*/

#include <TObject.h>
#include <vector>

//! container for time of flight west pisa hits
class TfwPISAHit : public TObject 
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
  
  //! track in subevent
  Int_t   track;  
  Int_t   panel;
  Int_t   isubevent; 
  Int_t   nfile;
  
  //! static interface
  static std::vector<TfwPISAHit> _hits;
  
  public:
  
  //! default constructor
  TfwPISAHit();
  
  //! constructor
  TfwPISAHit(
    Float_t xyzinloc[], Float_t xyzoutloc[], Float_t xyzinglo[],
    Float_t tof, Float_t dedx, Float_t pathLength, Int_t track,
    Int_t panel, Int_t id,
    Int_t isubevent,  Int_t mctrack, Int_t nfile); 
  
  //! destructor
  virtual ~TfwPISAHit() 
  {}
  
  //!@name static interface 
  //@{
  
  static Int_t GetTfwCount() 
  {return _hits.size(); }
  
  //! push hit
  static void AddHit( const TfwPISAHit& hit ) 
  { _hits.push_back( hit ); }
  
  static TfwPISAHit *GetTfwHitEvt() 
  { return _hits.empty() ? 0: &_hits[0]; }
  
  static void TfwClear()
  { _hits.clear(); }
  
  //@}
  
  void SetPanel(const Int_t ipanel) 
  { panel = ipanel; }
  
  Int_t GetPanel() const 
  { return panel; }
  
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
  
  ClassDef(TfwPISAHit,1)
    
};

#endif
