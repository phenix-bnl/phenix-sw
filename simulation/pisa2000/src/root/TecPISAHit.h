#ifndef _TecPISAHit_
#define _TecPISAHit_

// $Id: TecPISAHit.h,v 1.4 2007/11/13 22:27:48 hpereira Exp $

/*!
\file  TecPISAHit.h
\brief container for time expansion chamber pisa hits
\author  T. K. Ghosh
\version $Revision: 1.4 $
\date    $Date: 2007/11/13 22:27:48 $
*/

#include <TObject.h>
#include <vector>

//! container for time expansion chamber pisa hits
class TecPISAHit : public TObject 
{
  
  private:
  Int_t iArm;
  Int_t nfile;
  Int_t id;
  Int_t plane;
  Int_t isubevent;
  Int_t mctrack;
  Int_t track;
  Float_t xyzinloc[3];
  Float_t xyzoutloc[3];
  Float_t tof;
  Int_t sector;
  Float_t dedx;
  Float_t xyzinglo[3];    
  
  //! static interface
  static std::vector<TecPISAHit> _hits;
  
  public:
  
  //! default constructor
  TecPISAHit();
  
  //! constructor
  TecPISAHit( 
    Int_t mctrack, Float_t xyzinloc[], Float_t xyzoutloc [], Float_t tof, 
    Int_t sector, Float_t dedx, Float_t xyzinglo[], Int_t iArm, Int_t nfile, 
    Int_t id, Int_t plane, Int_t isubevent, Int_t track ); 
  
  //! destructor
  virtual ~TecPISAHit() 
  {}
  
  //!@name static interface 
  //@{
  
  static Int_t GetTecCount() 
  {return _hits.size(); }
  
  //! push hit
  static void AddHit( const TecPISAHit& hit ) 
  { _hits.push_back( hit ); }
  
  static TecPISAHit *GetTecHitEvt() 
  { return _hits.empty() ? 0: &_hits[0]; }
  
  static void TecClear()
  { _hits.clear(); }
  
  //@}
  
  Float_t GetXin() const 
  { return xyzinloc[0]; }
  
  Float_t GetYin() const 
  { return xyzinloc[1]; }
  
  Float_t GetZin() const 
  { return xyzinloc[2]; }
  
  Float_t GetTof() const
  { return tof; }
  
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
  
  void SetId(const Int_t idTec)
  { id = idTec; }
  
  Int_t GetId() const 
  { return id; }
  
  void SetMctrack(const Int_t ntrack) 
  { mctrack = ntrack; }
  
  Int_t GetMctrack() const
  { return mctrack; }
  
  Int_t GetSector() const 
  { return sector; }
  
  Float_t GetXing() const 
  { return xyzinglo[0]; }
  
  Float_t GetYing() const 
  { return xyzinglo[1]; }
  
  Float_t GetZing() const 
  { return xyzinglo[2]; }
  
  Float_t GetDeDx() const 
  { return dedx; }
  
  Int_t GetNfile() const 
  { return nfile; }
  
  void SetNfile(const Int_t file)
  { nfile = file; }
  
  ClassDef(TecPISAHit,1)
};


#endif
