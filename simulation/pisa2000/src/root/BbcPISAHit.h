#ifndef _BBCPISAHIT_
#define _BBCPISAHIT_

// $Id: BbcPISAHit.h,v 1.5 2007/11/13 22:27:43 hpereira Exp $

/*!
  \file  BbcPISAHit.h
  \brief container for BBC pisa hits
  \author  T. K. Ghosh
  \version $Revision: 1.5 $
  \date    $Date: 2007/11/13 22:27:43 $
*/

#include <TObject.h>
#include <vector>

//! container for BBC pisa hits
class BbcPISAHit : public TObject 
{

  private:
  
  //! array of hit position
  Float_t pos[3];        
  
  //! x position of hit 
  Float_t posx;          
  
  //! y position of hit 
  Float_t posy;          
  
  //! z position of hit
  Float_t posz;          
  
  //! momentum of hit 
  Float_t mom[3];        
  
  //! x momentum of hit 
  Float_t momx;          
  
  //! y momentum of hit 
  Float_t momy;          
  
  //! z momentum of hit 
  Float_t momz;          
  
  //! energy loss of hit 
  Float_t del;          
  
  //! time of flight of hit 
  Float_t tof;          
  
  //! path length
  Float_t len;           
  
  //! PMT+1000*Side, Side 1=muon 2=other
  Short_t pmt;           
  
  //! Particle ID
  Short_t pid;           
  
  //! Track in subevent number
  Int_t track;         
  
  //! Subevent number
  Int_t isubevent;     
  
  //! True track number of hit 
  Int_t mctrack;       
  
  //! Input file number for merging of hits
  Int_t nfile;         
  
  //! static interface
  static std::vector<BbcPISAHit> _hits;
  
  public:
  
  //! constructor
  BbcPISAHit(
    Float_t x = 0,
    Float_t y = 0,   
    Float_t z = 0,
    Float_t px = 0,
    Float_t py = 0, 
    Float_t pz = 0,
    Float_t del = 0,
    Float_t tof = 0, 
    Float_t len = 0,
    Short_t pmt = 0, 
    Short_t pid = 0, 
    Int_t track = 0, 
    Int_t isubevent = 0,
    Int_t mctrack = 0, 
    Int_t nfile = 0); 
  
  //! destructor
  virtual ~BbcPISAHit() 
  {}
  
  //!@name static interface 
  //@{
  
  static Int_t GetBbcCount() 
  {return _hits.size(); }
  
  //! push hit
  static void AddHit( const BbcPISAHit& hit ) 
  { _hits.push_back( hit ); }
  
  static BbcPISAHit *GetBbcHitEvt() 
  { return _hits.empty() ? 0: &_hits[0]; }
    
  static void BbcClear()
  { _hits.clear(); }
  
  //@}    

  Float_t get_pos(const int i) const 
  {return pos[i];}
  
  Float_t GetX() const 
  { return pos[0]; }
  
  Float_t GetY() const 
  { return pos[1]; }
  
  Float_t GetZ() const 
  { return pos[2]; }
  
  Float_t get_mom(const int i) const 
  {return mom[i];}
  
  Float_t GetPx() const 
  { return mom[0]; }
  
  Float_t GetPy() const 
  { return mom[1]; }
  
  Float_t GetPz() const 
  { return mom[2]; }
  
  Float_t GetDel() const 
  { return del; }
  
  Float_t GetTof() const 
  { return tof; }
  
  Float_t GetLen() const 
  { return len; }
  
  Short_t GetPmt() const 
  { return pmt; }
  
  Short_t GetPid()  const 
  { return pid; }
  
  Int_t GetNtrack() const
  { return track; }
  
  Int_t GetNfile() const 
  { return nfile; }
  
  void SetNfile(const Int_t file) 
  { nfile = file; }
  
  Int_t GetIsubevent() const 
  { return isubevent; }
  
  void SetMctrack(const Int_t ntrack) 
  { mctrack = ntrack; }
  
  Int_t GetMctrack() const 
  { return mctrack; }
  
  ClassDef(BbcPISAHit,1) 
};

#endif
