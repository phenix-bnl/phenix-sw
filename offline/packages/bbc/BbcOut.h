#ifndef __BBCOUT_H
#define __BBCOUT_H

#include "PHObject.h"

///
class BbcOut: public PHObject
{
 public:
  ///
  virtual ~BbcOut() {}
  
  /** identify Function from PHObject
      @param os Output Stream 
   */
  virtual void identify(std::ostream& os = std::cout) const; 
  /// Clear Event
  virtual void Reset();

  /// isValid returns non zero if object contains vailid data
  virtual int isValid() const;

  /// get ZVertex determined by Bbc
  virtual float get_VertexPoint() const;

  /// get Error on ZVertex determined by Bbc
  virtual float get_dVertexPoint() const;

  /// get T0 determined by Bbc
  virtual float get_TimeZero() const;

  /// get Error on T0 determined by Bbc
  virtual float get_dTimeZero() const;

  /** set T0, Error on T0, ZVertex and Error on ZVertex
      @param t0 Bbc T0
      @param t0err Bbc Error on T0
      @param vtx Bbc ZVertex 
      @param vtxerr Bbc Error on ZVertex
   */
  virtual void set_TimeVertex(const float t0, const float t0err, const float vtx, const float vtxerr)
  { 
    set_TimeZero( t0, t0err );
    set_Vertex( vtx, vtxerr );
  }

  /** set T0 for Bbc
      @param t0 Bbc T0
      @param t0err Bbc T0 error
   */
  virtual void set_TimeZero(const float t0, const float t0err = 0);

  //! set vertex
  virtual void set_Vertex( const float vtx, const float vtxerr);
  
  /** set Vtx Error for Bbc
      @param vtxerr Bbc Vtx Error
   */
  virtual void set_dZVertex(const float vtxerr);
  
  /** Add Bbc North/South object containing Number of pmt's, Energy and Timing
      @param npmt Number of Pmt's fired
      @param energy Energy in North/South
      @param timing Timing of North/South
      @param nBbc  Arm, use Bbc::North and Bbc::South
   */
  virtual void AddBbcNS(const short npmt, const float energy, 
                        const float timing, const short nBbc);

  /** Add Bbc North/South object containing Number of pmt's and Energy
      @param npmt Number of Pmt's fired
      @param energy Energy in North/South
      @param nBbc  Arm, use Bbc::North and Bbc::South
   */
  virtual void AddBbcNS(const short npmt, const float energy, const short nBbc);

  /** get Number of Pmt's fired in North/South Bbc
      @param nBbc  Arm, use Bbc::North and Bbc::South
   */
  virtual short get_nPmt(const short nBbc) const;

  /** get Charge Sum of North/South Bbc
      @param nBbc  Arm, use Bbc::North and Bbc::South
   */
  virtual float get_ChargeSum(const short nBbc) const;

  /** get Timing of North/South Bbc
      @param nBbc  Arm, use Bbc::North and Bbc::South
   */
  virtual float get_Timing(const short nBbc) const;

  virtual void FillFromClass(const BbcOut& old);
    
 private:
  void virtual_warning(const char *funcname) const;

  /// Root Internal Version
  ClassDef(BbcOut,1)


};

#endif

