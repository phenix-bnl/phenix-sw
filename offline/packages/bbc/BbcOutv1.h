#ifndef __BBCOUTV1_H
#define __BBCOUTV1_H

#include "BbcOut.h"
class TClonesArray;

///
class BbcOutv1: public BbcOut
{
 public:

  ///
  BbcOutv1();
  ///
  virtual ~BbcOutv1();

  /// Clear Event from memory
  virtual void Reset();

  /** identify Function from PHObject
      @param os Output Stream 
   */
  virtual void identify(std::ostream& os = std::cout) const;

  /// isValid returns non zero if object contains vailid data
  virtual int isValid() const;

  /// get ZVertex determined by Bbc
  virtual float get_VertexPoint()  const {return Bbc_ZVertex;}

  /// get Error on ZVertex determined by Bbc
  virtual float get_dVertexPoint() const {return Bbc_dZVertex;}

  /// get T0 determined by Bbc
  virtual float get_TimeZero() const {return Bbc_TimeZero;}

  /// get Error on T0 determined by Bbc
  virtual float get_dTimeZero() const {return Bbc_dTimeZero;}

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
  virtual void AddBbcNS(const short npmt, const float chargesum, const float timing, const short nBbc);

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

  protected:

  virtual void Clear(Option_t *option = "");
  
  virtual void Init();
  
  TClonesArray *GetBbcNS() const 
  {return BbcNS;}

  private:
  
  float Bbc_ZVertex;
  float Bbc_dZVertex;
  float Bbc_TimeZero;
  float Bbc_dTimeZero;
  TClonesArray *BbcNS;

 private: // so the ClassDef does not show up with doc++
  ClassDef(BbcOutv1,1)
};

#endif
