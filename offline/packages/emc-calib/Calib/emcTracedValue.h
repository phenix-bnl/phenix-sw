#ifndef  __EMCTRACEDVALUE_H__
#define  __EMCTRACEDVALUE_H__

#include <Rtypes.h>
#include <iosfwd>
#include <cstdio>

/** Stores the data on a single tracing line. 
    
    The decision is made to store lines without upper bounds for validity 
    periods (valid to infinity unless further lines exist). 

@ingroup calibration
*/

class emcTracedValue
{
 public:

  /// Ctor 
  emcTracedValue(Float_t* bp=0, bool isconstant=false);

  /// ctor
  emcTracedValue(int thetime, float constant, float slope, bool isconstant=false); 

  /// Copy ctor
  emcTracedValue(const emcTracedValue&);

  /// assignment operator
  emcTracedValue& operator=(const emcTracedValue&);

  /// Dtor
  ~emcTracedValue() {};

  /// This object does not use it slope really.
  bool isConstant(void) const { return fIsConstant; }

  /// Get starting X for this line.
  int GetX(void) const { return (int)GLine[0]; }

	/// Get the value of the constant of the line (intercept).
  float GetConstant(void) const { return GLine[1]; }

	/// Get the value of the slope of the line.
  float GetSlope(void) const { return GLine[2]; }

  /** Applicable if stored value was computed using EmcTraceAverage 
			(constant within the period of validity). */
  inline Float_t getValue(void) { return GLine[1]; }

  /** Applicable if stored value was computed using EmcTraceLine 
			(linear function with validity period). */
  Float_t getValue(Float_t _x);
	

  /// Same as above but for integer x (which is the real type of this x).
  inline Float_t getValue(Int_t _x) { 
    return getValue(static_cast<Float_t>(_x)); }

  /// Decide whether this object really uses its slope or not.
  void MakeConstant(bool isconstant=true) { fIsConstant = isconstant; }

  /// Read single line from input stream
  void readData(FILE *);

  ///
  void Set(int x, float constant, float slope, bool isconstant);

  ///
  void Set(int x, float constant, float slope);

  /// Write single line into output stream
  void writeData(FILE *);
  
  ///
  Bool_t operator== (const Int_t & time)
    {return ( GLine[0] == float(time)) ? kTRUE : kFALSE;} 
  ///
  Bool_t operator>  (const Int_t & time)
    {return ( GLine[0] >  float(time)) ? kTRUE : kFALSE;} 
  ///
  Bool_t operator<  (const Int_t & time)
    {return ( GLine[0] <  float(time)) ? kTRUE : kFALSE;} 
  ///
  Bool_t operator== (const emcTracedValue & GL)
    {return ( GLine[0] == GL.GLine[0]) ? kTRUE : kFALSE;} 
  ///
  Bool_t operator>  (const emcTracedValue & GL)
    {return ( GLine[0] >  GL.GLine[0]) ? kTRUE : kFALSE;} 
  ///
  Bool_t operator<  (const emcTracedValue & GL)
    {return ( GLine[0] <  GL.GLine[0]) ? kTRUE : kFALSE;} 

 protected:
  ///  {StartTime(integer in fact),constant,slope}.
  Float_t  GLine[3]; //!
  bool fIsConstant;

	///
  friend std::ostream& operator << (std::ostream&, const emcTracedValue&);
};

#endif
