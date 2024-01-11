#ifndef __EMCMIXEDDATAOBJECT_H__
#define __EMCMIXEDDATAOBJECT_H__

#ifndef __EMCDATAOBJECT_H__
#include "emcDataObject.h"
#endif
#include "TString.h"
#include <iosfwd>

/** (OLD) Intermediate object between RDO and CDO.
 
\deprecated

RDO = raw data object (that is, ADC counts, with pre and post stuff)
 
CDO = calibrated data object (that is, GeV and ns).
 
This MDO is ADC counts, with the pre-post operation done, pedestals are
subtracted and H/L ratio already applied.
 
? FIXME ?
 
The information contained in this object would better fit in a more
FEM-based object, whatever this can be, as there is no point in grouping
the channels in one big blob at this point of the calibration operation.
Only at the last stage (i.e. when you can get GeV and ns, i.e. the CDO
level) does it make sense really.
 
Anyway, for the moment this class \e is used and is a little
bit tricky as it shares some memory space with the RDO (look, that's
really a dirty implementation detail, and should not appear here, but
you need it to use this class properly, which is a very good sign that
this class should disappear, don't you think so?). More tricky, this
sharing is done through the base class emcDataObject (!) I was probably
ill when I accepted to do that...
 
L.A.

To be deprecated as soon as online code does not need it anylonger.

@ingroup oldemccalib
*/

class emcMixedDataObject : public emcDataObject
{

public:

  /// ctor
  emcMixedDataObject();

  /// dtor
  virtual ~emcMixedDataObject();

  /// Get adc,tdc using indexing in the calorimeter ITEMs space
  void Get(Int_t index, Float_t& adc, Float_t& tdc) const;

  /// Get adc,tdc,hg,lg using indexing in the calorimeter ITEMs space
  void Get(Int_t index, Float_t& adc, Float_t& tdc,
	   Float_t& hg, Float_t& lg) const;

  /// Get adc using indexing in the calorimeter ITEMs space
  Float_t GetADC(Int_t index) const;

  /// Get high gain using indexing in the calorimeter ITEMs space
  Float_t GetHG(Int_t index) const;

  /// Get low gain using indexing in the calorimeter ITEMs space
  Float_t GetLG(Int_t index) const;


  /** WARNING - WARNING - WARNING -
      Those methods are to be used in online monitoring processes ONLY.
      This very inelegant method is nevertheless a way of taking care
      of the pre-existing software (which is working).\\      
  */ 
  //@{
  ///
  void GetPointers(float*& ADC, float*& TDC, int*& KEY);

  ///
  void GetPointers(float*& ADC, float*& TDC,
		   float*& HG, float*& LG, int*& KEY);
  //@}

  /// Get time using indexing in the calorimeter ITEMs space
  Float_t GetTDC(Int_t index) const;

  ///
  bool IsPedestalSubtracted(void) const
  {
    return fPedestal;
  }

  /// To know is this object have the raw High and Low Gain values or not
  bool IsUsingHGLG(void) const
  {
    return fUseHGLG;
  }
  ///
  void SetPedestalFlag(bool value = true)
  {
    fPedestal = value;
  }
  ///
  bool IsHLRatioReal(void) const
  {
    return fHLRatio;
  }
  ///
  void Set(int index, float adc, float tdc);
  ///
  void Set(int index, float adc, float tdc, float hg, float lg);
  ///
  void SetHLRatioFlag(bool value = true)
  {
    fHLRatio = value;
  }
  ///
  void SetMaxSize(Int_t thesize);
  ///
  void SetToZero(int index);

  ///
  TString Status(void);

  /// Decide if this object must take care of raw High and Low Gain values
  void UseHGLG(bool use = false);

  /// Tells if channel is zero
  bool IsZero(Int_t index) const;

  ///
  friend std::ostream& operator << (std::ostream&, const emcMixedDataObject&);

private:
  // Copy constructor and assignation (disabled on purpose)
  emcMixedDataObject(const emcMixedDataObject& obj);
  emcMixedDataObject& operator = (const emcMixedDataObject& obj);

protected:

  /// adc value (pre-post)*H/L gain ratio
  Float_t* fADC;
  /// tdc value
  Float_t* fTDC;
  /// High gain value
  Float_t* fHG;
  /// Low gain value
  Float_t* fLG;
  /// tells if pedestals data are subtracted or not
  bool fPedestal;
  /// tells if real H/L ratio data are used or not
  bool fHLRatio;
  /** Indicates if WE are allocating the tower data arrays
      or not (true if reading from Root file, false in any other cases).  
    */
    bool fOwnAllocation;

    /// Switch to store or not high and low gain
    bool fUseHGLG;

  public:

    ClassDef(emcMixedDataObject, 1)
  };

#endif // #ifndef __emcMixedDataObject_h__
