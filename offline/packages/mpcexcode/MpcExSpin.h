#ifndef __MPCEXSPIN_H__
#define __MPCEXSPIN_H__

/**
 * @class  MpcExSpin
 * @author Liankun Zou
 * @date   July 2015
 * @brief  read the spin information from the database
 */

class MpcExSpin {
  
 private:

  //! number of bunch crossings
  enum { NCROSS=120 };

 public:

  //! destructor
  virtual ~MpcExSpin();

  //! grab the spin information from the database
  static MpcExSpin* instance();
 
  //!
  void Print() const;

  //!
  void InitRun();

  //!
  int GetRunNumber() const {
    return _runnumber;
  }

  //!
  int GetFillNumber() const {
    return _fillnumber;
  }

  //!
  int GetQALevel() const {
    return _qalevel;
  }

  //!
  int GetCrossingShift() const {
    return _cross_shift;
  }

  //!
  float GetPolBlue(const int bunch) const {
    return GetValue(bunch,_bpol);
  }

  //!
  float GetPolYellow(const int bunch) const {
    return GetValue(bunch,_ypol);
  }

  //!
  float GetPolErrorBlue(const int bunch) const {
    return GetValue(bunch,_bpolerr);
  }

  //!
  float GetPolErrorYellow(const int bunch) const {
    return GetValue(bunch,_ypolerr);
  }

  //!
  int GetSpinPatternBlue(const int bunch) {
    return GetValue(bunch,_bpat);
  }

  //!
  int GetSpinPatternYellow(const int bunch) const {
    return GetValue(bunch,_ypat);
  }

  //!
  int GetSpinPattern(const int bunch) const {
    return GetValue(bunch,_pattern);
  }
  
  //!
  long long GetScalerBbcVertexCut(const int bunch) const {
    return GetValue(bunch,_scaler_bbc_vtxcut);
  }

  //!
  long long GetScalerBbcNoCut(const int bunch) const {
    return GetValue(bunch,_scaler_bbc_nocut);
  }

  //!
  long long GetScalerZdcWide(const int bunch) const {
    return GetValue(bunch,_scaler_zdc_wide);
  }

  //!
  long long GetScalerZdcNarrow(const int bunch) const {
    return GetValue(bunch,_scaler_zdc_narrow);
  }

 private:

  //internal getter for the arrays to check for 
  //out of bounds indices
  template<typename T>
  T GetValue(const int bunch, T* array) const {
    int index = (bunch+_cross_shift)%NCROSS;
    if(index<0 || index>=NCROSS)
      return T (-9999);
    return array[index];
  }

  //!constructor
  MpcExSpin();

  //! the single instance of this class
  static MpcExSpin* _instance;

  //!
  int _runnumber;

  //!
  int _cross_shift;

  //!
  int _fillnumber;

  //!
  int _qalevel;

  //!
  float _bpol[NCROSS];

  //!
  float _bpolerr[NCROSS];

  //!
  float _bpolsys[NCROSS];

  //!
  float _ypol[NCROSS];

  //!
  float _ypolerr[NCROSS];

  //!
  float _ypolsys[NCROSS];

  //!
  int _bpat[NCROSS];

  //!
  int _ypat[NCROSS];

  //!
  int _pattern[NCROSS];

  //!
  long long _scaler_bbc_vtxcut[NCROSS];

  //!
  long long _scaler_bbc_nocut[NCROSS];

  //!
  long long _scaler_zdc_wide[NCROSS];

  //!
  long long _scaler_zdc_narrow[NCROSS];

};

#endif /* __MPCEXSPIN_H__*/

