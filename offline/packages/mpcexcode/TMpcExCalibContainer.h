#ifndef __TMPCEXCALIBCONTAINER_H__
#define __TMPCEXCALIBCONTAINER_H__

/**
 * @class  TMpcExCalibContainer
 * @author ngrau@augie.edu
 * @date   July 2015
 * @brief  Container of calibration objects ordered by their keys.
 */

#include "PHObject.h"
#include "TMpcExCalib.h"
#include <set>

class TMpcExCalibContainer : public PHObject {

 public:

  //! Construct an empty container
  TMpcExCalibContainer() : _calibs() {}

  //! Destructor calls clear()
  virtual ~TMpcExCalibContainer() {
    clear();
  }

  /**
   * @brief Reset does nothing
   *
   * I want to control when we reset this container
   * it needs to persist event-by-event rather than
   * be reset event by event.
   */
  void Reset() {}

  //! delete the contents of the container and clear them so that the container is completely empty
  void clear() {
    std::set<TMpcExCalib*>::iterator itr = _calibs.begin();
    std::set<TMpcExCalib*>::iterator last = _calibs.end();
    for(; itr!=last; ++itr)
      delete *itr;
    _calibs.clear();
  }

  //! add a calibration to the container, the container owns the pointer
  void addCalib(TMpcExCalib *calib){
    _calibs.insert(calib);
  }

  //! grab the calibration object for the given key, if none exists NULL is returned.
  TMpcExCalib* get(unsigned int key){
    TMpcExCalib *guess = new TMpcExCalib(key);
    std::set<TMpcExCalib*>::iterator itr = _calibs.find(guess);
    delete guess;
    if(itr==_calibs.end()){
      return NULL;
    }
    return *itr;
  }

 private:

  //! the internal set of calibration objects ordered by the key
  std::set<TMpcExCalib*,CompareCalibsByKey> _calibs;

};

#endif /* __TMPCEXCALIBCONTAINER_H__ */
