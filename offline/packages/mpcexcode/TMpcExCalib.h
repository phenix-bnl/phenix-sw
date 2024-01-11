#ifndef __TMPCEXCALIB_H__
#define __TMPCEXCALIB_H__

/**
 * @class  TMpcExCalib
 * @author ngrau@augie.edu
 * @date   July 2015
 * @brief  Object containing calibration data for each minipad and keyed by the key.
 */

class TMpcExCalib {

 public:

  //! Construct a object with a unique key
  TMpcExCalib(unsigned int key) : _key(key) {
    _low_pedestal = -9999.;
    _low_pedestal_width = -9999.;
    _low_pedestal_chi2 = -9999.;
    _low_dcm_threshold = 0xffff;
    _low_dead_hot_status = 0; //0 means ok, that should be the default
    _high_pedestal = -9999.;
    _high_pedestal_width = -9999.;
    _high_pedestal_chi2 = -9999.;
    _high_dcm_threshold = 0xffff;
    _high_dead_hot_status = 0; //0 means ok, that should be the default
    _high_low_ratio = 0; //0 means bad?
    _high_low_ratio_error = 0;
    _high_low_offset = 0;
    _high_low_offset_error = 0;
    _high_low_sigma = 0;
    _high_low_sigma_error = 0;
    _mip_in_sensor = 0; //0 means bad
    _mip_in_sensor_error = -9999.;
    _minipad_mip_correction = 0;
    _minipad_mip_correction_error = -9999.;
    _mip_layer_mpv = -9999.;
    _mip_layer_sigma = -9999.;
    _mip_correction = 1.;
    _mip_corr_mpv = 0.;
    _mip_corr_sigma = 0.;
    _mip_corr_cutoff_position = 0.;
    _mip_corr_cutoff_efficiency = 0.;
    _neighbor_key = 50000;
    _smear_scale = 1.;
    _smear = 0.;
  }

  //! set the low gain pedestal value for this minipad
  void set_low_pedestal(float val) { 
    _low_pedestal = val; 
  }

  //! set the low gain pedestal width value for this minipad
  void set_low_pedestal_width(float val) { 
    _low_pedestal_width = val; 
  }

  //! set the low gain pedestal fit chi2 for this minipad
  void set_low_pedestal_chi2(float val) { 
    _low_pedestal_chi2 = val; 
  }

  //! set the low gain dcm threshold for this minipad
  void set_low_dcm_threshold(unsigned short val) { 
    _low_dcm_threshold = val; 
  }

  //! set the high gain pedestal value for this minipad
  void set_high_pedestal(float val) { 
    _high_pedestal = val; 
  }

  //! set the high gain pedestal width value for this minipad
  void set_high_pedestal_width(float val) { 
    _high_pedestal_width = val; 
  }

  //! set the high gain pedestal fit chi2 for this minipad
  void set_high_pedestal_chi2(float val) { 
    _high_pedestal_chi2 = val; 
  }

  //! set the high gain dcm threshold for this minipad
  void set_high_dcm_threshold(unsigned short val) { 
    _high_dcm_threshold = val; 
  }

  //! set the dead-hot status for this minipad
  void set_high_dead_hot_status(int val) { 
    _high_dead_hot_status = val; 
  }

  //! set the dead-hot status for this minipad
  void set_low_dead_hot_status(int val) { 
    _low_dead_hot_status = val; 
  }

  //! set the high vs. low slope for this minipad
  void set_high_low_ratio(float val) {
    _high_low_ratio = val;
  }

  //! set the high vs. low slope error for this minipad
  void set_high_low_ratio_error(float val) {
    _high_low_ratio_error = val;
  }

  //! set the high vs. low y-intercept for this minipad
  void set_high_low_offset(float val) {
    _high_low_offset = val;
  }

  //! set the high vs. low y-intercept error for this minipad
  void set_high_low_offset_error(float val) {
    _high_low_offset_error = val;
  }

  //! set the high/low sigma for this minipad
  void set_high_low_sigma(float val) {
    _high_low_sigma = val;
  }

  //! set the high/low sigma error for this minipad
  void set_high_low_sigma_error(float val) {
    _high_low_sigma_error = val;
  }

  //! set the mip position for all minipads in a sensor
  void set_mip_in_sensor(float val) {
    _mip_in_sensor = val;
  }

  //! set the error in the mip position for all minipads in a sensor
  void set_mip_in_sensor_error(float val) {
    _mip_in_sensor_error = val;
  }

  //! set the mip correction for the particular minipad
  void set_minipad_mip_correction(float val) {
    _minipad_mip_correction = val;
  }

  //! set the error on the mip correction for the particular minipad
  void set_minipad_mip_correction_error(float val) {
    _minipad_mip_correction = val;
  }

  //! set the minipad's layer mpv
  void set_mip_layer_mpv(float val) {
    _mip_layer_mpv = val;
  }

  //! set the minipad's layer sigma
  void set_mip_layer_sigma(float val) {
    _mip_layer_sigma = val;
  }

  //! set the minipad's mip correction ratio
  void set_mip_correction(float val) {
    _mip_correction = val;
  }

  //! set the minipad's mip corrected Landau most probable value
  void set_mip_corr_mpv(float val) {
    _mip_corr_mpv = val;
  }

  //! set the minipad's mip corrected Landau sigma
  void set_mip_corr_sigma(float val) {
    _mip_corr_sigma = val;
  }

  //! set the minipad's mip corrected energy cutoff
  void set_mip_corr_cutoff_position(float val) {
    _mip_corr_cutoff_position = val;
  }

  //! set the minipad's mip corrected cutoff efficiency
  void set_mip_corr_cutoff_efficiency(float val) {
    _mip_corr_cutoff_efficiency = val;
  }

  //! set the minipad's neighbor key with good calibrations
  void set_minipad_neighbor(unsigned int key) {
    _neighbor_key = key;
  }

  //! set the minipad's MC smearing value
  void set_smear_scale(float scale) {
    _smear_scale = scale;
  }

  //! set the minipad's MC smearing value
  void set_smear(float smear) {
    _smear = smear;
  }

  //! get the key
  unsigned int key() const {
    return _key;
  }

  //! get the low gain pedestal value for this minipad
  float low_pedestal() const {
    return _low_pedestal;
  }

  //! get the low gain pedestal width value for this minipad
  float low_pedestal_width() const {
    return _low_pedestal_width;
  }

  //! get the low gain pedestal fit chi2 for this minipad
  float low_pedestal_chi2() const {
    return _low_pedestal_chi2;
  }

  //! get the low gain dcm threshold for this minipad
  unsigned short low_dcm_threshold() const {
    return _low_dcm_threshold;
  }

  //! get the high gain pedestal value for this minipad
  float high_pedestal() const {
    return _high_pedestal;
  }

  //! get the high gain pedestal width value for this minipad
  float high_pedestal_width() const {
    return _high_pedestal_width;
  }

  //! get the high gain pedestal fit chi2 for this minipad
  float high_pedestal_chi2() const {
    return _high_pedestal_chi2;
  }

  //! get the low gain dcm threshold for this minipad
  unsigned short high_dcm_threshold() const {
    return _high_dcm_threshold;
  }

  //! get the dead-hot status work for this minipad
  int high_dead_hot_status() const {
    return _high_dead_hot_status;
  }

  //! get the dead-hot status work for this minipad
  int low_dead_hot_status() const {
    return _low_dead_hot_status;
  }

  //! get the high vs. low slope for this minipad
  float get_high_low_ratio() const {
    return _high_low_ratio;
  }

  //! get the high vs. low slope error for this minipad
  float get_high_low_ratio_error() const {
    return _high_low_ratio_error;
  }

  //! get the high vs. low y-intercept for this minipad
  float get_high_low_offset() const {
    return _high_low_offset;
  }

  //! get the high vs. low y-intercept error for this minipad
  float get_high_low_offset_error() const {
    return _high_low_offset_error;
  }

  //! get the high/low sigma for this minipad
  float get_high_low_sigma() const {
    return _high_low_sigma;
  }

  //! get the high/low sigma error for this minipad
  float get_high_low_sigma_error() const {
    return _high_low_sigma_error;
  }

  //! get the mip position for all minipads in a sensor
  float get_mip_in_sensor() const {
    return _mip_in_sensor;
  }

  //! get the error in the mip position for all minipads in a sensor
  float get_mip_in_sensor_error() {
    return _mip_in_sensor_error;
  }

  //! get the mip correction for the particular minipad
  float get_minipad_mip_correction() const {
    return _minipad_mip_correction;
  }

  //! get the error on the mip correction for the particular minipad
  float get_minipad_mip_correction_error() const {
    return _minipad_mip_correction_error;
  }

  //! get the minipad's layer mpv
  float get_mip_layer_mpv() const {
    return _mip_layer_mpv;
  }

  //! get the minipad's layer sigma
  float get_mip_layer_sigma() const {
    return _mip_layer_sigma;
  }

  //! get the minipad's mip correction ratio
  float get_mip_correction() const {
    return _mip_correction;
  }

  //! get the minipad's mip corrected Landau most probable value
  float get_mip_corr_mpv() const {
    return _mip_corr_mpv;
  }

  //! get the minipad's mip corrected Landau sigma
  float get_mip_corr_sigma() const {
    return _mip_corr_sigma;
  }

  //! get the minipad's mip corrected energy cutoff
  float get_mip_corr_cutoff_position() const {
    return _mip_corr_cutoff_position;
  }

  //! get the minipad's mip corrected cutoff efficiency
  float get_mip_corr_cutoff_efficiency() const {
    return _mip_corr_cutoff_efficiency;
  }

  //! get the minipad's neighbor key with good calibrations
  unsigned int get_minipad_neighbor() const {
    return _neighbor_key;
  }

  //! get the minipad's MC smearing value
  float get_smear_scale() const {
    return _smear_scale;
  }

  //! get the minipad's MC smearing value
  float get_smear() const {
    return _smear;
  }

 private:

  //! the key
  unsigned int _key;

  //! the low gain pedestal
  float _low_pedestal;

  //! the low gain pedestal width
  float _low_pedestal_width;

  //! the low gain pedestal fit chi2
  float _low_pedestal_chi2;

  //! the applied dcm threshold for the low gain
  unsigned short _low_dcm_threshold;

  //! the high gain pedestal
  float _high_pedestal;

  //! the high gain pedestal width
  float _high_pedestal_width;

  //! the high gain pedestal fit chi2
  float _high_pedestal_chi2;

  //! the applied dcm threshold for the high gain
  unsigned short _high_dcm_threshold;

  //! the high gain dead/hot status word
  int _high_dead_hot_status; 

  //! the low gain dead/hot status word
  int _low_dead_hot_status; 

  //! the high vs. low slope for the minipad
  float _high_low_ratio;

  //! the high vs. low slope error for the minipad
  float _high_low_ratio_error;

  //! the high vs. low y-intercept for the minipad
  float _high_low_offset;

  //! the high vs. low y-intercept error for the minipad
  float _high_low_offset_error;

  //! the high/low sigma for the minipad
  float _high_low_sigma;

  //! the high/low sigma error for the minipad
  float _high_low_sigma_error;

  //! the mip position of the minipads in a sensor
  float _mip_in_sensor;

  //! the error in the mip position of the minipads in a sensor
  float _mip_in_sensor_error;

  //! the mip correction for the particular minipad
  float _minipad_mip_correction;

  //! the error on the mip correction for the particular minipad
  float _minipad_mip_correction_error;

  //! the minipad's layer mip mpv
  float _mip_layer_mpv;

  //! the minipad's layer mip sigma
  float _mip_layer_sigma;

  //! the minipad's mip correction ratio
  float _mip_correction;

  //! the minipad's mip corrected Landau most probable value
  float _mip_corr_mpv;

  //! the minipad's mip corrected Landau sigma
  float _mip_corr_sigma;

  //! the minipad's mip corrected energy cutoff
  float _mip_corr_cutoff_position;

  //! the minipad's mip corrected cutoff efficiency
  float _mip_corr_cutoff_efficiency;

  //! the minipad's neighbor minipad key with good calibrations
  unsigned int _neighbor_key;

  //! the MC smearing values
  float _smear_scale;

  //! the MC smearing values
  float _smear;
};

/**
 * @class  CompareCalibsByKey
 * @author ngrau@augie.edu
 * @date   July 2015
 * @brief  Class used to order calib objects by their key
 */

class CompareCalibsByKey {
 public:
  bool operator()(TMpcExCalib *calib1, TMpcExCalib *calib2){
    return calib1->key() < calib2->key();
  }
};

#endif /* __TMPCEXCALIB_H__ */
