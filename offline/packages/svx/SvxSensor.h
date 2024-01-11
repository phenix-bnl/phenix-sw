// =================
// FILE: SvxSensor.h
// =================
#ifndef __SVXSENSOR_HH_
#define __SVXSENSOR_HH_

#include <iostream>
#include <PHObject.h>
#include <phool.h>
#include <SvxHit.h>

class SvxGhitList;
class SvxRawhitList;
class SvxRawhit;
class SvxGhitRawhitList;
class SvxClusterList;
class SvxRawhitClusterList;
class SvxGhitClusterList;
class svxAddress;

/**
 * @brief  The base (partially implemented) class for a (stri)pixel sensor.
 *
 * Created  by V. L. Rykov: 15-Feb-2004
 * Modified by V. L. Rykov: 05-Apr-2004
 *      Comments to svxSection: : 0 - Barrel, 1 - North; 2 - South
 * Modified by V. L. Rykov: 22-Apr-2004
 *      Local<->Global transformation methods moved here.
 * Modified by V. L. Rykov: 15-May-2004
 *      Adjusted to using sorted hit containers.
 * Modified by T. Hachiya : 11-Dec-2011
 *      Add Offset adding to translation vector.
 */
class SvxSensor : public PHObject
{

 public:
  // Constructor(s) & destructor
  // """""""""""""""""""""""""""
  SvxSensor(const int sc=-1,const int lr=-1,const int ld=-1,const int sn=-1);
  virtual ~SvxSensor() { /*cout << "SvxSensor object destroyed" << endl;*/ }

  // Standard functions of all inheritors of PHObject classes...
  //""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  virtual void Reset   ()                                ;
  virtual int isValid  ()                           const;
  virtual void identify(std::ostream &os=std::cout) const;

  // Check hit's sensor ID
  // """""""""""""""""""""
  bool checkID(const SvxHit* hit) const;

  // Handling parameters
  // """""""""""""""""""
  // Set sensor ID
  // -------------
  void set_svxSection (int val) { svxSection = (short) val ;}
  void set_layer      (int val) { layer      = (short) val ;}
  void set_ladder     (int val) { ladder     = (short) val ;}
  void set_sensor     (int val) { sensor     = (short) val ;}

  // Set rotation matrix
  // -------------------
  void set_rotMatrix   (const int i, const int j, const float val) 
    { rotMatrix[i][j] = val; }

  void set_rotMatrix(const float val[][3])
    {
      for (int i=0; i<3; i++) {
	for (int j=0; j<3; j++) {
	  rotMatrix[i][j] = val[i][j];
	}
      }
    }

  // Set translation vector
  // ----------------------
  void set_transVector (const int i, const float val) 
    { transVector[i] = val; }

  void set_transVector(const float val[])
    {
      for (int i=0; i<3; i++) {
	transVector[i] = val[i];
      }
    }

  // set the origin offset of the coordinate system
  // 
  void set_originOffset (const int i, const float val) 
    { m_originOffset[i] = val; }
  void set_originOffset(const float val[])
    {
      for (int i=0; i<3; i++) {
	m_originOffset[i] = val[i];
      }
    }

  /**
   * @brief  Put charge asymmetry paramter.
   * Valid only for stripixel.
   *
   * Set the parameter about the asymmetric charge sharing between x & u strips.
   * @param[in] val  the Gaussian width of charge asymmetry (charge_x - charge_u)/(charge_x + charge_u).
   * @date  2008/05/16 M.Togawa
   */
  virtual void set_sAQ( float val ){
    sAQ = val;
  }
  /**
   * @brief  Set the parameter about noises in stripixel readout.
   * Valid only for stripixel.
   * @param[in] val  the Gaussian width of noise in ADC unit.
   */
  virtual void set_sNOISE(float val) { PHOOL_VIRTUAL_WARN("set_sNOISE()"); }
  /**
   * @brief  Set the parameter about (emulated) zero suppression.
   * Valid only for stripixel.
   * @param[in] val  noises with ADC < this value are not stored (zero suppressed) in SvxRawhitList
   */
  virtual void set_adcthre_zs(int val){ PHOOL_VIRTUAL_WARN("set_adcthre_zs()"); }
  /**
   * @brief  Set the parameter about clustering.
   * Valid only for stripixel.
   * @param[in] val  rawhits with ADC >= this value are used in clustering.
   */
  virtual void set_adcthre_rawhit    (int val) { PHOOL_VIRTUAL_WARN("set_adcthre_rawhit()"); }
  /**
   * @brief  Set the parameter about clustering.
   * Valid only for stripixel.
   * @param[in] val  rawhit groups (x & u separately) with summed ADC >= this value are used in clustering.
   */
  virtual void set_adcthre_rawhit_sum(int val) { PHOOL_VIRTUAL_WARN("set_adcthre_rawhit_sum()"); }

  // Global to local transformations and back
  // """"""""""""""""""""""""""""""""""""""""
  void   vector_global2local( const double vg[],    double vl[]    ) const;
  void position_global2local( const double pg[],    double pl[]    ) const;
  void   matrix_global2local( const double mg[][3], double ml[][3] ) const;

  void   vector_local2global( const double vl[],    double vg[]    ) const;
  void position_local2global( const double pl[],    double pg[]    ) const;
  void   matrix_local2global( const double ml[][3], double mg[][3] ) const;

  // Get sensor ID
  // -------------
  int get_svxSection () const { return (int) svxSection ;}
  int get_layer      () const { return (int) layer      ;}
  int get_ladder     () const { return (int) ladder     ;}
  int get_sensor     () const { return (int) sensor     ;}

  // Get rotation matrix and translation vector
  // ------------------------------------------
  float get_rotMatrix   (const int i, const int j) const
    { return rotMatrix[i][j]; }
  float get_transVector              (const int i) const
    { return transVector[i]; }

  float get_originOffset             (const int i) const
    { return m_originOffset[i]; }

  float get_correctedTransVector     (const int i) const
    { return transVector[i] + m_originOffset[i]; }

  // Printing sensor parameters
  // """"""""""""""""""""""""""
  void    printID          () const;   // print sensor ID in the SVX
  void    printPositioning () const;   // print sensor positioning
  virtual void printPar    () const;   // print parameters (ID & positioning)

  // Virtual function to be implemented by daughters
  // """""""""""""""""""""""""""""""""""""""""""""""
  virtual int   get_sensorType                              () const
    { PHOOL_VIRTUAL_WARN("get_sensorType()"); return -9999 ;}
  virtual int   get_nSection                                () const
    { PHOOL_VIRTUAL_WARN("get_nSection()"  ); return -9999 ;}
  virtual int   get_nReadout                                () const
    { PHOOL_VIRTUAL_WARN("get_nReadout()"  ); return -9999 ;}
  virtual int   get_secTYPE   (const int sc=0, const int rd=0) const
    { PHOOL_VIRTUAL_WARN("get_secTYPE()"   ); return -9999 ;}
  virtual double get_secXpos   (const int sc=0, const int rd=0) const
    { PHOOL_VIRTUAL_WARN("get_secXpos()"   ); return -9999.;}
  virtual double get_secZpos   (const int sc=0, const int rd=0) const
    { PHOOL_VIRTUAL_WARN("get_secZpos()"   ); return -9999.;}
  virtual bool  get_xcntRvrs  (const int sc=0, const int rd=0) const
    { PHOOL_VIRTUAL_WARN("get_xcntRvrs()"  ); return false ;}
  virtual bool  get_zcntRvrs  (const int sc=0, const int rd=0) const
    { PHOOL_VIRTUAL_WARN("get_zcntRvrs()"  ); return false ;}
  virtual int   get_chanOffs  (const int sc=0, const int rd=0) const
    { PHOOL_VIRTUAL_WARN("get_chanOffs()"  ); return -9999 ;}
  virtual int   get_chanUplim (const int sc=0, const int rd=0) const
    { PHOOL_VIRTUAL_WARN("get_chanUplim()" ); return -9999 ;}
  virtual int   get_xSlope    (const int sc=0, const int rd=0) const
    { PHOOL_VIRTUAL_WARN("get_xSlope()"    ); return -9999 ;}
  virtual int   get_zSlope    (const int sc=0, const int rd=0) const
    { PHOOL_VIRTUAL_WARN("get_zSlope()"    ); return -9999 ;}
  virtual float get_adcthresh (const int sc=0, const int rd=0) const
    { PHOOL_VIRTUAL_WARN("get_adcthresh()" ); return -9999.;}
  virtual int   get_nXpitch   (const int sc=0, const int rd=0) const
    { PHOOL_VIRTUAL_WARN("get_nXpitch()"   ); return -9999 ;}
  virtual double get_xPitch    (const int sc=0, const int rd=0) const
    { PHOOL_VIRTUAL_WARN("get_xPitch()"    ); return -9999.;}
  virtual double get_xhalfWidth(const int sc=0, const int rd=0) const
    { PHOOL_VIRTUAL_WARN("get_xhalfWidth()"); return -9999.;}
  virtual int   get_nZpitch   (const int sc=0, const int rd=0) const
    { PHOOL_VIRTUAL_WARN("get_nZpitch()"   ); return -9999 ;}
  virtual double get_zPitch    (const int sc=0, const int rd=0) const
    { PHOOL_VIRTUAL_WARN("get_zPitch()"    ); return -9999.;}
  virtual double get_zhalfWidth(const int sc=0, const int rd=0) const
    { PHOOL_VIRTUAL_WARN("get_zhalfWidth()"); return -9999.;}


  /**
   * @brief  Make raw hits from GEANT hits
   * 
   * @param[in]  glist    a Ghit list from which Ghits are read.
   * @param[out] rawlist  a rawhit list to which new rawhits are added.
   * @param[out] g2raw    a g2raw list to which new g2r relators are added.
   * @return  the number of rawhits added to the list.
   */
  virtual int makeRawhits(SvxGhitList*       glist,
			  SvxRawhitList*   rawlist,
			  SvxGhitRawhitList* g2raw,
                          svxAddress* SvxAddressObject) const {
    PHOOL_VIRTUAL_WARN(
     "int makeRawhits(SvxGhitList*,SvxRawhitList*,SvxGhitRawhitList*)");
    return 0;
  }

  /**
   * @brief  Add noises in SvxRawhitList (only for stripixel type 11 so far).
   *
   * This makes a noise in each channel.
   * If a channel has a real signal (the SvxRawhitList indecies from "rhit_first"
   * to "rhit_last" are searched for real signals), noise is added to the signal.
   * Otherwise, a new SvxRawhit entry for noise is added.
   * @param[in]  rhit_first  an index where real GEANT hits start.
   * @param[in]  rhit_last   an index where real GEANT hits end.
   * @param[in,out]  rawlist  a rawhit list to which noise hits are added.
   * @return  the number of noise hits added to the list.
   */
  virtual int AddNoise(const int rhit_first, const int rhit_last, SvxRawhitList* rawlist, 
                       SvxGhitRawhitList* g2raw, int g2rFirst) const {
    PHOOL_VIRTUAL_WARN("int AddNoise(...)");
    return 0;
  }

  /**
   * @brief  Clusters finders.
   *
   * @param[in]  rawlist  a SvxRawhitList.
   * @param[out] clslist  a SvxClusterList to which new clusters are added.
   * @param[out] raw2cls  a SvxRawhitClusterList to be filled.
   * @param[in]  ght2raw  a SvxGhitRawhitList used to make gth2cls.
   * @param[out] ght2cls  a SvxGhitClusterList to be filled.
   */
  virtual int findClusters(SvxRawhitList*        rawlist,
			   SvxClusterList*       clslist,
			   SvxRawhitClusterList* raw2cls = NULL,
			   SvxGhitRawhitList*    ght2raw = NULL,
			   SvxGhitClusterList*   ght2cls = NULL,
                           svxAddress* address = NULL) {
    PHOOL_VIRTUAL_WARN("int findClusters(...)");
    return 0;
  }

  /// this is for pixel
  virtual void set_hitmap_hit (int x, int z)
  { PHOOL_VIRTUAL_WARN("void set_hitmap_hit(...)"); }
  virtual void set_hitmap_rawhitID (int x, int z, int val)
  { PHOOL_VIRTUAL_WARN("void set_hitmap_rawhitID(...)"); }

  /// this is for stripixel
  virtual void add_rawhit (SvxRawhit *rawhit)
  { PHOOL_VIRTUAL_WARN("void add_rawhit(...)"); }
  /*
  virtual void set_hitmap_hit (int rout, int sec, int ch)
  { PHOOL_VIRTUAL_WARN("void set_hitmap_hit(...)"); }
  virtual void set_hitmap_rawhitID (int rout, int sec, int ch, int val)
  { PHOOL_VIRTUAL_WARN("void set_hitmap_rawhitID(...)"); }
  */
 protected:

  // Sensor ID in the SVX
  short svxSection      ;   ///< SVX section: 0 - Barrel; 1 - North; 2- South
  short layer           ;   ///< layer  # in the svxSection
  short ladder          ;   ///< ladder # in the layer
  short sensor          ;   ///< sensor # in the ladder

  // Sensor positioning in the SVX
  float rotMatrix[3][3] ;   ///< Rotation matrix
  float transVector[3]  ;   ///< Translation vector

  float m_originOffset[3]; // offset of the origin of coordinate system which are adding to transVector[3]

  float sAQ;  ///< Charge sharing parameter, i.e. the Gaussian width of charge asymmetry between x & u (=Q_x - Q_u)/(Q_x + Q_u)

  ClassDef(SvxSensor, 1);
};
#endif
