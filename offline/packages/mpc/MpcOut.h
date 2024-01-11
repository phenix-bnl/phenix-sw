#ifndef __MPCOUT_H__
#define __MPCOUT_H__

#include "PHObject.h"

///
class MpcOut: public PHObject
{
public:
  ///
  MpcOut();
  ///
  virtual ~MpcOut() {}
  
  /** get number of towers above threshold
      @param nMpc  Arm, use Mpc::North and Mpc::South
  */
  virtual Short_t get_ntow(const short nMpc) const;

  /** get Energy Sum of North/South Mpc
      @param nMpc  Arm, use Mpc::North and Mpc::South
   */
  virtual Float_t get_esum(const short nMpc) const;

  /** get Timing of North/South Mpc
      @param nMpc  Arm, use Mpc::North and Mpc::South
   */
  virtual Float_t get_time(const short nMpc) const;

  /// get ZVertex determined by Mpc
  virtual Float_t get_z() const;

  /// get Error on ZVertex determined by Mpc
  virtual Float_t get_dz() const;

  /// get T0 determined by Mpc
  virtual Float_t get_t0() const;

  /// get Error on T0 determined by Mpc
  virtual Float_t get_dt0() const;

  /** Set Number of Hit Towers
     @param s_ntow Hit Towers in South Mpc
     @param n_ntow Hit Towers in North Mpc
  */
  void set_ntow(const Short_t s_ntow, const Short_t n_ntow);

  /** Set Energy Sum
     @param s_esum Energy Sum in South Mpc
     @param n_esum Energy Sum in North Mpc
  */
  void set_esum(const Float_t s_esum, const Float_t n_esum);

  /** Set Time in MPC
     @param s_time Hit Time in South Mpc
     @param n_time Hit Time in North Mpc
  */
  void set_time(const Float_t s_time, const Float_t n_time);

  /** set T0, Error on T0, ZVertex and Error on ZVertex
      @param vtx Mpc ZVertex 
      @param t0 Mpc T0
      @param vtxerr Mpc Error on ZVertex
      @param t0err Mpc Error on T0
   */
  void set_vtxt0(const Float_t vtx, const Float_t t0, const Float_t vtxerr, const Float_t t0err);

  /** identify Function from PHObject
      @param os Output Stream 
   */
  virtual void identify(std::ostream& os = std::cout) const; 

  /// Clear Event
  virtual void Reset();

  /// isValid returns non zero if object contains vailid data
  virtual int isValid() const;

private:
  void virtual_warning(const char *funcname) const;

  /// Root Internal Version
  ClassDef(MpcOut,1)

};

#endif	// __MPCOUT_H__

