#ifndef __EMC_GEACLUSTER_CONTENTT_H__
#define __EMC_GEACLUSTER_CONTENTT_H__





#include <vector>

#include <emctypes.h>
#include <emcGeaClusterContent.h>
#include <emcGeaDeposit.h>



template<class T>
class emcGeaClusterContentT: public emcGeaClusterContent {
public:
  T content;


public:
  /// default constructor
  emcGeaClusterContentT(emc_clusterid_t clusterid = EMC_INVALID_CLUSTERID){ set_id(clusterid); };

  //  /// destructor
  //  virtual ~emcGeaClusterContent(){}

  virtual emcClusterContent * create(void) const { return new emcGeaClusterContentT< T >(); };

  virtual void copy(emcGeaClusterContent const * from);



public: /* functions implementing emcClusterContent by fwding call to content */

  virtual void Copy(const emcClusterContent & from);

  virtual int arm() const { return content.arm(); }
  virtual bool canBeMerged() const { return content.canBeMerged(); }
  virtual float chi2() const { return content.chi2(); }
  virtual float corrdispy() const { return content.corrdispy(); }
  virtual float corrdispz() const { return content.corrdispz(); }
  virtual unsigned int cutword() const { return content.cutword(); }
  virtual unsigned int deadmap() const { return content.deadmap(); }
  virtual float dispy() const { return content.dispy(); }
  virtual float dispz() const { return content.dispz(); }
  virtual float dx() const { return content.dx(); }
  virtual float dy() const { return content.dy(); }
  virtual float dz() const { return content.dz(); }
  virtual float e() const { return content.e(); }
  virtual float e9() const { return content.e9(); }
  virtual float ecore() const { return content.ecore(); }
  virtual float ecent() const { return content.ecent(); }
  virtual float etofmin() const { return content.etofmin(); }
  virtual float etofmax() const { return content.etofmax(); }
  virtual bool has_rawtdc() const { return content.has_rawtdc(); }
  virtual bool has_adc() const { return content.has_adc(); }
  virtual bool has_amutac() const { return content.has_amutac(); }
  virtual bool has_yz_cg() const { return content.has_yz_cg(); }
  virtual bool has_id() const { return content.has_id(); }
  virtual bool has_pid() const { return content.has_pid(); }
  virtual bool has_type() const { return content.has_type(); }
  virtual bool has_Dxyz() const { return content.has_Dxyz(); }
  virtual bool has_E9() const { return true; }
  virtual bool has_Etofmin() const { return content.has_Etofmin(); }
  virtual bool has_Etofmax() const { return content.has_Etofmax(); }
  virtual bool has_Quality() const { return content.has_Quality(); }
  virtual bool has_Phi() const { return content.has_Phi(); }
  virtual bool has_Theta() const { return content.has_Theta(); }
  virtual bool has_Tofdisp() const { return content.has_Tofdisp(); }
  virtual bool has_Tofmin() const { return content.has_Tofmin(); }
  virtual bool has_Tofmax() const { return content.has_Tofmax(); }
  virtual bool has_Tofcorrmin() const { return content.has_Tofcorrmin(); }
  virtual bool has_Tofcorrmax() const { return content.has_Tofcorrmax(); }
  virtual int id() const { return content.id(); }
  virtual int version() const { return content.version(); } 
  virtual bool isMerged() const { return content.isMerged(); }
  virtual bool isSimulated() const { return content.isSimulated(); }
  virtual int iypos() const { return content.iypos(); }
  virtual int izpos() const { return content.izpos(); }
  virtual float quality() const { return content.quality(); }
  virtual int multiplicity() const { return content.multiplicity(); }
  virtual float padispy() const { return content.padispy(); }
  virtual float padispz() const { return content.padispz(); }
  virtual float partesum(int index) const { return content.partesum(index); }
  virtual float prob_photon() const { return content.prob_photon(); }
  virtual float phi() const { return content.phi(); }
  virtual int pid() const { return content.pid(); }
  virtual float rawtdc() const { return content.rawtdc(); }
  virtual float adc() const { return content.adc(); }
  virtual short amutac() const { return content.amutac(); }
  virtual int sector() const { return content.sector(); }
  virtual float simfrac() const { return content.simfrac(); }
  virtual float tof() const { return content.tof(); }
  virtual float tofcorr() const { return content.tofcorr(); }
  virtual float tofhad() const { return content.tofhad(); }
  virtual float tofdisp() const { return content.tofdisp(); }
  virtual float tofmin() const { return content.tofmin(); }
  virtual float tofmax() const { return content.tofmax(); }
  virtual float tofcorrmin() const { return content.tofcorrmin(); }
  virtual float tofcorrmax() const { return content.tofcorrmax(); }
  virtual float theta() const { return content.theta(); }
  virtual int towerid(int index) const { return content.towerid(index); }
  virtual int type() const { return content.type(); }
  virtual unsigned int warnmap() const { return content.warnmap(); }
  virtual float x() const { return content.x(); }
  virtual float y() const { return content.y(); }
  virtual float z() const { return content.z(); }
  virtual float ycg() const { return content.ycg(); }
  virtual float zcg() const { return content.zcg(); }

  virtual short emcpc3       () const { return content.emcpc3(); }
  virtual short emcpc3neartrk() const { return content.emcpc3neartrk(); }
  virtual float emcpc3dz     () const { return content.emcpc3dz(); }
  virtual float emcpc3dphi   () const { return content.emcpc3dphi(); }
  virtual short emctrk       () const { return content.emctrk(); }
  virtual float emctrkdz     () const { return content.emctrkdz(); }
  virtual float emctrkdphi   () const { return content.emctrkdphi(); }
  virtual float pemctrk      () const { return content.pemctrk(); }
  virtual short emctrkquality() const { return content.emctrkquality(); }


  virtual void set_arm(int arm) { content.set_arm(arm); }
  virtual void set_chi2(float chi2) { content.set_chi2(chi2); }
  virtual void set_corrdisp(float corrdispy, float corrdispz) { content.set_corrdisp(corrdispy, corrdispz); }
  virtual void set_cutword(unsigned int w) { content.set_cutword(w); }
  virtual void set_disp(float dispy, float dispz) { content.set_disp(dispy, dispz); }
  virtual void set_dxyz(float dx, float dy, float dz) { content.set_dxyz(dx, dy, dz); }
  virtual void set_e(float e) { content.set_e(e); }
  virtual void set_e9(float e9) { content.set_e9(e9); }
  virtual void set_ecore(float ecore) { content.set_ecore(ecore); }
  virtual void set_ecent(float ecent) { content.set_ecent(ecent); }
  virtual void set_etofmin(float etofmin) { content.set_etofmin(etofmin); }
  virtual void set_etofmax(float etofmax) { content.set_etofmax(etofmax); }
  virtual void set_id(int id) { content.set_id(id); }
  virtual void set_ipos(int iy, int iz)  { content.set_ipos(iy, iz); }
  virtual void set_quality(float q) { content.set_quality(q); }
  virtual void set_maps(unsigned int deadmap, unsigned int warnmap) { content.set_maps(deadmap, warnmap); }
  virtual void set_multiplicity(int m) { content.set_multiplicity(m); }
  virtual void set_padisp(float padispy, float padispz) { content.set_padisp(padispy, padispz); }
  virtual void set_partesum(int index, float value) { content.set_partesum(index, value); }
  virtual void set_prob_photon(float pp) { content.set_prob_photon(pp); }
  virtual void set_phi(float phi) { content.set_phi(phi); }
  virtual void set_pid(int pid) { content.set_pid(pid); }
  virtual void set_rawtdc(float rawtdc) { content.set_rawtdc(rawtdc); }
  virtual void set_adc(float adc) { content.set_adc(adc); }
  virtual void set_amutac(short amutac) { content.set_amutac(amutac); }
  virtual void set_sector(int s) { content.set_sector(s); }
  virtual void set_simfrac(float simfrac) { content.set_simfrac(simfrac); }
  virtual void set_tof(float tof) { content.set_tof(tof); }
  virtual void set_tofhad(float tofhad) { content.set_tofhad(tofhad); }
  virtual void set_tofdisp(float tofdisp) { content.set_tofdisp(tofdisp); }
  virtual void set_tofmin(float tofmin) { content.set_tofmin(tofmin); }
  virtual void set_tofmax(float tofmax) { content.set_tofmax(tofmax); }
  virtual void set_tofcorr(float tofcorr) { content.set_tofcorr(tofcorr); }
  virtual void set_tofcorrmin(float tofcorrmin) { content.set_tofcorrmin(tofcorrmin); }
  virtual void set_tofcorrmax(float tofcorrmax) { content.set_tofcorrmax(tofcorrmax); }
  virtual void set_theta(float theta) { content.set_theta(theta); }
  virtual void set_towerid(int index, int value) { content.set_towerid(index, value); }
  virtual void set_type(int type) { content.set_type(type); }
  virtual void set_xyz(float x, float y, float z) { content.set_xyz(x, y, z); }
  virtual void set_yz_cg(float ycg, float zcg)   { content.set_yz_cg(ycg, zcg); }

  virtual void set_emcpc3       (short x) { content.set_emcpc3(x); }
  virtual void set_emcpc3neartrk(short x) { content.set_emcpc3neartrk(x); }
  virtual void set_emcpc3dz     (float x) { content.set_emcpc3dz(x); }
  virtual void set_emcpc3dphi   (float x) { content.set_emcpc3dphi(x); }
  virtual void set_emctrk       (short x) { content.set_emctrk(x); }
  virtual void set_emctrkdz     (float x) { content.set_emctrkdz(x); }
  virtual void set_emctrkdphi   (float x) { content.set_emctrkdphi(x); }
  virtual void set_pemctrk      (float x) { content.set_pemctrk(x); }
  virtual void set_emctrkquality(short x) { content.set_emctrkquality(x); }







public:

  ClassDef(emcGeaClusterContentT, 1)
};


templateClassImp(emcGeaClusterContentT);






template<class T>
void emcGeaClusterContentT<T>::Copy(const emcClusterContent & from){
  content.Copy( from );

  //  emcGeaClusterContent const * xfrom = dynamic_cast<emcGeaClusterContent const *>(&from);
  //  if( xfrom ) this->set_towercontainer( xfrom->get_towercontainer() );
}



template<class T>
void emcGeaClusterContentT<T>::copy(emcGeaClusterContent const * from){
  content.Copy( *from );
  //  this->set_towercontainer( from->get_towercontainer() );
}





#endif /* ! __EMC_GEACLUSTER_CONTENTT_H__ */

