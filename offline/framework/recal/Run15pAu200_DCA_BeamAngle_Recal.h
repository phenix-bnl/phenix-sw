#ifndef __RUN15PAU200_BEAMCENTERRECAL_H
#define __RUN15PAU200_BEAMCENTERRECAL_H

#include "Recalibrator.h"
#include <string>

#include <SubsysReco.h>
#include <PHTimeServer.h>
class VtxOut;
class PHGlobal;
class BbcOut;

class PHCentralTrack;
class PHCompositeNode;

class Run15pAu200_DCA_BeamAngle_Recal : public Recalibrator
{
public:

  Run15pAu200_DCA_BeamAngle_Recal(const std::string &name = "RUN15PAU_BEAMCENTERRECAL");
  virtual ~Run15pAu200_DCA_BeamAngle_Recal();

  int InitRun(PHCompositeNode *topNode);
  int isValidRun(const int runno) const;
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

  inline int Reset(PHCompositeNode *topNode)            {return 0;}
  inline int ResetEvent(PHCompositeNode *topNode)       {return 0;}
  inline void Print(const std::string&) const {}

  /**
   * Choose which vertex is used in the SvxCntTrack reconstruction
   *
   * Possible values:
   * - 0: Choose most precise vertex available. (default)
   * - 1: Use "SVX_PRECISE" : The vertex determined by the VTX
   * - 2: Use "SVS" : The seed vertex determined by the VTX
   * - 3: Use "BBC" : The vertex determined by the BBC's
   * - 4: Use "Combined" : The vertex determined by combining the VTX precise and seed
   * - 5: Use "Centrality" : The seed vertex for peripheral(cent>60, bbcq<160) otherwise the precise vertex
   *
   */
  void setVertexFlag(int flag) { m_vtxflag = flag; }

 /**
   * Indicate that reconstruction is to be run in p+p mode
   * Namely, tracks are reconstructed relative to the seed vertex
   * i.e., (bc_x, bc_y, seed_z)
   */
  void setPPFlag(int flag);

protected:
  int CreateNodeTree(PHCompositeNode *topNode);


private:

  void getPrimaryVtx(VtxOut* vtxout, float* vx, float *vy, float *vz);

protected:
  int m_runNumber;
  int m_ppflag;        ///< flag to indicate reconstruction in p+p mode, which determines pattern reco relative to the the seed vertex
  int m_vtxflag;       ///< flag which vertex is used for the association

  int   m_vtx_found; ///< 1: precise/sim found, 2: seed found, 0: BBC vertex
  float m_bbccharge; ///< bbcchargesum, if no bbcout, -9999.0 is filled
  float m_centrality; ///< centrality, if no PHGlobal, -9999.0 is filled

};

#endif
