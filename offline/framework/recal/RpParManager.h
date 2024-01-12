// ===============
// FILE: RpParManager.h
// ===============
#ifndef __RPPARMANAGER_H__
#define __RPPARMANAGER_H__

#include "Recalibrator.h"

#include <phool.h>

#include <iostream>

class PHCompositeNode;
class ReactionPlaneCalibv1;

/**
 * @brief  A SubsysReco module to coordinate ReactionPlaneCalibv1
 * @date  Created by Takashi Hachiya in Aug 2011
 *
 */

//
//  Need to add recoConst flag in the macro
//
//  recoConsts *rc = recoConsts::instance();
//  rc->set_IntFlag("RPCALIB_READFROMDB", 0);
//  rc->set_CharFlag("RPCALIB_CALIBFILENAME", "/phenix/subsys/vtx/hachiya/12.06/source/svxvn/wrk/Calib_para_349206-9000.dat");
//
//  rc->set_IntFlag("RPCALIB_VERBOSE", n)
//


class RpParManager : public Recalibrator
{

 public:

  RpParManager(const std::string &name = "RPPARMANAGER");
  virtual ~RpParManager();

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

  int isValidRun(const int runno) const;


  void set_RpCalibFromDB(bool fromDB){m_rpCalibFromDB=fromDB;} 
  void set_RpCalibFileName(const char *filename){ m_rpCalibFileName = filename; }

  ReactionPlaneCalibv1* getRpCalibv1()  const { return m_rpcalibv1; } 

 protected:
  int CreateNodeTree(PHCompositeNode *topNode);
  
 private:
  ReactionPlaneCalibv1 *m_rpcalibv1;

  bool                  m_rpCalibFromDB;
  std::string           m_rpCalibFileName;

 private:
  int                   m_nth;
  static int            m_nmodule;

};
#endif
