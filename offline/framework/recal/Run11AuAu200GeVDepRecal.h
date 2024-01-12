#ifndef __RUN11AUAU200GEVDEPRECAL_H__
#define __RUN11AUAU200GEVDEPRECAL_H__

#include "Recalibrator.h"
#include <string>

class PHCompositeNode;
class PHCentralTrack;
class PHGlobal;
class TH2F;

class Run11AuAu200GeVDepRecal : public Recalibrator
{
public:
    Run11AuAu200GeVDepRecal(const std::string &name = "Run11AuAu200GeVDepRecal");
    virtual ~Run11AuAu200GeVDepRecal() {}

    int process_event(PHCompositeNode *topNode);
    int InitRun(PHCompositeNode *topNode);
    int Init(PHCompositeNode *topNode);
    int isValidRun(const int runno) const;
    void help();


protected:

    int runNumber;

    PHCentralTrack *d_cnt;
    PHGlobal *d_global;
    int Calibrate_Run11AuAu200GeV_dep();
    float calculate_dep(const int sector, const int charge, const float pt, const float ep);
    TH2F *hdep_pt_e;
    TH2F *hsdep_pt_e;
    TH2F *hdep_pt_p;
    TH2F *hsdep_pt_p;



};

#endif /* __RUN11AUAU200GEVDEPRECAL_H__ */
