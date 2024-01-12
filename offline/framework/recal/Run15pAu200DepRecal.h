#ifndef __RUN15PAU200DEPRECAL_H__
#define __RUN15PAU200DEPRECAL_H__

#include "Recalibrator.h"
#include <string>

class PHCompositeNode;
class PHCentralTrack;
class PHGlobal;
class TH1F;
class TH2F;

class Run15pAu200DepRecal : public Recalibrator
{
public:
    Run15pAu200DepRecal(const std::string &name = "Run15pAu200DepRecal");
    virtual ~Run15pAu200DepRecal() {}  

    static const int NMOM_BINS = 23;
    static const int NSECT = 8;

    //Recalibration parameters for functional forms
    float mean_params_pos[NSECT][5];
    float mean_params_neg[NSECT][5];
    float sigma_params_pos[NSECT][4];
    float sigma_params_neg[NSECT][4];
    
    float mom_bin_lo[NMOM_BINS];
    float mom_bin_hi[NMOM_BINS];
   
    int process_event(PHCompositeNode *topNode);
    int InitRun(PHCompositeNode *topNode);
    int Init(PHCompositeNode *topNode);
    int isValidRun(const int runno) const;
    void help();
    void initialize_parameters();
    void initializeBins();

protected:

    int runNumber;
    PHCentralTrack *d_cnt;
    PHGlobal *d_global;
    int Calibrate_Run15pAu200GeV();
    float calculate_dep(int sector, float alpha, float mom, float eop);
    float sigmaFunc(float x, int sect, float alpha);
    float meanFunc(float x, int sect, float alpha);
    int getBin(float val);
};

#endif /* __RUN14AUAU200ELECTRONEMCMATCHING_H__ */
