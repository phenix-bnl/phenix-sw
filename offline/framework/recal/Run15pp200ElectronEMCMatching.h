#ifndef __RUN15PP200ELECTRONEMCMATCHING_H__
#define __RUN15PP200ELECTRONEMCMATCHING_H__

#include "Recalibrator.h"
#include <string>

class PHCompositeNode;
class PHCentralTrack;
class PHGlobal;
class TH1F;
class TH2F;

class Run15pp200ElectronEMCMatching : public Recalibrator
{
public:
    Run15pp200ElectronEMCMatching(const std::string &name = "Run15pp200ElectronEMCMatching");
    virtual ~Run15pp200ElectronEMCMatching() {}  

    static const int N_SECT = 8;
    static const int NCHARGE = 2;
    static const int NMOM_BINS = 13;
    static const int NTHETA_BINS = 14;
    static const int NZED_BINS = 8;

    float mean_emcdphi_pos[N_SECT][NMOM_BINS][NZED_BINS];
    float mean_emcdphi_neg[N_SECT][NMOM_BINS][NZED_BINS];
    
    float sigma_emcdphi_pos[N_SECT][NMOM_BINS][NZED_BINS];
    float sigma_emcdphi_neg[N_SECT][NMOM_BINS][NZED_BINS];

    float mean_emcdz_pos[N_SECT][NMOM_BINS][NTHETA_BINS];
    float mean_emcdz_neg[N_SECT][NMOM_BINS][NTHETA_BINS];

    float sigma_emcdz_pos[N_SECT][NMOM_BINS][NTHETA_BINS];
    float sigma_emcdz_neg[N_SECT][NMOM_BINS][NTHETA_BINS];

    float mom_bin_lo[NMOM_BINS];
    float mom_bin_hi[NMOM_BINS];

    float zed_bin_lo[NZED_BINS];
    float zed_bin_hi[NZED_BINS];

    float theta_bin_lo[NTHETA_BINS];
    float theta_bin_hi[NTHETA_BINS];

    float zed_bin_center[NZED_BINS];
    float theta_bin_center[NTHETA_BINS];

    int process_event(PHCompositeNode *topNode);
    int InitRun(PHCompositeNode *topNode);
    int Init(PHCompositeNode *topNode);
    int isValidRun(const int runno) const;
    void help();

    void initialize_mean_emcdz_pos();
    void initialize_mean_emcdz_neg();
    
    void initialize_sigma_emcdz_pos();
    void initialize_sigma_emcdz_neg();
    
    void initialize_mean_emcdphi_pos();
    void initialize_mean_emcdphi_neg();
    
    void initialize_sigma_emcdphi_pos();
    void initialize_sigma_emcdphi_neg();

    void initializeBins();

protected:

    int runNumber;
    float emcsdphi_e;
    float emcsdz_e;
    int numTracks;

    double mean;
    double sigma;
    double x0; 
    double y0_m; 
    double y0_s; 
    double x1; 
    double y1_m; 
    double y1_s;

    PHCentralTrack *d_cnt;
    PHGlobal *d_global;
    int Calibrate_Run14AuAu200GeV();
    float calculate_emcsdphi_e(int sector, float alpha, float mom, float zed, float emcdphi);
    float calculate_emcsdz_e(int sector, float alpha, float mom, float theta, float emcdz);
    double evalLinearInterpolation(double x0, double y0, double x1, double y1, double x);
    int getBin(int type, float val);
};

#endif /* __RUN14AUAU200ELECTRONEMCMATCHING_H__ */
