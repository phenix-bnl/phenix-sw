#ifndef __emcEfficiency_7percent_counts_h__
#define __emcEfficiency_7percent_counts_h__

// IMPORTANT MESSAGE: 
//
// This set of efficiency correction factors were produced running: 
//
// emcEfficiency::outputFinalEfficiencyFactors_all_cuts("counts",0.8,10,"thisfile.h")
// on file: /home/enterria/wa/efficiencies_final_ppg014/efficiency_7percent_smear_659_2pass_3dHagWeigh.root
// on file: /home/enterria/wa/efficiencies_final_ppg014_as_in_local_disk2077/efficiency_7percent_smear_659_3pass.root (same result)
//

// The parameters correspond to the fitting function: [0]+[1]*x+[2]*x*x+[3]*log(x)+[4]*log(x)*log(x)
//

const int CentClassesPi0 = 20;

double P0_NoCut_counts[CentClassesPi0] = { 
          0.989563, 0.989563, // Centrality [0-10[ % 
          1.053378, 1.053378, // Centrality [10-20[ % 
          0.893095, 0.893095, // Centrality [20-30[ % 
          1.085185, 1.085185, // Centrality [30-40[ % 
          1.043015, 1.043015, // Centrality [40-50[ % 
          1.052075, 1.052075, // Centrality [50-60[ % 
          1.128147, 1.128147, // Centrality [60-70[ % 
          1.043957, 1.043957, // Centrality [70-80[ % 
          1.061363, 1.043957, // Centrality [80-93[ %  and 60-92% (=70-80%) = (70-92% though not exactly true)
          1.128147, 1.037184  // 60-80%(=60-70%) and Min.bias
};
double P0ERR_NoCut_counts[CentClassesPi0] = { 
          0.166038, 0.166038, // Centrality [0-10[ % 
          0.205525, 0.205525, // Centrality [10-20[ % 
          0.124421, 0.124421, // Centrality [20-30[ % 
          0.117315, 0.117315, // Centrality [30-40[ % 
          0.112426, 0.112426, // Centrality [40-50[ % 
          0.128189, 0.128189, // Centrality [50-60[ % 
          0.106575, 0.106575, // Centrality [60-70[ % 
          0.105139, 0.105139, // Centrality [70-80[ % 
          0.095553, 0.105139, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.106575, 0.062618  // 60-80%(=60-70%) and Min.bias
};
double P1_NoCut_counts[CentClassesPi0] = { 
          0.184470, 0.184470, // Centrality [0-10[ % 
          -0.077360, -0.077360, // Centrality [10-20[ % 
          0.059430, 0.059430, // Centrality [20-30[ % 
          -0.252271, -0.252271, // Centrality [30-40[ % 
          -0.232237, -0.232237, // Centrality [40-50[ % 
          -0.231437, -0.231437, // Centrality [50-60[ % 
          -0.310316, -0.310316, // Centrality [60-70[ % 
          -0.240031, -0.240031, // Centrality [70-80[ % 
          -0.255917, -0.240031, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.310316, -0.185142  // 60-80%(=60-70%) and Min.bias
};
double P1ERR_NoCut_counts[CentClassesPi0] = { 
          0.178295, 0.178295, // Centrality [0-10[ % 
          0.219506, 0.219506, // Centrality [10-20[ % 
          0.133238, 0.133238, // Centrality [20-30[ % 
          0.124931, 0.124931, // Centrality [30-40[ % 
          0.119591, 0.119591, // Centrality [40-50[ % 
          0.136366, 0.136366, // Centrality [50-60[ % 
          0.112961, 0.112961, // Centrality [60-70[ % 
          0.111653, 0.111653, // Centrality [70-80[ % 
          0.101351, 0.111653, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.112961, 0.066661  // 60-80%(=60-70%) and Min.bias
};
double P2_NoCut_counts[CentClassesPi0] = { 
          -0.000891, -0.000891, // Centrality [0-10[ % 
          0.004317, 0.004317, // Centrality [10-20[ % 
          -0.001464, -0.001464, // Centrality [20-30[ % 
          0.006840, 0.006840, // Centrality [30-40[ % 
          0.006363, 0.006363, // Centrality [40-50[ % 
          0.005434, 0.005434, // Centrality [50-60[ % 
          0.007707, 0.007707, // Centrality [60-70[ % 
          0.005748, 0.005748, // Centrality [70-80[ % 
          0.006225, 0.005748, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.007707, 0.004050  // 60-80%(=60-70%) and Min.bias
};
double P2ERR_NoCut_counts[CentClassesPi0] = { 
          0.005599, 0.005599, // Centrality [0-10[ % 
          0.007653, 0.007653, // Centrality [10-20[ % 
          0.004219, 0.004219, // Centrality [20-30[ % 
          0.004017, 0.004017, // Centrality [30-40[ % 
          0.003868, 0.003868, // Centrality [40-50[ % 
          0.004590, 0.004590, // Centrality [50-60[ % 
          0.003605, 0.003605, // Centrality [60-70[ % 
          0.003566, 0.003566, // Centrality [70-80[ % 
          0.003234, 0.003566, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.003605, 0.002292  // 60-80%(=60-70%) and Min.bias
};
double P3_NoCut_counts[CentClassesPi0] = { 
          0.123164, 0.123164, // Centrality [0-10[ % 
          0.248716, 0.248716, // Centrality [10-20[ % 
          0.090458, 0.090458, // Centrality [20-30[ % 
          0.313465, 0.313465, // Centrality [30-40[ % 
          0.293243, 0.293243, // Centrality [40-50[ % 
          0.255624, 0.255624, // Centrality [50-60[ % 
          0.268583, 0.268583, // Centrality [60-70[ % 
          0.249725, 0.249725, // Centrality [70-80[ % 
          0.252889, 0.249725, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.268583, 0.241892  // 60-80%(=60-70%) and Min.bias
};
double P3ERR_NoCut_counts[CentClassesPi0] = { 
          0.143376, 0.143376, // Centrality [0-10[ % 
          0.185685, 0.185685, // Centrality [10-20[ % 
          0.105515, 0.105515, // Centrality [20-30[ % 
          0.099982, 0.099982, // Centrality [30-40[ % 
          0.095605, 0.095605, // Centrality [40-50[ % 
          0.110817, 0.110817, // Centrality [50-60[ % 
          0.089118, 0.089118, // Centrality [60-70[ % 
          0.088400, 0.088400, // Centrality [70-80[ % 
          0.080025, 0.088400, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.089118, 0.055117  // 60-80%(=60-70%) and Min.bias
};
double P4_NoCut_counts[CentClassesPi0] = { 
          -0.343076, -0.343076, // Centrality [0-10[ % 
          -0.012465, -0.012465, // Centrality [10-20[ % 
          -0.099331, -0.099331, // Centrality [20-30[ % 
          0.200919, 0.200919, // Centrality [30-40[ % 
          0.184268, 0.184268, // Centrality [40-50[ % 
          0.206923, 0.206923, // Centrality [50-60[ % 
          0.297766, 0.297766, // Centrality [60-70[ % 
          0.221245, 0.221245, // Centrality [70-80[ % 
          0.239891, 0.221245, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.297766, 0.160777  // 60-80%(=60-70%) and Min.bias
};
double P4ERR_NoCut_counts[CentClassesPi0] = { 
          0.143287, 0.143287, // Centrality [0-10[ % 
          0.159196, 0.159196, // Centrality [10-20[ % 
          0.107534, 0.107534, // Centrality [20-30[ % 
          0.099330, 0.099330, // Centrality [30-40[ % 
          0.094599, 0.094599, // Centrality [40-50[ % 
          0.104129, 0.104129, // Centrality [50-60[ % 
          0.090725, 0.090725, // Centrality [60-70[ % 
          0.089470, 0.089470, // Centrality [70-80[ % 
          0.081376, 0.089470, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.090725, 0.049781  // 60-80%(=60-70%) and Min.bias
};
double P0_FiduCut_counts[CentClassesPi0] = { 
          0.778479, 0.778479, // Centrality [0-10[ % 
          0.744037, 0.744037, // Centrality [10-20[ % 
          0.630236, 0.630236, // Centrality [20-30[ % 
          0.855937, 0.855937, // Centrality [30-40[ % 
          0.704695, 0.704695, // Centrality [40-50[ % 
          0.707115, 0.707115, // Centrality [50-60[ % 
          0.790931, 0.790931, // Centrality [60-70[ % 
          0.662850, 0.662850, // Centrality [70-80[ % 
          0.765758, 0.662850, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.790931, 0.703583  // 60-80%(=60-70%) and Min.bias
};
double P0ERR_FiduCut_counts[CentClassesPi0] = { 
          0.147333, 0.147333, // Centrality [0-10[ % 
          0.182874, 0.182874, // Centrality [10-20[ % 
          0.109216, 0.109216, // Centrality [20-30[ % 
          0.104885, 0.104885, // Centrality [30-40[ % 
          0.100683, 0.100683, // Centrality [40-50[ % 
          0.114185, 0.114185, // Centrality [50-60[ % 
          0.094494, 0.094494, // Centrality [60-70[ % 
          0.093692, 0.093692, // Centrality [70-80[ % 
          0.085217, 0.093692, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.094494, 0.055197  // 60-80%(=60-70%) and Min.bias
};
double P1_FiduCut_counts[CentClassesPi0] = { 
          0.087830, 0.087830, // Centrality [0-10[ % 
          -0.018014, -0.018014, // Centrality [10-20[ % 
          0.086171, 0.086171, // Centrality [20-30[ % 
          -0.232083, -0.232083, // Centrality [30-40[ % 
          -0.102060, -0.102060, // Centrality [40-50[ % 
          -0.094546, -0.094546, // Centrality [50-60[ % 
          -0.182821, -0.182821, // Centrality [60-70[ % 
          -0.061640, -0.061640, // Centrality [70-80[ % 
          -0.155785, -0.061640, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.182821, -0.066260  // 60-80%(=60-70%) and Min.bias
};
double P1ERR_FiduCut_counts[CentClassesPi0] = { 
          0.158216, 0.158216, // Centrality [0-10[ % 
          0.195411, 0.195411, // Centrality [10-20[ % 
          0.117053, 0.117053, // Centrality [20-30[ % 
          0.111690, 0.111690, // Centrality [30-40[ % 
          0.107318, 0.107318, // Centrality [40-50[ % 
          0.121663, 0.121663, // Centrality [50-60[ % 
          0.100370, 0.100370, // Centrality [60-70[ % 
          0.099730, 0.099730, // Centrality [70-80[ % 
          0.090509, 0.099730, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.100370, 0.058839  // 60-80%(=60-70%) and Min.bias
};
double P2_FiduCut_counts[CentClassesPi0] = { 
          -0.000395, -0.000395, // Centrality [0-10[ % 
          0.002271, 0.002271, // Centrality [10-20[ % 
          -0.003284, -0.003284, // Centrality [20-30[ % 
          0.005497, 0.005497, // Centrality [30-40[ % 
          0.001847, 0.001847, // Centrality [40-50[ % 
          0.001285, 0.001285, // Centrality [50-60[ % 
          0.003618, 0.003618, // Centrality [60-70[ % 
          0.000061, 0.000061, // Centrality [70-80[ % 
          0.002825, 0.000061, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.003618, -0.000113  // 60-80%(=60-70%) and Min.bias
};
double P2ERR_FiduCut_counts[CentClassesPi0] = { 
          0.005008, 0.005008, // Centrality [0-10[ % 
          0.006866, 0.006866, // Centrality [10-20[ % 
          0.003717, 0.003717, // Centrality [20-30[ % 
          0.003627, 0.003627, // Centrality [30-40[ % 
          0.003500, 0.003500, // Centrality [40-50[ % 
          0.004123, 0.004123, // Centrality [50-60[ % 
          0.003224, 0.003224, // Centrality [60-70[ % 
          0.003206, 0.003206, // Centrality [70-80[ % 
          0.002905, 0.003206, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.003224, 0.002045  // 60-80%(=60-70%) and Min.bias
};
double P3_FiduCut_counts[CentClassesPi0] = { 
          0.188726, 0.188726, // Centrality [0-10[ % 
          0.222101, 0.222101, // Centrality [10-20[ % 
          0.079747, 0.079747, // Centrality [20-30[ % 
          0.306141, 0.306141, // Centrality [30-40[ % 
          0.218421, 0.218421, // Centrality [40-50[ % 
          0.187421, 0.187421, // Centrality [50-60[ % 
          0.217376, 0.217376, // Centrality [60-70[ % 
          0.145827, 0.145827, // Centrality [70-80[ % 
          0.196830, 0.145827, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.217376, 0.171292  // 60-80%(=60-70%) and Min.bias
};
double P3ERR_FiduCut_counts[CentClassesPi0] = { 
          0.128322, 0.128322, // Centrality [0-10[ % 
          0.165878, 0.165878, // Centrality [10-20[ % 
          0.093282, 0.093282, // Centrality [20-30[ % 
          0.090389, 0.090389, // Centrality [30-40[ % 
          0.086605, 0.086605, // Centrality [40-50[ % 
          0.099619, 0.099619, // Centrality [50-60[ % 
          0.079880, 0.079880, // Centrality [60-70[ % 
          0.079623, 0.079623, // Centrality [70-80[ % 
          0.071976, 0.079623, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.079880, 0.049168  // 60-80%(=60-70%) and Min.bias
};
double P4_FiduCut_counts[CentClassesPi0] = { 
          -0.193524, -0.193524, // Centrality [0-10[ % 
          -0.046648, -0.046648, // Centrality [10-20[ % 
          -0.091208, -0.091208, // Centrality [20-30[ % 
          0.205046, 0.205046, // Centrality [30-40[ % 
          0.091484, 0.091484, // Centrality [40-50[ % 
          0.096645, 0.096645, // Centrality [50-60[ % 
          0.191758, 0.191758, // Centrality [60-70[ % 
          0.082050, 0.082050, // Centrality [70-80[ % 
          0.167812, 0.082050, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.191758, 0.080798  // 60-80%(=60-70%) and Min.bias
};
double P4ERR_FiduCut_counts[CentClassesPi0] = { 
          0.125907, 0.125907, // Centrality [0-10[ % 
          0.140832, 0.140832, // Centrality [10-20[ % 
          0.093998, 0.093998, // Centrality [20-30[ % 
          0.087569, 0.087569, // Centrality [30-40[ % 
          0.083975, 0.083975, // Centrality [40-50[ % 
          0.092059, 0.092059, // Centrality [50-60[ % 
          0.079913, 0.079913, // Centrality [60-70[ % 
          0.079208, 0.079208, // Centrality [70-80[ % 
          0.072126, 0.079208, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.079913, 0.043254  // 60-80%(=60-70%) and Min.bias
};
double P0_NoW3Cut_counts[CentClassesPi0] = { 
          0.715847, 0.715847, // Centrality [0-10[ % 
          0.753260, 0.753260, // Centrality [10-20[ % 
          0.687223, 0.687223, // Centrality [20-30[ % 
          0.822328, 0.822328, // Centrality [30-40[ % 
          0.816093, 0.816093, // Centrality [40-50[ % 
          0.746756, 0.746756, // Centrality [50-60[ % 
          0.863365, 0.863365, // Centrality [60-70[ % 
          0.773773, 0.773773, // Centrality [70-80[ % 
          0.818892, 0.773773, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.863365, 0.754062  // 60-80%(=60-70%) and Min.bias
};
double P0ERR_NoW3Cut_counts[CentClassesPi0] = { 
          0.149481, 0.149481, // Centrality [0-10[ % 
          0.189798, 0.189798, // Centrality [10-20[ % 
          0.110578, 0.110578, // Centrality [20-30[ % 
          0.104807, 0.104807, // Centrality [30-40[ % 
          0.101071, 0.101071, // Centrality [40-50[ % 
          0.114783, 0.114783, // Centrality [50-60[ % 
          0.095174, 0.095174, // Centrality [60-70[ % 
          0.093340, 0.093340, // Centrality [70-80[ % 
          0.085174, 0.093340, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.095174, 0.055775  // 60-80%(=60-70%) and Min.bias
};
double P1_NoW3Cut_counts[CentClassesPi0] = { 
          0.207852, 0.207852, // Centrality [0-10[ % 
          0.015645, 0.015645, // Centrality [10-20[ % 
          0.066751, 0.066751, // Centrality [20-30[ % 
          -0.170427, -0.170427, // Centrality [30-40[ % 
          -0.181718, -0.181718, // Centrality [40-50[ % 
          -0.094836, -0.094836, // Centrality [50-60[ % 
          -0.221593, -0.221593, // Centrality [60-70[ % 
          -0.145553, -0.145553, // Centrality [70-80[ % 
          -0.187930, -0.145553, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.221593, -0.082990  // 60-80%(=60-70%) and Min.bias
};
double P1ERR_NoW3Cut_counts[CentClassesPi0] = { 
          0.160985, 0.160985, // Centrality [0-10[ % 
          0.202962, 0.202962, // Centrality [10-20[ % 
          0.118572, 0.118572, // Centrality [20-30[ % 
          0.111781, 0.111781, // Centrality [30-40[ % 
          0.107610, 0.107610, // Centrality [40-50[ % 
          0.122249, 0.122249, // Centrality [50-60[ % 
          0.101064, 0.101064, // Centrality [60-70[ % 
          0.099333, 0.099333, // Centrality [70-80[ % 
          0.090484, 0.099333, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.101064, 0.059463  // 60-80%(=60-70%) and Min.bias
};
double P2_NoW3Cut_counts[CentClassesPi0] = { 
          -0.002282, -0.002282, // Centrality [0-10[ % 
          0.002004, 0.002004, // Centrality [10-20[ % 
          -0.001843, -0.001843, // Centrality [20-30[ % 
          0.004822, 0.004822, // Centrality [30-40[ % 
          0.005156, 0.005156, // Centrality [40-50[ % 
          0.001349, 0.001349, // Centrality [50-60[ % 
          0.005273, 0.005273, // Centrality [60-70[ % 
          0.003160, 0.003160, // Centrality [70-80[ % 
          0.004689, 0.003160, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.005273, 0.001366  // 60-80%(=60-70%) and Min.bias
};
double P2ERR_NoW3Cut_counts[CentClassesPi0] = { 
          0.005073, 0.005073, // Centrality [0-10[ % 
          0.007140, 0.007140, // Centrality [10-20[ % 
          0.003761, 0.003761, // Centrality [20-30[ % 
          0.003619, 0.003619, // Centrality [30-40[ % 
          0.003501, 0.003501, // Centrality [40-50[ % 
          0.004128, 0.004128, // Centrality [50-60[ % 
          0.003239, 0.003239, // Centrality [60-70[ % 
          0.003183, 0.003183, // Centrality [70-80[ % 
          0.002896, 0.003183, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.003239, 0.002066  // 60-80%(=60-70%) and Min.bias
};
double P3_NoW3Cut_counts[CentClassesPi0] = { 
          0.148220, 0.148220, // Centrality [0-10[ % 
          0.222024, 0.222024, // Centrality [10-20[ % 
          0.093798, 0.093798, // Centrality [20-30[ % 
          0.296073, 0.296073, // Centrality [30-40[ % 
          0.280476, 0.280476, // Centrality [40-50[ % 
          0.165816, 0.165816, // Centrality [50-60[ % 
          0.242089, 0.242089, // Centrality [60-70[ % 
          0.207428, 0.207428, // Centrality [70-80[ % 
          0.242254, 0.207428, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.242089, 0.194150  // 60-80%(=60-70%) and Min.bias
};
double P3ERR_NoW3Cut_counts[CentClassesPi0] = { 
          0.130320, 0.130320, // Centrality [0-10[ % 
          0.172983, 0.172983, // Centrality [10-20[ % 
          0.093932, 0.093932, // Centrality [20-30[ % 
          0.090419, 0.090419, // Centrality [30-40[ % 
          0.086578, 0.086578, // Centrality [40-50[ % 
          0.099744, 0.099744, // Centrality [50-60[ % 
          0.080290, 0.080290, // Centrality [60-70[ % 
          0.079108, 0.079108, // Centrality [70-80[ % 
          0.071788, 0.079108, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.080290, 0.049627  // 60-80%(=60-70%) and Min.bias
};
double P4_NoW3Cut_counts[CentClassesPi0] = { 
          -0.358472, -0.358472, // Centrality [0-10[ % 
          -0.111925, -0.111925, // Centrality [10-20[ % 
          -0.102086, -0.102086, // Centrality [20-30[ % 
          0.109139, 0.109139, // Centrality [30-40[ % 
          0.130044, 0.130044, // Centrality [40-50[ % 
          0.091657, 0.091657, // Centrality [50-60[ % 
          0.206105, 0.206105, // Centrality [60-70[ % 
          0.129910, 0.129910, // Centrality [70-80[ % 
          0.160557, 0.129910, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.206105, 0.061718  // 60-80%(=60-70%) and Min.bias
};
double P4ERR_NoW3Cut_counts[CentClassesPi0] = { 
          0.128616, 0.128616, // Centrality [0-10[ % 
          0.145429, 0.145429, // Centrality [10-20[ % 
          0.095579, 0.095579, // Centrality [20-30[ % 
          0.087939, 0.087939, // Centrality [30-40[ % 
          0.084504, 0.084504, // Centrality [40-50[ % 
          0.092923, 0.092923, // Centrality [50-60[ % 
          0.080650, 0.080650, // Centrality [60-70[ % 
          0.079177, 0.079177, // Centrality [70-80[ % 
          0.072329, 0.079177, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.080650, 0.043719  // 60-80%(=60-70%) and Min.bias
};
double P0_Deadwarn5x5Cut_counts[CentClassesPi0] = { 
          0.655037, 0.655037, // Centrality [0-10[ % 
          0.717106, 0.717106, // Centrality [10-20[ % 
          0.617069, 0.617069, // Centrality [20-30[ % 
          0.725431, 0.725431, // Centrality [30-40[ % 
          0.721729, 0.721729, // Centrality [40-50[ % 
          0.699682, 0.699682, // Centrality [50-60[ % 
          0.772295, 0.772295, // Centrality [60-70[ % 
          0.712878, 0.712878, // Centrality [70-80[ % 
          0.673099, 0.712878, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.772295, 0.725584  // 60-80%(=60-70%) and Min.bias
};
double P0ERR_Deadwarn5x5Cut_counts[CentClassesPi0] = { 
          0.118083, 0.118083, // Centrality [0-10[ % 
          0.109892, 0.109892, // Centrality [10-20[ % 
          0.092145, 0.092145, // Centrality [20-30[ % 
          0.085551, 0.085551, // Centrality [30-40[ % 
          0.084599, 0.084599, // Centrality [40-50[ % 
          0.080433, 0.080433, // Centrality [50-60[ % 
          0.079471, 0.079471, // Centrality [60-70[ % 
          0.078376, 0.078376, // Centrality [70-80[ % 
          0.071455, 0.078376, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.079471, 0.036102  // 60-80%(=60-70%) and Min.bias
};
double P1_Deadwarn5x5Cut_counts[CentClassesPi0] = { 
          0.172164, 0.172164, // Centrality [0-10[ % 
          -0.039916, -0.039916, // Centrality [10-20[ % 
          0.046825, 0.046825, // Centrality [20-30[ % 
          -0.143655, -0.143655, // Centrality [30-40[ % 
          -0.151437, -0.151437, // Centrality [40-50[ % 
          -0.123774, -0.123774, // Centrality [50-60[ % 
          -0.196180, -0.196180, // Centrality [60-70[ % 
          -0.151897, -0.151897, // Centrality [70-80[ % 
          -0.106586, -0.151897, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.196180, -0.130731  // 60-80%(=60-70%) and Min.bias
};
double P1ERR_Deadwarn5x5Cut_counts[CentClassesPi0] = { 
          0.126876, 0.126876, // Centrality [0-10[ % 
          0.117546, 0.117546, // Centrality [10-20[ % 
          0.098665, 0.098665, // Centrality [20-30[ % 
          0.091167, 0.091167, // Centrality [30-40[ % 
          0.089999, 0.089999, // Centrality [40-50[ % 
          0.085450, 0.085450, // Centrality [50-60[ % 
          0.084171, 0.084171, // Centrality [60-70[ % 
          0.083200, 0.083200, // Centrality [70-80[ % 
          0.075807, 0.083200, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.084171, 0.038416  // 60-80%(=60-70%) and Min.bias
};
double P2_Deadwarn5x5Cut_counts[CentClassesPi0] = { 
          -0.002246, -0.002246, // Centrality [0-10[ % 
          0.002142, 0.002142, // Centrality [10-20[ % 
          -0.001767, -0.001767, // Centrality [20-30[ % 
          0.003969, 0.003969, // Centrality [30-40[ % 
          0.003971, 0.003971, // Centrality [40-50[ % 
          0.002855, 0.002855, // Centrality [50-60[ % 
          0.004403, 0.004403, // Centrality [60-70[ % 
          0.003167, 0.003167, // Centrality [70-80[ % 
          0.001870, 0.003167, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.004403, 0.003176  // 60-80%(=60-70%) and Min.bias
};
double P2ERR_Deadwarn5x5Cut_counts[CentClassesPi0] = { 
          0.003991, 0.003991, // Centrality [0-10[ % 
          0.003783, 0.003783, // Centrality [10-20[ % 
          0.003129, 0.003129, // Centrality [20-30[ % 
          0.002914, 0.002914, // Centrality [30-40[ % 
          0.002912, 0.002912, // Centrality [40-50[ % 
          0.002735, 0.002735, // Centrality [50-60[ % 
          0.002688, 0.002688, // Centrality [60-70[ % 
          0.002661, 0.002661, // Centrality [70-80[ % 
          0.002422, 0.002661, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.002688, 0.001228  // 60-80%(=60-70%) and Min.bias
};
double P3_Deadwarn5x5Cut_counts[CentClassesPi0] = { 
          0.051613, 0.051613, // Centrality [0-10[ % 
          0.172851, 0.172851, // Centrality [10-20[ % 
          0.042189, 0.042189, // Centrality [20-30[ % 
          0.203799, 0.203799, // Centrality [30-40[ % 
          0.191920, 0.191920, // Centrality [40-50[ % 
          0.140829, 0.140829, // Centrality [50-60[ % 
          0.156541, 0.156541, // Centrality [60-70[ % 
          0.152290, 0.152290, // Centrality [70-80[ % 
          0.114783, 0.152290, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.156541, 0.168218  // 60-80%(=60-70%) and Min.bias
};
double P3ERR_Deadwarn5x5Cut_counts[CentClassesPi0] = { 
          0.100964, 0.100964, // Centrality [0-10[ % 
          0.095883, 0.095883, // Centrality [10-20[ % 
          0.078151, 0.078151, // Centrality [20-30[ % 
          0.072694, 0.072694, // Centrality [30-40[ % 
          0.071989, 0.071989, // Centrality [40-50[ % 
          0.067778, 0.067778, // Centrality [50-60[ % 
          0.066420, 0.066420, // Centrality [60-70[ % 
          0.065907, 0.065907, // Centrality [70-80[ % 
          0.059916, 0.065907, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.066420, 0.030658  // 60-80%(=60-70%) and Min.bias
};
double P4_Deadwarn5x5Cut_counts[CentClassesPi0] = { 
          -0.266505, -0.266505, // Centrality [0-10[ % 
          -0.017345, -0.017345, // Centrality [10-20[ % 
          -0.053447, -0.053447, // Centrality [20-30[ % 
          0.109603, 0.109603, // Centrality [30-40[ % 
          0.126727, 0.126727, // Centrality [40-50[ % 
          0.122268, 0.122268, // Centrality [50-60[ % 
          0.206792, 0.206792, // Centrality [60-70[ % 
          0.155782, 0.155782, // Centrality [70-80[ % 
          0.120616, 0.155782, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.206792, 0.115617  // 60-80%(=60-70%) and Min.bias
};
double P4ERR_Deadwarn5x5Cut_counts[CentClassesPi0] = { 
          0.102621, 0.102621, // Centrality [0-10[ % 
          0.092047, 0.092047, // Centrality [10-20[ % 
          0.079400, 0.079400, // Centrality [20-30[ % 
          0.072842, 0.072842, // Centrality [30-40[ % 
          0.071112, 0.071112, // Centrality [40-50[ % 
          0.068318, 0.068318, // Centrality [50-60[ % 
          0.067578, 0.067578, // Centrality [60-70[ % 
          0.066602, 0.066602, // Centrality [70-80[ % 
          0.060787, 0.066602, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.067578, 0.030618  // 60-80%(=60-70%) and Min.bias
};
double P0_EnergyCut_counts[CentClassesPi0] = { 
          0.860107, 0.860107, // Centrality [0-10[ % 
          0.911778, 0.911778, // Centrality [10-20[ % 
          0.843026, 0.843026, // Centrality [20-30[ % 
          0.941374, 0.941374, // Centrality [30-40[ % 
          0.930103, 0.930103, // Centrality [40-50[ % 
          0.884159, 0.884159, // Centrality [50-60[ % 
          0.984367, 0.984367, // Centrality [60-70[ % 
          0.894554, 0.894554, // Centrality [70-80[ % 
          0.919662, 0.894554, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.984367, 0.902227  // 60-80%(=60-70%) and Min.bias
};
double P0ERR_EnergyCut_counts[CentClassesPi0] = { 
          0.160854, 0.160854, // Centrality [0-10[ % 
          0.199733, 0.199733, // Centrality [10-20[ % 
          0.117982, 0.117982, // Centrality [20-30[ % 
          0.112629, 0.112629, // Centrality [30-40[ % 
          0.109110, 0.109110, // Centrality [40-50[ % 
          0.124816, 0.124816, // Centrality [50-60[ % 
          0.103647, 0.103647, // Centrality [60-70[ % 
          0.102206, 0.102206, // Centrality [70-80[ % 
          0.092792, 0.102206, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.103647, 0.060517  // 60-80%(=60-70%) and Min.bias
};
double P1_EnergyCut_counts[CentClassesPi0] = { 
          0.235702, 0.235702, // Centrality [0-10[ % 
          -0.015195, -0.015195, // Centrality [10-20[ % 
          0.013540, 0.013540, // Centrality [20-30[ % 
          -0.178570, -0.178570, // Centrality [30-40[ % 
          -0.188748, -0.188748, // Centrality [40-50[ % 
          -0.133467, -0.133467, // Centrality [50-60[ % 
          -0.231381, -0.231381, // Centrality [60-70[ % 
          -0.161734, -0.161734, // Centrality [70-80[ % 
          -0.184919, -0.161734, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.231381, -0.122727  // 60-80%(=60-70%) and Min.bias
};
double P1ERR_EnergyCut_counts[CentClassesPi0] = { 
          0.172814, 0.172814, // Centrality [0-10[ % 
          0.213407, 0.213407, // Centrality [10-20[ % 
          0.126014, 0.126014, // Centrality [20-30[ % 
          0.120089, 0.120089, // Centrality [30-40[ % 
          0.116221, 0.116221, // Centrality [40-50[ % 
          0.133055, 0.133055, // Centrality [50-60[ % 
          0.110073, 0.110073, // Centrality [60-70[ % 
          0.108778, 0.108778, // Centrality [70-80[ % 
          0.098648, 0.108778, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.110073, 0.064502  // 60-80%(=60-70%) and Min.bias
};
double P2_EnergyCut_counts[CentClassesPi0] = { 
          -0.002451, -0.002451, // Centrality [0-10[ % 
          0.002498, 0.002498, // Centrality [10-20[ % 
          -0.000241, -0.000241, // Centrality [20-30[ % 
          0.004810, 0.004810, // Centrality [30-40[ % 
          0.005147, 0.005147, // Centrality [40-50[ % 
          0.002791, 0.002791, // Centrality [50-60[ % 
          0.005505, 0.005505, // Centrality [60-70[ % 
          0.003609, 0.003609, // Centrality [70-80[ % 
          0.004200, 0.003609, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.005505, 0.002274  // 60-80%(=60-70%) and Min.bias
};
double P2ERR_EnergyCut_counts[CentClassesPi0] = { 
          0.005459, 0.005459, // Centrality [0-10[ % 
          0.007485, 0.007485, // Centrality [10-20[ % 
          0.004029, 0.004029, // Centrality [20-30[ % 
          0.003889, 0.003889, // Centrality [30-40[ % 
          0.003775, 0.003775, // Centrality [40-50[ % 
          0.004497, 0.004497, // Centrality [50-60[ % 
          0.003527, 0.003527, // Centrality [60-70[ % 
          0.003488, 0.003488, // Centrality [70-80[ % 
          0.003161, 0.003488, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.003527, 0.002234  // 60-80%(=60-70%) and Min.bias
};
double P3_EnergyCut_counts[CentClassesPi0] = { 
          0.152044, 0.152044, // Centrality [0-10[ % 
          0.274731, 0.274731, // Centrality [10-20[ % 
          0.191912, 0.191912, // Centrality [20-30[ % 
          0.327220, 0.327220, // Centrality [30-40[ % 
          0.322447, 0.322447, // Centrality [40-50[ % 
          0.254232, 0.254232, // Centrality [50-60[ % 
          0.270989, 0.270989, // Centrality [60-70[ % 
          0.261236, 0.261236, // Centrality [70-80[ % 
          0.262904, 0.261236, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.270989, 0.261197  // 60-80%(=60-70%) and Min.bias
};
double P3ERR_EnergyCut_counts[CentClassesPi0] = { 
          0.140204, 0.140204, // Centrality [0-10[ % 
          0.181641, 0.181641, // Centrality [10-20[ % 
          0.100734, 0.100734, // Centrality [20-30[ % 
          0.096915, 0.096915, // Centrality [30-40[ % 
          0.093471, 0.093471, // Centrality [40-50[ % 
          0.108792, 0.108792, // Centrality [50-60[ % 
          0.087414, 0.087414, // Centrality [60-70[ % 
          0.086666, 0.086666, // Centrality [70-80[ % 
          0.078429, 0.086666, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.087414, 0.053728  // 60-80%(=60-70%) and Min.bias
};
double P4_EnergyCut_counts[CentClassesPi0] = { 
          -0.401506, -0.401506, // Centrality [0-10[ % 
          -0.082682, -0.082682, // Centrality [10-20[ % 
          -0.072429, -0.072429, // Centrality [20-30[ % 
          0.117961, 0.117961, // Centrality [30-40[ % 
          0.130659, 0.130659, // Centrality [40-50[ % 
          0.101920, 0.101920, // Centrality [50-60[ % 
          0.213588, 0.213588, // Centrality [60-70[ % 
          0.133811, 0.133811, // Centrality [70-80[ % 
          0.163467, 0.133811, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.213588, 0.090704  // 60-80%(=60-70%) and Min.bias
};
double P4ERR_EnergyCut_counts[CentClassesPi0] = { 
          0.137649, 0.137649, // Centrality [0-10[ % 
          0.153464, 0.153464, // Centrality [10-20[ % 
          0.100431, 0.100431, // Centrality [20-30[ % 
          0.094616, 0.094616, // Centrality [30-40[ % 
          0.091367, 0.091367, // Centrality [40-50[ % 
          0.100946, 0.100946, // Centrality [50-60[ % 
          0.087862, 0.087862, // Centrality [60-70[ % 
          0.086659, 0.086659, // Centrality [70-80[ % 
          0.078700, 0.086659, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.087862, 0.047702  // 60-80%(=60-70%) and Min.bias
};
double P0_CosCut_counts[CentClassesPi0] = { 
          0.989563, 0.989563, // Centrality [0-10[ % 
          1.053378, 1.053378, // Centrality [10-20[ % 
          0.893095, 0.893095, // Centrality [20-30[ % 
          1.085185, 1.085185, // Centrality [30-40[ % 
          1.043015, 1.043015, // Centrality [40-50[ % 
          1.052075, 1.052075, // Centrality [50-60[ % 
          1.128147, 1.128147, // Centrality [60-70[ % 
          1.043957, 1.043957, // Centrality [70-80[ % 
          1.061363, 1.043957, // Centrality [80-93[ %  and 60-92% (=70-80%)
          1.128147, 1.037184  // 60-80%(=60-70%) and Min.bias
};
double P0ERR_CosCut_counts[CentClassesPi0] = { 
          0.166038, 0.166038, // Centrality [0-10[ % 
          0.205525, 0.205525, // Centrality [10-20[ % 
          0.124421, 0.124421, // Centrality [20-30[ % 
          0.117315, 0.117315, // Centrality [30-40[ % 
          0.112426, 0.112426, // Centrality [40-50[ % 
          0.128189, 0.128189, // Centrality [50-60[ % 
          0.106575, 0.106575, // Centrality [60-70[ % 
          0.105139, 0.105139, // Centrality [70-80[ % 
          0.095553, 0.105139, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.106575, 0.062618  // 60-80%(=60-70%) and Min.bias
};
double P1_CosCut_counts[CentClassesPi0] = { 
          0.184470, 0.184470, // Centrality [0-10[ % 
          -0.077360, -0.077360, // Centrality [10-20[ % 
          0.059430, 0.059430, // Centrality [20-30[ % 
          -0.252271, -0.252271, // Centrality [30-40[ % 
          -0.232237, -0.232237, // Centrality [40-50[ % 
          -0.231437, -0.231437, // Centrality [50-60[ % 
          -0.310316, -0.310316, // Centrality [60-70[ % 
          -0.240031, -0.240031, // Centrality [70-80[ % 
          -0.255917, -0.240031, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.310316, -0.185142  // 60-80%(=60-70%) and Min.bias
};
double P1ERR_CosCut_counts[CentClassesPi0] = { 
          0.178295, 0.178295, // Centrality [0-10[ % 
          0.219506, 0.219506, // Centrality [10-20[ % 
          0.133238, 0.133238, // Centrality [20-30[ % 
          0.124931, 0.124931, // Centrality [30-40[ % 
          0.119591, 0.119591, // Centrality [40-50[ % 
          0.136366, 0.136366, // Centrality [50-60[ % 
          0.112961, 0.112961, // Centrality [60-70[ % 
          0.111653, 0.111653, // Centrality [70-80[ % 
          0.101351, 0.111653, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.112961, 0.066661  // 60-80%(=60-70%) and Min.bias
};
double P2_CosCut_counts[CentClassesPi0] = { 
          -0.000891, -0.000891, // Centrality [0-10[ % 
          0.004317, 0.004317, // Centrality [10-20[ % 
          -0.001464, -0.001464, // Centrality [20-30[ % 
          0.006840, 0.006840, // Centrality [30-40[ % 
          0.006363, 0.006363, // Centrality [40-50[ % 
          0.005434, 0.005434, // Centrality [50-60[ % 
          0.007707, 0.007707, // Centrality [60-70[ % 
          0.005748, 0.005748, // Centrality [70-80[ % 
          0.006225, 0.005748, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.007707, 0.004050  // 60-80%(=60-70%) and Min.bias
};
double P2ERR_CosCut_counts[CentClassesPi0] = { 
          0.005599, 0.005599, // Centrality [0-10[ % 
          0.007653, 0.007653, // Centrality [10-20[ % 
          0.004219, 0.004219, // Centrality [20-30[ % 
          0.004017, 0.004017, // Centrality [30-40[ % 
          0.003868, 0.003868, // Centrality [40-50[ % 
          0.004590, 0.004590, // Centrality [50-60[ % 
          0.003605, 0.003605, // Centrality [60-70[ % 
          0.003566, 0.003566, // Centrality [70-80[ % 
          0.003234, 0.003566, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.003605, 0.002292  // 60-80%(=60-70%) and Min.bias
};
double P3_CosCut_counts[CentClassesPi0] = { 
          0.123164, 0.123164, // Centrality [0-10[ % 
          0.248716, 0.248716, // Centrality [10-20[ % 
          0.090458, 0.090458, // Centrality [20-30[ % 
          0.313465, 0.313465, // Centrality [30-40[ % 
          0.293243, 0.293243, // Centrality [40-50[ % 
          0.255624, 0.255624, // Centrality [50-60[ % 
          0.268583, 0.268583, // Centrality [60-70[ % 
          0.249725, 0.249725, // Centrality [70-80[ % 
          0.252889, 0.249725, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.268583, 0.241892  // 60-80%(=60-70%) and Min.bias
};
double P3ERR_CosCut_counts[CentClassesPi0] = { 
          0.143376, 0.143376, // Centrality [0-10[ % 
          0.185685, 0.185685, // Centrality [10-20[ % 
          0.105515, 0.105515, // Centrality [20-30[ % 
          0.099982, 0.099982, // Centrality [30-40[ % 
          0.095605, 0.095605, // Centrality [40-50[ % 
          0.110817, 0.110817, // Centrality [50-60[ % 
          0.089118, 0.089118, // Centrality [60-70[ % 
          0.088400, 0.088400, // Centrality [70-80[ % 
          0.080025, 0.088400, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.089118, 0.055117  // 60-80%(=60-70%) and Min.bias
};
double P4_CosCut_counts[CentClassesPi0] = { 
          -0.343076, -0.343076, // Centrality [0-10[ % 
          -0.012465, -0.012465, // Centrality [10-20[ % 
          -0.099331, -0.099331, // Centrality [20-30[ % 
          0.200919, 0.200919, // Centrality [30-40[ % 
          0.184268, 0.184268, // Centrality [40-50[ % 
          0.206923, 0.206923, // Centrality [50-60[ % 
          0.297766, 0.297766, // Centrality [60-70[ % 
          0.221245, 0.221245, // Centrality [70-80[ % 
          0.239891, 0.221245, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.297766, 0.160777  // 60-80%(=60-70%) and Min.bias
};
double P4ERR_CosCut_counts[CentClassesPi0] = { 
          0.143287, 0.143287, // Centrality [0-10[ % 
          0.159196, 0.159196, // Centrality [10-20[ % 
          0.107534, 0.107534, // Centrality [20-30[ % 
          0.099330, 0.099330, // Centrality [30-40[ % 
          0.094599, 0.094599, // Centrality [40-50[ % 
          0.104129, 0.104129, // Centrality [50-60[ % 
          0.090725, 0.090725, // Centrality [60-70[ % 
          0.089470, 0.089470, // Centrality [70-80[ % 
          0.081376, 0.089470, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.090725, 0.049781  // 60-80%(=60-70%) and Min.bias
};
double P0_BasicCuts_FiduNoW3Deadwarn5x5EnergyCosCut_counts[CentClassesPi0] = { 
          0.279769, 0.279769, // Centrality [0-10[ % 
          0.352314, 0.352314, // Centrality [10-20[ % 
          0.351343, 0.351343, // Centrality [20-30[ % 
          0.430733, 0.430733, // Centrality [30-40[ % 
          0.380371, 0.380371, // Centrality [40-50[ % 
          0.303277, 0.303277, // Centrality [50-60[ % 
          0.395417, 0.395417, // Centrality [60-70[ % 
          0.328221, 0.328221, // Centrality [70-80[ % 
          0.364891, 0.328221, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.395417, 0.369509  // 60-80%(=60-70%) and Min.bias
};
double P0ERR_BasicCuts_FiduNoW3Deadwarn5x5EnergyCosCut_counts[CentClassesPi0] = { 
          0.094863, 0.094863, // Centrality [0-10[ % 
          0.087205, 0.087205, // Centrality [10-20[ % 
          0.071273, 0.071273, // Centrality [20-30[ % 
          0.068553, 0.068553, // Centrality [30-40[ % 
          0.068514, 0.068514, // Centrality [40-50[ % 
          0.064467, 0.064467, // Centrality [50-60[ % 
          0.063991, 0.063991, // Centrality [60-70[ % 
          0.062713, 0.062713, // Centrality [70-80[ % 
          0.057293, 0.062713, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.063991, 0.027220  // 60-80%(=60-70%) and Min.bias
};
double P1_BasicCuts_FiduNoW3Deadwarn5x5EnergyCosCut_counts[CentClassesPi0] = { 
          0.210322, 0.210322, // Centrality [0-10[ % 
          0.045808, 0.045808, // Centrality [10-20[ % 
          0.034452, 0.034452, // Centrality [20-30[ % 
          -0.094800, -0.094800, // Centrality [30-40[ % 
          -0.054785, -0.054785, // Centrality [40-50[ % 
          0.034234, 0.034234, // Centrality [50-60[ % 
          -0.060210, -0.060210, // Centrality [60-70[ % 
          -0.005924, -0.005924, // Centrality [70-80[ % 
          -0.035539, -0.005924, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.060210, -0.024190  // 60-80%(=60-70%) and Min.bias
};
double P1ERR_BasicCuts_FiduNoW3Deadwarn5x5EnergyCosCut_counts[CentClassesPi0] = { 
          0.102262, 0.102262, // Centrality [0-10[ % 
          0.093441, 0.093441, // Centrality [10-20[ % 
          0.076259, 0.076259, // Centrality [20-30[ % 
          0.073299, 0.073299, // Centrality [30-40[ % 
          0.073172, 0.073172, // Centrality [40-50[ % 
          0.068893, 0.068893, // Centrality [50-60[ % 
          0.068132, 0.068132, // Centrality [60-70[ % 
          0.066914, 0.066914, // Centrality [70-80[ % 
          0.061025, 0.066914, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.068132, 0.029083  // 60-80%(=60-70%) and Min.bias
};
double P2_BasicCuts_FiduNoW3Deadwarn5x5EnergyCosCut_counts[CentClassesPi0] = { 
          -0.004878, -0.004878, // Centrality [0-10[ % 
          -0.001015, -0.001015, // Centrality [10-20[ % 
          -0.001771, -0.001771, // Centrality [20-30[ % 
          0.002206, 0.002206, // Centrality [30-40[ % 
          0.001146, 0.001146, // Centrality [40-50[ % 
          -0.001859, -0.001859, // Centrality [50-60[ % 
          0.000584, 0.000584, // Centrality [60-70[ % 
          -0.000978, -0.000978, // Centrality [70-80[ % 
          -0.000087, -0.000978, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.000584, -0.000079  // 60-80%(=60-70%) and Min.bias
};
double P2ERR_BasicCuts_FiduNoW3Deadwarn5x5EnergyCosCut_counts[CentClassesPi0] = { 
          0.003263, 0.003263, // Centrality [0-10[ % 
          0.003064, 0.003064, // Centrality [10-20[ % 
          0.002445, 0.002445, // Centrality [20-30[ % 
          0.002380, 0.002380, // Centrality [30-40[ % 
          0.002406, 0.002406, // Centrality [40-50[ % 
          0.002231, 0.002231, // Centrality [50-60[ % 
          0.002201, 0.002201, // Centrality [60-70[ % 
          0.002164, 0.002164, // Centrality [70-80[ % 
          0.001971, 0.002164, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.002201, 0.000950  // 60-80%(=60-70%) and Min.bias
};
double P3_BasicCuts_FiduNoW3Deadwarn5x5EnergyCosCut_counts[CentClassesPi0] = { 
          0.064044, 0.064044, // Centrality [0-10[ % 
          0.150055, 0.150055, // Centrality [10-20[ % 
          0.099799, 0.099799, // Centrality [20-30[ % 
          0.212734, 0.212734, // Centrality [30-40[ % 
          0.174028, 0.174028, // Centrality [40-50[ % 
          0.079578, 0.079578, // Centrality [50-60[ % 
          0.130889, 0.130889, // Centrality [60-70[ % 
          0.106031, 0.106031, // Centrality [70-80[ % 
          0.119940, 0.106031, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.130889, 0.142286  // 60-80%(=60-70%) and Min.bias
};
double P3ERR_BasicCuts_FiduNoW3Deadwarn5x5EnergyCosCut_counts[CentClassesPi0] = { 
          0.083077, 0.083077, // Centrality [0-10[ % 
          0.077537, 0.077537, // Centrality [10-20[ % 
          0.061227, 0.061227, // Centrality [20-30[ % 
          0.059740, 0.059740, // Centrality [30-40[ % 
          0.059636, 0.059636, // Centrality [40-50[ % 
          0.055642, 0.055642, // Centrality [50-60[ % 
          0.054736, 0.054736, // Centrality [60-70[ % 
          0.053901, 0.053901, // Centrality [70-80[ % 
          0.049012, 0.053901, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.054736, 0.023774  // 60-80%(=60-70%) and Min.bias
};
double P4_BasicCuts_FiduNoW3Deadwarn5x5EnergyCosCut_counts[CentClassesPi0] = { 
          -0.266690, -0.266690, // Centrality [0-10[ % 
          -0.082782, -0.082782, // Centrality [10-20[ % 
          -0.041770, -0.041770, // Centrality [20-30[ % 
          0.064530, 0.064530, // Centrality [30-40[ % 
          0.035036, 0.035036, // Centrality [40-50[ % 
          -0.021793, -0.021793, // Centrality [50-60[ % 
          0.069062, 0.069062, // Centrality [60-70[ % 
          0.016099, 0.016099, // Centrality [70-80[ % 
          0.044583, 0.016099, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.069062, 0.017175  // 60-80%(=60-70%) and Min.bias
};
double P4ERR_BasicCuts_FiduNoW3Deadwarn5x5EnergyCosCut_counts[CentClassesPi0] = { 
          0.081005, 0.081005, // Centrality [0-10[ % 
          0.071552, 0.071552, // Centrality [10-20[ % 
          0.060505, 0.060505, // Centrality [20-30[ % 
          0.057230, 0.057230, // Centrality [30-40[ % 
          0.056609, 0.056609, // Centrality [40-50[ % 
          0.054109, 0.054109, // Centrality [50-60[ % 
          0.053758, 0.053758, // Centrality [60-70[ % 
          0.052675, 0.052675, // Centrality [70-80[ % 
          0.048165, 0.052675, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.053758, 0.022543  // 60-80%(=60-70%) and Min.bias
};
double P0_Asym1Cut_counts[CentClassesPi0] = { 
          0.988599, 0.988599, // Centrality [0-10[ % 
          0.845083, 0.845083, // Centrality [10-20[ % 
          0.900670, 0.900670, // Centrality [20-30[ % 
          1.016836, 1.016836, // Centrality [30-40[ % 
          0.967383, 0.967383, // Centrality [40-50[ % 
          0.986841, 0.986841, // Centrality [50-60[ % 
          1.025490, 1.025490, // Centrality [60-70[ % 
          0.937379, 0.937379, // Centrality [70-80[ % 
          0.913684, 0.937379, // Centrality [80-93[ %  and 60-92% (=70-80%)
          1.025490, 0.946354  // 60-80%(=60-70%) and Min.bias
};
double P0ERR_Asym1Cut_counts[CentClassesPi0] = { 
          0.151648, 0.151648, // Centrality [0-10[ % 
          0.137835, 0.137835, // Centrality [10-20[ % 
          0.111800, 0.111800, // Centrality [20-30[ % 
          0.107685, 0.107685, // Centrality [30-40[ % 
          0.104273, 0.104273, // Centrality [40-50[ % 
          0.118963, 0.118963, // Centrality [50-60[ % 
          0.098352, 0.098352, // Centrality [60-70[ % 
          0.097255, 0.097255, // Centrality [70-80[ % 
          0.088374, 0.097255, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.098352, 0.057546  // 60-80%(=60-70%) and Min.bias
};
double P1_Asym1Cut_counts[CentClassesPi0] = { 
          0.097720, 0.097720, // Centrality [0-10[ % 
          0.064317, 0.064317, // Centrality [10-20[ % 
          -0.037062, -0.037062, // Centrality [20-30[ % 
          -0.247791, -0.247791, // Centrality [30-40[ % 
          -0.221505, -0.221505, // Centrality [40-50[ % 
          -0.231881, -0.231881, // Centrality [50-60[ % 
          -0.267762, -0.267762, // Centrality [60-70[ % 
          -0.201365, -0.201365, // Centrality [70-80[ % 
          -0.171920, -0.201365, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.267762, -0.161698  // 60-80%(=60-70%) and Min.bias
};
double P1ERR_Asym1Cut_counts[CentClassesPi0] = { 
          0.162365, 0.162365, // Centrality [0-10[ % 
          0.147010, 0.147010, // Centrality [10-20[ % 
          0.118835, 0.118835, // Centrality [20-30[ % 
          0.114394, 0.114394, // Centrality [30-40[ % 
          0.110663, 0.110663, // Centrality [40-50[ % 
          0.126370, 0.126370, // Centrality [50-60[ % 
          0.104037, 0.104037, // Centrality [60-70[ % 
          0.103129, 0.103129, // Centrality [70-80[ % 
          0.093597, 0.103129, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.104037, 0.061150  // 60-80%(=60-70%) and Min.bias
};
double P2_Asym1Cut_counts[CentClassesPi0] = { 
          0.002153, 0.002153, // Centrality [0-10[ % 
          -0.000611, -0.000611, // Centrality [10-20[ % 
          0.001611, 0.001611, // Centrality [20-30[ % 
          0.007062, 0.007062, // Centrality [30-40[ % 
          0.006424, 0.006424, // Centrality [40-50[ % 
          0.006089, 0.006089, // Centrality [50-60[ % 
          0.007098, 0.007098, // Centrality [60-70[ % 
          0.005282, 0.005282, // Centrality [70-80[ % 
          0.004433, 0.005282, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.007098, 0.003979  // 60-80%(=60-70%) and Min.bias
};
double P2ERR_Asym1Cut_counts[CentClassesPi0] = { 
          0.005108, 0.005108, // Centrality [0-10[ % 
          0.004790, 0.004790, // Centrality [10-20[ % 
          0.003767, 0.003767, // Centrality [20-30[ % 
          0.003672, 0.003672, // Centrality [30-40[ % 
          0.003570, 0.003570, // Centrality [40-50[ % 
          0.004234, 0.004234, // Centrality [50-60[ % 
          0.003303, 0.003303, // Centrality [60-70[ % 
          0.003279, 0.003279, // Centrality [70-80[ % 
          0.002974, 0.003279, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.003303, 0.002105  // 60-80%(=60-70%) and Min.bias
};
double P3_Asym1Cut_counts[CentClassesPi0] = { 
          0.116687, 0.116687, // Centrality [0-10[ % 
          0.061176, 0.061176, // Centrality [10-20[ % 
          0.077263, 0.077263, // Centrality [20-30[ % 
          0.240737, 0.240737, // Centrality [30-40[ % 
          0.214194, 0.214194, // Centrality [40-50[ % 
          0.190478, 0.190478, // Centrality [50-60[ % 
          0.173108, 0.173108, // Centrality [60-70[ % 
          0.163469, 0.163469, // Centrality [70-80[ % 
          0.130550, 0.163469, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.173108, 0.158732  // 60-80%(=60-70%) and Min.bias
};
double P3ERR_Asym1Cut_counts[CentClassesPi0] = { 
          0.130654, 0.130654, // Centrality [0-10[ % 
          0.120240, 0.120240, // Centrality [10-20[ % 
          0.093322, 0.093322, // Centrality [20-30[ % 
          0.091057, 0.091057, // Centrality [30-40[ % 
          0.087842, 0.087842, // Centrality [40-50[ % 
          0.102094, 0.102094, // Centrality [50-60[ % 
          0.081513, 0.081513, // Centrality [60-70[ % 
          0.081129, 0.081129, // Centrality [70-80[ % 
          0.073450, 0.081129, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.081513, 0.050415  // 60-80%(=60-70%) and Min.bias
};
double P4_Asym1Cut_counts[CentClassesPi0] = { 
          -0.273598, -0.273598, // Centrality [0-10[ % 
          -0.111354, -0.111354, // Centrality [10-20[ % 
          -0.002871, -0.002871, // Centrality [20-30[ % 
          0.197864, 0.197864, // Centrality [30-40[ % 
          0.179468, 0.179468, // Centrality [40-50[ % 
          0.205806, 0.205806, // Centrality [50-60[ % 
          0.258479, 0.258479, // Centrality [60-70[ % 
          0.183444, 0.183444, // Centrality [70-80[ % 
          0.165196, 0.183444, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.258479, 0.138169  // 60-80%(=60-70%) and Min.bias
};
double P4ERR_Asym1Cut_counts[CentClassesPi0] = { 
          0.130267, 0.130267, // Centrality [0-10[ % 
          0.113932, 0.113932, // Centrality [10-20[ % 
          0.096156, 0.096156, // Centrality [20-30[ % 
          0.091326, 0.091326, // Centrality [30-40[ % 
          0.088038, 0.088038, // Centrality [40-50[ % 
          0.097123, 0.097123, // Centrality [50-60[ % 
          0.084150, 0.084150, // Centrality [60-70[ % 
          0.083163, 0.083163, // Centrality [70-80[ % 
          0.075605, 0.083163, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.084150, 0.045600  // 60-80%(=60-70%) and Min.bias
};
double P0_BasicCuts5x5Asym1Cut_counts[CentClassesPi0] = { 
          0.316592, 0.316592, // Centrality [0-10[ % 
          0.326201, 0.326201, // Centrality [10-20[ % 
          0.327645, 0.327645, // Centrality [20-30[ % 
          0.394095, 0.394095, // Centrality [30-40[ % 
          0.344027, 0.344027, // Centrality [40-50[ % 
          0.271351, 0.271351, // Centrality [50-60[ % 
          0.365037, 0.365037, // Centrality [60-70[ % 
          0.303750, 0.303750, // Centrality [70-80[ % 
          0.289732, 0.303750, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.365037, 0.335634  // 60-80%(=60-70%) and Min.bias
};
double P0ERR_BasicCuts5x5Asym1Cut_counts[CentClassesPi0] = { 
          0.089440, 0.089440, // Centrality [0-10[ % 
          0.083072, 0.083072, // Centrality [10-20[ % 
          0.067480, 0.067480, // Centrality [20-30[ % 
          0.064951, 0.064951, // Centrality [30-40[ % 
          0.065079, 0.065079, // Centrality [40-50[ % 
          0.061062, 0.061062, // Centrality [50-60[ % 
          0.060592, 0.060592, // Centrality [60-70[ % 
          0.059357, 0.059357, // Centrality [70-80[ % 
          0.054407, 0.059357, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.060592, 0.025615  // 60-80%(=60-70%) and Min.bias
};
double P1_BasicCuts5x5Asym1Cut_counts[CentClassesPi0] = { 
          0.160037, 0.160037, // Centrality [0-10[ % 
          0.066205, 0.066205, // Centrality [10-20[ % 
          0.050860, 0.050860, // Centrality [20-30[ % 
          -0.063366, -0.063366, // Centrality [30-40[ % 
          -0.024054, -0.024054, // Centrality [40-50[ % 
          0.060284, 0.060284, // Centrality [50-60[ % 
          -0.035189, -0.035189, // Centrality [60-70[ % 
          0.012057, 0.012057, // Centrality [70-80[ % 
          0.038536, 0.012057, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.035189, 0.004357  // 60-80%(=60-70%) and Min.bias
};
double P1ERR_BasicCuts5x5Asym1Cut_counts[CentClassesPi0] = { 
          0.096212, 0.096212, // Centrality [0-10[ % 
          0.088977, 0.088977, // Centrality [10-20[ % 
          0.072104, 0.072104, // Centrality [20-30[ % 
          0.069405, 0.069405, // Centrality [30-40[ % 
          0.069448, 0.069448, // Centrality [40-50[ % 
          0.065190, 0.065190, // Centrality [50-60[ % 
          0.064433, 0.064433, // Centrality [60-70[ % 
          0.063260, 0.063260, // Centrality [70-80[ % 
          0.057892, 0.063260, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.064433, 0.027337  // 60-80%(=60-70%) and Min.bias
};
double P2_BasicCuts5x5Asym1Cut_counts[CentClassesPi0] = { 
          -0.002520, -0.002520, // Centrality [0-10[ % 
          -0.000907, -0.000907, // Centrality [10-20[ % 
          -0.001555, -0.001555, // Centrality [20-30[ % 
          0.001727, 0.001727, // Centrality [30-40[ % 
          0.000654, 0.000654, // Centrality [40-50[ % 
          -0.002162, -0.002162, // Centrality [50-60[ % 
          0.000395, 0.000395, // Centrality [60-70[ % 
          -0.000982, -0.000982, // Centrality [70-80[ % 
          -0.001694, -0.000982, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.000395, -0.000312  // 60-80%(=60-70%) and Min.bias
};
double P2ERR_BasicCuts5x5Asym1Cut_counts[CentClassesPi0] = { 
          0.003061, 0.003061, // Centrality [0-10[ % 
          0.002903, 0.002903, // Centrality [10-20[ % 
          0.002299, 0.002299, // Centrality [20-30[ % 
          0.002239, 0.002239, // Centrality [30-40[ % 
          0.002274, 0.002274, // Centrality [40-50[ % 
          0.002097, 0.002097, // Centrality [50-60[ % 
          0.002067, 0.002067, // Centrality [60-70[ % 
          0.002033, 0.002033, // Centrality [70-80[ % 
          0.001858, 0.002033, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.002067, 0.000888  // 60-80%(=60-70%) and Min.bias
};
double P3_BasicCuts5x5Asym1Cut_counts[CentClassesPi0] = { 
          0.068181, 0.068181, // Centrality [0-10[ % 
          0.102879, 0.102879, // Centrality [10-20[ % 
          0.053213, 0.053213, // Centrality [20-30[ % 
          0.152805, 0.152805, // Centrality [30-40[ % 
          0.113615, 0.113615, // Centrality [40-50[ % 
          0.024752, 0.024752, // Centrality [50-60[ % 
          0.077718, 0.077718, // Centrality [60-70[ % 
          0.058410, 0.058410, // Centrality [70-80[ % 
          0.028979, 0.058410, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.077718, 0.086803  // 60-80%(=60-70%) and Min.bias
};
double P3ERR_BasicCuts5x5Asym1Cut_counts[CentClassesPi0] = { 
          0.077956, 0.077956, // Centrality [0-10[ % 
          0.073395, 0.073395, // Centrality [10-20[ % 
          0.057479, 0.057479, // Centrality [20-30[ % 
          0.056099, 0.056099, // Centrality [30-40[ % 
          0.056162, 0.056162, // Centrality [40-50[ % 
          0.052247, 0.052247, // Centrality [50-60[ % 
          0.051346, 0.051346, // Centrality [60-70[ % 
          0.050557, 0.050557, // Centrality [70-80[ % 
          0.046122, 0.050557, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.051346, 0.022182  // 60-80%(=60-70%) and Min.bias
};
double P4_BasicCuts5x5Asym1Cut_counts[CentClassesPi0] = { 
          -0.243060, -0.243060, // Centrality [0-10[ % 
          -0.115632, -0.115632, // Centrality [10-20[ % 
          -0.066914, -0.066914, // Centrality [20-30[ % 
          0.031192, 0.031192, // Centrality [30-40[ % 
          0.004527, 0.004527, // Centrality [40-50[ % 
          -0.050681, -0.050681, // Centrality [50-60[ % 
          0.039416, 0.039416, // Centrality [60-70[ % 
          -0.006971, -0.006971, // Centrality [70-80[ % 
          -0.025532, -0.006971, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.039416, -0.016924  // 60-80%(=60-70%) and Min.bias
};
double P4ERR_BasicCuts5x5Asym1Cut_counts[CentClassesPi0] = { 
          0.076474, 0.076474, // Centrality [0-10[ % 
          0.068613, 0.068613, // Centrality [10-20[ % 
          0.057638, 0.057638, // Centrality [20-30[ % 
          0.054675, 0.054675, // Centrality [30-40[ % 
          0.054131, 0.054131, // Centrality [40-50[ % 
          0.051645, 0.051645, // Centrality [50-60[ % 
          0.051295, 0.051295, // Centrality [60-70[ % 
          0.050232, 0.050232, // Centrality [70-80[ % 
          0.046088, 0.050232, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.051295, 0.021367  // 60-80%(=60-70%) and Min.bias
};
double P0_Asym2Cut_counts[CentClassesPi0] = { 
          0.469467, 0.469467, // Centrality [0-10[ % 
          0.332207, 0.332207, // Centrality [10-20[ % 
          0.313177, 0.313177, // Centrality [20-30[ % 
          0.318231, 0.318231, // Centrality [30-40[ % 
          0.323782, 0.323782, // Centrality [40-50[ % 
          0.351306, 0.351306, // Centrality [50-60[ % 
          0.265425, 0.265425, // Centrality [60-70[ % 
          0.306138, 0.306138, // Centrality [70-80[ % 
          0.309106, 0.306138, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.265425, 0.338360  // 60-80%(=60-70%) and Min.bias
};
double P0ERR_Asym2Cut_counts[CentClassesPi0] = { 
          0.073103, 0.073103, // Centrality [0-10[ % 
          0.059241, 0.059241, // Centrality [10-20[ % 
          0.055124, 0.055124, // Centrality [20-30[ % 
          0.055878, 0.055878, // Centrality [30-40[ % 
          0.054415, 0.054415, // Centrality [40-50[ % 
          0.050257, 0.050257, // Centrality [50-60[ % 
          0.048926, 0.048926, // Centrality [60-70[ % 
          0.048904, 0.048904, // Centrality [70-80[ % 
          0.044306, 0.048904, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.048926, 0.019055  // 60-80%(=60-70%) and Min.bias
};
double P1_Asym2Cut_counts[CentClassesPi0] = { 
          -0.165327, -0.165327, // Centrality [0-10[ % 
          -0.077838, -0.077838, // Centrality [10-20[ % 
          -0.076725, -0.076725, // Centrality [20-30[ % 
          -0.108940, -0.108940, // Centrality [30-40[ % 
          -0.117387, -0.117387, // Centrality [40-50[ % 
          -0.140571, -0.140571, // Centrality [50-60[ % 
          -0.056460, -0.056460, // Centrality [60-70[ % 
          -0.104398, -0.104398, // Centrality [70-80[ % 
          -0.103461, -0.104398, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.056460, -0.122257  // 60-80%(=60-70%) and Min.bias
};
double P1ERR_Asym2Cut_counts[CentClassesPi0] = { 
          0.077649, 0.077649, // Centrality [0-10[ % 
          0.062876, 0.062876, // Centrality [10-20[ % 
          0.058328, 0.058328, // Centrality [20-30[ % 
          0.059290, 0.059290, // Centrality [30-40[ % 
          0.057596, 0.057596, // Centrality [40-50[ % 
          0.052981, 0.052981, // Centrality [50-60[ % 
          0.051616, 0.051616, // Centrality [60-70[ % 
          0.051491, 0.051491, // Centrality [70-80[ % 
          0.046654, 0.051491, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.051616, 0.020126  // 60-80%(=60-70%) and Min.bias
};
double P2_Asym2Cut_counts[CentClassesPi0] = { 
          0.005716, 0.005716, // Centrality [0-10[ % 
          0.002615, 0.002615, // Centrality [10-20[ % 
          0.002354, 0.002354, // Centrality [20-30[ % 
          0.003168, 0.003168, // Centrality [30-40[ % 
          0.003290, 0.003290, // Centrality [40-50[ % 
          0.003852, 0.003852, // Centrality [50-60[ % 
          0.001116, 0.001116, // Centrality [60-70[ % 
          0.002842, 0.002842, // Centrality [70-80[ % 
          0.002748, 0.002842, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.001116, 0.003443  // 60-80%(=60-70%) and Min.bias
};
double P2ERR_Asym2Cut_counts[CentClassesPi0] = { 
          0.002445, 0.002445, // Centrality [0-10[ % 
          0.001994, 0.001994, // Centrality [10-20[ % 
          0.001841, 0.001841, // Centrality [20-30[ % 
          0.001918, 0.001918, // Centrality [30-40[ % 
          0.001875, 0.001875, // Centrality [40-50[ % 
          0.001677, 0.001677, // Centrality [50-60[ % 
          0.001630, 0.001630, // Centrality [60-70[ % 
          0.001629, 0.001629, // Centrality [70-80[ % 
          0.001474, 0.001629, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.001630, 0.000641  // 60-80%(=60-70%) and Min.bias
};
double P3_Asym2Cut_counts[CentClassesPi0] = { 
          0.103381, 0.103381, // Centrality [0-10[ % 
          0.052467, 0.052467, // Centrality [10-20[ % 
          0.038590, 0.038590, // Centrality [20-30[ % 
          0.070467, 0.070467, // Centrality [30-40[ % 
          0.075356, 0.075356, // Centrality [40-50[ % 
          0.065916, 0.065916, // Centrality [50-60[ % 
          0.008452, 0.008452, // Centrality [60-70[ % 
          0.053389, 0.053389, // Centrality [70-80[ % 
          0.049505, 0.053389, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.008452, 0.070484  // 60-80%(=60-70%) and Min.bias
};
double P3ERR_Asym2Cut_counts[CentClassesPi0] = { 
          0.061934, 0.061934, // Centrality [0-10[ % 
          0.049659, 0.049659, // Centrality [10-20[ % 
          0.045539, 0.045539, // Centrality [20-30[ % 
          0.047244, 0.047244, // Centrality [30-40[ % 
          0.045771, 0.045771, // Centrality [40-50[ % 
          0.041254, 0.041254, // Centrality [50-60[ % 
          0.040101, 0.040101, // Centrality [60-70[ % 
          0.039975, 0.039975, // Centrality [70-80[ % 
          0.036179, 0.039975, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.040101, 0.015884  // 60-80%(=60-70%) and Min.bias
};
double P4_Asym2Cut_counts[CentClassesPi0] = { 
          0.117455, 0.117455, // Centrality [0-10[ % 
          0.056610, 0.056610, // Centrality [10-20[ % 
          0.066124, 0.066124, // Centrality [20-30[ % 
          0.095493, 0.095493, // Centrality [30-40[ % 
          0.104582, 0.104582, // Centrality [40-50[ % 
          0.138305, 0.138305, // Centrality [50-60[ % 
          0.070088, 0.070088, // Centrality [60-70[ % 
          0.101574, 0.101574, // Centrality [70-80[ % 
          0.102443, 0.101574, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.070088, 0.111851  // 60-80%(=60-70%) and Min.bias
};
double P4ERR_Asym2Cut_counts[CentClassesPi0] = { 
          0.062704, 0.062704, // Centrality [0-10[ % 
          0.050702, 0.050702, // Centrality [10-20[ % 
          0.047456, 0.047456, // Centrality [20-30[ % 
          0.046948, 0.046948, // Centrality [30-40[ % 
          0.045556, 0.045556, // Centrality [40-50[ % 
          0.043068, 0.043068, // Centrality [50-60[ % 
          0.042063, 0.042063, // Centrality [60-70[ % 
          0.041937, 0.041937, // Centrality [70-80[ % 
          0.038050, 0.041937, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.042063, 0.016211  // 60-80%(=60-70%) and Min.bias
};
double P0_BasicCuts5x5Asym2Cut_counts[CentClassesPi0] = { 
          0.222821, 0.222821, // Centrality [0-10[ % 
          0.132821, 0.132821, // Centrality [10-20[ % 
          0.143395, 0.143395, // Centrality [20-30[ % 
          0.141510, 0.141510, // Centrality [30-40[ % 
          0.142630, 0.142630, // Centrality [40-50[ % 
          0.145316, 0.145316, // Centrality [50-60[ % 
          0.101295, 0.101295, // Centrality [60-70[ % 
          0.130301, 0.130301, // Centrality [70-80[ % 
          0.113204, 0.130301, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.101295, 0.145080  // 60-80%(=60-70%) and Min.bias
};
double P0ERR_BasicCuts5x5Asym2Cut_counts[CentClassesPi0] = { 
          0.045229, 0.045229, // Centrality [0-10[ % 
          0.037633, 0.037633, // Centrality [10-20[ % 
          0.034369, 0.034369, // Centrality [20-30[ % 
          0.033049, 0.033049, // Centrality [30-40[ % 
          0.034556, 0.034556, // Centrality [40-50[ % 
          0.031303, 0.031303, // Centrality [50-60[ % 
          0.030832, 0.030832, // Centrality [60-70[ % 
          0.030261, 0.030261, // Centrality [70-80[ % 
          0.027827, 0.030261, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.030832, 0.011588  // 60-80%(=60-70%) and Min.bias
};
double P1_BasicCuts5x5Asym2Cut_counts[CentClassesPi0] = { 
          -0.088623, -0.088623, // Centrality [0-10[ % 
          -0.017265, -0.017265, // Centrality [10-20[ % 
          -0.038670, -0.038670, // Centrality [20-30[ % 
          -0.049315, -0.049315, // Centrality [30-40[ % 
          -0.052161, -0.052161, // Centrality [40-50[ % 
          -0.050696, -0.050696, // Centrality [50-60[ % 
          -0.006874, -0.006874, // Centrality [60-70[ % 
          -0.043064, -0.043064, // Centrality [70-80[ % 
          -0.020425, -0.043064, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.006874, -0.049162  // 60-80%(=60-70%) and Min.bias
};
double P1ERR_BasicCuts5x5Asym2Cut_counts[CentClassesPi0] = { 
          0.048186, 0.048186, // Centrality [0-10[ % 
          0.040073, 0.040073, // Centrality [10-20[ % 
          0.036530, 0.036530, // Centrality [20-30[ % 
          0.035224, 0.035224, // Centrality [30-40[ % 
          0.036678, 0.036678, // Centrality [40-50[ % 
          0.033138, 0.033138, // Centrality [50-60[ % 
          0.032661, 0.032661, // Centrality [60-70[ % 
          0.032008, 0.032008, // Centrality [70-80[ % 
          0.029422, 0.032008, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.032661, 0.012286  // 60-80%(=60-70%) and Min.bias
};
double P2_BasicCuts5x5Asym2Cut_counts[CentClassesPi0] = { 
          0.002868, 0.002868, // Centrality [0-10[ % 
          0.000644, 0.000644, // Centrality [10-20[ % 
          0.001083, 0.001083, // Centrality [20-30[ % 
          0.001311, 0.001311, // Centrality [30-40[ % 
          0.001443, 0.001443, // Centrality [40-50[ % 
          0.001238, 0.001238, // Centrality [50-60[ % 
          -0.000149, -0.000149, // Centrality [60-70[ % 
          0.001073, 0.001073, // Centrality [70-80[ % 
          0.000399, 0.001073, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.000149, 0.001279  // 60-80%(=60-70%) and Min.bias
};
double P2ERR_BasicCuts5x5Asym2Cut_counts[CentClassesPi0] = { 
          0.001536, 0.001536, // Centrality [0-10[ % 
          0.001279, 0.001279, // Centrality [10-20[ % 
          0.001160, 0.001160, // Centrality [20-30[ % 
          0.001135, 0.001135, // Centrality [30-40[ % 
          0.001208, 0.001208, // Centrality [40-50[ % 
          0.001058, 0.001058, // Centrality [50-60[ % 
          0.001042, 0.001042, // Centrality [60-70[ % 
          0.001023, 0.001023, // Centrality [70-80[ % 
          0.000938, 0.001023, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.001042, 0.000396  // 60-80%(=60-70%) and Min.bias
};
double P3_BasicCuts5x5Asym2Cut_counts[CentClassesPi0] = { 
          0.075574, 0.075574, // Centrality [0-10[ % 
          0.029779, 0.029779, // Centrality [10-20[ % 
          0.036104, 0.036104, // Centrality [20-30[ % 
          0.045855, 0.045855, // Centrality [30-40[ % 
          0.048668, 0.048668, // Centrality [40-50[ % 
          0.031081, 0.031081, // Centrality [50-60[ % 
          0.002677, 0.002677, // Centrality [60-70[ % 
          0.034945, 0.034945, // Centrality [70-80[ % 
          0.014753, 0.034945, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.002677, 0.040636  // 60-80%(=60-70%) and Min.bias
};
double P3ERR_BasicCuts5x5Asym2Cut_counts[CentClassesPi0] = { 
          0.038885, 0.038885, // Centrality [0-10[ % 
          0.032028, 0.032028, // Centrality [10-20[ % 
          0.028918, 0.028918, // Centrality [20-30[ % 
          0.028233, 0.028233, // Centrality [30-40[ % 
          0.029508, 0.029508, // Centrality [40-50[ % 
          0.026133, 0.026133, // Centrality [50-60[ % 
          0.025750, 0.025750, // Centrality [60-70[ % 
          0.025237, 0.025237, // Centrality [70-80[ % 
          0.023138, 0.025237, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.025750, 0.009855  // 60-80%(=60-70%) and Min.bias
};
double P4_BasicCuts5x5Asym2Cut_counts[CentClassesPi0] = { 
          0.063563, 0.063563, // Centrality [0-10[ % 
          0.006394, 0.006394, // Centrality [10-20[ % 
          0.031719, 0.031719, // Centrality [20-30[ % 
          0.043122, 0.043122, // Centrality [30-40[ % 
          0.044435, 0.044435, // Centrality [40-50[ % 
          0.053006, 0.053006, // Centrality [50-60[ % 
          0.016195, 0.016195, // Centrality [60-70[ % 
          0.042363, 0.042363, // Centrality [70-80[ % 
          0.024471, 0.042363, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.016195, 0.045472  // 60-80%(=60-70%) and Min.bias
};
double P4ERR_BasicCuts5x5Asym2Cut_counts[CentClassesPi0] = { 
          0.038372, 0.038372, // Centrality [0-10[ % 
          0.031969, 0.031969, // Centrality [10-20[ % 
          0.029381, 0.029381, // Centrality [20-30[ % 
          0.027874, 0.027874, // Centrality [30-40[ % 
          0.028580, 0.028580, // Centrality [40-50[ % 
          0.026613, 0.026613, // Centrality [50-60[ % 
          0.026252, 0.026252, // Centrality [60-70[ % 
          0.025695, 0.025695, // Centrality [70-80[ % 
          0.023681, 0.025695, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.026252, 0.009729  // 60-80%(=60-70%) and Min.bias
};
double P0_ToF1Cut_counts[CentClassesPi0] = { 
          0.944070, 0.944070, // Centrality [0-10[ % 
          0.788948, 0.788948, // Centrality [10-20[ % 
          0.908222, 0.908222, // Centrality [20-30[ % 
          1.010130, 1.010130, // Centrality [30-40[ % 
          0.935422, 0.935422, // Centrality [40-50[ % 
          0.942037, 0.942037, // Centrality [50-60[ % 
          1.018395, 1.018395, // Centrality [60-70[ % 
          0.924029, 0.924029, // Centrality [70-80[ % 
          0.910931, 0.924029, // Centrality [80-93[ %  and 60-92% (=70-80%)
          1.018395, 0.945249  // 60-80%(=60-70%) and Min.bias
};
double P0ERR_ToF1Cut_counts[CentClassesPi0] = { 
          0.147586, 0.147586, // Centrality [0-10[ % 
          0.139306, 0.139306, // Centrality [10-20[ % 
          0.114932, 0.114932, // Centrality [20-30[ % 
          0.108657, 0.108657, // Centrality [30-40[ % 
          0.107063, 0.107063, // Centrality [40-50[ % 
          0.122493, 0.122493, // Centrality [50-60[ % 
          0.101849, 0.101849, // Centrality [60-70[ % 
          0.100437, 0.100437, // Centrality [70-80[ % 
          0.091336, 0.100437, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.101849, 0.057855  // 60-80%(=60-70%) and Min.bias
};
double P1_ToF1Cut_counts[CentClassesPi0] = { 
          0.022094, 0.022094, // Centrality [0-10[ % 
          0.045269, 0.045269, // Centrality [10-20[ % 
          -0.108992, -0.108992, // Centrality [20-30[ % 
          -0.288288, -0.288288, // Centrality [30-40[ % 
          -0.225890, -0.225890, // Centrality [40-50[ % 
          -0.220866, -0.220866, // Centrality [50-60[ % 
          -0.296326, -0.296326, // Centrality [60-70[ % 
          -0.216609, -0.216609, // Centrality [70-80[ % 
          -0.197326, -0.216609, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.296326, -0.208748  // 60-80%(=60-70%) and Min.bias
};
double P1ERR_ToF1Cut_counts[CentClassesPi0] = { 
          0.158624, 0.158624, // Centrality [0-10[ % 
          0.149059, 0.149059, // Centrality [10-20[ % 
          0.122812, 0.122812, // Centrality [20-30[ % 
          0.115873, 0.115873, // Centrality [30-40[ % 
          0.114150, 0.114150, // Centrality [40-50[ % 
          0.130614, 0.130614, // Centrality [50-60[ % 
          0.108232, 0.108232, // Centrality [60-70[ % 
          0.106961, 0.106961, // Centrality [70-80[ % 
          0.097170, 0.106961, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.108232, 0.061680  // 60-80%(=60-70%) and Min.bias
};
double P2_ToF1Cut_counts[CentClassesPi0] = { 
          0.004382, 0.004382, // Centrality [0-10[ % 
          -0.000171, -0.000171, // Centrality [10-20[ % 
          0.003793, 0.003793, // Centrality [20-30[ % 
          0.008451, 0.008451, // Centrality [30-40[ % 
          0.006735, 0.006735, // Centrality [40-50[ % 
          0.005724, 0.005724, // Centrality [50-60[ % 
          0.007967, 0.007967, // Centrality [60-70[ % 
          0.005644, 0.005644, // Centrality [70-80[ % 
          0.005026, 0.005644, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.007967, 0.005492  // 60-80%(=60-70%) and Min.bias
};
double P2ERR_ToF1Cut_counts[CentClassesPi0] = { 
          0.005038, 0.005038, // Centrality [0-10[ % 
          0.004894, 0.004894, // Centrality [10-20[ % 
          0.003929, 0.003929, // Centrality [20-30[ % 
          0.003740, 0.003740, // Centrality [30-40[ % 
          0.003711, 0.003711, // Centrality [40-50[ % 
          0.004412, 0.004412, // Centrality [50-60[ % 
          0.003469, 0.003469, // Centrality [60-70[ % 
          0.003429, 0.003429, // Centrality [70-80[ % 
          0.003112, 0.003429, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.003469, 0.002147  // 60-80%(=60-70%) and Min.bias
};
double P3_ToF1Cut_counts[CentClassesPi0] = { 
          0.407294, 0.407294, // Centrality [0-10[ % 
          0.262251, 0.262251, // Centrality [10-20[ % 
          0.326807, 0.326807, // Centrality [20-30[ % 
          0.438389, 0.438389, // Centrality [30-40[ % 
          0.383681, 0.383681, // Centrality [40-50[ % 
          0.346867, 0.346867, // Centrality [50-60[ % 
          0.353656, 0.353656, // Centrality [60-70[ % 
          0.328357, 0.328357, // Centrality [70-80[ % 
          0.299908, 0.328357, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.353656, 0.366661  // 60-80%(=60-70%) and Min.bias
};
double P3ERR_ToF1Cut_counts[CentClassesPi0] = { 
          0.128441, 0.128441, // Centrality [0-10[ % 
          0.123246, 0.123246, // Centrality [10-20[ % 
          0.098275, 0.098275, // Centrality [20-30[ % 
          0.093248, 0.093248, // Centrality [30-40[ % 
          0.091990, 0.091990, // Centrality [40-50[ % 
          0.106839, 0.106839, // Centrality [50-60[ % 
          0.086056, 0.086056, // Centrality [60-70[ % 
          0.085303, 0.085303, // Centrality [70-80[ % 
          0.077307, 0.085303, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.086056, 0.051416  // 60-80%(=60-70%) and Min.bias
};
double P4_ToF1Cut_counts[CentClassesPi0] = { 
          -0.263353, -0.263353, // Centrality [0-10[ % 
          -0.134432, -0.134432, // Centrality [10-20[ % 
          0.004460, 0.004460, // Centrality [20-30[ % 
          0.187708, 0.187708, // Centrality [30-40[ % 
          0.136789, 0.136789, // Centrality [40-50[ % 
          0.153083, 0.153083, // Centrality [50-60[ % 
          0.242211, 0.242211, // Centrality [60-70[ % 
          0.158499, 0.158499, // Centrality [70-80[ % 
          0.150957, 0.158499, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.242211, 0.131742  // 60-80%(=60-70%) and Min.bias
};
double P4ERR_ToF1Cut_counts[CentClassesPi0] = { 
          0.126169, 0.126169, // Centrality [0-10[ % 
          0.114173, 0.114173, // Centrality [10-20[ % 
          0.097777, 0.097777, // Centrality [20-30[ % 
          0.091646, 0.091646, // Centrality [30-40[ % 
          0.089594, 0.089594, // Centrality [40-50[ % 
          0.099109, 0.099109, // Centrality [50-60[ % 
          0.086319, 0.086319, // Centrality [60-70[ % 
          0.085173, 0.085173, // Centrality [70-80[ % 
          0.077516, 0.085173, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.086319, 0.045379  // 60-80%(=60-70%) and Min.bias
};
double P0_BasicCuts5x5Asym1ToF1Cut_counts[CentClassesPi0] = { 
          0.289531, 0.289531, // Centrality [0-10[ % 
          0.340854, 0.340854, // Centrality [10-20[ % 
          0.313826, 0.313826, // Centrality [20-30[ % 
          0.398447, 0.398447, // Centrality [30-40[ % 
          0.334188, 0.334188, // Centrality [40-50[ % 
          0.280823, 0.280823, // Centrality [50-60[ % 
          0.351536, 0.351536, // Centrality [60-70[ % 
          0.300553, 0.300553, // Centrality [70-80[ % 
          0.270125, 0.300553, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.351536, 0.327506  // 60-80%(=60-70%) and Min.bias
};
double P0ERR_BasicCuts5x5Asym1ToF1Cut_counts[CentClassesPi0] = { 
          0.086148, 0.086148, // Centrality [0-10[ % 
          0.080873, 0.080873, // Centrality [10-20[ % 
          0.065966, 0.065966, // Centrality [20-30[ % 
          0.063775, 0.063775, // Centrality [30-40[ % 
          0.064077, 0.064077, // Centrality [40-50[ % 
          0.059997, 0.059997, // Centrality [50-60[ % 
          0.059606, 0.059606, // Centrality [60-70[ % 
          0.058482, 0.058482, // Centrality [70-80[ % 
          0.053661, 0.058482, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.059606, 0.025053  // 60-80%(=60-70%) and Min.bias
};
double P1_BasicCuts5x5Asym1ToF1Cut_counts[CentClassesPi0] = { 
          0.136298, 0.136298, // Centrality [0-10[ % 
          0.017084, 0.017084, // Centrality [10-20[ % 
          0.038826, 0.038826, // Centrality [20-30[ % 
          -0.086472, -0.086472, // Centrality [30-40[ % 
          -0.028223, -0.028223, // Centrality [40-50[ % 
          0.036958, 0.036958, // Centrality [50-60[ % 
          -0.036386, -0.036386, // Centrality [60-70[ % 
          0.003479, 0.003479, // Centrality [70-80[ % 
          0.046282, 0.003479, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.036386, -0.006769  // 60-80%(=60-70%) and Min.bias
};
double P1ERR_BasicCuts5x5Asym1ToF1Cut_counts[CentClassesPi0] = { 
          0.092827, 0.092827, // Centrality [0-10[ % 
          0.086681, 0.086681, // Centrality [10-20[ % 
          0.070613, 0.070613, // Centrality [20-30[ % 
          0.068202, 0.068202, // Centrality [30-40[ % 
          0.068450, 0.068450, // Centrality [40-50[ % 
          0.064111, 0.064111, // Centrality [50-60[ % 
          0.063489, 0.063489, // Centrality [60-70[ % 
          0.062405, 0.062405, // Centrality [70-80[ % 
          0.057185, 0.062405, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.063489, 0.026774  // 60-80%(=60-70%) and Min.bias
};
double P2_BasicCuts5x5Asym1ToF1Cut_counts[CentClassesPi0] = { 
          -0.001663, -0.001663, // Centrality [0-10[ % 
          0.000767, 0.000767, // Centrality [10-20[ % 
          -0.000941, -0.000941, // Centrality [20-30[ % 
          0.002715, 0.002715, // Centrality [30-40[ % 
          0.000996, 0.000996, // Centrality [40-50[ % 
          -0.001246, -0.001246, // Centrality [50-60[ % 
          0.000740, 0.000740, // Centrality [60-70[ % 
          -0.000483, -0.000483, // Centrality [70-80[ % 
          -0.001671, -0.000483, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.000740, 0.000271  // 60-80%(=60-70%) and Min.bias
};
double P2ERR_BasicCuts5x5Asym1ToF1Cut_counts[CentClassesPi0] = { 
          0.002963, 0.002963, // Centrality [0-10[ % 
          0.002836, 0.002836, // Centrality [10-20[ % 
          0.002256, 0.002256, // Centrality [20-30[ % 
          0.002204, 0.002204, // Centrality [30-40[ % 
          0.002243, 0.002243, // Centrality [40-50[ % 
          0.002063, 0.002063, // Centrality [50-60[ % 
          0.002039, 0.002039, // Centrality [60-70[ % 
          0.002007, 0.002007, // Centrality [70-80[ % 
          0.001837, 0.002007, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.002039, 0.000871  // 60-80%(=60-70%) and Min.bias
};
double P3_BasicCuts5x5Asym1ToF1Cut_counts[CentClassesPi0] = { 
          0.129531, 0.129531, // Centrality [0-10[ % 
          0.171282, 0.171282, // Centrality [10-20[ % 
          0.094713, 0.094713, // Centrality [20-30[ % 
          0.194259, 0.194259, // Centrality [30-40[ % 
          0.137424, 0.137424, // Centrality [40-50[ % 
          0.063158, 0.063158, // Centrality [50-60[ % 
          0.104416, 0.104416, // Centrality [60-70[ % 
          0.084830, 0.084830, // Centrality [70-80[ % 
          0.046294, 0.084830, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.104416, 0.120661  // 60-80%(=60-70%) and Min.bias
};
double P3ERR_BasicCuts5x5Asym1ToF1Cut_counts[CentClassesPi0] = { 
          0.075530, 0.075530, // Centrality [0-10[ % 
          0.071829, 0.071829, // Centrality [10-20[ % 
          0.056540, 0.056540, // Centrality [20-30[ % 
          0.055288, 0.055288, // Centrality [30-40[ % 
          0.055477, 0.055477, // Centrality [40-50[ % 
          0.051473, 0.051473, // Centrality [50-60[ % 
          0.050738, 0.050738, // Centrality [60-70[ % 
          0.049990, 0.049990, // Centrality [70-80[ % 
          0.045680, 0.049990, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.050738, 0.021806  // 60-80%(=60-70%) and Min.bias
};
double P4_BasicCuts5x5Asym1ToF1Cut_counts[CentClassesPi0] = { 
          -0.238759, -0.238759, // Centrality [0-10[ % 
          -0.089134, -0.089134, // Centrality [10-20[ % 
          -0.073348, -0.073348, // Centrality [20-30[ % 
          0.035663, 0.035663, // Centrality [30-40[ % 
          -0.004774, -0.004774, // Centrality [40-50[ % 
          -0.044732, -0.044732, // Centrality [50-60[ % 
          0.024481, 0.024481, // Centrality [60-70[ % 
          -0.012856, -0.012856, // Centrality [70-80[ % 
          -0.046355, -0.012856, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.024481, -0.022077  // 60-80%(=60-70%) and Min.bias
};
double P4ERR_BasicCuts5x5Asym1ToF1Cut_counts[CentClassesPi0] = { 
          0.073431, 0.073431, // Centrality [0-10[ % 
          0.066522, 0.066522, // Centrality [10-20[ % 
          0.056238, 0.056238, // Centrality [20-30[ % 
          0.053579, 0.053579, // Centrality [30-40[ % 
          0.053256, 0.053256, // Centrality [40-50[ % 
          0.050742, 0.050742, // Centrality [50-60[ % 
          0.050430, 0.050430, // Centrality [60-70[ % 
          0.049473, 0.049473, // Centrality [70-80[ % 
          0.045439, 0.049473, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.050430, 0.020857  // 60-80%(=60-70%) and Min.bias
};
double P0_ToF2Cut_counts[CentClassesPi0] = { 
          0.784596, 0.784596, // Centrality [0-10[ % 
          0.760166, 0.760166, // Centrality [10-20[ % 
          0.733936, 0.733936, // Centrality [20-30[ % 
          0.844422, 0.844422, // Centrality [30-40[ % 
          0.829102, 0.829102, // Centrality [40-50[ % 
          0.796789, 0.796789, // Centrality [50-60[ % 
          0.835872, 0.835872, // Centrality [60-70[ % 
          0.817289, 0.817289, // Centrality [70-80[ % 
          0.785680, 0.817289, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.835872, 0.802970  // 60-80%(=60-70%) and Min.bias
};
double P0ERR_ToF2Cut_counts[CentClassesPi0] = { 
          0.134576, 0.134576, // Centrality [0-10[ % 
          0.123181, 0.123181, // Centrality [10-20[ % 
          0.106559, 0.106559, // Centrality [20-30[ % 
          0.101264, 0.101264, // Centrality [30-40[ % 
          0.099947, 0.099947, // Centrality [40-50[ % 
          0.095853, 0.095853, // Centrality [50-60[ % 
          0.094807, 0.094807, // Centrality [60-70[ % 
          0.093971, 0.093971, // Centrality [70-80[ % 
          0.085451, 0.093971, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.094807, 0.041661  // 60-80%(=60-70%) and Min.bias
};
double P1_ToF2Cut_counts[CentClassesPi0] = { 
          -0.028026, -0.028026, // Centrality [0-10[ % 
          -0.100411, -0.100411, // Centrality [10-20[ % 
          -0.093386, -0.093386, // Centrality [20-30[ % 
          -0.261948, -0.261948, // Centrality [30-40[ % 
          -0.253951, -0.253951, // Centrality [40-50[ % 
          -0.208385, -0.208385, // Centrality [50-60[ % 
          -0.244272, -0.244272, // Centrality [60-70[ % 
          -0.239273, -0.239273, // Centrality [70-80[ % 
          -0.198314, -0.239273, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.244272, -0.207109  // 60-80%(=60-70%) and Min.bias
};
double P1ERR_ToF2Cut_counts[CentClassesPi0] = { 
          0.145017, 0.145017, // Centrality [0-10[ % 
          0.132101, 0.132101, // Centrality [10-20[ % 
          0.114294, 0.114294, // Centrality [20-30[ % 
          0.108344, 0.108344, // Centrality [30-40[ % 
          0.106825, 0.106825, // Centrality [40-50[ % 
          0.102448, 0.102448, // Centrality [50-60[ % 
          0.101146, 0.101146, // Centrality [60-70[ % 
          0.100295, 0.100295, // Centrality [70-80[ % 
          0.091183, 0.100295, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.101146, 0.044552  // 60-80%(=60-70%) and Min.bias
};
double P2_ToF2Cut_counts[CentClassesPi0] = { 
          0.004563, 0.004563, // Centrality [0-10[ % 
          0.004604, 0.004604, // Centrality [10-20[ % 
          0.003149, 0.003149, // Centrality [20-30[ % 
          0.007475, 0.007475, // Centrality [30-40[ % 
          0.007124, 0.007124, // Centrality [40-50[ % 
          0.005726, 0.005726, // Centrality [50-60[ % 
          0.006219, 0.006219, // Centrality [60-70[ % 
          0.006062, 0.006062, // Centrality [70-80[ % 
          0.004774, 0.006062, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.006219, 0.005726  // 60-80%(=60-70%) and Min.bias
};
double P2ERR_ToF2Cut_counts[CentClassesPi0] = { 
          0.004657, 0.004657, // Centrality [0-10[ % 
          0.004351, 0.004351, // Centrality [10-20[ % 
          0.003684, 0.003684, // Centrality [20-30[ % 
          0.003525, 0.003525, // Centrality [30-40[ % 
          0.003500, 0.003500, // Centrality [40-50[ % 
          0.003308, 0.003308, // Centrality [50-60[ % 
          0.003262, 0.003262, // Centrality [60-70[ % 
          0.003234, 0.003234, // Centrality [70-80[ % 
          0.002938, 0.003234, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.003262, 0.001457  // 60-80%(=60-70%) and Min.bias
};
double P3_ToF2Cut_counts[CentClassesPi0] = { 
          0.506934, 0.506934, // Centrality [0-10[ % 
          0.455252, 0.455252, // Centrality [10-20[ % 
          0.394926, 0.394926, // Centrality [20-30[ % 
          0.486837, 0.486837, // Centrality [30-40[ % 
          0.454979, 0.454979, // Centrality [40-50[ % 
          0.390795, 0.390795, // Centrality [50-60[ % 
          0.381681, 0.381681, // Centrality [60-70[ % 
          0.396589, 0.396589, // Centrality [70-80[ % 
          0.352515, 0.396589, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.381681, 0.423803  // 60-80%(=60-70%) and Min.bias
};
double P3ERR_ToF2Cut_counts[CentClassesPi0] = { 
          0.118629, 0.118629, // Centrality [0-10[ % 
          0.109640, 0.109640, // Centrality [10-20[ % 
          0.092598, 0.092598, // Centrality [20-30[ % 
          0.088235, 0.088235, // Centrality [30-40[ % 
          0.086952, 0.086952, // Centrality [40-50[ % 
          0.082602, 0.082602, // Centrality [50-60[ % 
          0.081292, 0.081292, // Centrality [60-70[ % 
          0.080679, 0.080679, // Centrality [70-80[ % 
          0.073245, 0.080679, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.081292, 0.036421  // 60-80%(=60-70%) and Min.bias
};
double P4_ToF2Cut_counts[CentClassesPi0] = { 
          -0.201410, -0.201410, // Centrality [0-10[ % 
          -0.040356, -0.040356, // Centrality [10-20[ % 
          -0.021939, -0.021939, // Centrality [20-30[ % 
          0.153978, 0.153978, // Centrality [30-40[ % 
          0.159438, 0.159438, // Centrality [40-50[ % 
          0.134401, 0.134401, // Centrality [50-60[ % 
          0.186616, 0.186616, // Centrality [60-70[ % 
          0.173030, 0.173030, // Centrality [70-80[ % 
          0.146933, 0.173030, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.186616, 0.120491  // 60-80%(=60-70%) and Min.bias
};
double P4ERR_ToF2Cut_counts[CentClassesPi0] = { 
          0.113881, 0.113881, // Centrality [0-10[ % 
          0.100920, 0.100920, // Centrality [10-20[ % 
          0.089910, 0.089910, // Centrality [20-30[ % 
          0.084658, 0.084658, // Centrality [30-40[ % 
          0.082944, 0.082944, // Centrality [40-50[ % 
          0.080703, 0.080703, // Centrality [50-60[ % 
          0.079873, 0.079873, // Centrality [60-70[ % 
          0.079177, 0.079177, // Centrality [70-80[ % 
          0.072064, 0.079177, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.079873, 0.034560  // 60-80%(=60-70%) and Min.bias
};
double P0_BasicCuts5x5Asym1ToF2Cut_counts[CentClassesPi0] = { 
          0.253446, 0.253446, // Centrality [0-10[ % 
          0.299310, 0.299310, // Centrality [10-20[ % 
          0.257369, 0.257369, // Centrality [20-30[ % 
          0.335752, 0.335752, // Centrality [30-40[ % 
          0.305487, 0.305487, // Centrality [40-50[ % 
          0.253401, 0.253401, // Centrality [50-60[ % 
          0.275729, 0.275729, // Centrality [60-70[ % 
          0.287030, 0.287030, // Centrality [70-80[ % 
          0.234924, 0.287030, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.275729, 0.282512  // 60-80%(=60-70%) and Min.bias
};
double P0ERR_BasicCuts5x5Asym1ToF2Cut_counts[CentClassesPi0] = { 
          0.079752, 0.079752, // Centrality [0-10[ % 
          0.075152, 0.075152, // Centrality [10-20[ % 
          0.062518, 0.062518, // Centrality [20-30[ % 
          0.060803, 0.060803, // Centrality [30-40[ % 
          0.060942, 0.060942, // Centrality [40-50[ % 
          0.057245, 0.057245, // Centrality [50-60[ % 
          0.056714, 0.056714, // Centrality [60-70[ % 
          0.055913, 0.055913, // Centrality [70-80[ % 
          0.051139, 0.055913, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.056714, 0.023573  // 60-80%(=60-70%) and Min.bias
};
double P1_BasicCuts5x5Asym1ToF2Cut_counts[CentClassesPi0] = { 
          0.093828, 0.093828, // Centrality [0-10[ % 
          -0.001937, -0.001937, // Centrality [10-20[ % 
          0.040285, 0.040285, // Centrality [20-30[ % 
          -0.070777, -0.070777, // Centrality [30-40[ % 
          -0.045579, -0.045579, // Centrality [40-50[ % 
          0.017849, 0.017849, // Centrality [50-60[ % 
          -0.005482, -0.005482, // Centrality [60-70[ % 
          -0.026305, -0.026305, // Centrality [70-80[ % 
          0.038339, -0.026305, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.005482, -0.010333  // 60-80%(=60-70%) and Min.bias
};
double P1ERR_BasicCuts5x5Asym1ToF2Cut_counts[CentClassesPi0] = { 
          0.086160, 0.086160, // Centrality [0-10[ % 
          0.080696, 0.080696, // Centrality [10-20[ % 
          0.067161, 0.067161, // Centrality [20-30[ % 
          0.065227, 0.065227, // Centrality [30-40[ % 
          0.065243, 0.065243, // Centrality [40-50[ % 
          0.061291, 0.061291, // Centrality [50-60[ % 
          0.060652, 0.060652, // Centrality [60-70[ % 
          0.059751, 0.059751, // Centrality [70-80[ % 
          0.054670, 0.059751, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.060652, 0.025255  // 60-80%(=60-70%) and Min.bias
};
double P2_BasicCuts5x5Asym1ToF2Cut_counts[CentClassesPi0] = { 
          -0.000790, -0.000790, // Centrality [0-10[ % 
          0.001288, 0.001288, // Centrality [10-20[ % 
          -0.000868, -0.000868, // Centrality [20-30[ % 
          0.002275, 0.002275, // Centrality [30-40[ % 
          0.001422, 0.001422, // Centrality [40-50[ % 
          -0.000712, -0.000712, // Centrality [50-60[ % 
          -0.000097, -0.000097, // Centrality [60-70[ % 
          0.000420, 0.000420, // Centrality [70-80[ % 
          -0.001332, 0.000420, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.000097, 0.000375  // 60-80%(=60-70%) and Min.bias
};
double P2ERR_BasicCuts5x5Asym1ToF2Cut_counts[CentClassesPi0] = { 
          0.002775, 0.002775, // Centrality [0-10[ % 
          0.002667, 0.002667, // Centrality [10-20[ % 
          0.002159, 0.002159, // Centrality [20-30[ % 
          0.002121, 0.002121, // Centrality [30-40[ % 
          0.002152, 0.002152, // Centrality [40-50[ % 
          0.001982, 0.001982, // Centrality [50-60[ % 
          0.001960, 0.001960, // Centrality [60-70[ % 
          0.001931, 0.001931, // Centrality [70-80[ % 
          0.001765, 0.001931, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.001960, 0.000828  // 60-80%(=60-70%) and Min.bias
};
double P3_BasicCuts5x5Asym1ToF2Cut_counts[CentClassesPi0] = { 
          0.191301, 0.191301, // Centrality [0-10[ % 
          0.218652, 0.218652, // Centrality [10-20[ % 
          0.134222, 0.134222, // Centrality [20-30[ % 
          0.214437, 0.214437, // Centrality [30-40[ % 
          0.177458, 0.177458, // Centrality [40-50[ % 
          0.104904, 0.104904, // Centrality [50-60[ % 
          0.116974, 0.116974, // Centrality [60-70[ % 
          0.131877, 0.131877, // Centrality [70-80[ % 
          0.084250, 0.131877, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.116974, 0.153239  // 60-80%(=60-70%) and Min.bias
};
double P3ERR_BasicCuts5x5Asym1ToF2Cut_counts[CentClassesPi0] = { 
          0.070619, 0.070619, // Centrality [0-10[ % 
          0.067363, 0.067363, // Centrality [10-20[ % 
          0.054352, 0.054352, // Centrality [20-30[ % 
          0.053428, 0.053428, // Centrality [30-40[ % 
          0.053315, 0.053315, // Centrality [40-50[ % 
          0.049565, 0.049565, // Centrality [50-60[ % 
          0.048985, 0.048985, // Centrality [60-70[ % 
          0.048195, 0.048195, // Centrality [70-80[ % 
          0.044065, 0.048195, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.048985, 0.020771  // 60-80%(=60-70%) and Min.bias
};
double P4_BasicCuts5x5Asym1ToF2Cut_counts[CentClassesPi0] = { 
          -0.199310, -0.199310, // Centrality [0-10[ % 
          -0.079541, -0.079541, // Centrality [10-20[ % 
          -0.086910, -0.086910, // Centrality [20-30[ % 
          0.014266, 0.014266, // Centrality [30-40[ % 
          0.004230, 0.004230, // Centrality [40-50[ % 
          -0.034979, -0.034979, // Centrality [50-60[ % 
          -0.012697, -0.012697, // Centrality [60-70[ % 
          0.005814, 0.005814, // Centrality [70-80[ % 
          -0.050872, 0.005814, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.012697, -0.026265  // 60-80%(=60-70%) and Min.bias
};
double P4ERR_BasicCuts5x5Asym1ToF2Cut_counts[CentClassesPi0] = { 
          0.067520, 0.067520, // Centrality [0-10[ % 
          0.061274, 0.061274, // Centrality [10-20[ % 
          0.052957, 0.052957, // Centrality [20-30[ % 
          0.050708, 0.050708, // Centrality [30-40[ % 
          0.050306, 0.050306, // Centrality [40-50[ % 
          0.048156, 0.048156, // Centrality [50-60[ % 
          0.047703, 0.047703, // Centrality [60-70[ % 
          0.047040, 0.047040, // Centrality [70-80[ % 
          0.043083, 0.047040, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.047703, 0.019459  // 60-80%(=60-70%) and Min.bias
};
double P0_RToFCut_counts[CentClassesPi0] = { 
          0.981374, 0.981374, // Centrality [0-10[ % 
          1.060814, 1.060814, // Centrality [10-20[ % 
          0.859715, 0.859715, // Centrality [20-30[ % 
          1.031490, 1.031490, // Centrality [30-40[ % 
          0.910853, 0.910853, // Centrality [40-50[ % 
          0.892043, 0.892043, // Centrality [50-60[ % 
          0.904419, 0.904419, // Centrality [60-70[ % 
          0.936721, 0.936721, // Centrality [70-80[ % 
          0.890106, 0.936721, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.904419, 0.971313  // 60-80%(=60-70%) and Min.bias
};
double P0ERR_RToFCut_counts[CentClassesPi0] = { 
          0.129670, 0.129670, // Centrality [0-10[ % 
          0.110284, 0.110284, // Centrality [10-20[ % 
          0.102281, 0.102281, // Centrality [20-30[ % 
          0.096850, 0.096850, // Centrality [30-40[ % 
          0.095069, 0.095069, // Centrality [40-50[ % 
          0.091820, 0.091820, // Centrality [50-60[ % 
          0.090658, 0.090658, // Centrality [60-70[ % 
          0.089818, 0.089818, // Centrality [70-80[ % 
          0.081472, 0.089818, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.090658, 0.037934  // 60-80%(=60-70%) and Min.bias
};
double P1_RToFCut_counts[CentClassesPi0] = { 
          -0.283590, -0.283590, // Centrality [0-10[ % 
          -0.464499, -0.464499, // Centrality [10-20[ % 
          -0.272923, -0.272923, // Centrality [20-30[ % 
          -0.510472, -0.510472, // Centrality [30-40[ % 
          -0.387427, -0.387427, // Centrality [40-50[ % 
          -0.357786, -0.357786, // Centrality [50-60[ % 
          -0.369670, -0.369670, // Centrality [60-70[ % 
          -0.415608, -0.415608, // Centrality [70-80[ % 
          -0.362361, -0.415608, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.369670, -0.434551  // 60-80%(=60-70%) and Min.bias
};
double P1ERR_RToFCut_counts[CentClassesPi0] = { 
          0.139423, 0.139423, // Centrality [0-10[ % 
          0.118123, 0.118123, // Centrality [10-20[ % 
          0.109483, 0.109483, // Centrality [20-30[ % 
          0.103377, 0.103377, // Centrality [30-40[ % 
          0.101432, 0.101432, // Centrality [40-50[ % 
          0.097944, 0.097944, // Centrality [50-60[ % 
          0.096563, 0.096563, // Centrality [60-70[ % 
          0.095618, 0.095618, // Centrality [70-80[ % 
          0.086688, 0.095618, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.096563, 0.040488  // 60-80%(=60-70%) and Min.bias
};
double P2_RToFCut_counts[CentClassesPi0] = { 
          0.012718, 0.012718, // Centrality [0-10[ % 
          0.015940, 0.015940, // Centrality [10-20[ % 
          0.009452, 0.009452, // Centrality [20-30[ % 
          0.016192, 0.016192, // Centrality [30-40[ % 
          0.012156, 0.012156, // Centrality [40-50[ % 
          0.011264, 0.011264, // Centrality [50-60[ % 
          0.011006, 0.011006, // Centrality [60-70[ % 
          0.012325, 0.012325, // Centrality [70-80[ % 
          0.010639, 0.012325, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.011006, 0.013536  // 60-80%(=60-70%) and Min.bias
};
double P2ERR_RToFCut_counts[CentClassesPi0] = { 
          0.004472, 0.004472, // Centrality [0-10[ % 
          0.003837, 0.003837, // Centrality [10-20[ % 
          0.003527, 0.003527, // Centrality [20-30[ % 
          0.003352, 0.003352, // Centrality [30-40[ % 
          0.003314, 0.003314, // Centrality [40-50[ % 
          0.003163, 0.003163, // Centrality [50-60[ % 
          0.003113, 0.003113, // Centrality [60-70[ % 
          0.003082, 0.003082, // Centrality [70-80[ % 
          0.002794, 0.003082, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.003113, 0.001310  // 60-80%(=60-70%) and Min.bias
};
double P3_RToFCut_counts[CentClassesPi0] = { 
          0.681036, 0.681036, // Centrality [0-10[ % 
          0.736106, 0.736106, // Centrality [10-20[ % 
          0.533652, 0.533652, // Centrality [20-30[ % 
          0.695653, 0.695653, // Centrality [30-40[ % 
          0.573338, 0.573338, // Centrality [40-50[ % 
          0.522334, 0.522334, // Centrality [50-60[ % 
          0.510363, 0.510363, // Centrality [60-70[ % 
          0.548034, 0.548034, // Centrality [70-80[ % 
          0.495939, 0.548034, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.510363, 0.613989  // 60-80%(=60-70%) and Min.bias
};
double P3ERR_RToFCut_counts[CentClassesPi0] = { 
          0.113858, 0.113858, // Centrality [0-10[ % 
          0.096930, 0.096930, // Centrality [10-20[ % 
          0.088399, 0.088399, // Centrality [20-30[ % 
          0.083949, 0.083949, // Centrality [30-40[ % 
          0.082393, 0.082393, // Centrality [40-50[ % 
          0.078856, 0.078856, // Centrality [50-60[ % 
          0.077509, 0.077509, // Centrality [60-70[ % 
          0.076735, 0.076735, // Centrality [70-80[ % 
          0.069480, 0.076735, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.077509, 0.032844  // 60-80%(=60-70%) and Min.bias
};
double P4_RToFCut_counts[CentClassesPi0] = { 
          0.000690, 0.000690, // Centrality [0-10[ % 
          0.233174, 0.233174, // Centrality [10-20[ % 
          0.101888, 0.101888, // Centrality [20-30[ % 
          0.320426, 0.320426, // Centrality [30-40[ % 
          0.238301, 0.238301, // Centrality [40-50[ % 
          0.225189, 0.225189, // Centrality [50-60[ % 
          0.251435, 0.251435, // Centrality [60-70[ % 
          0.287820, 0.287820, // Centrality [70-80[ % 
          0.252657, 0.287820, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.251435, 0.274257  // 60-80%(=60-70%) and Min.bias
};
double P4ERR_RToFCut_counts[CentClassesPi0] = { 
          0.109728, 0.109728, // Centrality [0-10[ % 
          0.091991, 0.091991, // Centrality [10-20[ % 
          0.086353, 0.086353, // Centrality [20-30[ % 
          0.081000, 0.081000, // Centrality [30-40[ % 
          0.079021, 0.079021, // Centrality [40-50[ % 
          0.077242, 0.077242, // Centrality [50-60[ % 
          0.076359, 0.076359, // Centrality [60-70[ % 
          0.075633, 0.075633, // Centrality [70-80[ % 
          0.068620, 0.075633, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.076359, 0.031785  // 60-80%(=60-70%) and Min.bias
};
double P0_BasicCuts5x5Asym2RToFCut_counts[CentClassesPi0] = { 
          0.133530, 0.133530, // Centrality [0-10[ % 
          0.080606, 0.080606, // Centrality [10-20[ % 
          0.076047, 0.076047, // Centrality [20-30[ % 
          0.097873, 0.097873, // Centrality [30-40[ % 
          0.113337, 0.113337, // Centrality [40-50[ % 
          0.102166, 0.102166, // Centrality [50-60[ % 
          0.061711, 0.061711, // Centrality [60-70[ % 
          0.089109, 0.089109, // Centrality [70-80[ % 
          0.077159, 0.089109, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.061711, 0.096667  // 60-80%(=60-70%) and Min.bias
};
double P0ERR_BasicCuts5x5Asym2RToFCut_counts[CentClassesPi0] = { 
          0.040205, 0.040205, // Centrality [0-10[ % 
          0.034780, 0.034780, // Centrality [10-20[ % 
          0.032564, 0.032564, // Centrality [20-30[ % 
          0.031488, 0.031488, // Centrality [30-40[ % 
          0.032974, 0.032974, // Centrality [40-50[ % 
          0.029960, 0.029960, // Centrality [50-60[ % 
          0.029444, 0.029444, // Centrality [60-70[ % 
          0.028837, 0.028837, // Centrality [70-80[ % 
          0.026452, 0.028837, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.029444, 0.010912  // 60-80%(=60-70%) and Min.bias
};
double P1_BasicCuts5x5Asym2RToFCut_counts[CentClassesPi0] = { 
          -0.023487, -0.023487, // Centrality [0-10[ % 
          0.015286, 0.015286, // Centrality [10-20[ % 
          0.018798, 0.018798, // Centrality [20-30[ % 
          -0.013884, -0.013884, // Centrality [30-40[ % 
          -0.030723, -0.030723, // Centrality [40-50[ % 
          -0.014958, -0.014958, // Centrality [50-60[ % 
          0.025761, 0.025761, // Centrality [60-70[ % 
          -0.007589, -0.007589, // Centrality [70-80[ % 
          0.008451, -0.007589, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.025761, -0.010091  // 60-80%(=60-70%) and Min.bias
};
double P1ERR_BasicCuts5x5Asym2RToFCut_counts[CentClassesPi0] = { 
          0.042995, 0.042995, // Centrality [0-10[ % 
          0.037170, 0.037170, // Centrality [10-20[ % 
          0.034731, 0.034731, // Centrality [20-30[ % 
          0.033581, 0.033581, // Centrality [30-40[ % 
          0.034979, 0.034979, // Centrality [40-50[ % 
          0.031736, 0.031736, // Centrality [50-60[ % 
          0.031224, 0.031224, // Centrality [60-70[ % 
          0.030507, 0.030507, // Centrality [70-80[ % 
          0.027992, 0.030507, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.031224, 0.011585  // 60-80%(=60-70%) and Min.bias
};
double P2_BasicCuts5x5Asym2RToFCut_counts[CentClassesPi0] = { 
          0.000819, 0.000819, // Centrality [0-10[ % 
          -0.000386, -0.000386, // Centrality [10-20[ % 
          -0.000641, -0.000641, // Centrality [20-30[ % 
          0.000230, 0.000230, // Centrality [30-40[ % 
          0.000728, 0.000728, // Centrality [40-50[ % 
          0.000158, 0.000158, // Centrality [50-60[ % 
          -0.001151, -0.001151, // Centrality [60-70[ % 
          -0.000046, -0.000046, // Centrality [70-80[ % 
          -0.000474, -0.000046, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.001151, 0.000081  // 60-80%(=60-70%) and Min.bias
};
double P2ERR_BasicCuts5x5Asym2RToFCut_counts[CentClassesPi0] = { 
          0.001375, 0.001375, // Centrality [0-10[ % 
          0.001193, 0.001193, // Centrality [10-20[ % 
          0.001106, 0.001106, // Centrality [20-30[ % 
          0.001086, 0.001086, // Centrality [30-40[ % 
          0.001155, 0.001155, // Centrality [40-50[ % 
          0.001015, 0.001015, // Centrality [50-60[ % 
          0.000998, 0.000998, // Centrality [60-70[ % 
          0.000975, 0.000975, // Centrality [70-80[ % 
          0.000894, 0.000975, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.000998, 0.000375  // 60-80%(=60-70%) and Min.bias
};
double P3_BasicCuts5x5Asym2RToFCut_counts[CentClassesPi0] = { 
          0.038453, 0.038453, // Centrality [0-10[ % 
          0.015272, 0.015272, // Centrality [10-20[ % 
          -0.003361, -0.003361, // Centrality [20-30[ % 
          0.018137, 0.018137, // Centrality [30-40[ % 
          0.028466, 0.028466, // Centrality [40-50[ % 
          0.004397, 0.004397, // Centrality [50-60[ % 
          -0.022438, -0.022438, // Centrality [60-70[ % 
          0.005159, 0.005159, // Centrality [70-80[ % 
          -0.005446, 0.005159, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.022438, 0.012644  // 60-80%(=60-70%) and Min.bias
};
double P3ERR_BasicCuts5x5Asym2RToFCut_counts[CentClassesPi0] = { 
          0.034657, 0.034657, // Centrality [0-10[ % 
          0.029994, 0.029994, // Centrality [10-20[ % 
          0.027658, 0.027658, // Centrality [20-30[ % 
          0.026985, 0.026985, // Centrality [30-40[ % 
          0.028161, 0.028161, // Centrality [40-50[ % 
          0.025059, 0.025059, // Centrality [50-60[ % 
          0.024666, 0.024666, // Centrality [60-70[ % 
          0.024024, 0.024024, // Centrality [70-80[ % 
          0.022050, 0.024024, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.024666, 0.009315  // 60-80%(=60-70%) and Min.bias
};
double P4_BasicCuts5x5Asym2RToFCut_counts[CentClassesPi0] = { 
          0.009783, 0.009783, // Centrality [0-10[ % 
          -0.021259, -0.021259, // Centrality [10-20[ % 
          -0.015890, -0.015890, // Centrality [20-30[ % 
          0.015681, 0.015681, // Centrality [30-40[ % 
          0.030340, 0.030340, // Centrality [40-50[ % 
          0.024265, 0.024265, // Centrality [50-60[ % 
          -0.009652, -0.009652, // Centrality [60-70[ % 
          0.015490, 0.015490, // Centrality [70-80[ % 
          0.000278, 0.015490, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.009652, 0.014011  // 60-80%(=60-70%) and Min.bias
};
double P4ERR_BasicCuts5x5Asym2RToFCut_counts[CentClassesPi0] = { 
          0.034190, 0.034190, // Centrality [0-10[ % 
          0.029381, 0.029381, // Centrality [10-20[ % 
          0.027793, 0.027793, // Centrality [20-30[ % 
          0.026482, 0.026482, // Centrality [30-40[ % 
          0.027197, 0.027197, // Centrality [40-50[ % 
          0.025444, 0.025444, // Centrality [50-60[ % 
          0.025040, 0.025040, // Centrality [60-70[ % 
          0.024510, 0.024510, // Centrality [70-80[ % 
          0.022492, 0.024510, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.025040, 0.009144  // 60-80%(=60-70%) and Min.bias
};
double P0_ChiSq1Cut_counts[CentClassesPi0] = { 
          0.833432, 0.833432, // Centrality [0-10[ % 
          0.762120, 0.762120, // Centrality [10-20[ % 
          0.861419, 0.861419, // Centrality [20-30[ % 
          1.023359, 1.023359, // Centrality [30-40[ % 
          1.007036, 1.007036, // Centrality [40-50[ % 
          1.020810, 1.020810, // Centrality [50-60[ % 
          1.106594, 1.106594, // Centrality [60-70[ % 
          1.031802, 1.031802, // Centrality [70-80[ % 
          1.060519, 1.031802, // Centrality [80-93[ %  and 60-92% (=70-80%)
          1.106594, 0.958075  // 60-80%(=60-70%) and Min.bias
};
double P0ERR_ChiSq1Cut_counts[CentClassesPi0] = { 
          0.153912, 0.153912, // Centrality [0-10[ % 
          0.193419, 0.193419, // Centrality [10-20[ % 
          0.119508, 0.119508, // Centrality [20-30[ % 
          0.115373, 0.115373, // Centrality [30-40[ % 
          0.111188, 0.111188, // Centrality [40-50[ % 
          0.127282, 0.127282, // Centrality [50-60[ % 
          0.106069, 0.106069, // Centrality [60-70[ % 
          0.104905, 0.104905, // Centrality [70-80[ % 
          0.095416, 0.104905, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.106069, 0.061171  // 60-80%(=60-70%) and Min.bias
};
double P1_ChiSq1Cut_counts[CentClassesPi0] = { 
          0.214948, 0.214948, // Centrality [0-10[ % 
          0.154892, 0.154892, // Centrality [10-20[ % 
          0.036495, 0.036495, // Centrality [20-30[ % 
          -0.217826, -0.217826, // Centrality [30-40[ % 
          -0.214530, -0.214530, // Centrality [40-50[ % 
          -0.210108, -0.210108, // Centrality [50-60[ % 
          -0.296232, -0.296232, // Centrality [60-70[ % 
          -0.230827, -0.230827, // Centrality [70-80[ % 
          -0.257908, -0.230827, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.296232, -0.132898  // 60-80%(=60-70%) and Min.bias
};
double P1ERR_ChiSq1Cut_counts[CentClassesPi0] = { 
          0.164975, 0.164975, // Centrality [0-10[ % 
          0.206564, 0.206564, // Centrality [10-20[ % 
          0.127557, 0.127557, // Centrality [20-30[ % 
          0.122806, 0.122806, // Centrality [30-40[ % 
          0.118254, 0.118254, // Centrality [40-50[ % 
          0.135394, 0.135394, // Centrality [50-60[ % 
          0.112443, 0.112443, // Centrality [60-70[ % 
          0.111392, 0.111392, // Centrality [70-80[ % 
          0.101199, 0.111392, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.112443, 0.065091  // 60-80%(=60-70%) and Min.bias
};
double P2_ChiSq1Cut_counts[CentClassesPi0] = { 
          -0.003822, -0.003822, // Centrality [0-10[ % 
          -0.004064, -0.004064, // Centrality [10-20[ % 
          -0.001848, -0.001848, // Centrality [20-30[ % 
          0.005426, 0.005426, // Centrality [30-40[ % 
          0.005575, 0.005575, // Centrality [40-50[ % 
          0.004755, 0.004755, // Centrality [50-60[ % 
          0.007199, 0.007199, // Centrality [60-70[ % 
          0.005468, 0.005468, // Centrality [70-80[ % 
          0.006262, 0.005468, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.007199, 0.002088  // 60-80%(=60-70%) and Min.bias
};
double P2ERR_ChiSq1Cut_counts[CentClassesPi0] = { 
          0.005198, 0.005198, // Centrality [0-10[ % 
          0.007223, 0.007223, // Centrality [10-20[ % 
          0.004057, 0.004057, // Centrality [20-30[ % 
          0.003952, 0.003952, // Centrality [30-40[ % 
          0.003828, 0.003828, // Centrality [40-50[ % 
          0.004559, 0.004559, // Centrality [50-60[ % 
          0.003590, 0.003590, // Centrality [60-70[ % 
          0.003559, 0.003559, // Centrality [70-80[ % 
          0.003230, 0.003559, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.003590, 0.002244  // 60-80%(=60-70%) and Min.bias
};
double P3_ChiSq1Cut_counts[CentClassesPi0] = { 
          -0.004216, -0.004216, // Centrality [0-10[ % 
          -0.003442, -0.003442, // Centrality [10-20[ % 
          0.054405, 0.054405, // Centrality [20-30[ % 
          0.264605, 0.264605, // Centrality [30-40[ % 
          0.267022, 0.267022, // Centrality [40-50[ % 
          0.232596, 0.232596, // Centrality [50-60[ % 
          0.255472, 0.255472, // Centrality [60-70[ % 
          0.241577, 0.241577, // Centrality [70-80[ % 
          0.253621, 0.241577, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.255472, 0.175975  // 60-80%(=60-70%) and Min.bias
};
double P3ERR_ChiSq1Cut_counts[CentClassesPi0] = { 
          0.132797, 0.132797, // Centrality [0-10[ % 
          0.173902, 0.173902, // Centrality [10-20[ % 
          0.101275, 0.101275, // Centrality [20-30[ % 
          0.098300, 0.098300, // Centrality [30-40[ % 
          0.094591, 0.094591, // Centrality [40-50[ % 
          0.110040, 0.110040, // Centrality [50-60[ % 
          0.088745, 0.088745, // Centrality [60-70[ % 
          0.088192, 0.088192, // Centrality [70-80[ % 
          0.079917, 0.088192, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.088745, 0.053803  // 60-80%(=60-70%) and Min.bias
};
double P4_ChiSq1Cut_counts[CentClassesPi0] = { 
          -0.278670, -0.278670, // Centrality [0-10[ % 
          -0.142407, -0.142407, // Centrality [10-20[ % 
          -0.035057, -0.035057, // Centrality [20-30[ % 
          0.190875, 0.190875, // Centrality [30-40[ % 
          0.180962, 0.180962, // Centrality [40-50[ % 
          0.194053, 0.194053, // Centrality [50-60[ % 
          0.289656, 0.289656, // Centrality [60-70[ % 
          0.214757, 0.214757, // Centrality [70-80[ % 
          0.242486, 0.214757, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.289656, 0.138498  // 60-80%(=60-70%) and Min.bias
};
double P4ERR_ChiSq1Cut_counts[CentClassesPi0] = { 
          0.132233, 0.132233, // Centrality [0-10[ % 
          0.150050, 0.150050, // Centrality [10-20[ % 
          0.102381, 0.102381, // Centrality [20-30[ % 
          0.097571, 0.097571, // Centrality [30-40[ % 
          0.093457, 0.093457, // Centrality [40-50[ % 
          0.103352, 0.103352, // Centrality [50-60[ % 
          0.090259, 0.090259, // Centrality [60-70[ % 
          0.089251, 0.089251, // Centrality [70-80[ % 
          0.081238, 0.089251, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.090259, 0.048527  // 60-80%(=60-70%) and Min.bias
};
double P0_BasicCuts5x5Asym1ChiSq1Cut_counts[CentClassesPi0] = { 
          0.183317, 0.183317, // Centrality [0-10[ % 
          0.174792, 0.174792, // Centrality [10-20[ % 
          0.278584, 0.278584, // Centrality [20-30[ % 
          0.365735, 0.365735, // Centrality [30-40[ % 
          0.309876, 0.309876, // Centrality [40-50[ % 
          0.258496, 0.258496, // Centrality [50-60[ % 
          0.352990, 0.352990, // Centrality [60-70[ % 
          0.294149, 0.294149, // Centrality [70-80[ % 
          0.289218, 0.294149, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.352990, 0.288920  // 60-80%(=60-70%) and Min.bias
};
double P0ERR_BasicCuts5x5Asym1ChiSq1Cut_counts[CentClassesPi0] = { 
          0.083007, 0.083007, // Centrality [0-10[ % 
          0.077342, 0.077342, // Centrality [10-20[ % 
          0.065285, 0.065285, // Centrality [20-30[ % 
          0.063736, 0.063736, // Centrality [30-40[ % 
          0.064334, 0.064334, // Centrality [40-50[ % 
          0.060579, 0.060579, // Centrality [50-60[ % 
          0.060311, 0.060311, // Centrality [60-70[ % 
          0.059226, 0.059226, // Centrality [70-80[ % 
          0.054336, 0.059226, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.060311, 0.024967  // 60-80%(=60-70%) and Min.bias
};
double P1_BasicCuts5x5Asym1ChiSq1Cut_counts[CentClassesPi0] = { 
          0.242681, 0.242681, // Centrality [0-10[ % 
          0.193509, 0.193509, // Centrality [10-20[ % 
          0.080299, 0.080299, // Centrality [20-30[ % 
          -0.047115, -0.047115, // Centrality [30-40[ % 
          0.002951, 0.002951, // Centrality [40-50[ % 
          0.068798, 0.068798, // Centrality [50-60[ % 
          -0.025845, -0.025845, // Centrality [60-70[ % 
          0.020518, 0.020518, // Centrality [70-80[ % 
          0.037996, 0.020518, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.025845, 0.040463  // 60-80%(=60-70%) and Min.bias
};
double P1ERR_BasicCuts5x5Asym1ChiSq1Cut_counts[CentClassesPi0] = { 
          0.089184, 0.089184, // Centrality [0-10[ % 
          0.082895, 0.082895, // Centrality [10-20[ % 
          0.069695, 0.069695, // Centrality [20-30[ % 
          0.068055, 0.068055, // Centrality [30-40[ % 
          0.068653, 0.068653, // Centrality [40-50[ % 
          0.064669, 0.064669, // Centrality [50-60[ % 
          0.064128, 0.064128, // Centrality [60-70[ % 
          0.063120, 0.063120, // Centrality [70-80[ % 
          0.057815, 0.063120, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.064128, 0.026638  // 60-80%(=60-70%) and Min.bias
};
double P2_BasicCuts5x5Asym1ChiSq1Cut_counts[CentClassesPi0] = { 
          -0.006018, -0.006018, // Centrality [0-10[ % 
          -0.005537, -0.005537, // Centrality [10-20[ % 
          -0.002829, -0.002829, // Centrality [20-30[ % 
          0.001035, 0.001035, // Centrality [30-40[ % 
          -0.000261, -0.000261, // Centrality [40-50[ % 
          -0.002489, -0.002489, // Centrality [50-60[ % 
          0.000061, 0.000061, // Centrality [60-70[ % 
          -0.001240, -0.001240, // Centrality [70-80[ % 
          -0.001692, -0.001240, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.000061, -0.001610  // 60-80%(=60-70%) and Min.bias
};
double P2ERR_BasicCuts5x5Asym1ChiSq1Cut_counts[CentClassesPi0] = { 
          0.002841, 0.002841, // Centrality [0-10[ % 
          0.002695, 0.002695, // Centrality [10-20[ % 
          0.002223, 0.002223, // Centrality [20-30[ % 
          0.002197, 0.002197, // Centrality [30-40[ % 
          0.002250, 0.002250, // Centrality [40-50[ % 
          0.002081, 0.002081, // Centrality [50-60[ % 
          0.002058, 0.002058, // Centrality [60-70[ % 
          0.002029, 0.002029, // Centrality [70-80[ % 
          0.001856, 0.002029, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.002058, 0.000865  // 60-80%(=60-70%) and Min.bias
};
double P3_BasicCuts5x5Asym1ChiSq1Cut_counts[CentClassesPi0] = { 
          -0.060778, -0.060778, // Centrality [0-10[ % 
          -0.040090, -0.040090, // Centrality [10-20[ % 
          0.006295, 0.006295, // Centrality [20-30[ % 
          0.125166, 0.125166, // Centrality [30-40[ % 
          0.086696, 0.086696, // Centrality [40-50[ % 
          0.013151, 0.013151, // Centrality [50-60[ % 
          0.067647, 0.067647, // Centrality [60-70[ % 
          0.051415, 0.051415, // Centrality [70-80[ % 
          0.028599, 0.051415, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.067647, 0.044447  // 60-80%(=60-70%) and Min.bias
};
double P3ERR_BasicCuts5x5Asym1ChiSq1Cut_counts[CentClassesPi0] = { 
          0.072146, 0.072146, // Centrality [0-10[ % 
          0.067531, 0.067531, // Centrality [10-20[ % 
          0.055489, 0.055489, // Centrality [20-30[ % 
          0.055006, 0.055006, // Centrality [30-40[ % 
          0.055560, 0.055560, // Centrality [40-50[ % 
          0.051834, 0.051834, // Centrality [50-60[ % 
          0.051086, 0.051086, // Centrality [60-70[ % 
          0.050447, 0.050447, // Centrality [70-80[ % 
          0.046060, 0.050447, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.051086, 0.021575  // 60-80%(=60-70%) and Min.bias
};
double P4_BasicCuts5x5Asym1ChiSq1Cut_counts[CentClassesPi0] = { 
          -0.260711, -0.260711, // Centrality [0-10[ % 
          -0.186156, -0.186156, // Centrality [10-20[ % 
          -0.072760, -0.072760, // Centrality [20-30[ % 
          0.028731, 0.028731, // Centrality [30-40[ % 
          -0.012565, -0.012565, // Centrality [40-50[ % 
          -0.053989, -0.053989, // Centrality [50-60[ % 
          0.034313, 0.034313, // Centrality [60-70[ % 
          -0.013330, -0.013330, // Centrality [70-80[ % 
          -0.024431, -0.013330, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.034313, -0.035963  // 60-80%(=60-70%) and Min.bias
};
double P4ERR_BasicCuts5x5Asym1ChiSq1Cut_counts[CentClassesPi0] = { 
          0.070887, 0.070887, // Centrality [0-10[ % 
          0.064462, 0.064462, // Centrality [10-20[ % 
          0.055733, 0.055733, // Centrality [20-30[ % 
          0.053579, 0.053579, // Centrality [30-40[ % 
          0.053451, 0.053451, // Centrality [40-50[ % 
          0.051217, 0.051217, // Centrality [50-60[ % 
          0.051054, 0.051054, // Centrality [60-70[ % 
          0.050115, 0.050115, // Centrality [70-80[ % 
          0.046024, 0.050115, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.051054, 0.020834  // 60-80%(=60-70%) and Min.bias
};
double P0_BasicCuts5x5Asym1ChiSq1ToF1Cut_counts[CentClassesPi0] = { 
          0.167886, 0.167886, // Centrality [0-10[ % 
          0.177341, 0.177341, // Centrality [10-20[ % 
          0.261419, 0.261419, // Centrality [20-30[ % 
          0.364661, 0.364661, // Centrality [30-40[ % 
          0.300631, 0.300631, // Centrality [40-50[ % 
          0.269029, 0.269029, // Centrality [50-60[ % 
          0.343319, 0.343319, // Centrality [60-70[ % 
          0.289466, 0.289466, // Centrality [70-80[ % 
          0.269718, 0.289466, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.343319, 0.280553  // 60-80%(=60-70%) and Min.bias
};
double P0ERR_BasicCuts5x5Asym1ChiSq1ToF1Cut_counts[CentClassesPi0] = { 
          0.080470, 0.080470, // Centrality [0-10[ % 
          0.075528, 0.075528, // Centrality [10-20[ % 
          0.063861, 0.063861, // Centrality [20-30[ % 
          0.062636, 0.062636, // Centrality [30-40[ % 
          0.063377, 0.063377, // Centrality [40-50[ % 
          0.059569, 0.059569, // Centrality [50-60[ % 
          0.059380, 0.059380, // Centrality [60-70[ % 
          0.058342, 0.058342, // Centrality [70-80[ % 
          0.053569, 0.058342, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.059380, 0.024453  // 60-80%(=60-70%) and Min.bias
};
double P1_BasicCuts5x5Asym1ChiSq1ToF1Cut_counts[CentClassesPi0] = { 
          0.214974, 0.214974, // Centrality [0-10[ % 
          0.161329, 0.161329, // Centrality [10-20[ % 
          0.075140, 0.075140, // Centrality [20-30[ % 
          -0.062610, -0.062610, // Centrality [30-40[ % 
          -0.001081, -0.001081, // Centrality [40-50[ % 
          0.044697, 0.044697, // Centrality [50-60[ % 
          -0.030825, -0.030825, // Centrality [60-70[ % 
          0.013590, 0.013590, // Centrality [70-80[ % 
          0.045705, 0.013590, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.030825, 0.031318  // 60-80%(=60-70%) and Min.bias
};
double P1ERR_BasicCuts5x5Asym1ChiSq1ToF1Cut_counts[CentClassesPi0] = { 
          0.086606, 0.086606, // Centrality [0-10[ % 
          0.081045, 0.081045, // Centrality [10-20[ % 
          0.068307, 0.068307, // Centrality [20-30[ % 
          0.066949, 0.066949, // Centrality [30-40[ % 
          0.067698, 0.067698, // Centrality [40-50[ % 
          0.063651, 0.063651, // Centrality [50-60[ % 
          0.063241, 0.063241, // Centrality [60-70[ % 
          0.062256, 0.062256, // Centrality [70-80[ % 
          0.057085, 0.062256, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.063241, 0.026128  // 60-80%(=60-70%) and Min.bias
};
double P2_BasicCuts5x5Asym1ChiSq1ToF1Cut_counts[CentClassesPi0] = { 
          -0.004911, -0.004911, // Centrality [0-10[ % 
          -0.004371, -0.004371, // Centrality [10-20[ % 
          -0.002430, -0.002430, // Centrality [20-30[ % 
          0.001791, 0.001791, // Centrality [30-40[ % 
          0.000073, 0.000073, // Centrality [40-50[ % 
          -0.001549, -0.001549, // Centrality [50-60[ % 
          0.000534, 0.000534, // Centrality [60-70[ % 
          -0.000794, -0.000794, // Centrality [70-80[ % 
          -0.001670, -0.000794, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.000534, -0.001079  // 60-80%(=60-70%) and Min.bias
};
double P2ERR_BasicCuts5x5Asym1ChiSq1ToF1Cut_counts[CentClassesPi0] = { 
          0.002769, 0.002769, // Centrality [0-10[ % 
          0.002642, 0.002642, // Centrality [10-20[ % 
          0.002182, 0.002182, // Centrality [20-30[ % 
          0.002164, 0.002164, // Centrality [30-40[ % 
          0.002221, 0.002221, // Centrality [40-50[ % 
          0.002049, 0.002049, // Centrality [50-60[ % 
          0.002032, 0.002032, // Centrality [60-70[ % 
          0.002002, 0.002002, // Centrality [70-80[ % 
          0.001834, 0.002002, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.002032, 0.000850  // 60-80%(=60-70%) and Min.bias
};
double P3_BasicCuts5x5Asym1ChiSq1ToF1Cut_counts[CentClassesPi0] = { 
          0.004756, 0.004756, // Centrality [0-10[ % 
          0.014358, 0.014358, // Centrality [10-20[ % 
          0.040332, 0.040332, // Centrality [20-30[ % 
          0.160138, 0.160138, // Centrality [30-40[ % 
          0.109503, 0.109503, // Centrality [40-50[ % 
          0.052228, 0.052228, // Centrality [50-60[ % 
          0.097939, 0.097939, // Centrality [60-70[ % 
          0.076439, 0.076439, // Centrality [70-80[ % 
          0.045812, 0.076439, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.097939, 0.076268  // 60-80%(=60-70%) and Min.bias
};
double P3ERR_BasicCuts5x5Asym1ChiSq1ToF1Cut_counts[CentClassesPi0] = { 
          0.070479, 0.070479, // Centrality [0-10[ % 
          0.066334, 0.066334, // Centrality [10-20[ % 
          0.054614, 0.054614, // Centrality [20-30[ % 
          0.054270, 0.054270, // Centrality [30-40[ % 
          0.054901, 0.054901, // Centrality [40-50[ % 
          0.051105, 0.051105, // Centrality [50-60[ % 
          0.050544, 0.050544, // Centrality [60-70[ % 
          0.049875, 0.049875, // Centrality [70-80[ % 
          0.045601, 0.049875, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.050544, 0.021242  // 60-80%(=60-70%) and Min.bias
};
double P4_BasicCuts5x5Asym1ChiSq1ToF1Cut_counts[CentClassesPi0] = { 
          -0.257018, -0.257018, // Centrality [0-10[ % 
          -0.173439, -0.173439, // Centrality [10-20[ % 
          -0.084175, -0.084175, // Centrality [20-30[ % 
          0.027040, 0.027040, // Centrality [30-40[ % 
          -0.021538, -0.021538, // Centrality [40-50[ % 
          -0.047512, -0.047512, // Centrality [50-60[ % 
          0.021860, 0.021860, // Centrality [60-70[ % 
          -0.020424, -0.020424, // Centrality [70-80[ % 
          -0.045149, -0.020424, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.021860, -0.042813  // 60-80%(=60-70%) and Min.bias
};
double P4ERR_BasicCuts5x5Asym1ChiSq1ToF1Cut_counts[CentClassesPi0] = { 
          0.068424, 0.068424, // Centrality [0-10[ % 
          0.062730, 0.062730, // Centrality [10-20[ % 
          0.054441, 0.054441, // Centrality [20-30[ % 
          0.052574, 0.052574, // Centrality [30-40[ % 
          0.052616, 0.052616, // Centrality [40-50[ % 
          0.050364, 0.050364, // Centrality [50-60[ % 
          0.050221, 0.050221, // Centrality [60-70[ % 
          0.049350, 0.049350, // Centrality [70-80[ % 
          0.045358, 0.049350, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.050221, 0.020368  // 60-80%(=60-70%) and Min.bias
};
double P0_BasicCuts5x5Asym1ChiSq1ToF2Cut_counts[CentClassesPi0] = { 
          0.127622, 0.127622, // Centrality [0-10[ % 
          0.146814, 0.146814, // Centrality [10-20[ % 
          0.208657, 0.208657, // Centrality [20-30[ % 
          0.306772, 0.306772, // Centrality [30-40[ % 
          0.275830, 0.275830, // Centrality [40-50[ % 
          0.238647, 0.238647, // Centrality [50-60[ % 
          0.269307, 0.269307, // Centrality [60-70[ % 
          0.275694, 0.275694, // Centrality [70-80[ % 
          0.233969, 0.275694, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.269307, 0.238521  // 60-80%(=60-70%) and Min.bias
};
double P0ERR_BasicCuts5x5Asym1ChiSq1ToF2Cut_counts[CentClassesPi0] = { 
          0.074334, 0.074334, // Centrality [0-10[ % 
          0.070398, 0.070398, // Centrality [10-20[ % 
          0.060646, 0.060646, // Centrality [20-30[ % 
          0.059705, 0.059705, // Centrality [30-40[ % 
          0.060257, 0.060257, // Centrality [40-50[ % 
          0.056879, 0.056879, // Centrality [50-60[ % 
          0.056492, 0.056492, // Centrality [60-70[ % 
          0.055856, 0.055856, // Centrality [70-80[ % 
          0.051114, 0.055856, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.056492, 0.023010  // 60-80%(=60-70%) and Min.bias
};
double P1_BasicCuts5x5Asym1ChiSq1ToF2Cut_counts[CentClassesPi0] = { 
          0.186441, 0.186441, // Centrality [0-10[ % 
          0.136000, 0.136000, // Centrality [10-20[ % 
          0.075829, 0.075829, // Centrality [20-30[ % 
          -0.050525, -0.050525, // Centrality [30-40[ % 
          -0.021547, -0.021547, // Centrality [40-50[ % 
          0.029420, 0.029420, // Centrality [50-60[ % 
          -0.001218, -0.001218, // Centrality [60-70[ % 
          -0.015682, -0.015682, // Centrality [70-80[ % 
          0.038489, -0.015682, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.001218, 0.026596  // 60-80%(=60-70%) and Min.bias
};
double P1ERR_BasicCuts5x5Asym1ChiSq1ToF2Cut_counts[CentClassesPi0] = { 
          0.080228, 0.080228, // Centrality [0-10[ % 
          0.075691, 0.075691, // Centrality [10-20[ % 
          0.065101, 0.065101, // Centrality [20-30[ % 
          0.064015, 0.064015, // Centrality [30-40[ % 
          0.064503, 0.064503, // Centrality [40-50[ % 
          0.060900, 0.060900, // Centrality [50-60[ % 
          0.060405, 0.060405, // Centrality [60-70[ % 
          0.059692, 0.059692, // Centrality [70-80[ % 
          0.054641, 0.059692, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.060405, 0.024649  // 60-80%(=60-70%) and Min.bias
};
double P2_BasicCuts5x5Asym1ChiSq1ToF2Cut_counts[CentClassesPi0] = { 
          -0.004360, -0.004360, // Centrality [0-10[ % 
          -0.003642, -0.003642, // Centrality [10-20[ % 
          -0.002314, -0.002314, // Centrality [20-30[ % 
          0.001482, 0.001482, // Centrality [30-40[ % 
          0.000595, 0.000595, // Centrality [40-50[ % 
          -0.001126, -0.001126, // Centrality [50-60[ % 
          -0.000266, -0.000266, // Centrality [60-70[ % 
          0.000094, 0.000094, // Centrality [70-80[ % 
          -0.001356, 0.000094, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.000266, -0.000926  // 60-80%(=60-70%) and Min.bias
};
double P2ERR_BasicCuts5x5Asym1ChiSq1ToF2Cut_counts[CentClassesPi0] = { 
          0.002591, 0.002591, // Centrality [0-10[ % 
          0.002493, 0.002493, // Centrality [10-20[ % 
          0.002093, 0.002093, // Centrality [20-30[ % 
          0.002083, 0.002083, // Centrality [30-40[ % 
          0.002130, 0.002130, // Centrality [40-50[ % 
          0.001970, 0.001970, // Centrality [50-60[ % 
          0.001952, 0.001952, // Centrality [60-70[ % 
          0.001929, 0.001929, // Centrality [70-80[ % 
          0.001764, 0.001929, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.001952, 0.000808  // 60-80%(=60-70%) and Min.bias
};
double P3_BasicCuts5x5Asym1ChiSq1ToF2Cut_counts[CentClassesPi0] = { 
          0.056364, 0.056364, // Centrality [0-10[ % 
          0.065205, 0.065205, // Centrality [10-20[ % 
          0.079488, 0.079488, // Centrality [20-30[ % 
          0.183650, 0.183650, // Centrality [30-40[ % 
          0.151350, 0.151350, // Centrality [40-50[ % 
          0.090914, 0.090914, // Centrality [50-60[ % 
          0.110928, 0.110928, // Centrality [60-70[ % 
          0.122925, 0.122925, // Centrality [70-80[ % 
          0.083010, 0.122925, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.110928, 0.109649  // 60-80%(=60-70%) and Min.bias
};
double P3ERR_BasicCuts5x5Asym1ChiSq1ToF2Cut_counts[CentClassesPi0] = { 
          0.065793, 0.065793, // Centrality [0-10[ % 
          0.062453, 0.062453, // Centrality [10-20[ % 
          0.052605, 0.052605, // Centrality [20-30[ % 
          0.052441, 0.052441, // Centrality [30-40[ % 
          0.052744, 0.052744, // Centrality [40-50[ % 
          0.049254, 0.049254, // Centrality [50-60[ % 
          0.048788, 0.048788, // Centrality [60-70[ % 
          0.048149, 0.048149, // Centrality [70-80[ % 
          0.044040, 0.048149, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.048788, 0.020237  // 60-80%(=60-70%) and Min.bias
};
double P4_BasicCuts5x5Asym1ChiSq1ToF2Cut_counts[CentClassesPi0] = { 
          -0.232110, -0.232110, // Centrality [0-10[ % 
          -0.159000, -0.159000, // Centrality [10-20[ % 
          -0.097422, -0.097422, // Centrality [20-30[ % 
          0.007775, 0.007775, // Centrality [30-40[ % 
          -0.009878, -0.009878, // Centrality [40-50[ % 
          -0.040954, -0.040954, // Centrality [50-60[ % 
          -0.014086, -0.014086, // Centrality [60-70[ % 
          -0.002149, -0.002149, // Centrality [70-80[ % 
          -0.050133, -0.002149, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.014086, -0.046536  // 60-80%(=60-70%) and Min.bias
};
double P4ERR_BasicCuts5x5Asym1ChiSq1ToF2Cut_counts[CentClassesPi0] = { 
          0.062746, 0.062746, // Centrality [0-10[ % 
          0.057952, 0.057952, // Centrality [10-20[ % 
          0.051373, 0.051373, // Centrality [20-30[ % 
          0.049738, 0.049738, // Centrality [30-40[ % 
          0.049680, 0.049680, // Centrality [40-50[ % 
          0.047831, 0.047831, // Centrality [50-60[ % 
          0.047499, 0.047499, // Centrality [60-70[ % 
          0.046984, 0.046984, // Centrality [70-80[ % 
          0.043057, 0.046984, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.047499, 0.019006  // 60-80%(=60-70%) and Min.bias
};
double P0_BasicCuts5x5Asym2ChiSq1RToFCut_counts[CentClassesPi0] = { 
          0.102540, 0.102540, // Centrality [0-10[ % 
          0.041415, 0.041415, // Centrality [10-20[ % 
          0.075548, 0.075548, // Centrality [20-30[ % 
          0.093834, 0.093834, // Centrality [30-40[ % 
          0.108614, 0.108614, // Centrality [40-50[ % 
          0.100160, 0.100160, // Centrality [50-60[ % 
          0.062008, 0.062008, // Centrality [60-70[ % 
          0.086296, 0.086296, // Centrality [70-80[ % 
          0.076126, 0.086296, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.062008, 0.086929  // 60-80%(=60-70%) and Min.bias
};
double P0ERR_BasicCuts5x5Asym2ChiSq1RToFCut_counts[CentClassesPi0] = { 
          0.038013, 0.038013, // Centrality [0-10[ % 
          0.033104, 0.033104, // Centrality [10-20[ % 
          0.031548, 0.031548, // Centrality [20-30[ % 
          0.030902, 0.030902, // Centrality [30-40[ % 
          0.032677, 0.032677, // Centrality [40-50[ % 
          0.029820, 0.029820, // Centrality [50-60[ % 
          0.029360, 0.029360, // Centrality [60-70[ % 
          0.028778, 0.028778, // Centrality [70-80[ % 
          0.026425, 0.028778, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.029360, 0.010698  // 60-80%(=60-70%) and Min.bias
};
double P1_BasicCuts5x5Asym2ChiSq1RToFCut_counts[CentClassesPi0] = { 
          -0.003230, -0.003230, // Centrality [0-10[ % 
          0.048598, 0.048598, // Centrality [10-20[ % 
          0.014192, 0.014192, // Centrality [20-30[ % 
          -0.013062, -0.013062, // Centrality [30-40[ % 
          -0.027693, -0.027693, // Centrality [40-50[ % 
          -0.013591, -0.013591, // Centrality [50-60[ % 
          0.024716, 0.024716, // Centrality [60-70[ % 
          -0.005142, -0.005142, // Centrality [70-80[ % 
          0.009294, -0.005142, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.024716, -0.002790  // 60-80%(=60-70%) and Min.bias
};
double P1ERR_BasicCuts5x5Asym2ChiSq1RToFCut_counts[CentClassesPi0] = { 
          0.040570, 0.040570, // Centrality [0-10[ % 
          0.035385, 0.035385, // Centrality [10-20[ % 
          0.033582, 0.033582, // Centrality [20-30[ % 
          0.032928, 0.032928, // Centrality [30-40[ % 
          0.034649, 0.034649, // Centrality [40-50[ % 
          0.031579, 0.031579, // Centrality [50-60[ % 
          0.031131, 0.031131, // Centrality [60-70[ % 
          0.030443, 0.030443, // Centrality [70-80[ % 
          0.027965, 0.030443, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.031131, 0.011364  // 60-80%(=60-70%) and Min.bias
};
double P2_BasicCuts5x5Asym2ChiSq1RToFCut_counts[CentClassesPi0] = { 
          0.000006, 0.000006, // Centrality [0-10[ % 
          -0.001522, -0.001522, // Centrality [10-20[ % 
          -0.000650, -0.000650, // Centrality [20-30[ % 
          0.000163, 0.000163, // Centrality [30-40[ % 
          0.000589, 0.000589, // Centrality [40-50[ % 
          0.000090, 0.000090, // Centrality [50-60[ % 
          -0.001120, -0.001120, // Centrality [60-70[ % 
          -0.000118, -0.000118, // Centrality [70-80[ % 
          -0.000503, -0.000118, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.001120, -0.000183  // 60-80%(=60-70%) and Min.bias
};
double P2ERR_BasicCuts5x5Asym2ChiSq1RToFCut_counts[CentClassesPi0] = { 
          0.001301, 0.001301, // Centrality [0-10[ % 
          0.001138, 0.001138, // Centrality [10-20[ % 
          0.001070, 0.001070, // Centrality [20-30[ % 
          0.001065, 0.001065, // Centrality [30-40[ % 
          0.001145, 0.001145, // Centrality [40-50[ % 
          0.001010, 0.001010, // Centrality [50-60[ % 
          0.000995, 0.000995, // Centrality [60-70[ % 
          0.000973, 0.000973, // Centrality [70-80[ % 
          0.000894, 0.000973, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.000995, 0.000370  // 60-80%(=60-70%) and Min.bias
};
double P3_BasicCuts5x5Asym2ChiSq1RToFCut_counts[CentClassesPi0] = { 
          0.006900, 0.006900, // Centrality [0-10[ % 
          -0.020151, -0.020151, // Centrality [10-20[ % 
          -0.007551, -0.007551, // Centrality [20-30[ % 
          0.014902, 0.014902, // Centrality [30-40[ % 
          0.023907, 0.023907, // Centrality [40-50[ % 
          0.001725, 0.001725, // Centrality [50-60[ % 
          -0.021901, -0.021901, // Centrality [60-70[ % 
          0.003450, 0.003450, // Centrality [70-80[ % 
          -0.006056, 0.003450, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.021901, 0.003693  // 60-80%(=60-70%) and Min.bias
};
double P3ERR_BasicCuts5x5Asym2ChiSq1RToFCut_counts[CentClassesPi0] = { 
          0.032655, 0.032655, // Centrality [0-10[ % 
          0.028512, 0.028512, // Centrality [10-20[ % 
          0.026699, 0.026699, // Centrality [20-30[ % 
          0.026454, 0.026454, // Centrality [30-40[ % 
          0.027881, 0.027881, // Centrality [40-50[ % 
          0.024920, 0.024920, // Centrality [50-60[ % 
          0.024593, 0.024593, // Centrality [60-70[ % 
          0.023977, 0.023977, // Centrality [70-80[ % 
          0.022028, 0.023977, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.024593, 0.009205  // 60-80%(=60-70%) and Min.bias
};
double P4_BasicCuts5x5Asym2ChiSq1RToFCut_counts[CentClassesPi0] = { 
          0.005051, 0.005051, // Centrality [0-10[ % 
          -0.041108, -0.041108, // Centrality [10-20[ % 
          -0.006021, -0.006021, // Centrality [20-30[ % 
          0.017075, 0.017075, // Centrality [30-40[ % 
          0.029790, 0.029790, // Centrality [40-50[ % 
          0.024308, 0.024308, // Centrality [50-60[ % 
          -0.008615, -0.008615, // Centrality [60-70[ % 
          0.013481, 0.013481, // Centrality [70-80[ % 
          -0.000370, 0.013481, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.008615, 0.010494  // 60-80%(=60-70%) and Min.bias
};
double P4ERR_BasicCuts5x5Asym2ChiSq1RToFCut_counts[CentClassesPi0] = { 
          0.032235, 0.032235, // Centrality [0-10[ % 
          0.027957, 0.027957, // Centrality [10-20[ % 
          0.026898, 0.026898, // Centrality [20-30[ % 
          0.025963, 0.025963, // Centrality [30-40[ % 
          0.026940, 0.026940, // Centrality [40-50[ % 
          0.025328, 0.025328, // Centrality [50-60[ % 
          0.024962, 0.024962, // Centrality [60-70[ % 
          0.024454, 0.024454, // Centrality [70-80[ % 
          0.022472, 0.024454, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.024962, 0.008908  // 60-80%(=60-70%) and Min.bias
};
double P0_ChiSq2Cut_counts[CentClassesPi0] = { 
          0.466167, 0.466167, // Centrality [0-10[ % 
          0.553939, 0.553939, // Centrality [10-20[ % 
          0.681816, 0.681816, // Centrality [20-30[ % 
          0.862086, 0.862086, // Centrality [30-40[ % 
          0.825633, 0.825633, // Centrality [40-50[ % 
          0.914228, 0.914228, // Centrality [50-60[ % 
          0.992036, 0.992036, // Centrality [60-70[ % 
          0.945457, 0.945457, // Centrality [70-80[ % 
          0.963966, 0.945457, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.992036, 0.807697  // 60-80%(=60-70%) and Min.bias
};
double P0ERR_ChiSq2Cut_counts[CentClassesPi0] = { 
          0.119930, 0.119930, // Centrality [0-10[ % 
          0.120605, 0.120605, // Centrality [10-20[ % 
          0.107000, 0.107000, // Centrality [20-30[ % 
          0.106059, 0.106059, // Centrality [30-40[ % 
          0.105934, 0.105934, // Centrality [40-50[ % 
          0.102831, 0.102831, // Centrality [50-60[ % 
          0.102975, 0.102975, // Centrality [60-70[ % 
          0.102348, 0.102348, // Centrality [70-80[ % 
          0.093274, 0.102348, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.102975, 0.043749  // 60-80%(=60-70%) and Min.bias
};
double P1_ChiSq2Cut_counts[CentClassesPi0] = { 
          0.334389, 0.334389, // Centrality [0-10[ % 
          0.199354, 0.199354, // Centrality [10-20[ % 
          0.084282, 0.084282, // Centrality [20-30[ % 
          -0.135695, -0.135695, // Centrality [30-40[ % 
          -0.091897, -0.091897, // Centrality [40-50[ % 
          -0.149506, -0.149506, // Centrality [50-60[ % 
          -0.218510, -0.218510, // Centrality [60-70[ % 
          -0.179296, -0.179296, // Centrality [70-80[ % 
          -0.190864, -0.179296, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.218510, -0.062791  // 60-80%(=60-70%) and Min.bias
};
double P1ERR_ChiSq2Cut_counts[CentClassesPi0] = { 
          0.128252, 0.128252, // Centrality [0-10[ % 
          0.128386, 0.128386, // Centrality [10-20[ % 
          0.114003, 0.114003, // Centrality [20-30[ % 
          0.112801, 0.112801, // Centrality [30-40[ % 
          0.112626, 0.112626, // Centrality [40-50[ % 
          0.109164, 0.109164, // Centrality [50-60[ % 
          0.109112, 0.109112, // Centrality [60-70[ % 
          0.108637, 0.108637, // Centrality [70-80[ % 
          0.098922, 0.108637, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.109112, 0.046425  // 60-80%(=60-70%) and Min.bias
};
double P2_ChiSq2Cut_counts[CentClassesPi0] = { 
          -0.009601, -0.009601, // Centrality [0-10[ % 
          -0.007376, -0.007376, // Centrality [10-20[ % 
          -0.003703, -0.003703, // Centrality [20-30[ % 
          0.002450, 0.002450, // Centrality [30-40[ % 
          0.001726, 0.001726, // Centrality [40-50[ % 
          0.003405, 0.003405, // Centrality [50-60[ % 
          0.004706, 0.004706, // Centrality [60-70[ % 
          0.003783, 0.003783, // Centrality [70-80[ % 
          0.004008, 0.003783, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.004706, 0.000336  // 60-80%(=60-70%) and Min.bias
};
double P2ERR_ChiSq2Cut_counts[CentClassesPi0] = { 
          0.004037, 0.004037, // Centrality [0-10[ % 
          0.004166, 0.004166, // Centrality [10-20[ % 
          0.003623, 0.003623, // Centrality [20-30[ % 
          0.003608, 0.003608, // Centrality [30-40[ % 
          0.003649, 0.003649, // Centrality [40-50[ % 
          0.003490, 0.003490, // Centrality [50-60[ % 
          0.003483, 0.003483, // Centrality [60-70[ % 
          0.003471, 0.003471, // Centrality [70-80[ % 
          0.003158, 0.003471, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.003483, 0.001497  // 60-80%(=60-70%) and Min.bias
};
double P3_ChiSq2Cut_counts[CentClassesPi0] = { 
          -0.266867, -0.266867, // Centrality [0-10[ % 
          -0.164922, -0.164922, // Centrality [10-20[ % 
          -0.051800, -0.051800, // Centrality [20-30[ % 
          0.132926, 0.132926, // Centrality [30-40[ % 
          0.129240, 0.129240, // Centrality [40-50[ % 
          0.150373, 0.150373, // Centrality [50-60[ % 
          0.173991, 0.173991, // Centrality [60-70[ % 
          0.183970, 0.183970, // Centrality [70-80[ % 
          0.182525, 0.183970, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.173991, 0.070820  // 60-80%(=60-70%) and Min.bias
};
double P3ERR_ChiSq2Cut_counts[CentClassesPi0] = { 
          0.101266, 0.101266, // Centrality [0-10[ % 
          0.103583, 0.103583, // Centrality [10-20[ % 
          0.089956, 0.089956, // Centrality [20-30[ % 
          0.089439, 0.089439, // Centrality [30-40[ % 
          0.090085, 0.090085, // Centrality [40-50[ % 
          0.086411, 0.086411, // Centrality [50-60[ % 
          0.086014, 0.086014, // Centrality [60-70[ % 
          0.085966, 0.085966, // Centrality [70-80[ % 
          0.078128, 0.085966, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.086014, 0.036992  // 60-80%(=60-70%) and Min.bias
};
double P4_ChiSq2Cut_counts[CentClassesPi0] = { 
          -0.270801, -0.270801, // Centrality [0-10[ % 
          -0.109844, -0.109844, // Centrality [10-20[ % 
          -0.039381, -0.039381, // Centrality [20-30[ % 
          0.155491, 0.155491, // Centrality [30-40[ % 
          0.099943, 0.099943, // Centrality [40-50[ % 
          0.156015, 0.156015, // Centrality [50-60[ % 
          0.237140, 0.237140, // Centrality [60-70[ % 
          0.183250, 0.183250, // Centrality [70-80[ % 
          0.200671, 0.183250, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.237140, 0.097181  // 60-80%(=60-70%) and Min.bias
};
double P4ERR_ChiSq2Cut_counts[CentClassesPi0] = { 
          0.104154, 0.104154, // Centrality [0-10[ % 
          0.100431, 0.100431, // Centrality [10-20[ % 
          0.091813, 0.091813, // Centrality [20-30[ % 
          0.090372, 0.090372, // Centrality [30-40[ % 
          0.088940, 0.088940, // Centrality [40-50[ % 
          0.087426, 0.087426, // Centrality [50-60[ % 
          0.087648, 0.087648, // Centrality [60-70[ % 
          0.087063, 0.087063, // Centrality [70-80[ % 
          0.079383, 0.087063, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.087648, 0.036865  // 60-80%(=60-70%) and Min.bias
};
double P0_BasicCuts5x5Asym1ChiSq2Cut_counts[CentClassesPi0] = { 
          0.019486, 0.019486, // Centrality [0-10[ % 
          0.094158, 0.094158, // Centrality [10-20[ % 
          0.190006, 0.190006, // Centrality [20-30[ % 
          0.277496, 0.277496, // Centrality [30-40[ % 
          0.231293, 0.231293, // Centrality [40-50[ % 
          0.198923, 0.198923, // Centrality [50-60[ % 
          0.282511, 0.282511, // Centrality [60-70[ % 
          0.255158, 0.255158, // Centrality [70-80[ % 
          0.243201, 0.255158, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.282511, 0.209835  // 60-80%(=60-70%) and Min.bias
};
double P0ERR_BasicCuts5x5Asym1ChiSq2Cut_counts[CentClassesPi0] = { 
          0.066932, 0.066932, // Centrality [0-10[ % 
          0.065601, 0.065601, // Centrality [10-20[ % 
          0.058590, 0.058590, // Centrality [20-30[ % 
          0.059034, 0.059034, // Centrality [30-40[ % 
          0.061073, 0.061073, // Centrality [40-50[ % 
          0.058212, 0.058212, // Centrality [50-60[ % 
          0.058506, 0.058506, // Centrality [60-70[ % 
          0.057716, 0.057716, // Centrality [70-80[ % 
          0.053149, 0.057716, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.058506, 0.022537  // 60-80%(=60-70%) and Min.bias
};
double P1_BasicCuts5x5Asym1ChiSq2Cut_counts[CentClassesPi0] = { 
          0.301287, 0.301287, // Centrality [0-10[ % 
          0.202217, 0.202217, // Centrality [10-20[ % 
          0.111331, 0.111331, // Centrality [20-30[ % 
          0.009298, 0.009298, // Centrality [30-40[ % 
          0.056926, 0.056926, // Centrality [40-50[ % 
          0.108760, 0.108760, // Centrality [50-60[ % 
          0.029928, 0.029928, // Centrality [60-70[ % 
          0.044478, 0.044478, // Centrality [70-80[ % 
          0.072717, 0.044478, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.029928, 0.086050  // 60-80%(=60-70%) and Min.bias
};
double P1ERR_BasicCuts5x5Asym1ChiSq2Cut_counts[CentClassesPi0] = { 
          0.071662, 0.071662, // Centrality [0-10[ % 
          0.070115, 0.070115, // Centrality [10-20[ % 
          0.062450, 0.062450, // Centrality [20-30[ % 
          0.062976, 0.062976, // Centrality [30-40[ % 
          0.065119, 0.065119, // Centrality [40-50[ % 
          0.062094, 0.062094, // Centrality [50-60[ % 
          0.062191, 0.062191, // Centrality [60-70[ % 
          0.061487, 0.061487, // Centrality [70-80[ % 
          0.056519, 0.061487, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.062191, 0.024013  // 60-80%(=60-70%) and Min.bias
};
double P2_BasicCuts5x5Asym1ChiSq2Cut_counts[CentClassesPi0] = { 
          -0.008946, -0.008946, // Centrality [0-10[ % 
          -0.006334, -0.006334, // Centrality [10-20[ % 
          -0.003994, -0.003994, // Centrality [20-30[ % 
          -0.000943, -0.000943, // Centrality [30-40[ % 
          -0.001992, -0.001992, // Centrality [40-50[ % 
          -0.003747, -0.003747, // Centrality [50-60[ % 
          -0.001665, -0.001665, // Centrality [60-70[ % 
          -0.002039, -0.002039, // Centrality [70-80[ % 
          -0.002896, -0.002039, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.001665, -0.003143  // 60-80%(=60-70%) and Min.bias
};
double P2ERR_BasicCuts5x5Asym1ChiSq2Cut_counts[CentClassesPi0] = { 
          0.002273, 0.002273, // Centrality [0-10[ % 
          0.002264, 0.002264, // Centrality [10-20[ % 
          0.001991, 0.001991, // Centrality [20-30[ % 
          0.002028, 0.002028, // Centrality [30-40[ % 
          0.002137, 0.002137, // Centrality [40-50[ % 
          0.001999, 0.001999, // Centrality [50-60[ % 
          0.001996, 0.001996, // Centrality [60-70[ % 
          0.001976, 0.001976, // Centrality [70-80[ % 
          0.001814, 0.001976, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.001996, 0.000780  // 60-80%(=60-70%) and Min.bias
};
double P3_BasicCuts5x5Asym1ChiSq2Cut_counts[CentClassesPi0] = { 
          -0.214116, -0.214116, // Centrality [0-10[ % 
          -0.115346, -0.115346, // Centrality [10-20[ % 
          -0.058404, -0.058404, // Centrality [20-30[ % 
          0.040999, 0.040999, // Centrality [30-40[ % 
          0.017804, 0.017804, // Centrality [40-50[ % 
          -0.036666, -0.036666, // Centrality [50-60[ % 
          0.011204, 0.011204, // Centrality [60-70[ % 
          0.021679, 0.021679, // Centrality [70-80[ % 
          -0.012132, 0.021679, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.011204, -0.022408  // 60-80%(=60-70%) and Min.bias
};
double P3ERR_BasicCuts5x5Asym1ChiSq2Cut_counts[CentClassesPi0] = { 
          0.057368, 0.057368, // Centrality [0-10[ % 
          0.056595, 0.056595, // Centrality [10-20[ % 
          0.049598, 0.049598, // Centrality [20-30[ % 
          0.050567, 0.050567, // Centrality [30-40[ % 
          0.052660, 0.052660, // Centrality [40-50[ % 
          0.049704, 0.049704, // Centrality [50-60[ % 
          0.049488, 0.049488, // Centrality [60-70[ % 
          0.049111, 0.049111, // Centrality [70-80[ % 
          0.044991, 0.049111, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.049488, 0.019419  // 60-80%(=60-70%) and Min.bias
};
double P4_BasicCuts5x5Asym1ChiSq2Cut_counts[CentClassesPi0] = { 
          -0.247575, -0.247575, // Centrality [0-10[ % 
          -0.162856, -0.162856, // Centrality [10-20[ % 
          -0.078524, -0.078524, // Centrality [20-30[ % 
          0.001676, 0.001676, // Centrality [30-40[ % 
          -0.044831, -0.044831, // Centrality [40-50[ % 
          -0.078562, -0.078562, // Centrality [50-60[ % 
          -0.004883, -0.004883, // Centrality [60-70[ % 
          -0.026470, -0.026470, // Centrality [70-80[ % 
          -0.043996, -0.026470, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.004883, -0.059628  // 60-80%(=60-70%) and Min.bias
};
double P4ERR_BasicCuts5x5Asym1ChiSq2Cut_counts[CentClassesPi0] = { 
          0.057419, 0.057419, // Centrality [0-10[ % 
          0.055042, 0.055042, // Centrality [10-20[ % 
          0.050016, 0.050016, // Centrality [20-30[ % 
          0.049799, 0.049799, // Centrality [30-40[ % 
          0.050677, 0.050677, // Centrality [40-50[ % 
          0.049211, 0.049211, // Centrality [50-60[ % 
          0.049540, 0.049540, // Centrality [60-70[ % 
          0.048831, 0.048831, // Centrality [70-80[ % 
          0.045007, 0.048831, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.049540, 0.018820  // 60-80%(=60-70%) and Min.bias
};
double P0_BasicCuts5x5Asym1ChiSq2ToF1Cut_counts[CentClassesPi0] = { 
          -0.004462, -0.004462, // Centrality [0-10[ % 
          0.085024, 0.085024, // Centrality [10-20[ % 
          0.166847, 0.166847, // Centrality [20-30[ % 
          0.281950, 0.281950, // Centrality [30-40[ % 
          0.230129, 0.230129, // Centrality [40-50[ % 
          0.206363, 0.206363, // Centrality [50-60[ % 
          0.276806, 0.276806, // Centrality [60-70[ % 
          0.254421, 0.254421, // Centrality [70-80[ % 
          0.223087, 0.254421, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.276806, 0.202058  // 60-80%(=60-70%) and Min.bias
};
double P0ERR_BasicCuts5x5Asym1ChiSq2ToF1Cut_counts[CentClassesPi0] = { 
          0.065209, 0.065209, // Centrality [0-10[ % 
          0.064291, 0.064291, // Centrality [10-20[ % 
          0.057463, 0.057463, // Centrality [20-30[ % 
          0.058095, 0.058095, // Centrality [30-40[ % 
          0.060307, 0.060307, // Centrality [40-50[ % 
          0.057280, 0.057280, // Centrality [50-60[ % 
          0.057598, 0.057598, // Centrality [60-70[ % 
          0.056838, 0.056838, // Centrality [70-80[ % 
          0.052433, 0.056838, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.057598, 0.022590  // 60-80%(=60-70%) and Min.bias
};
double P1_BasicCuts5x5Asym1ChiSq2ToF1Cut_counts[CentClassesPi0] = { 
          0.298579, 0.298579, // Centrality [0-10[ % 
          0.190467, 0.190467, // Centrality [10-20[ % 
          0.118037, 0.118037, // Centrality [20-30[ % 
          -0.009103, -0.009103, // Centrality [30-40[ % 
          0.046730, 0.046730, // Centrality [40-50[ % 
          0.088844, 0.088844, // Centrality [50-60[ % 
          0.021512, 0.021512, // Centrality [60-70[ % 
          0.034056, 0.034056, // Centrality [70-80[ % 
          0.081768, 0.034056, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.021512, 0.079823  // 60-80%(=60-70%) and Min.bias
};
double P1ERR_BasicCuts5x5Asym1ChiSq2ToF1Cut_counts[CentClassesPi0] = { 
          0.069997, 0.069997, // Centrality [0-10[ % 
          0.068836, 0.068836, // Centrality [10-20[ % 
          0.061366, 0.061366, // Centrality [20-30[ % 
          0.062035, 0.062035, // Centrality [30-40[ % 
          0.064351, 0.064351, // Centrality [40-50[ % 
          0.061161, 0.061161, // Centrality [50-60[ % 
          0.061328, 0.061328, // Centrality [60-70[ % 
          0.060627, 0.060627, // Centrality [70-80[ % 
          0.055843, 0.060627, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.061328, 0.024107  // 60-80%(=60-70%) and Min.bias
};
double P2_BasicCuts5x5Asym1ChiSq2ToF1Cut_counts[CentClassesPi0] = { 
          -0.008527, -0.008527, // Centrality [0-10[ % 
          -0.005792, -0.005792, // Centrality [10-20[ % 
          -0.003967, -0.003967, // Centrality [20-30[ % 
          -0.000136, -0.000136, // Centrality [30-40[ % 
          -0.001518, -0.001518, // Centrality [40-50[ % 
          -0.002950, -0.002950, // Centrality [50-60[ % 
          -0.001096, -0.001096, // Centrality [60-70[ % 
          -0.001502, -0.001502, // Centrality [70-80[ % 
          -0.002927, -0.001502, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.001096, -0.002719  // 60-80%(=60-70%) and Min.bias
};
double P2ERR_BasicCuts5x5Asym1ChiSq2ToF1Cut_counts[CentClassesPi0] = { 
          0.002227, 0.002227, // Centrality [0-10[ % 
          0.002228, 0.002228, // Centrality [10-20[ % 
          0.001960, 0.001960, // Centrality [20-30[ % 
          0.002000, 0.002000, // Centrality [30-40[ % 
          0.002113, 0.002113, // Centrality [40-50[ % 
          0.001969, 0.001969, // Centrality [50-60[ % 
          0.001970, 0.001970, // Centrality [60-70[ % 
          0.001950, 0.001950, // Centrality [70-80[ % 
          0.001794, 0.001950, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.001970, 0.000784  // 60-80%(=60-70%) and Min.bias
};
double P3_BasicCuts5x5Asym1ChiSq2ToF1Cut_counts[CentClassesPi0] = { 
          -0.174355, -0.174355, // Centrality [0-10[ % 
          -0.080423, -0.080423, // Centrality [10-20[ % 
          -0.039299, -0.039299, // Centrality [20-30[ % 
          0.075079, 0.075079, // Centrality [30-40[ % 
          0.041650, 0.041650, // Centrality [40-50[ % 
          -0.001688, -0.001688, // Centrality [50-60[ % 
          0.043320, 0.043320, // Centrality [60-70[ % 
          0.048343, 0.048343, // Centrality [70-80[ % 
          0.003130, 0.048343, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.043320, 0.003984  // 60-80%(=60-70%) and Min.bias
};
double P3ERR_BasicCuts5x5Asym1ChiSq2ToF1Cut_counts[CentClassesPi0] = { 
          0.056383, 0.056383, // Centrality [0-10[ % 
          0.055835, 0.055835, // Centrality [10-20[ % 
          0.048944, 0.048944, // Centrality [20-30[ % 
          0.049945, 0.049945, // Centrality [30-40[ % 
          0.052129, 0.052129, // Centrality [40-50[ % 
          0.049043, 0.049043, // Centrality [50-60[ % 
          0.048968, 0.048968, // Centrality [60-70[ % 
          0.048540, 0.048540, // Centrality [70-80[ % 
          0.044573, 0.048540, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.048968, 0.019550  // 60-80%(=60-70%) and Min.bias
};
double P4_BasicCuts5x5Asym1ChiSq2ToF1Cut_counts[CentClassesPi0] = { 
          -0.264298, -0.264298, // Centrality [0-10[ % 
          -0.165544, -0.165544, // Centrality [10-20[ % 
          -0.096853, -0.096853, // Centrality [20-30[ % 
          0.004188, 0.004188, // Centrality [30-40[ % 
          -0.046520, -0.046520, // Centrality [40-50[ % 
          -0.074762, -0.074762, // Centrality [50-60[ % 
          -0.014059, -0.014059, // Centrality [60-70[ % 
          -0.030058, -0.030058, // Centrality [70-80[ % 
          -0.065210, -0.030058, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.014059, -0.067327  // 60-80%(=60-70%) and Min.bias
};
double P4ERR_BasicCuts5x5Asym1ChiSq2ToF1Cut_counts[CentClassesPi0] = { 
          0.055786, 0.055786, // Centrality [0-10[ % 
          0.053795, 0.053795, // Centrality [10-20[ % 
          0.048979, 0.048979, // Centrality [20-30[ % 
          0.048945, 0.048945, // Centrality [30-40[ % 
          0.050006, 0.050006, // Centrality [40-50[ % 
          0.048424, 0.048424, // Centrality [50-60[ % 
          0.048727, 0.048727, // Centrality [60-70[ % 
          0.048070, 0.048070, // Centrality [70-80[ % 
          0.044384, 0.048070, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.048727, 0.018822  // 60-80%(=60-70%) and Min.bias
};
double P0_BasicCuts5x5Asym1ChiSq2ToF2Cut_counts[CentClassesPi0] = { 
          -0.036881, -0.036881, // Centrality [0-10[ % 
          0.058628, 0.058628, // Centrality [10-20[ % 
          0.132755, 0.132755, // Centrality [20-30[ % 
          0.231335, 0.231335, // Centrality [30-40[ % 
          0.207074, 0.207074, // Centrality [40-50[ % 
          0.186712, 0.186712, // Centrality [50-60[ % 
          0.210140, 0.210140, // Centrality [60-70[ % 
          0.243139, 0.243139, // Centrality [70-80[ % 
          0.192152, 0.243139, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.210140, 0.166760  // 60-80%(=60-70%) and Min.bias
};
double P0ERR_BasicCuts5x5Asym1ChiSq2ToF2Cut_counts[CentClassesPi0] = { 
          0.060261, 0.060261, // Centrality [0-10[ % 
          0.059850, 0.059850, // Centrality [10-20[ % 
          0.054755, 0.054755, // Centrality [20-30[ % 
          0.055308, 0.055308, // Centrality [30-40[ % 
          0.057378, 0.057378, // Centrality [40-50[ % 
          0.054684, 0.054684, // Centrality [50-60[ % 
          0.054821, 0.054821, // Centrality [60-70[ % 
          0.054366, 0.054366, // Centrality [70-80[ % 
          0.050015, 0.054366, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.054821, 0.021245  // 60-80%(=60-70%) and Min.bias
};
double P1_BasicCuts5x5Asym1ChiSq2ToF2Cut_counts[CentClassesPi0] = { 
          0.281893, 0.281893, // Centrality [0-10[ % 
          0.172963, 0.172963, // Centrality [10-20[ % 
          0.109991, 0.109991, // Centrality [20-30[ % 
          0.001145, 0.001145, // Centrality [30-40[ % 
          0.028686, 0.028686, // Centrality [40-50[ % 
          0.066239, 0.066239, // Centrality [50-60[ % 
          0.046023, 0.046023, // Centrality [60-70[ % 
          0.004088, 0.004088, // Centrality [70-80[ % 
          0.071654, 0.004088, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.046023, 0.074171  // 60-80%(=60-70%) and Min.bias
};
double P1ERR_BasicCuts5x5Asym1ChiSq2ToF2Cut_counts[CentClassesPi0] = { 
          0.064912, 0.064912, // Centrality [0-10[ % 
          0.064232, 0.064232, // Centrality [10-20[ % 
          0.058669, 0.058669, // Centrality [20-30[ % 
          0.059259, 0.059259, // Centrality [30-40[ % 
          0.061353, 0.061353, // Centrality [40-50[ % 
          0.058490, 0.058490, // Centrality [50-60[ % 
          0.058606, 0.058606, // Centrality [60-70[ % 
          0.058075, 0.058075, // Centrality [70-80[ % 
          0.053433, 0.058075, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.058606, 0.022731  // 60-80%(=60-70%) and Min.bias
};
double P2_BasicCuts5x5Asym1ChiSq2ToF2Cut_counts[CentClassesPi0] = { 
          -0.008095, -0.008095, // Centrality [0-10[ % 
          -0.005245, -0.005245, // Centrality [10-20[ % 
          -0.003583, -0.003583, // Centrality [20-30[ % 
          -0.000361, -0.000361, // Centrality [30-40[ % 
          -0.001018, -0.001018, // Centrality [40-50[ % 
          -0.002317, -0.002317, // Centrality [50-60[ % 
          -0.001746, -0.001746, // Centrality [60-70[ % 
          -0.000595, -0.000595, // Centrality [70-80[ % 
          -0.002529, -0.000595, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.001746, -0.002515  // 60-80%(=60-70%) and Min.bias
};
double P2ERR_BasicCuts5x5Asym1ChiSq2ToF2Cut_counts[CentClassesPi0] = { 
          0.002085, 0.002085, // Centrality [0-10[ % 
          0.002100, 0.002100, // Centrality [10-20[ % 
          0.001884, 0.001884, // Centrality [20-30[ % 
          0.001924, 0.001924, // Centrality [30-40[ % 
          0.002028, 0.002028, // Centrality [40-50[ % 
          0.001892, 0.001892, // Centrality [50-60[ % 
          0.001894, 0.001894, // Centrality [60-70[ % 
          0.001877, 0.001877, // Centrality [70-80[ % 
          0.001725, 0.001877, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.001894, 0.000745  // 60-80%(=60-70%) and Min.bias
};
double P3_BasicCuts5x5Asym1ChiSq2ToF2Cut_counts[CentClassesPi0] = { 
          -0.128831, -0.128831, // Centrality [0-10[ % 
          -0.036581, -0.036581, // Centrality [10-20[ % 
          0.000151, 0.000151, // Centrality [20-30[ % 
          0.098562, 0.098562, // Centrality [30-40[ % 
          0.080849, 0.080849, // Centrality [40-50[ % 
          0.039317, 0.039317, // Centrality [50-60[ % 
          0.058774, 0.058774, // Centrality [60-70[ % 
          0.094733, 0.094733, // Centrality [70-80[ % 
          0.041052, 0.094733, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.058774, 0.036267  // 60-80%(=60-70%) and Min.bias
};
double P3ERR_BasicCuts5x5Asym1ChiSq2ToF2Cut_counts[CentClassesPi0] = { 
          0.052757, 0.052757, // Centrality [0-10[ % 
          0.052528, 0.052528, // Centrality [10-20[ % 
          0.047241, 0.047241, // Centrality [20-30[ % 
          0.048220, 0.048220, // Centrality [30-40[ % 
          0.050105, 0.050105, // Centrality [40-50[ % 
          0.047224, 0.047224, // Centrality [50-60[ % 
          0.047286, 0.047286, // Centrality [60-70[ % 
          0.046819, 0.046819, // Centrality [70-80[ % 
          0.043029, 0.046819, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.047286, 0.018613  // 60-80%(=60-70%) and Min.bias
};
double P4_BasicCuts5x5Asym1ChiSq2ToF2Cut_counts[CentClassesPi0] = { 
          -0.257215, -0.257215, // Centrality [0-10[ % 
          -0.159652, -0.159652, // Centrality [10-20[ % 
          -0.101751, -0.101751, // Centrality [20-30[ % 
          -0.014117, -0.014117, // Centrality [30-40[ % 
          -0.037677, -0.037677, // Centrality [40-50[ % 
          -0.061072, -0.061072, // Centrality [50-60[ % 
          -0.045466, -0.045466, // Centrality [60-70[ % 
          -0.011195, -0.011195, // Centrality [70-80[ % 
          -0.067389, -0.011195, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.045466, -0.070653  // 60-80%(=60-70%) and Min.bias
};
double P4ERR_BasicCuts5x5Asym1ChiSq2ToF2Cut_counts[CentClassesPi0] = { 
          0.051201, 0.051201, // Centrality [0-10[ % 
          0.049679, 0.049679, // Centrality [10-20[ % 
          0.046409, 0.046409, // Centrality [20-30[ % 
          0.046271, 0.046271, // Centrality [30-40[ % 
          0.047247, 0.047247, // Centrality [40-50[ % 
          0.045982, 0.045982, // Centrality [50-60[ % 
          0.046111, 0.046111, // Centrality [60-70[ % 
          0.045723, 0.045723, // Centrality [70-80[ % 
          0.042122, 0.045723, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.046111, 0.017561  // 60-80%(=60-70%) and Min.bias
};
double P0_BasicCuts5x5Asym2ChiSq2RToFCut_counts[CentClassesPi0] = { 
          0.039589, 0.039589, // Centrality [0-10[ % 
          0.004695, 0.004695, // Centrality [10-20[ % 
          0.058311, 0.058311, // Centrality [20-30[ % 
          0.078988, 0.078988, // Centrality [30-40[ % 
          0.090484, 0.090484, // Centrality [40-50[ % 
          0.088295, 0.088295, // Centrality [50-60[ % 
          0.037731, 0.037731, // Centrality [60-70[ % 
          0.078156, 0.078156, // Centrality [70-80[ % 
          0.054319, 0.078156, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.037731, 0.058703  // 60-80%(=60-70%) and Min.bias
};
double P0ERR_BasicCuts5x5Asym2ChiSq2RToFCut_counts[CentClassesPi0] = { 
          0.030389, 0.030389, // Centrality [0-10[ % 
          0.028252, 0.028252, // Centrality [10-20[ % 
          0.028694, 0.028694, // Centrality [20-30[ % 
          0.028985, 0.028985, // Centrality [30-40[ % 
          0.031139, 0.031139, // Centrality [40-50[ % 
          0.028794, 0.028794, // Centrality [50-60[ % 
          0.028578, 0.028578, // Centrality [60-70[ % 
          0.028073, 0.028073, // Centrality [70-80[ % 
          0.025967, 0.028073, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.028578, 0.009998  // 60-80%(=60-70%) and Min.bias
};
double P1_BasicCuts5x5Asym2ChiSq2RToFCut_counts[CentClassesPi0] = { 
          0.037409, 0.037409, // Centrality [0-10[ % 
          0.068104, 0.068104, // Centrality [10-20[ % 
          0.017279, 0.017279, // Centrality [20-30[ % 
          -0.006358, -0.006358, // Centrality [30-40[ % 
          -0.016782, -0.016782, // Centrality [40-50[ % 
          -0.007494, -0.007494, // Centrality [50-60[ % 
          0.045112, 0.045112, // Centrality [60-70[ % 
          -0.001763, -0.001763, // Centrality [70-80[ % 
          0.028979, -0.001763, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.045112, 0.017416  // 60-80%(=60-70%) and Min.bias
};
double P1ERR_BasicCuts5x5Asym2ChiSq2RToFCut_counts[CentClassesPi0] = { 
          0.032260, 0.032260, // Centrality [0-10[ % 
          0.030114, 0.030114, // Centrality [10-20[ % 
          0.030452, 0.030452, // Centrality [20-30[ % 
          0.030822, 0.030822, // Centrality [30-40[ % 
          0.032957, 0.032957, // Centrality [40-50[ % 
          0.030452, 0.030452, // Centrality [50-60[ % 
          0.030298, 0.030298, // Centrality [60-70[ % 
          0.029671, 0.029671, // Centrality [70-80[ % 
          0.027473, 0.029671, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.030298, 0.010591  // 60-80%(=60-70%) and Min.bias
};
double P2_BasicCuts5x5Asym2ChiSq2RToFCut_counts[CentClassesPi0] = { 
          -0.001485, -0.001485, // Centrality [0-10[ % 
          -0.002334, -0.002334, // Centrality [10-20[ % 
          -0.000885, -0.000885, // Centrality [20-30[ % 
          -0.000064, -0.000064, // Centrality [30-40[ % 
          0.000321, 0.000321, // Centrality [40-50[ % 
          -0.000122, -0.000122, // Centrality [50-60[ % 
          -0.001766, -0.001766, // Centrality [60-70[ % 
          -0.000238, -0.000238, // Centrality [70-80[ % 
          -0.001129, -0.000238, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.001766, -0.000846  // 60-80%(=60-70%) and Min.bias
};
double P2ERR_BasicCuts5x5Asym2ChiSq2RToFCut_counts[CentClassesPi0] = { 
          0.001029, 0.001029, // Centrality [0-10[ % 
          0.000963, 0.000963, // Centrality [10-20[ % 
          0.000970, 0.000970, // Centrality [20-30[ % 
          0.000999, 0.000999, // Centrality [30-40[ % 
          0.001091, 0.001091, // Centrality [40-50[ % 
          0.000974, 0.000974, // Centrality [50-60[ % 
          0.000968, 0.000968, // Centrality [60-70[ % 
          0.000949, 0.000949, // Centrality [70-80[ % 
          0.000878, 0.000949, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.000968, 0.000343  // 60-80%(=60-70%) and Min.bias
};
double P3_BasicCuts5x5Asym2ChiSq2RToFCut_counts[CentClassesPi0] = { 
          -0.048856, -0.048856, // Centrality [0-10[ % 
          -0.053033, -0.053033, // Centrality [10-20[ % 
          -0.021806, -0.021806, // Centrality [20-30[ % 
          0.000348, 0.000348, // Centrality [30-40[ % 
          0.010992, 0.010992, // Centrality [40-50[ % 
          -0.006722, -0.006722, // Centrality [50-60[ % 
          -0.040342, -0.040342, // Centrality [60-70[ % 
          -0.000683, -0.000683, // Centrality [70-80[ % 
          -0.024603, -0.000683, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.040342, -0.019645  // 60-80%(=60-70%) and Min.bias
};
double P3ERR_BasicCuts5x5Asym2ChiSq2RToFCut_counts[CentClassesPi0] = { 
          0.025569, 0.025569, // Centrality [0-10[ % 
          0.024035, 0.024035, // Centrality [10-20[ % 
          0.024130, 0.024130, // Centrality [20-30[ % 
          0.024701, 0.024701, // Centrality [30-40[ % 
          0.026468, 0.026468, // Centrality [40-50[ % 
          0.023985, 0.023985, // Centrality [50-60[ % 
          0.023907, 0.023907, // Centrality [60-70[ % 
          0.023361, 0.023361, // Centrality [70-80[ % 
          0.021629, 0.023361, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.023907, 0.008489  // 60-80%(=60-70%) and Min.bias
};
double P4_BasicCuts5x5Asym2ChiSq2RToFCut_counts[CentClassesPi0] = { 
          -0.014173, -0.014173, // Centrality [0-10[ % 
          -0.047010, -0.047010, // Centrality [10-20[ % 
          -0.001120, -0.001120, // Centrality [20-30[ % 
          0.015861, 0.015861, // Centrality [30-40[ % 
          0.022093, 0.022093, // Centrality [40-50[ % 
          0.021575, 0.021575, // Centrality [50-60[ % 
          -0.023143, -0.023143, // Centrality [60-70[ % 
          0.012059, 0.012059, // Centrality [70-80[ % 
          -0.013970, 0.012059, // Centrality [80-93[ %  and 60-92% (=70-80%)
          -0.023143, -0.001831  // 60-80%(=60-70%) and Min.bias
};
double P4ERR_BasicCuts5x5Asym2ChiSq2RToFCut_counts[CentClassesPi0] = { 
          0.025952, 0.025952, // Centrality [0-10[ % 
          0.023999, 0.023999, // Centrality [10-20[ % 
          0.024421, 0.024421, // Centrality [20-30[ % 
          0.024318, 0.024318, // Centrality [30-40[ % 
          0.025626, 0.025626, // Centrality [40-50[ % 
          0.024442, 0.024442, // Centrality [50-60[ % 
          0.024304, 0.024304, // Centrality [60-70[ % 
          0.023824, 0.023824, // Centrality [70-80[ % 
          0.022074, 0.023824, // Centrality [80-93[ %  and 60-92% (=70-80%)
          0.024304, 0.008364  // 60-80%(=60-70%) and Min.bias
};

#endif
