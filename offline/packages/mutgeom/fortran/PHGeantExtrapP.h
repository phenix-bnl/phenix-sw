#ifndef __PHGEXTRAPP_H__
#define __PHGEXTRAPP_H__

/*  Prototype for function mut_kalman_track_param_extrap.F */

extern "C" void phgeantextrap_(
  int *Arm,     //! arm index
  int *Pid,     //! particle index
  double *Zin,
  double Pin[5],
  double Cin[25],
  double *Zout,
  double Pout[5],
  double Cout[25],
  double DPoutDPin[25],
  int *Failure
);

#endif 
