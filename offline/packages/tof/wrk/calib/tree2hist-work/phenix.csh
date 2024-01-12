#! /bin/csh -f
# Usage: source phenix.csh [-a] [-v] [pro] [new] [old]
#
#setenv PH_HOME      /phenix/workarea/kiyo/phool
#setenv PH_HOME      /ccj/u/kiyo/work/phool
setenv PH_HOME      /phenix/raid0/people/kiyo/phool

    #echo "pro Library"
    setenv ROOTSYS      /opt/phenix/root-2.25.03
    setenv OFFLINE_MAIN /afs/rhic/phenix/PHENIX_LIB/sys/i386_redhat61/pro.8
    source /opt/phenix/bin/phenix_setup.csh pro
    setenv LD_LIBRARY_PATH  ${PH_HOME}/install/pro/lib:${LD_LIBRARY_PATH}

# setup Objectivity  dbinstall/objyize.csh
#setenv OO_FD_BOOT $PHENIX_FD_BOOT
setenv OO_FD_BOOT ccjams0.riken.go.jp::/job_tmp/DB/objy/010228/FDB/PHENIX_NEW