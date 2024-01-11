#
# This is to connect PHENIX database of PHENIX (production dB)
#
echo "Setting up the Environment for Objectivity on LINUX"
echo "Setting up for PHENIX database"

setenv OBJY_LS_HOST rmine00.rhic.bnl.gov
#setenv OO_FD_BOOT rmine00.rhic.bnl.gov::/phenix/data02/phnxreco/objy/PHENIX
setenv OO_FD_BOOT $PHENIX_FD_BOOT
#setenv OBJY_FDID 26000
setenv OBJY_FDID 26700

setenv XFILESEARCHPATH ${OBJYSYS}/etc/app-defaults/%N
setenv XBMLANGPATH ${OBJYSYS}/etc/bitmaps/%N/%B

