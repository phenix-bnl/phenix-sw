#!/usr/local/bin/tcsh
#
# Check script
#
#  Usage: nbatch_rmhome.csh
#
#

source /opt/phenix/bin/phenix_setup.csh
set lsf_host_list = tmp_nbatch_lsfcheck.list
#set homedir = /home/${USER}
set homedir = /home/htorii_tmp

#bhosts phenix | awk '{if($2=="ok") print $1}' > ${lsf_host_list}
#bhosts phenix | awk '/^r/{ print $1}' > ${lsf_host_list}
#bhosts phenix | awk '{if($1!="HOST_NAME") print $1}' > ${lsf_host_list}

#bhosts phenix | awk '/rc/{if($2=="ok") print $1}' > ${lsf_host_list}
bhosts phenix | awk '/rc/{print $1}' > ${lsf_host_list}



echo "------------ nbatch_lsfcheck.csh -------- "
#cat ${lsf_host_list}

@ num = `wc ${lsf_host_list} | awk '{print $1}' `
echo " --- processing hosts " ${num}

while( $num > 0 )
    set lsf_host = `awk '{if (NR=='$num') print $1}' ${lsf_host_list}`
    echo -n " --- $num : Deleting ${homedir}@${lsf_host} : " 
#    @ home_size = ` lsrun -m ${lsf_host} du -s /home/htorii | awk '{print $1}' `
#    echo " --- deletting home size : " ${home_size}
#    if( ${home_size} > 0 ) then
    lsrun -m ${lsf_host} rm -fR ${homedir}
#    endif
    echo " done. "
    @ num = $num - 1
end

rm -f ${lsf_host_list}


#
