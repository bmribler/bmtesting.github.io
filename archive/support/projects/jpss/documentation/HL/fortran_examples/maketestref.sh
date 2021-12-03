##############################################################################
# 
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of the hl_region High-Level HDF5 APIs.  The full copyright 
# notice, including terms governing use, modification, and redistribution, 
# is contained in the file COPYING, which can be found at the root of the
# source code distribution tree and in the documentation directory (doc/html/). 
# If you do not have access to this file, you may request a copy of 
# "the hl_region High-Level HDF5 APIs copyright and license statement" from 
# help@hdfgroup.org.
# 
##############################################################################
#! /bin/sh
#
case $CC in
*/*)    H5DUMP=`echo $CC | sed -e 's/\/[^/]*$/\/h5dump/'`;
        test -x $H5DUMP || H5DUMP=h5dump;;
*)      H5DUMP=h5dump;;
esac

case `echo "testing\c"; echo 1,2,3`,`echo -n testing; echo 1,2,3` in
  *c*,-n*) ECHO_N= ECHO_C='
' ;;
  *c*,*  ) ECHO_N=-n ECHO_C= ;;
  *)       ECHO_N= ECHO_C='\c' ;;
esac
ECHO_N="echo $ECHO_N"

exout() {
    echo '*******************************'
    echo '*  Output of example program  *'
    echo '*******************************'
    echo
    $*
}

dumpout() {
    echo '**********************'
    echo '*  Output of h5dump  *'
    echo '**********************'
    echo
    $H5DUMP $*
}

dumpout2() {
    echo
    echo
    echo '**********************'
    echo '*  Output of h5dump  *'
    echo '**********************'
    echo
    $H5DUMP $*
}

topics="ex_bitfield ex_lite_copy_region ex_lite_read_region ex_ref_to_all ex_regref_copy_reference ex_regref_create ex_regref_read"

for topic in $topics
do
    fname=$topic
    $ECHO_N "Creating test reference file for fortran/examples/$fname...$ECHO_C"
    exout ./$fname >$fname.test
    dumpout2 $fname.h5 >>$fname.test
    rm -f $fname.h5
    echo "  Done."
done

