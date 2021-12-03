!
!##############################################################################
!# 
!# Copyright by The HDF Group.
!# All rights reserved.
!#
!# This file is part of the hl_region High-Level HDF5 APIs.  The full copyright 
!# notice, including terms governing use, modification, and redistribution, 
!# is contained in the file COPYING, which can be found at the root of the
!# source code distribution tree and in the documentation directory (doc/html/). 
!# If you do not have access to this file, you may request a copy of 
!# "the hl_region High-Level HDF5 APIs copyright and license statement" from 
!# help@hdfgroup.org.
!# 
!##############################################################################

!
!  This program shows how to create a dataset and region references associated
!  with hyperslabs. It then creates a new dataset composed of data from the 
!  data associated with a recursive loop over all the region references. 
!  Main illustrative function: H5LRcreate_ref_to_all_f
!

PROGRAM main

  USE ISO_C_BINDING
  USE HDF5 ! module of HDF5 library
  USE H5LT ! module of H5LT
  USE H5HL_REGION ! module for hl region library

  IMPLICIT NONE

  CHARACTER(LEN=16), PARAMETER :: filename = "ex_ref_to_all.h5"
  CHARACTER(LEN=7), PARAMETER :: dsetnamev = "MY_DATA" ! dataset name
  CHARACTER(LEN=6), PARAMETER :: dsetnamer = "MY_REF" ! region references name
  
  INTEGER, PARAMETER :: DIM0 =9   ! dataset dimensions
  INTEGER, PARAMETER :: DIM1 =8
  INTEGER, PARAMETER :: RANK = 2 ! dataset rank

  INTEGER(hid_t) :: file_id       ! file identifier
  INTEGER(hid_t) :: space_id_ref  ! region reference dataspace identifier
  INTEGER(hid_t) :: dset_id_ref   ! region reference dataset identifiers
  INTEGER(hsize_t), DIMENSION(1:rank) :: dims = (/DIM0,DIM1/) ! dataset dimensions
  INTEGER(hsize_t), DIMENSION(1:1) :: dims_ref = (/2/) ! region reference dimensions
  TYPE(hdset_reg_ref_t_f), DIMENSION(1:2) :: ref !  region references
  INTEGER, DIMENSION(1:dim0, 1:dim1) ::  DATA !  DATA
  INTEGER :: i,j
  CHARACTER(LEN=80), DIMENSION(1:2) :: path ! paths to the data for the region references
  ! hyperslab coordinates defining region references
  INTEGER(hsize_t), DIMENSION(1:8) :: block_coord = (/0, 0, 2, 2, 3, 3, 5, 5/)
  INTEGER(size_t) :: num_elem = 2

  INTEGER :: hdferr
  !
  ! Initialize FORTRAN predefined datatypes.
  !
  CALL h5open_f(hdferr)

  !*********************************************************  
  !  This section writes data to the HDF5 file.  
  !*********************************************************

  path(1) = "/MY_DATA"
  path(2) = "/MY_DATA"
  
  ! 
  ! Data initialization. 
  !
  WRITE(*,'(/,"FULL 2D DATA:")') 
  DO i= 1, DIM0
     WRITE(*,'(A)', ADVANCE="NO") "["
     DO j = 1, DIM1
        DATA(i,j) = (DIM0-1)*(i-1)+(j-1)
        WRITE(*,'(1x,I3)', ADVANCE="NO") DATA(i,j)
     ENDDO
     WRITE(*,'(A)') " ]"
  ENDDO


  ! Open the file.

  CALL H5Fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, hdferr)
  !
  ! Create and write the dataset.
  !
  CALL H5LTmake_dataset_int_f(file_id,  dsetnamev, rank, dims, data, hdferr)
  !
  ! Create two region references with hyperslab coordinates: (0,0)-(2,2) and (3,3)-(5,5).
  !
  CALL H5LRcreate_region_references_f(file_id, num_elem, path, block_coord, ref, hdferr)
  !
  ! Write the region references to the file
  !
  CALL H5Screate_simple_f(1, dims_ref, space_id_ref, hdferr)
  CALL H5Dcreate_f(file_id, dsetnamer, H5T_STD_REF_DSETREG, space_id_ref, dset_id_ref, hdferr)
  CALL H5Dwrite_f(dset_id_ref, H5T_STD_REF_DSETREG, ref, dims_ref, hdferr)
  !
  ! Close all objects.
  !
  CALL H5Sclose_f(space_id_ref, hdferr)
  CALL H5Dclose_f(dset_id_ref, hdferr)
  CALL H5Fclose_f(file_id, hdferr)
  
  !************************************************************* 
  !
  !  This section creates a data set from region references found
  !  recursively in the file starting at "/" for this example.
  !
  !*************************************************************
  
  !
  ! Reopen the file.
  !
  CALL H5Fopen_f(filename, H5F_ACC_RDWR_F, file_id, hdferr)
  
  !
  ! Create the "/MY_DATA" dataset from regions of data.
  !
  CALL H5LRcreate_ref_to_all_f(file_id, "/", &
       "/NEW_DATA", H5_INDEX_NAME_F, H5_ITER_INC_F, H5R_DATASET_REGION_F, hdferr)

END PROGRAM main



