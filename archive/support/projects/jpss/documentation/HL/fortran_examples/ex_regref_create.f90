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
!  This example creates a file and writes a two dimensional integer dataset
!  to it.
!
!  It then reopens the dataset and creates two region references to hyperslabs with
!  coordinates (2,5)-(4,6) and (0,0)-(2,1). It then creates a new data set composed of data
!  from the hyperslabs referenced by the newly created region references. The newly
!  created dataset is then read and printed to the screen.

PROGRAM main

  USE ISO_C_BINDING
  USE H5LT ! module of H5LT 
  USE HDF5 ! module of HDF5 library
  USE H5HL_REGION ! module for hl region library

  CHARACTER(LEN=19), PARAMETER :: filename = "ex_regref_create.h5"
  CHARACTER(LEN=2), PARAMETER :: dsetname = "DS" ! dataset name
  
  INTEGER, PARAMETER :: DIM0=8 ! dataset dimensions
  INTEGER, PARAMETER :: DIM1=7
  
  INTEGER, PARAMETER :: rank=2  ! dataset rank
  
  
  
  INTEGER(hid_t) :: file_id , did       ! file identifier
  INTEGER(hsize_t), DIMENSION(1:rank) :: dims = (/DIM0, DIM1/) ! dataset dimensions
  INTEGER(hsize_t), DIMENSION(1:rank) :: rdims = (/6, 2/) ! dataset dimensions
  INTEGER :: status
  INTEGER, DIMENSION(1:DIM0, 1:DIM1) :: DATA ! data
  INTEGER :: i, j
  INTEGER, DIMENSION(1:6,1:2) :: rdata !  buffer to READ the DATA into
  INTEGER(size_t) :: num_elem ! number of region references to create
  CHARACTER(LEN=80), DIMENSION(1:2) :: path ! paths to the DATA for the region references
  INTEGER(hsize_t), DIMENSION(1:8) :: block_coord = (/ 3, 6, 5, 7, 1, 1, 3, 2/) ! hyperslab coordinates defining region references
  TYPE(hdset_reg_ref_t_f), DIMENSION(1:2) ::  ref ! region references
  INTEGER(hid_t), DIMENSION(1:2) :: file_id_array ! file id of the region references

  INTEGER :: hdferr
  !
  ! Initialize FORTRAN predefined datatypes.
  !
  CALL h5open_f(hdferr)

  num_elem = 2

!*********************************************************  
!   This writes data to the HDF5 file.  
!*********************************************************
  path(1) = "/DS"
  path(2) = "/DS"

  ! 
  ! Data initialization. 
  !
  WRITE(*,'(/,"FULL 2D ARRAY:")') 
  DO i= 1, DIM0
     WRITE(*,'(A)', ADVANCE="NO") "["
     DO j = 1, DIM1
        DATA(i,j) = (DIM0-1)*(i-1)+(j-1)
        WRITE(*,'(1x,I3)', ADVANCE="NO") DATA(i,j)
     ENDDO
     WRITE(*,'(A)') " ]"
  ENDDO

  !
  ! Create file with default file access and file creation properties.
  !
  CALL H5Fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, hdferr)
  !
  ! Create and write the dataset.
  !
  CALL H5LTmake_dataset_int_f( file_id, dsetname, rank, dims, DATA, hdferr)

  CALL  H5Fclose_f(file_id, hdferr)

  !*************************************************************  
  !
  !  This creates a series of region reference
  !
  ! ************************************************************

  !
  ! Reopen the file.
  !
  CALL H5Fopen_f(filename, H5F_ACC_RDWR_F, file_id, hdferr)
  !
  !   Create an array of region references using an array of paths
  !   and an array of corresponding hyperslab descriptions.
  !
  CALL H5LRcreate_region_references_f(file_id, num_elem, path, block_coord, ref, hdferr)
  !
  ! We are creating the data set in the same file so fill the file_id path with the same file id.
  !
  DO i = 1, num_elem
     file_id_array(i) = file_id
  ENDDO

  !
  ! Check the region references: (1) create a new dataset from the data pointed to by the region references 
  !

  CALL H5LRmake_dataset_f(file_id, "/DS2a", H5T_NATIVE_INTEGER, num_elem, file_id_array, ref, hdferr)

  ! print the newly created data set

  CALL H5Dopen_f(file_id, "/DS2a", did, hdferr)

  CALL H5Dread_f(did, H5T_NATIVE_INTEGER, rdata, rdims, hdferr)

  WRITE(*,'(/,"FULL 2D DATA CREATED BY LIST OF REGION REFERENCES:")')

  DO i = 1, 6
     WRITE(*,'(A)', ADVANCE="NO") "["
     DO j = 1, 2
        WRITE(*,'(I4)', ADVANCE="NO") rdata(i,j)
     ENDDO
     WRITE(*,'(A)') " ]"
  ENDDO

  CALL H5Fclose_f(file_id, hdferr)

END PROGRAM main



