
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
! This example creates a file and writes a two dimensional real dataset to it.
!
! It then reopens the file and reads a region of data given a set of corner 
! coordinates main illustrative function: H5LTread_region

PROGRAM main

  USE ISO_C_BINDING
  USE H5LT ! module of H5LT 
  USE HDF5 ! module of HDF5 library
  USE H5HL_REGION ! module for hl region library
  
  IMPLICIT NONE
  
  CHARACTER(LEN=22) :: filename = "ex_lite_read_region.h5"
  CHARACTER(LEN=2)  :: DSETNAME = "DS" ! dataset name
  
  INTEGER, PARAMETER :: DIM0 = 5 ! dataset dimensions
  INTEGER, PARAMETER :: DIM1 = 4
  
  INTEGER, PARAMETER :: RANK = 2 ! dataset RANK
  
  INTEGER(HID_T) :: file_id ! file identifier
  
  INTEGER(HSIZE_T), DIMENSION(1:RANK) :: dims = (/DIM0, DIM1/) ! dataset dimension
  REAL, DIMENSION(DIM0,DIM1) :: DATA
  INTEGER :: i, j
  INTEGER(HSIZE_T), DIMENSION(1:4) :: block_coord = (/2, 2, 4, 3/)
  REAL, DIMENSION(1:3,1:2), TARGET :: rdata ! read data buffer
  TYPE(C_PTR) :: f_ptr
  INTEGER :: block_coord_rank
  INTEGER hdferr
  !
  ! Initialize FORTRAN predefined datatypes.
  !
  CALL h5open_f(hdferr) 
  
  !*********************************************************  
  !   This writes data to the HDF5 file.  
  !*********************************************************/ 
  
  ! 
  ! Data initialization. 
  !
  WRITE(*,'(/,"FULL 2D ARRAY:")') 
  DO i= 1, DIM0
     WRITE(*,'(A)', ADVANCE="NO") "["
     DO j = 1, DIM1
        DATA(i,j) = REAL( (DIM0-1)*(i-1)+(j-1))
        WRITE(*,'(1X,F8.5)', ADVANCE="NO") DATA(i,j)
     ENDDO
     WRITE(*,'(A)') " ]"
  ENDDO
  
  !
  ! Create file with default file access and file creation properties.
  !
  CALL H5Fcreate_f(filename, H5F_ACC_TRUNC_F,  file_id, hdferr)
  !
  ! Create and write the dataset.
  !
  
  CALL H5LTmake_dataset_float_f( file_id, DSETNAME, 2, dims, DATA, hdferr)

  CALL H5Fclose_f(file_id, hdferr)

  !*************************************************************  
  !
  !  This reads a region of data given corner coordinates
  !
  !************************************************************* 

  f_ptr = C_LOC(rdata)
  CALL H5LTread_region_f(filename,"/DS", block_coord, H5T_NATIVE_REAL, f_ptr, hdferr)

  WRITE(*,'(/,"REGION 2D HYPERSLAB BY CORNER COORDINATES, COORDINATES (",i1,",",i1,")-(",i1,",",i1,"):")') block_coord(1:4)

  DO i = 1, 3
     WRITE(*,'(A)', ADVANCE="NO") "["
     WRITE(*,'(2(1X,F8.5))', ADVANCE="NO") rdata(i,1:2)
     WRITE(*,'(A)') " ]"
  ENDDO

END PROGRAM main


