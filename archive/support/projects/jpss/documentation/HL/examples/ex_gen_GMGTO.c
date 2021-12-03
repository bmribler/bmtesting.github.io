/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * https://support.hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Description: Creates a datafile format similar to that specified in
 * the Geolocation Product VIIRS-MOD-EDR-GEO (Product: ID GMGTO) of the 
 * Ground Integrated Data Processing System for NPOESS. It produces both
 * little (GMGTO_LE.h5) and big endian formats (GMGTO_BE.h5) depending on 
 * the system. These files are used by the GMGTO.c example program. 
 * 
 */

#include "hdf5.h"

#define MSPACE1_RANK     1          /* Rank of the first dataset in memory */
#define MSPACE1_DIM      50         /* Dataset size in memory */

#define MSPACE2_RANK     1          /* Rank of the second dataset in memory */
#define MSPACE2_DIM      4          /* Dataset size in memory */

#define FSPACE_RANK      2          /* Dataset rank as it is stored in the file */
#define FSPACE_RANK1     1          /* Dataset rank as it is stored in the file */
#define FSPACE_DIM1      8          /* Dimension sizes of the dataset as it is
                                       stored in the file */
#define FSPACE_DIM2      9

                                    /* We will read dataset back from the file
                                       to the dataset in memory with these
                                       dataspace parameters. */
#define MSPACE_DIM1      8

#define NPOINTS          4          /* Number of points that will be selected
                                       and overwritten */
int
main (void)
{

   hid_t   file, dataset;           /* File and dataset identifiers */
   hid_t   mid1, fid;    /* Dataspace identifiers */
   hid_t   plist;                   /* Dataset property list identifier */

   hsize_t dim1[] = {MSPACE1_DIM};  /* Dimension size of the first dataset
                                       (in memory) */
   hsize_t dim1a[] = {5};  /* Dimension size of the first dataset
                                       (in memory) */
   hsize_t dim1b[] = {1};  /* Dimension size of the first dataset
                                       (in memory) */
   hsize_t fdim[] = {FSPACE_DIM1, FSPACE_DIM2};
                                    /* Dimension sizes of the dataset (on disk) */
   hsize_t fdim1[] = {2};
   hsize_t fdim2[] = {4};
   hsize_t fdim3[] = {1};
   hsize_t fdim1a[] = {MSPACE_DIM1};

   hsize_t start[2];  /* Start of hyperslab */
   hsize_t stride[2]; /* Stride of hyperslab */
   hsize_t count[2];  /* Block count */
   hsize_t block[2];  /* Block sizes */

   unsigned i,j;
   int fillvalue = -993;   /* Fill value for the dataset */

   int    vector[MSPACE1_DIM];                       /*dataset */
   float  vectorf[MSPACE1_DIM];

   float rfillvalue = -999.3;
   unsigned char rfillvalue1 = -7;
   hsize_t      maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED};
   hsize_t      maxdims1[1] = {H5S_UNLIMITED};
   hid_t       group, gcpl;

   unsigned char wdata[2];
   unsigned char wdata4[4];

   unsigned char vectorPad[MSPACE1_DIM];
   unsigned char vectorQflag[1];

   int64_t vectorTime[5];
   int64_t rfillvalue2 = -999;
   unsigned short rfillvalue3 = -7;
   char *filename;

   int test_endian = 1;

   int ret;

   /* Find endianness of operating system */
   if(*(char*)&test_endian == 1) { /* little endian */
     filename = "GMGTO_LE.h5";
   }
   else { /* big-endian */
     filename = "GMGTO_BE.h5";
   }

   /*
    * Buffers' initialization.
    */
    vector[0] = vector[MSPACE1_DIM - 1] = -1;
   for(i = 1; i < MSPACE1_DIM - 1; i++) {
     vector[i] = i;
     vectorf[i] = i+.1*(i+1);
     vectorPad[i] = 0;
     vectorPad[i] |= (i * 2 - 2) & 0x03;          /* Field "A" */
     vectorPad[i] |= (i & 0x03) << 2;             /* Field "B" */
     vectorPad[i] |= (2 & 0x03) << 4;             /* Field "C" */
     vectorPad[i] |= ( (i + 2) & 0x03 ) <<6;      /* Field "D" */
   }
  /*
   * Initialize data.  We will manually fill four 2-bit integers into
   * each unsigned char data element.
   */
   for (i=0; i<4; i++) {
     if(i<2) {
       wdata[i] = 0;
       wdata[i] |= (i * 2 - 2) & 0x03;          /* Field "A" */
       wdata[i] |= (i & 0x03) << 2;             /* Field "B" */
       wdata[i] |= (j & 0x03) << 4;             /* Field "C" */
       wdata[i] |= ( (i + 2) & 0x03 ) <<6;      /* Field "D" */
     }
    
     wdata4[i] = 0;
     wdata4[i] |= (i * 2 - 2) & 0x03;          /* Field "A" */
     wdata4[i] |= (i & 0x03) << 2;             /* Field "B" */
     wdata4[i] |= (2 & 0x03) << 4;             /* Field "C" */
     wdata4[i] |= ( (i + 2) & 0x03 ) <<6;      /* Field "D" */ 
     
   }

   vectorQflag[0] = wdata[1];

   for (i=0; i<5; i++) {
     vectorTime[i]=1422169169077220032+i;
   }

   /*
    * Create a file.
    */
   file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
   /*
    * Create group creation property list and set it to allow creation
    * of intermediate groups.
    */
   gcpl = H5Pcreate (H5P_LINK_CREATE);
   ret = H5Pset_create_intermediate_group (gcpl, 1);

  /*
   * Create a group named "/All_Data/VIIRS-MOD-GTM-EDR-GEO_ALL" in the file.
   */
   group = H5Gcreate (file, "/All_Data/VIIRS-MOD-GTM-EDR-GEO_ALL", gcpl, H5P_DEFAULT, H5P_DEFAULT);

   /*
    * Create property list for a dataset and set up fill values.
    */
   plist = H5Pcreate(H5P_DATASET_CREATE);
   ret   = H5Pset_chunk( plist, FSPACE_RANK, fdim);
   ret   = H5Pset_fill_value(plist, H5T_NATIVE_INT, &fillvalue);

    /*
     * Create dataspace for the dataset in the file.
     */
    fid = H5Screate_simple(FSPACE_RANK, fdim, maxdims);

    /*
     * Create dataset in the file. Notice that creation
     * property list plist is used.
     */
    dataset = H5Dcreate2(file, "/All_Data/VIIRS-MOD-GTM-EDR-GEO_ALL/Height",H5T_NATIVE_INT, fid, H5P_DEFAULT, plist, H5P_DEFAULT);

    /*
     * Select hyperslab for the dataset in the file, using 3x2 blocks,
     * (4,3) stride and (2,4) count starting at the position (0,1).
     */
    start[0]  = 1; start[1]  = 1;
    stride[0] = 3; stride[1] = 2;
    count[0]  = 2; count[1]  = 4;
    block[0]  = 3; block[1]  = 2;
    ret = H5Sselect_hyperslab(fid, H5S_SELECT_SET, start, stride, count, block);

    /*
     * Create dataspace for the first dataset.
     */
    mid1 = H5Screate_simple(MSPACE1_RANK, dim1, NULL);

    /*
     * Select hyperslab.
     * We will use 48 elements of the vector buffer starting at the second element.
     * Selected elements are 1 2 3 . . . 48
     */
    start[0]  = 1;
    stride[0] = 1;
    count[0]  = 48;
    block[0]  = 1;
    ret = H5Sselect_hyperslab(mid1, H5S_SELECT_SET, start, stride, count, block);

    /*
     * Write selection from the vector buffer to the dataset in the file.
     */
    ret = H5Dwrite(dataset, H5T_NATIVE_INT, mid1, fid, H5P_DEFAULT, vector);

    /*
     * Close memory file and memory dataspaces.
     */
    ret = H5Sclose(mid1);
    ret = H5Sclose(fid);
    ret = H5Pclose(plist);

    /*
     * Close dataset.
     */
    ret = H5Dclose(dataset);


   /*
    * Create property list for a dataset and set up fill values.
    */
   plist = H5Pcreate(H5P_DATASET_CREATE);
   ret   = H5Pset_chunk( plist, FSPACE_RANK, fdim);
   ret   = H5Pset_fill_value(plist, H5T_NATIVE_FLOAT, &rfillvalue);

    /*
     * Create dataspace for the dataset in the file.
     */
    fid = H5Screate_simple(FSPACE_RANK, fdim, maxdims);

    /*
     * Create dataset in the file. Notice that creation
     * property list plist is used.
     */
    dataset = H5Dcreate2(file, "/All_Data/VIIRS-MOD-GTM-EDR-GEO_ALL/Latitude", H5T_NATIVE_FLOAT, fid, H5P_DEFAULT, plist, H5P_DEFAULT);

    /*
     * Select hyperslab for the dataset in the file, using 3x2 blocks,
     * (4,3) stride and (2,4) count starting at the position (0,1).
     */
    start[0]  = 1; start[1]  = 1;
    stride[0] = 3; stride[1] = 2;
    count[0]  = 2; count[1]  = 4;
    block[0]  = 3; block[1]  = 2;
    ret = H5Sselect_hyperslab(fid, H5S_SELECT_SET, start, stride, count, block);

    /*
     * Create dataspace for the first dataset.
     */
    mid1 = H5Screate_simple(MSPACE1_RANK, dim1, NULL);

    /*
     * Select hyperslab.
     * We will use 48 elements of the vector buffer starting at the second element.
     * Selected elements are 1 2 3 . . . 48
     */
    start[0]  = 1;
    stride[0] = 1;
    count[0]  = 48;
    block[0]  = 1;
    ret = H5Sselect_hyperslab(mid1, H5S_SELECT_SET, start, stride, count, block);

    /*
     * Write selection from the vector buffer to the dataset in the file.
     */
     ret = H5Dwrite(dataset, H5T_NATIVE_FLOAT, mid1, fid, H5P_DEFAULT, vectorf);

     /*
      * Close memory file and memory dataspaces.
      */
     ret = H5Sclose(mid1);
     ret = H5Sclose(fid);
     ret = H5Pclose(plist);

    /*
     * Close dataset.
     */
     ret = H5Dclose(dataset);

    /* copy the dataset from Latitude to Longitude */
     ret = H5Ocopy(file, "/All_Data/VIIRS-MOD-GTM-EDR-GEO_ALL/Latitude", file, "/All_Data/VIIRS-MOD-GTM-EDR-GEO_ALL/Longitude", H5P_DEFAULT, H5P_DEFAULT);

     /*
      * Create property list for a dataset and set up fill values.
      */
     plist = H5Pcreate(H5P_DATASET_CREATE);
     ret   = H5Pset_chunk( plist, 1, fdim1);
     ret   = H5Pset_fill_value(plist, H5T_NATIVE_UCHAR, &rfillvalue1);

    /*
     * Create dataspace for the dataset in the file.
     */
    fid = H5Screate_simple(FSPACE_RANK1, fdim1, maxdims1);

    /*
     * Create dataset in the file. Notice that creation
     * property list plist is used.
     */
    dataset = H5Dcreate2(file, "/All_Data/VIIRS-MOD-GTM-EDR-GEO_ALL/PadByte1", H5T_NATIVE_UCHAR, fid, H5P_DEFAULT, plist, H5P_DEFAULT);

    /*
     * Create dataspace for the first dataset.
     */
    mid1 = H5Screate_simple(1, fdim1, NULL);

    ret = H5Sselect_all(mid1);

    /*
     * Write selection from the vector buffer to the dataset in the file.
     */
    ret = H5Dwrite(dataset,  H5T_NATIVE_UCHAR, mid1, fid, H5P_DEFAULT, wdata);

    /*
     * Close memory file and memory dataspaces.
     */
    ret = H5Sclose(mid1);
    ret = H5Sclose(fid);
    ret = H5Pclose(plist);

    /*
     * Close dataset.
     */
    ret = H5Dclose(dataset);

    /* PadByte2 */

    /*
     * Create property list for a dataset and set up fill values.
     */
    plist = H5Pcreate(H5P_DATASET_CREATE);
    ret   = H5Pset_chunk( plist, 1, fdim2);
    ret   = H5Pset_fill_value(plist, H5T_NATIVE_UCHAR, &rfillvalue1);

    /*
     * Create dataspace for the dataset in the file.
     */
    fid = H5Screate_simple(FSPACE_RANK1, fdim2, maxdims1);

    /*
     * Create dataset in the file. Notice that creation
     * property list plist is used.
     */
    dataset = H5Dcreate2(file, "/All_Data/VIIRS-MOD-GTM-EDR-GEO_ALL/PadByte2", H5T_NATIVE_UCHAR, fid, H5P_DEFAULT, plist, H5P_DEFAULT);

    /*
     * Create dataspace for the first dataset.
     */
    mid1 = H5Screate_simple(1, fdim2, NULL);

    ret = H5Sselect_all(mid1);

    /*
     * Write selection from the vector buffer to the dataset in the file.
     */
    ret = H5Dwrite(dataset,  H5T_NATIVE_UCHAR, mid1, fid, H5P_DEFAULT, wdata4);

    /*
     * Close memory file and memory dataspaces.
     */
    ret = H5Sclose(mid1);
    ret = H5Sclose(fid);
    ret = H5Pclose(plist);

    /*
     * Close dataset.
     */
    ret = H5Dclose(dataset);

    /*******************/
    /* PixelColSDR     */
    /*******************/

   /*
    * Create property list for a dataset and set up fill values.
    */
    plist = H5Pcreate(H5P_DATASET_CREATE);
    ret   = H5Pset_chunk( plist, FSPACE_RANK, fdim);
    ret   = H5Pset_fill_value(plist, H5T_NATIVE_INT, &rfillvalue3);

    /*
     * Create dataspace for the dataset in the file.
     */
    fid = H5Screate_simple(FSPACE_RANK, fdim, maxdims);

    /*
     * Create dataset in the file. Notice that creation
     * property list plist is used.
     */
    dataset = H5Dcreate2(file, "/All_Data/VIIRS-MOD-GTM-EDR-GEO_ALL/PixelColSDR", H5T_NATIVE_UINT16, fid, H5P_DEFAULT, plist, H5P_DEFAULT);

    /*
     * Select hyperslab for the dataset in the file, using 3x2 blocks,
     * (4,3) stride and (2,4) count starting at the position (0,1).
     */
    start[0]  = 1; start[1]  = 1;
    stride[0] = 3; stride[1] = 2;
    count[0]  = 2; count[1]  = 4;
    block[0]  = 3; block[1]  = 2;
    ret = H5Sselect_hyperslab(fid, H5S_SELECT_SET, start, stride, count, block);

    /*
     * Create dataspace for the first dataset.
     */
    mid1 = H5Screate_simple(MSPACE1_RANK, dim1, NULL);

    /*
     * Select hyperslab.
     * We will use 48 elements of the vector buffer starting at the second element.
     * Selected elements are 1 2 3 . . . 48
     */
    start[0]  = 1;
    stride[0] = 1;
    count[0]  = 48;
    block[0]  = 1;
    ret = H5Sselect_hyperslab(mid1, H5S_SELECT_SET, start, stride, count, block);

    /*
     * Write selection from the vector buffer to the dataset in the file.
     */
    ret = H5Dwrite(dataset,  H5T_NATIVE_INT, mid1, fid, H5P_DEFAULT, vector);

    /*
     * Close memory file and memory dataspaces.
     */
    ret = H5Sclose(mid1);
    ret = H5Sclose(fid);
    ret = H5Pclose(plist);

    /*
     * Close dataset.
     */
    ret = H5Dclose(dataset);

    
    /* copy the dataset from Latitude to Longitude */
    ret = H5Ocopy(file, "/All_Data/VIIRS-MOD-GTM-EDR-GEO_ALL/PixelColSDR", file, "/All_Data/VIIRS-MOD-GTM-EDR-GEO_ALL/PixelRowSDR", H5P_DEFAULT, H5P_DEFAULT);

   /*
    * Create property list for a dataset and set up fill values.
    */
    plist = H5Pcreate(H5P_DATASET_CREATE);
    ret   = H5Pset_chunk( plist, FSPACE_RANK, fdim);
    ret   = H5Pset_fill_value(plist, H5T_NATIVE_UCHAR, &rfillvalue1);

    /*
     * Create dataspace for the dataset in the file.
     */
    fid = H5Screate_simple(FSPACE_RANK, fdim, maxdims);

    /*
     * Create dataset in the file. Notice that creation
     * property list plist is used.
     */
    dataset = H5Dcreate2(file, "/All_Data/VIIRS-MOD-GTM-EDR-GEO_ALL/QF1_VIIRSGTMGEO", H5T_NATIVE_UCHAR, fid, H5P_DEFAULT, plist, H5P_DEFAULT);

    /*
     * Select hyperslab for the dataset in the file, using 3x2 blocks,
     * (4,3) stride and (2,4) count starting at the position (0,1).
     */
    start[0]  = 1; start[1]  = 1;
    stride[0] = 3; stride[1] = 2;
    count[0]  = 2; count[1]  = 4;
    block[0]  = 3; block[1]  = 2;
    ret = H5Sselect_hyperslab(fid, H5S_SELECT_SET, start, stride, count, block);

    /*
     * Create dataspace for the first dataset.
     */
    mid1 = H5Screate_simple(MSPACE1_RANK, dim1, NULL);

    /*
     * Select hyperslab.
     * We will use 48 elements of the vector buffer starting at the second element.
     * Selected elements are 1 2 3 . . . 48
     */
    start[0]  = 1;
    stride[0] = 1;
    count[0]  = 48;
    block[0]  = 1;
    ret = H5Sselect_hyperslab(mid1, H5S_SELECT_SET, start, stride, count, block);

    /*
     * Write selection from the vector buffer to the dataset in the file.
     */
     ret = H5Dwrite(dataset, H5T_NATIVE_UCHAR, mid1, fid, H5P_DEFAULT, vectorPad);

    /*
     * Close memory file and memory dataspaces.
     */
    ret = H5Sclose(mid1);
    ret = H5Sclose(fid);
    ret = H5Pclose(plist);

    /*
     * Close dataset.
     */
    ret = H5Dclose(dataset);

    /*******************/
    /* PixelColSDR     */
    /*******************/ 

   /*
    * Create property list for a dataset and set up fill values.
    */
    plist = H5Pcreate(H5P_DATASET_CREATE);
    ret   = H5Pset_chunk( plist, 1, fdim3);
    ret   = H5Pset_fill_value(plist, H5T_NATIVE_UCHAR, &rfillvalue1);

    /*
     * Create dataspace for the dataset in the file.
     */
    fid = H5Screate_simple(1, fdim3, maxdims);

    /*
     * Create dataset in the file. Notice that creation
     * property list plist is used.
     */
    dataset = H5Dcreate2(file, "/All_Data/VIIRS-MOD-GTM-EDR-GEO_ALL/QF2_VIIRSGTMGEO", H5T_NATIVE_UCHAR, fid, H5P_DEFAULT, plist, H5P_DEFAULT);

    /*
     * Select hyperslab for the dataset in the file
     */
    start[0]  = 0;
    stride[0] = 1; 
    count[0]  = 1;
    block[0]  = 1;
    ret = H5Sselect_hyperslab(fid, H5S_SELECT_SET, start, stride, count, block);

    /*
     * Create dataspace for the first dataset.
     */
    mid1 = H5Screate_simple(1, dim1b, NULL);

    /*
     * Write selection from the vector buffer to the dataset in the file.
     */
     ret = H5Dwrite(dataset, H5T_NATIVE_UCHAR, mid1, fid, H5P_DEFAULT, vectorQflag);

    /*
     * Close memory file and memory dataspaces.
     */
    ret = H5Sclose(mid1);
    ret = H5Sclose(fid);
    ret = H5Pclose(plist);

    /*
     * Close dataset.
     */
    ret = H5Dclose(dataset);

   /*
    * Create property list for a dataset and set up fill values.
    */
    plist = H5Pcreate(H5P_DATASET_CREATE);
    ret   = H5Pset_chunk( plist, 1, fdim1a);
    ret   = H5Pset_fill_value(plist,  H5T_NATIVE_INT, &rfillvalue2);

    /*
     * Create dataspace for the dataset in the file.
     */
    fid = H5Screate_simple(1, fdim1a, maxdims1);

    /*
     * Create dataset in the file. Notice that creation
     * property list plist is used. 
     */
    dataset = H5Dcreate2(file, "/All_Data/VIIRS-MOD-GTM-EDR-GEO_ALL/Time", H5T_NATIVE_LLONG, fid, H5P_DEFAULT, plist, H5P_DEFAULT);

    /*
     * Select hyperslab for the dataset in the file, using 3x2 blocks,
     * (4,3) stride and (2,4) count starting at the position (0,1).
     */
    start[0]  = 0;
    stride[0] = 1;
    count[0]  = 5;
    block[0]  = 1;
    ret = H5Sselect_hyperslab(fid, H5S_SELECT_SET, start, stride, count, block);

    /*
     * Create dataspace for the first dataset.
     */
    mid1 = H5Screate_simple(1, dim1a, NULL);

    /*
     * Select hyperslab.
     * We will use 48 elements of the vector buffer starting at the second element.
     * Selected elements are 1 2 3 . . . 48
     */
    start[0]  = 0;
    stride[0] = 1;
    count[0]  = 5;
    block[0]  = 1;
    ret = H5Sselect_hyperslab(mid1, H5S_SELECT_SET, start, stride, count, block);

    /*
     * Write selection from the vector buffer to the dataset in the file.
     */
    ret = H5Dwrite(dataset, H5T_NATIVE_LLONG, mid1, fid, H5P_DEFAULT, vectorTime);

    /*
     * Close memory file and memory dataspaces.
     */
    ret = H5Sclose(mid1);
    ret = H5Sclose(fid);
    ret = H5Pclose(plist);

    /*
     * Close dataset.
     */
    ret = H5Dclose(dataset);

    ret = H5Gclose (group);

   /*
    * Create group creation property list and set it to allow creation
    * of intermediate groups.
    */
    gcpl = H5Pcreate (H5P_LINK_CREATE);
    ret = H5Pset_create_intermediate_group (gcpl, 1);

    /*
     * Create a group named "/All_Data/VIIRS-MOD-GTM-EDR-GEO_ALL" in the file.
     */
    group = H5Gcreate (file, "/Datat_Products/VIIRS-MOD-GTM-EDR-GEO", gcpl, H5P_DEFAULT, H5P_DEFAULT);
   
    ret = H5Gclose (group);

    /*
     * Close the file.
     */
    ret = H5Fclose(file);

    return 0;
}

