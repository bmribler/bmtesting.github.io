/*
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
*/

/*
 * Description: Reads and test various hl_region HL functions on a datafile format 
 * similar to that specified in the Geolocation Product VIIRS-MOD-EDR-GEO 
 * (Product: ID GMGTO) of the Ground Integrated Data Processing System for NPOESS. 
 * It reads both little (GMGTO_LE.h5) and big endian formats (GMGTO_BE.h5). 
 */

#include <stdlib.h>
#include <string.h>
#include "hdf5.h"
#include "hdf5_hl.h"
#include "hl_region_H5LTpublic.h"
#include "hl_region_H5LRpublic.h"

#define dsetnamev "DS2"
#define dsetnamer "LAT_LONG"
#define num_flags  4

#define TESTING(WHAT)  {printf("%-70s", "Testing..." WHAT); fflush(stdout);}
#define PASSED()       {puts(" PASSED");fflush(stdout);}
#define FAILED()       {puts("*FAILED*");fflush(stdout);}

/* used by H5Lvisit_by_name */ 
static size_t  _regref_to_all_start;  /* starting index counter for placing data into 1D array, used by H5LRcreate_ref_to_all */ 
static H5R_type_t _regref_type;       /* type of data to create region references for */


/************************************************************

  Operator function:
     (1) Compares the number of elements in the region references
         to that of the dataset (operator_data). 

 ************************************************************/


static herr_t 
op_func (hid_t loc_id, const char *name, const H5O_info_t *info, void *operator_data)
{
    herr_t status; 
    hid_t dtype_id=-1, sid=-1;
    hid_t dtype_id2;
    hid_t sid2;
    hssize_t npoints, npoints_ref;
    hsize_t coord[1][1];
    hdset_reg_ref_t  reg_out[1];
    hid_t dataspace_id_reg;
    hsize_t dims[1];              /* array of the size of source dimension, reading into 1D array */
    hid_t mem_space = -1;
    hsize_t dims_out[1];
    if(_regref_type == H5R_DATASET_REGION) {

      if(info->type == H5O_TYPE_DATASET) {

	/* Open the dataset for a given path */
	if( ( dtype_id = H5Dopen2(loc_id, name, H5P_DEFAULT) ) < 0) goto out;
	
	/* Get the dataspace of the dataset */
	if( ( sid = H5Dget_space(dtype_id) ) < 0) goto out;
	
	/* Select the entire extent of the dataspace */
	if( H5Sselect_all( sid ) < 0 ) goto out;
	
	npoints = H5Sget_simple_extent_npoints(sid);
	
	if(H5Dclose(dtype_id) != 0) goto out;
	dtype_id = -1;
	if(H5Sclose(sid) != 0) goto out;
	sid = -1;

	/* Open the dataset for a given path */
	if( ( dtype_id2 = H5Dopen2(loc_id, "/PixelBlock/RegRef", H5P_DEFAULT) ) < 0) goto out;
	
	/* Get the dataspace of the dataset */
	if( ( sid2 = H5Dget_space(dtype_id2) ) < 0) goto out;
	
	/* get the extent of the region reference array */
	if( (H5Sget_simple_extent_dims(sid2, dims_out, NULL  ) )<1) goto out;
	/* only inquire the region references that were created (i.e. not /PixelBlock/RegRef itself) */
	if( _regref_to_all_start < dims_out[0] ) {
	  coord[0][0] = _regref_to_all_start;
	  status = H5Sselect_elements(sid2, H5S_SELECT_SET, 1, &coord[0][0]  );
	  if(status !=0) goto out;
	  
	  /* read the region reference */
	  dims[0] = 1;
	  mem_space = H5Screate_simple (1, dims, NULL);
	  status = H5Dread(dtype_id2, H5T_STD_REF_DSETREG,  mem_space, sid2, H5P_DEFAULT, reg_out);
	  if(status !=0) goto out;
	  
	  dataspace_id_reg = H5Rget_region( dtype_id2, H5R_DATASET_REGION, reg_out );
	  
	  npoints_ref = H5Sget_simple_extent_npoints(dataspace_id_reg);
	  
	  if((int)npoints != (int)npoints_ref) {
	    printf(" ERROR: incorrect number of elements referenced for region references\n");
	    goto out;
	  }

	  H5Dclose(dtype_id2);
	  H5Sclose(sid2);
	  
	  _regref_to_all_start += 1;
	}

      } /* if dataset */
    }
    return 0;

 out:
    if(dtype_id > 0)
      H5Dclose(dtype_id);
    if(sid > 0)
      H5Sclose(sid);

    return -2;
}


static herr_t op_func_L (hid_t loc_id, const char *name, const H5L_info_t *info,
			 void *operator_data)
{
    herr_t          status;
    H5O_info_t      infobuf;

    /*
     * Get type of the object and display its name and type.
     * The name of the object is passed to this function by
     * the Library.
     */
    status = H5Oget_info_by_name (loc_id, name, &infobuf, H5P_DEFAULT);
    if(status < 0) return -1;

    return op_func (loc_id, name, &infobuf, operator_data);
}

static int test_reading( const char *filename )
{
  hid_t file_id;        /* file identifier */
    herr_t status;
    hsize_t i,j,k;
    size_t nlength;
    int rank_out;
    hid_t dtype;
    hid_t mtype;
    hsize_t *buf;
    char *name;
    size_t numelem;
    size_t size_ref;
    H5S_sel_type sel_type;
    hdset_reg_ref_t ref_new;
    char *filename_out;

    hsize_t block_coord[4] ={ 1, 1, 6, 8};
    hsize_t block_coordw[4] ={ 0, 0, 5, 7};
    int Height[6][8];

    hid_t space; /* handles */
    hid_t qf_dset;
    unsigned int offset[4] = {0,2,4,6};  /* Starting bits to be extracted from element */
    unsigned int length[4] = {2,2,2,2};  /* Number of bits to be extracted for each value */
    int qf_data[8][num_flags];
    hsize_t start[2]={2,1};
    hsize_t count[2] = {1,8};
    const char *lat_long_path[2];    /* paths to the coordinate data for the region references */
    const char *Pixel_path[2];
    hdset_reg_ref_t ref[2]; /* region references */
    hsize_t block_coord_lat_long[8] = { 2,0,2,7,2,0,2,7};
    hid_t space_id_ref;   /* region reference dataspace identifier */
    hid_t dset_id_ref;    /* region reference dataset identifiers*/
    hsize_t dims_ref[1] = {2}; /* region reference dimensions */
    hsize_t rdims[2];
    hid_t file_id_array[1]; /* file id of the region references */
    hsize_t block_coord_lat_long_a[4] = { 2,0,2,7};
    hsize_t block_coord_Pixel[8] = { 1,1,6,8,1,1,6,8};
    hsize_t dims_Pixel[2] =  {8,9};
    int data_Pixel[8][9];

    float rdata2[1][8];
    float rdata1[6][8]; /* read data buffer */

    filename_out = "./ex_GMGTO.h5";

    /* start from a fresh output file */
    system("rm -f ./ex_GMGTO.h5");

    /* Read a region of the data using corner coordinates of a block */
    status = H5LTread_region(filename,
			     "/All_Data/VIIRS-MOD-GTM-EDR-GEO_ALL/Height",
	                     block_coord,
 			     H5T_NATIVE_INT,
 			     Height);
    if(status<0) goto out;

    printf("SECTION OF HEIGHT DATA:");
    for (i=0; i<6; i++) {
      printf("\n[ ");
      for (j=0; j<8; j++) {
	printf("%3d ", Height[i][j]);
      }
      printf("]");
    }
    printf("\n");
    printf("\n");

    /* Copy a Height region */
    status = H5LTcopy_region(filename,
 			     "/All_Data/VIIRS-MOD-GTM-EDR-GEO_ALL/Height",
 	                     block_coord,
 			     filename_out,
  			     "/All_Data/Height",
  			     block_coordw);
    if(status<0) goto out;

    /* Copy Latitude and Longitude regions */

    lat_long_path[0] ="/All_Data/Latitude";
    lat_long_path[1] ="/All_Data/Longitude";

    status = H5LTcopy_region(filename,
 			     "/All_Data/VIIRS-MOD-GTM-EDR-GEO_ALL/Latitude",
 	                     block_coord,
 			     filename_out,
  			     lat_long_path[0],
  			     block_coordw);
    if(status<0) goto out;

    status = H5LTcopy_region(filename,
 			     "/All_Data/VIIRS-MOD-GTM-EDR-GEO_ALL/Longitude",
 	                     block_coord,
 			     filename_out,
  			     lat_long_path[1],
  			     block_coordw);
    if(status<0) goto out;

    /* Read and print the Latitude destination region */
    for (k=0; k<2; k++) {
      
      file_id = H5Fopen(filename_out, H5F_ACC_RDWR,  H5P_DEFAULT);
      status = H5LTread_dataset(file_id, lat_long_path[k], H5T_NATIVE_FLOAT, rdata1);
      status = H5Fclose(file_id);
      
      printf("DATA AFTER H5LTCOPY_REGION: [(%d,%d)-(%d,%d)] --> [(%d,%d)-(%d,%d)]",
	     (int)block_coord[0],(int)block_coord[1], (int)block_coord[2],(int)block_coord[3],
	     (int)block_coordw[0], (int)block_coordw[1],(int)block_coordw[2], (int)block_coordw[3]);
      
      for (i=0; i< 6; i++)
	{
	  printf("\n  [ ");
	  for (j=0; j< 8; j++) {
	    printf("%6.4f ", rdata1[i][j]);
	  }
	  printf("]");
	}
      printf("\n");
    }
    printf("\n");

    /* Read the bit-field data */

    /* 
     * Open the data set
     */
    file_id = H5Fopen(filename,  H5F_ACC_RDONLY,  H5P_DEFAULT);
    qf_dset = H5Dopen (file_id, "/All_Data/VIIRS-MOD-GTM-EDR-GEO_ALL/QF1_VIIRSGTMGEO",H5P_DEFAULT );

    space = H5Dget_space(qf_dset);

    status = H5Sselect_hyperslab(space, H5S_SELECT_SET, start, NULL, count, NULL);

    /* select only a row to read and print */

    /*
     * For each element read the value that takes first two bits and
     * store it in a char buffer. This selects elements 1-8 of row 2
     */
    status = H5LTread_bitfield_value(qf_dset, num_flags, offset, length,
				     space, (int*)qf_data);

    if(status<0) goto out;


    printf("BITFIELD DATA:\n");
    printf (" [");
    for (i = 0; i<8; i++) {
      printf(" {");
      for (k = 0; k<num_flags; k++){
	printf(" %d ", qf_data[i][k]);
      }
      printf("} ");
    }
    printf("]\n");

    status = H5Dclose(qf_dset);
    status = H5Fclose(file_id);


    /* create a region reference to a row of the Latitude and Longitude coordinates */
    file_id = H5Fopen(filename_out, H5F_ACC_RDWR,  H5P_DEFAULT);

    status = H5LRcreate_region_references(file_id, 2, lat_long_path, 
					  block_coord_lat_long, ref);
    if(status<0) goto out;

    /*
     * Write the region references to the file
     */
    space_id_ref = H5Screate_simple(1, dims_ref, NULL);
    dset_id_ref = H5Dcreate2(file_id, dsetnamer, H5T_STD_REF_DSETREG, space_id_ref, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Dwrite(dset_id_ref, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT,ref);
    /*
     * Close all objects.
     */
    status = H5Sclose(space_id_ref);
    status = H5Dclose(dset_id_ref);
    status = H5Fclose(file_id);

    nlength = 0;
    rank_out = 0;
    

    /* Obtain information about the data associated with the region reference */
    file_id = H5Fopen(filename_out, H5F_ACC_RDWR,  H5P_DEFAULT);
    status = H5LRget_region_info(file_id,
				 (const hdset_reg_ref_t*)ref[0],
				 &nlength,
				 NULL,
				 &rank_out,
				 NULL,
				 &sel_type,
				 &numelem,
				 NULL);

    if(status<0) goto out;

    /* allocate the name */
    name = (char *) malloc (nlength);

    /* allocate space for hyperslab block
                            | NUMBLOCKS |   RANK    |  SIZEOF DATA TYPE | NUMBER COORDINATE POINTS | */
    buf = (hsize_t *)malloc(  numelem   * rank_out  *   sizeof(hsize_t) *          2                );

    /* Get region refererce information, hyperslab coordinates returned in buf */
    status = H5LRget_region_info(file_id,
				 (const hdset_reg_ref_t*)ref[0],
				 &nlength,
				 name,
				 &rank_out,
				 &dtype,
				 &sel_type,
				 &numelem,
				 buf );

    if(status<0) goto out;

    rdims[0] = buf[2] - buf[0] + 1;
    rdims[1] = buf[3] - buf[1] + 1;

    size_ref = H5Tget_size( dtype );
 
    float **rdata;           /* buffer to read data into */
    /*
     * Allocate array of pointers to rows.
     */
    rdata = (float **) malloc (rdims[0] * size_ref);

    /*
     * Allocate space for integer data.
     */
    rdata[0] = (float *) malloc (rdims[0] * rdims[1] * size_ref);

    /*
     * Set the rest of the pointers to rows to the correct addresses.
     */
    for (i=1; i<rdims[0]; i++)
        rdata[i] = rdata[0] + i * rdims[1];

    /* Read the latitude using a region reference and print it. */
    /* Firts, find the appropriate memory type to read in */
    mtype = H5Tget_native_type(dtype, H5T_DIR_ASCEND);

    status = H5LRread_region(file_id,(const hdset_reg_ref_t*)&ref[0],
			     mtype,
			     &numelem,
			     rdata[0]);

    if(status<0) goto out; 
    
    printf("\nREGION REFERENCED FOR LATITUDE 2D HYPERSLAB WITH");
    printf(" COORDINATES (%d,%d)-(%d,%d):",
	   (int)buf[0],(int)buf[1],(int)buf[2],(int)buf[3]);

    for (i=0; i< rdims[0]; i++)
      {
	printf("\n  [ ");
	for (j=0; j< rdims[1]; j++) {
	  printf("%f ", rdata[i][j]);
	}
	printf("]");
      }
    printf("\n\n");

    free(rdata);


    /*
     * Create a new dataset from the data pointed to by the region references 
     */

    file_id_array[0] = file_id;

    status = H5LRmake_dataset(file_id, "/All_Data/copy/Latitude", H5T_NATIVE_FLOAT, 1, file_id_array, ( const hdset_reg_ref_t *)ref[0]);

    if(status<0) goto out;

    /* print the newly created data set */

    status = H5LTread_dataset (file_id, "/All_Data/copy/Latitude", H5T_NATIVE_FLOAT, rdata2);
    
    printf("FULL 2D DATA CREATED BY LIST OF REGION REFERENCES:");
    for (i=0; i<1; i++) {
      printf("\n[ ");
      for (j=0; j<8; j++) {
	printf("%6.4f ", rdata2[i][j]);
      }
      printf("]");
    }
    printf("\n\n");

    status = H5Fclose(file_id);

    /* copy the region reference into a block_coord */

    file_id = H5Fopen(filename_out,  H5F_ACC_RDWR,  H5P_DEFAULT);

    status = H5LRcopy_region(file_id, &ref[0], filename_out, "/All_Data/copy_ref_Latitude", block_coord_lat_long_a);
    if(status<0) goto out;

    status = H5LRcopy_region(file_id, &ref[1], filename_out, "/All_Data/copy_ref_Longitude", block_coord_lat_long_a);
    if(status<0) goto out;

    /* read the latitude region that was copied from the region reference */
    status = H5LTread_dataset(file_id,"/All_Data/copy_ref_Latitude",H5T_NATIVE_FLOAT, rdata2);
    
    if(status<0) goto out;

    printf("LATITUDE DATA CREATED BY COPY OF DATA POINTED TO BY REGION REFERENCES:");
    for (i=0; i<block_coord_lat_long[2]-block_coord_lat_long[0]+1; i++) {
      printf("\n[ ");
      for (j=0; j<block_coord_lat_long[3]-block_coord_lat_long[1]+1; j++) {
	printf("%6.4f ", rdata2[i][j]);
      }
      printf("]");
    }
    printf("\n\n");

    /* read the longitude region that was copied from the region reference */
    status = H5LTread_dataset(file_id,"/All_Data/copy_ref_Latitude",H5T_NATIVE_FLOAT, rdata2);
    
    if(status<0) goto out;

    printf("LONGITUDE DATA CREATED BY COPY OF DATA POINTED TO BY REGION REFERENCES:");
    for (i=0; i<block_coord_lat_long[2]-block_coord_lat_long[0]+1; i++) {
      printf("\n[ ");
      for (j=0; j<block_coord_lat_long[3]-block_coord_lat_long[1]+1; j++) {
	printf("%6.4f ", rdata2[i][j]);
      }
      printf("]");
    }
    printf("\n\n");

    status = H5Fclose(file_id);

    /* Create a region reference to the PixelColSDR data */

    Pixel_path[0] = "/All_Data/VIIRS-MOD-GTM-EDR-GEO_ALL/PixelColSDR";
    Pixel_path[1] = "/All_Data/VIIRS-MOD-GTM-EDR-GEO_ALL/PixelRowSDR";

    file_id = H5Fopen(filename, H5F_ACC_RDWR,  H5P_DEFAULT);

    status = H5LRcreate_region_references(file_id, 2, Pixel_path,
					  block_coord_Pixel, ref);

    if(status<0) goto out;

    status = H5Fclose(file_id);

    /* Create a new data set to copy the pixel data pointed to by the region reference */
    file_id = H5Fopen(filename_out, H5F_ACC_RDWR,  H5P_DEFAULT);

    /*
     * initialize dataset.
     */
    for (i=0; i<8; i++) {
      for (j=0; j<9; j++) {
	data_Pixel[i][j] = 0;
      }
    }

    status = H5LTmake_dataset ( file_id, "/PixelColSDR", 2, dims_Pixel, H5T_NATIVE_INT, data_Pixel);

    status = H5LTmake_dataset ( file_id, "/PixelRowSDR", 2, dims_Pixel, H5T_NATIVE_INT, data_Pixel);

    status = H5Fclose(file_id);
    /*
     * copy the dataset pointed to by ref[0] into the /PixelCorSDR and /PixelRowSDR data
     * defined by the coordinates block_coord_Pixel. ref_new is the region
     * reference to this new hyperslab.
     */
    file_id = H5Fopen(filename, H5F_ACC_RDWR,  H5P_DEFAULT);

    status =  H5LRcopy_reference( file_id, &ref[0], filename_out,
				   "/PixelColSDR", block_coord_Pixel, &ref_new);
    status =  H5LRcopy_reference( file_id, &ref[1], filename_out,
				   "/PixelRowSDR", block_coord_Pixel, &ref_new);

    status = H5Fclose(file_id);

    file_id = H5Fopen(filename_out, H5F_ACC_RDWR,  H5P_DEFAULT);

    /* read the Pixel Column data that was copied from the region reference */
    status = H5LTread_dataset(file_id,"/PixelColSDR", H5T_NATIVE_INT, data_Pixel);

    printf("COLUMN PIXEL DATA CREATED BY COPY OF DATA POINTED TO BY REGION REFERENCES:");
    for (i=0; i<8; i++) {
      printf("\n[ ");
      for (j=0; j<9; j++) {
	printf("%3d ", data_Pixel[i][j]);
      }
      printf("]");
    }
    printf("\n\n");

    status = H5LTread_dataset(file_id,"/PixelRowSDR", H5T_NATIVE_INT, data_Pixel);

    printf("ROW PIXEL DATA CREATED BY COPY OF DATA POINTED TO BY REGION REFERENCES:");
    for (i=0; i<8; i++) {
      printf("\n[ ");
      for (j=0; j<9; j++) {
	printf("%3d ", data_Pixel[i][j]);
      }
      printf("]");
    }
    printf("\n\n");

    status = H5Fclose(file_id);

    /* Copy the region refernece into the new file */

    file_id = H5Fopen(filename_out, H5F_ACC_RDWR,  H5P_DEFAULT);
    status = H5LRcreate_ref_to_all(file_id, "/All_Data","/PixelBlock/RegRef", H5_INDEX_NAME, H5_ITER_INC, H5R_DATASET_REGION);

    if(status<0) goto out;

    /* print results */
    _regref_to_all_start=0;
    _regref_type = H5R_DATASET_REGION;

    status = H5Lvisit_by_name(file_id, "/All_Data", H5_INDEX_NAME, H5_ITER_INC, op_func_L, NULL, H5P_DEFAULT );

    
    if(status<0) {
      printf("REGION REFERENCES CREATION TO ALL DATA IN /All_Data FAILED\n\n");
      status = H5Fclose(file_id);
    }
    else {
      printf("REGION REFERENCES CREATION TO ALL DATA IN /All_Data SUCCESSFUL\n\n");
      status = H5Fclose(file_id);
      goto out;
    }

    return 0;
out:
/*     FAILED(); */
    return -1;
}

/*-------------------------------------------------------------------------
* the main program
*-------------------------------------------------------------------------
*/
int main( void )
{
    int  nerrors=0;

    printf("\n****************************************************\n");
    printf("* TEST READING NPOESS DATA FORMAT IN LITTLE ENDIAN *\n");
    printf("****************************************************\n\n");
    nerrors += test_reading("ex_GMGTO_LE.h5");  

    printf("\n*************************************************\n");
    printf("* TEST READING NPOESS DATA FORMAT IN BIG ENDIAN *\n");
    printf("*************************************************\n\n");
    nerrors += test_reading("ex_GMGTO_BE.h5");

    /* check for errors */
    if(nerrors)
        goto error;

    return 0;

error:
    return -1;

}

