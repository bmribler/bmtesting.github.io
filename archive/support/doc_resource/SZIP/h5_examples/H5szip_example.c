/*
 * Example illustrates the use of SZIP compression in HDF5
 */

#include "hdf5.h"
#define NX 500
#define NY 600
#define CH_NX 100
#define CH_NY 25

int main(void)
{
   hid_t file, data_space, dataset32, properties;
   float buf[NX][NY];
   float buf_r[NX][NY];
   hsize_t dims[2], chunk_size[2];
   int i, j;
   unsigned szip_options_mask;
   unsigned szip_pixels_per_block;

   /* Initiliaze data buffer with some bogus data. */
   for (i=0; i < NX; i++) {
       for (j=0; j < NY; j++) 
           buf[i][j] = i + j;
   }

   /* Describe the size of the array. */
   dims[0] = NX;
   dims[1] = NY;
   data_space = H5Screate_simple (2, dims, NULL);

  /*
   * Create a new file using read/write access, default file 
   * creation properties, and default file access properties.
   */
   file = H5Fcreate ("test.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

  /*
   * Set the dataset creation property list to specify that
   * the raw data is to be partitioned into 100x100 element
   * chunks and that each chunk is to be compressed.
   */
   chunk_size[0] = CH_NX;
   chunk_size[1] = CH_NY;
   properties = H5Pcreate (H5P_DATASET_CREATE);
   H5Pset_chunk (properties, 2, chunk_size);

  /* 
   * Set parameters for SZIP compression; check the description of
   * the H5Pset_szip function in the HDF5 Reference Manual for more 
   * information.
   */
   szip_options_mask=H5_SZIP_NN_OPTION_MASK;
   szip_pixels_per_block=32;

   H5Pset_szip (properties, szip_options_mask, szip_pixels_per_block);

  /*
   * Create a new dataset within the file.  The datatype
   * and data space describe the data on disk, which may
   * be different from the format used in the application's
   * memory.
   */

   dataset32 = H5Dcreate (file, "datasetF32", H5T_NATIVE_FLOAT,
                       data_space, properties);


  /*
   * Write the array to the file.  The datatype and dataspace
   * describe the format of the data in the `buf' buffer.
   * The raw data is translated to the format required on disk, 
   * as defined above.  We use default raw data transfer properties.
   */

   H5Dwrite (dataset32, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL,
            H5P_DEFAULT, buf);


  /*
   * Read the array.  This is similar to writing data,
   * except the data flows in the opposite direction.
   * Note: Decompression is automatic.
   */

   H5Dread (dataset32, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL,
           H5P_DEFAULT, buf_r);


   H5Dclose (dataset32);
   H5Sclose (data_space);
   H5Pclose (properties);
   H5Fclose (file);
}

