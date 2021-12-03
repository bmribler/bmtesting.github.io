/* This program writes/reads 64-bit floating data using SZIP compression */

#include <mfhdf.h>
#include <szlib.h>

#define FILE_NAMEfl64	"SDS_fl64_sziped.hdf"
#define SDS_NAME	"SzipedData"

#define RANK		2
#define WIDTH		6
#define LENGTH		9

int main()
{
    /************************* Variable declaration **************************/

    int32	sd_id, sds_id;
    intn 	status;
    int32	dim_sizes[2], array_rank, num_type, attributes;
    char	name[MAX_NC_NAME];
    comp_info	c_info;
    int32       start[2], edges[2];
    float64     fill_value = 0;   /* Fill value */
    int         i,j;
    int		num_errs = 0;    /* number of errors so far */
    float64	out_data[LENGTH][WIDTH];
    float64	in_data[LENGTH][WIDTH]={
	   			 100.0,100.0,200.0,200.0,300.0,400.0,
	   			 100.0,100.0,200.0,200.0,300.0,400.0,
				 100.0,100.0,200.0,200.0,300.0,400.0,
				 300.0,300.0,  0.0,400.0,300.0,400.0,
				 300.0,300.0,  0.0,400.0,300.0,400.0,
				 300.0,300.0,  0.0,400.0,300.0,400.0,
				   0.0,  0.0,600.0,600.0,300.0,400.0,
				 500.0,500.0,600.0,600.0,300.0,400.0,
				   0.0,  0.0,600.0,600.0,300.0,400.0};

    /********************* End of variable declaration ***********************/

    /* Create the file and initialize SD interface */
    sd_id = SDstart (FILE_NAMEfl64, DFACC_CREATE);

    /* Create the SDS */
    dim_sizes[0] = LENGTH;
    dim_sizes[1] = WIDTH;
    sds_id = SDcreate (sd_id, SDS_NAME, DFNT_FLOAT64, RANK, dim_sizes);

    /* Define the location, pattern, and size of the data set */
    for (i = 0; i < RANK; i++) {
	start[i] = 0;
	edges[i] = dim_sizes[i];
	}

    /* Fill the SDS array with the fill value */
    status = SDsetfillvalue (sds_id, (VOIDP)&fill_value);

    /* Initialization for SZIP */
    c_info.szip.pixels_per_block = 2;
    c_info.szip.options_mask = SZ_NN_OPTION_MASK;
    c_info.szip.options_mask |= SZ_RAW_OPTION_MASK;

    /* Set the compression */
    status = SDsetcompress (sds_id, COMP_CODE_SZIP, &c_info);

    /* Write data to the SDS */
    status = SDwritedata(sds_id, start, NULL, edges, (VOIDP)in_data);

    /* Terminate access to the data set */
    status = SDendaccess (sds_id);

    /* Terminate access to the SD interface and close the file to 
       flush the compressed info to the file */
    status = SDend (sd_id);

    /*
     * Verify the compressed data
     */

    /* Reopen the file and select the first SDS */
    sd_id = SDstart (FILE_NAMEfl64, DFACC_READ);

    sds_id = SDselect (sd_id, 0);

    /* Wipe out the output buffer */
    memset(&out_data, 0, sizeof(out_data));

    /* Read the data set */
    start[0] = 0;
    start[1] = 0;
    edges[0] = LENGTH;
    edges[1] = WIDTH;
    status = SDreaddata (sds_id, start, NULL, edges, (VOIDP)out_data);

    /* Compare read data against input data */
    for (j=0; j<LENGTH; j++) 
    {
        for (i=0; i<WIDTH; i++)
	    if (out_data[j][i] != in_data[j][i])
	    {
		fprintf(stderr,"Bogus val in loc [%d][%d] in compressed dset, want %ld got %ld\n", j, i, (long)in_data[j][i], (long)out_data[j][i]);
		num_errs++;
	    }
    }

    /* Terminate access to the data set */
    status = SDendaccess (sds_id);

    /* Terminate access to the SD interface and close the file */
    status = SDend (sd_id);

    return 0;
}  
