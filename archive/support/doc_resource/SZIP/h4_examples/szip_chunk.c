/* This example writes/reads chunked and compressed dataset using SZIP compression */

#include <mfhdf.h>
#include <szlib.h>

#define FILE_NAME	"SDSchunkedsziped.hdf"
#define SDS_NAME_CH	"ChunkedData"
#define RANK_CH         2	/* rank of the chunked data set */
#define WIDTH_CH 	4	/* width of the chunked data set */
#define LENGTH_CH	9	/* length of the chunked data set */
#define CWIDTH		2	/* width of the chunk */
#define CLENGTH		3	/* length of the chunk */

#define RANK		2
#define WIDTH		6
#define LENGTH		9

int main()
{
   /************************* Variable declaration **************************/

   int32         sd_id, sds_id, sds_index;
   intn          status;
   int32         flag, maxcache, new_maxcache;
   int32         dim_sizes[2], origin[2];
   HDF_CHUNK_DEF c_def; /* Chunking definitions */ 
   int32         comp_flag;
   int16         all_data[LENGTH_CH][WIDTH_CH];
   int32         start[2], edges[2];
   int16         chunk_out[CLENGTH][CWIDTH];
   int16         row[CWIDTH] = { 5, 5 };
   int16         column[CLENGTH] = { 4, 4, 4 };
   int16         fill_value = 0;   /* Fill value */
   comp_coder_t  comp_type;        /* to retrieve compression type into */
   comp_info     cinfo;            /* compression information structure */
   int    	 num_errs = 0;     /* number of errors so far */
   int           i,j;
   /*
   * Define all chunks.  Note that chunks 4 & 5 are not used to write,
   * only to verify the read data.  The 'row' and 'column' are used
   * to write in the place of these chunks.
   */
          int16 chunk1[CLENGTH][CWIDTH] = { 1, 1,
                                            1, 1,
                                            1, 1 }; 

          int16 chunk2[CLENGTH][CWIDTH] = { 2, 2,
                                            2, 2,
                                            2, 2 }; 

          int16 chunk3[CLENGTH][CWIDTH] = { 3, 3,
                                            3, 3,
                                            3, 3 }; 

          int16 chunk4[CLENGTH][CWIDTH] = { 0, 4,
                                            0, 4,
                                            0, 4 }; 

          int16 chunk5[CLENGTH][CWIDTH] = { 0, 0,
                                            5, 5,
                                            0, 0 }; 

          int16 chunk6[CLENGTH][CWIDTH] = { 6, 6,
                                            6, 6,
                                            6, 6 };

    /* Initialize chunk lengths. */
    c_def.comp.chunk_lengths[0] = CLENGTH;
    c_def.comp.chunk_lengths[1] = CWIDTH;

    /* Create the file and initialize SD interface. */
    sd_id = SDstart (FILE_NAME, DFACC_CREATE);

    /* Create LENGTH_CHxWIDTH_CH SDS. */
    dim_sizes[0] = LENGTH_CH;
    dim_sizes[1] = WIDTH_CH;
    sds_id = SDcreate (sd_id, SDS_NAME_CH,DFNT_INT16, RANK_CH, dim_sizes);

    /* Fill the SDS array with the fill value. */
    status = SDsetfillvalue (sds_id, (VOIDP)&fill_value);

    /* Set parameters for Chunking/SZIP */
    c_def.comp.comp_type = COMP_CODE_SZIP;
    c_def.comp.cinfo.szip.pixels_per_block = 2;
    c_def.comp.cinfo.szip.options_mask = SZ_NN_OPTION_MASK;
    c_def.comp.cinfo.szip.options_mask |= SZ_MSB_OPTION_MASK;
    c_def.comp.cinfo.szip.bits_per_pixel = 16;
    comp_flag = HDF_CHUNK | HDF_COMP;
    status = SDsetchunk (sds_id, c_def, comp_flag);

    /* Set chunk cache to hold maximum of 3 chunks. */
    maxcache = 3;
    flag = 0;
    new_maxcache = SDsetchunkcache (sds_id, maxcache, flag);

    /* 
     * Write chunks using SDwritechunk function.  Chunks can be written 
     * in any order. 
     */

    /* Write the chunk with the coordinates (0,0). */
    origin[0] = 0;
    origin[1] = 0;
    status = SDwritechunk (sds_id, origin, (VOIDP) chunk1);

    /* Write the chunk with the coordinates (1,0). */
    origin[0] = 1;
    origin[1] = 0;
    status = SDwritechunk (sds_id, origin, (VOIDP) chunk3);

    /* Write the chunk with the coordinates (0,1). */
    origin[0] = 0;
    origin[1] = 1;
    status = SDwritechunk (sds_id, origin, (VOIDP) chunk2);

    /* Write chunk with the coordinates (1,2) using SDwritedata function. */
    start[0] = 6;
    start[1] = 2;
    edges[0] = 3;
    edges[1] = 2;
    status = SDwritedata (sds_id, start, NULL, edges, (VOIDP) chunk6); 

    /* Fill second column in the chunk with the coordinates (1,1) using 
     * SDwritedata function. */
    start[0] = 3;
    start[1] = 3;
    edges[0] = 3;
    edges[1] = 1;
    status = SDwritedata (sds_id, start, NULL, edges, (VOIDP) column); 

    /* Fill second row in the chunk with the coordinates (0,2) using 
     * SDwritedata function. */
    start[0] = 7;
    start[1] = 0;
    edges[0] = 1;
    edges[1] = 2;
    status = SDwritedata (sds_id, start, NULL, edges, (VOIDP) row); 
           
    /* Terminate access to the data set. */
    status = SDendaccess (sds_id);

    /* Terminate access to the SD interface and close the file. */
    status = SDend (sd_id);

    /*
     * Verify the compressed data
     */

    /* Reopen the file and access the first data set. */
    sd_id = SDstart (FILE_NAME, DFACC_READ);
    sds_index = 0;
    sds_id = SDselect (sd_id, sds_index);

    /* Retrieve compression information about the dataset */
    comp_type = COMP_CODE_INVALID;  /* reset variables before retrieving info */
    memset(&cinfo, 0, sizeof(cinfo)) ;

    status = SDgetcompress(sds_id, &comp_type, &cinfo);

    /* Read the entire data set using SDreaddata function. */
    start[0] = 0;
    start[1] = 0;
    edges[0] = LENGTH_CH;
    edges[1] = WIDTH_CH;
    status = SDreaddata (sds_id, start, NULL, edges, (VOIDP)all_data);

    /* 
    * This is how the entire array should look like:
    *
    *          1 1 2 2
    *          1 1 2 2
    *          1 1 2 2
    *          3 3 0 4
    *          3 3 0 4
    *          3 3 0 4
    *          0 0 6 6
    *          5 5 6 6
    *          0 0 6 6
    */

    /* Read chunk #4 with the coordinates (1,1) and verify it. */
    origin[0] = 1;
    origin[1] = 1;    	
    status = SDreadchunk (sds_id, origin, chunk_out);

    for (j=0; j<CLENGTH; j++) 
    {
	for (i=0; i<CWIDTH; i++) 
	{
	    if (chunk_out[j][i] != chunk4[j][i])
	    {
		fprintf(stderr,"Bogus val in loc [%d][%d] in chunk #4, want %ld got %ld\n", j, i, chunk4[j][i], chunk_out[j][i]);
		num_errs++;
	    }
	}
    }

    /* 
    * Read chunk #5 with the coordinates (2,0) and verify it.
    */
    origin[0] = 2;
    origin[1] = 0;    	
    status = SDreadchunk (sds_id, origin, chunk_out);

    for (j=0; j<CLENGTH; j++) 
    {
	for (i=0; i<CWIDTH; i++) 
	    if (chunk_out[j][i] != chunk5[j][i])
	    {
		fprintf(stderr,"Bogus val in loc [%d][%d] in chunk #5, want %ld got %ld\n", j, i, chunk5[j][i], chunk_out[j][i]);
		num_errs++;
	    }
    }

    /* Terminate access to the data set. */
    status = SDendaccess (sds_id);

    /* Terminate access to the SD interface and close the file. */
    status = SDend (sd_id);

    return 0;

}   
