/********* LAST MODIFIED: 11/19/1998 **********/

#include <mfhdf.h>
#include <hdf.h>
#include <stdio.h>
#include <stdlib.h>

#define Y_LENGTH 360
#define X_LENGTH 720

/*************************************************************/
/*                                                           */
/* This program creates a chunked SDS file corresponding     */
/* to the data in the Pathfinder HDF file.                   */
/* The data in the Pathfinder HDF files had 2 Raster8 images */
/* This program converts this data into the new SDS chunked  */
/* format. You can use this as an example of how to convert  */
/* your data into chunked SDS's                              */
/*                                                           */
/* Input Parameters:                                         */
/*  argv[1] - Pathfinder HDF filename                        */
/*  argv[2] - length of chunk in X direction                 */
/*  argv[3] - the character 'X'                              */
/*  argv[4] - length of chunk in Y direction                 */
/*  argv[5] - Type of compression (gzip, rle or none)        */
/*  argv[6] - Output file name, eg. "chunked.hdf"            */
/*                                                           */
/* This program copies the raster images along with their    */
/* attributes to corresponding SD datasets in the chunked    */
/* HDF files. The global attributes of the file are also     */
/* copied.                                                   */
/*                                                           */ 
/* status=0 is GOOD, and status=-1 is BAD                    */
/* If you see any -1's in the output, something went wrong   */
/*                                                           */
/* -- Apu Kapadia (apu@ncsa.uiuc.edu, akapadia@uiuc.edu)     */
/*************************************************************/

/* set this to TRUE if you want the program to exit
   on any status = -1 */
int quit=TRUE;

double fabs(double x);

/**
 * This function saves the specified attribute from the original 
 * pathfinder HDF file, to the new chunked SDS file
 */ 
void save_attr(int32 source_sdsid, int32 target_sdsid, int32 num, char name[], 
	       int32 nt, int32 count)
{
  char *tbuff;
  int32 dsize;
  int32 status;
  
  /* figure out the size of the attribute */
  dsize = DFKNTsize(nt);  
  if(dsize < 1) return;
  
  /* allocate space for the attribute */
  tbuff = HDgetspace(dsize * (count + 1));
  if(tbuff == NULL) return;
  
  /* retrieve the attribute from the original file */
  status = SDreadattr(source_sdsid, num, tbuff);

  /* write the attribute to the new file */
  status = SDsetattr(target_sdsid, name, nt, count, (VOIDP) tbuff);
  printf("   SDsetattr \"%s\" status = %d\n", name, status); 
  fflush(stdout);
}

void toLowerCase(char upperCaseString[], int len)
{
  int i;
  char c;

  for(i=0;i<len;i++)
    {
      c = upperCaseString[i];
      if(c >= 'A' && c <='Z')
	upperCaseString[i] = c - 'A' + 'a';
    }
}

/**
 * This function is responsible for quitting the program if a status
 * of -1 is received at any stage. To disable this function, set 
 * the global variable: quit = FALSE
 */
void quitProgram()
{
  if(quit)
    {
      printf("\nReceived a -1 status, quitting...\n");
      fflush(stdout);
      exit(0);
    }
}

/**
 * This function simply checks the status.
 */
void check(int id)
{
  if(id < 0)
    quitProgram();
}

/**
 * this function is necessary for chunksizes that don't fit evenly into 
 * the dataset. If the correct cachesize isn't set, then there can be 
 * SEVERE performance degradation. For example, if the fastest changing 
 * dimension is equivalent to 5.5 chunks, then we must set the cachesize 
 * to 6 and not 5. A value of 6 v/s 5 could mean hours or even days!!
 */
int32 roundDouble(double x)
{
   int32 abs_x = (int32) fabs(x);
   if(x - abs_x > 0)
     return abs_x+1;
   else
     return abs_x;
}

/**
 * See the argument list above
 */
main(int argc, char* argv[])

{
  int32 i, ii, j, chunkflag, cachesize;                   
  int32 sdtmp=FAIL;
  int32 sdstmp=FAIL;
  intn     status_n;   
  int32    status, status_32;  
  int nonzero;
  int32 maxChunksPerDim;
  int32 chunkFlag;
  int compression_len;

  HDF_CHUNK_DEF c_def, r_def; 
  int32 dims[2], start[2], edges[2], rank; 

  uint8 *picture;

  char *filename, *compression, *outfile;
  int32 chunk_dims[2];
  int32 fid;
 
  int32 nsds,ngattr;

  int32 sds_id;
  int32  nt,nattrs;
  int32  dimsizes[50];
  char name[512];

  int32  count;
  
  /*   Parse Arguments   */

  printf("Parsing arguments..."); fflush(stdout);

  toLowerCase(argv[3], strlen(argv[3]));

  /* Check input args */
  if(argc != 7 || (strcmp(argv[3], "x") != 0))
    {
      printf("\nUsage: %s <filename> <chunk_dim x> X <chunk_dim y> <compression> <outfile>\n", argv[0]);
      printf("Example: %s test.hdf 256 X 512 gzip testchunked.hdf\n", argv[0]);
      exit(0);
    }
  filename = argv[1];
  outfile = argv[6];
  compression = argv[5];
  compression_len = strlen(compression);

  toLowerCase(compression, compression_len);

  chunk_dims[1] = atoi(argv[2]);
  chunk_dims[0] = atoi(argv[4]);
  printf("done\n");


  /*  Open files for input/output  */

  printf("Opening input file..."); fflush(stdout);
  fid = SDstart(filename, DFACC_RDONLY); 
  printf("fid = %d\n", fid);
  check(fid);

  printf("Opening output file..."); fflush(stdout);
  if ((sdtmp=SDstart(outfile, DFACC_CREATE))==FAIL){
    printf("sds_subset: failed to create chunked HDF file");
    exit(0);
  }
  printf("sdtmp = %d\n", sdtmp);

  /* Allcate picture array */
  printf("Allocating picture array...");fflush(stdout);
  picture = (uint8 *)HDmalloc(X_LENGTH * Y_LENGTH);
  printf("done\n");

  /* Get Information about the input HDF file */

  printf("Getting input file info..."); fflush(stdout);
  status = SDfileinfo(fid, &nsds, &ngattr);
  printf("status = %d\n", status);
  check(status);

  if(nsds + ngattr < 1) 
    {
      printf("Error with attributes...exit\n");
      exit(0);
    }


  /* Copy the Global Attributes to the new Chunked HDF file */

  printf("Looping through %d global attributes\n", ngattr);
  if(ngattr) {

    /* loop through attributes */
    for(i = 0; i < ngattr; i++) {
      /* get attribute info from input file */
      status = SDattrinfo(fid, i, name, &nt, &count);
      check(status);
      /* write attribute to new output file */
      save_attr(fid, sdtmp, i, name, nt, count);
    }
  }
  printf("Done saving global attributes\n");


  /* Loop through the datasets and save them as chunked datasets */
  printf("Looping through %d datasets\n", nsds);
 
  for(i = 0; i < nsds; i++) {
    printf("\nBegin dataset %d\n", i);

    printf(" Selecting dataset %d from input file...", i);
    
    /* select dataset from input file */
    sds_id = SDselect(fid, i);
    printf("sds_id = %d\n", sds_id);
    check(sds_id);

    /* Get Information on the dataset  */
    
    printf(" Getting info from dataset %d from input file...", i);
    fflush(stdout);
    /* get dataset info from the input file */
    status = SDgetinfo(sds_id, name, &rank, dimsizes, &nt, &nattrs);
    printf("status = %d\n", status);
    check(status);

    edges[0] = Y_LENGTH;
    edges[1] = X_LENGTH;

    /* Since X is the fastest changing dimension */
    /* note: this rounding is necessary, see comments for roundDouble() */
    maxChunksPerDim = roundDouble((double)edges[1]/(double)chunk_dims[1]); 

    printf("maxChunksPerDim = %d\n", maxChunksPerDim);
    
    /* these are the starting cooridinates of the dataset */
    start[0] = 0;
    start[1] = 0;


    /* Create the dataset in the output file  */
    /* we know that the SD dataset is of type unsigned int */
    nt = DFNT_UINT8;
 
    printf(" Creating dataset %d in output file...", i); fflush(stdout);
    sdstmp = SDcreate(sdtmp, name, nt, rank,
		      edges); 

    printf("sdstmp = %d\n", sdstmp);
    check(sdstmp);

    /* Read image from the HDF file */    
    printf(" Getting image from dataset %d from input file...", i); 
    fflush(stdout);
    status = DFR8getimage(filename, (VOIDP)picture, X_LENGTH,  
			  Y_LENGTH, NULL);
    printf("status = %d\n", status);
    check(status);

    /*  Copy Attributes of this dataset into the new Dataset  */
 
    printf(" Looping through attributes for dataset %d\n", i);
    for(j = 0; j < nattrs; j++) {
      
      /* Find the name, data type and number of values for local attributes */
      status = SDattrinfo(sds_id, j, name, &nt, &count);
      check(status);
      save_attr(sds_id, sdstmp, j, name, nt, count);
    }


    /*  Define the Chunking Parameters  */

    printf(" Defining chunking parameters...");

    /* specify the chunking dimensions */
    c_def.chunk_lengths[0] = chunk_dims[0];
    c_def.chunk_lengths[1] = chunk_dims[1];
    
    /* deal with the specified compression format */
    if(strcmp(compression, "gzip") == 0)
      {
	c_def.comp.chunk_lengths[0] = chunk_dims[0];
	c_def.comp.chunk_lengths[1] = chunk_dims[1];
	c_def.comp.comp_type = COMP_CODE_DEFLATE; /* GZIP */
	c_def.comp.cinfo.deflate.level = 6;  /* GZIP */  
	chunkFlag = HDF_CHUNK | HDF_COMP;
      }
    else if(strcmp(compression, "rle") == 0)
      {
	c_def.comp.comp_type = COMP_CODE_RLE; /* RLE */
	c_def.comp.chunk_lengths[0] = chunk_dims[0];
	c_def.comp.chunk_lengths[1] = chunk_dims[1];
	chunkFlag = HDF_CHUNK | HDF_COMP; /* RLE */
      }
    else if(strcmp(compression, "nbit") == 0)
      {
	c_def.nbit.chunk_lengths[0] = chunk_dims[0];
	c_def.nbit.chunk_lengths[1] = chunk_dims[1];
	chunkFlag = HDF_CHUNK | HDF_NBIT; /* NBIT */
      }
    else if(strcmp(compression, "none") == 0)
      {
	c_def.comp.chunk_lengths[0] = chunk_dims[0];
	c_def.comp.chunk_lengths[1] = chunk_dims[1];
	c_def.comp.comp_type = COMP_CODE_NONE; /* NONE */
	chunkFlag = HDF_CHUNK; /* NONE */
      }
    
    printf("done\n");


    /* Set chunk parameters */
    
    printf(" Setting the chunk parameters..."); fflush(stdout);
    status = SDsetchunk(sdstmp, c_def, chunkFlag);
    printf("done\n");
    check(status);

    /* Set chunk cache size */
    chunkflag = 0; 
    cachesize = maxChunksPerDim;
    printf(" setting chunk CACHE to %d\n", cachesize); 
    status = SDsetchunkcache(sdstmp, cachesize, chunkflag);
    check(status);

    /* Write chunked data  */    
    printf(" writing chunked data for dataset %d...", i);
    fflush(stdout);
    status = SDwritedata(sdstmp, start, NULL, edges, (VOIDP) picture); 
    printf("status = %d\n", status);
    check(status);

    /* close files */
    SDendaccess(sdstmp);
    SDendaccess(sds_id);
    printf("Done with dataset %d\n", i);
  }

  
  /* Terminate access to the SDS interface and close the file. */  

  printf("Closing input file...");
  fflush(stdout);
  status = SDend(fid);
  printf("done\n");

  printf("Closing output file...");
  fflush(stdout);
  status = SDend(sdtmp);
  printf("done\n");

} 

 
