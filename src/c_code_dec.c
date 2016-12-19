#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include <R.h>

// finds marginal table 
void findMargFreqs (int *data, int *nrow, int *ncol, int *dims, double *freq, double *margFreqs) {

  int i;
  
  int *cumDims = (int *) R_alloc (ncol[0], sizeof(int));
  
  if (cumDims == NULL) 
    error ("Memory allocation error.");
  
  cumDims[ncol[0] - 1] = 1;

  for (i = ncol[0] - 2; i >= 0; i--)
    cumDims[i] = cumDims[i+1] * dims[i+1];    
 
  int index = 0;

  for (i = 0; i < nrow[0] * ncol[0]; i++) {
    index = index + data[i] * cumDims [i % ncol[0]];   
    if ((i+1) % ncol[0] == 0) {
      margFreqs[index] += freq[i / ncol[0]];
      index = 0;      
    }
  }  
}

/*
void main () {

  int *data = malloc (128 * sizeof (int)); 
  data = (int[]){0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1};
  int nrow = 64;
  int ncol = 2;
  int *dims = malloc (2 * sizeof(int));
  dims = (int[]){2,2};
  int *data$freq = malloc (64 * sizeof(int));
  data$freq = (int[]) {44, 40, 112, 67, 129, 145, 12, 23, 35, 12, 80, 33, 109, 67, 7, 9, 23, 32, 70, 66, 50, 80, 7, 13, 24, 25, 73, 57, 51, 63, 7, 16, 5, 7, 21, 9, 9, 17, 1, 4, 4, 3, 11, 8, 14, 17, 5, 2, 7, 3, 14, 14, 9, 16, 2, 3, 4, 0, 13, 11, 5, 14,  4,  4};
  int *margFreqs = malloc (4 * sizeof(int));   
  int x = 5 / 4;
  printf("%d\n", x);
  findMargFreqs (data = data, nrow = nrow, ncol = ncol, dims = dims, data$freq = data$freq, margFreqs = margFreqs);
  
  int i;
 
  for (i = 0; i < 4; i++)
    printf ("%d ", margFreqs[i]);

  printf("\n");
}
*/
