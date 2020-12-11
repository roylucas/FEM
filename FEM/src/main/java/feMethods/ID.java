package feMethods;

public final class ID {
   private static int neq;
   private static int[][] id;

   public ID(int nnp, int ndf, int[][]idb) {
	   int[][] id = new int[ndf][nnp];
	   int neq = 0;
	   for(int i = 0;i<ndf;i++) {
			for(int j = 0;j<nnp;j++) {
				id[i][j] = -1;
			}
		}
		for(int e = 0; e<nnp;e++){
			for(int n = 0; n<ndf;n++){
				if(idb[n][e] == 0){
					id[n][e] = neq;
					neq = neq + 1;
				}
			}
		}
	   ID.neq = neq;
	   ID.id = id;
   }

   public int getneq() {
	   return neq;
   }

   public int[][] getIDtable() {
	   return id;
   }
}