package Matrix_Algebra;
import java.text.DecimalFormat;


public class Matrix_Algebra implements Cloneable, java.io.Serializable{
	
	public double[][] GJ_Inverse(double[][] matrix_input){
		
		double[][] input = matrixCopy(matrix_input);
		double[][] GJI = new double[input.length][input[0].length];
		for(int e = 0;e<input.length;e++){
			GJI[e][e] = 1;
		}
		
		for(int e = 0;e<input.length;e++){
			double GJdivisor = input[e][e];
			for(int x = 0;x<input.length;x++){
				input[e][x] = input[e][x]/GJdivisor;
				GJI[e][x] = GJI[e][x]/GJdivisor;
				
				
			}
			
			for(int n = 0;n<input.length;n++){
				if(n != e){
					double GJfactor = input[n][e];
					for(int x = 0;x<input.length;x++){
						input[n][x] = input[n][x]-input[e][x]*GJfactor;
						GJI[n][x] = GJI[n][x]-GJI[e][x]*GJfactor;
					}
				}
			}
			
			
			
		}
		return GJI;
	}
	public double[] matrixAdd(double[] m1, double[] m2){
		double[] mr = new double[m1.length];
		for(int e = 0;e<mr.length;e++){
			mr[e] = m1[e] + m2[e];
		}
		return mr;
	}
	public double[][] matrixAdd(double[][] m1, double[][] m2){
		double[][] mr = new double[m1.length][m1[0].length];
		for(int e = 0;e<mr.length;e++){
			for(int n = 0;n<mr[0].length;n++){
				mr[e][n] = m1[e][n] + m2[e][n];
			}
		}
		return mr;
	}
	public double[][][] matrixCopy(double[][][] input){
		double[][][] output = new double[input.length][input[0].length][input[0][0].length];
		for(int e=0; e<input.length;e++){
			for(int n=0;n<input[0].length;n++){
				for(int x=0;x<input[0][0].length;x++) {
					output[e][n] = input[e][n];
				}
			}
		}
		return output;
	}
	public double[][] matrixCopy(double[][] input){
		double[][] output = new double[input.length][input[0].length];
		for(int e=0; e<input.length;e++){
			for(int n=0;n<input[0].length;n++){
				output[e][n] = input[e][n];
			}
		}
		return output;
	}
	public double[] matrixCopy(double[] input){
		double[] output = new double[input.length];
		for(int e=0; e<input.length;e++){
			output[e] = input[e];
		}
		return output;
	}
	public double[] matrixColumn(double[][] input, int j){
		double[] output = new double[input.length];
		for(int e=0; e<input.length;e++){
			output[e] = input[e][j];
		}
		return output;
	}
	public double[][] matrixMult(double[][] m1, double[][] m2){
		int m1_rows = m1.length;
		int m1_cols = m1[0].length;
		int m2_cols = m2[0].length;
		double[][] mr = new double[m1_rows][m2_cols];
		for(int e = 0; e<m1_rows;e++){
			for(int n = 0;n<m2_cols;n++){
				double sumproduct = 0;
				for(int y = 0; y<m1_cols;y++){
					sumproduct = sumproduct + m1[e][y]*m2[y][n];
				}
				mr[e][n] = sumproduct;
			}
		}
		return mr;
	}
	public static double[] matrixMult(double[][] m1, double[] m2){
		//int m2_rows = m2.length;
		int m1_cols = m1[0].length;
		double[] mr = new double[m1_cols];
		for(int e = 0; e<m1_cols;e++){
			double sumproduct = 0;
			for(int y = 0; y<m1_cols;y++){
				sumproduct = sumproduct + m1[e][y]*m2[y];
			}
			mr[e] = sumproduct;
		}
		return mr;
	}
	public double matrixAbsMax(double[] m1){
		int m1_rows = m1.length;
		double max = m1[0];
		
		for(int e = 1; e<m1_rows;e++){
			if(Math.abs(m1[e])>max) {
				max = Math.abs(m1[e]);
			}
		}
		return max;
	}
	public double matrixAbsMax(double[][] m1){
		int m1_rows = m1.length;
		int m1_cols = m1[0].length;
		double max = m1[0][0];
		
		for(int e = 0;e<m1_rows;e++){
			for(int f = 0;f<m1_cols;f++) {
				if(Math.abs(m1[e][f])>max) {
					max = Math.abs(m1[e][f]);
				}
			}
		}
		return max;
	}
	public double matrixMax(double[] m1){
		int m1_rows = m1.length;
		double max = m1[0];
		
		for(int e = 1; e<m1_rows;e++){
			if(m1[e]>max) {
				max = m1[e];
			}
		}
		return max;
	}
	
	public double matrixMax(double[][] m1){
		int m1_rows = m1.length;
		int m1_cols = m1[0].length;
		double max = m1[0][0];
		
		for(int e = 0;e<m1_rows;e++){
			for(int f = 0;f<m1_cols;f++) {
				if(m1[e][f]>max) {
					max = m1[e][f];
				}
			}
		}
		return max;
	}
	public void matrixPrint(double[][] input){
		DecimalFormat myFormatter = new DecimalFormat("##.###");
		for(int e=0;e<input.length;e++){
			for(int n=0;n<input[0].length;n++){
				String output = myFormatter.format(input[e][n]);
				if(input[e][n]>=0){
					output = " " + output;
				}
				System.out.print(output);
				System.out.print('\t');
			}
			System.out.print('\r');
		}
		System.out.print('\r');
	}
	public void matrixPrint(double[] input){
		DecimalFormat myFormatter = new DecimalFormat("##.###");
		for(int e=0;e<input.length;e++){
			String output = myFormatter.format(input[e]);
			if(input[e]>=0){
				output = " " + output;
			}
			System.out.print(output);
			System.out.print('\r');
		}
		System.out.print('\r');
	}
	public void matrixPrint(int[] input){
		DecimalFormat myFormatter = new DecimalFormat("##.###");
		for(int e=0;e<input.length;e++){
			String output = myFormatter.format(input[e]);
			if(input[e]>=0){
				output = " " + output;
			}
			System.out.print(output);
			System.out.print('\r');
		}
		System.out.print('\r');
	}
	public double[] matrixSub(double[] m1, double[] m2){
		double[] mr = new double[m1.length];
		for(int e = 0;e<mr.length;e++){
			mr[e] = m1[e] - m2[e];
		}
		return mr;
	}
	public double[][] matrixSub(double[][] m1, double[][] m2){
		double[][] mr = new double[m1.length][m1[0].length];
		for(int e = 0;e<mr.length;e++){
			for(int n = 0;n<mr[0].length;n++){
				mr[e][n] = m1[e][n] - m2[e][n];
			}
		}
		return mr;
	}
	public double[][] matrixSet(double[][] m1, double[][] m2, int i, int j){
		double[][] mr = new double[m1.length][m1[0].length];
		for(int e = 0;e<mr.length;e++){
			for(int n = 0;n<mr[0].length;n++){
				mr[e+i][n+j] = m2[e][n];
			}
		}
		return mr;
	}
	public double[][] matrixSet(double[][] m1, double[] m2, int i, int j){
		double[][] mr = new double[m1.length][m1[0].length];
		for(int e = 0;e<mr.length;e++){
			mr[e+i][j] = m2[e];
		}
		return mr;
	}
	
	public double[] planerotate(double[] dp, double theta, double delta) { //This needs to be optimized 2020-10-12
		double[] output = new double[3];
		double dplength = 0;
    	for(int v = 0;v<3;v++) {
    		dplength = dplength + Math.pow(dp[v+3]-dp[v], 2.0);
    	}
    	dplength = Math.sqrt(dplength);
    	double[] un = new double[3];
    	un[0] = (dp[3]-dp[0])/dplength;
    	un[1] = (dp[4]-dp[1])/dplength;
    	un[2] = (dp[5]-dp[2])/dplength;
    	double[][] R = new double[3][3];
    	R[0][0] = Math.cos(theta)+Math.pow(un[0], 2.0)*(1-Math.cos(theta));
    	R[1][1] = Math.cos(theta)+Math.pow(un[1], 2.0)*(1-Math.cos(theta));
    	R[2][2] = Math.cos(theta)+Math.pow(un[2], 2.0)*(1-Math.cos(theta));
    	
    	R[0][1] = un[1]*un[0]*(1-Math.cos(theta))+un[2]*Math.sin(theta);
    	R[1][0] = un[1]*un[0]*(1-Math.cos(theta))-un[2]*Math.sin(theta);
    	
    	R[0][2] = un[2]*un[0]*(1-Math.cos(theta))-un[1]*Math.sin(theta);
    	R[2][0] = un[2]*un[0]*(1-Math.cos(theta))+un[1]*Math.sin(theta);
    	
    	R[1][2] = un[2]*un[1]*(1-Math.cos(theta))+un[0]*Math.sin(theta);
    	R[2][1] = un[2]*un[1]*(1-Math.cos(theta))-un[0]*Math.sin(theta);
    	double dptl = 0;
    	for(int v = 0;v<3;v++) {
    		dptl = dptl + Math.pow(dp[v+6]-dp[v], 2.0);
    	}
    	dptl = Math.sqrt(dptl);
    	double[] up = new double[3];
    	up[0] = (dp[6]-dp[0])/dplength;
    	up[1] = (dp[7]-dp[1])/dplength;
    	up[2] = (dp[8]-dp[2])/dplength;
    	output = matrixMult(R,up);
		return output;
	}
	
	private static final long serialVersionUID = 1;
}
