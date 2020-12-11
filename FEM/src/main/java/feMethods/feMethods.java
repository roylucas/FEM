package feMethods;

import java.io.FileInputStream;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;

import org.ejml.data.DMatrixRMaj;
import org.ejml.simple.SimpleMatrix;
import Matrix_Algebra.Matrix_Algebra;
import Jama.Matrix;

public class feMethods {
	
	// Force and Stiffness Matrix Assembly
	
	public double[] addforce(double[] F,int[][] id,double[][] fe,int[] ien,int nen,int ndf){
		for(int n = 0;n<nen;n++){
		    for(int i = 0;i<ndf;i++){
                if(id[i][ien[n]] >= 0){
                    int P=id[i][ien[n]];
                    F[P] = F[P]+fe[i][n];
                }
		    }
		}
		return F;
	}
	public double[][] addstiff(double[][] K,int[][] id,double[][] Ke,int[] ien,int nen,int ndf){
		for(int n = 0;n<nen;n++){
		    for(int i = 0;i<ndf;i++){
                if(id[i][ien[n]] >= 0){
                    int P=id[i][ien[n]];
                    for(int m = 0; m<nen; m++){
                        for(int j = 0;j<ndf;j++){
                            if(id[j][ien[m]] >= 0){
                                int Q = id[j][ien[m]];
                                K[P][Q] = K[P][Q] + Ke[i+n*ndf][j+m*ndf];
                            }
                        }
                    }
                }
		    }
		}
		return K;
	}
	
	public double[][] EJMLsolve(double[][] K) {
		org.ejml.data.Matrix Km = new DMatrixRMaj(K);
		double[][] Ki = ColMat_RecArray(((DMatrixRMaj)SimpleMatrix.wrap(Km).invert().getMatrix()));
		return Ki;
	}
	
	public double[][] JAMAsolve(double[][] K) {
		Matrix Km = new Matrix(K);
		Matrix Kim = Km.inverse();
		double[][] Ki = Kim.getArray();
		return Ki;
	}
	
	public double[][] ColMat_RecArray(DMatrixRMaj mat) {
		int r = mat.getNumRows();
		int c = mat.getNumCols();
		double[] data = mat.getData();
		double[][] a = new double[r][c];
		for(int i=0;i<r;i++){
			for(int j=0;j<c;j++) {
				a[i][j] = data[i*c+j];
			}
		}
		return a;
	}
	
	// Input Processing
	
	public int[] dupnp(double[][] tmp, int nsd) {
		int[] nflg = new int[tmp.length];

		for(int e = 0;e<tmp.length;e++) {
        	if(nflg[e] != 1) {
        		for(int i = e+1;i<tmp.length;i++) {
        			int count =0;
        			for(int o = 0;o<nsd;o++) {
        				if(tmp[e][o] == tmp[i][o]) {
        					count++;
        				}
        			}
        			if(count == nsd && nflg[i] != 1) {
        				nflg[i] = 1;
        			}
        			/*if(tmp[e][0] == tmp[i][0] && tmp[e][1] == tmp[i][1] && nflg[i] != 1) {
        				nflg[i] = 1;
        			}*/
        		}
        	}
        }
		return nflg;
	}
	public int nnp(double[][] tmp, int[] nflg, int nel,int nen, int nsd) {
		int counter = 0;
        for(int e = 0;e<nflg.length;e++) {
        	counter = counter + nflg[e];
    	}
        int nnp = nel*nen - counter;
		return nnp;
	}
	
	public double[] memlength(double[][] xn, int ien [][],int nel, int nsd) {
		double[] L = new double[nel];
		for(int e = 0;e<nel;e++){
			for(int v=0;v<nsd;v++) {
				double iend = xn[v][ien[e][0]];
				double jend = xn[v][ien[e][1]];
				L[e] = L[e] + Math.pow((jend-iend),2.0);
			}
			L[e] = Math.sqrt(L[e]);
		}
		return L;
	}
	
	public double minlength(int[] nec, double[] L) {
		double lMin = -1;
		for(int e=0;e<nec.length;e++) {
			if(nec[e]>0) {
				if(lMin<0 || lMin>L[e]) {
					lMin = L[e];
				}
			}
			
		}
		return lMin;
	}
	
	// Deformation and Post-Processing
	
	double[][] moment;
	double[] axial;
		
	public double[] trussaxial(int nel,int nen,int nsd,double[] E, double[] A, double[] L,int[][] id,int[][] ien,double[][] xn,double[] U) {
		double[] trussaxial = new double[nel];
		double[][][] coord = trussdeform(nel,nen,nsd,E,A,L,id,ien,xn,U);
		
		for(int e = 0;e<nel;e++) {
			double changeL = 0;
			for(int x = 0;x<nsd;x++) {
				changeL = changeL + Math.pow(coord[e][1][x]-coord[e][0][x], 2);
			}
			changeL = Math.pow(changeL, 0.5);
			trussaxial[e] = (changeL-L[e])*E[e]*A[e]/L[e];
		}
		return trussaxial;
	}
	public double[][][] trussdeform(int nel,int nen,int nsd,double[] E, double[] A, double[] L,int[][] id,int[][] ien,double[][] xn,double[] U) {
		double[][][] coord = new double[nel][nen][nsd];
		
		for(int e = 0;e<nel;e++) {
			for(int n = 0;n<nen;n++) {
				for(int x = 0;x<nsd;x++) {
					int dof = id[x][ien[e][n]];
					double displace = 0;
					if(dof != -1) {
						displace = U[dof];
					}
					coord[e][n][x] = xn[x][ien[e][n]] + displace;
				}
			}
		}
		return coord;
	}
	
	public double[][][] beamdeform(int nel,int nen,int nsd,double[] E, double[] A, double[] I, double[] L,int[][] id,int[][] ien,double[][] xn,double[] U,double scalefactor, int beamRes) {
		double[][][] coord = new double[nel][nen+beamRes][nsd];
		double[][] moment = new double[nel][nen];
		double[] axial = new double[nel];
		
		for(int e = 0;e<nel;e++) { //Loop through every element and calculate the deformed shape of the beam and internal points
			double[][] uDE = new double[nen][nsd]; //Undeformed Element Coordinates
			for(int n = 0;n<nen;n++) {
				for(int s = 0;s<nsd;s++) {
					uDE[n][s] = xn[s][ien[e][n]];
				}
			}
			
			double[][] DE = new double[nen][nsd]; //Deformed Element Coordinates
			for(int n = 0;n<nen;n++) {
				for(int s = 0;s<nsd;s++) {
					if(id[s][ien[e][n]] != -1) {
						DE[n][s] = U[id[s][ien[e][n]]] + uDE[n][s];
					}else {
						DE[n][s] = uDE[n][s];
					}
					
				}
			}
			
			double DL = 0; //Deformed Length
			for(int n = 0;n<nsd;n++) {
				DL = DL + Math.pow(DE[1][n]-DE[0][n],2);
			}
			DL = Math.sqrt(DL);
			
			double[] vuDE = new double[nsd]; //Undeformed Element Vector
			double[] vDE = new double[nsd]; //Deformed Element Vector
			for(int n = 0;n<nen;n++) {
				for(int s = 0;s<nsd;s++) {
					vuDE[s] = (uDE[1][s] - uDE[0][s])/L[e];
					vDE[s] = (DE[1][s] - DE[0][s])/DL;
					
				}
			}
			
			double[] uDA = new double[nsd - 1]; //Undeformed Element Angles, Start Point to End Point
			double[] DEA = new double [nsd - 1]; //Deformed Element Angles, Start Point to End Point
			for(int n = 0;n<uDA.length;n++) {
				uDA[n] = Math.atan((vuDE[n+1])/(vuDE[n]));
				DEA[n] = Math.atan((vDE[n+1])/(vDE[n]));
				if(vuDE[n] < 0 && vuDE[n+1] >= 0) {
					uDA[n] = Math.PI + uDA[n];
				}else if(vuDE[n] < 0 && vuDE[n+1] < 0) {
					uDA[n] = Math.PI + uDA[n];
				}else if(vuDE[n] >= 0 && vuDE[n+1] < 0) {
					uDA[n] = 2*Math.PI + uDA[n];
				}
				if(vDE[n] < 0 && vDE[n+1] >= 0) {
					DEA[n] = Math.PI + DEA[n];
				}else if(vDE[n] < 0 && vDE[n+1] < 0) {
					DEA[n] = Math.PI + DEA[n];
				}else if(vDE[n] >= 0 && vDE[n+1] < 0) {
					DEA[n] = 2*Math.PI + DEA[n];
				}
			}
			
			double[] DA = new double[nsd-1]; //Start Angle of Deformed Element
			double[] eDA = new double[nsd-1]; //End Angle of Deformed Element
			
			//for(int n = 0;n<uDA.length;n++) {
			//	
			//}
			if(id[2][ien[e][0]] != -1) {
				DA[0] = uDA[0] + U[id[2][ien[e][0]]];
			}else {
				DA[0] = uDA[0];
			}
			if(id[2][ien[e][1]] != -1) {
				eDA[0] = uDA[0] + U[id[2][ien[e][1]]];
			}else {
				eDA[0] = uDA[0];
			}
			
			if(DA[0]<0) {
				DA[0] = DA[0]+2*Math.PI;
			}
			if(eDA[0]<0) {
				eDA[0] = eDA[0]+2*Math.PI;
			}
			
			double tL = eDA[0] - DA[0]; //Rotation of end of element relative to Start Angle of Deformed Element
			double dL = (DEA[0] - DA[0]); //Deflection of Deformed Element end relative to Start Angle of Deformed Element, sin(x) = x
			if(dL>Math.PI) {
				dL = dL - 2*Math.PI; //Negative deflection requires a negative angle
			}
			if(dL<-Math.PI) {
				dL = dL + 2*Math.PI; //Negative deflection requires a negative angle
			}
			
			if(tL>Math.PI) {
				tL = tL - 2*Math.PI; //Negative deflection requires a negative angle
			}
			if(tL<-Math.PI) {
				tL = tL + 2*Math.PI; //Negative deflection requires a negative angle
			}
			
			dL = dL*L[e];
			
			double[] vDA = new double[nsd]; //Vector for Deformed Element Baseline
			
			//for(int n = 0;n<nsd;n++) {
			vDA[0] = Math.cos(DA[0]);
			vDA[1] = Math.sin(DA[0]);
			//}
			
			coord[e][0][0] = (DE[0][0]-uDE[0][0])*scalefactor+uDE[0][0];
			coord[e][0][1] = (DE[0][1]-uDE[0][1])*scalefactor+uDE[0][1];
			coord[e][beamRes+nen-1][0] = (DE[1][0]-uDE[1][0])*scalefactor+uDE[1][0];
			coord[e][beamRes+nen-1][1] = (DE[1][1]-uDE[1][1])*scalefactor+uDE[1][1];
			
			double mNaut = E[e]*I[e]*(4*tL/L[e]-6*dL/Math.pow(L[e],2));
			double pNaut = E[e]*I[e]*(12*dL/Math.pow(L[e],3)-6*tL/Math.pow(L[e],2));
			
			moment[e][0] = pNaut*DL+mNaut;
			moment[e][1] = mNaut;
			
			axial[e] = (DL-L[e])/L[e]*E[e]*A[e];
			
			double scaleDL = (DL-L[e])*scalefactor+L[e]; //Scaled, Deformed Length
			
			for(int n = 0;n<beamRes;n++) {
				double x = ((double) n+1)/((double) beamRes+1)*L[e];
				double scalex = ((double) n+1)/((double) beamRes+1)*scaleDL;
				double d = (12*dL/Math.pow(L[e],3)-6*tL/Math.pow(L[e],2))*(Math.pow(x,2)*L[e]/2-Math.pow(x,3)/6)+(4*tL/L[e]-6*dL/Math.pow(L[e],2))*(Math.pow(x,2)/2);
				coord[e][n+1][0] = (DE[0][0]-uDE[0][0])*scalefactor+uDE[0][0]-d*vDA[1]*scalefactor+scalex*((vDA[0]-vuDE[0])*scalefactor+vuDE[0]);
				coord[e][n+1][1] = (DE[0][1]-uDE[0][1])*scalefactor+uDE[0][1]+d*vDA[0]*scalefactor+scalex*((vDA[1]-vuDE[1])*scalefactor+vuDE[1]);
			}
			
			//coord[e][n][x] = xn[x][ien[e][n]] + displace*scalefactor;
		
		}
		this.moment = moment;
		this.axial = axial;
		return coord;
	}
	
	
	public boolean regionintersect(double[][] ien, double [][] xn, double[][] region) {
		boolean check = false;
		/*
		If intersection algorithm returns true then there is an intersection i.e. a conflict. 
		*/
		return check;
		
	}
	
	
	// Read Excel functions
	
	public double[][] readExcel(double[][] m, FileInputStream file, XSSFSheet s, int r, int c) throws Exception {
		 
		int i = m.length;
		int j = m[0].length;
		for(int e = 0;e<i;e++) {
			for(int n = 0;n<j;n++) {
				m[e][n] = (double) s.getRow(e+r).getCell(n+c).getNumericCellValue();
			}
		}
		return m;
	}
	public int[][] readExcel(int[][] m, FileInputStream file, XSSFSheet s, int r, int c) throws Exception {
		 
		int i = m.length;
		int j = m[0].length;
		for(int e = 0;e<i;e++) {
			for(int n = 0;n<j;n++) {
				m[e][n] = (int) s.getRow(e+r).getCell(n+c).getNumericCellValue();
			}
		}
		return m;
	}
	public int nrExcel(FileInputStream file, XSSFWorkbook wb, XSSFSheet s, int c) throws Exception {
		int x = 0;
		//String test = s.getRow(0).getCell(0).getStringCellValue();
		wb.setMissingCellPolicy(Row.CREATE_NULL_AS_BLANK);
		try {
		while(s.getRow(x).getCell(c).getCellType() != Cell.CELL_TYPE_BLANK) {
			x++;
		}}catch (Exception e) {
	         e.printStackTrace();
	      }
		return x;
	}
	public int ncExcel(FileInputStream file, XSSFWorkbook wb, XSSFSheet s, int r) throws Exception {
		int x = 0;
		//String test = s.getRow(0).getCell(0).getStringCellValue();
		wb.setMissingCellPolicy(Row.CREATE_NULL_AS_BLANK);
		try {
			//double p = s.getRow(r).getCell(x).getNumericCellValue();
		while(s.getRow(r).getCell(x).getCellType() != Cell.CELL_TYPE_BLANK) {
			x++;
		}}catch (Exception e) {
	         e.printStackTrace();
	      }
		return x;
	}
}
