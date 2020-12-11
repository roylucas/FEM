package Solver;

import Jama.Matrix;
import Matrix_Algebra.Matrix_Algebra;

public class Stiffness {
	
	Matrix_Algebra ma = new Matrix_Algebra();
	
	public double[][] Solver(int e, int nsd,int nen,int ndf,int nel, double[] E,double[] A,double[] I,double[] L,double[][] xn, int[][] ien,int[] flg) {
		double[][] Ke = new double[ndf*nen][ndf*nen];
		if(ndf == 3 && nsd == 2) {
			Ke = Ke_Beam(nel, E[e],A[e],I[e],L[e],xn,ien[e],ndf,nen,nsd);
		}else {
			Ke = Ke_Truss(nel,E[e],A[e],L[e],xn,ien[e],ndf,nen,nsd);
		}
		
		return Ke;
	}
	
	double[][] Ke_Beam(int nel, double E, double A, double I, double L, double[][] xn, int[] ien, int ndf, int nen, int nsd){

		double[][] ke = Two_D_Beam_Local_Stiff(E, A, I, L, nen, ndf).getArray();
		
		int n1 = ien[0];  //Returns global number of i node of element "e"
		int n2 = ien[1];  //Returns global number of j node of element "e"
		Matrix Qe = Two_D_Rotation_Matrix(xn, ien, L, n1, n2, nen, ndf, nsd);
		
		double[][] QeI = new double[nen*ndf][ndf*nen];
		QeI = Qe.inverse().getArray();
		
		double[][] Ke = ma.matrixMult(ma.matrixMult(QeI, ke), Qe.getArray());
		
		return Ke;
	}
	double[][] Ke_Truss(int nel, double E, double A, double L, double[][] xn, int[] ien, int ndf, int nen, int nsd){
		double[][] ke = Truss_Local_Stiff(E, A, L, nen, ndf).getArray();
		
		int n1 = ien[0];  //Returns global number of i node of element "e"
		int n2 = ien[1];  //Returns global number of j node of element "e"
		Matrix Qe = Truss_Rotation_Matrix(xn, ien, L, n1, n2, nen, ndf, nsd);
		
		double[][] QeT = new double[nen*ndf][nen];
		QeT = Qe.transpose().getArray();
		
		double[][] Ke = ma.matrixMult(ma.matrixMult(QeT, ke), Qe.getArray());
		
		return Ke;
	}
	Matrix Two_D_Beam_Local_Stiff(double E, double A, double I, double L, int nen, int ndf){
		Matrix ke = new Matrix(nen*ndf,nen*ndf);
		ke.set(0, 0, E*A/L);
		ke.set(0, 3, -E*A/L);
		ke.set(1,1,12*E*I/Math.pow(L,3.0));
		ke.set(1, 2, 6*E*I/Math.pow(L,2.0));
		ke.set(1, 4, -12*E*I/Math.pow(L,3.0));
		ke.set(1, 5, 6*E*I/Math.pow(L,2.0));
		ke.set(2, 1, 6*E*I/Math.pow(L,2.0));
		ke.set(2, 2, 4*E*I/L);
		ke.set(2, 4, -6*E*I/Math.pow(L,2.0));
		ke.set(2, 5, 2*E*I/L);
		ke.set(3, 0, -E*A/L);
		ke.set(3, 3, E*A/L);
		ke.set(4, 1, -12*E*I/Math.pow(L,3.0));
		ke.set(4, 2, -6*E*I/Math.pow(L,2.0));
		ke.set(4, 4, 12*E*I/Math.pow(L,3.0));
		ke.set(4, 5, -6*E*I/Math.pow(L,2.0));
		ke.set(5, 1, 6*E*I/Math.pow(L,2.0));
		ke.set(5, 2, 2*E*I/L);
		ke.set(5, 4, -6*E*I/Math.pow(L,2.0));
		ke.set(5, 5, 4*E*I/L);
		
		return ke;
	}
	Matrix Truss_Local_Stiff(double E, double A, double L, int nen, int ndf){
		Matrix ke = new Matrix(nen,nen);
		
			ke.set(0, 0, E*A/L);
			ke.set(0, 1, -E*A/L);
			ke.set(1, 0, -E*A/L);
			ke.set(1, 1, E*A/L);
	
		return ke;
	}
	Matrix Two_D_Rotation_Matrix(double[][] xn, int[] ien, double L, int n1, int n2, int nen, int ndf, int nsd){
		Matrix Qe = new Matrix(nen*ndf, nen*ndf);
		double[] v = new double[nsd];
		for(int x = 0;x<nsd;x++) {
			v[x] = (xn[x][n2] - xn[x][n1])/L;
		}
		
		//for(int x = 0;x<nsd;x++) {
		//	Qe.set(0,x,v[x]);
		//	Qe.set(1,x+nsd,v[x]);
		//}
		if(nsd == 2) {
			Qe.set(0, 0, v[0]);
		    Qe.set(0, 1, v[1]);
		    Qe.set(1,0,-v[1]);
		    Qe.set(1, 1, v[0]);
		    Qe.set(2, 2, 1);
		    Qe.set(3, 3, v[0]);
		    Qe.set(3, 4, v[1]);
		    Qe.set(4, 3, -v[1]);
		    Qe.set(4, 4, v[0]);
		    Qe.set(5, 5, 1);		    
		}else {
			//Qe.set(0, 0, v[0]);
		    //Qe.set(0, 1, v[1]);
		    //Qe.set(1,0,-v[1]);
		    //Qe.set(1, 1, v[0]);
		    //Qe.set(2, 2, 1);
		    //Qe.set(3, 3, v[0]);
		    //Qe.set(3, 4, v[1]);
		    //Qe.set(4, 3, -v[1]);
		    //Qe.set(4, 4, v[0]);
		    //Qe.set(5, 5, 1);
		}
		
	
		return Qe;
	}
	Matrix Truss_Rotation_Matrix(double[][] xn, int[] ien, double L, int n1, int n2, int nen, int ndf, int nsd){
		Matrix Qe = new Matrix(nen, nen*ndf);
		double[] v = new double[nsd];
		for(int x = 0;x<nsd;x++) {
			v[x] = (xn[x][n2] - xn[x][n1])/L;
			for(int n=0;n<nen;n++) {
				Qe.set(n, x+n*ndf, v[x]);
			}
		}
		
		return Qe;
	}
}
