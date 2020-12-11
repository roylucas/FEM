package Solver;
import Jama.Matrix;
import Matrix_Algebra.Matrix_Algebra;

public class Truss {
	
	public double[][] Ke_Truss(double[][] Ke, int nel, double E, double A, double L, double[][] xn, int[] ien, int ndf, int nen){

			
			Matrix_Algebra ma = new Matrix_Algebra();
			
			double[][] ke = Two_D_Truss_Local_Stiff(E, A, L, nen, ndf).getArray();
			
			Matrix Qe = new Matrix(ndf*nen,ndf*nen);
			int n1 = ien[0];  //Returns global number of i node of element "e"
			int n2 = ien[1];  //Returns global number of j node of element "e"
			Qe = Two_D_Truss_Rotation_Matrix(xn, ien, L, n1, n2, nen, ndf);
			
			
			double[][] QeI = new double[ndf*nen][ndf*nen];
			QeI = Qe.inverse().getArray();
			
			Ke = ma.matrixMult(ma.matrixMult(QeI, ke), Qe.getArray());
			
		return Ke;
	}
	
	Matrix Two_D_Truss_Local_Stiff(double E, double A, double L, int nen, int ndf){
		Matrix ke = new Matrix(nen*ndf,nen*ndf);
		ke.set(0, 0, E*A/L);
		ke.set(0, 2, -E*A/L);
		ke.set(2, 0, -E*A/L);
		ke.set(2, 2, E*A/L);
		
		return ke;
	}
	
	Matrix Two_D_Truss_Rotation_Matrix(double[][] xn, int[] ien, double L, int n1, int n2, int nen, int ndf){
		Matrix Qe = new Matrix(nen*ndf, nen*ndf);
		double[] v = new double[2];
		v[0] = (xn[0][n2] - xn[0][n1])/L;
		v[1] = (xn[1][n2] - xn[1][n1])/L;
		Qe.set(0, 0, v[0]);
        Qe.set(0, 1, v[1]);
        Qe.set(1, 0,-v[1]);
        Qe.set(1, 1, v[0]);
        Qe.set(2, 2, v[0]);
        Qe.set(2, 3, v[1]);
        Qe.set(3, 2,-v[1]);
        Qe.set(3, 3, v[0]);
	
		return Qe;
	}
	
	
}
